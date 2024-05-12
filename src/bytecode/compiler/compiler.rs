//! Bytecode emitter. Compiles bytecode from AST.

mod stack_frame;
mod macros;
pub mod error;
mod util;
mod init_state;

use crate::config::FrameAddress;
use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex, VariantIndex, STACK_ADDRESS_TYPE};
use crate::shared::{BindingContainer, TypeContainer, numeric::Numeric, meta::{Type, ImplTrait, Struct, Array, Enum, Function, FunctionKind, Binding, Constant, ConstantValue}, typed_ids::{BindingId, FunctionId, TypeId, ConstantId}};
use crate::frontend::{ast::{self, Typeable, TypeName, ControlFlow, Positioned}, resolver::resolved::{ResolvedProgram, Resolved}};
use crate::bytecode::{Constructor, Writer, StoreConst, Program, VMFunc, HeapRefOp, builtins::{builtin_types, BuiltinType}};
use stack_frame::{StackFrame, StackFrames};
use error::{CompileError, CompileErrorKind, CompileResult, OptionToCompileError};
use util::{LoopControlStack, LoopControl, Functions};
use init_state::{InitState, BranchingKind, BranchingPath, BranchingScope, BranchingState};
use macros::{comment, select_unsigned_opcode};

/// Bytecode emitter. Compiles bytecode from resolved program (AST).
pub(crate) struct Compiler<T> {
    /// Bytecode writer used to output to.
    pub(crate) writer: Writer<T>,
    /// Program metadata, e.g. types, functions,...
    resolved: Resolved,
    /// Maps from binding id to load-argument for each frame.
    locals: StackFrames,
    /// Non-primitive type constructors.
    constructors: UnorderedMap<TypeId, StackAddress>,
    /// Tracks call and function addresses so that calls made before the function address was known can be fixed.
    functions: Functions,
    /// Tracks variable initialization state.
    init_state: InitState,
    /// Tracks loop break/continue exit jump targets.
    loop_control: LoopControlStack,
    /// Module base path. For error reporting only.
    module_path: String,
    /// Contiguous trait function enumeration/mapping.
    trait_function_indices: UnorderedMap<FunctionId, ItemIndex>,
    /// Contiguous Trait implementor enumeration/mapping. Types implementing traits.
    trait_implementor_indices: UnorderedMap<TypeId, ItemIndex>,
    /// Base address of the trait vtable in the program const pool/stack.
    trait_vtable_base: StackAddress,
    /// Mapping from trait implementor function ids to trait function ids.
    trait_function_implementors: UnorderedMap<FunctionId, FunctionId>,
}

/// Compiles a resolved program into bytecode.
///
/// # Examples
///
/// The following example based on the resolve-example additionally compiles the program.
/// ```
/// use itsy::{itsy_api, parser, resolver, compiler};
///
/// // Define an API of Rust functions that are callable from the Itsy script.
/// itsy_api!(MyAPI<()> {
///     fn print(&mut context, value: &str) {
///         println!("print: {}", value);
///     }
///     // ... more functions ...
/// });
///
/// fn main() {
///     let module = parser::parse_module("
///         // An Itsy program that calls the Rust 'print' function.
///         fn main() {
///             MyAPI::print(\"Hello from Itsy!\");
///         }
///     ", "").unwrap();
///     let mut parsed = parser::ParsedProgram::new();
///     parsed.add_module(module);
///     let resolved = resolver::resolve::<MyAPI>(parsed, "main").unwrap();
///     let compiled = compiler::compile(resolved).unwrap();
/// }
/// ```
///
/// The returned [Program] is now ready to be run by [run](crate::run) or [VM::run](crate::runtime::VM::run).
pub fn compile<T>(program: ResolvedProgram<T>) -> CompileResult<Program<T>> where T: VMFunc<T> {

    let ResolvedProgram { modules, resolved, entry_fn, .. } = program;

    // find all trait functions and all trait implementors
    let trait_functions = Compiler::<T>::filter_trait_functions(&resolved);
    let trait_implementors: Vec<_> = resolved.implementors().collect();

    // save flattened list of trait implementations and their concrete function ids for later serialization (once compilation is done and absolute function addresses are known)
    let trait_function_implementations = Compiler::<T>::select_trait_function_implementations(&resolved, &trait_functions, &trait_implementors)?;

    // initialize compiler struct used while visiting every ast node
    let mut compiler = Compiler {
        writer                      : Writer::new(),
        locals                      : StackFrames::new(),
        functions                   : Functions::new(),
        constructors                : UnorderedMap::new(),
        init_state                  : InitState::new(),
        loop_control                : LoopControlStack::new(),
        module_path                 : "".to_string(),
        trait_vtable_base           : (trait_implementors.len() * size_of::<StackAddress>()) as StackAddress, // offset vtable by size of mapping from constructor => implementor-index
        trait_function_indices      : Compiler::<T>::enumerate_trait_function_indices(&trait_functions),
        trait_implementor_indices   : Compiler::<T>::enumerate_trait_implementor_indices(&trait_implementors),
        trait_function_implementors : Compiler::<T>::map_trait_function_implementors(&resolved, &trait_functions, &trait_implementors)?,
        resolved,
    };

    // reserve space for the trait vtable as well as an implementor-index => constructor mapping (required for trait object reference counting) in const pool
    let vtable_size = compiler.trait_function_indices.len() * compiler.trait_implementor_indices.len() * size_of::<StackAddress>();
    compiler.writer.reserve_const_data(compiler.trait_vtable_base + vtable_size as StackAddress); // FIXME: this does not consider endianess

    // serialize constructors onto const pool, ensure position 0 and 1 are not used
    if compiler.writer.const_len() < 2 {
        compiler.writer.store_const(111 as u8); // constructor address 0 indicates a virtual constructor (picked via trait vtable)
        compiler.writer.store_const(123 as u8); // constructor address 1 indicates a dynamic constructor (stored at end of heap object, used by closures)
    }
    for (type_id, _) in compiler.resolved.types().filter(|ty| ty.1.is_constructible()) {
        // store constructor, remember position
        let position = compiler.store_constructor(type_id, &mut None, &mut 0)?;
        compiler.constructors.insert(type_id, position);
        // for trait implementing types, link constructor to trait implementor (trait objects still need proper reference counting, which requires the constructor of the concrete type)
        if let Some(&implementor_index) = compiler.trait_implementor_indices.get(&type_id) {
            compiler.writer.update_const((implementor_index as usize * size_of::<StackAddress>()) as StackAddress, position);
        }
    }

    // write placeholder jump to program entry
    let initial_pos = compiler.writer.call(123, 0);
    compiler.writer.exit();

    // compile program
    for module in modules {
        compiler.module_path = module.path.clone();
        for statement in module.statements() {
            if let Err(mut err) = compiler.compile_statement(statement) {
                err.module_path = module.path.clone();
                return Err(err);
            }
        }
    }

    // write actual function offsets to vtable
    for (implementor_index, selected_function_id) in trait_function_implementations {
        if let Some(selected_function_id) = selected_function_id {
            let selected_function_offset = compiler.functions.get(selected_function_id).ice_msg("Missing function callinfo")?;
            let vtable_function_offset = compiler.vtable_function_offset(selected_function_id)?;
            compiler.writer.update_const(vtable_function_offset + (implementor_index * size_of::<StackAddress>()) as StackAddress, selected_function_offset);
        }
    }

    // overwrite placeholder with actual entry position
    let entry_addr = compiler.functions.get(entry_fn).ice_msg("Failed to locate entry function in generated code.")?;
    let entry_arg_size = compiler.resolved.function(entry_fn).arg_size(&compiler);
    compiler.writer.overwrite(initial_pos, |w| w.call(entry_addr, entry_arg_size));

    // return generated program
    Ok(compiler.writer.into_program())
}

/// Methods for compiling individual code structures.
impl<T> Compiler<T> where T: VMFunc<T> {

    /// Compiles the given statement.
    fn compile_statement(self: &mut Self, item: &ast::Statement) -> CompileResult {
        use self::ast::Statement as S;
        match item {
            S::StructDef(_) => Ok(()),
            S::Module(_) => Ok(()),
            S::Use(_) => Ok(()),
            S::EnumDef(_) => Ok(()),
            S::Function(function) => self.compile_function(function),
            S::ImplBlock(impl_block) => {
                for function in &impl_block.functions {
                    self.compile_function(function)?;
                    // if this is a trait impl check compatibility to trait
                    if let Some(TypeName { type_id, .. }) = impl_block.trt {
                        let trait_type_id = type_id.ice()?;
                        let trt = self.ty(&trait_type_id).as_trait().ice()?;
                        let function_id = self.constant_by_id(function.constant_id.ice()?).value.as_function_id().ice()?;
                        let function_name = &function.shared.sig.ident.name;
                        // check if function is defined in trait
                        let trait_constant_id = trt.provided.get(function_name).or(trt.required.get(function_name)).ice()?;
                        let trait_function_id = self.resolved.constant(trait_constant_id.ice()?).value.as_function_id().ice()?;
                        let trait_function = self.resolved.function(trait_function_id);
                        let impl_function = self.resolved.function(function_id);
                        if !self.is_compatible_function(trait_function, impl_function)? {
                            return Err(CompileError::new(function, CompileErrorKind::IncompatibleTraitMethod(function_name.clone()), &self.module_path));
                        }
                    }
                }
                Ok(())
            },
            S::TraitDef(trait_def) => {
                for function in &trait_def.functions {
                    if function.shared.block.is_some() {
                        self.compile_function(function)?;
                    }
                }
                Ok(())
            },
            S::LetBinding(binding) => self.compile_let_binding(binding),
            S::IfBlock(if_block) => {
                self.compile_if_block(if_block)?;
                if let Some(result) = &if_block.if_block.result {
                    let result_type = self.ty(result);
                    comment!(self, "discarding result");
                    self.write_discard(result_type)?;
                }
                Ok(())
            },
            S::ForLoop(for_loop) => self.compile_for_loop(for_loop),
            S::WhileLoop(while_loop) => self.compile_while_loop(while_loop),
            S::Block(block) => {
                self.compile_block(block)?;
                if let Some(result) = &block.result {
                    let result_type = self.ty(result);
                    comment!(self, "discarding result");
                    self.write_discard(result_type)?;
                }
                Ok(())
            },
            S::Expression(expression) => {
                self.compile_expression(expression)?;
                let result_type = self.ty(expression);
                comment!(self, "discarding result");
                self.write_discard(result_type)?;
                Ok(())
            },
            S::Break(_) => {
                // write break-jump placeholder and record the opcode-position in loop control so it can be fixed later
                comment!(self, "break");
                self.write_scope_destructor(BranchingScope::Loop)?;
                let break_jump = self.writer.jmp(123);
                self.loop_control.add_jump(LoopControl::Break(break_jump));
                Ok(())
            },
            S::Continue(_) => {
                // write continue-jump placeholder and record the opcode-position in loop control so it can be fixed later
                comment!(self, "continue");
                self.write_scope_destructor(BranchingScope::Loop)?;
                let continue_jump = self.writer.jmp(123);
                self.loop_control.add_jump(LoopControl::Continue(continue_jump));
                Ok(())
            },
            S::Return(ast::Return { expr, .. }) => {
                comment!(self, "block returning");
                self.compile_expression(expr)?;
                // inc result, then dec everything
                self.write_cnt(self.ty(expr), true, HeapRefOp::Inc)?;
                self.write_scope_destructor(BranchingScope::Function)?;
                self.write_cnt(self.ty(expr), true, HeapRefOp::DecNoFree)?;
                self.write_ret()?;
                Ok(())
            },
        }
    }

    /// Compiles the given expression.
    fn compile_expression(self: &mut Self, item: &ast::Expression) -> CompileResult {
        use self::ast::Expression::*;
        match item {
            Literal(literal) => self.compile_literal(literal),
            Variable(variable) => self.compile_variable(variable),
            Constant(constant) => self.compile_constant(constant),
            Assignment(assignment) => self.compile_assignment(assignment),
            BinaryOp(binary_op) => self.compile_binary_op(binary_op),
            UnaryOp(unary_op) => self.compile_unary_op(unary_op),
            Block(block) => self.compile_block(block),
            IfBlock(if_block) => self.compile_if_block(if_block),
            MatchBlock(match_block) => self.compile_match_block(match_block),
            AnonymousFunction(anonymous_function) => self.compile_anonymous_function(anonymous_function),
            Closure(closure) => self.compile_closure(closure),
        }
    }

    /// Compiles the assignment operation.
    fn compile_assignment(self: &mut Self, item: &ast::Assignment) -> CompileResult {
        use self::ast::Expression::*;
        comment!(self, "{}", item);
        match &item.left {
            Variable(_) => self.compile_assignment_to_var(item),
            BinaryOp(_) => self.compile_assignment_to_offset(item),
            _ => Self::ice_at(item, "Attempted to assign to non-assignable"),
        }
    }

    /// Compiles an assignment to a variable.
    fn compile_assignment_to_var(self: &mut Self, item: &ast::Assignment) -> CompileResult {
        //use crate::frontend::ast::BinaryOperator as BO;
        use ast::{BinaryOperator::*, Expression as E};
        comment!(self, "direct assignment");
        let binding_id = item.left.as_variable().ice()?.binding_id;
        let loc = self.locals.lookup(binding_id);

        match (item.op, &item.right) {
            (AddAssign, E::Literal(r)) if self.ty(&item.left).is_numeric() => {
                self.write_add_vi(self.ty(&item.left), loc, r.value.as_numeric().ice()?)?;
            },
            (SubAssign, E::Literal(r)) if self.ty(&item.left).is_numeric() => {
                self.write_sub_vi(self.ty(&item.left), loc, r.value.as_numeric().ice()?)?;
            },
            (Assign, _) => {
                self.compile_expression(&item.right)?;
                self.write_store(self.ty(&item.left), loc, Some(binding_id))?;
                self.init_state.initialize(binding_id);
            },
            _ => {
                self.check_initialized(item.left.as_variable().ice()?)?;
                self.write_load(self.ty(&item.left), loc)?; // stack: left
                self.compile_expression(&item.right)?; // stack: left right
                let ty = self.ty(&item.left);
                match item.op { // stack: result
                    AddAssign => self.write_add(ty)?,
                    SubAssign => self.write_sub(ty)?,
                    MulAssign => self.write_mul(ty)?,
                    DivAssign => self.write_div(ty)?,
                    RemAssign => self.write_rem(ty)?,
                    _ => Self::ice_at(item, "Invalid assignment operator while assigning to var")?,
                };
                self.write_store(self.ty(&item.left), loc, Some(binding_id))?; // stack --
            },
        };
        Ok(())
    }

    /// Compiles an assignment to an index- or access-write operation.
    fn compile_assignment_to_offset(self: &mut Self, item: &ast::Assignment) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        comment!(self, "offset assignment");
        match item.op {
            BO::Assign => {
                self.compile_expression(&item.left)?;       // stack: &left
                self.compile_expression(&item.right)?;      // stack: &left right
                let ty = self.ty(&item.left);
                self.write_heap_put(ty, false)?;    // stack: --
            },
            _ => {
                self.compile_expression(&item.left)?;       // stack: &left
                self.write_clone_ref();                     // stack: &left &left
                let ty = self.ty(&item.left);
                self.write_heap_fetch(ty)?;                      // stack: &left left
                self.compile_expression(&item.right)?;      // stack: &left left right
                let ty = self.ty(&item.left);
                match item.op {                                 // stack: &left result
                    BO::AddAssign => self.write_add(ty)?,
                    BO::SubAssign => self.write_sub(ty)?,
                    BO::MulAssign => self.write_mul(ty)?,
                    BO::DivAssign => self.write_div(ty)?,
                    BO::RemAssign => self.write_rem(ty)?,
                    _ => Self::ice_at(item, "Invalid assignment operator while assigning to offset")?,
                };
                self.write_heap_put(ty, false)?; // stack -
            },
        }
        Ok(())
    }

    /// Compiles function call arguments.
    fn compile_call_args(self: &mut Self, item: &ast::BinaryOp, function_kind: FunctionKind) -> CompileResult {
        let args = &item.right.as_argument_list().ice()?.args;
        // put args on stack, increase ref count to ensure temporaries won't be dropped after access
        // function is responsible for decrementing argument ref-count on exit
        for (_index, arg) in args.iter().enumerate() {
            comment!(self, "{}() arg {}", item.left, arg);
            self.compile_expression(arg)?;
            match function_kind {
                FunctionKind::Method(_) | FunctionKind::Function => {
                    self.write_cnt(self.ty(arg), true, HeapRefOp::Inc)?;
                },
                _ => { }
            }
        }
        Ok(())
    }

    /// Compiles the given call.
    fn compile_binary_op_call(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        use ast::Expression::*;
        match item.left.as_expression().ice()? {
            Constant(_) => self.compile_call_constant(item),
            Variable(var) => {
                self.compile_call_args(item, FunctionKind::Function)?;
                self.compile_variable(var)?;
                self.write_call_dynamic(var.type_id(self).ice()?)?;
                Ok(())
            },
            BinaryOp(binary_op) => {
                self.compile_call_args(item, FunctionKind::Function)?;
                self.compile_binary_op(binary_op)?;
                self.write_call_dynamic(binary_op.type_id(self).ice()?)?;
                Ok(())
            },
            _ => Self::ice_at(item, "Invalid call operand")?,
        }
    }

    /// Compiles the given statically resolved call.
    fn compile_call_constant(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        let constant = item.left.as_expression().ice()?.as_constant().ice()?;
        comment!(self, "prepare {}() args", constant.path);
        let function_id = self.resolved.constant(constant.constant_id.ice()?).value.as_function_id().ice()?;
        let function_kind = self.resolved.function(function_id).kind.ice()?;
        match function_kind {
            FunctionKind::Rust(rust_fn_index) => {
                self.compile_call_args(item, function_kind)?;
                comment!(self, "call {}()", constant.path);
                self.writer.call_rust(T::from_index(rust_fn_index));
            },
            FunctionKind::Builtin(type_id, builtin_type) => {
                self.compile_call_args(item, function_kind)?;
                comment!(self, "call {}()", constant.path);
                self.write_builtin(self.ty(&type_id), builtin_type)?;
            },
            FunctionKind::Method(object_type_id) => {
                self.compile_call_args(item, function_kind)?;
                if self.ty(&object_type_id).as_trait().is_some() {
                    // dynamic dispatch
                    let function_offset = self.vtable_function_offset(function_id)?;
                    let function_arg_size = self.resolved.function(function_id).arg_size(self);
                    comment!(self, "call {}()", constant.path);
                    self.writer.call_virtual(function_offset, function_arg_size);
                } else {
                    // static dispatch
                    comment!(self, "call {}()", constant.path);
                    self.write_call(function_id);
                }
            },
            FunctionKind::Function => {
                self.compile_call_args(item, function_kind)?;
                comment!(self, "call {}()", constant.path);
                self.write_call(function_id);
            },
            FunctionKind::Variant(type_id, variant_index) => {
                let index_type = Type::unsigned(size_of::<VariantIndex>());
                self.write_immediate(&index_type, Numeric::Unsigned(variant_index as u64))?;
                self.compile_call_args(item, function_kind)?;
                let arg_size = self.resolved.function(function_id).arg_size(self) as StackAddress;
                self.writer.upload(arg_size + index_type.primitive_size() as StackAddress, *self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
            },
        }
        Ok(())
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_let_binding(self: &mut Self, item: &ast::LetBinding) -> CompileResult {
        let binding_id = item.binding_id;
        self.init_state.declare(binding_id);
        if let Some(expr) = &item.expr {
            comment!(self, "let {} = ...", item.ident.name);
            // optimization: write literal unless it is zero and we're in the root block of a function (stackframe is already zero-initialized)
            if !(expr.is_zero_literal() && self.init_state.len() == 2) {
                self.compile_expression(expr)?;
                let local = self.locals.lookup(binding_id);
                self.write_store(self.ty(item), local, Some(binding_id))?;
            }
            self.init_state.initialize(binding_id);
        }
        Ok(())
    }

    /// Compiles the given variable.
    fn compile_variable(self: &mut Self, item: &ast::Variable) -> CompileResult {
        comment!(self, "variable {}", item);
        let load_index = self.load_variable(item)?;
        self.write_load(self.ty(item), load_index)?;
        Ok(())
    }

    /// Compiles the given constant.
    fn compile_constant(self: &mut Self, item: &ast::Constant) -> CompileResult {
        comment!(self, "constant {}", item);
        let constant_id = item.constant_id.ice_msg("Unresolved constant encountered")?;
        let constant = self.resolved.constant(constant_id);

        match constant.value {
            ConstantValue::Function(function_id) => {
                let function_addr = self.functions.register_call(function_id, self.writer.position(), true);
                // load function address
                comment!(self, "\npush @{}", item.path);
                self.writer.immediate64(function_addr as u64); // TODO: depends on stack address size but may not use small immediate optimization
                // load constructor (none)
                self.writer.immediate64(0 as u64); // TODO: immediate_sa bla
                // upload both into heap object (this is for compatibility with closures and unfortunate overkill for anonymous functions)
                self.writer.upload(size_of::<StackAddress>() * 2, 0);
            },
            ConstantValue::Numeric(numeric) => {
                let ty = self.ty(item);
                self.write_immediate(ty, numeric)?;
            },
        }
        Ok(())
    }

    /// Compiles an if block without else part.
    fn compile_if_only_block(self: &mut Self, item: &ast::IfBlock) -> CompileResult {
        let exit_jump = self.writer.j0(123);
        self.init_state.push(BranchingKind::Double, BranchingScope::Block);
        let result = self.compile_block(&item.if_block);
        self.init_state.pop();
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
        result
    }

    /// Compiles an if+else block.
    fn compile_if_else_block(self: &mut Self, if_block: &ast::Block, else_block: &ast::Block) -> CompileResult {

        let else_jump = self.writer.j0(123);
        self.init_state.push(BranchingKind::Double, BranchingScope::Block);
        self.init_state.set_path(BranchingPath::A);
        let result = self.compile_block(if_block);
        let exit_jump = if if_block.control_flow() != Some(ast::ControlFlowType::Return) {
            Some(self.writer.jmp(123))
        } else {
            None
        };

        comment!(self, "else");
        let else_target = self.writer.position();
        self.init_state.set_path(BranchingPath::B);
        self.compile_block(else_block)?;
        self.init_state.pop();

        let exit_target = self.writer.position();

        // go back and fix jump targets
        self.writer.overwrite(else_jump, |w| w.j0(else_target));
        if let Some(exit_jump) = exit_jump {
            self.writer.overwrite(exit_jump, |w| w.jmp(exit_target));
        }

        result
    }

    /// Compiles the given if block.
    fn compile_if_block(self: &mut Self, item: &ast::IfBlock) -> CompileResult {

        // compile condition and jump placeholder
        comment!(self, "{}", item);
        self.compile_expression(&item.cond)?;

        if item.else_block.is_none() {
            self.compile_if_only_block(item)
        } else {
            self.compile_if_else_block(&item.if_block, item.else_block.as_ref().ice()?)
        }
    }

    /// Write match arm block code.
    fn compile_match_arm(self: &mut Self, exit_jumps: &mut Vec<StackAddress>, block: &ast::Block) -> CompileResult {
        self.compile_block(block)?;
        if block.control_flow() != Some(ast::ControlFlowType::Return) {
            exit_jumps.push(self.writer.jmp(123));
        }
        Ok(())
    }

    /// Split match block into recursive tree of A/B branches.
    fn compile_match_block_recursive(self: &mut Self, exit_jumps: &mut Vec<StackAddress>, remaining_branches: &[(ast::Pattern, ast::Block)]) -> CompileResult {
        if remaining_branches.len() == 1 {
            self.init_state.push(BranchingKind::Single, BranchingScope::Block); // patterns are required to be exhaustive
            self.compile_match_arm(exit_jumps, &remaining_branches[0].1)?;
            self.init_state.pop();
        } else if remaining_branches.len() > 1 {
            self.init_state.push(BranchingKind::Double, BranchingScope::Block);
            self.init_state.set_path(BranchingPath::A);
            self.compile_match_arm(exit_jumps, &remaining_branches[0].1)?;
            self.init_state.set_path(BranchingPath::B);
            self.compile_match_block_recursive(exit_jumps, &remaining_branches[1..])?;
            self.init_state.pop();
        }
        Ok(())
    }

    /// Compiles a match block.
    fn compile_match_block(self: &mut Self, item: &ast::MatchBlock) -> CompileResult {
        comment!(self, "{}", item);
        self.compile_expression(&item.expr)?;
        let mut exit_jumps = Vec::new();
        self.compile_match_block_recursive(&mut exit_jumps, &item.branches)?;
        // fix branch jump outs
        let exit_target = self.writer.position();
        while let Some(exit_jump) = exit_jumps.pop() {
            self.writer.overwrite(exit_jump, |w| w.jmp(exit_target));
        }
        Ok(())
    }

    /// Compiles a while loop.
    fn compile_while_loop(self: &mut Self, item: &ast::WhileLoop) -> CompileResult {
        comment!(self, "{}", item);
        let start_target = self.writer.position();
        self.compile_expression(&item.expr)?;
        let exit_jump = self.writer.j0(123);
        self.init_state.push(BranchingKind::Single, BranchingScope::Loop);
        self.loop_control.push();
        self.compile_block(&item.block)?;
        let loop_controls = self.loop_control.pop();
        self.init_state.pop();
        self.writer.jmp(start_target);
        // fix jump addresses
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
        for loop_control in &loop_controls {
            match loop_control {
                &LoopControl::Break(addr) => self.writer.overwrite(addr, |w| w.jmp(exit_target)),
                &LoopControl::Continue(addr) => self.writer.overwrite(addr, |w| w.jmp(start_target)),
            };
        }
        Ok(())
    }

    fn compile_for_loop_range(self: &mut Self, item: &ast::ForLoop, iter_loc: FrameAddress, iter_type_id: TypeId) -> CompileResult {
        comment!(self, "for in range");
        // store lower range bound in iter variable
        let binary_op = item.expr.as_binary_op().ice()?;
        self.compile_expression(binary_op.left.as_expression().ice()?)?;          // stack current=lower
        self.write_store(self.ty(&iter_type_id), iter_loc, None)?;                      // stack -         // TODO this doesn't do a storex, why does it work anyways?
        // push upper range bound
        self.compile_expression(binary_op.right.as_expression().ice()?)?;             // stack upper
        // precheck bounds
        let skip_jump = if binary_op.op == ast::BinaryOperator::Range {
            // non-inclusive range: check if lower bound greater than or equal to upper bound. also note, while is always inclusive, so we have to subtract 1 from upper bound
            let iter_ty = self.ty(&iter_type_id);
            self.write_clone(iter_ty);                                   // stack: upper upper
            self.write_load(iter_ty, iter_loc)?;                   // stack: upper upper lower
            self.write_lte(iter_ty)?;                                         // stack: upper upper_lte_lower
            let skip_jump = self.writer.jn0(123);                // stack: upper
            self.write_sub_pi(iter_ty, Numeric::Unsigned(1))?;                                        // stack: upper=upper-1
            skip_jump
        } else {
            // inclusive range: check if lower bound greater than upper bound.
            let iter_ty = self.ty(&iter_type_id);
            self.write_clone(iter_ty);                                   // stack: upper upper
            self.write_load(iter_ty, iter_loc)?;                   // stack: upper upper lower
            self.write_lt(iter_ty)?;                                         // stack: upper upper_lte_lower
            self.writer.jn0(123)                                    // stack: upper
        };
        // compile block
        let start_target = self.writer.position();              // stack: upper
        self.loop_control.push();
        self.compile_block(&item.block)?;
        let loop_controls = self.loop_control.pop();
        // load bounds, increment and compare
        let iter_ty = self.ty(&iter_type_id);
        //let increment_target = self.write_while(iter_loc as FrameOffset, -(iter_ty.primitive_size() as FrameOffset), start_target, iter_ty);    // stack: upper
        let increment_target = self.write_clone(iter_ty);       // stack upper upper
        self.write_preinc(iter_ty, iter_loc)?;      // stack upper upper new_current(=current+1)
        self.write_lt(iter_ty)?;                                                    // stack upper new_current>upper
        self.writer.j0(start_target);                                              // stack upper        // fix jump addresses
        // exit loop
        let exit_target = self.writer.position();
        self.writer.overwrite(skip_jump, |w| w.jn0(exit_target));
        for loop_control in &loop_controls {
            match loop_control {
                &LoopControl::Break(addr) => self.writer.overwrite(addr, |w| w.jmp(exit_target)),
                &LoopControl::Continue(addr) => self.writer.overwrite(addr, |w| w.jmp(increment_target)),
            };
        }
        // discard counter
        self.write_discard(iter_ty)?;
        Ok(())
    }

    fn compile_for_loop_array(self: &mut Self, item: &ast::ForLoop, element_loc: FrameAddress, element_type_id: TypeId) -> CompileResult {
        comment!(self, "for in array");

        self.compile_expression(&item.expr)?;                       // stack &array
        let array_ty = self.ty(&item.expr);
        self.write_cnt(array_ty, true, HeapRefOp::Inc)?;
        self.write_clone_ref();                                         // stack &array &array
        self.write_builtin(array_ty, BuiltinType::Array(builtin_types::Array::len))?;             // stack &array len
        let exit_jump = self.writer.j0sa_nc(123);
        let loop_start = self.write_sub_pi(&STACK_ADDRESS_TYPE, Numeric::Unsigned(1))?;        // stack &array index (indexing from the end to be able to count downwards from len)

        let element_ty = self.ty(&element_type_id);
        self.write_heap_tail_element_nc(array_ty, element_ty)?;   // stack &array index element

        if element_ty.is_ref() {
            self.write_clone(element_ty);                                   // stack &array index element element
            self.write_cnt(element_ty, true, HeapRefOp::Inc)?;
        }

        self.write_store(element_ty, element_loc, None)?;                        // stack &array index <is_ref ? element>  // TODO: this doesn't do storex, why does it work?
        self.loop_control.push();
        self.compile_block(&item.block)?;                           // stack &array index <is_ref ? element>
        let loop_controls = self.loop_control.pop();

        let element_ty = self.ty(&element_type_id);

        let cnt_target = if element_ty.is_ref() {
            Some(self.write_cnt(element_ty, false, HeapRefOp::Dec)?)
        } else {                                                          // stack &array index
            None
        };

        let cond_target = self.writer.jn0sa_nc(loop_start);

        // if the loop contains a break we need a target for the break jump that decrefs the potential element
        // reference but doesn't jump back to the start of the loop. the normal loop exit has to jump over this.
        let break_target = if element_ty.is_ref() && loop_controls.iter().find(|&lc| matches!(lc, LoopControl::Break(_))).is_some() {
            let exit_skip_target = self.writer.jmp(123);
            let break_target = self.write_cnt(element_ty, false, HeapRefOp::Dec)?;
            let exit_target = self.writer.position();
            self.writer.overwrite(exit_skip_target, |w| w.jmp(exit_target));
            Some(break_target)
        } else {
            None
        };

        // fix jump addresses
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0sa_nc(exit_target));
        for loop_control in &loop_controls {
            match loop_control {
                &LoopControl::Break(addr) => self.writer.overwrite(addr, |w| w.jmp(break_target.unwrap_or(exit_target))),
                &LoopControl::Continue(addr) => self.writer.overwrite(addr, |w| w.jmp(cnt_target.unwrap_or(cond_target))),
            };
        }
        // discard counter
        self.write_discard(&STACK_ADDRESS_TYPE)?;    // stack &array
        self.write_cnt(self.ty(&item.expr), false, HeapRefOp::Dec)?;         // stack --
        Ok(())
    }

    /// Compiles a for - in loop
    fn compile_for_loop(self: &mut Self, item: &ast::ForLoop) -> CompileResult {
        use ast::{Expression::*, BinaryOperator as Op};
        // initialize iter variable
        let binding_id = item.iter.binding_id;
        self.init_state.push(BranchingKind::Single, BranchingScope::Loop);
        let iter_local = self.locals.lookup(binding_id);
        self.init_state.initialize(binding_id);
        let iter_type_id = item.iter.type_id(self).ice()?;
        // handle ranges or arrays
        let result = match &item.expr { // NOTE: these need to match Resolver::resolve_for_loop
            BinaryOp(bo) if bo.op == Op::Range || bo.op == Op::RangeInclusive => {
                self.compile_for_loop_range(item, iter_local, iter_type_id)
            },
            Literal(_) | Constant(_) | Variable(_) | Block(_) | IfBlock(_) | MatchBlock(_)  => {
                self.compile_for_loop_array(item, iter_local, iter_type_id)
            },
            _ => Self::ice_at(item, "Invalid for loop expression"),

        };
        self.init_state.pop();
        result
    }

    /// Compiles an anonymous function.
    fn compile_anonymous_function(self: &mut Self, item: &ast::Function) -> CompileResult {
        // write code to skip over function instructions
        comment!(self, "skip inlined {} body", item.shared.sig.ident.name);
        let function_skip_jump = self.writer.jmp(123);
        let function_addr = self.writer.position();
        self.compile_function(item)?;
        let function_done_jump = self.writer.position();
        self.writer.overwrite(function_skip_jump, |w| w.jmp(function_done_jump));
        // load function address
        comment!(self, "\npush @{}", item.shared.sig.ident.name);
        self.writer.immediate64(function_addr as u64); // TODO: depends on stack address size but may not use small immediate optimization
        // load constructor (none)
        self.writer.immediate64(0 as u64); // TODO: immediate_sa bla
        // upload both into heap object (this is for compatibility with closures and unfortunate overkill for anonymous functions)
        self.writer.upload(size_of::<StackAddress>() * 2, 0);
        Ok(())
    }

    /// Compiles the given closure.
    fn compile_closure(self: &mut Self, item: &ast::Closure) -> CompileResult {
        // write code to skip over function instructions
        comment!(self, "skip inlined {} body", item.shared.sig.ident.name);
        let function_skip_jump = self.writer.jmp(123);
        let function_addr = self.writer.position();
        self.compile_function_inner(&item.shared, item.function_id.ice()?, &item.required_bindings)?;
        let function_done_jump = self.writer.position();
        self.writer.overwrite(function_skip_jump, |w| w.jmp(function_done_jump));
        // load captures
        comment!(self, "\ncapture variables for {}", item.shared.sig.ident.name);
        let mut size = 0;
        for &capture_binding_id in item.required_bindings.iter() {
            let loc = self.locals.lookup(capture_binding_id);
            let type_id = self.binding_by_id(capture_binding_id).type_id.ice()?;
            let ty = self.ty(&type_id);
            size += ty.primitive_size() as StackAddress;
            self.write_load(ty, loc)?;
        }
        // load function address
        self.writer.immediate64(function_addr as u64); // TODO: depends on stack address size but may not use small immediate optimization, implement immediate_sa
        size += size_of::<StackAddress>();
        // load constructor
        let constructor = self.constructor(self.ty(&item.struct_type_id))?;
        self.writer.immediate64(constructor as u64); // TODO: immediate_sa bla
        size += size_of::<StackAddress>();
        // upload captures, address and constructor into heap object
        comment!(self, "push @{}", item.shared.sig.ident.name);
        self.writer.upload(size as StackAddress, 0);
        Ok(())
    }

    /// Compiles the given function.
    fn compile_function(self: &mut Self, item: &ast::Function) -> CompileResult {
        let function_id = self.constant_by_id(item.constant_id.ice()?).value.as_function_id().ice()?;
        let captures = Vec::new();
        self.compile_function_inner(&item.shared, function_id, &captures)
    }

    /// Compiles the given function.
    fn compile_function_inner(self: &mut Self, item: &ast::FunctionShared, function_id: FunctionId, closure_captures: &[ BindingId ]) -> CompileResult {

        // register function bytecode index
        let position = self.writer.position();
        comment!(self, "\nfn {}", item.sig.ident.name);

        // create arguments in local environment
        self.init_state.push(BranchingKind::Single, BranchingScope::Function);
        let mut frame = StackFrame::new();
        frame.ret_size = item.sig.ret.as_ref().map_or(0, |ret| self.ty(ret).primitive_size());
        for arg in item.sig.args.iter() {
            frame.insert(arg.binding_id, frame.arg_pos);
            self.init_state.declare(arg.binding_id);
            self.init_state.initialize(arg.binding_id);
            frame.arg_pos += self.ty(arg).primitive_size() as FrameAddress;
        }

        // clone closed over variables
        for &binding_id in closure_captures {
            frame.insert(binding_id, frame.arg_pos);
            self.init_state.declare(binding_id);
            self.init_state.initialize(binding_id);
            let capture_type = self.binding_by_id(binding_id).type_id.ice()?;
            frame.arg_pos += self.ty(&capture_type).primitive_size() as FrameAddress;
        }

        // create variables in local environment and reserve space on the stack
        frame.var_pos = frame.arg_pos + size_of::<StackAddress>() as FrameAddress * 2; // space for prev fp and prev pc
        self.create_stack_frame_block(item.block.as_ref().ice()?, &mut frame)?;
        let var_size = frame.var_pos - (frame.arg_pos + size_of::<StackAddress>() as FrameAddress * 2);
        if var_size > 0 {
            self.writer.reserve(var_size as FrameAddress);
        }

        // store call info required to compile calls to this function and fix calls that were made before the function address was known
        let arg_size = frame.arg_pos;
        if let Some(calls) = self.functions.register_function(function_id, position) {
            let backup_position = self.writer.position();
            for &(call_address, load_only) in calls.iter() {
                self.writer.set_position(call_address);
                if load_only {
                    self.writer.immediate64(position as u64); // TODO: depends on stack address size but may not use small immediate optimization
                } else {
                    self.writer.call(position, arg_size);
                }
            }
            self.writer.set_position(backup_position);
        }

        // push local environment on the locals stack so that it is accessible from nested compile_*
        self.locals.push(frame);
        self.compile_block(item.block.as_ref().ice()?)?;
        self.locals.pop();
        self.init_state.pop();
        Ok(())
    }

    /// Compiles the given block.
    fn compile_block(self: &mut Self, item: &ast::Block) -> CompileResult {
        self.init_state.push(BranchingKind::Single, BranchingScope::Block);
        for statement in item.statements.iter() {
            self.compile_statement(statement)?;
        }
        if let Some(result) = &item.result {
            comment!(self, "block resulting");
            self.compile_expression(result)?;
            // inc result, then dec everything
            self.write_cnt(self.ty(result), true, HeapRefOp::Inc)?;
            self.write_scope_destructor(BranchingScope::Block)?;
            self.write_cnt(self.ty(result), true, HeapRefOp::DecNoFree)?;
        } else if item.control_flow() == None {
            comment!(self, "block ending");
            self.write_scope_destructor(BranchingScope::Block)?;
        }

        // decrease local variable ref-count
        self.init_state.pop();
        Ok(())
    }

    /// Compiles the given literal
    fn compile_literal(self: &mut Self, item: &ast::Literal) -> CompileResult {
        use crate::frontend::ast::LiteralValue;
        comment!(self, "{}", item);
        let ty = self.ty(item);
        match &item.value {
            LiteralValue::Void => { },
            LiteralValue::Numeric(numeric) => { self.write_immediate(ty, *numeric)?; }
            LiteralValue::Bool(v) =>  {
                match ty {
                    Type::bool => { if *v { self.writer.immediate8(1); } else { self.writer.immediate8(0); } },
                    _ => Self::ice_at(item, "Unexpected boolean literal type")?,
                };
            },
            /*LiteralValue::Variant(ref variant) if ty.as_enum().map_or(false, |e| e.primitive.is_some()) => {
                // primitive enums don't need to be heap allocated
                let enum_def = ty.as_enum().or_ice_msg("Non-enum type on enum variant")?;
                let enum_ty = self.ty(&enum_def.primitive.or_ice()?.0);
                let variant_value = enum_def.variant_value(&variant.ident.name).or_ice()?;
                self.write_immediate(enum_ty, variant_value)?;
            },*/
            LiteralValue::String(ref string_literal) => {
                // store string on const pool and write instruction to load it from const pool directly onto th heap
                // note: we get the offset from current const_len() and not the store_const() result as that points to
                // the start of the string  while the we neeed to point at the meta data it writes before the string (the length)
                let string_const = self.writer.const_len();
                self.writer.store_const(string_literal.as_str());
                self.writer.upload_const(string_const);
            }
            LiteralValue::Array(array_literal) => {
                // write instructions to construct the array on the stack and once complete, upload it to the heap
                for element in &array_literal.elements {
                    self.compile_expression(element)?;
                }
                let array_ty = self.ty(item).as_array().ice_msg("Expected array type, got something else")?;
                let size = array_literal.elements.len() as StackAddress * self.ty(&array_ty.type_id).primitive_size() as StackAddress;
                let type_id = item.type_id(self).ice()?;
                self.writer.upload(size, *self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
            },
            LiteralValue::Struct(struct_literal) => {
                // write instructions to construct the struct on the stack and once complete, upload it to the heap
                let struct_def = ty.as_struct().ice_msg("Expected struct, got something else")?;
                // collect fields first to avoid borrow checker
                let fields: Vec<_> = struct_def.fields.iter().map(|(name, _)| struct_literal.fields.get(name).expect("Missing struct field")).collect();
                for field in fields {
                    self.compile_expression(field)?;
                }
                let struct_ty = self.ty(item).as_struct().ice_msg("Expected struct type, got something else")?;
                let size = struct_ty.fields.iter().fold(0, |acc, f| acc + self.ty(f.1).primitive_size() as StackAddress);
                let type_id = item.type_id(self).ice()?;
                self.writer.upload(size, *self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
            },
            /*LiteralValue::Variant(variant) => {
                // write instructions to construct the data-variant enum on the stack and once complete, upload it to the heap
                let enum_def = self.ty(item).as_enum().or_ice_msg("Encountered non-enum type on enum variant")?;
                let index_type = Type::unsigned(size_of::<VariantIndex>());
                let variant_index = enum_def.variant_index(&variant.ident.name).or_ice()?;
                self.write_immediate(&index_type, Numeric::Unsigned(variant_index as u64))?;
                let size = index_type.primitive_size() as StackAddress;
                let type_id = item.type_id(self).or_ice()?;
                self.writer.upload(size, *self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
            },*/
        }
        Ok(())
    }

    /// Compiles the given unary operation.
    fn compile_unary_op(self: &mut Self, item: &ast::UnaryOp) -> CompileResult {
        use crate::frontend::ast::UnaryOperator as UO;
        match item.op {
            // logical
            UO::Not => {
                self.compile_expression(&item.expr)?;
                comment!(self, "{}", item);
                self.writer.not();
            }
            // arithmetic
            UO::Plus => { /* nothing to do */ }
            UO::Minus => {
                self.compile_expression(&item.expr)?;
                comment!(self, "negate");
                let item_type = self.ty(&item.expr);
                self.write_neg(item_type)?;
            }
        }
        Ok(())
    }

    /// Compiles a simple, non-shortcutting binary operation.
    fn compile_binary_op_simple(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        // compile left, right and store references, left and right will be consumed
        self.compile_expression(item.left.as_expression().ice()?)?; // stack: left
        comment!(self, "{}", item.op);
        self.compile_expression(item.right.as_expression().ice()?)?; // stack: left right
        let ty_result = self.ty(item);
        let ty_left = self.ty(&item.left);
        match item.op {                             // stack: result
            // arithmetic/concat
            BO::Add => self.write_add(ty_result)?,
            BO::Sub => self.write_sub(ty_result)?,
            BO::Mul => self.write_mul(ty_result)?,
            BO::Div => self.write_div(ty_result)?,
            BO::Rem => self.write_rem(ty_result)?,
            // comparison
            BO::Greater     => self.write_gt(ty_left)?,
            BO::GreaterOrEq => self.write_gte(ty_left)?,
            BO::Less        => self.write_lt(ty_left)?,
            BO::LessOrEq    => self.write_lte(ty_left)?,
            BO::Equal       => self.write_eq(ty_left)?,
            BO::NotEqual    => self.write_neq(ty_left)?,
            _ => Self::ice_at(item, "Invalid simple-operation")?,
        };
        Ok(())
    }

    /// Compiles a short-circuiting binary operation (and/or)
    fn compile_binary_op_shortcircuiting(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        match item.op {
            BO::And => {
                self.compile_expression(item.left.as_expression().ice()?)?;
                let exit_jump = self.writer.j0_nc(123); // left is false, result cannot ever be true, skip right
                self.compile_expression(item.right.as_expression().ice()?)?;
                self.writer.and();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.j0_nc(exit_target));
            },
            BO::Or => {
                self.compile_expression(item.left.as_expression().ice()?)?;
                let exit_jump = self.writer.jn0_nc(123); // left is true, result cannot ever be false, skip right
                self.compile_expression(item.right.as_expression().ice()?)?;
                self.writer.or();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.jn0_nc(exit_target));
            },
            _ => Self::ice_at(item, "Invalid shortcircuit-operation")?,
        }
        Ok(())
    }

    /// Compiles an offsetting binary operation, i.e.. indexing and member access.
    fn compile_binary_op_offseting(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        use crate::frontend::ast::BinaryOperator::*;
        self.compile_expression(item.left.as_expression().ice()?)?;
        if item.op == Index || item.op == IndexWrite {
            self.compile_expression(item.right.as_expression().ice()?)?;
        }
        let result_type = self.ty(item);
        let compare_type = self.ty(&item.left);
        match item.op {
            Index => {
                comment!(self, "[{}]", &item.right);
                // fetch heap value at reference-target
                self.write_heap_fetch_element(compare_type, result_type)?;
            },
            IndexWrite => {
                comment!(self, "[{}] (writing)", &item.right);
                // compute and push the address of the reference-target
                self.writer.index(result_type.primitive_size());
            },
            Access => {
                comment!(self, ".{}", &item.right);
                // fetch heap value at reference-target
                let struct_ = compare_type.as_struct().ice()?;
                let offset = self.compute_member_offset(struct_, &item.right.as_member().ice()?.ident.name);
                self.write_heap_fetch_member(compare_type, result_type, offset)?;
            },
            AccessWrite => {
                comment!(self, ".{} (writing)", &item.right);
                // compute and push the address of the reference-target
                let struct_ = compare_type.as_struct().ice()?;
                let offset = self.compute_member_offset(struct_, &item.right.as_member().ice()?.ident.name);
                self.write_member_offset(offset);

            },
            _ => Self::ice_at(item, "Invalid offset-operation")?,

        }
        Ok(())
    }

    /// Compiles the given binary operation.
    fn compile_binary_op(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        use ast::{BinaryOperand as Operand, BinaryOperator::*, Expression as E};
        let exprs = match (&item.left, &item.right) {
            (Operand::Expression(left), Operand::Expression(right)) => Some((left, right)),
            _ => None,
        };
        match (item.op, exprs) {
            // TODO: move to future optimizer. here to test whether these opcodes are even beneficial
            // Multiplication optimizations
            (Mul, Some((E::Variable(vl), E::Variable(vr)))) => {
                let addr_l = self.load_variable(vl)?;
                let addr_r = self.load_variable(vr)?;
                self.write_mul_vv(self.ty(&item.left), addr_l, addr_r)?;
                Ok(())
            },
            (Mul, Some((left, E::Variable(right)))) |
            (Mul, Some((E::Variable(right), left))) if self.ty(&item.left).is_numeric() => {
                self.compile_expression(left)?;
                let addr_r = self.load_variable(right)?;
                self.write_mul_pv(self.ty(&item.left), addr_r)?;
                Ok(())
            },
            (Mul, Some((left, E::Literal(right)))) |
            (Mul, Some((E::Literal(right), left))) if self.ty(right).is_numeric() => {
                self.compile_expression(left)?;
                let numeric = right.value.as_numeric().ice()?;
                self.write_mul_pi(self.ty(&item.left), numeric)?;
                Ok(())
            },
            // Addition optimizations
            (Add, Some((E::Variable(vl), E::Variable(vr)))) if self.ty(&item.left).is_numeric() => {
                let addr_l = self.load_variable(vl)?;
                let addr_r = self.load_variable(vr)?;
                self.write_add_vv(self.ty(&item.left), addr_l, addr_r)?;
                Ok(())
            },
            (Add, Some((left, E::Variable(right)))) |
            (Add, Some((E::Variable(right), left))) if self.ty(&item.left).is_numeric() => {
                self.compile_expression(left)?;
                let addr_r = self.load_variable(right)?;
                self.write_add_pv(self.ty(&item.left), addr_r)?;
                Ok(())
            },
            (Add, Some((left, E::Literal(right)))) |
            (Add, Some((E::Literal(right), left))) if self.ty(right).is_numeric() => {
                self.compile_expression(left)?;
                let numeric = right.value.as_numeric().ice()?;
                self.write_add_pi(self.ty(&item.left), numeric)?;
                Ok(())
            },
            // Subtraction optimizations
            (Sub, Some((E::Variable(vl), E::Variable(vr)))) if self.ty(&item.left).is_numeric() => {
                let addr_l = self.load_variable(vl)?;
                let addr_r = self.load_variable(vr)?;
                self.write_sub_vv(self.ty(&item.left), addr_l, addr_r)?;
                Ok(())
            },
            (Sub, Some((left, E::Variable(right)))) if self.ty(&item.left).is_numeric() => {
                self.compile_expression(left)?;
                let addr_r = self.load_variable(right)?;
                self.write_sub_pv(self.ty(&item.left), addr_r)?;
                Ok(())
            },
            (Sub, Some((left, E::Literal(right)))) if self.ty(right).is_numeric() => {
                self.compile_expression(left)?;
                let numeric = right.value.as_numeric().ice()?;
                self.write_sub_pi(self.ty(&item.left), numeric)?;
                Ok(())
            },
            // General operations
            (Add | Sub | Mul | Div | Rem | Less | Greater | LessOrEq | GreaterOrEq | Equal | NotEqual, _) => {
                self.compile_binary_op_simple(item)
            },
            (And | Or, _) => {
                self.compile_binary_op_shortcircuiting(item)
            },
            (Index | IndexWrite | Access | AccessWrite, _) => {
                self.compile_binary_op_offseting(item)
            },
            (Cast, _) => {
                self.compile_binary_op_cast(item)
            },
            (Call, _) => {
                self.compile_binary_op_call(item)
            },
            (Assign | AddAssign | SubAssign | MulAssign | DivAssign | RemAssign | Range | RangeInclusive, _) => {
                Self::ice_at(item, "Unexpected operator in compile_binary_op")
            },
        }
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_binary_op_cast(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        let expr = item.left.as_expression().ice()?;
        self.compile_expression(expr)?;
        let from = self.ty(expr);
        let to = self.ty(item.right.as_type_name().ice()?);
        self.write_cast(from, to)?;
        Ok(())
    }

}

impl<T> Compiler<T> where T: VMFunc<T> {

    /// Returns the type of given AST item.
    fn ty(self: &Self, item: &impl Typeable) -> &Type {
        match item.type_id(self) {
            None => panic!("Unresolved type encountered."),
            Some(type_id) => self.type_by_id(type_id)
        }
    }

    /// Returns constructor index for given type or 0.
    pub(super) fn constructor(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        if ty.is_callable() { // TODO unify cases, consider always storing the concrete constuctor as meta information on the heap object (like currently the implementor index)
            Ok(1) // callables use constructor offset 1 to indicate the actual offset is stored at the end of the heap object
        } else {
            let type_id = self.resolved.types().find(|m| m.1 == ty).ice()?.0;
            Ok(*self.constructors.get(&type_id).unwrap_or(&0)) // offset 0 means to look into the heap object's implementor index
        }
    }

    /// Generates an internal compiler error at the given item. Message should not end in a ".".
    fn ice_at<R>(item: &dyn Positioned, message: &str) -> CompileResult<R> {
        #[cfg(feature="ice_panics")]
        panic!("Internal compiler error: {} at position {:?}.", message, item.position());
        #[cfg(not(feature="ice_panics"))]
        Err(CompileError::new(item, CompileErrorKind::Internal(format!("{message}.")), ""))
    }

    /// Generates an internal compiler error.
    fn ice<R>(message: &str) -> CompileResult<R> {
        #[cfg(feature="ice_panics")]
        panic!("Internal compiler error: {}", message);
        #[cfg(not(feature="ice_panics"))]
        Err(CompileError::ice(message.to_string()))
    }

    /// Checks if the given functions are compatible.
    fn is_compatible_function(self: &Self, target: &Function, other: &Function) -> CompileResult<bool> {
        if discriminant(&target.kind.ice()?) != discriminant(&other.kind.ice()?) {
            return Ok(false);
        }
        let target_type = self.ty(&target.signature_type_id).as_callable().ice()?;
        let other_type = self.ty(&other.signature_type_id).as_callable().ice()?;
        if target_type.ret_type_id != other_type.ret_type_id {
            return Ok(false);
        }
        if target_type.arg_type_ids.len() != other_type.arg_type_ids.len() {
            return Ok(false);
        }
        for (target_arg, other_arg) in target_type.arg_type_ids.iter().zip(other_type.arg_type_ids.iter()) {
            if !self.type_accepted_for(other_arg.ice()?, target_arg.ice()?) {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Computes struct member offset in bytes.
    fn compute_member_offset(self: &Self, struct_: &Struct, member_name: &str) -> StackAddress {
        let mut offset = 0;
        for (field_name, field_type_id) in struct_.fields.iter() {
            if field_name == member_name {
                break;
            }
            let field_type = self.ty(field_type_id);
            // use reference size for references, otherwise shallow type size
            offset += field_type.primitive_size() as StackAddress;
        }
        offset
    }

    /// Creates stack frame variables for expressions. // FIXME move this into AST
    fn create_stack_frame_exp(self: &Self, expression: &ast::Expression, frame: &mut StackFrame) -> CompileResult {
        if let ast::Expression::Block(block) = expression {
            self.create_stack_frame_block(block, frame)?;
        } else if let ast::Expression::Assignment(assignment) = expression {
            if let ast::Expression::Block(block) = &assignment.right {
                self.create_stack_frame_block(block, frame)?;
            }
        } else if let ast::Expression::BinaryOp(binary_op) = expression {
            if let ast::BinaryOperand::Expression(ast::Expression::Block(block)) = &binary_op.left {
                self.create_stack_frame_block(block, frame)?;
            }
            if binary_op.op == ast::BinaryOperator::Call {
                for arg in &binary_op.right.as_argument_list().ice()?.args {
                    if let ast::Expression::Block(block) = arg {
                        self.create_stack_frame_block(block, frame)?;
                    }
                }
            } else {
                if let ast::BinaryOperand::Expression(ast::Expression::Block(block)) = &binary_op.right {
                    self.create_stack_frame_block(block, frame)?;
                }
            }
        } else if let ast::Expression::UnaryOp(unary_op) = expression {
            if let ast::Expression::Block(block) = &unary_op.expr {
                self.create_stack_frame_block(block, frame)?;
            }
        } else if let ast::Expression::IfBlock(if_block) = expression {
            self.create_stack_frame_block(&if_block.if_block, frame)?;
            if let Some(block) = &if_block.else_block {
                self.create_stack_frame_block(block, frame)?;
            }
        }
        Ok(())
    }

    /// Creates stack frame variables for blocks. // FIXME move this into AST
    fn create_stack_frame_block(self: &Self, item: &ast::Block, frame: &mut StackFrame) -> CompileResult {
        // todo: this is pretty bad. need to come up with better solution. trait on ast?
        for statement in item.statements.iter() {
            if let ast::Statement::LetBinding(binding) = statement {
                frame.insert(binding.binding_id, frame.var_pos);
                frame.var_pos += self.ty(binding).primitive_size() as FrameAddress;
                if let Some(expression) = &binding.expr {
                    self.create_stack_frame_exp(expression, frame)?;
                }
            } else if let ast::Statement::ForLoop(for_loop) = statement {
                frame.insert(for_loop.iter.binding_id, frame.var_pos);
                frame.var_pos += self.ty(&for_loop.iter).primitive_size() as FrameAddress;
                self.create_stack_frame_block(&for_loop.block, frame)?;
            } else if let ast::Statement::WhileLoop(while_loop) = statement {
                self.create_stack_frame_block(&while_loop.block, frame)?;
            } else if let ast::Statement::Block(block) = statement {
                self.create_stack_frame_block(&block, frame)?;
            } else if let ast::Statement::IfBlock(if_block) = statement {
                self.create_stack_frame_block(&if_block.if_block, frame)?;
                if let Some(block) = &if_block.else_block {
                    self.create_stack_frame_block(block, frame)?;
                }
            } else if let ast::Statement::Expression(expression) = statement {
                self.create_stack_frame_exp(&expression, frame)?;
            }
        }
        if let Some(result) = &item.result {
            self.create_stack_frame_exp(result, frame)?;
        }
        Ok(())
    }

    /// Checks whether the given variable is initialized
    fn check_initialized(self: &Self, item: &ast::Variable) -> CompileResult {
        let state = self.init_state.initialized(item.binding_id);
        if state == BranchingState::Uninitialized || state == BranchingState::MaybeInitialized {
            let variable = item.ident.name.clone();
            Err(CompileError::new(item, if state == BranchingState::Uninitialized { CompileErrorKind::Uninitialized(variable) } else { CompileErrorKind::MaybeInitialized(variable) }, &self.module_path))
        } else {
            Ok(())
        }
    }

    /// Returns a variable index, errors if variable is not initialized.
    fn load_variable(self: &mut Self, item: &ast::Variable) -> CompileResult<FrameAddress> {
        self.check_initialized(item)?;
        Ok(self.locals.lookup(item.binding_id))
    }

    /// Stores an instance constructor on the const pool.
    fn store_constructor(self: &Self, type_id: TypeId, prev_primitive: &mut Option<(StackAddress, ItemIndex)>, fields_merged: &mut ItemIndex) -> CompileResult<StackAddress> {
        let store_len = |inner: &mut dyn FnMut() -> CompileResult<StackAddress>| -> CompileResult {
            let len_position = self.writer.const_len();
            self.writer.store_const(123 as ItemIndex);
            let inner_position = self.writer.const_len();
            inner()?;
            let inner_len = self.writer.const_len() - inner_position;
            self.writer.update_const(len_position, inner_len as ItemIndex);
            Ok(())
        };
        let position = self.writer.const_len();
        match self.ty(&type_id) {
            Type::Array(array) => {
                *prev_primitive = None;
                self.writer.store_const(Constructor::Array);
                store_len(&mut || {
                    self.store_constructor(array.type_id.ice_msg("Unresolved array element type")?, &mut None, &mut 0)
                })?;
                *prev_primitive = None;
            }
            Type::Struct(structure) => {
                *prev_primitive = None;
                self.writer.store_const(Constructor::Struct);
                store_len(&mut || {
                    self.writer.store_const(*self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
                    let field_size_offset = self.writer.store_const(structure.fields.len() as ItemIndex);
                    let mut fields_merged = 0;
                    let mut prev_primitive = None;
                    for field in &structure.fields {
                        self.store_constructor(field.1.ice_msg("Unresolved struct field type")?, &mut prev_primitive, &mut fields_merged)?;
                    }
                    self.writer.update_const(field_size_offset, structure.fields.len() as ItemIndex - fields_merged);
                    Ok(0)
                })?;
            }
            Type::Callable(_callable) => {
                *prev_primitive = None;
                self.writer.store_const(Constructor::Closure);
            },
            Type::String => {
                *prev_primitive = None;
                self.writer.store_const(Constructor::String);
            }
            Type::Enum(enumeration) => {
                *prev_primitive = None;
                self.writer.store_const(Constructor::Enum);
                store_len(&mut || {
                    self.writer.store_const(*self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
                    self.writer.store_const(enumeration.variants.len() as ItemIndex);
                    let variant_offsets_pos = self.writer.const_len();
                    // reserve space for variant offsets
                    for _ in &enumeration.variants {
                        self.writer.store_const(123 as StackAddress);
                    }
                    // write variants, remember offsets
                    let mut variant_offsets = Vec::with_capacity(enumeration.variants.len());
                    for (_, fields) in &enumeration.variants {
                        let num_fields = fields.as_data().map_or(0, |f| f.len());
                        let variant_offset = self.writer.store_const(num_fields as ItemIndex);
                        variant_offsets.push(variant_offset);
                        if num_fields > 0 {
                            // TODO: implement primitive field merge (see Struct case)
                            for field in fields.as_data().ice()? {
                                self.store_constructor(field.ice_msg("Unresolved enum field type")?, &mut None, &mut 0)?;
                            }
                        }
                    }
                    // write variant offsets
                    for (index, &variant_offset) in variant_offsets.iter().enumerate() {
                        let const_position = variant_offsets_pos + (index as StackAddress) * size_of::<StackAddress>() as StackAddress;
                        self.writer.update_const(const_position, variant_offset as StackAddress);
                    }
                    Ok(0)
                })?;
                *prev_primitive = None;
            }
            ty @ _ if ty.is_primitive() => {
                // try to merge primitive with previous primitive
                if let Some((offset, len)) = prev_primitive {
                    let primitive_size = ty.primitive_size() as ItemIndex;
                    *len += primitive_size as ItemIndex;
                    *fields_merged += 1;
                    self.writer.update_const(*offset, *len);
                } else {
                    self.writer.store_const(Constructor::Primitive);
                    let primitive_size = ty.primitive_size() as ItemIndex;
                    *prev_primitive = Some((self.writer.const_len(), primitive_size));
                    self.writer.store_const(primitive_size);
                }
            },
            ty @ _ => Self::ice(&format!("Unsupported type {:?} in constructor serialization", ty))?,
        }
        Ok(position)
    }
}

/// Opcode writer functions.
impl<T> Compiler<T> where T: VMFunc<T> {

    /// Writes a return instruction for the current function being compiled.
    fn write_ret(self: &Self) -> CompileResult<StackAddress> {
        let arg_size = self.locals.arg_size();
        Ok(match self.locals.ret_size() {
            0 => self.writer.ret0(arg_size),
            1 => self.writer.ret8(arg_size),
            2 => self.writer.ret16(arg_size),
            4 => self.writer.ret32(arg_size),
            8 => self.writer.ret64(arg_size),
            _ => Self::ice(&format!("Unsupported arg_size {:?} for ret instruction", arg_size))?,
        })
    }

    /// Writes instructions to decreases reference counts for block locals.
    fn write_scope_destructor(self: &mut Self, target_scope: BranchingScope) -> CompileResult {
        let frame = self.locals.pop();
        for (&binding_id, &local) in frame.map.iter() {
            if self.init_state.declared(binding_id, Some(target_scope)) {
                let state = self.init_state.initialized(binding_id);
                if state != BranchingState::Uninitialized {
                    let type_id = self.binding_by_id(binding_id).type_id.ice()?;
                    let ty = self.ty(&type_id);
                    if ty.is_ref() {
                        comment!(self, "freeing local {}", local);
                        if state == BranchingState::Initialized {
                            self.write_load(ty, local)?;
                            self.write_cnt(ty, false, HeapRefOp::Dec)?;
                        } else if state == BranchingState::MaybeInitialized {
                            self.write_load(ty, local)?;
                            let init_jump = self.writer.jn0sa_nc(123);
                            self.write_discard(ty)?;
                            let uninit_jump = self.writer.jmp(123);
                            let init_target = self.write_cnt(ty, false, HeapRefOp::Dec)?;
                            self.writer.overwrite(init_jump, |w| w.jn0sa_nc(init_target));
                            let done_target = self.writer.position();
                            self.writer.overwrite(uninit_jump, |w| w.jmp(done_target));
                        }
                    }
                }
            }
        }
        self.locals.push(frame);
        Ok(())
    }

    /// Writes given numeric as an immediate value. // FIXME: this will cause endianess issues when compiled/run on different endianess
    fn write_immediate(self: &Self, ty: &Type, numeric: Numeric) -> CompileResult<StackAddress> {
        let small_immediate = |value: u8, ty: &Type| -> CompileResult<StackAddress> {
            Ok(match ty.primitive_size() {
                1 => self.writer.immediate8(value),
                2 => self.writer.immediate16_8(value),
                4 => self.writer.immediate32_8(value),
                8 => self.writer.immediate64_8(value),
                size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, ty))?,
            })
        };
        Ok(match numeric {
            Numeric::Unsigned(v) if v <= u8::MAX as u64 => small_immediate(v as u8, ty)?,
            Numeric::Signed(v) if v >= u8::MIN as i64 && v <= u8::MAX as i64 => small_immediate(v as u8, ty)?,
            Numeric::Signed(v) => {
                match ty {
                    Type::i8 => self.writer.immediate8(v as u8),
                    Type::i16 => self.writer.immediate16(v as u16),
                    Type::i32 => self.writer.immediate32(v as u32),
                    Type::i64 => self.writer.immediate64(v as u64),
                    _ => Self::ice(&format!("Unexpected signed integer literal type: {:?}", ty))?,
                }
            },
            Numeric::Unsigned(v) => {
                match ty {
                    Type::i8 | Type::u8 => self.writer.immediate8(v as u8),
                    Type::i16 | Type::u16 => self.writer.immediate16(v as u16),
                    Type::i32 | Type::u32 => self.writer.immediate32(v as u32),
                    Type::i64 | Type::u64 => self.writer.immediate64(v as u64),
                    _ => Self::ice(&format!("Unexpected unsigned integer literal type: {:?}", ty))?,
                }
            },
            Numeric::Float(v) => {
                match ty {
                    Type::f32 => self.writer.immediate32(u32::from_ne_bytes((v as f32).to_ne_bytes())),
                    Type::f64 => self.writer.immediate64(u64::from_ne_bytes(v.to_ne_bytes())),
                    _ => Self::ice(&format!("Unexpected float literal type: {:?}", ty))?,
                }
            },
        })
    }

    /// Writes a primitive cast.
    fn write_cast(self: &Self, from: &Type, to: &Type) -> CompileResult<StackAddress> {
        let position = self.writer.position();
        if from.is_signed() && !to.is_signed() && !to.is_float() && !to.is_string() {
            self.write_zclamp(from)?;
        }
        if (from.is_integer() || from.is_bool()) && to.is_integer() {
            self.write_integer_cast(from, to)?;
        } else if from.is_float() && to.is_float() {
            self.write_float_integer_cast(from, to)?;
        } else if from.is_float() && to.is_integer() {
            let temp_to = if to.is_signed() { &Type::i64 } else { &Type::u64 };
            self.write_float_integer_cast(from, temp_to)?;
            if to.primitive_size() != 8 {
                self.write_integer_cast(temp_to, to)?;
            }
        } else if from.is_integer() && to.is_float() {
            let temp_from = if from.is_signed() { &Type::i64 } else { &Type::u64 };
            if from.primitive_size() != 8 {
                self.write_integer_cast(from, temp_from)?;
            }
            self.write_float_integer_cast(temp_from, to)?;
        } else if from.is_integer() && to.is_string() {
            let temp_from = if from.is_signed() { &Type::i64 } else { &Type::u64 };
            if from.primitive_size() != 8 {
                self.write_integer_cast(from, temp_from)?;
            }
            match temp_from {
                Type::i64 => self.writer.i64_to_string(),
                Type::u64 => self.writer.u64_to_string(),
                _ => unreachable!("Match arm is not reachable because temp_from is created as an i64 or u64."),
            };
        } else if from == &Type::f32 && to.is_string() {
            self.writer.f32_to_string();
        } else if from == &Type::f64 && to.is_string() {
            self.writer.f64_to_string();
        } else if let Some(Enum { primitive: Some((primitive, _)), .. }) = from.as_enum() {
            let from = self.ty(primitive);
            self.write_cast(from, to)?;
        } else if from != to {
            return Err(CompileError::ice(format!("Unsupported cast from {from:?} to {to:?}")));
        }
        Ok(position)
    }

    /// Writes a cast from one float to another or from/to a 64 bit integer.
    fn write_float_integer_cast(self: &Self, from: &Type, to: &Type) -> CompileResult<StackAddress> {
        Ok(match (from, to) {
            (Type::i64, Type::f32) => self.writer.i64_to_f32(),
            (Type::u64, Type::f32) => self.writer.u64_to_f32(),
            (Type::f64, Type::f32) => self.writer.f64_to_f32(),

            (Type::i64, Type::f64) => self.writer.i64_to_f64(),
            (Type::u64, Type::f64) => self.writer.u64_to_f64(),
            (Type::f32, Type::f64) => self.writer.f32_to_f64(),

            (Type::f32, Type::i64) => self.writer.f32_to_i64(),
            (Type::f64, Type::i64) => self.writer.f64_to_i64(),

            (Type::f32, Type::u64) => self.writer.f32_to_u64(),
            (Type::f64, Type::u64) => self.writer.f64_to_u64(),
            _ => Self::ice(&format!("Invalid float/int cast {:?} to {:?}", from, to))?,
        })
    }

    /// Writes a cast from one integer to another.
    fn write_integer_cast(self: &Self, from: &Type, to: &Type) -> CompileResult<StackAddress> {
        let from_size = (from.primitive_size() * 8) as u8;
        let to_size = (to.primitive_size() * 8) as u8;
        Ok(if to_size < from_size || (to_size == from_size && !from.is_signed() && to.is_signed()) {
            if to.is_signed() {
                match from_size {
                    64 => self.writer.trims64(to_size),
                    32 => self.writer.trims32(to_size),
                    16 => self.writer.trims16(to_size),
                    _ => Self::ice(&format!("Invalid integer cast {:?} to {:?}", from, to))?,
                }
            } else {
                match from_size {
                    64 => self.writer.trimu64(to_size),
                    32 => self.writer.trimu32(to_size),
                    16 => self.writer.trimu16(to_size),
                    _ => Self::ice(&format!("Invalid integer cast {:?} to {:?}", from, to))?,
                }
            }
        } else if to_size > from_size {
            if from.is_signed() {
                match from_size {
                    32 => self.writer.extends32(to_size),
                    16 => self.writer.extends16(to_size),
                    8 => self.writer.extends8(to_size),
                    _ => Self::ice(&format!("Invalid integer cast {:?} to {:?}", from, to))?,
                }
            } else {
                match from_size {
                    32 => self.writer.extendu32(to_size),
                    16 => self.writer.extendu16(to_size),
                    8 => self.writer.extendu8(to_size),
                    _ => Self::ice(&format!("Invalid integer cast {:?} to {:?}", from, to))?,
                }
            }
        } else {
            // no-op cast to self
            self.writer.position()
        })
    }

    /// Writes reference counting operation for given Typeable.
    fn write_cnt(self: &Self, ty: &Type, nc: bool, op: HeapRefOp) -> CompileResult<StackAddress> {
        Ok(if ty.is_ref() {
            let constructor = self.constructor(ty)?;
            match nc {
                true => select_unsigned_opcode!(self, cnt_8_nc, cnt_16_nc, cnt_sa_nc, constructor, op),
                false => select_unsigned_opcode!(self, cnt_8, cnt_16, cnt_sa, constructor, op),
            }
        } else {
            self.writer.position()
        })
    }

    /// Writes instructions to compute member offset for access on a struct.
    fn write_member_offset(self: &Self, offset: StackAddress) {
        if offset > 0 {
            select_unsigned_opcode!(self, offsetx_8, offsetx_16, none, offset as StackOffset);
        }
    }

    /// Writes instructions to fetch a value of the given size from the target of the top heap reference on the stack.
    fn write_heap_fetch(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(match ty.primitive_size() {
            1 => self.writer.heap_fetch8(),
            2 => self.writer.heap_fetch16(),
            4 => self.writer.heap_fetch32(),
            8 => self.writer.heap_fetch64(),
            //16 => { self.writer.heap_fetch128(); },
            size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, ty))?,
        })
    }

    /// Writes instructions to put a value of the given type at the target of the top heap reference on the stack.
    /// If the value being written is a heap reference itself, its refcount will be increased and unless is_new_heap_ref is true
    /// and the replaced value will have its refcount decreased.
    fn write_heap_put(self: &Self, ty: &Type, is_new_heap_ref: bool) -> CompileResult<StackAddress> {
        Ok(if ty.is_ref() {
            let constructor = self.constructor(ty)?;
            if !is_new_heap_ref {
                self.writer.heap_putx_replace(constructor)
            } else {
                self.writer.heap_putx_new(constructor)
            }
        } else {
            match ty.primitive_size() {
                1 => self.writer.heap_put8(),
                2 => self.writer.heap_put16(),
                4 => self.writer.heap_put32(),
                8 => self.writer.heap_put64(),
                size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, ty))?,
            }
        })
    }

    /// Writes instructions to fetch a member of the struct whose reference is at the top of the stack.
    fn write_heap_fetch_member(self: &Self, container_type: &Type, result_type: &Type, offset: StackAddress) -> CompileResult<StackAddress> {
        let constructor = self.constructor(container_type)?;
        Ok(if result_type.is_ref() {
            self.writer.heap_fetch_memberx(offset, constructor)
        } else {
            match result_type.primitive_size() {
                1 => self.writer.heap_fetch_member8(offset, constructor),
                2 => self.writer.heap_fetch_member16(offset, constructor),
                4 => self.writer.heap_fetch_member32(offset, constructor),
                8 => self.writer.heap_fetch_member64(offset, constructor),
                //16 => { self.writer.heap_fetch_member128(offset); },
                size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, result_type))?,
            }
        })
    }

    /// Writes instructions to fetch an element of the array whose reference is at the top of the stack.
    fn write_heap_fetch_element(self: &Self, container_type: &Type, result_type: &Type) -> CompileResult<StackAddress> {
        let constructor = self.constructor(container_type)?;
        Ok(if result_type.is_ref() {
            self.writer.heap_fetch_elementx(constructor)
        } else {
            match result_type.primitive_size() {
                1 => self.writer.heap_fetch_element8(constructor),
                2 => self.writer.heap_fetch_element16(constructor),
                4 => self.writer.heap_fetch_element32(constructor),
                8 => self.writer.heap_fetch_element64(constructor),
                //16 => { self.writer.heap_fetch_element128(); },
                size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, result_type))?,
            }
        })
    }

    /// Writes instructions to fetch an element from the end of the array whose reference is at the top of the stack.
    fn write_heap_tail_element_nc(self: &Self, container_type: &Type, result_type: &Type) -> CompileResult<StackAddress> {
        let constructor = self.constructor(container_type)?;
        Ok(match result_type.primitive_size() {
            1 => self.writer.heap_tail_element8_nc(constructor),
            2 => self.writer.heap_tail_element16_nc(constructor),
            4 => self.writer.heap_tail_element32_nc(constructor),
            8 => self.writer.heap_tail_element64_nc(constructor),
            size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, result_type))?,
        })
    }

    /// Writes an appropriate variant of the store instruction.
    /// If the value being written is a heap reference, its refcount will be increased and unless the local is not active
    /// and the replaced value will have its refcount decreased.
    fn write_store(self: &Self, ty: &Type, loc: FrameAddress, binding_id: Option<BindingId>) -> CompileResult<StackAddress> {
        Ok(if ty.is_ref() && binding_id.is_some() {
            let constructor = self.constructor(ty)?;
            match self.init_state.initialized(binding_id.ice()?) {
                BranchingState::Initialized => self.writer.storex_replace(loc, constructor),
                BranchingState::Uninitialized => self.writer.storex_new(loc, constructor),
                BranchingState::MaybeInitialized => self.writer.storex_replace(loc, constructor), // used to be separate instruction, replace now handles both
            }
        } else {
            match ty.primitive_size() {
                1 => select_unsigned_opcode!(self, store8_8, store8_16, none, loc),
                2 => select_unsigned_opcode!(self, store16_8, store16_16, none, loc),
                4 => select_unsigned_opcode!(self, store32_8, store32_16, none, loc),
                8 => select_unsigned_opcode!(self, store64_8, store64_16, none, loc),
                size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, ty))?,
            }
        })
    }

    /// Writes an appropriate variant of the load instruction.
    fn write_load(self: &Self, ty: &Type, loc: FrameAddress) -> CompileResult<StackAddress> {
        Ok(match ty.primitive_size() {
            1 => select_unsigned_opcode!(self, load8_8, load8_16, none, loc),
            2 => select_unsigned_opcode!(self, load16_8, load16_16, none, loc),
            4 => select_unsigned_opcode!(self, load32_8, load32_16, none, loc),
            8 => select_unsigned_opcode!(self, load64_8, load64_16, none, loc),
            size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, ty))?,
        })
    }

    /// Discard topmost stack value and drop temporaries for reference types.
    fn write_discard(self: &Self, ty: &Type) -> CompileResult {
        if ty.is_ref() {
            self.write_cnt(ty, true, HeapRefOp::Free)?;
        }
        if ty.primitive_size() > 0 {
            self.writer.discard(ty.primitive_size());
        }
        Ok(())
    }

    /// Clone stack value at (negative of) given offset to the top of the stack.
    fn write_clone(self: &Self, ty: &Type) -> StackAddress {
        self.writer.clone(ty.primitive_size())
    }

    fn write_clone_ref(self: &Self) -> StackAddress {
        self.writer.clone(size_of::<crate::HeapAddress>() as u8)
    }

    /// Writes a call instruction. If the function address is not known yet, a placeholder will be written.
    fn write_call(self: &mut Self, function_id: FunctionId) -> StackAddress {
        let function_addr = self.functions.register_call(function_id, self.writer.position(), false);
        let function_arg_size = self.resolved.function(function_id).arg_size(self);
        self.writer.call(function_addr, function_arg_size)
    }

    /// Writes a call_dynamic instruction.
    fn write_call_dynamic(self: &mut Self, type_id: TypeId) -> CompileResult<StackAddress> {
        let arg_size = self.ty(&type_id).as_callable().ice()?.arg_size(self);
        Ok(self.writer.call_dynamic(arg_size))
    }

    /// Writes instructions for build-in len method.
    fn write_builtin(self: &Self, ty: &Type, builtin: BuiltinType) -> CompileResult<StackAddress> {
        let type_id = self.resolved.types().find(|m| m.1 == ty).ice()?.0;
        #[allow(unreachable_patterns)]
        Ok(match (ty, builtin) {
            (&Type::Array(Array { type_id: inner_type_id @ Some(_) }), BuiltinType::Array(array_builtin)) => {
                array_builtin.write(self, type_id, inner_type_id)
            },
            (Type::f32 | Type::f64, BuiltinType::Float(float_builtin)) => {
                float_builtin.write(self, type_id, None)
            },
            (Type::i8 | Type::i16 | Type::i32 | Type::i64 | Type::u8 | Type::u16 | Type::u32 | Type::u64, BuiltinType::Integer(integer_builtin)) => {
                integer_builtin.write(self, type_id, None)
            },
            (Type::String, BuiltinType::String(string_builtin)) => {
                string_builtin.write(self, type_id, None)
            },
            _ => Self::ice(&format!("Builtin {builtin:?} not implemented for {ty}"))?,
        })
    }

    /// Writes clamp to zero instruction.
    fn write_zclamp(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::f32 => self.writer.zclampf32(),
            Type::f64 => self.writer.zclampf64(),
            Type::i8 => self.writer.zclampi8(),
            Type::i16 => self.writer.zclampi16(),
            Type::i32 => self.writer.zclampi32(),
            Type::i64 => self.writer.zclampi64(),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write pre-increment instruction.
    fn write_preinc(self: &Self, ty: &Type, loc: FrameAddress) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i64 | Type::u64 => self.writer.predeci64(loc, -1),
            Type::i32 | Type::u32 => self.writer.predeci32(loc, -1),
            Type::i16 | Type::u16 => self.writer.predeci16(loc, -1),
            Type::i8 | Type::u8 => self.writer.predeci8(loc, -1),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write subtraction instruction.
    fn write_sub(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i8 => self.writer.subs8(),
            Type::i16 => self.writer.subs16(),
            Type::i32 => self.writer.subs32(),
            Type::i64 => self.writer.subs64(),
            Type::u8 => self.writer.subu8(),
            Type::u16 => self.writer.subu16(),
            Type::u32 => self.writer.subu32(),
            Type::u64 => self.writer.subu64(),
            Type::f32 => self.writer.subf32(),
            Type::f64 => self.writer.subf64(),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write variable+variable subtraction instruction.
    fn write_sub_vv(self: &Self, ty: &Type, left: FrameAddress, right: FrameAddress) -> CompileResult<StackAddress>{
        Ok(match ty {
            Type::i8 => self.writer.subs8_vv(left, right),
            Type::i16 => self.writer.subs16_vv(left, right),
            Type::i32 => self.writer.subs32_vv(left, right),
            Type::i64 => self.writer.subs64_vv(left, right),
            Type::u8 => self.writer.subu8_vv(left, right),
            Type::u16 => self.writer.subu16_vv(left, right),
            Type::u32 => self.writer.subu32_vv(left, right),
            Type::u64 => self.writer.subu64_vv(left, right),
            Type::f32 => self.writer.subf32_vv(left, right),
            Type::f64 => self.writer.subf64_vv(left, right),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write expression+variable subtraction instruction.
    fn write_sub_pv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i8 => self.writer.subs8_pv(right),
            Type::i16 => self.writer.subs16_pv(right),
            Type::i32 => self.writer.subs32_pv(right),
            Type::i64 => self.writer.subs64_pv(right),
            Type::u8 => self.writer.subu8_pv(right),
            Type::u16 => self.writer.subu16_pv(right),
            Type::u32 => self.writer.subu32_pv(right),
            Type::u64 => self.writer.subu64_pv(right),
            Type::f32 => self.writer.subf32_pv(right),
            Type::f64 => self.writer.subf64_pv(right),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write expression+immediate subtraction instruction.
    fn write_sub_pi(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i8 => self.writer.subs8_pi(right.as_signed().ice()? as i8),
            Type::i16 => self.writer.subs16_pi(right.as_signed().ice()? as i16),
            Type::i32 => self.writer.subs32_pi(right.as_signed().ice()? as i32),
            Type::i64 => self.writer.subs64_pi(right.as_signed().ice()? as i64),
            Type::u8 => self.writer.subu8_pi(right.as_unsigned().ice()? as u8),
            Type::u16 => self.writer.subu16_pi(right.as_unsigned().ice()? as u16),
            Type::u32 => self.writer.subu32_pi(right.as_unsigned().ice()? as u32),
            Type::u64 => self.writer.subu64_pi(right.as_unsigned().ice()? as u64),
            Type::f32 => self.writer.subf32_pi(right.as_float().ice()? as f32),
            Type::f64 => self.writer.subf64_pi(right.as_float().ice()? as f64),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write variable+=immediate subtraction instruction.
    fn write_sub_vi(self: &Self, ty: &Type, left: FrameAddress, right: Numeric) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i8 => self.writer.subs8_vi(left, right.as_signed().ice()? as i8),
            Type::i16 => self.writer.subs16_vi(left, right.as_signed().ice()? as i16),
            Type::i32 => self.writer.subs32_vi(left, right.as_signed().ice()? as i32),
            Type::i64 => self.writer.subs64_vi(left, right.as_signed().ice()? as i64),
            Type::u8 => self.writer.subu8_vi(left, right.as_unsigned().ice()? as u8),
            Type::u16 => self.writer.subu16_vi(left, right.as_unsigned().ice()? as u16),
            Type::u32 => self.writer.subu32_vi(left, right.as_unsigned().ice()? as u32),
            Type::u64 => self.writer.subu64_vi(left, right.as_unsigned().ice()? as u64),
            Type::f32 => self.writer.subf32_vi(left, right.as_float().ice()? as f32),
            Type::f64 => self.writer.subf64_vi(left, right.as_float().ice()? as f64),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write general addition instruction.
    fn write_add(self: &Self, ty: &Type) -> CompileResult<StackAddress>{
        Ok(match ty {
            Type::i8 => self.writer.adds8(),
            Type::i16 => self.writer.adds16(),
            Type::i32 => self.writer.adds32(),
            Type::i64 => self.writer.adds64(),
            Type::u8 => self.writer.addu8(),
            Type::u16 => self.writer.addu16(),
            Type::u32 => self.writer.addu32(),
            Type::u64 => self.writer.addu64(),
            Type::f32 => self.writer.addf32(),
            Type::f64 => self.writer.addf64(),
            Type::String => self.writer.string_concatx(),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write variable+variable addition instruction.
    fn write_add_vv(self: &Self, ty: &Type, left: FrameAddress, right: FrameAddress) -> CompileResult<StackAddress>{
        Ok(match ty {
            Type::i8 => self.writer.adds8_vv(left, right),
            Type::i16 => self.writer.adds16_vv(left, right),
            Type::i32 => self.writer.adds32_vv(left, right),
            Type::i64 => self.writer.adds64_vv(left, right),
            Type::u8 => self.writer.addu8_vv(left, right),
            Type::u16 => self.writer.addu16_vv(left, right),
            Type::u32 => self.writer.addu32_vv(left, right),
            Type::u64 => self.writer.addu64_vv(left, right),
            Type::f32 => self.writer.addf32_vv(left, right),
            Type::f64 => self.writer.addf64_vv(left, right),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write expression+variable addition instruction.
    fn write_add_pv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i8 => self.writer.adds8_pv(right),
            Type::i16 => self.writer.adds16_pv(right),
            Type::i32 => self.writer.adds32_pv(right),
            Type::i64 => self.writer.adds64_pv(right),
            Type::u8 => self.writer.addu8_pv(right),
            Type::u16 => self.writer.addu16_pv(right),
            Type::u32 => self.writer.addu32_pv(right),
            Type::u64 => self.writer.addu64_pv(right),
            Type::f32 => self.writer.addf32_pv(right),
            Type::f64 => self.writer.addf64_pv(right),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write expression+immediate addition instruction.
    fn write_add_pi(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i8 => self.writer.adds8_pi(right.as_signed().ice()? as i8),
            Type::i16 => self.writer.adds16_pi(right.as_signed().ice()? as i16),
            Type::i32 => self.writer.adds32_pi(right.as_signed().ice()? as i32),
            Type::i64 => self.writer.adds64_pi(right.as_signed().ice()? as i64),
            Type::u8 => self.writer.addu8_pi(right.as_unsigned().ice()? as u8),
            Type::u16 => self.writer.addu16_pi(right.as_unsigned().ice()? as u16),
            Type::u32 => self.writer.addu32_pi(right.as_unsigned().ice()? as u32),
            Type::u64 => self.writer.addu64_pi(right.as_unsigned().ice()? as u64),
            Type::f32 => self.writer.addf32_pi(right.as_float().ice()? as f32),
            Type::f64 => self.writer.addf64_pi(right.as_float().ice()? as f64),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write variable+=immediate addition instruction.
    fn write_add_vi(self: &Self, ty: &Type, left: FrameAddress, right: Numeric) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i8 => self.writer.adds8_vi(left, right.as_signed().ice()? as i8),
            Type::i16 => self.writer.adds16_vi(left, right.as_signed().ice()? as i16),
            Type::i32 => self.writer.adds32_vi(left, right.as_signed().ice()? as i32),
            Type::i64 => self.writer.adds64_vi(left, right.as_signed().ice()? as i64),
            Type::u8 => self.writer.addu8_vi(left, right.as_unsigned().ice()? as u8),
            Type::u16 => self.writer.addu16_vi(left, right.as_unsigned().ice()? as u16),
            Type::u32 => self.writer.addu32_vi(left, right.as_unsigned().ice()? as u32),
            Type::u64 => self.writer.addu64_vi(left, right.as_unsigned().ice()? as u64),
            Type::f32 => self.writer.addf32_vi(left, right.as_float().ice()? as f32),
            Type::f64 => self.writer.addf64_vi(left, right.as_float().ice()? as f64),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write general multiplication instruction.
    fn write_mul(self: &Self, ty: &Type) -> CompileResult<StackAddress>{
        Ok(match ty {
            Type::i8 => self.writer.muls8(),
            Type::i16 => self.writer.muls16(),
            Type::i32 => self.writer.muls32(),
            Type::i64 => self.writer.muls64(),
            Type::u8 => self.writer.mulu8(),
            Type::u16 => self.writer.mulu16(),
            Type::u32 => self.writer.mulu32(),
            Type::u64 => self.writer.mulu64(),
            Type::f32 => self.writer.mulf32(),
            Type::f64 => self.writer.mulf64(),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write variable*variable multiplication instruction.
    fn write_mul_vv(self: &Self, ty: &Type, left: FrameAddress, right: FrameAddress) -> CompileResult<StackAddress>{
        Ok(match ty {
            Type::i8 => self.writer.muls8_vv(left, right),
            Type::i16 => self.writer.muls16_vv(left, right),
            Type::i32 => self.writer.muls32_vv(left, right),
            Type::i64 => self.writer.muls64_vv(left, right),
            Type::u8 => self.writer.mulu8_vv(left, right),
            Type::u16 => self.writer.mulu16_vv(left, right),
            Type::u32 => self.writer.mulu32_vv(left, right),
            Type::u64 => self.writer.mulu64_vv(left, right),
            Type::f32 => self.writer.mulf32_vv(left, right),
            Type::f64 => self.writer.mulf64_vv(left, right),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write expression*variable multiplication instruction.
    fn write_mul_pv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i8 => self.writer.muls8_pv(right),
            Type::i16 => self.writer.muls16_pv(right),
            Type::i32 => self.writer.muls32_pv(right),
            Type::i64 => self.writer.muls64_pv(right),
            Type::u8 => self.writer.mulu8_pv(right),
            Type::u16 => self.writer.mulu16_pv(right),
            Type::u32 => self.writer.mulu32_pv(right),
            Type::u64 => self.writer.mulu64_pv(right),
            Type::f32 => self.writer.mulf32_pv(right),
            Type::f64 => self.writer.mulf64_pv(right),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write expression*immediate multiplication instruction.
    fn write_mul_pi(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::i8 => self.writer.muls8_pi(right.as_signed().ice()? as i8),
            Type::i16 => self.writer.muls16_pi(right.as_signed().ice()? as i16),
            Type::i32 => self.writer.muls32_pi(right.as_signed().ice()? as i32),
            Type::i64 => self.writer.muls64_pi(right.as_signed().ice()? as i64),
            Type::u8 => self.writer.mulu8_pi(right.as_unsigned().ice()? as u8),
            Type::u16 => self.writer.mulu16_pi(right.as_unsigned().ice()? as u16),
            Type::u32 => self.writer.mulu32_pi(right.as_unsigned().ice()? as u32),
            Type::u64 => self.writer.mulu64_pi(right.as_unsigned().ice()? as u64),
            Type::f32 => self.writer.mulf32_pi(right.as_float().ice()? as f32),
            Type::f64 => self.writer.mulf64_pi(right.as_float().ice()? as f64),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write division instruction.
    fn write_div(self: &Self, ty: &Type) -> CompileResult<StackAddress>{
        Ok(match ty {
            Type::i8 => self.writer.divs8(),
            Type::i16 => self.writer.divs16(),
            Type::i32 => self.writer.divs32(),
            Type::i64 => self.writer.divs64(),
            Type::u8 => self.writer.divu8(),
            Type::u16 => self.writer.divu16(),
            Type::u32 => self.writer.divu32(),
            Type::u64 => self.writer.divu64(),
            Type::f32 => self.writer.divf32(),
            Type::f64 => self.writer.divf64(),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write remainder instruction.
    fn write_rem(self: &Self, ty: &Type) -> CompileResult<StackAddress>{
        Ok(match ty {
            Type::i8 => self.writer.rems8(),
            Type::i16 => self.writer.rems16(),
            Type::i32 => self.writer.rems32(),
            Type::i64 => self.writer.rems64(),
            Type::u8 => self.writer.remu8(),
            Type::u16 => self.writer.remu16(),
            Type::u32 => self.writer.remu32(),
            Type::u64 => self.writer.remu64(),
            Type::f32 => self.writer.remf32(),
            Type::f64 => self.writer.remf64(),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write negation instruction.
    fn write_neg(self: &Self, ty: &Type) -> CompileResult<StackAddress>{
        Ok(match ty {
            Type::i8 => self.writer.negs8(),
            Type::i16 => self.writer.negs16(),
            Type::i32 => self.writer.negs32(),
            Type::i64 => self.writer.negs64(),
            Type::f32 => self.writer.negf32(),
            Type::f64 => self.writer.negf64(),
            _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
        })
    }

    /// Write compare equal instruction.
    fn write_eq(self: &Self, ty: &Type) -> CompileResult<StackAddress>{
        Ok(if ty.is_primitive() {
            match ty.primitive_size() {
                1 => self.writer.ceq8(),
                2 => self.writer.ceq16(),
                4 => self.writer.ceq32(),
                8 => self.writer.ceq64(),
                size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, ty))?,
            }
        } else if ty.is_string() {
            self.writer.string_ceq()
        } else {
            unimplemented!("General heap compare");
        })
    }

    /// Write compare not equal instruction.
    fn write_neq(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            match ty.primitive_size() {
                1 => self.writer.cneq8(),
                2 => self.writer.cneq16(),
                4 => self.writer.cneq32(),
                8 => self.writer.cneq64(),
                size @ _ => Self::ice(&format!("Unsupported size {} for type {:?}", size, ty))?,
            }
        } else if ty.is_string() {
            self.writer.string_cneq()
        } else {
            unimplemented!("General heap compare");
        })
    }

    /// Write compare less than instruction.
    fn write_lt(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            match ty {
                Type::i8 => self.writer.clts8(),
                Type::i16 => self.writer.clts16(),
                Type::i32 => self.writer.clts32(),
                Type::i64 => self.writer.clts64(),
                Type::u8 => self.writer.cltu8(),
                Type::u16 => self.writer.cltu16(),
                Type::u32 => self.writer.cltu32(),
                Type::u64 => self.writer.cltu64(),
                Type::f32 => self.writer.cltf32(),
                Type::f64 => self.writer.cltf64(),
                _ => Self::ice(&format!("Unsupported operation for type {ty}"))?,
            }
        } else if ty.is_string() {
            self.writer.string_clt()
        } else {
            Self::ice(&format!("Unsupported type {ty} for lt operation"))?
        })
    }

    /// Write compare less than or equal instruction.
    fn write_lte(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            match ty {
                Type::i8 => self.writer.cltes8(),
                Type::i16 => self.writer.cltes16(),
                Type::i32 => self.writer.cltes32(),
                Type::i64 => self.writer.cltes64(),
                Type::u8 => self.writer.clteu8(),
                Type::u16 => self.writer.clteu16(),
                Type::u32 => self.writer.clteu32(),
                Type::u64 => self.writer.clteu64(),
                Type::f32 => self.writer.cltef32(),
                Type::f64 => self.writer.cltef64(),
                _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
            }
        } else if ty.is_string() {
            self.writer.string_clte()
        } else {
            Self::ice(&format!("Unsupported type {ty} for lt operation"))?
        })
    }

    /// Write compare greater than instruction.
    fn write_gt(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            match ty {
                Type::i8 => self.writer.cgts8(),
                Type::i16 => self.writer.cgts16(),
                Type::i32 => self.writer.cgts32(),
                Type::i64 => self.writer.cgts64(),
                Type::u8 => self.writer.cgtu8(),
                Type::u16 => self.writer.cgtu16(),
                Type::u32 => self.writer.cgtu32(),
                Type::u64 => self.writer.cgtu64(),
                Type::f32 => self.writer.cgtf32(),
                Type::f64 => self.writer.cgtf64(),
                _ => Self::ice(&format!("Unsupported operation for type {ty}"))?,
            }
        } else if ty.is_string() {
            self.writer.string_cgt()
        } else {
            Self::ice(&format!("Unsupported type {ty} for lt operation"))?
        })
    }

    /// Write compare greater than or equal instruction.
    fn write_gte(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            match ty {
                Type::i8 => self.writer.cgtes8(),
                Type::i16 => self.writer.cgtes16(),
                Type::i32 => self.writer.cgtes32(),
                Type::i64 => self.writer.cgtes64(),
                Type::u8 => self.writer.cgteu8(),
                Type::u16 => self.writer.cgteu16(),
                Type::u32 => self.writer.cgteu32(),
                Type::u64 => self.writer.cgteu64(),
                Type::f32 => self.writer.cgtef32(),
                Type::f64 => self.writer.cgtef64(),
                _ => Self::ice(&format!("Unsupported operation for type {:?}", ty))?,
            }
        } else if ty.is_string() {
            self.writer.string_cgte()
        } else {
            Self::ice(&format!("Unsupported type {ty} for gt operation"))?
        })
    }
}

/// Itsy-trait support functions. // TODO: holy crap those signatures need some custom types
impl<T> Compiler<T> {

    /// Computes the vtable function base-offset for the given function. To get the final offset the dynamic types trait_implementor_index * sizeof(StackAddress) has to be added.
    fn vtable_function_offset(self: &Self, function_id: FunctionId) -> CompileResult<StackAddress> {
        let trait_function_id = *self.trait_function_implementors.get(&function_id).unwrap_or(&function_id);
        let function_index = *self.trait_function_indices.get(&trait_function_id).ice_msg("Invalid trait function id")?;
        Ok(self.trait_vtable_base + (function_index as usize * size_of::<StackAddress>() * self.trait_implementor_indices.len()) as StackAddress)
    }

    /// Generate flat list of all traits and their functions
    fn filter_trait_functions(resolved: &Resolved) -> Vec<(TypeId, &String, FunctionId)> {
        resolved.traits()
            .flat_map(|(type_id, trt)| {
                trt.provided.iter().map(move |(function_name, constant_id)| (type_id, function_name, resolved.constant(constant_id.unwrap()).value.as_function_id().unwrap()))
                .chain(trt.required.iter().map(move |(function_name, constant_id)| (type_id, function_name, resolved.constant(constant_id.unwrap()).value.as_function_id().unwrap())))
            })
            .collect()
    }

    /// Contiguously index trait functions so vtable position can be computed by multiplying indices
    fn enumerate_trait_function_indices(trait_functions: &Vec<(TypeId, &String, FunctionId)>) -> UnorderedMap<FunctionId, ItemIndex> {
        let mut trait_function_indices = UnorderedMap::new();
        for (index, &(_, _, function_id)) in trait_functions.iter().enumerate() {
            trait_function_indices.insert(function_id, index as ItemIndex);
        }
        trait_function_indices
    }

    /// Contiguously index trait implementors so vtable position can be computed by multiplying indices
    fn enumerate_trait_implementor_indices(trait_implementors: &Vec<(TypeId, &Map<TypeId, ImplTrait>)>) -> UnorderedMap<TypeId, ItemIndex> {
        let mut trait_implementor_indices = UnorderedMap::new();
        for (index, &(type_id, _)) in trait_implementors.iter().enumerate() {
            trait_implementor_indices.insert(type_id, index as ItemIndex);
        }
        trait_implementor_indices
    }

    /// Map trait implementor function ids to trait function ids
    fn map_trait_function_implementors(resolved: &Resolved, trait_functions: &Vec<(TypeId, &String, FunctionId)>, trait_implementors: &Vec<(TypeId, &Map<TypeId, ImplTrait>)>) -> CompileResult<UnorderedMap<FunctionId, FunctionId>> {
        let mut trait_function_implementors = UnorderedMap::new();
        for &(trait_type_id, function_name, trait_function_id) in trait_functions.iter() {
            for &(_, implementor_traits) in trait_implementors.iter() {
                if let Some(impl_trait) = implementor_traits.get(&trait_type_id) {
                    if let Some(&implementor_function_id) = impl_trait.functions.get(function_name) {
                        let implementor_function_id = resolved.constant(implementor_function_id.ice_msg("Unresolved implementor function")?).value.as_function_id().ice()?;
                        trait_function_implementors.insert(implementor_function_id, trait_function_id);
                    }
                }
            }
        }
        Ok(trait_function_implementors)
    }

    /// Create table of all trait implementors/concrete function id permutations (implementor or trait-provided).
    /// To facility direct lookups, this table contains *all* implementor/trait permuations, including those that are not implemented.
    /// This allows for the lookup position to be computed as `table_start + function_index * num_trait_implementors + trait_implementor_index` (sizeof<ItemIndex> multipliers omitted)
    /// Since the first three components are statically known their result can be statically supplied to call_virtual. The implementor index is stored
    /// on the heap object the function is being called on.
    fn select_trait_function_implementations(resolved: &Resolved, trait_functions: &Vec<(TypeId, &String, FunctionId)>, trait_implementors: &Vec<(TypeId, &Map<TypeId, ImplTrait>)>) -> CompileResult<Vec<(usize, Option<FunctionId>)>> {
        let mut trait_implementation_mapping = Vec::new();
        for &(trait_type_id, function_name, trait_function_id) in trait_functions.iter() {
            for (implementor_index, &(_, implementor_traits)) in trait_implementors.iter().enumerate() {
                trait_implementation_mapping.push((implementor_index, match implementor_traits.get(&trait_type_id) {
                    Some(impl_trait) => {
                        if let Some(constant_id) = impl_trait.functions.get(function_name) {
                            Some(resolved.constant(constant_id.ice()?).value.as_function_id().ice()?)
                        } else {
                            Some(trait_function_id)
                        }
                    },
                    None => None,
                }));
            }
        }
        Ok(trait_implementation_mapping)
    }
}

/// Support TypeContainer for Bindings so that methods that need to follow type_ids can be implemented once and be used in both
/// the Resolver where types are scored in Scopes and the Compiler where types are a stored in a Vec.
impl<T> TypeContainer for Compiler<T> {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type {
        self.resolved.ty(type_id)
    }
    fn type_by_id_mut(self: &mut Self, _type_id: TypeId) -> &mut Type {
        unreachable!("Compiler does not mutate types.")
    }
    fn type_flat_name(self: &Self, _type_id: TypeId) -> Option<&String> {
        None // TODO
    }
}

/// A container holding binding id to Binding mappings
impl<T> BindingContainer for Compiler<T> {
    fn binding_by_id(self: &Self, binding_id: BindingId) -> &Binding {
        self.resolved.binding(binding_id)
    }
    fn binding_by_id_mut(self: &mut Self, _binding_id: BindingId) -> &mut Binding {
        unreachable!("Compiler does not mutate bindings.")
    }
    fn constant_by_id(self: &Self, constant_id: ConstantId) -> &Constant {
        self.resolved.constant(constant_id)
    }
    fn constant_by_id_mut(self: &mut Self, _: ConstantId) -> &mut Constant {
        unreachable!("Compiler does not mutate constants.")
    }
}