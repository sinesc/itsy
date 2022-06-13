//! Bytecode emitter. Compiles bytecode from AST.

mod stack_frame;
#[macro_use] mod macros;
pub mod error;
mod util;
mod binding_state;

use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex, VariantIndex, STACK_ADDRESS_TYPE};
use crate::shared::{BindingContainer, TypeContainer, numeric::Numeric, meta::{Type, ImplTrait, Struct, Array, Enum, Function, FunctionKind, BuiltinGroup, Binding}, typed_ids::{BindingId, FunctionId, TypeId}};
use crate::frontend::{ast::{self, Typeable, TypeName, Returns}, resolver::resolved::{ResolvedProgram, IdMappings}};
use crate::bytecode::{Constructor, Writer, StoreConst, Program, VMFunc, ARG1, ARG2, ARG3, runtime::heap::HeapRefOp};
use stack_frame::{Local, StackFrame, StackFrames, LocalOrigin};
use error::{CompileError, CompileErrorKind, CompileResult};
use util::CallInfo;
use binding_state::{BindingState, BranchingKind, BranchingPath};

/// Bytecode emitter. Compiles bytecode from resolved program (AST).
struct Compiler<T> {
    /// Bytecode writer used to output to.
    writer: Writer<T>,
    /// Type and mutability data.
    id_mappings: IdMappings,
    /// Maps from binding id to load-argument for each frame.
    locals: StackFrames,
    /// Non-primitive type constructors.
    constructors: UnorderedMap<TypeId, StackAddress>,
    // Maps functions to their call index.
    functions: UnorderedMap<FunctionId, CallInfo>,
    /// Bytecode locations of function call instructions that need their target address fixed (because the target wasn't written yet).
    call_placeholder: UnorderedMap<FunctionId, Vec<StackAddress>>,
    /// Tracks variable initialization state.
    init_state: BindingState,
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
/// itsy_api!(MyAPI, (), {
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
///             print(\"Hello from Itsy!\");
///         }
///     ", "").unwrap();
///     let mut parsed = parser::ParsedProgram::new();
///     parsed.add_module(module);
///     let resolved = resolver::resolve::<MyAPI>(parsed, "main").unwrap();
///     let compiled = compiler::compile(resolved).unwrap();
/// }
/// ```
///
/// The returned [Program] is now ready to be run by [run](crate::run) or [VM::run](crate::VM::run).
pub fn compile<T>(program: ResolvedProgram<T>) -> CompileResult<Program<T>> where T: VMFunc<T> {

    let ResolvedProgram { modules, id_mappings, entry_fn, .. } = program;

    // find all trait functions and all trait implementors
    let trait_functions = Compiler::<T>::filter_trait_functions(&id_mappings);
    let trait_implementors: Vec<_> = id_mappings.implementors().collect();

    // save flattened list of trait implementations and their concrete function ids for later serialization (once compilation is done and absolute function addresses are known)
    let trait_function_implementations = Compiler::<T>::select_trait_function_implementations(&trait_functions, &trait_implementors);

    // initialize compiler struct used while visiting every ast node
    let mut compiler = Compiler {
        writer                      : Writer::new(),
        locals                      : StackFrames::new(),
        functions                   : UnorderedMap::new(),
        call_placeholder            : UnorderedMap::new(),
        constructors                : UnorderedMap::new(),
        init_state                  : BindingState::new(),
        module_path                 : "".to_string(),
        trait_vtable_base           : (trait_implementors.len() * size_of::<StackAddress>()) as StackAddress, // offset vtable by size of mapping from constructor => implementor-index
        trait_function_indices      : Compiler::<T>::enumerate_trait_function_indices(&trait_functions),
        trait_implementor_indices   : Compiler::<T>::enumerate_trait_implementor_indices(&trait_implementors),
        trait_function_implementors : Compiler::<T>::map_trait_function_implementors(&trait_functions, &trait_implementors),
        id_mappings                 : id_mappings,
    };

    // reserve space for the trait vtable as well as an implementor-index => constructor mapping (required for trait object reference counting) in const pool
    let vtable_size = compiler.trait_function_indices.len() * compiler.trait_implementor_indices.len() * size_of::<StackAddress>();
    compiler.writer.reserve_const_data(compiler.trait_vtable_base + vtable_size as StackAddress); // FIXME: this does not consider endianess

    // serialize constructors onto const pool, ensure position 0 is not used as that indicates a virtual constructor
    if compiler.writer.position() == 0 {
        compiler.writer.store_const(101 as u8);
    }
    for (type_id, ty) in compiler.id_mappings.types() {
        if !ty.is_primitive() && !ty.as_trait().is_some() {
            // store constructor, remember position
            let position = compiler.store_constructor(type_id);
            compiler.constructors.insert(type_id, position);
            // for trait implementing types, link constructor to trait implementor (trait objects still need proper reference counting, which requires the constructor of the concrete type)
            if let Some(&implementor_index) = compiler.trait_implementor_indices.get(&type_id) {
                compiler.writer.update_const((implementor_index as usize * size_of::<StackAddress>()) as StackAddress, position);
            }
        }
    }

    // write placeholder jump to program entry
    let initial_pos = compiler.writer.call(123, 0);
    compiler.writer.exit();

    // compile program
    for module in modules {
        compiler.module_path = module.path.clone();
        for statement in module.iter() {
            compiler.compile_statement(statement)?;
        }
    }

    // write actual function offsets to vtable
    for (implementor_index, selected_function_id) in trait_function_implementations {
        if let Some(selected_function_id) = selected_function_id {
            let selected_function_offset = compiler.functions.get(&selected_function_id).expect("Missing function callinfo").addr;
            let vtable_function_offset = compiler.vtable_function_offset(selected_function_id);
            compiler.writer.update_const(vtable_function_offset + (implementor_index * size_of::<StackAddress>()) as StackAddress, selected_function_offset);
        }
    }

    // overwrite placeholder with actual entry position
    let &entry_call = compiler.functions.get(&entry_fn).expect("Failed to locate entry function in generated code.");
    compiler.writer.overwrite(initial_pos, |w| w.call(entry_call.addr, entry_call.arg_size));

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
            S::Return(_) => unreachable!("Return AST nodes should have been rewritten"),
            S::Function(function) => self.compile_function(function),
            S::ImplBlock(impl_block) => {
                for function in &impl_block.functions {
                    self.compile_function(function)?;
                    // if this is a trait impl check compatibility to trait
                    if let Some(TypeName { type_id, .. }) = impl_block.trt {
                        let trait_type_id = type_id.expect("Unresolved trait encountered");
                        let trt = self.type_by_id(trait_type_id).as_trait().unwrap();
                        let function_id = function.function_id;
                        let function_name = &function.sig.ident.name;
                        // check if function is defined in trait
                        let trait_function_id = trt.provided.get(function_name).or(trt.required.get(function_name)).unwrap();
                        let trait_function = self.id_mappings.function(trait_function_id.unwrap());
                        let impl_function = self.id_mappings.function(function_id.unwrap());
                        if !self.is_compatible_function(trait_function, impl_function) {
                            return Err(CompileError::new(function, CompileErrorKind::IncompatibleTraitMethod(function_name.clone()), &self.module_path));
                        }
                    }
                }
                Ok(())
            }
            S::TraitDef(trait_def) => {
                for function in &trait_def.functions {
                    if function.block.is_some() {
                        self.compile_function(function)?;
                    }
                }
                Ok(())
            },
            S::Binding(binding) => self.compile_binding(binding),
            S::IfBlock(if_block) => {
                self.compile_if_block(if_block)?;
                if let Some(result) = &if_block.if_block.result {
                    let result_type = self.item_type(result);
                    self.write_discard(result_type);
                }
                Ok(())
            }
            S::ForLoop(for_loop) => self.compile_for_loop(for_loop),
            S::WhileLoop(while_loop) => self.compile_while_loop(while_loop),
            S::Block(block) => {
                self.compile_block(block)?;
                if let Some(result) = &block.result {
                    let result_type = self.item_type(result);
                    self.write_discard(result_type);
                }
                Ok(())
            }
            S::Expression(expression) => {
                self.compile_expression(expression)?;
                let result_type = self.item_type(expression);
                self.write_discard(result_type);
                Ok(())
            }
        }
    }

    /// Compiles the given expression.
    fn compile_expression(self: &mut Self, item: &ast::Expression) -> CompileResult {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.compile_literal(literal),
            E::Variable(variable)       => self.compile_variable(variable),
            E::Member(_)                => Ok(()),
            E::Call(call)               => self.compile_call(call),
            E::Assignment(assignment)   => self.compile_assignment(assignment),
            E::BinaryOp(binary_op)      => self.compile_binary_op(binary_op),
            E::UnaryOp(unary_op)        => self.compile_unary_op(unary_op),
            E::Cast(cast)               => self.compile_cast(cast),
            E::Block(block)             => self.compile_block(block),
            E::IfBlock(if_block)        => self.compile_if_block(if_block),
            E::MatchBlock(match_block)        => self.compile_match_block(match_block),
        }
    }

    /// Compiles the assignment operation.
    fn compile_assignment(self: &mut Self, item: &ast::Assignment) -> CompileResult {
        comment!(self, "{}", item);
        match item.left {
            ast::Expression::Variable(_) => self.compile_assignment_to_var(item),
            ast::Expression::BinaryOp(_) => self.compile_assignment_to_offset(item),
            _ => panic!("cannot assign to left expression"),
        }
    }

    /// Compiles an assignment to a variable.
    fn compile_assignment_to_var(self: &mut Self, item: &ast::Assignment) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        comment!(self, "direct assignment");
        let binding_id = item.left.as_variable().unwrap().binding_id.unwrap();
        let local = self.locals.lookup(binding_id);
        match item.op {
            BO::Assign => {
                self.compile_expression(&item.right)?;
                self.write_storex(local, &item.left, binding_id);
                self.init_state.initialize(binding_id);
            },
            _ => {
                if !self.init_state.initialized(binding_id) {
                    let variable = item.left.as_variable().unwrap();
                    return Err(CompileError::new(item, CompileErrorKind::Uninitialized(variable.ident.name.clone()), &self.module_path));
                }
                let ty = self.item_type(&item.left);
                self.write_load(local.index as StackOffset, ty); // stack: left
                self.compile_expression(&item.right)?; // stack: left right
                let ty = self.item_type(&item.left);
                match item.op { // stack: result
                    BO::AddAssign => self.write_add(ty),
                    BO::SubAssign => self.write_sub(ty),
                    BO::MulAssign => self.write_mul(ty),
                    BO::DivAssign => self.write_div(ty),
                    BO::RemAssign => self.write_rem(ty),
                    op @ _ => unreachable!("Invalid assignment operator {}", op),
                };
                self.write_storex(local, &item.left, binding_id); // stack --
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
                let ty = self.item_type(&item.left);
                self.write_heap_putx(ty, false);    // stack: --
            },
            _ => {
                self.compile_expression(&item.left)?;       // stack: &left
                self.write_clone_ref();                     // stack: &left &left
                let ty = self.item_type(&item.left);
                self.write_heap_fetch(ty);                      // stack: &left left
                self.compile_expression(&item.right)?;      // stack: &left left right
                let ty = self.item_type(&item.left);
                match item.op {                                 // stack: &left result
                    BO::AddAssign => self.write_add(ty),
                    BO::SubAssign => self.write_sub(ty),
                    BO::MulAssign => self.write_mul(ty),
                    BO::DivAssign => self.write_div(ty),
                    BO::RemAssign => self.write_rem(ty),
                    _ => unreachable!("Unsupported assignment operator encountered"),
                };
                self.write_heap_putx(ty, false); // stack -
            },
        }
        Ok(())
    }

    /// Compiles function call arguments.
    fn compile_call_args(self: &mut Self, function: &Function, item: &ast::Call) -> CompileResult {
        // put args on stack, increase ref count to ensure temporaries won't be dropped after access
        // function is responsible for decrementing argument ref-count on exit
        for (_index, arg) in item.args.iter().enumerate() {
            comment!(self, "{}() arg {}", item.ident.name, arg);
            self.compile_expression(arg)?;
            match function.kind.unwrap() {
                FunctionKind::Method(_) | FunctionKind::Function => {
                    self.item_cnt(arg, true, HeapRefOp::Inc);
                },
                _ => { }
            }
        }
        Ok(())
    }

    /// Compiles the given call.
    fn compile_call(self: &mut Self, item: &ast::Call) -> CompileResult {
        comment!(self, "prepare {}() args", item.ident.name);
        let function_id = item.function_id.expect("Unresolved function encountered");
        let function = self.id_mappings.function(function_id).clone();
        match function.kind.unwrap() {
            FunctionKind::Rust(rust_fn_index) => {
                self.compile_call_args(&function, item)?;
                comment!(self, "call {}()", item.ident.name);
                self.writer.rustcall(T::from_index(rust_fn_index));
            },
            FunctionKind::Builtin(type_id, builtin_group) => {
                self.compile_call_args(&function, item)?;
                comment!(self, "call {}()", item.ident.name);
                self.write_builtin(builtin_group, self.type_by_id(type_id));
            },
            FunctionKind::Method(object_type_id) => {
                self.compile_call_args(&function, item)?;
                if self.type_by_id(object_type_id).as_trait().is_some() {
                    // dynamic dispatch
                    let function_offset = self.vtable_function_offset(function_id);
                    let function_arg_size = self.id_mappings.function_arg_size(function_id);
                    comment!(self, "call {}()", item.ident.name);
                    self.writer.vcall(function_offset, function_arg_size);
                } else {
                    // static dispatch
                    comment!(self, "call {}()", item.ident.name);
                    self.write_call(function_id);
                }
            },
            FunctionKind::Function => {
                self.compile_call_args(&function, item)?;
                comment!(self, "call {}()", item.ident.name);
                self.write_call(function_id);
            },
            FunctionKind::Variant(type_id, variant_index) => {
                let index_type = Type::unsigned(size_of::<VariantIndex>());
                self.write_literal_numeric(Numeric::Unsigned(variant_index as u64), &index_type);
                self.compile_call_args(&function, item)?;
                let function_id = item.function_id.expect("Unresolved function encountered");
                let arg_size = self.id_mappings.function_arg_size(function_id);
                self.writer.upload(arg_size + index_type.primitive_size() as StackAddress, *self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
            },
        }
        Ok(())
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_binding(self: &mut Self, item: &ast::Binding) -> CompileResult {
        let binding_id = item.binding_id.expect("Unresolved binding encountered");
        self.init_state.activate(binding_id);
        if let Some(expr) = &item.expr {
            comment!(self, "let {} = ...", item.ident.name);
            // optimization: write literal unless it is zero and we're in the root block of a function (stackframe is already zero-initialized)
            if !(expr.is_zero_literal() && self.init_state.len() == 2) {
                self.compile_expression(expr)?;
                let local = self.locals.lookup(binding_id);
                self.write_storex(local, item, binding_id);
            }
            self.init_state.initialize(binding_id);
        }
        Ok(())
    }

    /// Compiles the given variable.
    fn compile_variable(self: &mut Self, item: &ast::Variable) -> CompileResult {
        comment!(self, "variable {}", item);
        let load_index = {
            let binding_id = item.binding_id.expect("Unresolved binding encountered");
            let local = self.locals.lookup(binding_id);
            if !self.init_state.initialized(binding_id) {
                return Err(CompileError::new(item, CompileErrorKind::Uninitialized(item.ident.name.clone()), &self.module_path));
            }
            local.index
        };
        self.write_load(load_index as StackOffset, self.item_type(item));
        Ok(())
    }

    /// Compiles an if block without else part.
    fn compile_if_only_block(self: &mut Self, item: &ast::IfBlock) -> CompileResult {
        comment!(self, "{}", item);
        let exit_jump = self.writer.j0(123);
        self.init_state.push(BranchingKind::Double);
        let result = self.compile_block(&item.if_block);
        self.init_state.pop();
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
        result
    }

    /// Compiles an if+else block.
    fn compile_if_else_block(self: &mut Self, if_block: &ast::Block, else_block: &ast::Block) -> CompileResult {

        let else_jump = self.writer.j0(123);
        self.init_state.push(BranchingKind::Double);
        self.init_state.set_path(BranchingPath::A);
        let result = self.compile_block(if_block);
        let exit_jump = if !if_block.returns() {
            Some(self.writer.jmp(123))
        } else {
            None
        };

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
        self.compile_expression(&item.cond)?;

        if item.else_block.is_none() {
            self.compile_if_only_block(item)
        } else {
            self.compile_if_else_block(&item.if_block, item.else_block.as_ref().unwrap())
        }
    }

    /// Write match arm block code.
    fn compile_match_arm(self: &mut Self, exit_jumps: &mut Vec<StackAddress>, block: &ast::Block) -> CompileResult {
        self.compile_block(block)?;
        if !block.returns() {
            exit_jumps.push(self.writer.jmp(123));
        }
        Ok(())
    }

    /// Split match block into recursive tree of A/B branches.
    fn compile_match_block_recursive(self: &mut Self, exit_jumps: &mut Vec<StackAddress>, remaining_branches: &[(ast::Pattern, ast::Block)]) -> CompileResult {
        if remaining_branches.len() == 1 {
            self.init_state.push(BranchingKind::Single); // patterns are required to be exhaustive
            self.compile_match_arm(exit_jumps, &remaining_branches[0].1)?;
            self.init_state.pop();
        } else if remaining_branches.len() > 1 {
            self.init_state.push(BranchingKind::Double);
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
        self.compile_block(&item.block)?;
        self.writer.jmp(start_target);
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
        Ok(())
    }

    fn compile_for_loop_range(self: &mut Self, item: &ast::ForLoop, iter_local: Local, iter_type_id: TypeId) -> CompileResult {
        comment!(self, "for in range");
        // store lower range bound in iter variable
        let binary_op = item.expr.as_binary_op().unwrap();
        self.compile_expression(&binary_op.left)?;
        let iter_ty = self.type_by_id(iter_type_id);
        self.write_store(iter_local, iter_ty);
        // push upper range bound
        self.compile_expression(&binary_op.right)?;
        // precheck (could be avoided by moving condition to the end but not trivial due to stack top clone order)
        let iter_ty = self.type_by_id(iter_type_id);
        self.write_load(iter_local.index as StackOffset, iter_ty);
        //self.write_clone(iter_ty, iter_ty.primitive_size()); // clone upper bound for comparison, skip over iter inbetween
        self.write_load(-(2 * iter_ty.primitive_size() as StackOffset), iter_ty);
        if binary_op.op == ast::BinaryOperator::Range {
            self.write_lt(iter_ty);
        } else {
            self.write_lte(iter_ty);
        }
        let exit_jump = self.writer.j0(123);
        // compile block
        let start_target = self.writer.position();
        self.compile_block(&item.block)?;
        // load bounds, increment and compare
        let iter_ty = self.type_by_id(iter_type_id);
        self.write_preinc(iter_local.index as StackOffset, iter_ty);
        //self.write_clone(iter_ty, iter_ty.primitive_size()); // clone upper bound for comparison, skip over iter inbetween
        self.write_load(-(2 * iter_ty.primitive_size() as StackOffset), iter_ty);
        if binary_op.op == ast::BinaryOperator::Range {
            self.write_lt(iter_ty);
        } else {
            self.write_lte(iter_ty);
        }
        self.writer.jn0(start_target);
        // exit position
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
        self.write_discard(iter_ty);
        Ok(())
    }

    fn compile_for_loop_array(self: &mut Self, item: &ast::ForLoop, element_local: Local, element_type_id: TypeId) -> CompileResult {
        comment!(self, "for in array");
        let element_ty = self.type_by_id(element_type_id);
        let element_constructor = if element_ty.is_ref() { self.get_constructor(element_ty) } else { 0 };
        let array_ty = self.item_type(&item.expr);
        let array_constructor = self.get_constructor(array_ty);

        self.compile_expression(&item.expr)?;   // stack &array
        self.write_cnt_nc(array_constructor, HeapRefOp::Inc);
        self.write_clone_ref();
        let array_ty = self.item_type(&item.expr);
        self.write_builtin(BuiltinGroup::ArrayLen, array_ty);             // stack &array len
        let exit_jump = self.writer.j0_sa_nc(123);
        let loop_start = self.write_dec(&STACK_ADDRESS_TYPE);        // stack &array index

        // TODO: consuming heap fetch would need this too
        //self.write_clone(array_ty, STACK_ADDRESS_TYPE.primitive_size());    // stack &array index &array
        //self.write_clone(&STACK_ADDRESS_TYPE, array_ty.primitive_size());    // stack &array index &array index

        let element_ty = self.type_by_id(element_type_id);
        self.write_heap_tail_element_nc(array_ty, element_ty);   // stack &array index element

        if element_ty.is_ref() {
            self.write_clone(element_ty);       // stack &array index element element
            self.write_cnt_nc(element_constructor, HeapRefOp::Inc);
        }

        self.write_store(element_local, element_ty);      // stack &array index <is_ref ? element>
        self.compile_block(&item.block)?;       // stack &array index <is_ref ? element>

        let element_ty = self.type_by_id(element_type_id);
        if element_ty.is_ref() {
            self.write_cnt(element_constructor, HeapRefOp::Dec);         // stack &array index
        }

        self.writer.jn0_sa_nc(loop_start);

        // exit position
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0_sa_nc(exit_target));
        self.write_discard(&STACK_ADDRESS_TYPE);    // stack &array
        self.write_cnt(array_constructor, HeapRefOp::Dec);         // stack --
        Ok(())
    }

    /// Compiles a for - in loop
    fn compile_for_loop(self: &mut Self, item: &ast::ForLoop) -> CompileResult {
        use ast::{Expression, BinaryOperator as Op};
        // initialize iter variable
        let binding_id = item.iter.binding_id.expect("Unresolved binding encountered");
        self.init_state.push(BranchingKind::Single);
        let iter_local = self.locals.lookup(binding_id);
        self.init_state.initialize(binding_id);
        let iter_type_id = item.iter.type_id(self).unwrap();
        // handle ranges or arrays
        let result = match &item.expr { // NOTE: these need to match Resolver::resolve_for_loop
            Expression::BinaryOp(bo) if bo.op == Op::Range || bo.op == Op::RangeInclusive => {
                self.compile_for_loop_range(item, iter_local, iter_type_id)
            },
            Expression::Block(_) | Expression::Call(_) | Expression::IfBlock(_) | Expression::Literal(_) | Expression::Variable(_) => {
                self.compile_for_loop_array(item, iter_local, iter_type_id)
            },
            _ => Err(CompileError::new(item, CompileErrorKind::Internal, &self.module_path))
        };
        self.init_state.pop();
        result
    }

    /// Compiles the given function.
    fn compile_function(self: &mut Self, item: &ast::Function) -> CompileResult {

        // register function bytecode index, check if any bytecode needs fixing
        let position = self.writer.position();
        comment!(self, "\nfn {}", item.sig.ident.name);

        // create arguments in local environment
        self.init_state.push(BranchingKind::Single);
        let mut frame = StackFrame::new();
        frame.ret_size = item.sig.ret.as_ref().map_or(0, |ret| self.item_type(ret).primitive_size());
        for arg in item.sig.args.iter() {
            frame.insert(arg.binding_id.unwrap(), frame.arg_pos, LocalOrigin::Argument);
            self.init_state.initialize(arg.binding_id.unwrap());
            frame.arg_pos += self.item_type(arg).primitive_size() as StackAddress;
        }

        // create variables in local environment and reserve space on the stack
        frame.var_pos = frame.arg_pos + size_of::<StackAddress>() as StackAddress * 2;
        let arg_size = frame.arg_pos;
        let ret_size = frame.ret_size;
        self.create_stack_frame_block(item.block.as_ref().unwrap(), &mut frame);
        let var_size = frame.var_pos - (frame.arg_pos + size_of::<StackAddress>() as StackAddress * 2);
        if var_size > 0 {
            self.writer.reserve(var_size as u8);
        }

        // store call info required to compile calls to this function
        let function_id = item.function_id.unwrap();
        let call_info = CallInfo { addr: position, arg_size: frame.arg_pos };
        self.functions.insert(function_id, call_info);
        self.fix_targets(function_id, call_info);

        // push local environment on the locals stack so that it is accessible from nested compile_*
        self.locals.push(frame);
        self.compile_block(item.block.as_ref().unwrap())?;
        self.init_state.pop();
        let mut frame = self.locals.pop();

        // fix forward-to-exit jmps within the function
        let exit_address = self.writer.position();
        while let Some(jmp_address) = frame.exit_placeholder.pop() {
            self.writer.overwrite(jmp_address, |w| w.jmp(exit_address));
        }

        // exit-position: decrease argument ref-count
        // TODO: these are not contained in the root function block and are therefore not handled
        //   by decref_block_locals. That means we have to inc+decnofree the result again, wasting refcount operations.
        //   The solution would be to handle this in decref_block_locals.
        if let Some(ret) = &item.sig.ret {
            self.item_cnt(ret, true, HeapRefOp::Inc);
        }
        for arg in item.sig.args.iter() {
            let ty = self.item_type(arg);
            if ty.is_ref() {
                let local = frame.lookup(arg.binding_id.unwrap());
                comment!(self, "freeing argument {}", local.index);
                self.write_load(local.index as StackOffset, ty);
                self.write_cnt(self.get_constructor(ty), HeapRefOp::Dec);
            }
        }
        if let Some(ret) = &item.sig.ret {
            self.item_cnt(ret, true, HeapRefOp::DecNoFree);
        }

        comment!(self, "exiting fn {}", item.sig.ident.name);
        match ret_size {
            0 => self.writer.ret0(arg_size),
            1 => self.writer.ret8(arg_size),
            2 => self.writer.ret16(arg_size),
            4 => self.writer.ret32(arg_size),
            8 => self.writer.ret64(arg_size),
            _ => unreachable!(),
        };

        Ok(())
    }

    /// Compiles the given block.
    fn compile_block(self: &mut Self, item: &ast::Block) -> CompileResult {
        self.init_state.push(BranchingKind::Single);
        for statement in item.statements.iter() {
            self.compile_statement(statement)?;
        }
        if let Some(returns) = &item.returns {
            comment!(self, "block returning");
            self.compile_expression(returns)?;
            // inc result, then dec everything
            self.item_cnt(returns, true, HeapRefOp::Inc);
            self.decref_block_locals();
            self.item_cnt(returns, true, HeapRefOp::DecNoFree);
            let exit_jump = self.writer.jmp(123);
            self.locals.add_exit_placeholder(exit_jump);
        } else if let Some(result) = &item.result {
            comment!(self, "block resulting");
            self.compile_expression(result)?;
            // inc result, then dec everything
            self.item_cnt(result, true, HeapRefOp::Inc);
            self.decref_block_locals();
            self.item_cnt(result, true, HeapRefOp::DecNoFree);
        } else {
            comment!(self, "block ending");
            self.decref_block_locals();
        }

        // decrease local variable ref-count
        self.init_state.pop();
        Ok(())
    }

    /// Compiles the given literal
    fn compile_literal(self: &mut Self, item: &ast::Literal) -> CompileResult {
        use crate::frontend::ast::LiteralValue;
        comment!(self, "{}", item);
        let ty = self.item_type(item);
        match item.value {
            LiteralValue::Numeric(numeric) => self.write_literal_numeric(numeric, ty),
            LiteralValue::Bool(v) =>  {
                match ty {
                    Type::bool => { if v { self.writer.one8(); } else { self.writer.zero8(); } },
                    _ => panic!("Unexpected boolean literal type: {:?}", ty)
                };
            },
            LiteralValue::Variant(ref variant) if ty.as_enum().map_or(false, |e| e.primitive.is_some()) => {
                // primitive enums don't need to be heap allocated
                let enum_def = ty.as_enum().expect("Encountered non-enum type on enum variant");
                let enum_ty = self.type_by_id(enum_def.primitive.unwrap().0);
                let variant_value = enum_def.variant_value(&variant.ident.name).unwrap();
                self.write_literal_numeric(variant_value, enum_ty);
            },
            LiteralValue::Array(_) | LiteralValue::Struct(_) | LiteralValue::String(_) | LiteralValue::Variant(_) => {
                if item.value.is_const() {
                    // simple constant constructor, construct from const pool prototypes
                    let constructor = self.get_constructor(ty);
                    let prototype = self.store_literal_prototype(item);
                    self.writer.construct(constructor, prototype);
                } else {
                    // non-constant constructor, construct from stack
                    let type_id = item.type_id(self).unwrap();
                    let size = self.write_literal_prototype_builder(item)?;
                    self.writer.upload(size, *self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
                }
            },
        }
        Ok(())
    }

    /// Compiles the given unary operation.
    fn compile_unary_op(self: &mut Self, item: &ast::UnaryOp) -> CompileResult {
        use crate::frontend::ast::{UnaryOperator as UO, BinaryOperator};
        match item.op {
            // logical
            UO::Not => {
                self.compile_expression(&item.expr)?;
                comment!(self, "{}", item);
                self.writer.not();
            }
            // arithmetic
            UO::IncBefore | UO::DecBefore | UO::IncAfter | UO::DecAfter => {
                if let ast::Expression::Variable(var) = &item.expr {
                    comment!(self, "{}", item);
                    let load_index = {
                        let binding_id = var.binding_id.expect("Unresolved binding encountered");
                        self.locals.lookup(binding_id).index
                    };
                    let exp_type = self.item_type(&item.expr);
                    match item.op {
                        UO::IncBefore => self.write_preinc(load_index as StackOffset, &exp_type),
                        UO::DecBefore => self.write_predec(load_index as StackOffset, &exp_type),
                        UO::IncAfter => self.write_postinc(load_index as StackOffset, &exp_type),
                        UO::DecAfter => self.write_postdec(load_index as StackOffset, &exp_type),
                        _ => panic!("Internal error in operator handling"),
                    };
                } else if let ast::Expression::BinaryOp(binary_op) = &item.expr {
                    assert!(binary_op.op == BinaryOperator::IndexWrite || binary_op.op == BinaryOperator::AccessWrite, "Expected IndexWrite or AccessWrite operation");
                    self.compile_expression(&item.expr)?;           // stack: &left
                    comment!(self, "{}", item);
                    let exp_type = self.item_type(&item.expr);
                    match item.op {
                        UO::IncBefore => self.write_heap_preinc(&exp_type),
                        UO::DecBefore => self.write_heap_predec(&exp_type),
                        UO::IncAfter => self.write_heap_postinc(&exp_type),
                        UO::DecAfter => self.write_heap_postdec(&exp_type),
                        _ => panic!("Internal error in operator handling"),
                    };
                } else {
                    panic!("Operator {:?} can not be used here", item.op);
                }
            },
        }
        Ok(())
    }

    /// Compiles a simple, non-shortcutting binary operation.
    fn compile_binary_op_simple(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        // compile left, right and store references, left and right will be consumed
        self.compile_expression(&item.left)?; // stack: left
        comment!(self, "{}", item.op);
        self.compile_expression(&item.right)?; // stack: left right
        let ty_result = self.item_type(item);
        let ty_left = self.item_type(&item.left);
        match item.op {                             // stack: result
            // arithmetic/concat
            BO::Add => self.write_add(ty_result),
            BO::Sub => self.write_sub(ty_result),
            BO::Mul => self.write_mul(ty_result),
            BO::Div => self.write_div(ty_result),
            BO::Rem => self.write_rem(ty_result),
            // comparison
            BO::Greater     => { self.write_swap(ty_left); self.write_lt(ty_left); },
            BO::GreaterOrEq => { self.write_swap(ty_left); self.write_lte(ty_left); },
            BO::Less        => self.write_lt(ty_left),
            BO::LessOrEq    => self.write_lte(ty_left),
            BO::Equal       => self.write_eq(ty_left),
            BO::NotEqual    => self.write_neq(ty_left),
            _ => unreachable!("Invalid simple-operation {:?} in compile_binary_op", item.op),
        }
        Ok(())
    }

    /// Compiles a short-circuiting binary operation (and/or)
    fn compile_binary_op_shortcircuiting(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        match item.op {
            BO::And => {
                self.compile_expression(&item.left)?;
                let exit_jump = self.writer.j0_nc(123); // left is false, result cannot ever be true, skip right
                self.compile_expression(&item.right)?;
                self.writer.and();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.j0_nc(exit_target));
            },
            BO::Or => {
                self.compile_expression(&item.left)?;
                let exit_jump = self.writer.jn0_nc(123); // left is true, result cannot ever be false, skip right
                self.compile_expression(&item.right)?;
                self.writer.or();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.jn0_nc(exit_target));
            },
            _ => unreachable!("Invalid shortcircuit-operation {:?} in compile_binary_op", item.op),
        }
        Ok(())
    }

    /// Compiles an offsetting binary operation, i.e.. indexing and member access.
    fn compile_binary_op_offseting(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        self.compile_expression(&item.left)?;
        self.compile_expression(&item.right)?;
        let result_type = self.item_type(item);
        let compare_type = self.item_type(&item.left);
        match item.op {
            BO::Index => {
                comment!(self, "[{}]", &item.right);
                // fetch heap value at reference-target
                self.write_heap_fetch_element(compare_type, result_type);
            },
            BO::IndexWrite => {
                comment!(self, "[{}] (writing)", &item.right);
                // compute and push the address of the reference-target
                self.writer.index(result_type.primitive_size());
            },
            BO::Access => {
                comment!(self, ".{}", &item.right);
                // fetch heap value at reference-target
                let struct_ = compare_type.as_struct().unwrap();
                let offset = self.compute_member_offset(struct_, &item.right.as_member().unwrap().ident.name);
                self.write_heap_fetch_member(compare_type, result_type, offset);
            },
            BO::AccessWrite => {
                comment!(self, ".{} (writing)", &item.right);
                // compute and push the address of the reference-target
                let struct_ = compare_type.as_struct().unwrap();
                let offset = self.compute_member_offset(struct_, &item.right.as_member().unwrap().ident.name);
                self.write_member_offset(offset);

            },
            _ => unreachable!("Invalid offset-operation {:?} in compile_binary_op", item.op),
        }
        Ok(())
    }

    /// Compiles the given binary operation.
    fn compile_binary_op(self: &mut Self, item: &ast::BinaryOp) -> CompileResult {
        if item.op.is_simple() {
            self.compile_binary_op_simple(item)
        } else if item.op.is_shortcircuit() {
            self.compile_binary_op_shortcircuiting(item)
        } else if item.op.is_offset() {
            self.compile_binary_op_offseting(item)
        } else {
            unreachable!()
        }
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_cast(self: &mut Self, item: &ast::Cast) -> CompileResult {

        self.compile_expression(&item.expr)?;
        let from = self.item_type(&item.expr);
        let to = self.item_type(&item.ty);
        self.write_cast(from, to);
        Ok(())
    }

}

impl<T> Compiler<T> where T: VMFunc<T> {

    /// Returns the type of given AST item.
    fn item_type(self: &Self, item: &impl Typeable) -> &Type {
        match item.type_id(self) {
            None => panic!("Unresolved type encountered"),
            Some(type_id) => self.type_by_id(type_id)
        }
    }

    /// Returns constructor index for given type or 0.
    fn get_constructor(self: &Self, ty: &Type) -> StackAddress {
        let type_id = self.id_mappings.types().find(|m| m.1 == ty).unwrap().0;
        *self.constructors.get(&type_id).unwrap_or(&0)
    }

    /// Checks if the given functions are compatible.
    fn is_compatible_function(self: &Self, target: &Function, other: &Function) -> bool {
        if discriminant(&target.kind.unwrap()) != discriminant(&other.kind.unwrap()) {
            return false;
        }
        if target.ret_type != other.ret_type {
            return false;
        }
        if target.arg_type.len() != other.arg_type.len() {
            return false;
        }
        for (target_arg, other_arg) in target.arg_type.iter().zip(other.arg_type.iter()) {
            if !self.type_accepted_for(other_arg.unwrap(), target_arg.unwrap()) {
                return false;
            }
        }
        true
    }

    /// Computes struct member offset in bytes.
    fn compute_member_offset(self: &Self, struct_: &Struct, member_name: &str) -> StackAddress {
        let mut offset = 0;
        for (field_name, field_type_id) in struct_.fields.iter() {
            if field_name == member_name {
                break;
            }
            let field_type = self.type_by_id(field_type_id.expect("Unresolved struct field encountered"));
            // use reference size for references, otherwise shallow type size
            offset += field_type.primitive_size() as StackAddress;
        }
        offset
    }

    /// Fixes function call targets for previously not generated functions.
    fn fix_targets(self: &mut Self, function_id: FunctionId, info: CallInfo) {
        if let Some(targets) = self.call_placeholder.remove(&function_id) {
            let backup_position = self.writer.position();
            for &target in targets.iter() {
                self.writer.set_position(target);
                self.writer.call(info.addr, info.arg_size);
            }
            self.writer.set_position(backup_position);
        }
    }

    /// Creates stack frame variables for expressions. // FIXME move this into AST
    fn create_stack_frame_exp(self: &Self, expression: &ast::Expression, frame: &mut StackFrame) {
        if let ast::Expression::Block(block) = expression {
            self.create_stack_frame_block(block, frame);
        } else if let ast::Expression::Call(call) = expression {
            for arg in &call.args {
                if let ast::Expression::Block(block) = arg {
                    self.create_stack_frame_block(block, frame);
                }
            }
        } else if let ast::Expression::Assignment(assignment) = expression {
            if let ast::Expression::Block(block) = &assignment.right {
                self.create_stack_frame_block(block, frame);
            }
        } else if let ast::Expression::BinaryOp(binary_op) = expression {
            if let ast::Expression::Block(block) = &binary_op.left {
                self.create_stack_frame_block(block, frame);
            }
            if let ast::Expression::Block(block) = &binary_op.right {
                self.create_stack_frame_block(block, frame);
            }
        } else if let ast::Expression::UnaryOp(unary_op) = expression {
            if let ast::Expression::Block(block) = &unary_op.expr {
                self.create_stack_frame_block(block, frame);
            }
        } else if let ast::Expression::IfBlock(if_block) = expression {
            self.create_stack_frame_block(&if_block.if_block, frame);
            if let Some(block) = &if_block.else_block {
                self.create_stack_frame_block(block, frame);
            }
        }
    }

    /// Creates stack frame variables for blocks. // FIXME move this into AST
    fn create_stack_frame_block(self: &Self, item: &ast::Block, frame: &mut StackFrame) {
        // todo: this is pretty bad. need to come up with better solution. trait on ast?
        for statement in item.statements.iter() {
            if let ast::Statement::Binding(binding) = statement {
                frame.insert(binding.binding_id.unwrap(), frame.var_pos, LocalOrigin::Binding);
                frame.var_pos += self.item_type(binding).primitive_size() as StackAddress;
                if let Some(expression) = &binding.expr {
                    self.create_stack_frame_exp(expression, frame);
                }
            } else if let ast::Statement::ForLoop(for_loop) = statement {
                frame.insert(for_loop.iter.binding_id.unwrap(), frame.var_pos, LocalOrigin::Binding);
                frame.var_pos += self.item_type(&for_loop.iter).primitive_size() as StackAddress;
                self.create_stack_frame_block(&for_loop.block, frame);
            } else if let ast::Statement::WhileLoop(while_loop) = statement {
                self.create_stack_frame_block(&while_loop.block, frame);
            } else if let ast::Statement::Block(block) = statement {
                self.create_stack_frame_block(&block, frame);
            } else if let ast::Statement::IfBlock(if_block) = statement {
                self.create_stack_frame_block(&if_block.if_block, frame);
                if let Some(block) = &if_block.else_block {
                    self.create_stack_frame_block(block, frame);
                }
            } else if let ast::Statement::Expression(expression) = statement {
                self.create_stack_frame_exp(&expression, frame);
            }
        }
        if let Some(result) = &item.result {
            self.create_stack_frame_exp(result, frame);
        }
    }

    /// Writes reference counting operation on given item.
    fn item_cnt(self: &Self, item: &impl Typeable, nc: bool, op: HeapRefOp) {
        let ty = self.item_type(item);
        if ty.is_ref() {
            match nc {
                true => self.write_cnt_nc(self.get_constructor(ty), op),
                false => self.write_cnt(self.get_constructor(ty), op),
            };
        }
    }

    // FIXME: to handle ret instruction this may need to run recursively
    fn decref_block_locals(self: &mut Self) {
        let frame = self.locals.pop();
        for (&binding_id, local) in frame.map.iter() {
            if self.init_state.activated(binding_id) && self.init_state.initialized(binding_id) /*&& local.origin == LocalOrigin::Binding*/ {
                let type_id = self.binding_by_id(binding_id).type_id.unwrap();
                let ty = self.type_by_id(type_id);
                if ty.is_ref() {
                    comment!(self, "freeing local {}", local.index);
                    self.write_load(local.index as StackOffset, ty);
                    self.write_cnt(self.get_constructor(ty), HeapRefOp::Dec);
                }
            }
        }
        self.locals.push(frame);
    }
}

/// Const pool writer functions.
impl<T> Compiler<T> where T: VMFunc<T> {

    /// Stores an instance constructor on the const pool.
    fn store_constructor(self: &Self, type_id: TypeId) -> StackAddress {
        let store_len = |inner: &mut dyn FnMut()| {
            let len_position = self.writer.const_len();
            self.writer.store_const(123 as ItemIndex);
            let inner_position = self.writer.const_len();
            inner();
            let inner_len = self.writer.const_len() - inner_position;
            self.writer.update_const(len_position, inner_len as ItemIndex);
        };
        let position = self.writer.const_len();
        match self.type_by_id(type_id) {
            Type::Array(array) => {
                self.writer.store_const(Constructor::Array);
                store_len(&mut || {
                    self.store_constructor(array.type_id.expect("Unresolved array element type"));
                });
            }
            Type::Struct(structure) => {
                self.writer.store_const(Constructor::Struct);
                store_len(&mut || {
                    self.writer.store_const(*self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
                    self.writer.store_const(structure.fields.len() as ItemIndex);
                    for field in &structure.fields {
                        self.store_constructor(field.1.expect("Unresolved struct field type"));
                    }
                });
            }
            Type::String => {
                self.writer.store_const(Constructor::String);
            }
            Type::Enum(enumeration) => {
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
                            for field in fields.as_data().unwrap() {
                                self.store_constructor(field.expect("Unresolved enum field type"));
                            }
                        }
                    }
                    // write variant offsets
                    for (index, &variant_offset) in variant_offsets.iter().enumerate() {
                        let const_position = variant_offsets_pos + (index as StackAddress) * size_of::<StackAddress>() as StackAddress;
                        self.writer.update_const(const_position, variant_offset as StackAddress);
                    }
                });
            }
            Type::Trait(_) => unimplemented!("trait constructor"),
            ty @ _ => {
                self.writer.store_const(Constructor::Primitive);
                self.writer.store_const(ty.primitive_size() as ItemIndex);
            }
        }
        position
    }

    /// Stores constant literal prototype on the const pool.
    fn store_literal_prototype(self: &Self, item: &ast::Literal) -> StackAddress {
        use crate::frontend::ast::LiteralValue;
        let ty = self.item_type(item);
        let pos = self.writer.const_len();
        match &item.value {
            &LiteralValue::Numeric(int) => {
                self.store_numeric_prototype(int, ty);
            },
            &LiteralValue::Bool(boolean) =>  {
                match ty {
                    Type::bool => self.writer.store_const(if boolean { 1u8 } else { 0u8 }),
                    _ => panic!("Unexpected boolean literal type: {:?}", ty)
                };
            },
            LiteralValue::String(string_literal) => {
                self.writer.store_const(string_literal.as_str());
            },
            LiteralValue::Array(array_literal) => {
                self.writer.store_const(array_literal.elements.len() as ItemIndex);
                for element in &array_literal.elements {
                    self.store_literal_prototype(element.as_literal().unwrap());
                }
            },
            LiteralValue::Struct(struct_literal) => {
                let struct_def = ty.as_struct().expect("Expected struct, got something else");
                for (name, _) in struct_def.fields.iter() {
                    let field = struct_literal.fields.get(&name[..]).expect("Missing struct field");
                    self.store_literal_prototype(field.as_literal().unwrap());
                }
            },
            LiteralValue::Variant(variant) => {
                let enum_def = ty.as_enum().expect("Encountered non-enum type on enum variant");
                let index_type = Type::unsigned(size_of::<VariantIndex>());
                let variant_index = enum_def.variant_index(&variant.ident.name).unwrap();
                self.store_numeric_prototype(Numeric::Unsigned(variant_index as u64), &index_type);
            },
        };
        pos
    }

    /// Stores given numeric on the const pool.
    fn store_numeric_prototype(self: &Self, numeric: Numeric, ty: &Type) -> StackAddress {
        match numeric {
            Numeric::Signed(v) => {
                match ty {
                    Type::i8 => self.writer.store_const(v as i8),
                    Type::i16 => self.writer.store_const(v as i16),
                    Type::i32 => self.writer.store_const(v as i32),
                    Type::i64 => self.writer.store_const(v as i64),
                    _ => panic!("Unexpected signed integer literal type: {:?}", ty)
                }
            },
            Numeric::Unsigned(v) => {
                match ty {
                    Type::i8 | Type::u8 => self.writer.store_const(v as u8),
                    Type::i16 | Type::u16 => self.writer.store_const(v as u16),
                    Type::i32 | Type::u32 => self.writer.store_const(v as u32),
                    Type::i64 | Type::u64 => self.writer.store_const(v as u64),
                    _ => panic!("Unexpected unsigned integer literal type: {:?}", ty)
                }
            },
            Numeric::Float(v) => {
                match ty {
                    Type::f32 => self.writer.store_const(v as f32),
                    Type::f64 => self.writer.store_const(v as f64),
                    _ => panic!("Unexpected float literal type: {:?}", ty)
                }
            },
            //Numeric::Overflow => panic!("Literal computation overflow")
        }
    }
}

/// Opcode writer functions.
impl<T> Compiler<T> where T: VMFunc<T> {

    /// Writes given numeric, using const pool if necessary.
    fn write_literal_numeric(self: &Self, numeric: Numeric, ty: &Type) {
        match numeric { // todo: try to refactor this mess

            Numeric::Unsigned(0) if ty.is_integer() && ty.primitive_size() == 1 => { self.writer.zero8(); }
            Numeric::Unsigned(0) if ty.is_integer() && ty.primitive_size() == 2 => { self.writer.zero16(); }
            Numeric::Unsigned(0) if ty.is_integer() && ty.primitive_size() == 4 => { self.writer.zero32(); }
            Numeric::Unsigned(0) if ty.is_integer() && ty.primitive_size() == 8 => { self.writer.zero64(); }

            Numeric::Unsigned(1) if ty.is_integer() && ty.primitive_size() == 1 => { self.writer.one8(); }
            Numeric::Unsigned(1) if ty.is_integer() && ty.primitive_size() == 2 => { self.writer.one16(); }
            Numeric::Unsigned(1) if ty.is_integer() && ty.primitive_size() == 4 => { self.writer.one32(); }
            Numeric::Unsigned(1) if ty.is_integer() && ty.primitive_size() == 8 => { self.writer.one64(); }

            Numeric::Signed(-1) if ty.is_signed() && ty.primitive_size() == 1 => { self.writer.fill8(); }
            Numeric::Signed(-1) if ty.is_signed() && ty.primitive_size() == 2 => { self.writer.fill16(); }
            Numeric::Signed(-1) if ty.is_signed() && ty.primitive_size() == 4 => { self.writer.fill32(); }
            Numeric::Signed(-1) if ty.is_signed() && ty.primitive_size() == 8 => { self.writer.fill64(); }

            Numeric::Unsigned(val) if ty.is_integer() && ty.primitive_size() == 1 => { self.writer.literali8(val as u8); }
            Numeric::Unsigned(val) if ty.is_integer() && ty.primitive_size() == 4 && val <= u8::MAX as u64 => { self.writer.literalu32(val as u8); }

            Numeric::Signed(val) if ty.is_signed() && ty.primitive_size() == 1 => { self.writer.literali8((val as i8) as u8); }
            Numeric::Signed(val) if ty.is_signed() && ty.primitive_size() == 4 && val >= i8::MIN as i64 && val <= i8::MAX as i64 => { self.writer.literals32(val as i8); }

            _ if ty.is_integer() || ty.is_float() => {
                let address = self.store_numeric_prototype(numeric, ty);
                self.write_const(address, ty);
            },
            _ => panic!("Unexpected numeric literal type: {:?}", ty),
        }
    }

    /// Writes instructions to assemble a prototype.
    fn write_literal_prototype_builder(self: &mut Self, item: &ast::Literal) -> CompileResult<StackAddress> {
        use crate::frontend::ast::LiteralValue;
        let ty = self.item_type(item);
        Ok(match &item.value {
            LiteralValue::Numeric(_) | LiteralValue::Bool(_) => unreachable!("Invalid prototype type"),
            LiteralValue::String(_) => {
                // we cannot generate the string onto the stack since string operations (e.g. x = MyStruct { a: "hello" + "world" }) require references.
                // this would also cause a lot of copying from heap to stack and back. instead we treat strings normally and
                // later use a special constructor instruction that assumes strings are already on the heap.
                let constructor = self.get_constructor(ty);
                let prototype = self.store_literal_prototype(item);
                self.writer.construct(constructor, prototype);
                Type::String.primitive_size() as StackAddress
            },
            LiteralValue::Array(array_literal) => {
                for element in &array_literal.elements {
                    self.compile_expression(element)?;
                }
                let array_ty = self.item_type(item).as_array().expect("Expected array type, got something else");
                array_literal.elements.len() as StackAddress * self.type_by_id(array_ty.type_id.unwrap()).primitive_size() as StackAddress
            },
            LiteralValue::Struct(struct_literal) => {
                let struct_def = ty.as_struct().expect("Expected struct, got something else");
                // collect fields first to avoid borrow checker
                let fields: Vec<_> = struct_def.fields.iter().map(|(name, _)| struct_literal.fields.get(name).expect("Missing struct field")).collect();
                for field in fields {
                    self.compile_expression(field)?;
                }
                let struct_ty = self.item_type(item).as_struct().expect("Expected struct type, got something else");
                struct_ty.fields.iter().fold(0, |acc, f| acc + self.type_by_id(f.1.unwrap()).primitive_size() as StackAddress)
            },
            LiteralValue::Variant(variant) => {
                let enum_def = self.item_type(item).as_enum().expect("Encountered non-enum type on enum variant");
                let index_type = Type::unsigned(size_of::<VariantIndex>());
                let variant_index = enum_def.variant_index(&variant.ident.name).unwrap();
                self.write_literal_numeric(Numeric::Unsigned(variant_index as u64), &index_type);
                index_type.primitive_size() as StackAddress
            },
        })
    }

    /// Writes a primitive cast.
    fn write_cast(self: &Self, from: &Type, to: &Type) {
        if from.is_signed() && !to.is_signed() && !to.is_float() && !to.is_string() {
            self.write_zclamp(from);
        }
        if from.is_integer() && to.is_integer() {
            self.write_integer_cast(from, to);
        } else if from.is_float() && to.is_float() {
            self.write_float_integer_cast(from, to);
        } else if from.is_float() && to.is_integer() {
            let temp_to = if to.is_signed() { &Type::i64 } else { &Type::u64 };
            self.write_float_integer_cast(from, temp_to);
            if to.primitive_size() != 8 {
                self.write_integer_cast(temp_to, to);
            }
        } else if from.is_integer() && to.is_float() {
            let temp_from = if from.is_signed() { &Type::i64 } else { &Type::u64 };
            if from.primitive_size() != 8 {
                self.write_integer_cast(from, temp_from);
            }
            self.write_float_integer_cast(temp_from, to);
        } else if from.is_integer() && to.is_string() {
            let temp_from = if from.is_signed() { &Type::i64 } else { &Type::u64 };
            if from.primitive_size() != 8 {
                self.write_integer_cast(from, temp_from);
            }
            match temp_from {
                Type::i64 => self.writer.i64_to_string(),
                Type::u64 => self.writer.u64_to_string(),
                _ => unreachable!(),
            };
        } else if from == &Type::f32 && to.is_string() {
            self.writer.f32_to_string();
        } else if from == &Type::f64 && to.is_string() {
            self.writer.f64_to_string();
        } else if let Some(Enum { primitive: Some((primitive, _)), .. }) = from.as_enum() {
            let from = self.type_by_id(*primitive);
            self.write_cast(from, to);
        } else if from != to {
            unreachable!("Invalid cast {:?} to {:?}", from, to);
        }
    }

    /// Writes a cast from one float to another or from/to a 64 bit integer.
    fn write_float_integer_cast(self: &Self, from: &Type, to: &Type) {
        match (from, to) {
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
            _ => unreachable!("Invalid float/int cast {:?} to {:?}", from, to),
        };
    }

    /// Writes a cast from one integer to another.
    fn write_integer_cast(self: &Self, from: &Type, to: &Type) {
        let from_size = (from.primitive_size() * 8) as u8;
        let to_size = (to.primitive_size() * 8) as u8;
        if to_size < from_size || (to_size == from_size && !from.is_signed() && to.is_signed()) {
            if to.is_signed() {
                match from_size {
                    64 => self.writer.trims64(to_size),
                    32 => self.writer.trims32(to_size),
                    16 => self.writer.trims16(to_size),
                    _ => unreachable!("Invalid integer cast {:?} to {:?}", from, to),
                };
            } else {
                match from_size {
                    64 => self.writer.trimu64(to_size),
                    32 => self.writer.trimu32(to_size),
                    16 => self.writer.trimu16(to_size),
                    _ => unreachable!("Invalid integer cast {:?} to {:?}", from, to),
                };
            }
        } else if to_size > from_size {
            if from.is_signed() {
                match from_size {
                    32 => self.writer.extends32(to_size),
                    16 => self.writer.extends16(to_size),
                    8 => self.writer.extends8(to_size),
                    _ => unreachable!("Invalid integer cast {:?} to {:?}", from, to),
                };
            } else {
                match from_size {
                    32 => self.writer.extendu32(to_size),
                    16 => self.writer.extendu16(to_size),
                    8 => self.writer.extendu8(to_size),
                    _ => unreachable!("Invalid integer cast {:?} to {:?}", from, to),
                };
            }
        }
    }

    /// Writes an appropriate variant of the cnt_nc instruction.
    fn write_cnt_nc(self: &Self, constructor: StackAddress, op: HeapRefOp) {
        select_unsigned_opcode!(self, cnt_8_nc, cnt_16_nc, cnt_sa_nc, constructor, op);
    }

    /// Writes an appropriate variant of the cnt instruction.
    fn write_cnt(self: &Self, constructor: StackAddress, op: HeapRefOp) {
        select_unsigned_opcode!(self, cnt_8, cnt_16, cnt_sa, constructor, op);
    }

    /// Writes instructions to compute member offset for access on a struct.
    fn write_member_offset(self: &Self, offset: StackAddress) {
        if offset > 0 {
            select_signed_opcode!(self, offsetx_8, offsetx_16, offsetx_sa, offset as StackOffset);
        }
    }

    /// Writes an appropriate variant of the const instruction.
    fn write_const(self: &Self, index: StackAddress, ty: &Type) {
        match ty.primitive_size() {
            2 => select_unsigned_opcode!(self, const16_8, const16_16, const16_sa, index),
            4 => select_unsigned_opcode!(self, const32_8, const32_16, const32_sa, index),
            8 => select_unsigned_opcode!(self, const64_8, const64_16, const64_sa, index),
            //16 => opcode_unsigned!(self, const128_8, const128_16, const128_sa, index),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Writes instructions to fetch a value of the given size from the target of the top heap reference on the stack.
    fn write_heap_fetch(self: &Self, ty: &Type) {
        match ty.primitive_size() {
            1 => { self.writer.heap_fetch8(); },
            2 => { self.writer.heap_fetch16(); },
            4 => { self.writer.heap_fetch32(); },
            8 => { self.writer.heap_fetch64(); },
            //16 => { self.writer.heap_fetch128(); },
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        }
    }

    /// Writes instructions to put a value of the given type at the target of the top heap reference on the stack.
    fn write_heap_put(self: &Self, ty: &Type) -> StackAddress {
        match ty.primitive_size() {
            1 => self.writer.heap_put8(),
            2 => self.writer.heap_put16(),
            4 => self.writer.heap_put32(),
            8 => self.writer.heap_put64(),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        }
    }

    /// Writes instructions to put a value of the given type at the target of the top heap reference on the stack.
    /// If the value being written is a heap reference itself, its refcount will be increased and unless is_new_heap_ref is true
    /// and the replaced value will have its refcount decreased.
    fn write_heap_putx(self: &Self, ty: &Type, is_new_heap_ref: bool) -> StackAddress {
        if ty.is_ref() {
            let constructor = self.get_constructor(ty);
            if !is_new_heap_ref {
                self.writer.heap_putx_replace(constructor)
            } else {
                self.writer.heap_putx_new(constructor)
            }
        } else {
            self.write_heap_put(ty)
        }
    }

    /// Writes instructions to fetch a member of the struct whose reference is at the top of the stack.
    fn write_heap_fetch_member(self: &Self, container_type: &Type, result_type: &Type, offset: StackAddress) {
        let constructor = self.get_constructor(container_type);
        match result_type.primitive_size() {
            1 => { self.writer.heap_fetch_member8(offset, constructor); },
            2 => { self.writer.heap_fetch_member16(offset, constructor); },
            4 => { self.writer.heap_fetch_member32(offset, constructor); },
            8 => { self.writer.heap_fetch_member64(offset, constructor); },
            //16 => { self.writer.heap_fetch_member128(offset); },
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, result_type),
        }
    }

    /// Writes instructions to fetch an element of the array whose reference is at the top of the stack.
    fn write_heap_fetch_element(self: &Self, container_type: &Type, result_type: &Type) {
        let constructor = self.get_constructor(container_type);
        match result_type.primitive_size() {
            1 => { self.writer.heap_fetch_element8(constructor); },
            2 => { self.writer.heap_fetch_element16(constructor); },
            4 => { self.writer.heap_fetch_element32(constructor); },
            8 => { self.writer.heap_fetch_element64(constructor); },
            //16 => { self.writer.heap_fetch_element128(); },
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, result_type),
        }
    }

    /// Writes instructions to fetch an element from the end of the array whose reference is at the top of the stack.
    fn write_heap_tail_element_nc(self: &Self, container_type: &Type, result_type: &Type) {
        let constructor = self.get_constructor(container_type);
        match result_type.primitive_size() {
            1 => { self.writer.heap_tail_element8_nc(constructor); },
            2 => { self.writer.heap_tail_element16_nc(constructor); },
            4 => { self.writer.heap_tail_element32_nc(constructor); },
            8 => { self.writer.heap_tail_element64_nc(constructor); },
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, result_type),
        }
    }

    /// Writes an appropriate variant of the store instruction.
    fn write_store(self: &Self, local: Local, ty: &Type) -> StackAddress {
        match ty.primitive_size() {
            1 => select_signed_opcode!(self, store8_8, store8_16, store8_sa, local.index as StackOffset),
            2 => select_signed_opcode!(self, store16_8, store16_16, store16_sa, local.index as StackOffset),
            4 => select_signed_opcode!(self, store32_8, store32_16, store32_sa, local.index as StackOffset),
            8 => select_signed_opcode!(self, store64_8, store64_16, store64_sa, local.index as StackOffset),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        }
    }

    /// Writes an appropriate variant of the store instruction.
    /// If the value being written is a heap reference, its refcount will be increased and unless the local is not active
    /// and the replaced value will have its refcount decreased.
    fn write_storex(self: &Self, local: Local, item: &impl Typeable, binding_id: BindingId) -> StackAddress {
        let ty = self.item_type(item);
        if ty.is_ref() {
            let constructor = self.get_constructor(ty);
            if self.init_state.initialized(binding_id) {
                self.writer.storex_replace(local.index as StackOffset, constructor)
            } else {
                self.writer.storex_new(local.index as StackOffset, constructor)
            }
        } else {
            self.write_store(local, ty)
        }
    }

    /// Writes an appropriate variant of the load instruction.
    fn write_load(self: &Self, index: StackOffset, ty: &Type) {
        match ty.primitive_size() {
            1 => select_signed_opcode!(self, load8_8, load8_16, load8_sa, index),
            2 => select_signed_opcode!(self, load16_8, load16_16, load16_sa, index),
            4 => match index {
                ARG1 => self.writer.load_arg1(),
                ARG2 => self.writer.load_arg2(),
                ARG3 => self.writer.load_arg3(),
                _ => select_signed_opcode!(self, load32_8, load32_16, load32_sa, index),
            },
            8 => select_signed_opcode!(self, load64_8, load64_16, load64_sa, index),
            //16 => opcode_signed!(self, load128_8, load128_16, load128_sa, index),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Discard topmost stack value and drop temporaries for reference types.
    fn write_discard(self: &Self, ty: &Type) {
        comment!(self, "discarding result");
        if ty.is_ref() {
            let constructor = self.get_constructor(ty);
            self.write_cnt_nc(constructor, HeapRefOp::Free);
        }
        match ty.primitive_size() {
            0 => 0,
            1 => self.writer.discard8(),
            2 => self.writer.discard16(),
            4 => self.writer.discard32(),
            8 => self.writer.discard64(),
            //16 => self.writer.discard128(),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Swap topmost 2 stack values.
    fn write_swap(self: &Self, ty: &Type) {
        match ty.primitive_size() {
            1 => self.writer.swap8(),
            2 => self.writer.swap16(),
            4 => self.writer.swap32(),
            8 => self.writer.swap64(),
            //16 => self.writer.swap128(),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Clone stack value at (negative of) given offset to the top of the stack.
    fn write_clone(self: &Self, ty: &Type) -> StackAddress {
        match ty.primitive_size() {
            1 => self.writer.clone8(),
            2 => self.writer.clone16(),
            4 => self.writer.clone32(),
            8 => self.writer.clone64(),
            //16 => self.writer.clone128(offset),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        }
    }

    fn write_clone_ref(self: &Self) -> StackAddress {
        match size_of::<crate::HeapAddress>() {
            1 => self.writer.clone8(),
            2 => self.writer.clone16(),
            4 => self.writer.clone32(),
            8 => self.writer.clone64(),
            //16 => self.writer.clone128(offset),
            size @ _ => unreachable!("Unsupported size {} for heap address", size),
        }
    }

    /// Writes a call instruction. If the function address is not known yet, a placeholder will be written.
    fn write_call(self: &mut Self, function_id: FunctionId) -> StackAddress {
        let target = if let Some(&target) = self.functions.get(&function_id) {
            target
        } else {
            let call_position = self.writer.position();
            self.call_placeholder.entry(function_id).or_insert(Vec::new()).push(call_position);
            CallInfo::PLACEHOLDER
        };
        self.writer.call(target.addr, target.arg_size)
    }

    /// Writes instructions for build-in len method.
    fn write_builtin(self: &Self, builtin: BuiltinGroup, ty: &Type) {
        let constructor = self.get_constructor(ty);
        #[allow(unreachable_patterns)]
        match ty {
            &Type::Array(Array { type_id }) => {
                let inner_ty = self.type_by_id(type_id.unwrap());
                match builtin {
                    BuiltinGroup::ArrayLen => {
                        self.writer.heap_size(constructor);
                        self.writer.shrsa(match inner_ty.primitive_size() {
                            1 => 0,
                            2 => 1,
                            4 => 2,
                            8 => 3,
                            _ => unreachable!("Unsupported inner size for type {} for builtin group {:?}", ty, builtin),
                        });
                    }
                    BuiltinGroup::ArrayPush => select_builtin!(self, inner_ty, array_push8, array_push16, array_push32, array_push64, array_pushx),
                    BuiltinGroup::ArrayPop => select_builtin!(self, inner_ty, array_pop8, array_pop16, array_pop32, array_pop64, array_popx),
                    BuiltinGroup::ArrayTruncate => select_builtin!(self, inner_ty, array_truncate8, array_truncate16, array_truncate32, array_truncate64, array_truncatex),
                    BuiltinGroup::ArrayRemove => select_builtin!(self, inner_ty, array_remove8, array_remove16, array_remove32, array_remove64, array_removex),
                    _ => unreachable!("Unsupported type {} for builtin group {:?}", ty, builtin),
                }
            }
            _ => unreachable!("Unsupported type {}", ty),
        }
    }

    /// Writes clamp to zero instruction.
    fn write_zclamp(self: &Self, ty: &Type) {
        match ty {
            Type::f32 => self.writer.zclampf32(),
            Type::f64 => self.writer.zclampf64(),
            Type::i8 => self.writer.zclampi8(),
            Type::i16 => self.writer.zclampi16(),
            Type::i32 => self.writer.zclampi32(),
            Type::i64 => self.writer.zclampi64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write decrement instruction.
    fn write_dec(self: &Self, ty: &Type) -> StackAddress {
        match ty {
            Type::i64 | Type::u64 => self.writer.deci64(1),
            Type::i32 | Type::u32 => self.writer.deci32(1),
            Type::i16 | Type::u16 => self.writer.deci16(1),
            Type::i8 | Type::u8 => self.writer.deci8(1),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        }
    }

    /// Write pre-increment instruction.
    fn write_preinc(self: &Self, index: StackOffset, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.predeci64(index, -1),
            Type::i32 | Type::u32 => self.writer.predeci32(index, -1),
            Type::i16 | Type::u16 => self.writer.predeci16(index, -1),
            Type::i8 | Type::u8 => self.writer.predeci8(index, -1),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write pre-decrement instruction.
    fn write_predec(self: &Self, index: StackOffset, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.predeci64(index, 1),
            Type::i32 | Type::u32 => self.writer.predeci32(index, 1),
            Type::i16 | Type::u16 => self.writer.predeci16(index, 1),
            Type::i8 | Type::u8 => self.writer.predeci8(index, 1),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write post-increment instruction.
    fn write_postinc(self: &Self, index: StackOffset, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.postdeci64(index, -1),
            Type::i32 | Type::u32 => self.writer.postdeci32(index, -1),
            Type::i16 | Type::u16 => self.writer.postdeci16(index, -1),
            Type::i8 | Type::u8 => self.writer.postdeci8(index, -1),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write post-decrement instruction.
    fn write_postdec(self: &Self, index: StackOffset, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.postdeci64(index, 1),
            Type::i32 | Type::u32 => self.writer.postdeci32(index, 1),
            Type::i16 | Type::u16 => self.writer.postdeci16(index, 1),
            Type::i8 | Type::u8 => self.writer.postdeci8(index, 1),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write heap pre-increment instruction.
    fn write_heap_preinc(self: &Self, ty: &Type) -> StackAddress {
        match ty {
            Type::i64 | Type::u64 => self.writer.heap_predeci64(-1),
            Type::i32 | Type::u32 => self.writer.heap_predeci32(-1),
            Type::i16 | Type::u16 => self.writer.heap_predeci16(-1),
            Type::i8 | Type::u8 => self.writer.heap_predeci8(-1),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        }
    }

    /// Write heap pre-decrement instruction.
    fn write_heap_predec(self: &Self, ty: &Type) -> StackAddress {
        match ty {
            Type::i64 | Type::u64 => self.writer.heap_predeci64(1),
            Type::i32 | Type::u32 => self.writer.heap_predeci32(1),
            Type::i16 | Type::u16 => self.writer.heap_predeci16(1),
            Type::i8 | Type::u8 => self.writer.heap_predeci8(1),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        }
    }

    /// Write heap post-increment instruction.
    fn write_heap_postinc(self: &Self, ty: &Type) -> StackAddress {
        match ty {
            Type::i64 | Type::u64 => self.writer.heap_postdeci64(-1),
            Type::i32 | Type::u32 => self.writer.heap_postdeci32(-1),
            Type::i16 | Type::u16 => self.writer.heap_postdeci16(-1),
            Type::i8 | Type::u8 => self.writer.heap_postdeci8(-1),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        }
    }

    /// Write heap post-decrement instruction.
    fn write_heap_postdec(self: &Self, ty: &Type) -> StackAddress {
        match ty {
            Type::i64 | Type::u64 => self.writer.heap_postdeci64(1),
            Type::i32 | Type::u32 => self.writer.heap_postdeci32(1),
            Type::i16 | Type::u16 => self.writer.heap_postdeci16(1),
            Type::i8 | Type::u8 => self.writer.heap_postdeci8(1),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        }
    }

    /// Write subtraction instruction.
    fn write_sub(self: &Self, ty: &Type) {
        match ty {
            Type::i8 | Type::u8 => self.writer.subi8(),
            Type::i16 | Type::u16 => self.writer.subi16(),
            Type::i32 | Type::u32 => self.writer.subi32(),
            Type::i64 | Type::u64 => self.writer.subi64(),
            Type::f32 => self.writer.subf32(),
            Type::f64 => self.writer.subf64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write addition instruction.
    fn write_add(self: &Self, ty: &Type) {
        match ty {
            Type::i8 | Type::u8 => self.writer.addi8(),
            Type::i16 | Type::u16 => self.writer.addi16(),
            Type::i32 | Type::u32 => self.writer.addi32(),
            Type::i64 | Type::u64 => self.writer.addi64(),
            Type::f32 => self.writer.addf32(),
            Type::f64 => self.writer.addf64(),
            Type::String => self.writer.string_concatx(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write multiplication instruction.
    fn write_mul(self: &Self, ty: &Type) {
        match ty {
            Type::i8 | Type::u8 => self.writer.muli8(),
            Type::i16 | Type::u16 => self.writer.muli16(),
            Type::i32 | Type::u32 => self.writer.muli32(),
            Type::i64 | Type::u64 => self.writer.muli64(),
            Type::f32 => self.writer.mulf32(),
            Type::f64 => self.writer.mulf64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write division instruction.
    fn write_div(self: &Self, ty: &Type) {
        match ty {
            Type::i8 => self.writer.divs8(),
            Type::u8 => self.writer.divu8(),
            Type::i16 => self.writer.divs16(),
            Type::u16 => self.writer.divu16(),
            Type::i32 => self.writer.divs32(),
            Type::u32 => self.writer.divu32(),
            Type::i64 => self.writer.divs64(),
            Type::u64 => self.writer.divu64(),
            Type::f32 => self.writer.divf32(),
            Type::f64 => self.writer.divf64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write remainder instruction.
    fn write_rem(self: &Self, ty: &Type) {
        match ty {
            Type::i8 => self.writer.rems8(),
            Type::u8 => self.writer.remu8(),
            Type::i16 => self.writer.rems16(),
            Type::u16 => self.writer.remu16(),
            Type::i32 => self.writer.rems32(),
            Type::u32 => self.writer.remu32(),
            Type::i64 => self.writer.rems64(),
            Type::u64 => self.writer.remu64(),
            Type::f32 => self.writer.remf32(),
            Type::f64 => self.writer.remf64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write compare equal instruction.
    fn write_eq(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty.primitive_size() {
                1 => self.writer.ceq8(),
                2 => self.writer.ceq16(),
                4 => self.writer.ceq32(),
                8 => self.writer.ceq64(),
                size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
            };
        } else if ty.is_string() {
            self.writer.string_ceq();
        } else {
            unimplemented!("general heap compare not yet implemented");
        }
    }

    /// Write compare not equal instruction.
    fn write_neq(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty.primitive_size() {
                1 => self.writer.cneq8(),
                2 => self.writer.cneq16(),
                4 => self.writer.cneq32(),
                8 => self.writer.cneq64(),
                size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
            };
        } else if ty.is_string() {
            self.writer.string_cneq();
        } else {
            unimplemented!("general heap compare not yet implemented");
        }
    }

    /// Write compare less than instruction.
    fn write_lt(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty {
                Type::i8 => self.writer.clts8(),
                Type::u8 => self.writer.cltu8(),
                Type::i16 => self.writer.clts16(),
                Type::u16 => self.writer.cltu16(),
                Type::i32 => self.writer.clts32(),
                Type::u32 => self.writer.cltu32(),
                Type::i64 => self.writer.clts64(),
                Type::u64 => self.writer.cltu64(),
                Type::f32 => self.writer.cltf32(),
                Type::f64 => self.writer.cltf64(),
                _ => unreachable!("Unsupported operation for type {:?}", ty),
            };
        } else if ty.is_string() {
            self.writer.string_clt();
        } else {
            panic!("unsupported type")
        }
    }

    /// Write compare less than or equal instruction.
    fn write_lte(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty {
                Type::i8 => self.writer.cltes8(),
                Type::u8 => self.writer.clteu8(),
                Type::i16 => self.writer.cltes16(),
                Type::u16 => self.writer.clteu16(),
                Type::i32 => self.writer.cltes32(),
                Type::u32 => self.writer.clteu32(),
                Type::i64 => self.writer.cltes64(),
                Type::u64 => self.writer.clteu64(),
                Type::f32 => self.writer.cltef32(),
                Type::f64 => self.writer.cltef64(),
                _ => unreachable!("Unsupported operation for type {:?}", ty),
            };
        } else if ty.is_string() {
            self.writer.string_clte();
        } else {
            panic!("unsupported type")
        }
    }
}

/// Itsy-trait support functions.
impl<T> Compiler<T> {

    /// Computes the vtable function base-offset for the given function. To get the final offset the dynamic types trait_implementor_index * sizeof(StackAddress) has to be added.
    fn vtable_function_offset(self: &Self, function_id: FunctionId) -> StackAddress {
        let trait_function_id = *self.trait_function_implementors.get(&function_id).unwrap_or(&function_id);
        let function_index = *self.trait_function_indices.get(&trait_function_id).expect("Invalid trait function id");
        self.trait_vtable_base + (function_index as usize * size_of::<StackAddress>() * self.trait_implementor_indices.len()) as StackAddress
    }

    /// Generate flat list of all traits and their functions
    fn filter_trait_functions(id_mappings: &IdMappings) -> Vec<(TypeId, &String, FunctionId)> {
        id_mappings.traits()
            .flat_map(|(type_id, trt)| {
                trt.provided.iter().map(move |(function_name, function_id)| (type_id, function_name, function_id.unwrap()))
                .chain(trt.required.iter().map(move |(function_name, function_id)| (type_id, function_name, function_id.unwrap())))
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
    fn map_trait_function_implementors(trait_functions: &Vec<(TypeId, &String, FunctionId)>, trait_implementors: &Vec<(TypeId, &Map<TypeId, ImplTrait>)>) -> UnorderedMap<FunctionId, FunctionId> {
        let mut trait_function_implementors = UnorderedMap::new();
        for &(trait_type_id, function_name, trait_function_id) in trait_functions.iter() {
            for &(_, implementor_traits) in trait_implementors.iter() {
                if let Some(impl_trait) = implementor_traits.get(&trait_type_id) {
                    if let Some(&implementor_function_id) = impl_trait.functions.get(function_name) {
                        trait_function_implementors.insert(implementor_function_id.expect("Unresolved implementor function"), trait_function_id);
                    }
                }
            }
        }
        trait_function_implementors
    }

    /// Create table of all trait implementors/concrete function id permutations (implementor or trait-provided).
    /// To facility direct lookups, this table contains *all* implementor/trait permuations, including those that are not implemented.
    /// This allows for the lookup position to be computed as `table_start + function_index * num_trait_implementors + trait_implementor_index` (sizeof<ItemIndex> multipliers omitted)
    /// Since the first three components are statically known their result can be statically supplied to vcall. The implementor index is stored
    /// on the heap object the function is being called on.
    fn select_trait_function_implementations(trait_functions: &Vec<(TypeId, &String, FunctionId)>, trait_implementors: &Vec<(TypeId, &Map<TypeId, ImplTrait>)>) -> Vec<(usize, Option<FunctionId>)> {
        let mut trait_implementation_mapping = Vec::new();
        for &(trait_type_id, function_name, trait_function_id) in trait_functions.iter() {
            for (implementor_index, &(_, implementor_traits)) in trait_implementors.iter().enumerate() {
                trait_implementation_mapping.push((implementor_index, match implementor_traits.get(&trait_type_id) {
                    Some(impl_trait) => *impl_trait.functions.get(function_name).unwrap_or(&Some(trait_function_id)),
                    None => None,
                }));
            }
        }
        trait_implementation_mapping
    }
}

/// Support TypeContainer for Bindings so that methods that need to follow type_ids can be implemented once and be used in both
/// the Resolver where types are scored in Scopes and the Compiler where types are a stored in a Vec.
impl<T> TypeContainer for Compiler<T> {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type {
        let index: usize = type_id.into();
        &self.id_mappings.type_map[index]
    }
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type {
        let index: usize = type_id.into();
        &mut self.id_mappings.type_map[index]
    }
    fn type_flat_name(self: &Self, _type_id: TypeId) -> Option<&String> {
        None // TODO
    }
}

/// A container holding binding id to BindingInfo mappings
#[cfg(feature="compiler")]
impl<T> BindingContainer for Compiler<T> {
    fn binding_by_id(self: &Self, binding_id: BindingId) -> &Binding {
        let binding_index = Into::<usize>::into(binding_id);
        &self.id_mappings.binding_map[binding_index]
    }
    fn binding_by_id_mut(self: &mut Self, binding_id: BindingId) -> &mut Binding {
        let binding_index = Into::<usize>::into(binding_id);
        &mut self.id_mappings.binding_map[binding_index]
    }
}