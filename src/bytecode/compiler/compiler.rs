//! Bytecode emitter. Compiles bytecode from AST.

mod stack_frame;
mod macros;
pub mod error;
mod placeholder;
mod init_state;

use crate::config::FrameAddress;
use crate::{prelude::*, HeapAddress};
use crate::{StackAddress, ItemIndex, VariantIndex};
use crate::shared::{MetaContainer, numeric::Numeric, meta::{Type, ImplTrait, Struct, Array, Enum, Function, FunctionKind, Binding, Constant, ConstantValue, UserConstValue}, typed_ids::{BindingId, FunctionId, TypeId, ConstantId}};
use crate::frontend::{ast::{self, Typeable, TypeName, ControlFlow, Positioned}, resolver::resolved::{ResolvedProgram, Resolved}, walker::AstVisitor, parser::types::ParsedModule};
use crate::bytecode::{Constructor, GEN_PRIMITIVE_CTOR, Writer, StoreConst, Program, VMFunc, HeapRefOp, ConstDescriptor, builtins::{Builtin, BuiltinType}};
#[cfg(feature="call_function")]
use crate::bytecode::call_function::build_function_table;
use stack_frame::{StackFrame, StackFrames};
use error::{CompileError, CompileErrorKind, CompileResult, OptionToCompileError};
use placeholder::{LoopControlStack, LoopControl, Functions, ForLoopStack, ForLoopCleanup};
use init_state::{InitState, BranchingKind, BranchingPath, BranchingScope, BranchingState};
use macros::{comment, select_integer_type, select_numeric_type, select_numeric_cast_type, select_primitive_size};

/// Bytecode emitter. Compiles bytecode from resolved program (AST).
pub(crate) struct Compiler<'ast, T> {
    /// Bytecode writer used to output to.
    pub(crate) writer: Writer<T>,
    /// Program metadata, e.g. types, functions,...
    resolved: Resolved,
    /// Maps from binding id to load-argument for each frame.
    locals: StackFrames,
    /// Non-primitive type constructors.
    constructors: UnorderedMap<TypeId, StackAddress>,
    /// Tracks call and function addresses so that calls made before the function address was known can be fixed.
    pub(crate) functions: Functions,
    /// Tracks variable initialization state.
    init_state: InitState,
    /// Tracks loop break/continue exit jump targets.
    loop_control: LoopControlStack,
    /// Stack of active for-loop cleanup requirements (for `return` handling).
    for_loop_cleanup: ForLoopStack,
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
    /// Reference-type user consts collected during the first pass.
    /// Each entry: (constant_id, const pool offset of the reserved heap address slot).
    user_const_heap_refs: Vec<(ConstantId, StackAddress)>,
    /// Reference to the program modules (for finding ConstDef AST nodes during init code emission).
    modules_ref: Option<&'ast [ParsedModule]>,
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
///     fn print(&mut context, value: str) {
///         println!("print: {}", value);
///     }
///     // ... more functions ...
/// });
///
/// fn main() {
///     let mut state = parser::LinkState::new();
///     let module = parser::parse_module(&mut state, "
///         // An Itsy program that calls the Rust 'print' function.
///         fn main() {
///             MyAPI::print(\"Hello from Itsy!\");
///         }
///     ", "").unwrap();
///     let mut parsed = parser::ParsedProgram::new();
///     parsed.add_module(module);
///     parsed.set_link_state(state);
///     let resolved = resolver::resolve::<MyAPI>(parsed, "main").unwrap();
///     let compiled = compiler::compile(resolved).unwrap();
/// }
/// ```
///
/// The returned [Program] is now ready to be run by [run](crate::run) or [VM::run](crate::runtime::VM::run).
pub fn compile<T>(program: ResolvedProgram<T>) -> CompileResult<Program<T>> where T: VMFunc<T> {

    let ResolvedProgram { modules, resolved, entry_fn, .. } = program;

    // find all trait functions and all trait implementors
    let trait_functions = Compiler::<T>::filter_trait_functions(&resolved)?;
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
        for_loop_cleanup            : ForLoopStack::new(),
        module_path                 : "".to_string(),
        trait_vtable_base           : (trait_implementors.len() * size_of::<StackAddress>()) as StackAddress, // offset vtable by size of mapping from constructor => implementor-index
        trait_function_indices      : Compiler::<T>::enumerate_trait_function_indices(&trait_functions),
        trait_implementor_indices   : Compiler::<T>::enumerate_trait_implementor_indices(&trait_implementors),
        trait_function_implementors : Compiler::<T>::map_trait_function_implementors(&resolved, &trait_functions, &trait_implementors)?,
        user_const_heap_refs        : Vec::new(),
        modules_ref                 : None,
        resolved,
    };

    // reserve space for the trait vtable as well as an implementor-index => constructor mapping (required for trait object reference counting) in const pool
    let vtable_size = compiler.trait_function_indices.len() * compiler.trait_implementor_indices.len() * size_of::<StackAddress>();
    compiler.writer.reserve_const_data(compiler.trait_vtable_base + vtable_size as StackAddress);

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

    // patch generator carrier constructors with their value/key constructor offsets now that all
    // constructors exist (they were emitted as forward-reference placeholders in store_constructor)
    let generator_carriers: Vec<(StackAddress, (Option<TypeId>, TypeId))> = compiler.constructors.iter()
        .filter_map(|(&type_id, &position)| compiler.generator_signature(type_id).map(|sig| (position, sig)))
        .collect();
    for (position, (key_type_id, value_type_id)) in generator_carriers {
        let value_ctor = compiler.generator_slot_constructor(Some(value_type_id))?;
        let key_ctor = compiler.generator_slot_constructor(key_type_id)?;
        let value_ctor_pos = position + size_of::<Constructor>() as StackAddress; // skip the 1-byte Generator op
        compiler.writer.update_const(value_ctor_pos, value_ctor);
        compiler.writer.update_const(value_ctor_pos + size_of::<StackAddress>() as StackAddress, key_ctor);
    }

    // Collect all reference-type user consts and reserve const pool space for their heap addresses.
    // This must happen before compiling the program so that const references resolve correctly.
    compiler.modules_ref = Some(&modules);
    compiler.collect_user_const_heap_refs()?;

    // Emit pre-main initialization code: upload each reference-type const to the heap.
    // This runs before the entry function call.
    compiler.write_const_init()?;

    // write placeholder jump to program entry
    let initial_pos = compiler.writer.call(123, 0);

    // Emit post-main cleanup code: decrement refcounts for reference-type consts.
    // This runs after the entry function returns, before exit.
    compiler.write_const_cleanup()?;

    // the exit following the entry call doubles as the return target for host-initiated calls
    // (VM::call_function): a function returning here breaks the exec loop back to the host
    #[cfg(feature="call_function")]
    let host_return_addr = compiler.writer.position();
    compiler.writer.exit();

    // compile program
    for module in &modules {
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

    // build the host-callable function table from all top-level (free) functions so a host can invoke
    // them by name via VM::call_function (see bytecode::call_function)
    #[cfg(feature="call_function")]
    let functions = build_function_table(&compiler, &modules)?;

    // return generated program
    #[cfg_attr(not(feature="call_function"), allow(unused_mut))]
    let mut program = compiler.writer.into_program();
    #[cfg(feature="call_function")]
    {
        program.functions = functions;
        program.host_return_addr = host_return_addr;
    }
    #[cfg(feature = "optimizer")]
    {
        let vtable_size = compiler.trait_function_indices.len()
            * compiler.trait_implementor_indices.len()
            * size_of::<StackAddress>();
        if vtable_size > 0 {
            use crate::bytecode::VtableRegion;
            program.vtable_region = Some(VtableRegion {
                base: compiler.trait_vtable_base,
                size: vtable_size as StackAddress,
            });
        }
    }
    Ok(program)
}

/// Methods for compiling individual code structures.
impl<'ast, T> Compiler<'ast, T> where T: VMFunc<T> {

    /// Compiles the given statement.
    fn compile_statement(self: &mut Self, item: &ast::Statement) -> CompileResult {
        use self::ast::Statement as S;
        match item {
            S::StructDef(_) => Ok(()),
            S::Module(_) => Ok(()),
            S::UseDecl(_) => Ok(()),
            S::EnumDef(_) => Ok(()),
            S::Function(function) => self.compile_function(function),
            S::ImplBlock(impl_block) => {
                for function in &impl_block.functions {
                    self.compile_function(function)?;
                    // if this is a trait impl check compatibility to trait
                    if let Some(TypeName { type_id, .. }) = impl_block.trt {
                        let trait_type_id = type_id.ice()?;
                        let trt = self.ty(&trait_type_id).as_trait().ice()?;
                        // intrinsic traits whose method signatures are impl-defined (e.g. `Index`) carry only
                        // placeholder signatures; their impls vary per type and are never virtually dispatched,
                        // so there is nothing meaningful to check against.
                        if trt.impl_defined {
                            continue;
                        }
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
            S::LetPattern(let_pattern) => self.compile_let_pattern(let_pattern),
            S::ConstDef(_) => {
                // All work is done in the first pass (collect_user_const_heap_refs + emit_const_init).
                // Value types are compiled inline at use sites via compile_constant.
                // Reference types were already uploaded before main.
                Ok(())
            },
            S::IfBlock(if_block) => {
                self.compile_if_block(if_block)?;
                if let Some(result) = &if_block.if_block.result {
                    let result_type = self.ty(result);
                    if result_type.primitive_size() > 0 {
                        comment!(self, "discard if result");
                    }
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
                    if result_type.primitive_size() > 0 {
                        comment!(self, "discard block result");
                    }
                    self.write_discard(result_type)?;
                }
                Ok(())
            },
            S::Expression(expression) => {
                self.compile_expression(expression)?;
                let result_type = self.ty(expression);
                if result_type.primitive_size() > 0 {
                    comment!(self, "discard expression result");
                }
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
            S::Suspend(_) => {
                // suspend the VM; execution resumes at the following instruction on the next run() call.
                // No stack/heap teardown: the retained frame is what makes resumption possible.
                comment!(self, "suspend");
                self.writer.suspend();
                Ok(())
            },
            S::Yield(yield_stmt) => {
                // a ref-typed value/key is retained by the generator's value/key slot (replace semantics);
                // its constructor drives that refcounting, GEN_PRIMITIVE_CTOR marks a primitive (no refcount).
                let (value_size, value_ctor) = { let ty = self.ty(&yield_stmt.value); (ty.primitive_size() as FrameAddress, if ty.is_ref() { self.constructor(ty)? } else { GEN_PRIMITIVE_CTOR }) };
                comment!(self, "yield");
                // record which ref-typed frame slots are live at this suspension point so a mid-flight
                // drop can release them; the map's const-pool offset is stashed in the generator header.
                let live_ref_map = self.write_generator_live_ref_map(&self.collect_generator_live_refs()?);
                match &yield_stmt.key {
                    Some(key) => {
                        let (key_size, key_ctor) = { let ty = self.ty(key); (ty.primitive_size() as FrameAddress, if ty.is_ref() { self.constructor(ty)? } else { GEN_PRIMITIVE_CTOR }) };
                        // push key then value (value ends on top, matching gen_yield_kv's expectation)
                        self.compile_expression(key)?;
                        self.compile_expression(&yield_stmt.value)?;
                        self.writer.gen_yield_kv(live_ref_map, key_ctor, value_ctor, key_size, value_size);
                    },
                    None => {
                        self.compile_expression(&yield_stmt.value)?;
                        self.writer.gen_yield(live_ref_map, value_ctor, value_size);
                    },
                }
                Ok(())
            },
            S::Return(ast::Return { expr, .. }) => {
                if self.locals.is_generator() {
                    // a generator's `return` carries no value (resolver-enforced); release locals and
                    // complete the generator instead of performing a normal value return. The value/key
                    // constructors release any reference still held in the header value/key slots.
                    comment!(self, "generator returning");
                    self.write_for_loop_cleanup()?;
                    self.write_scope_destructor(BranchingScope::Function)?;
                    let (value_ctor, key_ctor) = self.locals.generator_slot_ctors();
                    self.writer.gen_return(value_ctor, key_ctor);
                } else {
                    comment!(self, "block returning");
                    self.compile_expression(expr)?;
                    // inc result, then dec everything
                    self.write_cnt(self.ty(expr), true, HeapRefOp::Inc)?;
                    self.write_for_loop_cleanup()?;
                    self.write_scope_destructor(BranchingScope::Function)?;
                    self.write_cnt(self.ty(expr), true, HeapRefOp::DecNoFree)?;
                    self.write_ret()?;
                }
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
            Cast(cast) => self.compile_cast(cast),
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
        use ast::{BinaryOperator::*, Expression as E};
        comment!(self, "direct assignment");
        let binding_id = item.left.as_variable().ice()?.binding_id;
        let loc = self.locals.lookup(binding_id);

        match (item.op, &item.right) {
            (AddAssign, E::Literal(r)) if self.ty(&item.left).is_numeric() => {
                self.write_addvc(self.ty(&item.left), loc, r.value.as_numeric().ice()?)?;
            },
            (SubAssign, E::Literal(r)) if self.ty(&item.left).is_numeric() => {
                self.write_subvc(self.ty(&item.left), loc, r.value.as_numeric().ice()?)?;
            },
            (Assign, _) => {
                self.compile_expression(&item.right)?;
                self.write_store(self.ty(&item.left), loc, Some(binding_id))?;
                self.init_state.initialize(binding_id);
            },
            _ => {
                self.check_initialized(item.left.as_variable().ice()?)?;
                self.write_load(self.ty(&item.left), loc)?; // stack: left
                if let Some(ast::CompoundDispatch::Method(constant_id)) = &item.op_dispatch {
                    // operator-trait dispatch on a custom target: the loaded value is `self` and the
                    // right-hand side is `rhs`. Both are ref-counted up per the call convention (the callee
                    // decrements them); the call replaces them with the result, which is then stored back
                    // (write_store decrements the variable's previous value).
                    let constant_id = *constant_id;
                    self.write_cnt(self.ty(&item.left), true, HeapRefOp::Inc)?;   // stack: left(self)
                    self.compile_expression(&item.right)?;                        // stack: self right
                    self.write_cnt(self.ty(&item.right), true, HeapRefOp::Inc)?;  // stack: self rhs
                    self.compile_method_dispatch(constant_id)?;                   // stack: result
                } else {
                    self.compile_expression(&item.right)?; // stack: left right
                    let ty = self.ty(&item.left);
                    match item.op { // stack: result
                        AddAssign => self.write_add(ty)?,
                        SubAssign => self.write_sub(ty)?,
                        MulAssign => self.write_mul(ty)?,
                        DivAssign => self.write_div(ty)?,
                        RemAssign => self.write_rem(ty)?,
                        BitAndAssign => self.write_bitand(ty)?,
                        BitOrAssign => self.write_bitor(ty)?,
                        BitXorAssign => self.write_bitxor(ty)?,
                        ShlAssign => { self.write_shift_amount_cast(self.ty(&item.right))?; self.write_shl(ty)? },
                        ShrAssign => { self.write_shift_amount_cast(self.ty(&item.right))?; self.write_shr(ty)? },
                        _ => Self::ice_at(item, "Invalid assignment operator while assigning to var")?,
                    };
                }
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
                // map index-write `m[k] = v` has no fixed lvalue address; lower it to a map insert.
                if let Some(bin) = item.left.as_binary_op() {
                    if (bin.op == BO::Index || bin.op == BO::IndexWrite) && self.ty(&bin.left).as_map().is_some() {
                        let constructor = self.constructor(self.ty(&bin.left))?;
                        self.compile_expression(bin.left.as_expression().ice()?)?;  // stack: map
                        self.compile_expression(bin.right.as_expression().ice()?)?; // stack: map key
                        self.compile_expression(&item.right)?;                      // stack: map key value
                        self.writer.call_builtinx(Builtin::map_insert, 0, constructor);                        // stack: --
                        return Ok(());
                    }
                }
                self.compile_expression(&item.left)?;       // stack: &left
                self.compile_expression(&item.right)?;      // stack: &left right
                let ty = self.ty(&item.left);
                self.write_heap_put(ty, false)?;    // stack: --
            },
            _ => {
                // Custom Index compound assignment: `a[i] += v` -> `a.set(i, a.get(i) OP v)`
                // with single evaluation of `a` and `i` using temp bindings.
                if let Some(ast::CompoundDispatch::IndexMethod { get_method, set_method, op_method, recv, idx }) = &item.op_dispatch {
                    let recv_loc = self.locals.lookup(*recv);
                    let idx_loc = self.locals.lookup(*idx);
                    let recv_ty_id = self.binding_by_id(*recv).type_id.ice()?;
                    let idx_ty_id = self.binding_by_id(*idx).type_id.ice()?;

                    // Store recv and idx once.
                    comment!(self, "store recv/idx temps");
                    self.compile_expression(item.left.as_binary_op().ice()?.left.as_expression().ice()?)?;
                    self.write_store(self.ty(&recv_ty_id), recv_loc, None)?;
                    self.compile_expression(item.left.as_binary_op().ice()?.right.as_expression().ice()?)?;
                    self.write_store(self.ty(&idx_ty_id), idx_loc, None)?;

                    // Pre-load recv/idx for set (parked at bottom of stack).
                    comment!(self, "park set args");
                    self.write_load(self.ty(&recv_ty_id), recv_loc)?;
                    self.write_cnt(self.ty(&recv_ty_id), true, HeapRefOp::Inc)?;
                    self.write_load(self.ty(&idx_ty_id), idx_loc)?;
                    self.write_cnt(self.ty(&idx_ty_id), true, HeapRefOp::Inc)?;

                    // Load recv/idx again for get.
                    comment!(self, "load for get");
                    self.write_load(self.ty(&recv_ty_id), recv_loc)?;
                    self.write_cnt(self.ty(&recv_ty_id), true, HeapRefOp::Inc)?;
                    self.write_load(self.ty(&idx_ty_id), idx_loc)?;
                    self.write_cnt(self.ty(&idx_ty_id), true, HeapRefOp::Inc)?;

                    // Call get: [recv_set idx_set recv_get idx_get] -> [recv_set idx_set V]
                    // (get decrements its own recv/idx arguments per the method-call convention).
                    comment!(self, "get");
                    self.compile_method_dispatch(*get_method)?;

                    // Apply the operator to `V <op> right`, producing V'. Two ref-counting conventions apply:
                    // a custom operator is a method call (callee decrements both operands, so the caller must
                    // increment them); a built-in operator (`write_add` etc.) instead drops only *unowned*
                    // temporaries (`HeapRefOp::Free`), so its operands must NOT be pre-incremented.
                    comment!(self, "op");
                    if let Some(op_id) = op_method {
                        self.write_cnt(self.ty(&item.right), true, HeapRefOp::Inc)?;  // V (self)
                        self.compile_expression(&item.right)?;
                        self.write_cnt(self.ty(&item.right), true, HeapRefOp::Inc)?;  // right (rhs)
                        self.compile_method_dispatch(*op_id)?;
                    } else {
                        use crate::frontend::ast::BinaryOperator as BO;
                        self.compile_expression(&item.right)?;
                        match item.op {
                            BO::AddAssign => self.write_add(self.ty(&item.right))?,
                            BO::SubAssign => self.write_sub(self.ty(&item.right))?,
                            BO::MulAssign => self.write_mul(self.ty(&item.right))?,
                            BO::DivAssign => self.write_div(self.ty(&item.right))?,
                            BO::RemAssign => self.write_rem(self.ty(&item.right))?,
                            BO::BitAndAssign => self.write_bitand(self.ty(&item.right))?,
                            BO::BitOrAssign => self.write_bitor(self.ty(&item.right))?,
                            BO::BitXorAssign => self.write_bitxor(self.ty(&item.right))?,
                            BO::ShlAssign => { self.write_shift_amount_cast(self.ty(&item.right))?; self.write_shl(self.ty(&item.right))? },
                            BO::ShrAssign => { self.write_shift_amount_cast(self.ty(&item.right))?; self.write_shr(self.ty(&item.right))? },
                            _ => Self::ice_at(item, "Invalid compound assignment operator for Index")?,
                        };
                    }

                    // Call set: [recv_set idx_set V'] -> []. set is a method call and decrements all three
                    // arguments, so V' (the operator result) must be incremented like the parked recv/idx.
                    comment!(self, "set");
                    self.write_cnt(self.ty(&item.right), true, HeapRefOp::Inc)?;
                    self.compile_method_dispatch(*set_method)?;
                    return Ok(());
                }

                // Standard compound assignment (field/struct offset).
                self.compile_expression(&item.left)?;       // stack: &left
                self.writer.clone(size_of::<HeapAddress>() as FrameAddress);// stack: &left &left
                self.write_heap_fetch(self.ty(&item.left))?;// stack: &left left
                if let Some(ast::CompoundDispatch::Method(constant_id)) = &item.op_dispatch {
                    // operator-trait dispatch on a custom target, evaluated in place: the fetched target
                    // value is `self` and the right-hand side is `rhs`. Both are ref-counted up per the call
                    // convention (the callee decrements them); the call replaces them with the result.
                    let constant_id = *constant_id;
                    self.write_cnt(self.ty(&item.left), true, HeapRefOp::Inc)?;  // stack: &left left(self)
                    self.compile_expression(&item.right)?;                       // stack: &left self right
                    self.write_cnt(self.ty(&item.right), true, HeapRefOp::Inc)?; // stack: &left self rhs
                    self.compile_method_dispatch(constant_id)?;                  // stack: &left result
                } else {
                    self.compile_expression(&item.right)?;      // stack: &left left right
                    let ty = self.ty(&item.left);
                    match item.op {                                 // stack: &left result
                        BO::AddAssign => self.write_add(ty)?,
                        BO::SubAssign => self.write_sub(ty)?,
                        BO::MulAssign => self.write_mul(ty)?,
                        BO::DivAssign => self.write_div(ty)?,
                        BO::RemAssign => self.write_rem(ty)?,
                        BO::BitAndAssign => self.write_bitand(ty)?,
                        BO::BitOrAssign => self.write_bitor(ty)?,
                        BO::BitXorAssign => self.write_bitxor(ty)?,
                        BO::ShlAssign => { self.write_shift_amount_cast(self.ty(&item.right))?; self.write_shl(ty)? },
                        BO::ShrAssign => { self.write_shift_amount_cast(self.ty(&item.right))?; self.write_shr(ty)? },
                        _ => Self::ice_at(item, "Invalid assignment operator while assigning to offset")?,
                    };
                }
                self.write_heap_put(self.ty(&item.left), false)?; // stack -
            },
        }
        Ok(())
    }

    /// Emits a call to the method identified by the given constant, with its arguments already on the stack
    /// (ref-counted per the call convention). Used to dispatch in-place compound-assignment operator-trait
    /// calls; mirrors the method-call dispatch in `compile_call_constant`.
    fn compile_method_dispatch(self: &mut Self, constant_id: ConstantId) -> CompileResult {
        let function_id = self.resolved.constant(constant_id).value.as_function_id().ice()?;
        match self.resolved.function(function_id).kind.ice()? {
            FunctionKind::Method(object_type_id) => {
                if self.ty(&object_type_id).is_trait_object() {
                    let function_offset = self.vtable_function_offset(function_id)?;
                    let function_arg_size = self.resolved.function(function_id).arg_size(self);
                    self.writer.call_virtual(function_offset, function_arg_size);
                } else {
                    self.write_call(function_id);
                }
                Ok(())
            },
            _ => Self::ice("Compound assignment dispatch target is not a method"),
        }
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
                if self.ty(&object_type_id).is_trait_object() {
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
                // a call to a generator function constructs a generator object instead of running the body
                let ret_type_id = self.resolved.function(function_id).ret_type_id(self);
                if ret_type_id.map_or(false, |ret_type_id| self.generator_signature(ret_type_id).is_some()) {
                    comment!(self, "make generator {}()", constant.path);
                    let arg_size = self.resolved.function(function_id).arg_size(self);
                    // live-ref-map for the captured ref-typed args, released if the generator is dropped before it is ever started
                    let entry_map = self.write_generator_entry_map(function_id)?;
                    // push the entry address (forward-reference safe) above the args, then construct
                    let function_addr = self.functions.register_call(function_id, self.writer.position(), true);
                    self.write_target(function_addr)?;
                    self.writer.gen_new(arg_size, entry_map);
                } else {
                    comment!(self, "call {}()", constant.path);
                    self.write_call(function_id);
                }
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

    /// Compiles a destructuring let binding, e.g. `let Struct { a, b } = value;`. Mirrors a single irrefutable
    /// match arm: compile the subject, bind its fields, then release the subject. Bindings live in the current scope.
    fn compile_let_pattern(self: &mut Self, item: &ast::LetPattern) -> CompileResult {
        comment!(self, "{}", item);
        let subject_type_id = item.expr.type_id(self).ice()?;
        self.compile_expression(&item.expr)?;
        self.compile_pattern_bind(subject_type_id, &[], &item.pattern)?;
        self.write_discard(self.ty(&subject_type_id))?;
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

        match &constant.value {
            ConstantValue::Function(function_id) => {
                // load function address
                comment!(self, "\npush @{}", item.path);
                // register call: may capture writer position for placeholder, must directly preceede write_target
                let function_addr = self.functions.register_call(*function_id, self.writer.position(), true);
                self.write_target(function_addr)?;
                // load constructor (none)
                self.write_immediate_sa(0)?;
                // upload both into heap object (this is for compatibility with closures and unfortunate overkill for anonymous functions)
                self.writer.upload(size_of::<StackAddress>() * 2, 0);
            },
            ConstantValue::Discriminant(numeric) => {
                let type_id = item.type_id(self).ice()?;
                let enumeration = self.ty(item).as_enum().ice()?;
                match enumeration.primitive {
                    // Primitive (C-like) enum: the value is the bare discriminant.
                    Some((primitive_type_id, _)) => {
                        let primitive_type = self.ty(&primitive_type_id);
                        self.write_immediate(primitive_type, *numeric)?;
                    },
                    // Reference-type enum (has data variants): a unit variant is a heap object
                    // holding just the variant index, consistent with how data variants are built.
                    None => {
                        let variant_name = &item.path.segments.last().ice()?.name;
                        let variant_index = enumeration.variant_index(variant_name).ice()?;
                        let implementor_index = *self.trait_implementor_indices.get(&type_id).unwrap_or(&0);
                        let index_type = Type::unsigned(size_of::<VariantIndex>());
                        comment!(self, "construct unit variant {}", item.path);
                        self.write_immediate(&index_type, Numeric::Unsigned(variant_index as u64))?;
                        self.writer.upload(index_type.primitive_size() as StackAddress, implementor_index);
                    },
                }
            },
            ConstantValue::UserConst(user_const) => {
                let type_id = item.type_id(self).ice()?;
                let ty = self.ty(&type_id);
                match user_const {
                    UserConstValue::HeapRef(offset) => {
                        // Reference-type const: load the heap address from the const pool.
                        self.writer.load_pool(*offset);
                    },
                    UserConstValue::Bool(b) => {
                        self.write_immediate(ty, Numeric::Unsigned(*b as u64))?;
                    },
                    UserConstValue::Numeric(n) => {
                        // For primitive enums, use the underlying primitive type
                        let write_ty = if let Type::Enum(e) = ty {
                            if let Some((primitive_type_id, _)) = e.primitive {
                                self.ty(&primitive_type_id)
                            } else {
                                ty
                            }
                        } else {
                            ty
                        };
                        self.write_immediate(write_ty, *n)?;
                    },
                    UserConstValue::Unresolved => {
                        Self::ice(&format!("Unresolved const type"))?
                    },
                }
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

    /// Emits code that tests whether the value selected by `access` matches the variant named by `path`, without
    /// consuming the subject. On mismatch a `j0` is emitted and collected in `fail_jumps`.
    fn compile_variant_tag_test(self: &mut Self, subject_type_id: TypeId, access: &[(StackAddress, TypeId)], path: &ast::Path, fail_jumps: &mut Vec<StackAddress>) -> CompileResult {
        let value_type_id = self.pattern_value_type(subject_type_id, access);
        let variant_name = &path.segments.last().ice()?.name;
        // extract owned metadata so the enum borrow is released before emitting code
        let enum_ = self.ty(&value_type_id).as_enum().ice()?;
        let primitive_type_id = enum_.primitive.map(|(type_id, _)| type_id);
        let variant_index = enum_.variant_index(variant_name);
        let discriminant = enum_.variant_value(variant_name);
        if let Some(primitive_type_id) = primitive_type_id {
            // primitive (C-like) enum: the subject is the bare discriminant, compared as its underlying type.
            // these carry no data, so they only ever appear at the top level (access is empty).
            let discriminant = discriminant.ice()?;
            self.write_clone(self.ty(&primitive_type_id));
            self.write_immediate(self.ty(&primitive_type_id), discriminant)?;
            self.write_ceq(self.ty(&primitive_type_id))?;
        } else {
            // reference enum: the variant index is the first field of the heap object
            let variant_index = variant_index.ice()?;
            let index_type = Type::unsigned(size_of::<VariantIndex>());
            self.write_pattern_value_load(subject_type_id, access)?;
            self.write_heap_fetch(&index_type)?;
            self.write_immediate(&index_type, Numeric::Unsigned(variant_index as u64))?;
            self.write_ceq(&index_type)?;
        }
        fail_jumps.push(self.writer.j0(123));
        Ok(())
    }

    /// Emits non-destructive tests for the given pattern against the value selected by `access` (the empty path
    /// being the subject on top of the stack). Mismatch jumps are collected in `fail_jumps` for the caller to
    /// point at the next arm. Recurses through nested variant/path sub-patterns, navigating across heap boundaries.
    fn compile_pattern_test(self: &mut Self, subject_type_id: TypeId, access: &[(StackAddress, TypeId)], pattern: &ast::Pattern, fail_jumps: &mut Vec<StackAddress>) -> CompileResult {
        match pattern {
            // wildcard and binding patterns match unconditionally
            ast::Pattern::Wildcard(_) | ast::Pattern::Binding(_) => {},
            // a literal is matched by comparing the selected value against it
            ast::Pattern::Literal(literal) => {
                let value_type_id = self.pattern_value_type(subject_type_id, access);
                self.write_pattern_value_load(subject_type_id, access)?;
                self.compile_literal(literal)?;
                self.write_ceq(self.ty(&value_type_id))?;
                fail_jumps.push(self.writer.j0(123));
            },
            // a range is matched by two bound comparisons against the selected value
            ast::Pattern::Range(range) => {
                let value_type_id = self.pattern_value_type(subject_type_id, access);
                // lower bound: value >= lo
                self.write_pattern_value_load(subject_type_id, access)?;
                self.compile_literal(&range.lo)?;
                self.write_cgte(self.ty(&value_type_id))?;
                fail_jumps.push(self.writer.j0(123));
                // upper bound: value <= hi (inclusive) or value < hi (exclusive)
                self.write_pattern_value_load(subject_type_id, access)?;
                self.compile_literal(&range.hi)?;
                if range.inclusive {
                    self.write_clte(self.ty(&value_type_id))?;
                } else {
                    self.write_clt(self.ty(&value_type_id))?;
                }
                fail_jumps.push(self.writer.j0(123));
            },
            // a unit variant is matched by its tag
            ast::Pattern::Path(path) => {
                self.compile_variant_tag_test(subject_type_id, access, path, fail_jumps)?;
            },
            // a data variant is matched by its tag and then field-wise against the sub-patterns
            ast::Pattern::VariantTuple(variant) => {
                self.compile_variant_tag_test(subject_type_id, access, &variant.path, fail_jumps)?;
                let variant_name = variant.path.segments.last().ice()?.name.clone();
                let value_type_id = self.pattern_value_type(subject_type_id, access);
                let field_type_ids = self.variant_data_fields(value_type_id, &variant_name)?;
                for (index, element) in variant.elements.iter().enumerate() {
                    let offset = self.variant_field_offset(&field_type_ids, index)?;
                    let field_type_id = field_type_ids[index].ice()?;
                    let mut element_access = access.to_vec();
                    element_access.push((offset, field_type_id));
                    self.compile_pattern_test(subject_type_id, &element_access, element, fail_jumps)?;
                }
            },
            // a struct has no tag; it is matched field-wise against the sub-patterns
            ast::Pattern::Struct(structure) => {
                let value_type_id = self.pattern_value_type(subject_type_id, access);
                let struct_ = self.ty(&value_type_id).as_struct().ice()?.clone();
                for (field_name, field_pattern) in structure.fields.iter() {
                    let offset = self.compute_member_offset(&struct_, &field_name.name);
                    let field_type_id = struct_.type_id(&field_name.name).ice()?;
                    let mut element_access = access.to_vec();
                    element_access.push((offset, field_type_id));
                    self.compile_pattern_test(subject_type_id, &element_access, field_pattern, fail_jumps)?;
                }
            },
            // an or-pattern matches if any alternative matches: test each in turn, short-circuiting to the
            // body on the first success and only failing the arm if every alternative fails. Each alternative's
            // test is stack-neutral, so converging branches all see the same (subject-only) stack.
            ast::Pattern::Or(or) => {
                let mut success_jumps = Vec::new();
                let last = or.alternatives.len() - 1;
                for (index, alternative) in or.alternatives.iter().enumerate() {
                    if index == last {
                        // final alternative: its failures are the or-pattern's failures (caller points them at the next arm)
                        self.compile_pattern_test(subject_type_id, access, alternative, fail_jumps)?;
                    } else {
                        // earlier alternative: on success skip the rest, on failure fall through to the next alternative
                        let mut alternative_fail = Vec::new();
                        self.compile_pattern_test(subject_type_id, access, alternative, &mut alternative_fail)?;
                        success_jumps.push(self.writer.jmp(123));
                        let next_alternative = self.writer.position();
                        for fail_jump in alternative_fail {
                            self.writer.overwrite(fail_jump, |w| w.j0(next_alternative));
                        }
                    }
                }
                // every successful alternative lands here, ready for the caller to bind and run the body
                let matched = self.writer.position();
                for success_jump in success_jumps {
                    self.writer.overwrite(success_jump, |w| w.jmp(matched));
                }
            },
        }
        Ok(())
    }

    /// Emits code that binds the pattern's variables, reading the selected values out of the subject (the empty
    /// access path being the subject itself). Recurses through nested variant/path sub-patterns.
    fn compile_pattern_bind(self: &mut Self, subject_type_id: TypeId, access: &[(StackAddress, TypeId)], pattern: &ast::Pattern) -> CompileResult {
        match pattern {
            // or-patterns bind nothing (bindings inside them are rejected by the resolver)
            ast::Pattern::Wildcard(_) | ast::Pattern::Literal(_) | ast::Pattern::Path(_) | ast::Pattern::Or(_) | ast::Pattern::Range(_) => {},
            // bind the selected value
            ast::Pattern::Binding(binding) => {
                let value_type_id = self.pattern_value_type(subject_type_id, access);
                self.init_state.declare(binding.binding_id);
                let slot = self.locals.lookup(binding.binding_id);
                self.write_pattern_value_load(subject_type_id, access)?;
                self.write_store(self.ty(&value_type_id), slot, Some(binding.binding_id))?;
                self.init_state.initialize(binding.binding_id);
            },
            // descend into each field sub-pattern, binding whatever variables it introduces
            ast::Pattern::VariantTuple(variant) => {
                let variant_name = variant.path.segments.last().ice()?.name.clone();
                let value_type_id = self.pattern_value_type(subject_type_id, access);
                let field_type_ids = self.variant_data_fields(value_type_id, &variant_name)?;
                for (index, element) in variant.elements.iter().enumerate() {
                    let offset = self.variant_field_offset(&field_type_ids, index)?;
                    let field_type_id = field_type_ids[index].ice()?;
                    let mut element_access = access.to_vec();
                    element_access.push((offset, field_type_id));
                    self.compile_pattern_bind(subject_type_id, &element_access, element)?;
                }
            },
            // descend into each struct field sub-pattern, binding whatever variables it introduces
            ast::Pattern::Struct(structure) => {
                let value_type_id = self.pattern_value_type(subject_type_id, access);
                let struct_ = self.ty(&value_type_id).as_struct().ice()?.clone();
                for (field_name, field_pattern) in structure.fields.iter() {
                    let offset = self.compute_member_offset(&struct_, &field_name.name);
                    let field_type_id = struct_.type_id(&field_name.name).ice()?;
                    let mut element_access = access.to_vec();
                    element_access.push((offset, field_type_id));
                    self.compile_pattern_bind(subject_type_id, &element_access, field_pattern)?;
                }
            },
        }
        Ok(())
    }

    /// Compiles a single match arm: tests the pattern, and on match binds its variables, releases the subject
    /// and runs the body. Pattern bindings and body locals share the current branching scope.
    fn compile_match_arm(self: &mut Self, exit_jumps: &mut Vec<StackAddress>, subject_type_id: TypeId, pattern: &ast::Pattern, block: &ast::Block) -> CompileResult {
        // emit non-destructive match tests; failures jump to the next arm
        let mut fail_jumps = Vec::new();
        self.compile_pattern_test(subject_type_id, &[], pattern, &mut fail_jumps)?;
        // matched: bind variables (still need the subject), then release the subject and run the body
        self.compile_pattern_bind(subject_type_id, &[], pattern)?;
        self.write_discard(self.ty(&subject_type_id))?; // balances the protective increment in compile_match_block
        for statement in block.statements.iter() {
            self.compile_statement(statement)?;
        }
        if let Some(result) = &block.result {
            comment!(self, "match arm resulting");
            self.compile_expression(result)?;
            let result_type_id = result.type_id(self).ice()?;
            self.write_cnt(self.ty(&result_type_id), true, HeapRefOp::Inc)?;
            self.write_scope_destructor(BranchingScope::Block)?;
            self.write_cnt(self.ty(&result_type_id), true, HeapRefOp::DecNoFree)?;
        } else if block.control_flow() == None {
            self.write_scope_destructor(BranchingScope::Block)?;
        }
        if block.control_flow() != Some(ast::ControlFlowType::Return) {
            exit_jumps.push(self.writer.jmp(123));
        }
        // failed tests land here, at the next arm
        let next_target = self.writer.position();
        for fail_jump in fail_jumps {
            self.writer.overwrite(fail_jump, |w| w.j0(next_target));
        }
        Ok(())
    }

    /// Split match block into recursive tree of A/B branches.
    fn compile_match_block_recursive(self: &mut Self, exit_jumps: &mut Vec<StackAddress>, subject_type_id: TypeId, remaining_branches: &[(ast::Pattern, ast::Block)]) -> CompileResult {
        if remaining_branches.len() == 1 {
            self.init_state.push(BranchingKind::Single, BranchingScope::Block); // patterns are required to be exhaustive
            self.compile_match_arm(exit_jumps, subject_type_id, &remaining_branches[0].0, &remaining_branches[0].1)?;
            self.init_state.pop();
        } else if remaining_branches.len() > 1 {
            self.init_state.push(BranchingKind::Double, BranchingScope::Block);
            self.init_state.set_path(BranchingPath::A);
            self.compile_match_arm(exit_jumps, subject_type_id, &remaining_branches[0].0, &remaining_branches[0].1)?;
            self.init_state.set_path(BranchingPath::B);
            self.compile_match_block_recursive(exit_jumps, subject_type_id, &remaining_branches[1..])?;
            self.init_state.pop();
        }
        Ok(())
    }

    /// Compiles a match block.
    fn compile_match_block(self: &mut Self, item: &ast::MatchBlock) -> CompileResult {
        comment!(self, "{}", item);
        let subject_type_id = item.expr.type_id(self).ice()?;
        self.compile_expression(&item.expr)?;
        // the subject stays on the stack across the arms; field reads during dispatch don't free it, and the
        // matching arm discards it (write_discard's Free drops a temporary but leaves a borrowed value to its owner)
        let mut exit_jumps = Vec::new();
        self.compile_match_block_recursive(&mut exit_jumps, subject_type_id, &item.branches)?;
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

    // Compiles a for-in-range loop.
    fn compile_for_loop_range(self: &mut Self, item: &ast::ForLoop, iter_loc: FrameAddress, iter_type_id: TypeId) -> CompileResult {
        comment!(self, "for in range");
        // store lower range bound in iter variable
        let binary_op = item.expr.as_binary_op().ice()?;
        self.compile_expression(binary_op.left.as_expression().ice()?)?; // stack current=lower
        self.write_store(self.ty(&iter_type_id), iter_loc, None)?; // stack -
        // push upper range bound
        self.compile_expression(binary_op.right.as_expression().ice()?)?; // stack upper
        // precheck bounds (otherwise we'd always be looping at least one time)
        let skip_jump = if binary_op.op == ast::BinaryOperator::Range {
            // non-inclusive range: check if lower bound greater than or equal to upper bound. also note, while is always inclusive, so we have to subtract 1 from upper bound
            let iter_ty = self.ty(&iter_type_id);
            self.write_clone(iter_ty); // stack: upper upper
            self.write_load(iter_ty, iter_loc)?; // stack: upper upper lower
            self.write_clte(iter_ty)?; // stack: upper upper_lte_lower
            let skip_jump = self.writer.jn0(123); // stack: upper
            comment!(self, "push upper bound - 1");
            self.write_subxc(iter_ty, Numeric::Unsigned(1))?; // stack: upper=upper-1
            skip_jump
        } else {
            // inclusive range: check if lower bound greater than upper bound.
            let iter_ty = self.ty(&iter_type_id);
            comment!(self, "clone upper bound");
            self.write_clone(iter_ty); // stack: upper upper
            self.write_load(iter_ty, iter_loc)?; // stack: upper upper lower
            self.write_clt(iter_ty)?; // stack: upper upper_lte_lower
            self.writer.jn0(123) // stack: upper
        };
        // compile inner block
        let start_target = self.writer.position(); // stack: upper
        let range_size = self.ty(&iter_type_id).primitive_size() as StackAddress;
        self.loop_control.push();
        self.for_loop_cleanup.push(ForLoopCleanup::Range(range_size));
        self.compile_block(&item.block)?;
        let loop_controls = self.loop_control.pop();
        // loop instruction will check iter and either do nothing or increment and jump back to start
        let increment_target = self.write_loop(self.ty(&iter_type_id), iter_loc, start_target)?; // stack: upper
        // get exit location, fix skip_jump location to point here
        let exit_target = self.writer.position();
        self.writer.overwrite(skip_jump, |w| w.jn0(exit_target));
        // fix break/continue addresses
        for loop_control in &loop_controls {
            match loop_control {
                &LoopControl::Break(addr) => self.writer.overwrite(addr, |w| w.jmp(exit_target)),
                &LoopControl::Continue(addr) => self.writer.overwrite(addr, |w| w.jmp(increment_target)),
            };
        }
        self.for_loop_cleanup.pop();
        // discard counter
        comment!(self, "discard upper bound");
        self.write_discard(self.ty(&iter_type_id))?;
        Ok(())
    }

    /// Evaluates the loop's collection expression and leaves two references on the stack: the original
    /// source `[S]` and an independent shallow clone `[C]` (`[S, C]`, clone on top), both retained
    /// (recursively reference-counted) for the loop's duration. The loop walks the clone, so the body may
    /// freely mutate the original; [`compile_for_loop_release`] releases both on exit.
    ///
    /// Retaining the source first is what makes cloning a *temporary* (e.g. `for v in [a, b]`) safe: the
    /// clone builtin internally drops its receiver, which would otherwise free the temporary's
    /// uniquely-owned sub-references before the clone could retain them. Owning the source beforehand
    /// turns that internal drop into a no-op, and keeping the source retained alongside the clone keeps
    /// the shared sub-references alive for the whole loop.
    fn compile_for_loop_clone(self: &mut Self, item: &ast::ForLoop) -> CompileResult {
        let collection_type_id = item.expr.type_id(self).ice()?;
        self.compile_expression(&item.expr)?;                                    // [S]
        self.write_cnt(self.ty(&collection_type_id), true, HeapRefOp::Inc)?;     // own source + sub-refs
        self.write_clone(self.ty(&collection_type_id));                          // [S, S]
        self.write_collection_clone(collection_type_id)?;                        // [S, C]
        self.write_cnt(self.ty(&collection_type_id), true, HeapRefOp::Inc)?;     // own clone + sub-refs
        Ok(())
    }

    /// Releases the source and clone references left on the stack by [`compile_for_loop_clone`].
    fn compile_for_loop_release(self: &mut Self, item: &ast::ForLoop) -> CompileResult {
        let collection_type_id = item.expr.type_id(self).ice()?;
        self.write_cnt(self.ty(&collection_type_id), false, HeapRefOp::Dec)?;    // release clone -> [S]
        self.write_cnt(self.ty(&collection_type_id), false, HeapRefOp::Dec)?;    // release source -> []
        Ok(())
    }

    /// Compiles iteration over an array. The array is cloned at loop entry and the clone is retained for
    /// the loop's duration (so the body may mutate the original); `array_iter`/`array_iter_iv` walks the
    /// clone, binding each element into `element_loc` and (for `for k, v`) the running index into
    /// `index_loc`.
    fn compile_for_loop_array(self: &mut Self, item: &ast::ForLoop, index_loc: Option<FrameAddress>, element_loc: FrameAddress, element_type_id: TypeId) -> CompileResult {
        comment!(self, "for in array");
        // clone the array; the clone (top of stack) is walked while the body may mutate the original
        self.compile_for_loop_clone(item)?;
        let collection_type_id = item.expr.type_id(self).ice()?;
        self.for_loop_cleanup.push(ForLoopCleanup::Collection(collection_type_id));
        let loop_start = self.write_array_iter_iv(self.ty(&element_type_id), index_loc, element_loc, 123)?;
        // compile inner block
        self.loop_control.push();
        self.compile_block(&item.block)?;
        let loop_controls = self.loop_control.pop();
        // repeat/exit loop, fix exit address
        self.writer.jmp(loop_start);
        let exit_target = self.writer.position();
        self.writer.overwrite(loop_start, |_| self.write_array_iter_iv(self.ty(&element_type_id), index_loc, element_loc, exit_target))?;
        // fix break/continue addresses
        for loop_control in &loop_controls {
            match loop_control {
                &LoopControl::Break(addr) => self.writer.overwrite(addr, |w| w.jmp(exit_target)),
                &LoopControl::Continue(addr) => self.writer.overwrite(addr, |w| w.jmp(loop_start)),
            };
        }
        self.for_loop_cleanup.pop();
        self.compile_for_loop_release(item)?;
        Ok(())
    }

    /// Compiles iteration over a map. The map is cloned at loop entry and the clone is retained for the
    /// loop's duration (so the body may mutate the original); `map_iter`/`map_iter_kv` walks the clone's
    /// insertion-ordered entries, binding each value into `value_loc` and (for `for k, v`) the key into
    /// `key_loc`.
    fn compile_for_loop_map(self: &mut Self, item: &ast::ForLoop, key_loc: Option<FrameAddress>, value_loc: FrameAddress) -> CompileResult {
        comment!(self, "for in map");
        // clone the map; the clone (top of stack) is walked while the body may mutate the original
        self.compile_for_loop_clone(item)?;
        let collection_type_id = item.expr.type_id(self).ice()?;
        self.for_loop_cleanup.push(ForLoopCleanup::Collection(collection_type_id));
        let constructor = self.constructor(self.ty(&item.expr))?;
        let loop_start = self.write_map_iter(key_loc, value_loc, constructor, 123);
        // compile inner block
        self.loop_control.push();
        self.compile_block(&item.block)?;
        let loop_controls = self.loop_control.pop();
        // repeat/exit loop, fix exit address
        self.writer.jmp(loop_start);
        let exit_target = self.writer.position();
        self.writer.overwrite(loop_start, |_| self.write_map_iter(key_loc, value_loc, constructor, exit_target));
        // fix break/continue addresses
        for loop_control in &loop_controls {
            match loop_control {
                &LoopControl::Break(addr) => self.writer.overwrite(addr, |w| w.jmp(exit_target)),
                &LoopControl::Continue(addr) => self.writer.overwrite(addr, |w| w.jmp(loop_start)),
            };
        }
        self.for_loop_cleanup.pop();
        self.compile_for_loop_release(item)?;
        Ok(())
    }

    /// Compiles iteration over a generator, desugaring `for v in g { .. }` to the equivalent of
    /// `while g.next() { let v = g.value(); .. }` (and `for k, v in g` to additionally bind `g.key()`).
    /// The generator reference is evaluated once and kept on the stack for the loop's duration, cloned
    /// for each `gen_next`/`gen_value`/`gen_key` (each borrows the reference; its refcount is owned here).
    fn compile_for_loop_generator(self: &mut Self, item: &ast::ForLoop, key_loc: Option<FrameAddress>, key_type_id: Option<TypeId>, value_loc: Option<FrameAddress>, value_type_id: Option<TypeId>) -> CompileResult {
        comment!(self, "for in generator");
        let gen_type_id = item.expr.type_id(self).ice()?;
        // evaluate the generator once, keep its reference on the stack and inc it for the loop's duration
        self.compile_expression(&item.expr)?;
        self.write_cnt(self.ty(&item.expr), true, HeapRefOp::Inc)?;
        let loop_start = self.writer.position();
        // resume the generator (gen_next consumes a clone of the reference); exit when it reports done
        self.write_clone(self.ty(&gen_type_id));
        self.writer.gen_next();
        let exit_jump = self.writer.j0(123);
        // bind the key (for `for k, _` / `for k, v`) and/or value (for `for v` / `for k, v`)
        if let (Some(key_loc), Some(key_type_id)) = (key_loc, key_type_id) {
            self.write_clone(self.ty(&gen_type_id));
            self.writer.gen_key(self.ty(&key_type_id).primitive_size() as FrameAddress);
            self.write_store(self.ty(&key_type_id), key_loc, None)?;
        }
        if let (Some(value_loc), Some(value_type_id)) = (value_loc, value_type_id) {
            self.write_clone(self.ty(&gen_type_id));
            self.writer.gen_value(self.ty(&value_type_id).primitive_size() as FrameAddress);
            self.write_store(self.ty(&value_type_id), value_loc, None)?;
        }
        // compile inner block
        self.loop_control.push();
        self.for_loop_cleanup.push(ForLoopCleanup::Generator(gen_type_id));
        self.compile_block(&item.block)?;
        let loop_controls = self.loop_control.pop();
        self.writer.jmp(loop_start);
        // fix exit and break/continue addresses
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
        for loop_control in &loop_controls {
            match loop_control {
                &LoopControl::Break(addr) => self.writer.overwrite(addr, |w| w.jmp(exit_target)),
                &LoopControl::Continue(addr) => self.writer.overwrite(addr, |w| w.jmp(loop_start)),
            };
        }
        self.for_loop_cleanup.pop();
        // release the generator reference held across the loop (cnt pops it off the stack)
        self.write_cnt(self.ty(&item.expr), false, HeapRefOp::Dec)?;
        Ok(())
    }

    /// Compiles a for - in loop
    fn compile_for_loop(self: &mut Self, item: &ast::ForLoop) -> CompileResult {
        use ast::{Expression::*, BinaryOperator as Op};
        self.init_state.push(BranchingKind::Single, BranchingScope::Loop);
        // resolve frame locations and types of the bindings present in this loop form, marking them
        // initialized. The value binding (`iter`) holds the array element / map value; the key binding
        // (`iter_key`) holds the array index / map key for the two-binding forms.
        if let Some(binding) = &item.iter_key { self.init_state.initialize(binding.binding_id); }
        if let Some(binding) = &item.iter { self.init_state.initialize(binding.binding_id); }
        let value_loc = item.iter.as_ref().map(|b| self.locals.lookup(b.binding_id));
        let value_type_id = match item.iter.as_ref() { Some(b) => Some(b.type_id(self).ice()?), None => None };
        let key_loc = item.iter_key.as_ref().map(|b| self.locals.lookup(b.binding_id));
        let key_type_id = match item.iter_key.as_ref() { Some(b) => Some(b.type_id(self).ice()?), None => None };
        // handle ranges, generators, maps or arrays. Map key-only iteration was normalized to `map.keys()`
        // (an array) during resolution; array key-only iteration to a counting range.
        // (NOTE: must match Resolver::resolve_for_loop.)
        let is_generator = item.expr.type_id(self).map_or(false, |t| self.generator_signature(t).is_some());
        let result = if is_generator {
            self.compile_for_loop_generator(item, key_loc, key_type_id, value_loc, value_type_id)
        } else {
            match &item.expr {
                BinaryOp(bo) if bo.op == Op::Range || bo.op == Op::RangeInclusive => {
                    self.compile_for_loop_range(item, value_loc.ice()?, value_type_id.ice()?)
                },
                _ => {
                    let is_map = item.expr.type_id(self).map_or(false, |t| self.ty(&t).as_map().is_some());
                    if is_map {
                        self.compile_for_loop_map(item, key_loc, value_loc.ice()?)
                    } else {
                        self.compile_for_loop_array(item, key_loc, value_loc.ice()?, value_type_id.ice()?)
                    }
                },
            }
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
        self.write_target(function_addr)?;
        // load constructor (none)
        self.write_immediate_sa(0)?;
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
        self.write_target(function_addr)?;
        size += size_of::<StackAddress>();
        // load constructor
        let constructor = self.constructor(self.ty(&item.struct_type_id))?;
        self.write_immediate_sa(constructor)?;
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
        // use the resolved return type rather than the (possibly absent, for inferred closures) explicit annotation
        frame.ret_size = self.function_by_id(function_id).ret_size(self);
        // a function returning `Generator<..>` is a generator: it suspends via `yield` and completes via
        // the generator-return opcode instead of a normal `ret`.
        let ret_type_id = self.function_by_id(function_id).ret_type_id(self);
        if let Some((key_type_id, value_type_id)) = ret_type_id.and_then(|rt| self.generator_signature(rt)) {
            frame.is_generator = true;
            // value/key slot constructors, used to release a yielded reference held in the header on completion
            frame.generator_value_ctor = self.generator_slot_constructor(Some(value_type_id))?;
            frame.generator_key_ctor = self.generator_slot_constructor(key_type_id)?;
        }
        for arg in item.sig.params.iter() {
            frame.insert(arg.binding_id, frame.arg_pos);
            self.init_state.declare(arg.binding_id);
            self.init_state.initialize(arg.binding_id);
            frame.arg_pos += self.ty(arg).primitive_size() as FrameAddress;
        }

        // clone closed over variables
        for &binding_id in closure_captures {
            frame.insert(binding_id, frame.arg_pos);
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
                    self.write_target(position)?;
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
            LiteralValue::String(string_literal) => {
                // store string on const pool and write instruction to load it from const pool directly onto the heap
                // note: we get the offset from current const_len() and not the store_const() result as that points to
                // the start of the string while the we neeed to point at the meta data it writes before the string (the length)
                let string_const = self.writer.const_len();
                self.writer.store_const(string_literal.as_str());
                self.writer.upload_const(string_const, ItemIndex::MAX);
            }
            LiteralValue::Array(array_literal) => {
                if item.value.is_const_serializable() {
                    // all elements are primitive literals - hoist to const pool
                    let const_offset = self.store_const_primitive_array(&array_literal.elements, item)?;
                    // emit upload_const - const_offset points to the size on the stack
                    let type_id = item.type_id(self).ice()?;
                    let implementor_index = *self.trait_implementor_indices.get(&type_id).unwrap_or(&0);
                    self.writer.upload_const(const_offset, implementor_index);
                } else {
                    // at least one element is non-primitive - use existing push-then-upload path
                    for element in &array_literal.elements {
                        self.compile_expression(element)?;
                    }
                    let array_ty = self.ty(item).as_array().ice_msg("Expected array type, got something else")?;
                    let size = array_literal.elements.len() as StackAddress * self.ty(&array_ty.type_id).primitive_size() as StackAddress;
                    let type_id = item.type_id(self).ice()?;
                    self.writer.upload(size, *self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
                }
            },
            LiteralValue::Map(map_literal) => {
                // allocate an empty map, then populate it entry by entry. The map reference is duplicated
                // before each insert (which consumes its copy) so the original remains as the result.
                let constructor = self.constructor(self.ty(item))?;
                self.writer.map_new();
                for (key, value) in &map_literal.entries {
                    self.writer.clone(size_of::<HeapAddress>() as FrameAddress); // stack: map map
                    self.compile_expression(key)?;                              // stack: map map key
                    self.compile_expression(value)?;                            // stack: map map key value
                    self.writer.map_append(constructor);                        // stack: map
                }
            },
            LiteralValue::Struct(struct_literal) => {
                let struct_def = ty.as_struct().ice_msg("Expected struct, got something else")?;
                // collect fields first to avoid borrow checker
                let fields: Vec<_> = struct_def.fields.iter().map(|(name, _)| struct_literal.fields.get(name).expect("Missing struct field")).collect();

                if item.value.is_const_serializable() {
                    // all fields are primitive - hoist to const pool
                    let const_offset = self.store_const_primitive_struct(&fields, item)?;
                    // emit upload_const - const_offset points to the size on the stack
                    let type_id = item.type_id(self).ice()?;
                    let implementor_index = *self.trait_implementor_indices.get(&type_id).unwrap_or(&0);
                    self.writer.upload_const(const_offset, implementor_index);
                } else {
                    // at least one field is non-constant - use existing push-then-upload path
                    for field in &fields {
                        self.compile_expression(field)?;
                    }
                    let struct_ty = self.ty(item).as_struct().ice_msg("Expected struct type, got something else")?;
                    let size = struct_ty.fields.iter().fold(0, |acc, f| acc + self.ty(f.1).primitive_size() as StackAddress);
                    let type_id = item.type_id(self).ice()?;
                    self.writer.upload(size, *self.trait_implementor_indices.get(&type_id).unwrap_or(&0));
                }
            },
        }
        Ok(())
    }

    /// Compiles the given unary operation.
    fn compile_unary_op(self: &mut Self, item: &ast::UnaryOp) -> CompileResult {
        use crate::frontend::ast::UnaryOperator as UO;
        match item.op {
            // logical-not for bool, bitwise-not for integers
            UO::Not => {
                self.compile_expression(&item.expr)?;
                comment!(self, "{}", item);
                let item_type = self.ty(&item.expr);
                if item_type.is_integer() {
                    self.write_bitnot(item_type)?;
                } else {
                    self.writer.not();
                }
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
            // bitwise
            BO::BitAnd => self.write_bitand(ty_result)?,
            BO::BitOr => self.write_bitor(ty_result)?,
            BO::BitXor => self.write_bitxor(ty_result)?,
            // shifts: the amount (right operand) is an independent integer normalized to u32
            BO::Shl => { self.write_shift_amount_cast(self.ty(&item.right))?; self.write_shl(ty_result)? },
            BO::Shr => { self.write_shift_amount_cast(self.ty(&item.right))?; self.write_shr(ty_result)? },
            // comparison
            BO::Greater     => self.write_cgt(ty_left)?,
            BO::GreaterOrEq => self.write_cgte(ty_left)?,
            BO::Less        => self.write_clt(ty_left)?,
            BO::LessOrEq    => self.write_clte(ty_left)?,
            BO::Equal       => self.write_ceq(ty_left)?,
            BO::NotEqual    => self.write_cneq(ty_left)?,
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
        if compare_type.as_map().is_some() {
            // stack: map-reference, key. Map index-write is handled in compile_assignment_to_offset and
            // never reaches here, so only the read (Index) is expected.
            let constructor = self.constructor(compare_type)?;
            match item.op {
                Index => { self.writer.map_index(constructor); },
                _ => Self::ice_at(item, "unsupported map offset operation")?,
            }
            return Ok(());
        }
        match item.op {
            Index => {
                comment!(self, "[{}]", &item.right);
                // fetch heap value at reference-target
                self.write_heap_fetch_element(compare_type, result_type)?;
            },
            IndexWrite => {
                comment!(self, "[{}] (writing)", &item.right);
                // compute and push the address of the reference-target
                self.writer.index(result_type.primitive_size() as u16);
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
            // TODO: move to future optimizer. here to test whether these opcodes are even beneficial (they are)
            // Variable/variable optimizations
            (op @ (Mul | Add | Sub | Less | LessOrEq | Greater | GreaterOrEq), Some((E::Variable(var_l), E::Variable(var_r)))) if self.ty(&item.left).is_numeric() => {
                let addr_l = self.load_variable(var_l)?;
                let addr_r = self.load_variable(var_r)?;
                comment!(self, "{}{}{}", var_l.ident, item.op, var_r.ident);
                match op {
                    Mul         => self.write_mulvv(self.ty(&item.left), addr_l, addr_r)?,
                    Add         => self.write_addvv(self.ty(&item.left), addr_l, addr_r)?,
                    Sub         => self.write_subvv(self.ty(&item.left), addr_l, addr_r)?,
                    Less        => self.write_cltvv(self.ty(&item.left), addr_l, addr_r)?,
                    LessOrEq    => self.write_cltevv(self.ty(&item.left), addr_l, addr_r)?,
                    Greater     => self.write_cltvv(self.ty(&item.left), addr_r, addr_l)?, // swapped arguments, a>b equals b<a
                    GreaterOrEq => self.write_cltevv(self.ty(&item.left), addr_r, addr_l)?, // swapped arguments, a>=b equals b<=a
                    _ => unreachable!(),
                };
                Ok(())
            },
            // Expression/variable optimizations
            (op @ (Mul | Add | Sub), Some((exp_l, E::Variable(var_r)))) if self.ty(&item.left).is_numeric() => {
                self.compile_expression(exp_l)?;
                let addr_r = self.load_variable(var_r)?;
                comment!(self, "{}{}", item.op, var_r.ident);
                match op {
                    Mul         => self.write_mulxv(self.ty(&item.left), addr_r)?,
                    Add         => self.write_addxv(self.ty(&item.left), addr_r)?,
                    Sub         => self.write_subxv(self.ty(&item.left), addr_r)?,
                    Less        => self.write_cltxv(self.ty(&item.left), addr_r)?,
                    LessOrEq    => self.write_cltexv(self.ty(&item.left), addr_r)?,
                    Greater     => self.write_cgtxv(self.ty(&item.left), addr_r)?,
                    GreaterOrEq => self.write_cgtexv(self.ty(&item.left), addr_r)?,
                    _ => unreachable!(),
                };
                Ok(())
            },
            // Variable/expression (swapped) optimizations
            (op @ (Mul | Add), Some((E::Variable(var_r), exp_l))) if self.ty(&item.left).is_numeric() => {
                self.compile_expression(exp_l)?;
                let addr_r = self.load_variable(var_r)?;
                comment!(self, "{}{}", item.op, var_r.ident);
                match op {
                    Mul         => self.write_mulxv(self.ty(&item.left), addr_r)?,
                    Add         => self.write_addxv(self.ty(&item.left), addr_r)?,
                    // Sub is not commuatative
                    Less        => self.write_cgtxv(self.ty(&item.left), addr_r)?, // swapped gt/lt
                    LessOrEq    => self.write_cgtexv(self.ty(&item.left), addr_r)?,
                    Greater     => self.write_cltxv(self.ty(&item.left), addr_r)?,
                    GreaterOrEq => self.write_cltexv(self.ty(&item.left), addr_r)?,
                    _ => unreachable!(),
                };
                Ok(())
            },
            // Expression/literal optimizations
            (op @ (Mul | Add | Sub | Less | LessOrEq | Greater | GreaterOrEq), Some((exp_l, E::Literal(lit_r)))) if self.ty(lit_r).is_numeric() => {
                self.compile_expression(exp_l)?;
                let numeric = lit_r.value.as_numeric().ice()?;
                comment!(self, "{}{}", item.op, numeric);
                match op {
                    Mul         => self.write_mulxc(self.ty(&item.left), numeric)?,
                    Add         => self.write_addxc(self.ty(&item.left), numeric)?,
                    Sub         => self.write_subxc(self.ty(&item.left), numeric)?,
                    Less        => self.write_cltxc(self.ty(&item.left), numeric)?,
                    LessOrEq    => self.write_cltexc(self.ty(&item.left), numeric)?,
                    Greater     => self.write_cgtxc(self.ty(&item.left), numeric)?,
                    GreaterOrEq => self.write_cgtexc(self.ty(&item.left), numeric)?,
                    _ => unreachable!(),
                };
                Ok(())
            },
            // Literal/Expression (swapped) optimizations
            (op @ (Mul | Add | Less | LessOrEq | Greater | GreaterOrEq), Some((E::Literal(lit_r), exp_l))) if self.ty(lit_r).is_numeric() => {
                self.compile_expression(exp_l)?;
                let numeric = lit_r.value.as_numeric().ice()?;
                comment!(self, "{}{}", item.op, numeric);
                match op {
                    Mul         => self.write_mulxc(self.ty(&item.left), numeric)?,
                    Add         => self.write_addxc(self.ty(&item.left), numeric)?,
                    // Sub is not commuatative
                    Less        => self.write_cgtxc(self.ty(&item.left), numeric)?, // swapped gt/lt
                    LessOrEq    => self.write_cgtexc(self.ty(&item.left), numeric)?,
                    Greater     => self.write_cltxc(self.ty(&item.left), numeric)?,
                    GreaterOrEq => self.write_cltexc(self.ty(&item.left), numeric)?,
                    _ => unreachable!(),
                };
                Ok(())
            },
            // General operations
            (Add | Sub | Mul | Div | Rem | BitAnd | BitOr | BitXor | Shl | Shr
                | Less | Greater | LessOrEq | GreaterOrEq | Equal | NotEqual, _) => {
                self.compile_binary_op_simple(item)
            },
            (And | Or, _) => {
                self.compile_binary_op_shortcircuiting(item)
            },
            (Index | IndexWrite | Access | AccessWrite, _) => {
                self.compile_binary_op_offseting(item)
            },
            (Call, _) => {
                self.compile_binary_op_call(item)
            },
            (Assign | AddAssign | SubAssign | MulAssign | DivAssign | RemAssign
                | BitAndAssign | BitOrAssign | BitXorAssign | ShlAssign | ShrAssign
                | Range | RangeInclusive, _) => {
                Self::ice_at(item, "Unexpected operator in compile_binary_op")
            },
        }
    }

    /// Compiles a primitive type cast. Casts backed by an intrinsic conversion trait (e.g. `ToString`)
    /// are lowered to method calls during resolution and never reach this point.
    fn compile_cast(self: &mut Self, item: &ast::Cast) -> CompileResult {
        self.compile_expression(&item.expr)?;
        let from = self.ty(&item.expr);
        let to = self.ty(&item.ty);
        self.write_cast(from, to)?;
        Ok(())
    }
}

impl<T> Compiler<'_, T> where T: VMFunc<T> {

    /// Returns the type of given AST item.
    fn ty(self: &Self, item: &impl Typeable) -> &Type {
        match item.type_id(self) {
            None => panic!("Unresolved type encountered."),
            Some(type_id) => self.type_by_id(type_id)
        }
    }

    /// Returns the constructor offset for a generator value/key slot of the given type, or
    /// `GEN_PRIMITIVE_CTOR` if the type is primitive or absent (no key). Reference types are refcounted
    /// in the slot; primitives are not.
    fn generator_slot_constructor(self: &Self, type_id: Option<TypeId>) -> CompileResult<StackAddress> {
        Ok(match type_id {
            Some(type_id) if self.ty(&type_id).is_ref() => self.constructor(self.ty(&type_id))?,
            _ => GEN_PRIMITIVE_CTOR,
        })
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
        let target_type = self.ty(&target.callable_type_id).as_callable().ice()?;
        let other_type = self.ty(&other.callable_type_id).as_callable().ice()?;
        if !self.type_accepted_for(other_type.ret_type_id.ice()?, target_type.ret_type_id.ice()?) {
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
            // For Index compound assignment, slot the temp bindings if they are typed.
            if let Some(ast::CompoundDispatch::IndexMethod { recv, idx, .. }) = &assignment.op_dispatch {
                if let Some(recv_type_id) = self.binding_by_id(*recv).type_id {
                    frame.insert(*recv, frame.var_pos);
                    frame.var_pos += self.ty(&recv_type_id).primitive_size() as FrameAddress;
                }
                if let Some(idx_type_id) = self.binding_by_id(*idx).type_id {
                    frame.insert(*idx, frame.var_pos);
                    frame.var_pos += self.ty(&idx_type_id).primitive_size() as FrameAddress;
                }
            }
            // Also walk the left side for nested expressions (e.g. the index receiver).
            self.create_stack_frame_exp(&assignment.left, frame)?;
            self.create_stack_frame_exp(&assignment.right, frame)?;
        } else if let ast::Expression::BinaryOp(binary_op) = expression {
            if let ast::BinaryOperand::Expression(ast::Expression::Block(block)) = &binary_op.left {
                self.create_stack_frame_block(block, frame)?;
            }
            if binary_op.op == ast::BinaryOperator::Call {
                for arg in &binary_op.right.as_argument_list().ice()?.args {
                    self.create_stack_frame_exp(arg, frame)?;
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
        } else if let ast::Expression::MatchBlock(match_block) = expression {
            self.create_stack_frame_exp(&match_block.expr, frame)?;
            for (pattern, block) in &match_block.branches {
                self.create_stack_frame_pattern(pattern, frame);
                self.create_stack_frame_block(block, frame)?;
            }
        }
        Ok(())
    }

    /// Allocates stack frame slots for the bindings introduced by a (possibly nested) match pattern.
    fn create_stack_frame_pattern(self: &Self, pattern: &ast::Pattern, frame: &mut StackFrame) {
        match pattern {
            ast::Pattern::Binding(binding) => {
                if let Some(type_id) = self.binding_by_id(binding.binding_id).type_id {
                    frame.insert(binding.binding_id, frame.var_pos);
                    frame.var_pos += self.ty(&type_id).primitive_size() as FrameAddress;
                }
            },
            ast::Pattern::VariantTuple(variant) => {
                for element in &variant.elements {
                    self.create_stack_frame_pattern(element, frame);
                }
            },
            ast::Pattern::Struct(structure) => {
                for (_, field_pattern) in &structure.fields {
                    self.create_stack_frame_pattern(field_pattern, frame);
                }
            },
            _ => {},
        }
    }

    /// Creates stack frame variables for blocks.
    fn create_stack_frame_block(self: &Self, item: &ast::Block, frame: &mut StackFrame) -> CompileResult {
        // todo: check if this would be more readable as a trait on AST?
        for statement in item.statements.iter() {
            if let ast::Statement::LetBinding(binding) = statement {
                frame.insert(binding.binding_id, frame.var_pos);
                frame.var_pos += self.ty(binding).primitive_size() as FrameAddress;
                if let Some(expression) = &binding.expr {
                    self.create_stack_frame_exp(expression, frame)?;
                }
            } else if let ast::Statement::LetPattern(let_pattern) = statement {
                self.create_stack_frame_pattern(&let_pattern.pattern, frame);
                self.create_stack_frame_exp(&let_pattern.expr, frame)?;
            } else if let ast::Statement::ForLoop(for_loop) = statement {
                for binding in [ for_loop.iter_key.as_ref(), for_loop.iter.as_ref() ].into_iter().flatten() {
                    frame.insert(binding.binding_id, frame.var_pos);
                    frame.var_pos += self.ty(binding).primitive_size() as FrameAddress;
                }
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
            } else if let ast::Statement::Return(ret) = statement {
                // any new bindings within a returned expression may need to be allocated
                self.create_stack_frame_exp(&ret.expr, frame)?;
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
        // a generator carrier is structurally a struct, but its heap object holds an opaque header +
        // frozen frame, not its `$value`/`$key` fields. Emit a Generator constructor so the refcounter
        // refcounts the object without recursing into the (non-existent) field layout. The frozen frame's
        // own nested refs are released on the happy path by the body's scope destructors; on a mid-flight
        // drop they are released via the live-ref-map referenced from the carrier's header
        if self.generator_signature(type_id).is_some() {
            *prev_primitive = None;
            self.writer.store_const(Constructor::Generator);
            // value- and key-constructor offsets, used to release the yielded value/key held in the header
            // on drop. Written as placeholders here and patched after all constructors are serialized,
            // since the value/key types' constructors may not exist yet (forward references).
            self.writer.store_const(GEN_PRIMITIVE_CTOR);
            self.writer.store_const(GEN_PRIMITIVE_CTOR);
            return Ok(position);
        }
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
            Type::Map(map) => {
                // keys and values are boxed (stored inline as HeapRefs), so the key/value constructors
                // describe the target of those references: the boxed primitive object or the ref-type value.
                *prev_primitive = None;
                self.writer.store_const(Constructor::Map);
                store_len(&mut || {
                    self.store_constructor(map.key_type_id.ice_msg("Unresolved map key type")?, &mut None, &mut 0)?;
                    self.store_constructor(map.value_type_id.ice_msg("Unresolved map value type")?, &mut None, &mut 0)
                })?;
                *prev_primitive = None;
            }
            Type::Callable(_callable) => {
                *prev_primitive = None;
                self.writer.store_const(Constructor::Closure);
            },
            Type::String => {
                *prev_primitive = None;
                self.writer.store_const(Constructor::String);
            }
            Type::Trait(_) | Type::TraitBound(_) => {
                // A nested trait-object reference (single trait or multiple-trait bound). Its concrete
                // type (and thus constructor) is only known at runtime, so emit a virtual marker that the
                // VM resolves via the referenced object's implementor index, mirroring the offset-0
                // indirection used for top-level trait objects.
                *prev_primitive = None;
                self.writer.store_const(Constructor::Virtual);
            }
            // simple (C-like) enums are stored inline as their discriminant; they are primitives and fall
            // through to the primitive arm below. Only data-carrying enums need the Enum constructor.
            Type::Enum(enumeration) if enumeration.primitive.is_none() => {
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

    /// Serializes a struct literal with only primitive fields into the const pool and emits one `upload_const`.
    fn store_const_primitive_struct(self: &mut Self, fields: &[&ast::Expression], item: &ast::Literal) -> CompileResult<StackAddress> {
        // get struct type and compute total byte size
        let struct_ty = self.ty(item).as_struct().ice_msg("Expected struct type")?;
        let total_size: StackAddress = struct_ty.fields.iter().fold(0, |acc, (_, type_id_opt)| {
            let type_id = type_id_opt.expect("Unresolved struct field type");
            acc + self.type_by_id(type_id).primitive_size() as StackAddress
        });
        // record const pool position BEFORE writing (this is where the size will be)
        let const_offset = self.writer.const_len();
        // write size prefix into const pool
        self.writer.store_const(total_size);
        // write raw field bytes into const pool
        let mut program = self.writer.program();
        program.const_descriptors.push(ConstDescriptor { size: total_size });
        let consts = &mut program.consts;
        for (field_expr, (_, type_id_opt)) in fields.iter().zip(struct_ty.fields.iter()) {
            let type_id = type_id_opt.expect("Unresolved struct field type");
            let field_type = self.type_by_id(type_id);
            self.serialize_literal_bytes(field_expr, field_type, consts)?;
        }
        Ok(const_offset)
    }

    /// Serializes an array literal with primitive elements into the const pool and emits one `upload_const`.
    fn store_const_primitive_array(self: &mut Self, elements: &[ast::Expression], item: &ast::Literal) -> CompileResult<StackAddress> {
        let array_ty = self.ty(item).as_array().ice_msg("Expected array type")?;
        let element_type_id = array_ty.type_id.expect("Unresolved array element type");
        let element_type = self.type_by_id(element_type_id);
        let element_size = element_type.primitive_size() as StackAddress;
        let total_size: StackAddress = elements.len() as StackAddress * element_size;
        // record const pool position BEFORE writing (this is where the size will be)
        let const_offset = self.writer.const_len();
        // write size prefix into const pool
        self.writer.store_const(total_size);
        // write raw element bytes into const pool
        let mut program = self.writer.program();
        program.const_descriptors.push(ConstDescriptor { size: total_size });
        let consts = &mut program.consts;
        for element in elements {
            self.serialize_literal_bytes(element, element_type, consts)?;
        }
        Ok(const_offset)
    }

    /// Appends the raw bytes of a const-serializable literal to the given buffer.
    fn serialize_literal_bytes(self: &Self, expr: &ast::Expression, field_type: &Type, buf: &mut Vec<u8>) -> CompileResult {
        let literal = expr.as_literal().ice_msg("Expected literal expression for const aggregate")?;
        match &literal.value {
            ast::LiteralValue::Numeric(numeric) => {
                match numeric {
                    Numeric::Signed(v) => match field_type {
                        Type::i8  => buf.extend_from_slice(&(*v as i8).to_ne_bytes()),
                        Type::i16 => buf.extend_from_slice(&(*v as i16).to_ne_bytes()),
                        Type::i32 => buf.extend_from_slice(&(*v as i32).to_ne_bytes()),
                        Type::i64 => buf.extend_from_slice(&(*v as i64).to_ne_bytes()),
                        _ => Self::ice("Unexpected signed type in const aggregate")?,
                    },
                    Numeric::Unsigned(v) => match field_type {
                        Type::u8 | Type::i8  => buf.extend_from_slice(&(*v as u8).to_ne_bytes()),
                        Type::u16 | Type::i16 => buf.extend_from_slice(&(*v as u16).to_ne_bytes()),
                        Type::u32 | Type::i32 => buf.extend_from_slice(&(*v as u32).to_ne_bytes()),
                        Type::u64 | Type::i64 => buf.extend_from_slice(&(*v as u64).to_ne_bytes()),
                        _ => Self::ice("Unexpected unsigned type in const aggregate")?,
                    },
                    Numeric::Float(v) => match field_type {
                        Type::f32 => buf.extend_from_slice(&(*v as f32).to_ne_bytes()),
                        Type::f64 => buf.extend_from_slice(&v.to_ne_bytes()),
                        _ => Self::ice("Unexpected float type in const aggregate")?,
                    },
                };
            }
            ast::LiteralValue::Bool(v) => {
                buf.push(*v as u8);
            }
            ast::LiteralValue::Void => {
                // Zero bytes - nothing to write
            }
            // Nested structs/arrays/strings/maps are not const-serializable
            _ => Self::ice("Non-serializable literal in const aggregate")?,
        }
        Ok(())
    }

    /// Collects the ref-typed frame slots that are live (in scope and initialized) at the current
    /// position within a generator body. Each entry is `(frame_offset, constructor_offset)`; on a
    /// mid-flight drop the VM uses these to release the frozen frame's heap references. `MaybeInitialized`
    /// slots are included - at runtime a null reference (heap index 0) marks them as actually uninitialized
    /// and is skipped.
    fn collect_generator_live_refs(self: &Self) -> CompileResult<Vec<(StackAddress, StackAddress)>> {
        let frame = self.locals.current();
        let mut entries = Vec::new();
        for (&binding_id, &local) in frame.map.iter() {
            if self.init_state.in_scope(binding_id) && self.init_state.initialized(binding_id) != BranchingState::Uninitialized {
                let type_id = self.binding_by_id(binding_id).type_id.ice()?;
                let ty = self.ty(&type_id);
                if ty.is_ref() {
                    entries.push((local as StackAddress, self.constructor(ty)?));
                }
            }
        }
        Ok(entries)
    }

    /// Computes the byte offset of a data-variant field within its heap object (past the leading variant index).
    fn variant_field_offset(self: &Self, field_type_ids: &[Option<TypeId>], index: usize) -> CompileResult<StackAddress> {
        let mut offset = size_of::<VariantIndex>() as StackAddress;
        for field_type_id in &field_type_ids[..index] {
            offset += self.ty(&field_type_id.ice()?).primitive_size() as StackAddress;
        }
        Ok(offset)
    }

    /// Returns the cloned field type-ids of the named data variant of the given enum type.
    fn variant_data_fields(self: &Self, subject_type_id: TypeId, variant_name: &str) -> CompileResult<Vec<Option<TypeId>>> {
        let enum_ = self.ty(&subject_type_id).as_enum().ice()?;
        let variant = enum_.variants.iter().find(|(name, _)| name == variant_name).ice()?;
        Ok(variant.1.as_data().ice()?.clone())
    }

    /// The type of the value a pattern matches against, given the access path to it. The empty path refers
    /// to the match subject itself; otherwise the type of the last navigation step is the value's type.
    fn pattern_value_type(self: &Self, subject_type_id: TypeId, access: &[(StackAddress, TypeId)]) -> TypeId {
        access.last().map(|&(_, type_id)| type_id).unwrap_or(subject_type_id)
    }

    /// Walks the AST to find all reference-type user consts and reserves const pool space for their
    /// heap addresses. Updates resolved metadata with the real const pool offset.
    fn collect_user_const_heap_refs(self: &mut Self) -> CompileResult {
        let modules = self.modules_ref.expect("modules_ref not set");
        for module in modules {
            for statement in module.statements() {
                self.walk_statement(statement);
            }
        }
        Ok(())
    }

    /// Visitor helper that checks if a ConstDef is a reference type and reserve const pool space for its heap address.
    fn collect_user_const_heap_refs_visitor(&mut self, cd: &ast::ConstDef) {
        let constant_id = cd.constant_id.expect("ConstDef not resolved");
        let constant = self.resolved.constant(constant_id);
        let type_id = constant.type_id.expect("Const type not resolved");
        let ty = self.ty(&type_id);
        if ty.is_ref() {
            let offset = self.writer.store_const(HeapAddress::default());
            self.resolved.constant_mut(constant_id).value = ConstantValue::UserConst(UserConstValue::HeapRef(offset));
            self.user_const_heap_refs.push((constant_id, offset));
        }
    }

    /// Finds the ConstDef AST node for a given constant_id by walking the modules.
    fn find_const_def_in_modules(modules: &[ParsedModule], constant_id: ConstantId) -> Option<&ast::ConstDef> {
        let mut finder = ConstDefFinder { constant_id, found: None };
        for module in modules {
            for statement in module.statements() {
                finder.walk_statement(statement);
                if finder.found.is_some() {
                    return finder.found;
                }
            }
        }
        None
    }
}

/// Opcode writer functions.
impl<T> Compiler<'_, T> where T: VMFunc<T> {

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

    /// Writes refcount cleanup for all active for-loops when exiting early via `return`.
    ///
    /// Array/map loops hold two references (clone + source) that must be Dec'd.
    /// Generator loops hold one reference (generator) that must be Dec'd.
    /// Range loops leave the upper bound on the stack; it must be discarded.
    fn write_for_loop_cleanup(self: &mut Self) -> CompileResult {
        // Iterate in reverse (outermost first) so that inner-loop stack artifacts
        // are discarded before outer-loop refs are popped.
        for cleanup in self.for_loop_cleanup.iter().rev() {
            match cleanup {
                ForLoopCleanup::Collection(type_id) => {
                    let ty = self.ty(type_id);
                    comment!(self, "for-loop collection cleanup (return)");
                    self.write_cnt(ty, false, HeapRefOp::Dec)?;    // release clone
                    self.write_cnt(ty, false, HeapRefOp::Dec)?;    // release source
                }
                ForLoopCleanup::Generator(type_id) => {
                    let ty = self.ty(type_id);
                    comment!(self, "for-loop generator cleanup (return)");
                    self.write_cnt(ty, false, HeapRefOp::Dec)?;    // release generator reference
                }
                ForLoopCleanup::Range(size) => {
                    comment!(self, "for-loop range cleanup (return)");
                    self.writer.discard(*size as FrameAddress);    // discard upper bound
                }
            }
        }
        Ok(())
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

    /// Emits pre-main initialization code: compile each reference-type const expression and store
    /// the resulting heap address in the const pool slot.
    fn write_const_init(self: &mut Self) -> CompileResult {
        // Collect (constant_id, offset) pairs first to avoid borrow conflicts.
        let init_items: Vec<(ConstantId, StackAddress)> = self.user_const_heap_refs.clone();
        let modules = self.modules_ref.expect("modules_ref not set");
        for (constant_id, offset) in init_items {
            let const_def = Self::find_const_def_in_modules(modules, constant_id).expect("ConstDef not found");
            comment!(self, "init user const {}", const_def.ident.name);
            // Get the constructor for the const's type
            let type_id = const_def.type_id(self).expect("Const type not resolved");
            let constructor = self.constructor(self.ty(&type_id))?;
            // Compile the const expression — stack: heap_addr
            self.compile_expression(const_def.expr.as_ref().ice()?)?;
            // Clone it — stack: heap_addr heap_addr
            self.writer.clone(size_of::<HeapAddress>() as FrameAddress);
            // Store one copy in the const pool slot — stack: heap_addr
            self.writer.store_pool(offset);
            // Increment refcount (pops the heap address) — stack: --
            self.writer.cnt_sa(constructor, HeapRefOp::Inc);
        }
        Ok(())
    }

    /// Emits post-main cleanup code: decrement refcounts for reference-type consts.
    fn write_const_cleanup(self: &mut Self) -> CompileResult {
        let modules = self.modules_ref.expect("modules_ref not set");
        for (constant_id, offset) in &self.user_const_heap_refs {
            let const_def = Self::find_const_def_in_modules(modules, *constant_id).expect("ConstDef not found");
            comment!(self, "cleanup user const {}", const_def.ident.name);
            let type_id = const_def.type_id(self).expect("Const type not resolved");
            let constructor = self.constructor(self.ty(&type_id))?;
            self.writer.load_pool(*offset);
            self.writer.cnt_sa(constructor, HeapRefOp::Dec);
        }
        Ok(())
    }

    /// Writes given numeric as an immediate value.
    fn write_immediate(self: &Self, ty: &Type, numeric: Numeric) -> CompileResult<StackAddress> {
        Ok(match numeric {
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
                    Type::i8 | Type::u8 | Type::bool => self.writer.immediate8(v as u8),
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

    fn write_immediate_sa(self: &Self, v: StackAddress) -> CompileResult<StackAddress> {
        Ok(match size_of::<StackAddress>() {
            1 => self.writer.immediate8(v as u8),
            2 => self.writer.immediate16(v as u16),
            4 => self.writer.immediate32(v as u32),
            8 => self.writer.immediate64(v as u64),
            size @ _ => Self::ice(&format!("Invalid stack address size: {:?}", size))?,
        })
    }

    /// Emits a `target` instruction that pushes a bytecode address onto the
    /// stack.  Unlike `write_immediate_sa`, the optimizer knows this value
    /// is a code address and will remap it when instructions are removed.
    fn write_target(self: &Self, addr: StackAddress) -> CompileResult<StackAddress> {
        Ok(self.writer.target(addr as u32))
    }

    /// Builds the generator entry live-ref-map: the ref-typed arguments captured into a not-yet-started
    /// generator's frame, released if it is dropped before the first `next()`. Arguments occupy the start
    /// of the frame in declaration order, so their frame offsets are the cumulative argument sizes.
    fn write_generator_entry_map(self: &Self, function_id: FunctionId) -> CompileResult<StackAddress> {
        let mut entries = Vec::new();
        let mut frame_offset: StackAddress = 0;
        for arg_type_id in self.resolved.function(function_id).arg_type_ids(self) {
            let ty = self.ty(&arg_type_id.ice()?);
            if ty.is_ref() {
                entries.push((frame_offset, self.constructor(ty)?));
            }
            frame_offset += ty.primitive_size() as StackAddress;
        }
        Ok(self.write_generator_live_ref_map(&entries))
    }

    /// Serializes a generator live-ref-map into the const pool and returns its offset. The map is a count
    /// of live reference slots followed by that many `(frame_offset, constructor_offset)` pairs (each
    /// StackAddress-sized). Its offset is stored in the generator header so the VM can release the frozen
    /// frame's references on a mid-flight drop.
    fn write_generator_live_ref_map(self: &Self, entries: &[ (StackAddress, StackAddress) ]) -> StackAddress {
        let position = self.writer.const_len();
        self.writer.store_const(entries.len() as ItemIndex);
        for &(frame_offset, constructor_offset) in entries {
            self.writer.store_const(frame_offset);
            self.writer.store_const(constructor_offset);
        }
        position
    }

    /// Emits code that pushes a borrowed copy of the value selected by `access` onto the stack, starting from
    /// the match subject which is expected on top of the stack. Each step offsets into the current heap object
    /// and fetches the field there, dereferencing across heap boundaries for nested (reference) values. No
    /// refcounts are touched, so the result is a borrow the caller must consume (compare) or store (which incs).
    fn write_pattern_value_load(self: &mut Self, subject_type_id: TypeId, access: &[(StackAddress, TypeId)]) -> CompileResult {
        self.write_clone(self.ty(&subject_type_id));
        for &(offset, field_type_id) in access {
            self.writer.offsetx_16(offset as FrameAddress);
            self.write_heap_fetch(self.ty(&field_type_id))?;
        }
        Ok(())
    }

    /// Emits the buffer-clone builtin for the collection reference on top of the stack, consuming it and
    /// pushing an independent shallow clone (the clone shares the original's element/boxed sub-references
    /// at their current refcount, per the heap-aggregate refcount-0 convention).
    fn write_collection_clone(self: &mut Self, collection_type_id: TypeId) -> CompileResult {
        if self.ty(&collection_type_id).as_map().is_some() {
            let constructor = self.constructor(self.ty(&collection_type_id))?;
            self.writer.call_builtinx(Builtin::map_clone, 0, constructor);
        } else {
            let element_type_id = self.ty(&collection_type_id).as_array().ice()?.type_id.ice()?;
            let constructor = self.constructor(self.ty(&collection_type_id))?;
            if self.ty(&element_type_id).is_ref() {
                let element_constructor = self.constructor(self.ty(&element_type_id))?;
                self.writer.call_builtinx(Builtin::array_clonex, constructor, element_constructor);
            } else {
                match self.ty(&element_type_id).primitive_size() {
                    8 => self.writer.call_builtinx(Builtin::array_clone64, constructor, 0),
                    4 => self.writer.call_builtinx(Builtin::array_clone32, constructor, 0),
                    2 => self.writer.call_builtinx(Builtin::array_clone16, constructor, 0),
                    1 => self.writer.call_builtinx(Builtin::array_clone8, constructor, 0),
                    size @ _ => Self::ice(&format!("Unsupported array element size {} for for-loop clone", size))?,
                };
            }
        }
        Ok(())
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
            let small = constructor as StackAddress <= u16::MAX as StackAddress;
            match (nc, small) {
                (true, true) => self.writer.cnt_16_nc(constructor as FrameAddress, op),
                (true, false) => self.writer.cnt_sa_nc(constructor, op),
                (false, true) => self.writer.cnt_16(constructor as FrameAddress, op),
                (false, false) => self.writer.cnt_sa(constructor, op),
            }
        } else {
            self.writer.position()
        })
    }

    /// Writes instructions to compute member offset for access on a struct.
    fn write_member_offset(self: &Self, offset: StackAddress) {
        if offset > 0 {
            self.writer.offsetx_16(offset as FrameAddress);
        }
    }

    /// Writes instructions to fetch a value of the given size from the target of the top heap reference on the stack.
    fn write_heap_fetch(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_primitive_size!(self, ty, heap_fetch))
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
            select_primitive_size!(self, ty, heap_put)
        })
    }

    /// Writes instructions to fetch a member of the struct whose reference is at the top of the stack.
    fn write_heap_fetch_member(self: &Self, container_type: &Type, result_type: &Type, offset: StackAddress) -> CompileResult<StackAddress> {
        let constructor = self.constructor(container_type)?;
        Ok(if result_type.is_ref() {
            self.writer.heap_fetch_memberx(offset, constructor)
        } else {
            select_primitive_size!(self, result_type, heap_fetch_member, offset, constructor)
        })
    }

    /// Writes instructions to fetch an element of the array whose reference is at the top of the stack.
    fn write_heap_fetch_element(self: &Self, container_type: &Type, result_type: &Type) -> CompileResult<StackAddress> {
        let constructor = self.constructor(container_type)?;
        Ok(if result_type.is_ref() {
            self.writer.heap_fetch_elementx(constructor)
        } else {
            select_primitive_size!(self, result_type, heap_fetch_element, constructor)
        })
    }

    /// Writes an appropriate variant of the store instruction.
    /// If the value being written is a heap reference, its refcount will be increased and unless the local is not active
    /// and the replaced value will have its refcount decreased.
    fn write_store(self: &Self, ty: &Type, loc: FrameAddress, binding_id: Option<BindingId>) -> CompileResult<StackAddress> {
        Ok(if ty.is_ref() && binding_id.is_some() {
            let constructor = self.constructor(ty)?;
            let binding_id = binding_id.ice()?;
            match self.init_state.initialized(binding_id) {
                BranchingState::Initialized => self.writer.storex_replace(loc, constructor),
                BranchingState::MaybeInitialized => self.writer.storex_replace(loc, constructor), // used to be separate instruction, replace now handles both
                // a binding declared outside a loop may already hold a value from a previous iteration, so it
                // needs replace (decrement-old) semantics even though this single pass still sees it uninitialized
                BranchingState::Uninitialized if self.init_state.assigned_across_loop_iteration(binding_id) => self.writer.storex_replace(loc, constructor),
                BranchingState::Uninitialized => self.writer.storex_new(loc, constructor),
            }
        } else {
            select_primitive_size!(self, ty, store, loc)
        })
    }

    /// Writes an appropriate variant of the load instruction.
    fn write_load(self: &Self, ty: &Type, loc: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_primitive_size!(self, ty, load, loc))
    }

    /// Discard topmost stack value and drop temporaries for reference types.
    fn write_discard(self: &Self, ty: &Type) -> CompileResult {
        if ty.is_ref() {
            self.write_cnt(ty, true, HeapRefOp::Free)?;
        }
        if ty.primitive_size() > 0 {
            self.writer.discard(ty.primitive_size() as FrameAddress);
        }
        Ok(())
    }

    /// Write instruction to clone the top stack value.
    fn write_clone(self: &Self, ty: &Type) -> StackAddress {
        self.writer.clone(ty.primitive_size() as FrameAddress)
    }

    /// Write call instruction. If the function address is not known yet, a placeholder will be written.
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
            (Type::Map(_), BuiltinType::Map(map_builtin)) => {
                map_builtin.write(self, type_id, None)
            },
            (_, BuiltinType::Generator(generator_builtin)) => {
                // Generator builtins use dedicated gen_* opcodes (handled in the default @write arm).
                generator_builtin.write(self, type_id, None)
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

    /// Write loop instruction.
    fn write_loop(self: &Self, ty: &Type, iter: FrameAddress, start: StackAddress) -> CompileResult<StackAddress> {
        Ok(select_integer_type!(self, ty, loop, iter, start))
    }

    /// Write array:iter instruction.
    fn write_array_iter(self: &Self, element_ty: &Type, element: FrameAddress, exit: StackAddress) -> CompileResult<StackAddress> {
        Ok(if element_ty.is_ref() {
            self.writer.array_iter64(element, exit)
        } else {
            select_primitive_size!(self, element_ty, array_iter, element, exit)
        })
    }

    /// Writes an array iteration instruction: `array:iter` for value-only iteration (`index` is `None`),
    /// or `array_iter_iv` which additionally stores the running index into `index` (`for k, v`).
    fn write_array_iter_iv(self: &Self, element_ty: &Type, index: Option<FrameAddress>, element: FrameAddress, exit: StackAddress) -> CompileResult<StackAddress> {
        Ok(match index {
            None => self.write_array_iter(element_ty, element, exit)?,
            Some(index) => if element_ty.is_ref() {
                self.writer.array_iter_iv64(index, element, exit)
            } else {
                select_primitive_size!(self, element_ty, array_iter_iv, index, element, exit)
            },
        })
    }

    /// Writes a map iteration instruction: `map_iter` for value-only iteration (`key` is `None`), or
    /// `map_iter_kv` which additionally unboxes the current key into `key` (`for k, v`).
    fn write_map_iter(self: &Self, key: Option<FrameAddress>, value: FrameAddress, constructor: StackAddress, exit: StackAddress) -> StackAddress {
        match key {
            None => self.writer.map_iter(value, exit, constructor),
            Some(key) => self.writer.map_iter_kv(key, value, exit, constructor),
        }
    }

    /// Write subtraction instruction.
    fn write_sub(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, sub))
    }

    /// Write variable+variable subtraction instruction.
    fn write_subvv(self: &Self, ty: &Type, left: FrameAddress, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, subvv, left, right))
    }

    /// Write expression+variable subtraction instruction.
    fn write_subxv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, subxv, right))
    }

    /// Write expression+immediate subtraction instruction.
    fn write_subxc(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(select_numeric_cast_type!(self, ty, subxc, right))
    }

    /// Write variable+=immediate subtraction instruction.
    fn write_subvc(self: &Self, ty: &Type, left: FrameAddress, right: Numeric) -> CompileResult<StackAddress> {
        Ok(select_numeric_cast_type!(self, ty, subvc [ left ] , right))
    }

    /// Write general addition instruction.
    fn write_add(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(match ty {
            Type::String => self.writer.string_concatx(),
            _ => select_numeric_type!(self, ty, add),
        })
    }

    /// Write variable+variable addition instruction.
    fn write_addvv(self: &Self, ty: &Type, left: FrameAddress, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, addvv, left, right))
    }

    /// Write expression+variable addition instruction.
    fn write_addxv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, addxv, right))
    }

    /// Write expression+immediate addition instruction.
    fn write_addxc(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(select_numeric_cast_type!(self, ty, addxc, right))
    }

    /// Write variable+=immediate addition instruction.
    fn write_addvc(self: &Self, ty: &Type, left: FrameAddress, right: Numeric) -> CompileResult<StackAddress> {
        Ok(select_numeric_cast_type!(self, ty, addvc [ left ] , right))
    }

    /// Write general multiplication instruction.
    fn write_mul(self: &Self, ty: &Type) -> CompileResult<StackAddress>{
        Ok(select_numeric_type!(self, ty, mul))
    }

    /// Write variable*variable multiplication instruction.
    fn write_mulvv(self: &Self, ty: &Type, left: FrameAddress, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, mulvv, left, right))
    }

    /// Write expression*variable multiplication instruction.
    fn write_mulxv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, mulxv, right))
    }

    /// Write expression*immediate multiplication instruction.
    fn write_mulxc(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(select_numeric_cast_type!(self, ty, mulxc, right))
    }

    /// Write division instruction.
    fn write_div(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, div))
    }

    /// Write remainder instruction.
    fn write_rem(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, rem))
    }

    /// Write bitwise and instruction.
    fn write_bitand(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_integer_type!(self, ty, bitand))
    }

    /// Write bitwise or instruction.
    fn write_bitor(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_integer_type!(self, ty, bitor))
    }

    /// Write bitwise xor instruction.
    fn write_bitxor(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_integer_type!(self, ty, bitxor))
    }

    /// Write bitwise not instruction.
    fn write_bitnot(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_integer_type!(self, ty, bitnot))
    }

    /// Write left-shift instruction (selected by the type of the value being shifted).
    fn write_shl(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_integer_type!(self, ty, shl))
    }

    /// Write right-shift instruction (selected by the type of the value being shifted).
    fn write_shr(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(select_integer_type!(self, ty, shr))
    }

    /// Casts a shift amount (already on the stack) to the u32 width expected by the shift opcodes.
    fn write_shift_amount_cast(self: &Self, from: &Type) -> CompileResult {
        if from != &Type::u32 {
            self.write_cast(from, &Type::u32)?;
        }
        Ok(())
    }

    /// Write negation instruction.
    fn write_neg(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
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
    fn write_ceq(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            select_primitive_size!(self, ty, ceq)
        } else if ty.is_string() {
            self.writer.string_ceq()
        } else if ty.as_enum().is_some() || ty.as_struct().is_some() || ty.as_array().is_some() {
            let constructor = self.constructor(ty)?;
            if constructor <= u16::MAX as StackAddress {
                self.writer.heap_ceq_16(constructor as FrameAddress)
            } else {
                self.writer.heap_ceq_sa(constructor)
            }
        } else {
            Self::ice(&format!("ceq: Unsupported type {:?}", ty))?
        })
    }

    /// Write compare not equal instruction.
    fn write_cneq(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            select_primitive_size!(self, ty, cneq)
        } else if ty.is_string() {
            self.writer.string_cneq()
        } else if ty.as_enum().is_some() || ty.as_struct().is_some() || ty.as_array().is_some() {
            let constructor = self.constructor(ty)?;
            if constructor <= u16::MAX as StackAddress {
                self.writer.heap_cneq_16(constructor as FrameAddress)
            } else {
                self.writer.heap_cneq_sa(constructor)
            }
        } else {
            Self::ice(&format!("cneq: Unsupported type {:?}", ty))?
        })
    }

    /// Write compare less than instruction.
    fn write_clt(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            select_numeric_type!(self, ty, clt)
        } else if ty.is_string() {
            self.writer.string_clt()
        } else {
            Self::ice(&format!("clt: Unsupported type {:?}", ty))?
        })
    }

    /// Write variable<variable comparison instruction.
    fn write_cltvv(self: &Self, ty: &Type, left: FrameAddress, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, cltvv, left, right))
    }

    /// Write expression<immediate comparison instruction.
    fn write_cltxc(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(select_numeric_cast_type!(self, ty, cltxc, right))
    }

    /// Write expression<variable comparison instruction.
    fn write_cltxv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, cltxv, right))
    }

    /// Write compare less than or equal instruction.
    fn write_clte(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            select_numeric_type!(self, ty, clte)
        } else if ty.is_string() {
            self.writer.string_clte()
        } else {
            Self::ice(&format!("clte: Unsupported type {:?}", ty))?
        })
    }

    /// Write variable<=variable comparison instruction.
    fn write_cltevv(self: &Self, ty: &Type, left: FrameAddress, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, cltevv, left, right))
    }

    /// Write expression<=immediate comparison instruction.
    fn write_cltexc(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(select_numeric_cast_type!(self, ty, cltexc, right))
    }

    /// Write expression<=variable comparison instruction.
    fn write_cltexv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, cltexv, right))
    }

    /// Write compare greater than instruction.
    fn write_cgt(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            select_numeric_type!(self, ty, cgt)
        } else if ty.is_string() {
            self.writer.string_cgt()
        } else {
            Self::ice(&format!("cgt: Unsupported type {:?}", ty))?
        })
    }

    /// Write expression<=immediate comparison instruction.
    fn write_cgtxc(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(select_numeric_cast_type!(self, ty, cgtxc, right))
    }

    /// Write expression<=variable comparison instruction.
    fn write_cgtxv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, cgtxv, right))
    }

    /// Write compare greater than or equal instruction.
    fn write_cgte(self: &Self, ty: &Type) -> CompileResult<StackAddress> {
        Ok(if ty.is_primitive() {
            select_numeric_type!(self, ty, cgte)
        } else if ty.is_string() {
            self.writer.string_cgte()
        } else {
            Self::ice(&format!("cgte: Unsupported type {:?}", ty))?
        })
    }

    /// Write expression<=immediate comparison instruction.
    fn write_cgtexc(self: &Self, ty: &Type, right: Numeric) -> CompileResult<StackAddress> {
        Ok(select_numeric_cast_type!(self, ty, cgtexc, right))
    }

    /// Write expression<=variable comparison instruction.
    fn write_cgtexv(self: &Self, ty: &Type, right: FrameAddress) -> CompileResult<StackAddress> {
        Ok(select_numeric_type!(self, ty, cgtexv, right))
    }
}

/// Itsy-trait support functions. // TODO: holy crap those signatures need some custom types
impl<T> Compiler<'_, T> {

    /// Computes the vtable function base-offset for the given function. To get the final offset the dynamic types trait_implementor_index * sizeof(StackAddress) has to be added.
    fn vtable_function_offset(self: &Self, function_id: FunctionId) -> CompileResult<StackAddress> {
        let trait_function_id = *self.trait_function_implementors.get(&function_id).unwrap_or(&function_id);
        let function_index = *self.trait_function_indices.get(&trait_function_id).ice_msg("Invalid trait function id")?;
        Ok(self.trait_vtable_base + (function_index as usize * size_of::<StackAddress>() * self.trait_implementor_indices.len()) as StackAddress)
    }

    /// Generate flat list of all traits and their functions
    fn filter_trait_functions(resolved: &Resolved) -> CompileResult<Vec<(TypeId, &String, FunctionId)>> {
        let mut result = Vec::new();
        for (type_id, trt) in resolved.traits() {
            // intrinsic traits with impl-defined signatures (e.g. `Index`) are never virtually dispatched and
            // their placeholder method constants carry no real signature, so they get no vtable slots.
            if trt.impl_defined {
                continue;
            }
            for (function_name, constant_id) in trt.provided.iter() {
                let constant_id = constant_id.ice_msg("Unresolved trait provided function constant")?;
                let function_id = resolved.constant(constant_id).value.as_function_id().ice_msg("Trait provided function constant is not a function")?;
                result.push((type_id, function_name, function_id));
            }
            for (function_name, constant_id) in trt.required.iter() {
                let constant_id = constant_id.ice_msg("Unresolved trait required function constant")?;
                let function_id = resolved.constant(constant_id).value.as_function_id().ice_msg("Trait required function constant is not a function")?;
                result.push((type_id, function_name, function_id));
            }
        }
        Ok(result)
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

/// Support MetaContainer for Bindings so that methods that need to follow type_ids can be implemented once and be used in both
/// the Resolver where types are scored in Scopes and the Compiler where types are a stored in a Vec.
impl<T> MetaContainer for Compiler<'_, T> {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type {
        self.resolved.ty(type_id)
    }
    fn type_by_id_mut(self: &mut Self, _type_id: TypeId) -> &mut Type {
        unreachable!("Compiler does not mutate types.")
    }
    fn type_flat_name(self: &Self, _type_id: TypeId) -> Option<&String> {
        None // TODO
    }
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
    fn function_by_id(self: &Self, function_id: FunctionId) -> &Function {
        self.resolved.function(function_id)
    }
    fn function_by_id_mut(self: &mut Self, _: FunctionId) -> &mut Function {
        unreachable!("Compiler does not mutate functions.")
    }
}

/// Visitor that reserves const pool slots for const heap refs.
impl<'ast, T> AstVisitor<'ast> for Compiler<'ast, T> where T: VMFunc<T> {
    fn visit_statement(&mut self, stmt: &'ast ast::Statement) -> bool {
        match stmt {
            ast::Statement::ConstDef(cd) => {
                self.collect_user_const_heap_refs_visitor(cd);
            },
            ast::Statement::ImplBlock(ib) => {
                for cd in &ib.consts {
                    self.collect_user_const_heap_refs_visitor(cd);
                }
            },
            ast::Statement::TraitDef(td) => {
                for cd in &td.consts {
                    // Skip required consts (no default value)
                    if cd.expr.is_some() {
                        self.collect_user_const_heap_refs_visitor(cd);
                    }
                }
            },
            _ => {},
        }
        true // keep walking
    }
}

/// Visitor that finds a ConstDef by constant_id during AST walking.
struct ConstDefFinder<'ast> {
    constant_id: ConstantId,
    found: Option<&'ast ast::ConstDef>,
}

impl<'ast> AstVisitor<'ast> for ConstDefFinder<'ast> {
    fn visit_statement(&mut self, stmt: &'ast ast::Statement) -> bool {
        if self.found.is_some() {
            return false; // short-circuit
        }
        match stmt {
            ast::Statement::ConstDef(cd) => {
                if cd.constant_id == Some(self.constant_id) {
                    self.found = Some(cd);
                    return false;
                }
            },
            ast::Statement::ImplBlock(ib) => {
                for cd in &ib.consts {
                    if cd.constant_id == Some(self.constant_id) {
                        self.found = Some(cd);
                        return false;
                    }
                }
            },
            ast::Statement::TraitDef(td) => {
                for cd in &td.consts {
                    if cd.constant_id == Some(self.constant_id) {
                        self.found = Some(cd);
                        return false;
                    }
                }
            },
            _ => {},
        }
        true // keep walking
    }
}
