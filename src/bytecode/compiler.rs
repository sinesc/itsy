//! Bytecode emitter. Compiles bytecode from AST.

use std::{collections::HashMap, fmt::Debug, cell::RefCell};
use crate::util::{Numeric, BindingId, FunctionId, Type, TypeId, TypeKind, FnKind, HeapRef};
use crate::frontend::{ast::{self, Bindable, Returns, CallType}, ResolvedProgram};
use crate::bytecode::{Writer, StoreConst, Program, ARG1, ARG2, ARG3};
use crate::runtime::{VMFunc, CONSTPOOL_INDEX};

/// Describes a single local variable of a stack frame
#[derive(Copy,Clone)]
struct Local {
    /// The load-index for this variable.
    index   : i32,
    /// Whether this variable is currently in scope (stack frame does not equal scope!)
    in_scope: bool,
}

impl Local {
    fn new(index: i32) -> Self {
        Local { index, in_scope: false }
    }
}

/// Maps bindings and arguments to indices relative to the stack frame.
struct Locals {
    map     : HashMap<BindingId, Local>,
    next_arg: i32,
    next_var: i32,
    arg_size: u32,
    ret_size: u32,
}

impl Locals {
    pub fn new() -> Self {
        Locals {
            map     : HashMap::new(),
            next_arg: ARG1,
            next_var: 0,
            arg_size: 0,
            ret_size: 0,
        }
    }
}

/// A stack Locals mappings for nested structures.
struct LocalsStack(RefCell<Vec<Locals>>);

impl LocalsStack {
    const NO_STACK: &'static str = "Attempted to access empty LocalsStack";
    const UNKNOWN_BINDING: &'static str = "Unknown local binding";
    /// Create new local stack frame descriptor stack.
    fn new() -> Self {
        LocalsStack(RefCell::new(Vec::new()))
    }
    /// Push stack frame descriptor.
    fn push(self: &Self, frame: Locals) {
        self.0.borrow_mut().push(frame);
    }
    /// Pop stack frame descriptor and return it.
    fn pop(self: &Self) -> Locals {
        self.0.borrow_mut().pop().expect(Self::NO_STACK)
    }
    /// Borrow the top stack frame descriptor within given function.
    fn borrow(self: &Self, func: impl FnOnce(&Locals)) {
        let inner = self.0.borrow_mut();
        let locals = inner.last().expect(Self::NO_STACK);
        func(&locals);
    }
    /// Returns the argument size in stack elements for the top stack frame descriptor.
    fn arg_size(self: &Self) -> u32 {
        self.0.borrow_mut().last().expect(Self::NO_STACK).arg_size
    }
    /// Returns the return value size in stack elements for the top stack frame descriptor.
    fn ret_size(self: &Self) -> u32 {
        self.0.borrow_mut().last().expect(Self::NO_STACK).ret_size
    }
    /// Look up local variable descriptor for the given BindingId.
    fn lookup(self: &Self, binding_id: BindingId) -> Local {
        *self.0.borrow().last().expect(Self::NO_STACK).map.get(&binding_id).expect(Self::UNKNOWN_BINDING)
    }
    /// Sets whether the given local variable is currently in scope.
    fn set_active(self: &Self, binding_id: BindingId, active: bool) {
        let mut inner = self.0.borrow_mut();
        let locals = inner.last_mut().expect(Self::NO_STACK);
        locals.map.get_mut(&binding_id).expect(Self::UNKNOWN_BINDING).in_scope = active;
    }
}

/// Bytecode emitter. Compiles bytecode from resolved program (AST).
pub struct Compiler<T> where T: VMFunc<T> {
    /// Bytecode writer used to output to.
    writer          : Writer<T>,
    /// List of bindings mapped to their TypeIds
    bindingtype_ids : Vec<TypeId>,
    /// List of registered types, effectively mapped via vector index = TypeId.
    types           : Vec<Type>,
    /// Maps from binding id to load-argument for each frame.
    locals          : LocalsStack,
    // Maps functions to their call index.
    functions       : RefCell<HashMap<FunctionId, u32>>,
    /// Bytecode locations of function call instructions that need their target address fixed (because the target wasn't written yet). //TODO: rename variable
    unresolved      : RefCell<HashMap<FunctionId, Vec<u32>>>,
}

/// Compiles a resolved program into bytecode.
pub fn compile<'ast, T>(program: ResolvedProgram<'ast, T>) -> Program<T> where T: VMFunc<T>+Debug {
    let mut compiler = Compiler::new();
    compiler.compile(program);
    compiler.into_program()
}

// Writes a comment to the bytecode when in debug mode. // TODO: conditionally based on debug/release build or a runtime setting
macro_rules! comment {
    ($self:ident, $format:literal $(, $value:expr)*) => {
        $self.writer.comment(&format!($format $(, $value)*));
    }
}

// Writes a 8, 16 or 32 bit variant of an instruction that takes one signed argument.
macro_rules! signed {
    ($self:ident, $variant8:ident, $varian16:ident, $variant32:ident, $value:expr) => {{
        use std::{i8, i16};
        if $value >= i8::MIN as i32 && $value <= i8::MAX as i32 {
            $self.writer.$variant8($value as i8);
        } else if $value >= i16::MIN as i32 && $value <= i16::MAX as i32 {
            $self.writer.$varian16($value as i16);
        } else {
            $self.writer.$variant32($value);
        }
    }}
}

// Writes a 8, 16 or 32 bit variant of an instruction that takes one unsigned argument.
macro_rules! unsigned {
    ($self:ident, $variant8:ident, $varian16:ident, $variant32:ident, $value:expr) => {{
        use std::{u8, u16};
        if $value <= u8::MAX as u32 {
            $self.writer.$variant8($value as u8);
        } else if $value <= u16::MAX as u32 {
            $self.writer.$varian16($value as u16);
        } else {
            $self.writer.$variant32($value);
        }
    }}
}

/// Basic compiler functionality.
impl<'ast, T> Compiler<T> where T: VMFunc<T> {

    /// Creates a new compiler.
    fn new() -> Self {
        Compiler {
            writer          : Writer::new(),
            types           : Vec::new(),
            bindingtype_ids : Vec::new(),
            locals          : LocalsStack::new(),
            functions       : RefCell::new(HashMap::new()),
            unresolved      : RefCell::new(HashMap::new()),
        }
    }

    /// Compiles the current program.
    fn compile(self: &mut Self, program: ResolvedProgram<'ast, T>) {

        let ResolvedProgram { ast: statements, bindingtype_ids, types, entry_fn, .. } = program;

        // write placeholder jump to program entry
        let initial_pos = self.writer.call(123);
        self.writer.exit();

        // compile program
        self.types = types;
        self.bindingtype_ids = bindingtype_ids;
        for statement in statements.0.iter() {
            self.compile_statement(statement);
        }

        // overwrite placeholder with actual entry position
        let &entry_fn_pos = self.functions.borrow().get(&entry_fn).expect("Failed to locate entry function in generated code.");
        self.writer.overwrite(initial_pos, |w| w.call(entry_fn_pos));
    }

    /// Returns compiled bytecode program.
    fn into_program(self: Self) -> Program<T> {
        self.writer.into_program()
    }
}

/// Methods for compiling individual code structures.
impl<'ast, T> Compiler<T> where T: VMFunc<T> {

    /// Compiles the given statement.
    fn compile_statement(self: &Self, item: &ast::Statement<'ast>) {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.compile_function(function),
            S::Structure(_)             => { /* nothing to do here */ },
            S::Binding(binding)         => self.compile_binding(binding),
            S::IfBlock(if_block)        => {
                self.compile_if_block(if_block);
                if let Some(result) = &if_block.if_block.result {
                    for _ in 0..self.bindingtype(result).quadsize() {
                        self.writer.discard32();
                    }
                }
            }
            S::ForLoop(for_loop)        => self.compile_for_loop(for_loop),
            S::WhileLoop(while_loop)    => self.compile_while_loop(while_loop),
            S::Block(block)             => {
                self.compile_block(block);
                if let Some(result) = &block.result {
                    for _ in 0..self.bindingtype(result).quadsize() {
                        self.writer.discard32();
                    }
                }
            }
            S::Return(ret)              => self.compile_return(ret),
            S::Expression(expression)   => {
                self.compile_expression(expression);
                for _ in 0..self.bindingtype(expression).quadsize() {
                    self.writer.discard32();
                }
            }
        }
    }

    /// Compiles the given expression.
    fn compile_expression(self: &Self, item: &ast::Expression<'ast>) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.compile_literal(literal),
            E::Variable(variable)       => self.compile_variable(variable),
            E::Member(_)                => { /* nothing to do here */ }
            E::Call(call)               => self.compile_call(call),
            E::Assignment(assignment)   => self.compile_assignment(assignment),
            E::BinaryOp(binary_op)      => self.compile_binary_op(binary_op),
            E::UnaryOp(unary_op)        => self.compile_unary_op(unary_op),
            E::Cast(cast)               => self.compile_cast(cast),
            E::Block(block)             => self.compile_block(block),
            E::IfBlock(if_block)        => self.compile_if_block(if_block),
        };
    }

    /// Compiles an assignment to a local variable.
    fn compile_variable_assignment(self: &Self, item: &ast::Assignment<'ast>) {
        use crate::frontend::ast::BinaryOperator as BO;
        let binding_id = item.left.binding_id().unwrap();
        let index = self.locals.lookup(binding_id).index;
        let ty = self.bindingtype(&item.left);
        match item.op {
            BO::Assign => {
                self.compile_expression(&item.right);
                self.write_tmp_ref(&item.right);
                if ty.is_primitive() {
                    // stack: [ ..., PRIM_SRC ], store directly to variable
                    self.write_store(index, ty);
                } else {
                    // stack: [ ..., OBJ_SRC ], load target variable for heap copy (consumes src+dest)
                    self.write_load(index, ty); // [ ..., OBJ_SRC, OBJ_DEST ]
                    self.writer.heap_copy_32(self.compute_type_size(&ty));
                }
                self.write_tmp_unref(&item.right);
            },
            _ => {
                self.write_load(index, ty);
                self.compile_expression(&item.right);
                self.write_tmp_ref(&item.right);
                // stack: [ ..., PRIM_DEST, PRIM_SRC ]
                match item.op {
                    BO::AddAssign => self.write_add(&ty),
                    BO::SubAssign => self.write_sub(&ty),
                    BO::MulAssign => self.write_mul(&ty),
                    BO::DivAssign => self.write_div(&ty),
                    BO::RemAssign => self.write_rem(&ty),
                    _ => panic!("Unsupported assignment operator encountered"),
                };
                self.write_store(index, ty);
                self.write_tmp_unref(&item.right);
            },
        };
    }

    /// Compiles an assignment to an index operation.
    fn compile_index_assignment(self: &Self, item: &ast::Assignment<'ast>) {
        use crate::frontend::ast::BinaryOperator as BO;
        let ty = self.bindingtype(&item.left);
        self.compile_expression(&item.right);
        self.write_tmp_ref(&item.right);
        self.compile_expression(&item.left);
        match item.op {
            BO::Assign => {
                if ty.is_primitive() {
                    // stack: [ ..., PRIM_SRC, OBJ_DEST ], copy primitive to heap object, consumes both
                    self.write_heap_put(ty.size());
                } else {
                    // stack: [ ..., OBJ_SRC, OBJ_DEST ], copy from object to object, consumes both
                    self.writer.heap_copy_32(self.compute_type_size(&ty));
                }
            },
            _ => {
                // stack: [ ..., X_SRC, OBJ_DEST ]
                self.writer.push_tmp96();
                // stack: [ ..., X_SRC ], tmp: [ ..., OBJ_DEST ]
                if !ty.is_primitive() {
                    self.write_heap_fetch(ty.size());
                }
                // stack: [ ..., PRIM_SRC ], tmp: [ ..., OBJ_DEST ]
                self.writer.load_tmp96(); // todo: for primitives this will result in push+load which equals a store. optimize in optimizer or here?
                // stack: [ ..., PRIM_SRC, OBJ_DEST  ], tmp: [ ..., OBJ_DEST ]
                self.write_heap_fetch(ty.size());
                // stack: [ ..., PRIM_SRC, PRIM_DEST  ], tmp: [ ..., OBJ_DEST ]
                if ty.size() == 8 { // todo: swap not required for add/mul
                    self.writer.swap64();
                } else {
                    self.writer.swap32();
                }
                // stack: [ ..., PRIM_DEST, PRIM_SRC  ], tmp: [ ..., OBJ_DEST ]
                match item.op {
                    BO::AddAssign => self.write_add(&ty),
                    BO::SubAssign => self.write_sub(&ty),
                    BO::MulAssign => self.write_mul(&ty),
                    BO::DivAssign => self.write_div(&ty),
                    BO::RemAssign => self.write_rem(&ty),
                    _ => panic!("Unsupported assignment operator encountered"),
                };
                // stack: [ ..., PRIM_RESULT  ], tmp: [ ..., OBJ_DEST ]
                self.writer.pop_tmp96();
                // stack: [ ..., PRIM_RESULT, OBJ_DEST  ]
                self.write_heap_put(ty.size());
            },
        }
        self.write_tmp_unref(&item.right);
    }

    /// Compiles the assignment operation.
    fn compile_assignment(self: &Self, item: &ast::Assignment<'ast>) {
        match item.left {
            ast::Expression::Variable(_) => self.compile_variable_assignment(item),
            ast::Expression::BinaryOp(_) => self.compile_index_assignment(item),
            _ => panic!("cannot assign to left expression"),
        }
    }

    /// Compiles the given function.
    fn compile_function(self: &Self, item: &ast::Function<'ast>) {
        // register function bytecode index, check if any bytecode needs fixing
        let position = self.writer.position();
        comment!(self, "\nFunction {}", item.sig.ident.name);
        let function_id = item.function_id.unwrap();
        self.functions.borrow_mut().insert(function_id, position);
        self.fix_targets(function_id, position);
        // create local environment
        let mut frame = Locals::new();
        frame.ret_size = item.sig.ret.as_ref().map_or(0, |ret| self.get_type(ret.type_id).quadsize()) as u32;
        for arg in item.sig.args.iter() {
            let arg_size = self.bindingtype(arg).quadsize();
            frame.next_arg -= arg_size as i32 - 1;
            frame.map.insert(arg.binding_id.unwrap(), Local::new(frame.next_arg));
            frame.next_arg -= 1;
            frame.arg_size += arg_size as u32;
        }
        self.create_stack_frame_block(&item.block, &mut frame);
        // reserve space for local variables on the stack
        if frame.next_var > 0 {
            self.writer.reserve(frame.next_var as u8);
        }
        // push local environment on the locals stack so that it is accessible from nested compile_*
        self.locals.push(frame);
        self.compile_block(&item.block);
        if !item.block.returns() {
            comment!(self, "Cleanup function {}", item.sig.ident.name);
            let return_heap_object = item.sig.ret.as_ref().map_or(false, |ret| !self.get_type(ret.type_id).is_primitive());
            self.write_heap_decref_all(return_heap_object);
            self.write_ret();
        }
        // destroy local environment
        self.locals.pop();
    }

    /// Compiles the given call.
    fn compile_call(self: &Self, item: &ast::Call<'ast>) {
        comment!(self, "Call");

        // put args on stack, ensure temporaries are cleaned up later
        for (index, arg) in item.args.iter().enumerate() {
            comment!(self, "arg {}", index);
            self.compile_expression(arg);
            self.write_tmp_ref(arg);
        }

        if let FnKind::Rust(rust_fn_index) = item.call_kind {

            // rust function
            self.writer.rustcall(T::from_u16(rust_fn_index));

        } else if let FnKind::Intrinsic(_intrinsic) = &item.call_kind {

            // intrinsics // TODO: actually check which one once there are some
            if let CallType::Method(exp) = &item.call_type {
                let ty = self.bindingtype(&**exp);
                if let Type::Array(array) = ty {
                    self.write_numeric(Numeric::Unsigned(array.len.unwrap() as u64), &Type::u32);
                }
            }

        } else {

            // normal function: identify call target or write dummy
            let function_id = item.function_id.expect(&format!("Unresolved function \"{}\" encountered", item.ident.name));
            let call_position = self.writer.position();

            let target = if let Some(&target) = self.functions.borrow().get(&function_id) {
                target
            } else {
                self.unresolved.borrow_mut().entry(function_id).or_insert(Vec::new()).push(call_position);
                123
            };

            self.writer.call(target);
        }

        for arg in item.args.iter() {
            self.write_tmp_unref(arg);
        }
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_binding(self: &Self, item: &ast::Binding<'ast>) {
        if let Some(expr) = &item.expr {
            comment!(self, "Binding {}", item.ident.name);
            self.compile_expression(expr);
            let ty = self.bindingtype(item);
            if !ty.is_primitive() {
                self.writer.heap_incref();
            }
            let binding_id = item.binding_id.expect("Unresolved binding encountered");
            let index = self.locals.lookup(binding_id).index;
            self.write_store(index, ty);
            self.locals.set_active(binding_id, true);
        }
    }

    /// Compiles the given variable.
    fn compile_variable(self: &Self, item: &ast::Variable<'ast>) {
        let load_index = {
            let binding_id = item.binding_id.expect("Unresolved binding encountered");
            self.locals.lookup(binding_id).index
        };
        self.write_load(load_index, self.bindingtype(item));
    }

    /// Compiles an if block without else part.
    fn compile_if_only_block(self: &Self, item: &ast::IfBlock<'ast>) {

        let exit_jump = self.writer.j0(123);
        self.compile_block(&item.if_block);
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
    }

    /// Compiles an if+else block.
    fn compile_if_else_block(self: &Self, if_block: &ast::Block<'ast>, else_block: &ast::Block<'ast>) {

        let else_jump = self.writer.j0(123);
        self.compile_block(if_block);
        let exit_jump = if !if_block.returns() {
            Some(self.writer.jmp(123))
        } else {
            None
        };

        let else_target = self.writer.position();
        self.compile_block(else_block);

        let exit_target = self.writer.position();

        // go back and fix jump targets
        self.writer.overwrite(else_jump, |w| w.j0(else_target));
        if let Some(exit_jump) = exit_jump {
            self.writer.overwrite(exit_jump, |w| w.jmp(exit_target));
        }
    }

    /// Compiles the given if block.
    fn compile_if_block(self: &Self, item: &ast::IfBlock<'ast>) {

        // compile condition and jump placeholder
        self.compile_expression(&item.cond);

        if item.else_block.is_none() {
            self.compile_if_only_block(item);
        } else {
            self.compile_if_else_block(&item.if_block, item.else_block.as_ref().unwrap());
        }
    }

    /// Compiles a while loop.
    fn compile_while_loop(self: &Self, item: &ast::WhileLoop<'ast>) {
        let start_target = self.writer.position();
        self.compile_expression(&item.expr);
        let exit_jump = self.writer.j0(123);
        self.compile_block(&item.block);
        self.writer.jmp(start_target);
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
    }

    /// Compiles a for - in loop
    fn compile_for_loop(self: &Self, item: &ast::ForLoop<'ast>) {
        if let Some(binary_op) = item.range.as_binary_op() {

            if binary_op.op == ast::BinaryOperator::Range || binary_op.op == ast::BinaryOperator::RangeInclusive {
                comment!(self, "For loop");
                // todo: refactor this mess
                let (var_index, var_type) = self.range_info(item);
                // store lower range bound in iter variable
                self.compile_expression(&binary_op.left);
                self.write_store(var_index, var_type);
                // push upper range bound
                self.compile_expression(&binary_op.right);
                // precheck (could be avoided by moving condition to the end but not trivial due to stack top clone order) // TODO: tmp stack now?
                self.write_load(var_index, var_type);
                self.write_clone(var_type, if var_type.size() == 8 { 2 } else { 1 }); // clone upper bound for comparison, skip over iter inbetween
                if binary_op.op == ast::BinaryOperator::Range {
                    self.write_lt(var_type);
                } else {
                    self.write_lte(var_type);
                }
                let exit_jump = self.writer.j0(123);
                // compile block
                comment!(self, "For loop block");
                let start_target = self.writer.position();
                self.compile_block(&item.block);
                // load bounds, increment and compare
                comment!(self, "For loop compare");
                self.write_preinc(var_index, var_type);
                self.write_clone(var_type, if var_type.size() == 8 { 2 } else { 1 }); // clone upper bound for comparison, skip over iter inbetween
                if binary_op.op == ast::BinaryOperator::Range {
                    self.write_lt(var_type);
                } else {
                    self.write_lte(var_type);
                }
                self.writer.jn0(start_target);
                // exit position
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
            } else {
                unimplemented!("non-range binary op");
            }

        } else {
            panic!("expected binary op");
        }
    }

    /// Compiles a return statement
    fn compile_return(self: &Self, item: &ast::Return<'ast>) {
        if let Some(expr) = &item.expr {
            self.compile_expression(expr);
            let ty = self.bindingtype(expr);
            self.write_heap_decref_all(!ty.is_primitive());
        } else {
            self.write_heap_decref_all(false);
        }
        self.write_ret();
    }

    /// Compiles the given block.
    fn compile_block(self: &Self, item: &ast::Block<'ast>) {

        for statement in item.statements.iter() {
            self.compile_statement(statement);
        }

        if let Some(result) = &item.result {
            self.compile_expression(result);
        }
    }

    /// Compiles the given literal
    fn compile_literal(self: &Self, item: &ast::Literal<'ast>) {
        use crate::frontend::ast::LiteralValue;
        let lit_type = self.bindingtype(item);
        match item.value {
            LiteralValue::Numeric(numeric) => self.write_numeric(numeric, lit_type),
            LiteralValue::Bool(v) =>  {
                match lit_type {
                    Type::bool => { if v { self.writer.lit1(); } else { self.writer.lit0(); } },
                    _ => panic!("Unexpected boolean literal type: {:?}", lit_type)
                };
            },
            LiteralValue::String(_) => { // todo refactor string, array, struct into one block
                match lit_type {
                    Type::String => {
                        self.write_heap_ref(self.store_literal(item));
                        // fixme: this needs to clone into a new heap ref or I need to implement copy-on-write
                    },
                    _ => panic!("Unexpected string literal type: {:?}", lit_type)
                };
            },
            LiteralValue::Array(_) => {
                match lit_type {
                    Type::Array(_) => {
                        self.write_heap_ref(self.store_literal(item));
                        // fixme: this needs to clone into a new heap ref or I need to implement copy-on-write
                    },
                    _ => panic!("Unexpected array literal type: {:?}", lit_type)
                };
            },
            LiteralValue::Struct(_) => {
                match lit_type {
                    Type::Struct(_) => {
                        self.write_heap_ref(self.store_literal(item));
                        // fixme: this needs to clone into a new heap ref or I need to implement copy-on-write
                    },
                    _ => panic!("Unexpected struct literal type: {:?}", lit_type)
                };
            }
        }
    }

    /// Compiles the given unary operation.
    fn compile_unary_op(self: &Self, item: &ast::UnaryOp<'ast>) {
        use crate::frontend::ast::UnaryOperator as UO;

        let exp_type = self.bindingtype(&item.expr);

        match item.op {
            // logical
            UO::Not => {
                self.compile_expression(&item.expr);
                self.writer.not();
            }
            // arithmetic
            UO::IncBefore | UO::DecBefore | UO::IncAfter | UO::DecAfter => {
                if let ast::Expression::Variable(var) = &item.expr {
                    let load_index = {
                        let binding_id = var.binding_id.expect("Unresolved binding encountered");
                        self.locals.lookup(binding_id).index
                    };
                    match item.op {
                        UO::IncBefore => self.write_preinc(load_index, &exp_type),
                        UO::DecBefore => self.write_predec(load_index, &exp_type),
                        UO::IncAfter => self.write_postinc(load_index, &exp_type),
                        UO::DecAfter => self.write_postdec(load_index, &exp_type),
                        _ => panic!("Internal error in operator handling"),
                    };
                } else {
                    panic!("Operator {:?} can only be used on variable bindings", item.op); // FIXME not true anymore
                }
            },
        }
    }

    /// Compiles the given binary operation.
    fn compile_binary_op(self: &Self, item: &ast::BinaryOp<'ast>) {
        use crate::frontend::ast::BinaryOperator as BO;

        if item.op != BO::And && item.op != BO::Or { // these short-circuit and need special handling // todo: can this be refactored?
            self.compile_expression(&item.left);
            self.write_tmp_ref(&item.left);
            self.compile_expression(&item.right);
            self.write_tmp_ref(&item.right);
        }

        let result_type = self.bindingtype(item);
        let compare_type = self.bindingtype(&item.left);

        match item.op {
            // arithmetic
            BO::Add => self.write_add(&result_type),
            BO::Sub => self.write_sub(&result_type),
            BO::Mul => self.write_mul(&result_type),
            BO::Div => self.write_div(&result_type),
            BO::Rem => self.write_rem(&result_type),
            // comparison
            BO::Greater     => { self.write_swap(compare_type, compare_type); self.write_lt(&compare_type); },
            BO::GreaterOrEq => { self.write_swap(compare_type, compare_type); self.write_lte(&compare_type); },
            BO::Less        => self.write_lt(&compare_type),
            BO::LessOrEq    => self.write_lte(&compare_type),
            BO::Equal       => self.write_eq(&compare_type),
            BO::NotEqual    => self.write_neq(&compare_type),
            // boolean
            BO::And => {
                self.compile_expression(&item.left);
                let exit_jump = self.writer.j0_top(123); // left is false, result cannot ever be true, skip right
                self.compile_expression(&item.right);
                self.writer.and();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.j0_top(exit_target));
            },
            BO::Or => {
                self.compile_expression(&item.left);
                let exit_jump = self.writer.jn0_top(123); // left is true, result cannot ever be false, skip right
                self.compile_expression(&item.right);
                self.writer.or();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.jn0_top(exit_target));
            },
            // special
            BO::Range => unimplemented!("range"),
            BO::Index => {
                comment!(self, "Index");
                // fetch heap object value if the result of the index read is directly used (primitives and for string, their reference)
                if result_type.is_primitive() || result_type.is_string() {
                    self.write_heap_fetch_element(result_type.size());
                } else {
                    self.write_element_offset(self.compute_type_size(&result_type));
                }
            },
            BO::IndexWrite => {
                comment!(self, "IndexWrite");
                // stack: <heap_index> <heap_offset> <index_operand>
                let size = if result_type.is_string() { result_type.size() as u32 } else { self.compute_type_size(&result_type) }; // todo: need size_stack() or similar
                self.write_element_offset(size); // pop <index_operand> and <heap_offset> and push <heap_offset+index_operand*element_size>
                // stack now <heap_index> <new_heap_offset>
            },
            BO::Access => {
                comment!(self, "Access");
                // fetch heap object value if the result of the member read is directly used (primitives and for string, their reference)
                if result_type.is_primitive() || result_type.is_string() {
                    let offset = self.compute_member_offset(&compare_type, item.right.as_member().unwrap().index.unwrap());
                    self.write_heap_fetch_member(result_type.size(), offset);
                } else {
                    let offset = self.compute_member_offset(&compare_type, item.right.as_member().unwrap().index.unwrap());
                    self.write_member_offset(offset); // push additional offset, pop both offsets, write computed offset
                }
            },
            BO::AccessWrite => {
                comment!(self, "AccessWrite");
                // stack: <heap_index> <heap_offset>
                let offset = self.compute_member_offset(&compare_type, item.right.as_member().unwrap().index.unwrap());
                self.write_member_offset(offset); // push additional offset, pop both offsets, write computed offset
                // stack now <heap_index> <new_heap_offset>
            },
            // others are handled elsewhere
            _ => panic!("Encountered invalid operation {:?} in compile_binary_op", item.op)
        };

        if item.op != BO::And && item.op != BO::Or {
            self.write_tmp_unref(&item.right);
            self.write_tmp_unref(&item.left);
        }
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_cast(self: &Self, item: &ast::Cast<'ast>) {

        self.compile_expression(&item.expr);

        let mut from = self.bindingtype(&item.expr);
        let mut to = self.get_type(item.ty.type_id);
        let orig_to = to;

        // float to integer: convert fxx to i64, then convert that to the correct integer type
        if from.is_float() && !to.is_float() {
            if from.size() == 4 {
                self.writer.f32toi64();
                from = &Type::i64;
            } else if from.size() == 8 {
                self.writer.f64toi64();
                from = &Type::i64;
            }
        }

        // integer to float: cast input to i64 first, then convert that to the correct float type
        if !from.is_float() && to.is_float() {
            to = &Type::i64;
        }

        // convert float to float, integer to integer
        if from.is_float() && to.is_float() {
            if from.size() == 8 && to.size() == 4 {
                self.writer.f64tof32();
            } else if from.size() == 4 && to.size() == 8 {
                self.writer.f32tof64();
            }
        } else if from.is_unsigned() {
            if from.size() > to.size() {
                if from.size() == 8 {
                    self.writer.trunc64(to.size() * 8);
                } else if from.size() <= 4 {
                    self.writer.trunc(to.size() * 8);
                } else {
                    panic!("invalid type size")
                }
            }
        } else if from.is_signed() {
            if from.size() > to.size() {
                if from.size() == 8 {
                    self.writer.trunc64(to.size() * 8);
                } else if from.size() <= 4 {
                    self.writer.trunc(to.size() * 8);
                } else {
                    panic!("invalid type size")
                }
            } else if from.size() < to.size() {
                if to.size() == 8 {
                    self.writer.extends64((to.size() - from.size()) * 8);
                } else if to.size() <= 4 {
                    self.writer.extends((to.size() - from.size()) * 8);
                } else {
                    panic!("invalid type size")
                }
            }
        }

        // integer to float: was cast to i64, now convert that to the correct float type
        if !from.is_float() && orig_to.is_float() {
            if orig_to.size() == 4 {
                self.writer.i64tof32();
            } else if orig_to.size() == 8 {
                self.writer.i64tof64();
            }
        }
    }
}

impl<'ast, T> Compiler<T> where T: VMFunc<T> {

    /// Returns the type of the given binding.
    fn bindingtype<B>(self: &Self, item: &B) -> &Type where B: Bindable {
        let binding_id = Into::<usize>::into(item.binding_id().expect("Unresolved binding encountered."));
        let type_id = self.bindingtype_ids[binding_id];
        &self.types[Into::<usize>::into(type_id)]
    }

    /// Returns type for given type_id.
    fn get_type(self: &Self, type_id: Option<TypeId>) -> &Type {
        &self.types[Into::<usize>::into(type_id.expect("Unresolved type encountered."))]
    }

    /// Fixes function call targets for previously not generated functions.
    fn fix_targets(self: &Self, function_id: FunctionId, position: u32) {
        if let Some(targets) = self.unresolved.borrow_mut().remove(&function_id) {
            let backup_position = self.writer.position();
            for &target in targets.iter() {
                self.writer.set_position(target);
                self.writer.call(position);
            }
            self.writer.set_position(backup_position);
        }
    }

    /// Retrieve for-in loop range variable index/type
    fn range_info(self: &Self, item: &ast::ForLoop<'ast>) -> (i32, &Type) {
        let var_index = {
            let binding_id = item.iter.binding_id.expect("Unresolved binding encountered");
            self.locals.lookup(binding_id).index
        };
        let var_type = self.bindingtype(&item.iter);
        (var_index, var_type)
    }

    /// Creates stack frame variables for expressions.
    fn create_stack_frame_exp(self: &Self, expression: &ast::Expression<'ast>, frame: &mut Locals) {
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

    /// Creates stack frame variables for blocks.
    fn create_stack_frame_block(self: &Self, item: &ast::Block<'ast>, frame: &mut Locals) {
        // todo: this is pretty bad. need to come up with better solution. trait on ast?
        for statement in item.statements.iter() {
            if let ast::Statement::Binding(binding) = statement {
                let next_var = frame.next_var;
                frame.map.insert(binding.binding_id.unwrap(), Local::new(next_var));
                frame.next_var += self.bindingtype(binding).quadsize() as i32;
                if let Some(expression) = &binding.expr {
                    self.create_stack_frame_exp(expression, frame);
                }
            } else if let ast::Statement::ForLoop(for_loop) = statement {
                let next_var = frame.next_var;
                frame.map.insert(for_loop.iter.binding_id.unwrap(), Local::new(next_var));
                frame.next_var += self.bindingtype(&for_loop.iter).quadsize() as i32;
                self.create_stack_frame_block(&for_loop.block, frame);
            } else if let ast::Statement::WhileLoop(while_loop) = statement {
                self.create_stack_frame_block(&while_loop.block, frame);
            } else if let ast::Statement::Block(block) = statement {
                self.create_stack_frame_block(block, frame);
            } else if let ast::Statement::IfBlock(if_block) = statement {
                self.create_stack_frame_block(&if_block.if_block, frame);
                if let Some(block) = &if_block.else_block {
                    self.create_stack_frame_block(block, frame);
                }
            } else if let ast::Statement::Expression(expression) = statement {
                self.create_stack_frame_exp(expression, frame);
            }
        }
        if let Some(result) = &item.result {
            self.create_stack_frame_exp(result, frame);
        }
    }

    /// Writes a heap reference (which will always be a heap 0 = constpool ref)
    fn write_heap_ref(self: &Self, item: HeapRef) {
        assert!(item.index == 0);
        self.writer.lit0();
        self.write_numeric(Numeric::Unsigned(item.len as u64), &Type::u32);
        self.write_numeric(Numeric::Unsigned(item.offset as u64), &Type::u32);
    }

    /// Writes given numeric, using const pool if necessary.
    fn write_numeric(self: &Self, numeric: Numeric, ty: &Type) {
        use std::u8;
        match numeric { // todo: try to refactor this mess
            Numeric::Unsigned(0) if ty.is_integer() && ty.size() <= 4 => { self.writer.lit0(); }
            Numeric::Unsigned(1) if ty.is_integer() && ty.size() <= 4 => { self.writer.lit1(); }
            Numeric::Unsigned(2) if ty.is_integer() && ty.size() <= 4 => { self.writer.lit2(); }
            Numeric::Signed(-1) if ty.is_signed() && ty.size() <= 4 => { self.writer.litm1(); }
            Numeric::Unsigned(val) if ty.is_unsigned() && ty.size() <= 4 && val <= u8::MAX as u64 => { self.writer.litu(numeric.as_unsigned().unwrap() as u8); }
            _ => {
                match ty {
                    Type::i8 => { self.writer.lits(numeric.as_signed().unwrap() as i8); },
                    Type::u8 => { self.writer.litu(numeric.as_unsigned().unwrap() as u8); },
                    _ if ty.is_integer() || ty.is_float() => {
                         let item = self.store_numeric(numeric, ty);
                         self.write_const_fetch(item.offset, ty);
                    },
                    _ => panic!("Unexpected numeric literal type: {:?}", ty)
                }
            }
        }
    }

    /// Stores given numeric on the const pool.
    fn store_numeric(self: &Self, numeric: Numeric, ty: &Type) -> HeapRef {
        let offset = match numeric {
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
                    Type::i8 => self.writer.store_const(v as i8),
                    Type::i16 => self.writer.store_const(v as i16),
                    Type::i32 => self.writer.store_const(v as i32),
                    Type::i64 => self.writer.store_const(v as i64),
                    Type::u8 => self.writer.store_const(v as u8),
                    Type::u16 => self.writer.store_const(v as u16),
                    Type::u32 => self.writer.store_const(v as u32),
                    Type::u64 => self.writer.store_const(v as u64),
                    _ => panic!("Unexpected unsigned integer literal type: {:?}", ty)
                }
            },
            Numeric::Float(v) => {
                match ty {
                    Type::f32 => self.writer.store_const(v as f32),
                    Type::f64 => self.writer.store_const(v),
                    _ => panic!("Unexpected float literal type: {:?}", ty)
                }
            },
            Numeric::Overflow => panic!("Literal computation overflow")
        };
        HeapRef { index: CONSTPOOL_INDEX, offset: offset, len: ty.size() as u32 }
    }

    /// Stores given literal on the const pool.
    fn store_literal(self: &Self, item: &ast::Literal<'ast>) -> HeapRef {
        use crate::frontend::ast::LiteralValue;
        let ty = self.bindingtype(item);
        match item.value {
            LiteralValue::Numeric(int) => self.store_numeric(int, ty),
            LiteralValue::Bool(v) =>  {
                match ty {
                    Type::bool => HeapRef::from_const(self.writer.store_const(if v { 1u8 } else { 0u8 }), Type::bool.size() as u32) ,
                    _ => panic!("Unexpected boolean literal type: {:?}", ty)
                }
            },
            LiteralValue::String(_) => {
                self.store_unsized_literal_data(item);
                self.store_literal_data(item)
            },
            LiteralValue::Array(_) => {
                self.store_unsized_literal_data(item);
                self.store_literal_data(item)
            },
            LiteralValue::Struct(_) => {
                self.store_unsized_literal_data(item);
                self.store_literal_data(item)
            },
        }
    }

    /// Recursively stores all unsized data and stores a reference to the data on the AST object.
    fn store_unsized_literal_data(self: &Self, item: &ast::Literal<'ast>) {
        use crate::frontend::ast::LiteralValue;
        let ty = self.bindingtype(item);
        match &item.value { // todo: probably better to match based on ty
            LiteralValue::Array(array) => {
                let inner_ty = self.get_type(ty.as_array().unwrap().type_id);
                // primitives won't contain nested unsized data, we can skip them (performance, otherwise doesn't matter)
                if !inner_ty.is_primitive() {
                    for item in &array.elements {
                        self.store_unsized_literal_data(item);
                    }
                }
                // unsized arrays need to be stored here to get their reference
                if !ty.as_array().unwrap().is_sized() {
                    // FIXME: this isn't symmetric with string (store_literal_data vs store_literal)
                    let heap_ref = self.store_literal_data(item);
                    item.heap_ref.set(Some(heap_ref));
                }
            }
            LiteralValue::Struct(struct_) => {
                for (name, type_id) in ty.as_struct().unwrap().fields.iter() {
                    let inner_ty = self.get_type(*type_id);
                    // primitives won't contain nested unsized data, we can skip them (performance, otherwise doesn't matter)
                    if !inner_ty.is_primitive() {
                        let item = &struct_.fields[&name[..]];
                        self.store_unsized_literal_data(item);
                    }
                }
            }
            LiteralValue::String(string) => {
                let heap_ref = HeapRef::from_const(self.writer.store_const(*string), string.len() as u32);
                item.heap_ref.set(Some(heap_ref));
            }
            _ => { }
        }
    }

    /// Stores array data in constpool. Note: when read with const_fetch_object() array size needs to be written first!
    fn store_literal_data(self: &Self, item: &ast::Literal<'ast>) -> HeapRef {
        use crate::frontend::ast::LiteralValue;
        let ty = self.bindingtype(item);
        match &item.value {
            LiteralValue::Array(array) => {
                let num_elements = array.elements.len() as u32;
                if num_elements > 0 {
                    let inner_ty = self.get_type(ty.as_array().unwrap().type_id);
                    let pos = self.writer.const_len();
                    for item in &array.elements {
                        if let Some(heap_ref) = item.heap_ref.get() {
                            self.writer.store_const(heap_ref);
                        } else {
                            self.store_literal_data(item);
                        }
                    }
                    HeapRef::from_const(pos, (inner_ty.size() as usize * array.elements.len()) as u32)
                } else {
                    let pos = self.writer.const_len();
                    HeapRef::from_const(pos, 0)
                }
            }
            LiteralValue::Struct(struct_) => {
                let num_fields = ty.as_struct().unwrap().fields.len() as u32;
                if num_fields > 0 {
                    let pos = self.writer.const_len();
                    let mut size = 0;
                    for (name, _) in ty.as_struct().unwrap().fields.iter() {
                        let item = &struct_.fields[&name[..]];
                        if let Some(heap_ref) = item.heap_ref.get() {
                            self.writer.store_const(heap_ref);
                            size += HeapRef::size() as u32;
                        } else {
                            let heap_ref = self.store_literal_data(item);
                            size += heap_ref.len;
                        }
                    }
                    HeapRef::from_const(pos, size)
                } else {
                    let pos = self.writer.const_len();
                    HeapRef::from_const(pos, 0)
                }
            }
            LiteralValue::String(_) => {
                item.heap_ref.get().unwrap()
            }
            _ => {
                self.store_literal(item)
            }
        }
    }

    /// Computes the shallow (unless array with inlined data) size of a type in bytes.
    fn compute_type_size(self: &Self, ty: &Type) -> u32 {
        match ty {
            Type::Array(array) => {
                let len = array.len.unwrap();
                if len == 0 {
                    0
                } else {
                    let inner_type = self.get_type(array.type_id);
                    // sized types are inlined, unsized referenced (use reference size)
                    len as u32 * if !inner_type.is_sized() { inner_type.size() as u32 } else { self.compute_type_size(inner_type) }
                }
            }
            Type::Struct(struct_) => {
                struct_.fields.iter().map(|&(_, type_id)| {
                    let field_type = self.get_type(type_id);
                    // sized types are inlined, unsized referenced (use reference size)
                    if !field_type.is_sized() { field_type.size() as u32 } else { self.compute_type_size(field_type) }
                }).sum()
            }
            Type::Enum(_) => unimplemented!("enum"),
            Type::String => panic!("invalid type string"),
            _ => ty.size() as u32
        }
    }

    /// Computes struct member offset in bytes.
    fn compute_member_offset(self: &Self, ty: &Type, member_index: u32) -> u32 {
        let struct_ = ty.as_struct().unwrap();
        let mut offset = 0;
        for index in 0 .. member_index {
            let field_type = self.get_type(struct_.fields[index as usize].1);
            // sized types are inlined, unsized referenced (use reference size)
            offset += if !field_type.is_sized() { field_type.size() as u32 } else { self.compute_type_size(field_type) };
        }
        offset
    }

    /// Writes instructions to compute member offset for access on a struct.
    fn write_member_offset(self: &Self, offset: u32) {
        use std::u8;
        if offset == 0 {
            // nothing to do
        } else if offset == 1 {
            self.writer.lit1();
            self.writer.addi();
        } else if offset == 2 {
            self.writer.lit2();
            self.writer.addi();
        } else if offset <= u8::MAX as u32 {
            self.writer.litu(offset as u8);
            self.writer.addi();
        } else {
            let const_id = self.writer.store_const(offset);
            self.write_const_fetch(const_id, &Type::u32);
            self.writer.addi();
        }
    }

    /// Writes instructions to compute element offset for indexing of an array.
    fn write_element_offset(self: &Self, element_size: u32) {
        unsigned!(self, index_8, index_16, index_32, element_size);
    }

    /// Writes instructions to fetch a primitive or heap reference (without dereferencing it) constant.
    fn write_const_fetch(self: &Self, index: u32, ty: &Type) {
        assert!(ty.is_primitive());
        match ty.size() {
            2 => unsigned!(self, const_fetch16, const_fetch16_16, const_fetch16_32, index),
            4 => unsigned!(self, const_fetch32, const_fetch32_16, const_fetch32_32, index),
            8 => unsigned!(self, const_fetch64, const_fetch64_16, const_fetch64_32, index),
            12 => unsigned!(self, const_fetch96, const_fetch96_16, const_fetch96_32, index),
            _ => panic!("Unsupported type size"),
        }
    }

    /// Writes instructions to fetch a value of the given size from the target of the top heap reference on the stack.
    fn write_heap_fetch(self: &Self, result_size: u8) {
        match result_size {
            1 => { self.writer.heap_fetch8(); },
            2 => { self.writer.heap_fetch16(); },
            4 => { self.writer.heap_fetch32(); },
            8 => { self.writer.heap_fetch64(); },
            12 => { self.writer.heap_fetch96(); },
            _ => panic!("Invalid result size {} for heap_fetch", result_size)
        }
    }

    /// Writes instructions to put a value of the given size at the target of the top heap reference on the stack.
    fn write_heap_put(self: &Self, result_size: u8) {
        match result_size {
            1 => { self.writer.heap_put8(); },
            2 => { self.writer.heap_put16(); },
            4 => { self.writer.heap_put32(); },
            8 => { self.writer.heap_put64(); },
            12 => { self.writer.heap_put96(); },
            _ => panic!("Invalid result size {} for heap_put", result_size)
        }
    }

    /// Writes instructions to fetch a member of the struct whose reference is at the top of the stack.
    fn write_heap_fetch_member(self: &Self, result_size: u8, offset: u32) {
        match result_size {
            1 => { self.writer.heap_fetch_member8(offset); },
            2 => { self.writer.heap_fetch_member16(offset); },
            4 => { self.writer.heap_fetch_member32(offset); },
            8 => { self.writer.heap_fetch_member64(offset); },
            12 => { self.writer.heap_fetch_member96(offset); },
            _ => panic!("Invalid result size {} for heap_fetch_member", result_size)
        }
    }

    /// Writes instructions to fetch an element of the array whose reference is at the top of the stack.
    fn write_heap_fetch_element(self: &Self, element_size: u8) {
        match element_size {
            1 => { self.writer.heap_fetch_element8(); },
            2 => { self.writer.heap_fetch_element16(); },
            4 => { self.writer.heap_fetch_element32(); },
            8 => { self.writer.heap_fetch_element64(); },
            12 => { self.writer.heap_fetch_element96(); },
            _ => panic!("Invalid element size {} for heap_fetch_element", element_size)
        }
    }

    /// If item is a temporary heap object, writes operations to store topmost heap object reference on the tmp-stack.
    fn write_tmp_ref(self: &Self, item: &ast::Expression<'ast>) {
        if !item.is_variable() {
            let ty = self.bindingtype(item);
            if !ty.is_primitive() {
                self.writer.store_tmp96();
                self.writer.heap_incref();
            }
        }
    }

    /// If item is a temporary heap object, writes operations to unref the tmp-stack's topmost heap object.
    fn write_tmp_unref(self: &Self, item: &ast::Expression<'ast>) {
        if !item.is_variable() {
            let ty = self.bindingtype(item);
            if !ty.is_primitive() {
                self.writer.pop_tmp96();
                self.writer.heap_decref();
            }
        }
    }

    /// Writes operations to unref local heap objects. Set check_prior to avoid unref'ing function results.
    fn write_heap_decref_all(self: &Self, check_prior: bool) {
        self.locals.borrow(|locals| {
            for (&binding_id, local) in locals.map.iter().filter(|(_, local)| local.in_scope) {
                let type_id = self.bindingtype_ids[binding_id.into_usize()];
                let ty = &self.types[type_id.into_usize()];
                if !ty.is_primitive() {
                    self.write_load(local.index, ty);
                    if check_prior {
                        self.writer.heap_decref_result();
                    } else {
                        self.writer.heap_decref();
                    }
                }
            }
        });
    }

    /// Writes an appropriate variant of the store instruction.
    fn write_store(self: &Self, index: i32, ty: &Type) {
        match ty.size() {
            12 => signed!(self, storer96_s8, storer96_s16, storer96_s32, index),
            8 => signed!(self, storer64_s8, storer64_s16, storer64_s32, index),
            4 | 2 | 1 => signed!(self, storer32_s8, storer32_s16, storer32_s32, index),
            _ => panic!("Unsupported type {:?} for store operation", ty)
        }
    }

    /// Writes an appropriate variant of the load instruction.
    fn write_load(self: &Self, index: i32, ty: &Type) {
        match ty.size() {
            12 => signed!(self, loadr96_s8, loadr96_s16, loadr96_s32, index),
            8 => signed!(self, loadr64_s8, loadr64_s16, loadr64_s32, index),
            4 | 2 | 1 =>  {
                match index {
                    ARG1 => { self.writer.load_arg1(); }
                    ARG2 => { self.writer.load_arg2(); }
                    ARG3 => { self.writer.load_arg3(); }
                    _ => signed!(self, loadr32_s8, loadr32_s16, loadr32_s32, index),
                };
            }
            _ => panic!("Unsupported type {:?} for load operation", ty)
        }
    }

    /// Swap 2 stack values, ty_a being topmost, ty_b next below.
    fn write_swap(self: &Self, ty_a: &Type, ty_b: &Type) {
        let size_a = ty_a.size();
        let size_b = ty_b.size();
        if size_a == 12 && size_b == 12 {
            self.writer.swap96();
        } else if size_a == 8 && size_b == 8 {
            self.writer.swap64();
        } else if size_a < 8 && size_b < 8 {
            self.writer.swap32();
        } else if size_a == 8 && size_b < 8 {
            self.writer.swap64_32();
        } else if size_a < 8 && size_b == 8 {
            self.writer.swap32_64();
        } else {
            panic!("Unsupported swap sizes");
        }
    }

    fn write_clone(self: &Self, ty: &Type, offset: u8) {
        match ty.size() {
            1 | 2 | 4 => self.writer.clone32(offset),
            8 => self.writer.clone64(offset),
            _ => panic!("unsupported type size"),
        };
    }

    fn write_preinc(self: &Self, index: i32, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.preinci64(index),
            ref ty @ _ if ty.is_integer() && ty.size() <= 4 => self.writer.preinci(index),
            ty @ _ => panic!("Unsupported Inc operand {:?}", ty),
        };
    }

    fn write_predec(self: &Self, index: i32, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.predeci64(index),
            ref ty @ _ if ty.is_integer() && ty.size() <= 4 => self.writer.predeci(index),
            ty @ _ => panic!("Unsupported Dec operand {:?}", ty),
        };
    }

    fn write_postinc(self: &Self, index: i32, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.postinci64(index),
            ref ty @ _ if ty.is_integer() && ty.size() <= 4 => self.writer.postinci(index),
            ty @ _ => panic!("Unsupported Inc operand {:?}", ty),
        };
    }

    fn write_postdec(self: &Self, index: i32, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.postdeci64(index),
            ref ty @ _ if ty.is_integer() && ty.size() <= 4 => self.writer.postdeci(index),
            ty @ _ => panic!("Unsupported Dec operand {:?}", ty),
        };
    }

    fn write_sub(self: &Self, ty: &Type) {
        match ty {
            Type::f64 => self.writer.subf64(),
            Type::i64 | Type::u64 => self.writer.subi64(),
            Type::f32 => self.writer.subf(),
            ref ty @ _ if ty.is_integer() && ty.size() <= 4 => self.writer.subi(),
            ty @ _ => panic!("Unsupported Sub operand {:?}", ty),
        };
    }

    fn write_add(self: &Self, ty: &Type) {
        match ty {
            Type::f64 => self.writer.addf64(),
            Type::i64 | Type::u64 => self.writer.addi64(),
            Type::f32 => self.writer.addf(),
            Type::String => self.writer.string_concat(),
            ref ty @ _ if ty.is_integer() && ty.size() <= 4 => self.writer.addi(),
            ty @ _ => panic!("Unsupported Add operand {:?}", ty),
        };
    }

    fn write_mul(self: &Self, ty: &Type) {
        match ty {
            Type::f64 => self.writer.mulf64(),
            Type::i64 | Type::u64 => self.writer.muli64(),
            Type::f32 => self.writer.mulf(),
            ref ty @ _ if ty.is_integer() && ty.size() <= 4 => self.writer.muli(),
            ty @ _ => panic!("Unsupported Mul operand {:?}", ty),
        };
    }

    fn write_div(self: &Self, ty: &Type) {
        match ty {
            Type::f64 => self.writer.divf64(),
            Type::i64 | Type::u64 => self.writer.divi64(),
            Type::f32 => self.writer.divf(),
            ref ty @ _ if ty.is_integer() && ty.size() <= 4 => self.writer.divi(),
            ty @ _ => panic!("Unsupported Div operand {:?}", ty),
        };
    }

    fn write_rem(self: &Self, ty: &Type) {
        match ty {
            Type::f64 => self.writer.remf64(),
            Type::i64 | Type::u64 => self.writer.remi64(),
            Type::f32 => self.writer.remf(),
            ref ty @ _ if ty.is_integer() && ty.size() <= 4 => self.writer.remi(),
            ty @ _ => panic!("Unsupported Rem operand {:?}", ty),
        };
    }

    fn write_eq(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty.size() {
                1 | 2 | 4 => self.writer.ceqr32(),
                8 => self.writer.ceqr64(),
                _ => panic!("Unsupported type size"),
            };
        } else if ty.is_string() {
            self.writer.string_ceq(0);
        } else {
            self.writer.heap_ceq(self.compute_type_size(ty));
        }
    }

    fn write_neq(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty.size() {
                1 | 2 | 4 => self.writer.cneqr32(),
                8 => self.writer.cneqr64(),
                _ => panic!("Unsupported type size"),
            };
        } else if ty.is_string() {
            self.writer.string_cneq(0);
        } else {
            self.writer.heap_cneq(self.compute_type_size(ty));
        }
    }

    fn write_lt(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty.size() {
                1 | 2 | 4 => match ty.kind() {
                    TypeKind::Signed => self.writer.clts32(),
                    TypeKind::Unsigned => self.writer.cltu32(),
                    TypeKind::Float => self.writer.cltf32(),
                    _ => panic!("Unsupported type kind"),
                },
                8 => match ty.kind() {
                    TypeKind::Signed => self.writer.clts64(),
                    TypeKind::Unsigned => self.writer.cltu64(),
                    TypeKind::Float => self.writer.cltf64(),
                    _ => panic!("Unsupported type kind"),
                },
                _ => panic!("unsupported type size"),
            };
        } else if ty.is_string() {
            self.writer.string_clt(0);
        } else {
            panic!("unsupported type")
        }
    }

    fn write_lte(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty.size() {
                1 | 2 | 4 => match ty.kind() {
                    TypeKind::Signed => self.writer.cltes32(),
                    TypeKind::Unsigned => self.writer.clteu32(),
                    TypeKind::Float => self.writer.cltef32(),
                    _ => panic!("Unsupported type kind"),
                },
                8 => match ty.kind() {
                    TypeKind::Signed => self.writer.cltes64(),
                    TypeKind::Unsigned => self.writer.clteu64(),
                    TypeKind::Float => self.writer.cltef64(),
                    _ => panic!("Unsupported type kind"),
                },
                _ => panic!("unsupported type size"),
            };
        } else if ty.is_string() {
            self.writer.string_clte(0);
        } else {
            panic!("unsupported type")
        }
    }

    /// Writes a return instruction with the correct arguments for the current stack frame.
    fn write_ret(self: &Self) {
        self.writer.ret(self.locals.ret_size() as u8, self.locals.arg_size() as u8);
    }
}