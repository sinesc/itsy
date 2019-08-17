//! Bytecode emitter. Compiles bytecode from AST.

use std::{collections::HashMap, fmt::Debug, cell::RefCell};
use crate::util::{Numeric, BindingId, FunctionId, Type, TypeId, TypeKind};
use crate::frontend::{ast::{self, Bindable, Returns}, ResolvedProgram};
use crate::bytecode::{Writer, WriteConst, Program, ARG1, ARG2, ARG3};
use crate::runtime::VMFunc;

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
    writer          : Writer<T>,
    /// List of bindings mapped to their TypeIds
    bindingtype_ids : Vec<TypeId>,
    /// List of registered types, effectively mapped via vector index = TypeId.
    types           : Vec<Type>,
    /// Maps from binding id to load-argument for each frame.
    locals          : LocalsStack,
    // Maps functions to their call index.
    functions       : RefCell<HashMap<FunctionId, u32>>,
    /// List of unresolved calls for each function.
    unresolved      : RefCell<HashMap<FunctionId, Vec<u32>>>,
}

/// Compiles a resolved program into bytecode.
pub fn compile<'ast, T>(program: ResolvedProgram<'ast, T>) -> Program<T> where T: VMFunc<T>+Debug {
    let mut compiler = Compiler::new();
    compiler.compile(program);
    compiler.into_program()
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
                        self.writer.discard();
                    }
                }
            }
            S::ForLoop(for_loop)        => self.compile_for_loop(for_loop),
            S::WhileLoop(while_loop)    => self.compile_while_loop(while_loop),
            S::Block(block)             => {
                self.compile_block(block);
                if let Some(result) = &block.result {
                    for _ in 0..self.bindingtype(result).quadsize() {
                        self.writer.discard();
                    }
                }
            }
            S::Return(ret)              => self.compile_return(ret),
            S::Expression(expression)   => {
                self.compile_expression(expression, false);
                for _ in 0..self.bindingtype(expression).quadsize() {
                    self.writer.discard();
                }
            }
        }
    }

    /// Compiles the given expression.
    fn compile_expression(self: &Self, item: &ast::Expression<'ast>, is_compound_assignment: bool) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.compile_literal(literal),
            E::Variable(variable)       => self.compile_variable(variable),
            E::Member(_)                => { /* nothing to do here */ }
            E::Call(call)               => self.compile_call(call),
            E::Assignment(assignment)   => self.compile_assignment(assignment),
            E::BinaryOp(binary_op)      => self.compile_binary_op(binary_op, is_compound_assignment),
            E::UnaryOp(unary_op)        => self.compile_unary_op(unary_op),
            E::Cast(cast)               => self.compile_cast(cast),
            E::Block(block)             => self.compile_block(block),
            E::IfBlock(if_block)        => self.compile_if_block(if_block),
        };
    }

    /// Compiles the assignment operation.
    fn compile_stack_assignment(self: &Self, item: &ast::Assignment<'ast>) {
        use crate::frontend::ast::BinaryOperator as BO;
        let binding_id = item.left.binding_id().unwrap();
        let index = self.locals.lookup(binding_id).index;
        let ty = self.bindingtype(&item.left);
        match item.op {
            BO::Assign => {
                self.compile_expression(&item.right, false);
                if ty.is_primitive() {
                    self.write_store(index, ty);
                } else {
                    self.compile_expression(&item.left, false);
                    self.writer.heap_ref();
                }
            },
            _ => {
                self.write_load(index, ty);
                self.compile_expression(&item.right, false);
                match item.op {
                    BO::AddAssign => self.write_add(&ty),
                    BO::SubAssign => self.write_sub(&ty),
                    BO::MulAssign => self.write_mul(&ty),
                    BO::DivAssign => self.write_div(&ty),
                    BO::RemAssign => self.write_rem(&ty),
                    _ => panic!("Unsupported assignment operator encountered"),
                };
                self.write_store(index, ty);
            },
        };
    }

    /// Compiles the assignment operation.
    fn compile_heap_assignment(self: &Self, item: &ast::Assignment<'ast>) {
        use crate::frontend::ast::BinaryOperator as BO;
        match item.op {
            BO::Assign => {
                self.compile_expression(&item.right, false);
                self.compile_expression(&item.left, false);
                self.writer.heap_ref();
            },
            _ => {
                self.compile_expression(&item.right, false);
                self.compile_expression(&item.left, true);
                self.writer.heap_ref();
                let ty = self.bindingtype(&item.left);
                if ty.size() == 8 {
                    self.writer.swap64();
                    self.writer.clone64(2);
                } else {
                    self.writer.swap64_32();
                    self.writer.clone64(1);
                }
                self.write_heap_fetch(ty.size());
                match item.op {
                    BO::AddAssign => self.write_add(&ty),
                    BO::SubAssign => self.write_sub(&ty),
                    BO::MulAssign => self.write_mul(&ty),
                    BO::DivAssign => self.write_div(&ty),
                    BO::RemAssign => self.write_rem(&ty),
                    _ => panic!("Unsupported assignment operator encountered"),
                };
                self.write_heap_put(ty.size());
            },
        }
    }

    /// Compiles the assignment operation.
    fn compile_assignment(self: &Self, item: &ast::Assignment<'ast>) {
        match item.left {
            ast::Expression::Variable(_) => self.compile_stack_assignment(item), // fixme: not sufficient, it might be a complete heap object
            ast::Expression::BinaryOp(_) => self.compile_heap_assignment(item),
            _ => panic!("invalid assignable"),
        }
    }

    /// Compiles the given function.
    fn compile_function(self: &Self, item: &ast::Function<'ast>) {
        self.writer.comment(&format!("\n{}", item.sig.ident.name));

        // register function bytecode index, check if any bytecode needs fixing
        let position = self.writer.position();
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
            if item.sig.ret.as_ref().map_or(false, |ret| !self.get_type(ret.type_id).is_primitive()) {
                self.writer.heap_ref(); // todo: this refs result again so that the following unref doesn't remove it. instead: "just" don't unref this one
            }
            self.write_heap_unref();
            self.write_ret();
        }
        // destroy local environment
        self.locals.pop();
    }

    /// Compiles the given call.
    fn compile_call(self: &Self, item: &ast::Call<'ast>) {

        // put args on stack
        for arg in item.args.iter() {
            self.compile_expression(arg, false);
        }

        if let Some(rust_fn_index) = item.rust_fn_index {

            // rust function
            self.writer.rustcall(T::from_u16(rust_fn_index));

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
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_binding(self: &Self, item: &ast::Binding<'ast>) {
        if let Some(expr) = &item.expr {
            self.compile_expression(expr, false);
            let ty = self.bindingtype(item);
            if !ty.is_primitive() {
                self.writer.heap_ref();
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

    fn compile_if_only_block(self: &Self, item: &ast::IfBlock<'ast>) {

        let exit_jump = self.writer.j0(123);
        self.compile_block(&item.if_block);
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
    }

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
        self.compile_expression(&item.cond, false);

        if item.else_block.is_none() {
            self.compile_if_only_block(item);
        } else {
            self.compile_if_else_block(&item.if_block, item.else_block.as_ref().unwrap());
        }
    }

    /// Compiles a while loop.
    fn compile_while_loop(self: &Self, item: &ast::WhileLoop<'ast>) {
        let start_target = self.writer.position();
        self.compile_expression(&item.expr, false);
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
                // todo: refactor this mess
                let (var_index, var_type) = self.range_info(item);
                // store lower range bound in iter variable
                self.compile_expression(&binary_op.left, false);
                self.write_store(var_index, var_type);
                // push upper range bound
                self.compile_expression(&binary_op.right, false);
                // precheck (could be avoided by moving condition to the end but not trivial due to stack top clone order)
                self.write_load(var_index, var_type);
                self.write_clone(var_type, if var_type.size() == 8 { 2 } else { 1 }); // clone upper bound for comparison, skip over iter inbetween
                if binary_op.op == ast::BinaryOperator::Range {
                    self.write_lt(var_type);
                } else {
                    self.write_lte(var_type);
                }
                let exit_jump = self.writer.j0(123);
                // compile block
                let start_target = self.writer.position();
                self.compile_block(&item.block);
                // load bounds, increment and compare
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
            self.compile_expression(expr, false);
            let ty = self.bindingtype(expr);
            if !ty.is_primitive() {
                self.writer.heap_ref();// todo: this refs result again so that the following unref doesn't remove it. instead: "just" don't unref this one
            }
        }
        self.write_heap_unref();
        self.write_ret();
    }

    /// Compiles the given block.
    fn compile_block(self: &Self, item: &ast::Block<'ast>) {

        for statement in item.statements.iter() {
            self.compile_statement(statement);
        }

        if let Some(result) = &item.result {
            self.compile_expression(result, false);
        }
    }

    /// Compiles the given literal
    fn compile_literal(self: &Self, item: &ast::Literal<'ast>) {
        use crate::frontend::ast::LiteralValue;
        let lit_type = self.bindingtype(item);
        match item.value {
            LiteralValue::Numeric(Numeric::Unsigned(0)) if lit_type.is_integer() && lit_type.size() <= 4 => { self.writer.lit0(); }
            LiteralValue::Numeric(Numeric::Unsigned(1)) if lit_type.is_integer() && lit_type.size() <= 4 => { self.writer.lit1(); }
            LiteralValue::Numeric(Numeric::Unsigned(2)) if lit_type.is_integer() && lit_type.size() <= 4 => { self.writer.lit2(); }
            LiteralValue::Numeric(Numeric::Signed(-1)) if lit_type.is_signed() && lit_type.size() <= 4 => { self.writer.litm1(); }
            LiteralValue::Numeric(int) => {
                match lit_type {
                    Type::i8 => { self.writer.lits(int.as_signed().unwrap() as i8); },
                    Type::u8 => { self.writer.litu(int.as_unsigned().unwrap() as u8); },
                    _ if lit_type.is_integer() || lit_type.is_float() => {
                         let pos = self.store_literal(item); self.write_const_fetch(pos, lit_type);
                    },
                    _ => panic!("Unexpected numeric literal type: {:?}", lit_type)
                }
            }
            LiteralValue::Bool(v) =>  {
                match lit_type {
                    Type::bool => { if v { self.writer.lit1(); } else { self.writer.lit0(); } },
                    _ => panic!("Unexpected boolean literal type: {:?}", lit_type)
                };
            },
            LiteralValue::String(_) => { // todo refactor string, array, struct into one block
                match lit_type {
                    Type::String => {
                        let pos = self.store_literal(item);
                        self.write_const_fetch(pos, lit_type);  // string heap index
                        self.writer.lit0();             //  offset into the string
                    },
                    _ => panic!("Unexpected string literal type: {:?}", lit_type)
                };
            },
            LiteralValue::Array(_) => {
                match lit_type {
                    Type::Array(_) => {
                        let pos = self.store_literal(item);
                        self.write_const_fetch(pos, lit_type);  // array heap index
                        self.writer.lit0();             //  offset into the array
                    },
                    _ => panic!("Unexpected array literal type: {:?}", lit_type)
                };
            },
            LiteralValue::Struct(_) => {
                match lit_type {
                    Type::Struct(_) => {
                        let pos = self.store_literal(item);
                        self.write_const_fetch(pos, lit_type);  // struct heap index
                        self.writer.lit0();             //  offset into the struct
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
                self.compile_expression(&item.expr, false);
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
    fn compile_binary_op(self: &Self, item: &ast::BinaryOp<'ast>, is_compound_assignment: bool) {
        use crate::frontend::ast::BinaryOperator as BO;

        if item.op != BO::And && item.op != BO::Or { // these short-circuit and need special handling // todo: can this be refactored?
            self.compile_expression(&item.left, is_compound_assignment);
            self.compile_expression(&item.right, is_compound_assignment);
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
                self.compile_expression(&item.left, is_compound_assignment);
                let exit_jump = self.writer.j0_top(123); // left is false, result cannot ever be true, skip right
                self.compile_expression(&item.right, is_compound_assignment);
                self.writer.and();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.j0_top(exit_target));
            },
            BO::Or => {
                self.compile_expression(&item.left, is_compound_assignment);
                let exit_jump = self.writer.jn0_top(123); // left is true, result cannot ever be false, skip right
                self.compile_expression(&item.right, is_compound_assignment);
                self.writer.or();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.jn0_top(exit_target));
            },
            // special
            BO::Range => unimplemented!("range"),
            BO::Index | BO::IndexWrite => {
                // todo need to handle both constpool (a byte array) and heap (arrays of byte arrays)
                //   use bitflag or simply negative numbers to indicate constpool items? simplifies copy-on-write
                if result_type.is_primitive() && !is_compound_assignment {
                    if item.op == BO::Index {
                        self.write_heap_fetch_element(result_type.size());
                    } else {
                        self.write_heap_put_element(result_type.size());
                    }
                } else {
                    // stack: <heap_index> <heap_offset> <index_operand>
                    let size = self.type_size(&result_type);
                    self.write_element_offset(size); // pop <index_operand> and <heap_offset> and push <heap_offset+index_operand*element_size>
                    // stack now <heap_index> <new_heap_offset>
                }
            },
            BO::Access | BO::AccessWrite => {
                if result_type.is_primitive() && !is_compound_assignment {
                    if item.op == BO::Access {
                        let offset = self.member_offset(&compare_type, item.right.as_member().unwrap().index.unwrap());
                        self.write_heap_fetch_member(result_type.size(), offset);
                    } else {
                        let offset = self.member_offset(&compare_type, item.right.as_member().unwrap().index.unwrap());
                        self.write_heap_put_member(result_type.size(), offset);
                    }
                } else {
                    // stack: <heap_index> <heap_offset>
                    let offset = self.member_offset(&compare_type, item.right.as_member().unwrap().index.unwrap());
                    self.write_member_offset(offset); // push additional offset, pop both offsets, write computed offset
                    // stack now <heap_index> <new_heap_offset>
                }
            },
            // others are handled elsewhere
            _ => panic!("Encountered invalid operation {:?} in compile_binary_op", item.op)
        };
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_cast(self: &Self, item: &ast::Cast<'ast>) {

        self.compile_expression(&item.expr, false);

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
    /// Stores given literal on the const pool.
    fn store_literal(self: &Self, item: &ast::Literal<'ast>) -> u32 {
        use crate::frontend::ast::LiteralValue;
        let lit_type = self.bindingtype(item);
        match item.value {
            LiteralValue::Numeric(int) => {
                match int {
                    Numeric::Signed(v) => {
                        match lit_type {
                            Type::i8 => self.writer.store_const(v as i8),
                            Type::i16 => self.writer.store_const(v as i16),
                            Type::i32 => self.writer.store_const(v as i32),
                            Type::i64 => self.writer.store_const(v as i64),
                            _ => panic!("Unexpected signed integer literal type: {:?}", lit_type)
                        }
                    },
                    Numeric::Unsigned(v) => {
                        match lit_type {
                            Type::i8 => self.writer.store_const(v as i8),
                            Type::i16 => self.writer.store_const(v as i16),
                            Type::i32 => self.writer.store_const(v as i32),
                            Type::i64 => self.writer.store_const(v as i64),
                            Type::u8 => self.writer.store_const(v as u8),
                            Type::u16 => self.writer.store_const(v as u16),
                            Type::u32 => self.writer.store_const(v as u32),
                            Type::u64 => self.writer.store_const(v as u64),
                            _ => panic!("Unexpected unsigned integer literal type: {:?}", lit_type)
                        }
                    },
                    Numeric::Float(v) => {
                        match lit_type {
                            Type::f32 => self.writer.store_const(v as f32),
                            Type::f64 => self.writer.store_const(v),
                            _ => panic!("Unexpected float literal type: {:?}", lit_type)
                        }
                    },
                    Numeric::Overflow => panic!("Literal computation overflow")
                }
            },
            LiteralValue::Bool(v) =>  {
                match lit_type {
                    Type::bool => self.writer.store_const(if v { 1u8 } else { 0u8 }),
                    _ => panic!("Unexpected boolean literal type: {:?}", lit_type)
                }
            },
            LiteralValue::String(v) => {
                match lit_type {
                    Type::String => self.writer.store_const(v),
                    _ => panic!("Unexpected string literal type: {:?}", lit_type)
                }
            },
            LiteralValue::Array(_) => {
                let ty = self.bindingtype(item);
                let size = self.type_size(ty);
                let pos = self.writer.store_const(size);
                self.store_literal_data(item);
                pos
            },
            LiteralValue::Struct(_) => {
                let ty = self.bindingtype(item);
                let size = self.type_size(ty);
                let pos = self.writer.store_const(size);
                self.store_literal_data(item);
                pos
            },
        }
    }
    /// Stores array data in constpool. Note: when read with const_fetch_object() array size needs to be written first!
    fn store_literal_data(self: &Self, item: &ast::Literal<'ast>) {
        use crate::frontend::ast::LiteralValue;

        match &item.value {
            LiteralValue::Array(array) => {
                if array.elements.len() > 0 {
                    for item in &array.elements {
                        self.store_literal_data(item);
                    }
                }
            }
            LiteralValue::Struct(struct_) => {
                let ty = self.bindingtype(item);
                for (name, _) in ty.as_struct().unwrap().fields.iter() {
                    let literal = &struct_.fields[&name[..]];
                    self.store_literal_data(literal);
                }
            }
            _ => {
                self.store_literal(item);
            }
        }
    }
    /// Computes the size of a type in bytes.
    fn type_size(self: &Self, ty: &Type) -> u32 {
        match ty {
            Type::Array(array) => {
                let len = array.len.unwrap();
                if len == 0 {
                    0
                } else {
                    len as u32 * self.type_size(self.get_type(array.type_id))
                }
            }
            Type::Struct(struct_) => {
                struct_.fields.iter().map(|&(_, type_id)| { self.type_size(self.get_type(type_id)) }).sum()
            }
            Type::Enum(_) => unimplemented!("enum"),
            _ => ty.size() as u32
        }
    }
    /// Computes struct member offset in bytes.
    fn member_offset(self: &Self, ty: &Type, member_index: u32) -> u32 {
        let struct_ = ty.as_struct().unwrap();
        let mut offset = 0;
        for index in 0 .. member_index {
            offset += self.type_size(self.get_type(struct_.fields[index as usize].1));
        }
        offset
    }
    fn write_member_offset(self: &Self, offset: u32) {
        if offset == 0 {
            // nothing to do
        } else if offset == 1 {
            self.writer.lit1();
            self.writer.addi();
        } else if offset == 2 {
            self.writer.lit2();
            self.writer.addi();
        } else if offset <= 255 {
            self.writer.litu(offset as u8);
            self.writer.addi();
        } else {
            let const_id = self.writer.store_const(offset);
            self.write_const_fetch(const_id, &Type::u32);
            self.writer.addi();
        }
    }
    fn write_element_offset(self: &Self, element_size: u32) {
        if element_size < (1 << 8) {
            self.writer.index(element_size as u8);
        } else if element_size < (1 << 16) {
            self.writer.index_16(element_size as u16);
        } else {
            self.writer.index_32(element_size);
        }
    }
    fn write_heap_fetch(self: &Self, result_size: u8) {
        match result_size {
            1 => { self.writer.heap_fetch8(); },
            2 => { self.writer.heap_fetch16(); },
            4 => { self.writer.heap_fetch32(); },
            8 => { self.writer.heap_fetch64(); },
            _ => panic!("Invalid result size {} for heap_fetch", result_size)
        }
    }
    fn write_heap_put(self: &Self, result_size: u8) {
        match result_size {
            1 => { self.writer.heap_put8(); },
            2 => { self.writer.heap_put16(); },
            4 => { self.writer.heap_put32(); },
            8 => { self.writer.heap_put64(); },
            _ => panic!("Invalid result size {} for heap_put", result_size)
        }
    }
    fn write_heap_fetch_member(self: &Self, result_size: u8, offset: u32) {
        match result_size {
            1 => { self.writer.heap_fetch_member8(offset); },
            2 => { self.writer.heap_fetch_member16(offset); },
            4 => { self.writer.heap_fetch_member32(offset); },
            8 => { self.writer.heap_fetch_member64(offset); },
            _ => panic!("Invalid result size {} for heap_fetch_member", result_size)
        }
    }
    fn write_heap_fetch_element(self: &Self, element_size: u8) {
        match element_size {
            1 => { self.writer.heap_fetch_element8(); },
            2 => { self.writer.heap_fetch_element16(); },
            4 => { self.writer.heap_fetch_element32(); },
            8 => { self.writer.heap_fetch_element64(); },
            _ => panic!("Invalid element size {} for heap_fetch_element", element_size)
        }
    }
    fn write_heap_put_member(self: &Self, result_size: u8, offset: u32) {
        match result_size {
            1 => { self.writer.heap_put_member8(offset); },
            2 => { self.writer.heap_put_member16(offset); },
            4 => { self.writer.heap_put_member32(offset); },
            8 => { self.writer.heap_put_member64(offset); },
            _ => panic!("Invalid result size {} for heap_put_member", result_size)
        }
    }
    fn write_heap_put_element(self: &Self, element_size: u8) {
        match element_size {
            1 => { self.writer.heap_put_element8(); },
            2 => { self.writer.heap_put_element16(); },
            4 => { self.writer.heap_put_element32(); },
            8 => { self.writer.heap_put_element64(); },
            _ => panic!("Invalid element size {} for heap_put_element", element_size)
        }
    }
    /// Writes operations to unref local heap objects.
    fn write_heap_unref(self: &Self/*, exclude: Option<BindingId>*/) {
        self.locals.borrow(|locals| {
            // fixme: exclude returned binding
            for (&binding_id, local) in locals.map.iter().filter(|(_, local)| local.in_scope) {
                let type_id = self.bindingtype_ids[binding_id.into_usize()];
                let ty = &self.types[type_id.into_usize()];
                if !ty.is_primitive() {
                    self.write_load(local.index, ty);
                    self.writer.heap_unref();
                }
            }
        });
    }
    /// Swap 2 stack values, ty_a being topmost, ty_b next below.
    fn write_swap(self: &Self, ty_a: &Type, ty_b: &Type) {
        let size_a = ty_a.size();
        let size_b = ty_b.size();
        if size_a == 8 && size_b == 8 {
            self.writer.swap64();
        } else if size_a < 8 && size_b < 8 {
            self.writer.swap32();
        } else if size_a == 8 && size_b < 8 {
            self.writer.swap64_32();
        } else if size_a < 8 && size_b == 8 {
            self.writer.swap32_64();
        }
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
        match ty.size() {
            1 | 2 | 4 => self.writer.ceqr32(),
            8 => self.writer.ceqr64(),
            _ => panic!("Unsupported type size"),
        };
    }
    fn write_neq(self: &Self, ty: &Type) {
        match ty.size() {
            1 | 2 | 4 => self.writer.cneqr32(),
            8 => self.writer.cneqr64(),
            _ => panic!("Unsupported type size"),
        };
    }
    fn write_lt(self: &Self, ty: &Type) {
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
    }
    fn write_lte(self: &Self, ty: &Type) {
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
    }
    fn write_const_fetch(self: &Self, index: u32, ty: &Type) {
        use std::{u8, u16, u32};
        match ty.size() {
            2 => {
                if index <= u8::MAX as u32 {
                    self.writer.const_fetch16(index as u8);
                } else if index <= u16::MAX as u32 {
                    self.writer.const_fetch16_16(index as u16);
                } else {
                    self.writer.const_fetch16_32(index);
                }
            },
            4 => {
                if index <= u8::MAX as u32 {
                    self.writer.const_fetch(index as u8);
                } else if index <= u16::MAX as u32 {
                    self.writer.const_fetch_16(index as u16);
                } else {
                    self.writer.const_fetch_32(index);
                }
            },
            8 => {
                if ty.is_primitive() {
                    if index <= u8::MAX as u32 {
                        self.writer.const_fetch64(index as u8);
                    } else if index <= u16::MAX as u32 {
                        self.writer.const_fetch64_16(index as u16);
                    } else {
                        self.writer.const_fetch64_32(index as u32);
                    }
                } else {
                    if index <= u8::MAX as u32 {
                        self.writer.const_fetch_object(index as u8);
                    } else if index <= u16::MAX as u32 {
                        self.writer.const_fetch_object_16(index as u16);
                    } else {
                        self.writer.const_fetch_object_32(index);
                    }
                }
            },
            _ => panic!("Unsupported type size"),
        }
    }
    /// Writes an appropriate variant of the store instruction.
    fn write_store(self: &Self, index: i32, ty: &Type) {
        use std::{i8, i16, i32};
        let size = ty.size();
        let kind = ty.kind();
        if size == 8 || kind == TypeKind::String || kind == TypeKind::Array {
            if index >= i8::MIN as i32 && index <= i8::MAX as i32 {
                self.writer.storer64_s8(index as i8);
            } else if index >= i16::MIN as i32 && index <= i16::MAX as i32 {
                self.writer.storer64_s16(index as i16);
            } else {
                self.writer.storer64_s32(index);
            }
        } else if size <= 4 {
            if index >= i8::MIN as i32 && index <= i8::MAX as i32 {
                self.writer.storer_s8(index as i8);
            } else if index >= i16::MIN as i32 && index <= i16::MAX as i32 {
                self.writer.storer_s16(index as i16);
            } else {
                self.writer.storer_s32(index);
            }
        } else {
            panic!("Unsupported type {:?} for store operation", ty);
        }
    }
    /// Writes an appropriate variant of the load instruction.
    fn write_load(self: &Self, index: i32, ty: &Type) {
        use std::{i8, i16, i32};
        let size = ty.size();
        let kind = ty.kind();
        if size == 8 || kind == TypeKind::String || kind == TypeKind::Array {
            if index >= i8::MIN as i32 && index <= i8::MAX as i32 {
                self.writer.loadr64_s8(index as i8);
            } else if index >= i16::MIN as i32 && index <= i16::MAX as i32 {
                self.writer.loadr64_s16(index as i16);
            } else {
                self.writer.loadr64_s32(index);
            }
        } else if size <= 4 {
            match index {
                ARG1 => self.writer.load_arg1(),
                ARG2 => self.writer.load_arg2(),
                ARG3 => self.writer.load_arg3(),
                _ => if index >= i8::MIN as i32 && index <= i8::MAX as i32 {
                    self.writer.loadr_s8(index as i8)
                } else if index >= i16::MIN as i32 && index <= i16::MAX as i32 {
                    self.writer.loadr_s16(index as i16)
                } else {
                    self.writer.loadr_s32(index)
                }
            };
        } else {
            panic!("Unsupported type {:?} for load operation", ty);
        }
    }
    fn write_clone(self: &Self, ty: &Type, offset: u8) {
        match ty.size() {
            1 | 2 | 4 => self.writer.clone32(offset),
            8 => self.writer.clone64(offset),
            _ => panic!("unsupported type size"),
        };
    }
    /// Writes a return instruction with the correct arguments for the current stack frame.
    fn write_ret(self: &Self) {
        self.writer.ret(self.locals.ret_size() as u8, self.locals.arg_size() as u8);
    }
}