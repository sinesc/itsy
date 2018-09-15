//! Bytecode emitter. Compiles bytecode from AST.

// todo: remove
#![allow(unused_variables)]

use std::collections::HashMap;
use frontend::{ast, ResolvedProgram, util::{Integer, BindingId, FunctionId, Type}};
use bytecode::{Writer, Program, RustFnId};
use std::fmt::Debug;

/// Maps bindings and arguments to indices relative to the stackframe.
struct Locals {
    map     : HashMap<BindingId, i32>,
    next_arg: i32,
    next_var: i32,
}

impl Locals {
    pub fn new() -> Self {
        Locals {
            map     : HashMap::new(),
            next_arg: -4,
            next_var: 0,
        }
    }
}

/// A stack Locals mappings for nested structures.
struct LocalsStack(Vec<Locals>);

impl LocalsStack {
    fn new() -> Self {
        LocalsStack(Vec::new())
    }
    fn push(self: &mut Self, frame: Locals) {
        self.0.push(frame);
    }
    fn pop(self: &mut Self) -> Locals {
        self.0.pop().expect("Attempted to pop empty LocalsStack")
    }
    fn lookup(self: &Self, binding: &BindingId) -> Option<i32> {
        self.0.last().expect("Attempted to lookup stack item without LocalsStack").map.get(binding).map(|b| *b)
    }
}

/// Bytecode emitter. Compiles bytecode from resolved program (AST).
pub struct Compiler<T> where T: RustFnId {
    writer          : Writer<T>,
    types           : Vec<Type>,
    /// Maps from binding id to load-argument for each frame.
    locals          : LocalsStack,
    // Maps functions to their call index
    functions       : HashMap<FunctionId, u32>,
    /// List of unresolved calls for each function.
    unresolved      : HashMap<FunctionId, Vec<u32>>,
}

/// Basic compiler functionality.
impl<'a, T> Compiler<T> where T: RustFnId {

    /// Creates a new compiler.
    pub fn new() -> Self {
        Compiler {
            writer      : Writer::new(),
            types       : Vec::new(),
            locals      : LocalsStack::new(),
            functions   : HashMap::new(),
            unresolved  : HashMap::new(),
        }
    }

    /// Creates a new compiler using given writer.
    pub fn with_writer(writer: Writer<T>) -> Self {
        Compiler {
            writer,
            types       : Vec::new(),
            locals      : LocalsStack::new(),
            functions   : HashMap::new(),
            unresolved  : HashMap::new(),
        }
    }

    /// Compiles the current program.
    pub fn compile(self: &mut Self, program: ResolvedProgram<'a, T>) {

        let ResolvedProgram { ast: statements, types, .. } = program;

        self.types = types;

        for statement in statements.iter() {
            self.compile_statement(statement);
        }
    }

    /// Returns compiled bytecode program.
    pub fn into_program(self: Self) -> Program<T> {
        self.writer.into_program()
    }

    /// Returns writer containing compiled bytecode program.
    pub fn into_writer(self: Self) -> Writer<T> {
        self.writer
    }
}

/// Methods for compiling individual code structures.
impl<'a, T> Compiler<T> where T: RustFnId {

    /// Compiles the given statement.
    pub fn compile_statement(self: &mut Self, item: &ast::Statement<'a>) {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.compile_function(function),
            S::Structure(structure)     => { }, // todo: handle
            S::Binding(binding)         => self.compile_binding(binding),
            S::IfBlock(if_block)        => self.compile_if_block(if_block),
            S::ForLoop(for_loop)        => { }, // todo: handle
            S::WhileLoop(while_loop)    => self.compile_while_loop(while_loop),
            S::Block(block)             => self.compile_block(block),
            S::Return(ret)              => self.compile_return(ret),
            S::Expression(expression)   => self.compile_expression(expression),
        }
    }

    /// Compiles the given expression.
    pub fn compile_expression(self: &mut Self, item: &ast::Expression<'a>) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.compile_literal(literal),
            E::Variable(variable)       => self.compile_variable(variable),
            E::Call(call)               => self.compile_call(call),
            E::Assignment(assignment)   => self.compile_assignment(assignment),
            E::BinaryOp(binary_op)      => self.compile_binary_op(binary_op),
            E::UnaryOp(unary_op)        => { }, // todo: handle,
            E::Block(block)             => { }, // todo: handle,
            E::IfBlock(if_block)        => self.compile_if_block(if_block),
        };
    }

    /// Compiles the assignment operation.
    pub fn compile_assignment(self: &mut Self, item: &ast::Assignment<'a>) {
        use frontend::ast::BinaryOperator as BO;
        let binding_id = item.left.binding_id.unwrap();
        let index = self.locals.lookup(&binding_id).expect("Unresolved binding encountered");
        match item.op {
            BO::Assign => {
                self.compile_expression(&item.right);
                self.writer.store(index);
            },
            compound_assign @ _ => {
                self.compile_expression(&item.right);
                self.writer.load(index);
                match compound_assign {
                    BO::AddAssign => self.writer.add(),
                    BO::SubAssign => self.writer.sub(),
                    BO::MulAssign => self.writer.mul(),
                    BO::DivAssign => unimplemented!("divassign"),
                    BO::RemAssign => unimplemented!("remassign"),
                    _ => panic!("Unsupported assignment operator encountered"),
                };
                self.writer.store(index);
            },
        };
    }

    /// Compiles the given function.
    pub fn compile_function(self: &mut Self, item: &ast::Function<'a>) {

        // register function bytecode index, check if any bytecode needs fixing
        let position = self.writer.position();
        let function_id = item.function_id.unwrap();
        let num_args = item.sig.args.len() as u8;
        self.functions.insert(function_id, position);
        self.fix_targets(function_id, position, num_args);

        // create local environment
        let mut frame = Locals::new();

        for arg in item.sig.args.iter() {
            let next_arg = frame.next_arg;
            frame.map.insert(arg.binding_id.unwrap(), next_arg);
            frame.next_arg -= 1;
        }

        for statement in item.block.statements.iter() {
            if let ast::Statement::Binding(binding) = statement {
                let next_var = frame.next_var;
                frame.map.insert(binding.binding_id.unwrap(), next_var);
                frame.next_var += 1;
            }
        }

        if frame.next_var > 0 {
            self.writer.reserve(frame.next_var as u8);
        }

        self.locals.push(frame);
        self.compile_block(&item.block);
        self.writer.ret(); // todo: skip if last statement was "return"
        self.locals.pop();
    }

    /// Compiles the given call.
    pub fn compile_call(self: &mut Self, item: &ast::Call<'a>) {

        // put args on stack
        for arg in item.args.iter() { // todo: optional parameter? we need the exact signature
            self.compile_expression(arg);
        }

        if let Some(rust_fn_index) = item.rust_fn_index {

            // rust function
            self.writer.rustcall(T::from_u16(rust_fn_index));

        } else {

            // normal function: identify call target or write dummy
            let function_id = item.function_id.expect(&format!("Unresolved function \"{}\" encountered", item.path.0[0]));
            let call_position = self.writer.position();

            let target = if let Some(&target) = self.functions.get(&function_id) {
                target
            } else {
                self.unresolved.entry(function_id).or_insert(Vec::new()).push(call_position);
                123
            };

            self.writer.call(target, item.args.len() as u8);       // todo: get args from sig, not call (maybe?) should match once resolve does the checking
        }
    }

    /// Compiles a variable binding and optional assignment.
    pub fn compile_binding(self: &mut Self, item: &ast::Binding<'a>) {
        if let Some(expr) = &item.expr {
            self.compile_expression(expr);
            let binding_id = item.binding_id.unwrap();
            self.writer.store(self.locals.lookup(&binding_id).expect("Unresolved binding encountered"));
        }
    }

    /// Compiles the given variable.
    pub fn compile_variable(self: &mut Self, item: &ast::Variable<'a>) {
        let load_index = {
            let binding_id = item.binding_id.expect("Unresolved binding encountered");
            self.locals.lookup(&binding_id).expect("Failed to look up local variable index")
        };
        self.write_load(load_index);
    }

    /// Compiles the given if block.
    pub fn compile_if_block(self: &mut Self, item: &ast::IfBlock<'a>) {

        // compile condition and jump placeholder
        self.compile_expression(&item.cond);
        let else_jump = self.writer.j0(123);

        // compile if-case and remember position just after it (else/remaining code will go there)
        self.compile_block(&item.if_block);

        let done_jump = if item.else_block.is_some() {
            Some(self.writer.jmp(123))
        } else {
            None
        };

        // go back and overwrite placeholder with else/remaining code position
        let else_code = self.writer.position;
        self.writer.overwrite(else_jump, |w| w.j0(else_code));

        // compile else-case if we have one
        if let Some(else_block) = &item.else_block {
            self.compile_block(else_block);
        }

        // go back and fix else-skip jump for the if-case
        if let Some(done_jump) = done_jump {
            let done_code = self.writer.position;
            self.writer.overwrite(done_jump, |w| w.jmp(done_code));
        }
    }

    /// Compiles a while loop.
    pub fn compile_while_loop(self: &mut Self, item: &ast::WhileLoop<'a>) {
        let start_target = self.writer.position;
        self.compile_expression(&item.expr);
        let exit_jump = self.writer.j0(123);
        self.compile_block(&item.block);
        self.writer.jmp(start_target);
        let exit_target = self.writer.position;
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
    }

    /// Compiles a return statement
    pub fn compile_return(self: &mut Self, item: &ast::Return<'a>) {
        if let Some(expr) = &item.expr {
            self.compile_expression(expr);
        } else {
            self.writer.lit0(); // todo: need to return something for now
        }
        self.writer.ret();
    }

    /// Compiles the given block.
    pub fn compile_block(self: &mut Self, item: &ast::Block<'a>) {

        for statement in item.statements.iter() {
            self.compile_statement(statement);
        }

        if let Some(result) = &item.result {
            self.compile_expression(result);
        }
    }

    /// Compiles the given literal
    pub fn compile_literal(self: &mut Self, item: &ast::Literal<'a>) {
        use frontend::ast::LiteralValue;
        match item.value {
            LiteralValue::Integer(int) => match int {
                Integer::Signed(v) => self.write_lit(v),
                Integer::Unsigned(v) => self.write_lit(v as i64), // todo: u64>i64
            }
            _ => unimplemented!(),
        };
    }

    /// Compiles the given binary operation.
    pub fn compile_binary_op(self: &mut Self, item: &ast::BinaryOp<'a>) {
        use frontend::ast::BinaryOperator as BO;
        self.compile_expression(&item.right);
        self.compile_expression(&item.left);
        match item.op {
            // arithmetic
            BO::Add => self.writer.add(),
            BO::Sub => self.writer.sub(),
            BO::Mul => unimplemented!("mul"),
            BO::Div => unimplemented!("div"),
            BO::Rem => unimplemented!("rem"),
            // assigments
            BO::Assign => unimplemented!("assign"),
            BO::AddAssign => unimplemented!("addassign"),
            BO::SubAssign => unimplemented!("subassign"),
            BO::MulAssign => unimplemented!("mulassign"),
            BO::DivAssign => unimplemented!("divassgi"),
            BO::RemAssign => unimplemented!("remassign"),
            // comparison
            BO::Less => self.writer.clts(),
            BO::Greater => self.writer.cgts(),
            BO::LessOrEq => unimplemented!("lessoreq"),
            BO::GreaterOrEq => unimplemented!("greateroreq"),
            BO::Equal => self.writer.ceq(),
            BO::NotEqual => unimplemented!("notequal"),
            // boolean
            BO::And => unimplemented!("and"),
            BO::Or => unimplemented!("or"),
            // special
            BO::Range => unimplemented!("range"),
        };
    }
}

impl<'a, T> Compiler<T> where T: RustFnId {
    /// Fixes function call targets for previously not generated functions.
    fn fix_targets(self: &mut Self, function_id: FunctionId, position: u32, num_args: u8) {
        if let Some(targets) = self.unresolved.remove(&function_id) {
            let backup_position = self.writer.position();
            for &target in targets.iter() {
                self.writer.set_position(target);
                self.writer.call(position, num_args); // todo: may have to handle multiple call types
            }
            self.writer.set_position(backup_position);
        }
    }
    /// Writes an appropriate variant of the load instruction.
    fn write_load(self: &mut Self, index: i32) {
        match index {
            -4 => self.writer.load_arg1(),
            -5 => self.writer.load_arg2(),
            -6 => self.writer.load_arg3(),
            _ => self.writer.load(index),
        };
    }
    /// Writes an appropriate variant of the lit instruction.
    fn write_lit(self: &mut Self, value: i64) {
        match value {
            0 => self.writer.lit0(),
            1 => self.writer.lit1(),
            2 => self.writer.lit2(),
            -1 => self.writer.litm1(),
            _ => self.writer.lit_u32(value as u32), // todo: other lit types
        };
    }
}

/// Compiles a resolved program into bytecode.
pub fn compile<'a, T>(program: ResolvedProgram<'a, T>) -> Writer<T> where T: RustFnId+Debug {
    let mut compiler = Compiler::new();
    compiler.compile(program);
    compiler.into_writer()
}