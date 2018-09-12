//! Bytecode emitter. Compiles bytecode from AST.

// todo: remove
#![allow(unused_variables)]

use std::collections::HashMap;
use frontend::{ast, ResolvedProgram, util::{Integer, BindingId}};
use bytecode::{Type, Writer, Program};

struct StackFrameMap {
    map: HashMap<BindingId, i32>,
    next_arg: i32,
    next_var: i32,
}

impl StackFrameMap {
    fn new() -> Self {
        StackFrameMap {
            map: HashMap::new(),
            next_arg: -4,
            next_var: 0,
        }
    }
}



/// Bytecode emitter. Compiles bytecode from resolved program (AST).
pub struct Compiler {
    writer  : Writer,
    types   : Vec<Type>,
    /// Stack of stackframe layouts. Contains a map from binding id to load-argument for each frame.
    locals  : Vec<StackFrameMap>,
}

impl<'a> Compiler {

    /// Creates a new compiler.
    pub fn new() -> Self {
        Compiler {
            writer  : Writer::new(),
            types   : Vec::new(),
            locals  : Vec::new(),
        }
    }

    /// Creates a new compiler using given writer.
    pub fn with_writer(writer: Writer) -> Self {
        Compiler {
            writer,
            types   : Vec::new(),
            locals  : Vec::new(),
        }
    }

    /// Returns compiled bytecode program.
    pub fn into_program(self: Self) -> Program {
        self.writer.into_program()
    }

    /// Returns writer containing compiled bytecode program.
    pub fn into_writer(self: Self) -> Writer {
        self.writer
    }

    /// Compiles the current program.
    pub fn compile(self: &mut Self, program: ResolvedProgram<'a>) {

        let ResolvedProgram { ast: statements, types } = program;

        self.types = types;

        for statement in statements.iter() {
            self.compile_statement(statement);
        }
    }

    /// Compiles the given statement.
    pub fn compile_statement(self: &mut Self, item: &ast::Statement<'a>) {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.compile_function(function),
            S::Structure(structure)     => { }, // todo: handle
            S::Binding(binding)         => { }, // todo: handle
            S::IfBlock(if_block)        => self.compile_if_block(if_block),
            S::ForLoop(for_loop)        => { }, // todo: handle
            S::Block(block)             => self.compile_block(block),
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
            E::Assignment(assignment)   => { }, // todo: handle,
            E::BinaryOp(binary_op)      => self.compile_binary_op(binary_op),
            E::UnaryOp(unary_op)        => { }, // todo: handle,
            E::Block(block)             => { }, // todo: handle,
            E::IfBlock(if_block)        => self.compile_if_block(if_block),
        };
    }

    /// Compiles the given function.
    pub fn compile_function(self: &mut Self, item: &ast::Function<'a>) {

        // create local environment

        let mut frame = StackFrameMap::new();

        for arg in item.sig.args.iter() {
            let next_arg = frame.next_arg;
            frame.map.insert(arg.binding_id.unwrap(), next_arg);
            frame.next_arg -= 1;
        }

        self.locals.push(frame);
        self.compile_block(&item.block);
        self.writer.ret();
        self.locals.pop();
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

    /// Compiles the given variable.
    pub fn compile_variable(self: &mut Self, item: &ast::Variable<'a>) {
        let load_index = {
            let binding_id = item.binding_id.expect("Unresolved binding encountered");
            let locals = &self.locals.last().expect("Frame stack was empty").map;
            *locals.get(&binding_id).expect("Failed to look up local variable index")
        };
        self.write_load(load_index);
    }

    /// Compiles the given call.
    pub fn compile_call(self: &mut Self, item: &ast::Call<'a>) {
        // print hack
        if item.path.0[0] == "print" {
            self.writer.print();
            return;
        }

        // put args on stack
        for arg in item.args.iter() {
            self.compile_expression(arg);
        }

        self.writer.call(0, item.args.len() as u8);       // todo: 0
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
            BO::Mul => unimplemented!(),
            BO::Div => unimplemented!(),
            BO::Rem => unimplemented!(),
            // assigments
            BO::Assign => unimplemented!(),
            BO::AddAssign => unimplemented!(),
            BO::SubAssign => unimplemented!(),
            BO::MulAssign => unimplemented!(),
            BO::DivAssign => unimplemented!(),
            BO::RemAssign => unimplemented!(),
            // comparison
            BO::Less => self.writer.clts(),
            BO::Greater => unimplemented!(),
            BO::LessOrEq => unimplemented!(),
            BO::GreaterOrEq => unimplemented!(),
            BO::Equal => unimplemented!(),
            BO::NotEqual => unimplemented!(),
            // boolean
            BO::And => unimplemented!(),
            BO::Or => unimplemented!(),
            // special
            BO::Range => unimplemented!(),
        };
    }
}

impl<'a> Compiler {
    fn write_load(self: &mut Self, index: i32) {
        match index {
            -4 => self.writer.load_arg1(),
            -5 => self.writer.load_arg2(),
            -6 => self.writer.load_arg3(),
            _ => self.writer.load(index),
        };
    }
    fn write_lit(self: &mut Self, value: i64) {
        match value {
            0 => self.writer.lit0(),
            1 => self.writer.lit1(),
            2 => self.writer.lit2(),
            -1 => self.writer.litm1(),
            _ => self.writer.lit_u8(value as u8), // todo: other lit types
        };
    }
}

/// Compiles a resolved program into bytecode.
pub fn compile<'a>(program: ResolvedProgram<'a>) -> Writer {
    let mut compiler = Compiler::new();
    compiler.compile(program);
    compiler.into_writer()
}