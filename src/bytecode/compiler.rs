//! Bytecode emitter. Compiles bytecode from AST.

// todo: remove
#![allow(unused_variables)]

use frontend::{ast, ResolvedProgram};
use bytecode::{Type, Writer, Program};

/// Bytecode emitter. Compiles bytecode from resolved program (AST).
pub struct Compiler {
    //program : ResolvedProgram<'a>,
    writer  : Writer,
    types   : Vec<Type>,
}

impl<'a> Compiler {

    /// Creates a new compiler for given AST.
    pub fn new() -> Self {
        Compiler {
            //program : program,
            writer  : Writer::new(),
            types   : Vec::new(), // set by compile :/&
        }
    }

    /// Compiles the current program.
    pub fn compile(self: &mut Self, program: ResolvedProgram<'a>) {

        let ResolvedProgram { ast: statements, types } = program;

        self.types = types;

        for statement in statements.iter() {
            self.compile_statement(statement);
        }
    }

    /// Returns compiled bytecode program.
    pub fn into_program(self: Self) -> Program {
        self.writer.into_program()
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

    }

    /// Compiles the given if block.
    pub fn compile_if_block(self: &mut Self, item: &ast::IfBlock<'a>) {

    }

    /// Compiles the given block.
    pub fn compile_block(self: &mut Self, item: &ast::Block<'a>) {

    }

    /// Compiles the given literal
    pub fn compile_literal(self: &mut Self, item: &ast::Literal<'a>) {

    }

    /// Compiles the given variable.
    pub fn compile_variable(self: &mut Self, item: &ast::Variable<'a>) {

    }

    /// Compiles the given call.
    pub fn compile_call(self: &mut Self, item: &ast::Call<'a>) {

    }

    /// Compiles the given binary operation.
    pub fn compile_binary_op(self: &mut Self, item: &ast::BinaryOp<'a>) {

    }
}

/// Compiles a resolved program into bytecode.
pub fn compile<'a>(program: ResolvedProgram<'a>) -> Program {
    let mut compiler = Compiler::new();
    compiler.compile(program);
    compiler.into_program()
}