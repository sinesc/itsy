//! Bytecode emitter. Compiles bytecode from AST.

// todo: remove
#![allow(unused_variables)]

use frontend::ResolvedProgram;
use bytecode::{Writer, Program};

/// Bytecode emitter. Compiles bytecode from resolved program (AST).
pub struct Compiler<'a> {
    ast     : ResolvedProgram<'a>,
    writer  : Writer,
}

impl<'a> Compiler<'a> {

    /// Creates a new compiler for given AST.
    pub fn new(program: ResolvedProgram<'a>) -> Self {
        Compiler {
            ast     : program,
            writer  : Writer::new(),
        }
    }

    /// Returns compiled bytecode program.
    pub fn into_program(self: Self) -> Program {
        self.writer.into_program()
    }
}