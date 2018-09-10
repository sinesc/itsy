//! Bytecode buffer and writer.

use bytecode::Program;

/// Bytecode buffer and writer.
#[derive(Debug)]
pub struct Writer {
    pub(crate) program: Program,
}

impl Writer {
    /// Creates a new writer instance.
    pub fn new() -> Self {
        Writer {
            program: Vec::new(),
        }
    }
    /// Converts the writer into the underlying buffer.
    pub fn into_program(self: Self) -> Program {
        self.program
    }
}