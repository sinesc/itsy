//! Bytecode generation.

mod writer;
mod opcodes;
mod compiler;

use std::marker::PhantomData;
pub use self::writer::*;
pub use self::compiler::compile;

/// An Itsy bytecode program.
#[derive(Debug)]
pub struct Program<T> where T: crate::runtime::VMFunc<T> {
    rust_fn: PhantomData<T>,
    pub(crate) instructions : Vec<u8>,
    pub(crate) consts       : Vec<u8>,
}

impl<T> Program<T> where T: crate::runtime::VMFunc<T> {
    pub fn new() -> Self {
        Program {
            rust_fn     : PhantomData,
            instructions: Vec::new(),
            consts      : Vec::new(),
        }
    }
}
