//! Bytecode generation.

mod writer;
mod opcodes;
mod compiler;
mod optimizer;

use std::marker::PhantomData;
pub use self::writer::*;
pub use self::compiler::compile;
pub use self::opcodes::OpCode;

const ARG1: i32 = -3;
const ARG2: i32 = -4;
const ARG3: i32 = -5;

/// An Itsy bytecode program.
#[derive(Clone, Debug)]
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
