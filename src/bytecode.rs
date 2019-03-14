//! Bytecode generation and execution.

#[macro_use]
mod vm;
mod writer;
mod opcodes;
mod compiler;
mod stack;
mod heap;

use std::marker::PhantomData;
pub use self::vm::{VM, VMState};
pub use self::writer::*;
pub use self::compiler::compile;
pub use self::stack::*;
pub use self::heap::*;

/// A stack value.
pub(crate) type Value = i32;
pub(crate) type Value64 = i64;

/// An Itsy bytecode program.
#[derive(Debug)]
pub struct Program<T> where T: crate::VMFunc<T> {
    rust_fn: PhantomData<T>,
    pub(crate) instructions : Vec<u8>,
    pub(crate) consts       : Vec<u8>,
}

impl<T> Program<T> where T: crate::VMFunc<T> {
    pub fn new() -> Self {
        Program {
            rust_fn     : PhantomData,
            instructions: Vec::new(),
            consts      : Vec::new(),
        }
    }
}