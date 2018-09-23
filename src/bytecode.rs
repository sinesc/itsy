//! Bytecode generation and execution.

// todo: remove
#![allow(dead_code)]

mod vm;
#[macro_use]
mod macros;
mod writer;
mod opcodes;
mod compiler;
mod stack;

use std::marker::PhantomData;
pub use self::vm::{VM, VMState};
pub use self::writer::*;
pub use self::compiler::{compile, Compiler};
pub use self::stack::*;

/// A stack value.
pub(crate) type Value = i32;

/// An Itsy bytecode program.
#[derive(Debug)]
pub struct Program<T> where T: crate::ExternRust<T> {
    rust_fn: PhantomData<T>,
    pub(crate) instructions: Vec<u8>,
    pub(crate) consts      : Vec<Value>,
}

impl<T> Program<T> where T: crate::ExternRust<T> {
    pub fn new() -> Self {
        Program {
            rust_fn     : PhantomData,
            instructions: Vec::new(),
            consts      : Vec::new(),
        }
    }
}