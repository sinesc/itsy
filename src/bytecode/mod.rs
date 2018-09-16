//! Bytecode generation and execution.

// todo: remove
#![allow(dead_code)]

mod vm;
mod writer;
#[macro_use]
mod macros;
mod opcodes;
mod compiler;
mod stack;

use std::marker::PhantomData;
pub use self::vm::{VM, VMState};
pub use self::writer::Writer;
pub use self::compiler::{compile, Compiler};
pub use self::stack::*;

/// A stack value.
pub(crate) type Value = i32;

/// An Itsy bytecode program.
#[derive(Debug)]
pub struct Program<T> where T: ::ExternRust<T> {
    rust_fn: PhantomData<T>,
    pub(crate) instructions: Vec<u8>,
    pub(crate) consts      : Vec<Value>,
}

impl<T> Program<T> where T: ::ExternRust<T> {
    pub fn new() -> Self {
        Program {
            rust_fn     : PhantomData,
            instructions: Vec::new(),
            consts      : Vec::new(),
        }
    }
}