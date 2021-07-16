//! Bytecode generation.

pub mod writer;
#[macro_use]
pub mod impl_vm;
pub mod opcodes;
#[path="compiler/compiler.rs"]
pub mod compiler;
#[path="runtime/runtime.rs"]
pub mod runtime;

use std::fmt::Debug;
use std::collections::HashMap;
use std::marker::PhantomData;
use writer::*;
use crate::shared::types::{StackAddress, StackOffset};
use crate::bytecode::runtime::vm::VM;

const CALLSIZE: StackOffset = 2 * 4; // previous FP and PC
const ARG1: StackOffset = -CALLSIZE - 1 * 4;
const ARG2: StackOffset = -CALLSIZE - 2 * 4;
const ARG3: StackOffset = -CALLSIZE - 3 * 4;

/// An internal trait used to make resolver, compiler and VM generic over a user-defined set of Rust functions.
/// Use the `vm_func!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMFunc<T>: Clone + Debug + 'static where T: VMFunc<T> {
    #[doc(hidden)]
    fn from_u16(index: u16) -> Self;
    #[doc(hidden)]
    fn into_u16(self: Self) -> u16;
    #[doc(hidden)]
    fn call_info() -> HashMap<&'static str, (u16, &'static str, Vec<&'static str>)>;
}

/// An internal trait used to make VM generic over a user-defined data context.
/// Use the `vm_func!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMData<T, U> where T: VMFunc<T> {
    #[doc(hidden)]
    fn exec(self: Self, vm: &mut VM<T, U>, context: &mut U);
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub(crate) enum ConstEndianness {
    None    = 0,
    Integer = 1,
    // architectures may use differing endianesses for floats and integers, so we have to differentiate here
    Float   = 2,
}

#[derive(Clone, Debug)]
pub(crate) struct ConstDescriptor {
    pub(crate) position    : StackAddress,
    pub(crate) size        : StackAddress,
    pub(crate) endianness  : ConstEndianness,
}

/// An Itsy bytecode program. Programs can be created using the bytecode [`Writer`](struct.Writer.html).
#[derive(Clone, Debug)]
pub struct Program<T> where T: VMFunc<T> {
    rust_fn: PhantomData<T>,
    pub(crate) instructions     : Vec<u8>,
    pub(crate) consts           : Vec<u8>,
    pub(crate) const_descriptors: Vec<ConstDescriptor>,
}

impl<T> Program<T> where T: VMFunc<T> {
    pub(crate) fn new() -> Self {
        Program {
            rust_fn             : PhantomData,
            instructions        : Vec::new(),
            consts              : Vec::new(),
            const_descriptors   : Vec::new(),
        }
    }
}
