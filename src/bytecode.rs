//! Bytecode generation.

mod writer;
mod opcodes;
mod compiler;
mod optimizer;

use std::marker::PhantomData;
pub use self::writer::*;
pub use self::compiler::{compile, error::{CompileError, CompileErrorKind}};
pub use self::opcodes::OpCode;
use crate::util::{StackAddress, StackOffset};

const CALLSIZE: StackOffset = 2 * 4; // previous FP and PC
const ARG1: StackOffset = -CALLSIZE - 1 * 4;
const ARG2: StackOffset = -CALLSIZE - 2 * 4;
const ARG3: StackOffset = -CALLSIZE - 3 * 4;

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub(crate) enum ConstEndianness {
    None    = 0,
    Integer = 1,
    // architectures may use differing endianesses for floats and integers, so we have to differentiate here
    Float   = 2,
}

/* impl ConstEndianness {
    pub fn from_u8(raw: u8) -> ConstEndianness {
        match raw {
            0 => ConstEndianness::None,
            1 => ConstEndianness::Integer,
            2 => ConstEndianness::Float,
            _ => panic!("Invalid ConstEndianess byte: {}", raw),
        }
    }
} */

#[derive(Clone, Debug)]
pub(crate) struct ConstDescriptor {
    pub(crate) position    : StackAddress,
    pub(crate) size        : StackAddress,
    pub(crate) endianness  : ConstEndianness,
}

/// An Itsy bytecode program. Programs can be created using the bytecode [`Writer`](struct.Writer.html).
#[derive(Clone, Debug)]
pub struct Program<T> where T: crate::runtime::VMFunc<T> {
    rust_fn: PhantomData<T>,
    pub(crate) instructions     : Vec<u8>,
    pub(crate) consts           : Vec<u8>,
    pub(crate) const_descriptors: Vec<ConstDescriptor>,
}

impl<T> Program<T> where T: crate::runtime::VMFunc<T> {
    pub(crate) fn new() -> Self {
        Program {
            rust_fn             : PhantomData,
            instructions        : Vec::new(),
            consts              : Vec::new(),
            const_descriptors   : Vec::new(),
        }
    }
}
