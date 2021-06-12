//! Bytecode generation.

mod writer;
mod opcodes;
mod compiler;
mod optimizer;

use std::marker::PhantomData;
pub use self::writer::*;
pub use self::compiler::{compile, CompileError, CompileErrorKind};
pub use self::opcodes::OpCode;

const CALLSIZE: i32 = 2 * 4; // previous FP and PC
const ARG1: i32 = -CALLSIZE - 1 * 4;
const ARG2: i32 = -CALLSIZE - 2 * 4;
const ARG3: i32 = -CALLSIZE - 3 * 4;

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
    pub(crate) position    : u32,
    pub(crate) size        : u32,
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
