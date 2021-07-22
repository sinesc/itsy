//! Bytecode generation.

pub mod writer;
#[macro_use]
pub mod impl_vm;
pub mod opcodes;
#[path="compiler/compiler.rs"]
pub mod compiler;
#[path="runtime/runtime.rs"]
pub mod runtime;

use std::fmt::{self, Debug};
use std::collections::HashMap;
use std::marker::PhantomData;
use writer::{Writer, StoreConst};
use crate::{StackAddress, StackOffset, HeapAddress, HEAP_OFFSET_BITS};
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

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Constructor {
    Primitive   = 174,  // Primitive(num_bytes): copies primitive data
    Array       = 175,  // Array(num_elements, element constructor): copies an array
    Struct      = 176,  // Struct(num_fields, field constructor, field constructor, ...): copies a struct
    String      = 177,  // String: copies a string
}

impl Constructor {
    pub fn from_u8(raw: u8) -> Constructor {
        //un safe { ::std::mem::trans mute(raw) }
        match raw {
            x if x == Self::Primitive as u8 => Self::Primitive,
            x if x == Self::Array as u8 => Self::Array,
            x if x == Self::Struct as u8 => Self::Struct,
            x if x == Self::String as u8 => Self::String,
            index @ _ => unreachable!("Invalid constructor type {}", index),
        }
    }
}

/// A heap reference as it would appear on the stack
#[derive(Copy, Clone)]
pub struct HeapRef {
    pub(crate) address: HeapAddress,
}

impl HeapRef {
    /// Mask for extracting the index of HeapRef.
    const INDEX_MASK: HeapAddress = !((1 << HEAP_OFFSET_BITS) - 1);

    /// Mask for extracting the offset of HeapRef
    const OFFSET_MASK: HeapAddress = (1 << HEAP_OFFSET_BITS) - 1;

    /// Creates a new HeapRef.
    pub fn new(index: StackAddress, offset: StackAddress) -> HeapRef {
        Self { address: ((index as HeapAddress) << HEAP_OFFSET_BITS) | (offset as HeapAddress) }
    }
    /// Returns the index of this HeapRef.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn index(self: &Self) -> StackAddress {
        ((self.address & HeapRef::INDEX_MASK) >> HEAP_OFFSET_BITS) as StackAddress
    }
    /// Returns the offset of this HeapRef.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn offset(self: &Self) -> StackAddress {
        (self.address & HeapRef::OFFSET_MASK) as StackAddress
    }
    /// Adds given offset to this HeapRef.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn add_offset(self: &mut Self, offset: StackOffset) {
        debug_assert!((self.offset() as StackOffset + offset) as HeapAddress <= (1 << HEAP_OFFSET_BITS) - 1);
        let new_offset = self.offset() as StackOffset + offset;
        let index = self.index();
        self.address = ((index as HeapAddress) << HEAP_OFFSET_BITS) | (new_offset as HeapAddress)
    }
    /// Returns a clone of this heap reference offset by the given value.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn with_offset(self: Self, offset: StackOffset) -> Self {
        debug_assert!((self.offset() as StackOffset + offset) as HeapAddress <= (1 << HEAP_OFFSET_BITS) - 1);
        HeapRef::new(self.index(), (self.offset() as StackOffset + offset) as StackAddress)
    }
    /// Returns a HeapSlice of this heap reference with the given length set.
    pub fn to_slice(self: Self, len: StackAddress) -> HeapSlice {
        HeapSlice {
            len     : len,
            heap_ref: self,
        }
    }
    /// Returns the HeapRef as byte array.
    pub fn to_ne_bytes(self: &Self) -> [ u8; ::std::mem::size_of::<HeapAddress>() ] {
        self.address.to_ne_bytes()
    }
    /// Creates a HeapRef from byte array.
    pub fn from_ne_bytes(bytes: [ u8; ::std::mem::size_of::<HeapAddress>() ]) -> Self {
        HeapRef { address: HeapAddress::from_ne_bytes(bytes) }
    }
    /// Returns the size of heap references.
    pub const fn primitive_size() -> u8 {
        std::mem::size_of::<Self>() as u8
    }
}

impl From<HeapRef> for (StackAddress, StackAddress) {
    fn from(heap_ref: HeapRef) -> (StackAddress, StackAddress) {
        (
            ((heap_ref.address & HeapRef::INDEX_MASK) >> HEAP_OFFSET_BITS) as StackAddress,
            (heap_ref.address & HeapRef::OFFSET_MASK) as StackAddress
        )
    }
}

impl Debug for HeapRef {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "HeapRef(index:{}, offset:{})", self.index(), self.offset())
    }
}

/// A heap slice as it would appear on the stack
#[derive(Debug, Copy, Clone)]
pub struct HeapSlice {
    pub len     : StackAddress,
    pub heap_ref: HeapRef,
}

impl HeapSlice {
    /// Returns a clone of this heap reference offset by the given value.
    pub fn with_offset(self: Self, offset: StackOffset) -> Self {
        Self {
            heap_ref: self.heap_ref.with_offset(offset),
            len     : self.len,
        }
    }
    /// Returns a HeapSlice of this heap reference with the given length set.
    pub fn to_ref(self: Self) -> HeapRef {
        self.heap_ref
    }
    /// Returns the size of heap references.
    pub fn size() -> u8 {
        std::mem::size_of::<Self>() as u8
    }
}