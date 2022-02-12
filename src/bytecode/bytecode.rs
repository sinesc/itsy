//! Bytecode generation.

#[cfg(feature="compiler")]
pub mod writer;
#[macro_use]
pub mod impl_opcodes;
#[macro_use]
pub mod impl_builtins;
pub mod opcodes;
pub mod builtins;
#[path="compiler/compiler.rs"]
#[cfg(feature="compiler")]
pub mod compiler;
#[path="runtime/runtime.rs"]
pub mod runtime;

use crate::prelude::*;
#[cfg(feature="compiler")]
use writer::{Writer, StoreConst};
use crate::{StackAddress, StackOffset, HeapAddress, HEAP_OFFSET_BITS, RustFnIndex};
use crate::bytecode::runtime::vm::VM;

const ARG1: StackOffset = 0;
const ARG2: StackOffset = 4;
const ARG3: StackOffset = 8;

/// An internal trait used to make resolver, compiler and VM generic over a user-defined set of Rust functions.
/// Use the `vm_func!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMFunc<T>: Debug {
    #[doc(hidden)]
    fn from_index(index: RustFnIndex) -> Self;
    #[doc(hidden)]
    fn into_index(self: Self) -> RustFnIndex;
    #[doc(hidden)]
    fn resolve_info() -> UnorderedMap<&'static str, (RustFnIndex, &'static str, Vec<&'static str>)>;
}

/// An internal trait used to make VM generic over a user-defined data context.
/// Use the `vm_func!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMData<T: VMFunc<T>, U> {
    #[doc(hidden)]
    fn exec(self: Self, vm: &mut VM<T, U>, context: &mut U);
}

/// An Itsy bytecode program. Programs can be created using the bytecode [`Writer`](struct.Writer.html).
#[derive(Clone, Debug)]
pub struct Program<T> {
    rust_fn: PhantomData<T>,
    pub(crate) instructions     : Vec<u8>,
    pub(crate) consts           : Vec<u8>,
    pub(crate) const_descriptors: Vec<ConstDescriptor>,
}

impl<T> Program<T> where T: VMFunc<T> {
    #[cfg(feature="compiler")]
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
#[allow(dead_code)]
pub(crate) enum ConstEndianness {
    None    = 0,
    Integer = 1,
    // architectures may use differing endianesses for floats and integers, so we have to differentiate here
    Float   = 2,
}

#[derive(Clone, Debug)]
pub(crate) struct ConstDescriptor { // todo: order serially, remove position, reduce size to u8 (requires reserve_const_data removal)
    pub(crate) position    : StackAddress,
    pub(crate) size        : StackAddress,
    pub(crate) endianness  : ConstEndianness,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Constructor {
    Primitive   = 174,  // Primitive(num_bytes): copies primitive data
    ArrayDyn    = 176,  // ArrayDyn(element constructor): copies an array, determining element count from prototype or heap object size
    Struct      = 177,  // Struct(num_fields, implementor index, field constructor, field constructor, ...): copies a struct
    String      = 178,  // String: copies a string
}

impl Constructor {
    pub fn from_u8(raw: u8) -> Constructor {
        //un safe { ::std::mem::transmute(raw) }
        match raw {
            x if x == Self::Primitive as u8 => Self::Primitive,
            x if x == Self::ArrayDyn as u8 => Self::ArrayDyn,
            x if x == Self::Struct as u8 => Self::Struct,
            x if x == Self::String as u8 => Self::String,
            index @ _ => unreachable!("Invalid constructor type {}", index),
        }
    }
    pub fn to_u8(self: Self) -> u8 {
        self as u8
    }
}

/// A heap reference as it would appear on the stack
#[derive(Copy, Clone)]
pub struct HeapRef {
    address: HeapAddress,
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
    /*
    /// Returns a HeapSlice of this heap reference with the given length set.
    pub fn to_slice(self: Self, len: StackAddress) -> HeapSlice {
        HeapSlice {
            len     : len,
            heap_ref: self,
        }
    }
    */
    /// Returns the HeapRef as byte array.
    pub fn to_ne_bytes(self: &Self) -> [ u8; size_of::<HeapAddress>() ] {
        self.address.to_ne_bytes()
    }
    /// Creates a HeapRef from byte array.
    pub fn from_ne_bytes(bytes: [ u8; size_of::<HeapAddress>() ]) -> Self {
        HeapRef { address: HeapAddress::from_ne_bytes(bytes) }
    }
    /// Returns the size of heap references.
    pub const fn primitive_size() -> u8 {
        size_of::<Self>() as u8
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

impl PartialEq for HeapRef {
    fn eq(&self, other: &Self) -> bool {
        self.address == other.address
    }
}

impl Debug for HeapRef {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "HeapRef(index:{}, offset:{})", self.index(), self.offset())
    }
}
/*
/// A heap slice as it would appear on the stack.
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
    /// Returns the size of heap reference slices.
    pub const fn primitive_size() -> u8 {
        size_of::<Self>() as u8
    }
}
*/