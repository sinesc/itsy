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
use crate::{StackAddress, StackOffset, HeapAddress, HEAP_OFFSET_BITS, ItemIndex, RustFnIndex};
use crate::bytecode::runtime::{vm::VM, stack::{Stack, StackOp}};

const ARG1: StackOffset = 0;
const ARG2: StackOffset = 4;
const ARG3: StackOffset = 8;

/// An internal trait used to make resolver, compiler and VM generic over a user-defined set of Rust functions.
/// Use the `itsy_api!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMFunc<T>: Debug {
    #[doc(hidden)]
    fn from_index(index: RustFnIndex) -> Self;
    #[doc(hidden)]
    fn into_index(self: Self) -> RustFnIndex;
    #[doc(hidden)]
    fn resolve_info() -> UnorderedMap<&'static str, (RustFnIndex, &'static str, Vec<&'static str>)>;
}

/// An internal trait used to make VM generic over a user-defined data context.
/// Use the `itsy_api!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMData<T: VMFunc<T>, U> {
    #[doc(hidden)]
    fn exec(self: Self, vm: &mut VM<T, U>, context: &mut U);
}

/// An Itsy bytecode program. Programs can be created using the [compile](compiler::compile) function or the bytecode [Writer] and can
/// be executed by [run](crate::run) or [VM::run](crate::VM::run).
#[derive(Clone, Debug)]
pub struct Program<T> {
    rust_fn: PhantomData<T>,
    pub(crate) instructions     : Vec<u8>,
    pub(crate) consts           : Vec<u8>,
    pub(crate) const_descriptors: Vec<ConstDescriptor>,
}

/// Consume bytes from the front of a slice and return them.
// TODO: once stabilized, nightly take() could replace this
fn read<'a>(slice: &mut &'a[ u8 ], num_bytes: usize) -> Option<&'a[ u8 ]> {
    if slice.len() < num_bytes {
        None
    } else {
        let result = &slice[0..num_bytes];
        *slice = &slice[num_bytes..];
        Some(result)
    }
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
    /// Serializes the program to a byte vector, e.g. to be saved to a file.
    pub fn to_bytes(self: &Self) -> Vec<u8> {
        let mut result = Vec::new();
        result.extend_from_slice("itsy".as_bytes());
        // save instructions
        result.extend_from_slice(&self.instructions.len().to_le_bytes()[..]);
        result.extend_from_slice(&self.instructions[..]);
        // save constants
        result.extend_from_slice(&self.consts.len().to_le_bytes()[..]);
        result.extend_from_slice(&self.consts[..]);
        // save constant descriptors
        result.extend_from_slice(&self.const_descriptors.len().to_le_bytes()[..]);
        for descriptor in &self.const_descriptors {
            result.extend_from_slice(&descriptor.to_bytes()[..]);
        }
        result
    }
    /// Deserializes a program from a byte vector, returning the programm on succcess or None if the input data is not a valid program.
    pub fn from_bytes(mut program: &[ u8 ]) -> Option<Program<T>> {
        const USIZE: usize = size_of::<usize>();
        // verify header
        if read(&mut program, 4)? != "itsy".as_bytes() {
            return None;
        }
        // read instructions
        let instructions_size: usize = usize::from_le_bytes(read(&mut program, USIZE)?.try_into().ok()?);
        let instructions: Vec<u8> = read(&mut program, instructions_size)?.into();
        // read constants
        let consts_size: usize = usize::from_le_bytes(read(&mut program, USIZE)?.try_into().ok()?);
        let consts: Vec<u8> = read(&mut program, consts_size)?.into();
        // read descriptors
        let const_descriptors_size: usize = usize::from_le_bytes(read(&mut program, USIZE)?.try_into().ok()?);
        let mut const_descriptors: Vec<ConstDescriptor> = Vec::new();
        for _ in 0..const_descriptors_size {
            const_descriptors.push(ConstDescriptor::from_bytes(read(&mut program, ConstDescriptor::SERIALIZED_SIZE)?)?);
        }
        Some(Self {
            rust_fn: PhantomData,
            instructions,
            consts,
            const_descriptors,
        })
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

impl ConstEndianness {
    pub(crate) fn from_u8(index: u8) -> Option<Self> {
        match index {
            x if x == Self::None as u8 => Some(Self::None),
            x if x == Self::Integer as u8 => Some(Self::Integer),
            x if x == Self::Float as u8 => Some(Self::Float),
            _ => None
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ConstDescriptor { // todo: order serially, remove position, reduce size to u8 (requires reserve_const_data removal)
    pub(crate) position    : StackAddress,
    pub(crate) size        : StackAddress,
    pub(crate) endianness  : ConstEndianness,
}

impl ConstDescriptor {
    const SERIALIZED_SIZE: usize = 9;
    fn to_bytes(self: &Self) -> [ u8; Self::SERIALIZED_SIZE ] {
        let mut result = [ 0u8; Self::SERIALIZED_SIZE ];
        let position = (self.position as u32).to_le_bytes();
        let size = (self.size as u32).to_le_bytes();
        // whoa clunky, can't copy to specific position into an array?
        let position_slice = &mut result[0..4];
        position_slice.copy_from_slice(&position);
        let size_slice = &mut result[4..8];
        size_slice.copy_from_slice(&size);
        result[8] = self.endianness as u8;
        result
    }
    fn from_bytes(mut descriptor: &[ u8 ]) -> Option<Self> {
        const U32: usize = size_of::<u32>();
        let position = u32::from_le_bytes(read(&mut descriptor, U32)?.try_into().ok()?) as usize;
        let size = u32::from_le_bytes(read(&mut descriptor, U32)?.try_into().ok()?) as usize;
        let endianness = ConstEndianness::from_u8(read(&mut descriptor, 1)?[0])?;
        Some(Self {
            position,
            size,
            endianness,
        })
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Constructor {
    Primitive   = 174,  // Primitive(num_bytes): copies primitive data
    Array       = 176,  // Array(constructor_size, element constructor): copies an array, determining element count from prototype or heap object size
    Struct      = 177,  // Struct(constructor_size, implementor index, num_fields, field constructor, field constructor, ...): copies a struct
    String      = 178,  // String: copies a string
    Enum        = 179,  // Enum(constructor_size, implementor index, num_variants, variant 1 num_fields, field constructor, ..., variant 2 num_fields, ...): copies an enum
}

/// Information about a serialized constructor
pub struct ConstructorData {
    /// Construction operation
    pub op: Constructor,
    /// Offset to construction parameters (after size-info and operation)
    pub offset: StackAddress,
    /// Address of the next constructor
    pub next: StackAddress,
}

impl Constructor {
    pub fn from_u8(raw: u8) -> Constructor {
        //un safe { ::std::mem::transmute(raw) }
        match raw {
            x if x == Self::Primitive as u8 => Self::Primitive,
            x if x == Self::Array as u8 => Self::Array,
            x if x == Self::Struct as u8 => Self::Struct,
            x if x == Self::String as u8 => Self::String,
            x if x == Self::Enum as u8 => Self::Enum,
            index @ _ => unreachable!("Invalid constructor type {}", index),
        }
    }
    pub fn to_u8(self: Self) -> u8 {
        self as u8
    }
    // Parses a constructor.
    pub fn parse(stack: &Stack, mut offset: StackAddress) -> ConstructorData {
        let constructor = Constructor::from_u8(stack.load(offset));
        offset += size_of_val(&constructor) as StackAddress;
        Self::parse_with(stack, offset, constructor)
    }
    // Parses serialized constructor parameters with given constructor op.
    pub fn parse_with(stack: &Stack, mut offset: StackAddress, constructor: Constructor) -> ConstructorData {
        match constructor {
            Constructor::Array | Constructor::Struct | Constructor::Enum => {
                // first value is the total constructor size
                let constructor_size: ItemIndex = stack.load(offset);
                offset += size_of_val(&constructor_size) as StackAddress;
                ConstructorData {
                    op: constructor,
                    offset,
                    next: offset + constructor_size as StackAddress,
                }
            },
            Constructor::Primitive => {
                // constructor size is size of one ItemIndex
                const SIZE: StackAddress = size_of::<ItemIndex>() as StackAddress;
                ConstructorData {
                    op: constructor,
                    offset,
                    next: offset + SIZE,
                }
            },
            Constructor::String => {
                // no additional data attached to string
                ConstructorData {
                    op: constructor,
                    offset,
                    next: offset,
                }
            },
        }
    }
    // Parses primitive constructor parameters.
    pub fn parse_primitive(stack: &Stack, offset: StackAddress) -> ItemIndex {
        stack.load(offset)
    }
    // Parses struct constructor parameters.
    pub fn parse_struct(stack: &Stack, mut offset: StackAddress) -> (ItemIndex, ItemIndex, StackAddress) {
        let implementor_index: ItemIndex = stack.load(offset);
        offset += size_of_val(&implementor_index) as StackAddress;
        let num_fields: ItemIndex = stack.load(offset);
        (implementor_index, num_fields, offset + size_of_val(&implementor_index) as StackAddress)
    }
    // Parses an enum variant table and returns the number of fields and the offset of the given variant.
    pub fn parse_variant_table(stack: &Stack, mut offset: StackAddress, variant_index: ItemIndex) -> (ItemIndex, StackAddress) {
        // load variant offset from index
        offset += (size_of::<StackAddress>() * variant_index as usize) as StackAddress;
        // jump to variant offset
        offset = stack.load(offset);
        // read and construct fields for the variant
        let num_fields = stack.load(offset);
        (num_fields, offset + size_of_val(&num_fields) as StackAddress)
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

    /// Mask for extracting the offset of HeapRef.
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