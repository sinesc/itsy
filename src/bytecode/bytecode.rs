//! Bytecode generation.

#[cfg(feature="compiler")]
pub mod writer;
#[path="macros/macros.rs"]
pub mod macros;
pub mod opcodes;
pub mod builtins;
#[path="compiler/compiler.rs"]
#[cfg(feature="compiler")]
pub mod compiler;
#[path="runtime/runtime.rs"]
#[cfg(feature="runtime")]
pub mod runtime;
pub mod marshal;
#[cfg(feature="call_function")]
pub mod call_function;

use crate::prelude::*;
use crate::{StackAddress, StackOffset, HeapAddress, HEAP_OFFSET_BITS, RustFnIndex};
#[cfg(feature="compiler")]
use writer::{Writer, StoreConst};
#[cfg(feature="runtime")]
use crate::{ItemIndex, bytecode::runtime::{vm::VM, stack::{Stack, StackOp}}};
#[cfg(feature="call_function")]
use call_function::FunctionMeta;

/// An internal trait used to make resolver, compiler and VM generic over a user-defined set of Rust functions.
/// Use the `itsy_api!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMFunc<T>: Debug {
    #[doc(hidden)]
    fn from_index(index: RustFnIndex) -> Self;
    #[doc(hidden)]
    #[cfg(feature="compiler")]
    fn to_index(self: Self) -> RustFnIndex;
    #[doc(hidden)]
    #[cfg(feature="compiler")]
    fn resolve_info() -> UnorderedMap<&'static str, (RustFnIndex, Option<crate::internals::marshal::ApiType>, Vec<crate::internals::marshal::ApiType>)>;
    /// Returns the name given to the API in `itsy_api!`. API types are registered under this namespace
    /// (e.g. `MyAPI::MyStruct`), mirroring how API functions are namespaced.
    #[doc(hidden)]
    #[cfg(feature="compiler")]
    fn api_name() -> &'static str;
    /// Returns the transitive set of struct/enum types used in the API's function signatures, so the
    /// resolver can register them in the root scope.
    #[doc(hidden)]
    #[cfg(feature="compiler")]
    fn resolve_types() -> Vec<crate::internals::marshal::ApiTypeDef> { Vec::new() }
    /// Returns the top-level Itsy functions the host declared it will call (`callables { ... }`), as
    /// `(name, return type, argument types)`, so the resolver can verify each is defined in the script
    /// with the declared signature.
    #[doc(hidden)]
    #[cfg(feature="compiler")]
    fn resolve_callables() -> Vec<(&'static str, Option<crate::internals::marshal::ApiType>, Vec<crate::internals::marshal::ApiType>)> { Vec::new() }
}

/// An internal trait used to make VM generic over a user-defined data context.
/// Use the `itsy_api!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMData<T: VMFunc<T>, U> {
    #[doc(hidden)]
    #[cfg(feature="runtime")]
    fn exec(self: Self, vm: &mut VM<T, U>, context: &mut U);
}

/// Const-pool region describing the trait vtable.
/// `base` is the const-pool offset where vtable entries start.
/// `size` is the total vtable size in bytes (always a multiple of `sizeof(StackAddress)`).
#[cfg(feature = "optimizer")]
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct VtableRegion {
    pub(crate) base: StackAddress,
    pub(crate) size: StackAddress,
}

/// An Itsy bytecode program. Programs can be created using the [compile](compiler::compile) function or the bytecode [Writer] and can
/// be executed by [run](crate::run) or [VM::run](crate::runtime::VM::run).
#[derive(Clone, Debug)]
pub struct Program<T> {
    rust_fn: PhantomData<T>,
    pub(crate) instructions     : Vec<u8>,
    pub(crate) consts           : Vec<u8>,
    pub(crate) const_descriptors: Vec<ConstDescriptor>,
    /// Table of host-callable top-level functions, keyed by name.
    #[cfg(feature="call_function")]
    pub(crate) functions        : Map<String, FunctionMeta>,
    /// Address of a halt instruction used as the return target for host-initiated function calls.
    #[cfg(feature="call_function")]
    pub(crate) host_return_addr : StackAddress,
    /// Const-pool region of the trait vtable. `None` means the program has no trait objects.
    /// Used by the optimizer to remap stale function addresses after instruction compaction.
    #[cfg(feature = "optimizer")]
    pub(crate) vtable_region: Option<VtableRegion>,
}

/// Consume bytes from the front of a slice and return them.
// TODO: once stabilized, nightly take() could replace this
pub(crate) fn read<'a>(slice: &mut &'a[ u8 ], num_bytes: usize) -> Option<&'a[ u8 ]> {
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
            #[cfg(feature="call_function")]
            functions           : Map::new(),
            #[cfg(feature="call_function")]
            host_return_addr    : 0,
            #[cfg(feature = "optimizer")]
            vtable_region       : None,
        }
    }
    /// Serializes the program to a byte vector, e.g. to be saved to a file.
    pub fn to_bytes(self: &Self) -> Vec<u8> {
        let mut result = Vec::new();
        result.extend_from_slice("itsy".as_bytes());
        // save instructions
        result.extend_from_slice(&self.instructions.len().to_ne_bytes()[..]);
        result.extend_from_slice(&self.instructions[..]);
        // save constants
        result.extend_from_slice(&self.consts.len().to_ne_bytes()[..]);
        result.extend_from_slice(&self.consts[..]);
        // save constant descriptors
        result.extend_from_slice(&self.const_descriptors.len().to_ne_bytes()[..]);
        for descriptor in &self.const_descriptors {
            result.extend_from_slice(&descriptor.to_bytes()[..]);
        }
        // save host-call return address and the callable function table
        #[cfg(feature="call_function")]
        call_function::serialize_function_table(self.host_return_addr, &self.functions, &mut result);
        result
    }

    /// Returns a reference to the raw instruction stream.
    /// Used by optimizer tests to inspect generated bytecode.
    #[cfg(feature = "optimizer")]
    pub fn instructions(&self) -> &[u8] {
        &self.instructions
    }

    /// Deserializes a program from a byte vector, returning the programm on succcess or None if the input data is not a valid program.
    pub fn from_bytes(mut program: &[ u8 ]) -> Option<Program<T>> {
        const USIZE: usize = size_of::<usize>();
        // verify header
        if read(&mut program, 4)? != "itsy".as_bytes() {
            return None;
        }
        // read instructions
        let instructions_size: usize = usize::from_ne_bytes(read(&mut program, USIZE)?.try_into().ok()?);
        let instructions: Vec<u8> = read(&mut program, instructions_size)?.into();
        // read constants
        let consts_size: usize = usize::from_ne_bytes(read(&mut program, USIZE)?.try_into().ok()?);
        let consts: Vec<u8> = read(&mut program, consts_size)?.into();
        // read descriptors
        let const_descriptors_size: usize = usize::from_ne_bytes(read(&mut program, USIZE)?.try_into().ok()?);
        let mut const_descriptors: Vec<ConstDescriptor> = Vec::new();
        for _ in 0..const_descriptors_size {
            const_descriptors.push(ConstDescriptor::from_bytes(read(&mut program, ConstDescriptor::SERIALIZED_SIZE)?)?);
        }
        // read host-call return address and the callable function table
        #[cfg(feature="call_function")]
        let (host_return_addr, functions) = call_function::deserialize_function_table(&mut program)?;
        Some(Self {
            rust_fn: PhantomData,
            instructions,
            consts,
            const_descriptors,
            #[cfg(feature="call_function")]
            functions,
            #[cfg(feature="call_function")]
            host_return_addr,
            #[cfg(feature = "optimizer")]
            vtable_region: None,
        })
    }
}


/// Allowed heap reference counting operations.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum HeapRefOp {
    /// Increase reference counter.
    Inc,
    /// Decrease reference counter, free on zero.
    Dec,
    /// Decrease reference counter but do not free on zero.
    DecNoFree,
    /// Free if reference counter is 0, otherwise do nothing.
    Free,
}

impl HeapRefOp {
    #[cfg(feature="runtime")]
    pub(crate) fn from_u8(index: u8) -> Self {
        match index {
            x if x == Self::Inc as u8 => Self::Inc,
            x if x == Self::Dec as u8 => Self::Dec,
            x if x == Self::Free as u8 => Self::Free,
            x if x == Self::DecNoFree as u8 => Self::DecNoFree,
            _ => panic!("Invalid HeapRefOp index {}.", index),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ConstDescriptor {
    pub(crate) size: StackAddress,
}

impl ConstDescriptor {
    // Descriptors describe consecutive blocks of the const pool in write order, so each block's
    // position is the running sum of all preceding sizes and does not need to be stored.
    const SERIALIZED_SIZE: usize = 4;
    fn to_bytes(self: &Self) -> [ u8; Self::SERIALIZED_SIZE ] {
        (self.size as u32).to_ne_bytes()
    }
    fn from_bytes(mut descriptor: &[ u8 ]) -> Option<Self> {
        const U32: usize = size_of::<u32>();
        let size = u32::from_ne_bytes(read(&mut descriptor, U32)?.try_into().ok()?) as StackAddress;
        Some(Self { size })
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Constructor {
    Primitive   = 174,  // Primitive(num_bytes): copies primitive data
    Virtual     = 175,  // Virtual: a nested trait-object reference whose concrete constructor is resolved at runtime via the referenced heap object's implementor index
    Array       = 176,  // Array(constructor_size, element constructor): copies an array, determining element count from prototype or heap object size
    Struct      = 177,  // Struct(constructor_size, implementor index, num_fields, field constructor, field constructor, ...): copies a struct
    String      = 178,  // String: copies a string
    Enum        = 179,  // Enum(constructor_size, implementor index, num_variants, variant 1 num_fields, field constructor, ..., variant 2 num_fields, ...): copies an enum
    Closure     = 181,
    Map         = 182,  // Map(constructor_size, key constructor, value constructor): copies a map. Keys and values are boxed, i.e. each stored inline as a HeapRef.
    Generator   = 183,  // Generator(value_ctor, key_ctor): an opaque generator carrier. The two StackAddress args are the value/key constructor offsets (GEN_PRIMITIVE_CTOR = no ref slot); they release the yielded value/key held in the header. The frozen frame's live refs are released via a separate live-ref-map referenced from that header (see runtime::vm).
}

/// Sentinel stored in a `Constructor::Generator` value/key constructor slot (and passed to `gen_yield`/
/// `gen_yield_kv`/`gen_return`) to indicate the slot holds a primitive, not a heap reference, and must
/// not be refcounted. Distinct from constructor offset 0 (trait/virtual) and 1 (dynamic/callable).
pub const GEN_PRIMITIVE_CTOR: StackAddress = StackAddress::MAX;

/// Information about a serialized constructor
#[cfg(feature="runtime")]
pub struct ConstructorData {
    /// Offset to construction parameters (after size-info and operation)
    pub offset: StackAddress,
    /// Address of the next constructor
    pub next: StackAddress,
}

#[cfg(feature="compiler")]
impl Constructor {
    /// Returns u8 representation of this constructor.
    pub fn to_u8(self: Self) -> u8 {
        self as u8
    }
}

#[cfg(feature="runtime")]
impl Constructor {
    /// Creates a constructor instance from given u8.
    pub fn from_u8(raw: u8) -> Constructor {
        //un safe { ::std::mem::transmute(raw) }
        match raw {
            x if x == Self::Primitive as u8 => Self::Primitive,
            x if x == Self::Virtual as u8 => Self::Virtual,
            x if x == Self::Array as u8 => Self::Array,
            x if x == Self::Struct as u8 => Self::Struct,
            x if x == Self::String as u8 => Self::String,
            x if x == Self::Enum as u8 => Self::Enum,
            x if x == Self::Closure as u8 => Self::Closure,
            x if x == Self::Map as u8 => Self::Map,
            x if x == Self::Generator as u8 => Self::Generator,
            index @ _ => panic!("Invalid constructor type {}.", index),
        }
    }
    // Parses serialized constructor parameters with given constructor op.
    pub fn parse_with(stack: &Stack, mut offset: StackAddress, constructor: Constructor) -> ConstructorData {
        match constructor {
            Self::Array | Self::Struct | Self::Enum | Self::Map => {
                // first value is the total constructor size
                let constructor_size: ItemIndex = stack.load(offset);
                offset += size_of_val(&constructor_size) as StackAddress;
                ConstructorData {
                    offset,
                    next: offset + constructor_size as StackAddress,
                }
            },
            Self::Primitive => {
                // constructor size is size of one ItemIndex
                const SIZE: StackAddress = size_of::<ItemIndex>() as StackAddress;
                ConstructorData {
                    offset,
                    next: offset + SIZE,
                }
            },
            Self::String | Self::Closure | Self::Virtual => {
                // no additional data attached: a virtual constructor is resolved at runtime via the
                // referenced object's implementor index, strings/closures carry no nested layout
                ConstructorData {
                    offset,
                    next: offset,
                }
            },
            Self::Generator => {
                // two StackAddress args: the value- and key-constructor offsets (0 = primitive/none),
                // used to release the yielded value/key held in the carrier header on drop. The frozen
                // frame's own live refs are described by a separate map referenced from that header.
                const SIZE: StackAddress = (2 * size_of::<StackAddress>()) as StackAddress;
                ConstructorData {
                    offset,
                    next: offset + SIZE,
                }
            },
        }
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
    /// Reads a constructor opcode from the stack at the given offset, advancing the offset past it.
    pub fn read_op(stack: &Stack, offset: &mut StackAddress) -> Self {
        let op = Self::from_u8(stack.load(*offset));
        *offset += size_of_val(&op) as StackAddress;
        op
    }
    /// Reads an ItemIndex-sized constructor argument from the stack, advancing the offset past it.
    pub fn read_index(stack: &Stack, offset: &mut StackAddress) -> ItemIndex {
        let arg: ItemIndex = stack.load(*offset);
        *offset += size_of_val(&arg) as StackAddress;
        arg
    }
    /// Given the offset of a serialized constructor, returns the offset immediately following it.
    pub fn skip(stack: &Stack, mut offset: StackAddress) -> StackAddress {
        let op = Self::from_u8(stack.load(offset));
        offset += size_of_val(&op) as StackAddress;
        Self::parse_with(stack, offset, op).next
    }
    /// Splits a map's constructor offset into its key and value sub-constructor offsets.
    pub fn map_sub_constructors(stack: &Stack, mut offset: StackAddress) -> (StackAddress, StackAddress) {
        let op = Self::from_u8(stack.load(offset));
        debug_assert_eq!(op, Self::Map);
        offset += size_of_val(&op) as StackAddress;
        let parsed = Self::parse_with(stack, offset, op);
        let key_ctor = parsed.offset;
        let value_ctor = Self::skip(stack, key_ctor);
        (key_ctor, value_ctor)
    }
    /// Returns whether the constructor at the given offset is a (boxed) primitive.
    pub fn is_primitive(stack: &Stack, ctor_offset: StackAddress) -> bool {
        let op = Self::from_u8(stack.load(ctor_offset));
        op == Self::Primitive
    }
    /// Returns the byte size of a (boxed) primitive described by the constructor at the given offset.
    pub fn primitive_size(stack: &Stack, mut ctor_offset: StackAddress) -> usize {
        let op = Self::from_u8(stack.load(ctor_offset));
        debug_assert_eq!(op, Self::Primitive);
        ctor_offset += size_of_val(&op) as StackAddress;
        let index: ItemIndex = stack.load(ctor_offset);
        index as usize
    }
}

/// A heap reference as it would appear on the stack
#[derive(Copy, Clone, PartialOrd)]
pub struct HeapRef {
    address: HeapAddress,
}

impl HeapRef {
    /// Mask for extracting the index of HeapRef.
    const INDEX_MASK: HeapAddress = !((1 << HEAP_OFFSET_BITS) - 1);

    /// Mask for extracting the offset of HeapRef.
    const OFFSET_MASK: HeapAddress = (1 << HEAP_OFFSET_BITS) - 1;

    /// The largest index a HeapRef can represent. Usable as a sentinel since it can never be a valid
    /// heap object index (it round-trips through `new`/`index`, unlike `StackAddress::MAX`).
    pub const MAX_INDEX: StackAddress = (HeapRef::INDEX_MASK >> HEAP_OFFSET_BITS) as StackAddress;

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
    pub fn add_offset(self: &mut Self, offset: StackOffset) {
        debug_assert!((self.offset() as StackOffset + offset) as HeapAddress <= (1 << HEAP_OFFSET_BITS) - 1);
        let new_offset = self.offset() as StackOffset + offset;
        let index = self.index();
        self.address = ((index as HeapAddress) << HEAP_OFFSET_BITS) | (new_offset as HeapAddress)
    }
    /// Returns a clone of this heap reference offset by the given value.
    pub fn with_offset(self: Self, offset: StackOffset) -> Self {
        debug_assert!((self.offset() as StackOffset + offset) as HeapAddress <= (1 << HEAP_OFFSET_BITS) - 1);
        HeapRef::new(self.index(), (self.offset() as StackOffset + offset) as StackAddress)
    }
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