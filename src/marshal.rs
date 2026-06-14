//! Marshalling support for passing Rust structs and enums across the Itsy API boundary.
//!
//! Host types used in `itsy_api!` function signatures implement the traits in this module, usually
//! via `#[derive(VMValue)]` from the `itsy-derive` crate. The traits split into a compiler-side half
//! ([VMType], used by the resolver to register the type) and a runtime-side half ([VMValue]/[VMField],
//! used by the VM to read arguments and write return values).
//!
//! There are two representations a type can have at the API boundary:
//! - **reference types** (structs, data-carrying enums) live on the heap; their stack representation
//!   is a [HeapRef].
//! - **value types** (primitive/C-like enums) are stored inline; their stack representation is their
//!   discriminant integer.
//!
//! [VMValue::Stack] abstracts over both so the `itsy_api!` macro can marshal any type uniformly.

#[cfg(feature = "runtime")]
use crate::prelude::size_of;
#[cfg(feature = "runtime")]
use crate::{ItemIndex, VariantIndex};
#[cfg(feature = "runtime")]
use crate::bytecode::{HeapRef, HeapRefOp};
#[cfg(feature = "runtime")]
use crate::bytecode::runtime::heap::Heap;

/// Compile-time structural description of an API type, used by the resolver to register the type in
/// the root scope so that scripts and the Rust API agree on its layout.
#[cfg(feature = "compiler")]
#[derive(Clone, Debug)]
pub struct ApiTypeDef {
    /// Itsy type name.
    pub name: &'static str,
    /// Type structure.
    pub kind: ApiTypeKind,
}

/// The structure of an [ApiTypeDef].
#[cfg(feature = "compiler")]
#[derive(Clone, Debug)]
pub enum ApiTypeKind {
    /// A struct with named fields, given as `(field name, field type name)` pairs. Order is irrelevant;
    /// the resolver stores fields in a [BTreeMap](std::collections::BTreeMap) keyed by name, which
    /// determines the heap layout.
    Struct {
        fields: Vec<(&'static str, &'static str)>,
    },
    /// A primitive/C-like enum (no variant carries data), given as `(variant name, discriminant)`
    /// pairs in declaration order. Marshalled as an `i32` discriminant.
    PrimitiveEnum {
        variants: Vec<(&'static str, i64)>,
    },
    /// A data-carrying enum, given as `(variant name, field type names)` pairs in declaration order.
    /// Variant fields are positional and keep their declaration order (unlike struct fields).
    DataEnum {
        variants: Vec<(&'static str, Vec<&'static str>)>,
    },
}

/// Provides the resolver with the Itsy type definition for a Rust type appearing in an API signature.
///
/// Implemented (with the default no-op bodies) for primitives, `String` and `str`; derived for
/// `#[derive(VMValue)]` types.
#[cfg(feature = "compiler")]
pub trait VMType {
    /// The Itsy type name this Rust type maps to.
    const ITSY_NAME: &'static str;
    /// Returns the Itsy type definition for this type, or `None` for built-in types that the resolver
    /// already knows about (primitives, strings).
    fn itsy_type_def() -> Option<ApiTypeDef> {
        None
    }
    /// Appends this type's definition and those of all types reachable through its fields/variants to
    /// `out`, skipping types already present (by name). This computes the transitive set of types that
    /// need to be registered, so a nested type referenced only as a field is still registered.
    fn collect_type_defs(_out: &mut Vec<ApiTypeDef>) {}
}

/// A type that can be marshalled to and from the VM stack as an API argument or return value.
/// Derived for `#[derive(VMValue)]` structs and enums.
#[cfg(feature = "runtime")]
pub trait VMValue: Sized {
    /// The value's representation on the VM stack: a [HeapRef] for reference types (structs,
    /// data-carrying enums), or the discriminant integer for primitive enums.
    type Stack;
    /// Builds the value from its stack representation (reading the heap for reference types).
    fn from_stack(heap: &Heap, raw: Self::Stack) -> Self;
    /// Produces the stack representation of the value (allocating on the heap for reference types).
    /// Reference types are created with a reference count of 0, matching a freshly constructed Itsy value.
    fn to_stack(self, heap: &mut Heap) -> Self::Stack;
    /// Recursively frees any heap object owned by the stack representation if it is no longer
    /// referenced. A no-op for value types.
    fn free_stack(heap: &mut Heap, raw: Self::Stack);
}

/// A value that can appear as a field within a marshalled struct or enum variant. Implemented for
/// primitives and `String`, and derived for structs/enums (where the representation is either inline
/// bytes for value types or a [HeapRef] for reference types).
#[cfg(feature = "runtime")]
pub trait VMField: Sized {
    /// Number of bytes this value occupies inside its containing heap object: the primitive size for
    /// value types, or the size of a [HeapRef] for reference types.
    const HEAP_SIZE: usize;
    /// Reads the field from the containing object `base` at the given byte `offset`.
    fn read_field(heap: &Heap, base: HeapRef, offset: usize) -> Self;
    /// Appends this field's in-object byte representation to `buf`, allocating nested heap objects as
    /// needed for reference-typed fields.
    fn write_field(self, heap: &mut Heap, buf: &mut Vec<u8>);
    /// Recursively frees any heap object reachable through this field. A no-op for value types.
    fn free_field(heap: &mut Heap, base: HeapRef, offset: usize);
}

// --- compiler-side built-in type registrations ---

#[cfg(feature = "compiler")]
macro_rules! impl_vmtype_builtin {
    ($($t:ty => $name:literal),* $(,)?) => {
        $(
            impl VMType for $t {
                const ITSY_NAME: &'static str = $name;
            }
        )*
    };
}

#[cfg(feature = "compiler")]
impl_vmtype_builtin!(
    u8 => "u8", u16 => "u16", u32 => "u32", u64 => "u64",
    i8 => "i8", i16 => "i16", i32 => "i32", i64 => "i64",
    f32 => "f32", f64 => "f64", bool => "bool",
    String => "String", str => "str",
);

// --- runtime-side built-in marshalling ---

/// Returns a byte slice of `len` bytes at `offset` within the heap object referenced by `base`.
#[cfg(feature = "runtime")]
fn field_bytes(heap: &Heap, base: HeapRef, offset: usize, len: usize) -> &[u8] {
    let start = base.offset() as usize + offset;
    &heap.item(base.index()).data[start..start + len]
}

/// Reads a [HeapRef] stored as a field at `offset` within the object referenced by `base`.
#[cfg(feature = "runtime")]
fn read_heap_ref(heap: &Heap, base: HeapRef, offset: usize) -> HeapRef {
    let bytes = field_bytes(heap, base, offset, size_of::<HeapRef>());
    HeapRef::from_ne_bytes(bytes.try_into().unwrap())
}

/// Reads a nested [HeapRef] stored as a reference-typed field at `offset` within the object referenced
/// by `base`. Used by `#[derive(VMValue)]`-generated [VMField] implementations.
#[cfg(feature = "runtime")]
pub fn read_nested_ref(heap: &Heap, base: HeapRef, offset: usize) -> HeapRef {
    read_heap_ref(heap, base, offset)
}

/// The number of bytes occupied by a data-carrying enum's variant tag at the start of its heap object.
#[cfg(feature = "runtime")]
pub const VARIANT_TAG_SIZE: usize = size_of::<VariantIndex>();

/// Appends a data-carrying enum's variant tag (the 0-based variant index) to `buf`.
#[cfg(feature = "runtime")]
pub fn write_variant_tag(buf: &mut Vec<u8>, variant_index: usize) {
    buf.extend_from_slice(&(variant_index as VariantIndex).to_ne_bytes());
}

/// Reads a data-carrying enum's variant tag (the 0-based variant index) from the start of the heap
/// object referenced by `this`.
#[cfg(feature = "runtime")]
pub fn read_variant_tag(heap: &Heap, this: HeapRef) -> usize {
    let bytes = field_bytes(heap, this, 0, VARIANT_TAG_SIZE);
    VariantIndex::from_ne_bytes(bytes.try_into().unwrap()) as usize
}

/// Allocates a fresh heap object (reference count 0) from `buf` and returns a reference to it. Used by
/// `#[derive(VMValue)]`-generated reference-type marshalling.
#[cfg(feature = "runtime")]
pub fn alloc_value(heap: &mut Heap, buf: &[u8]) -> HeapRef {
    HeapRef::new(heap.alloc_copy(buf, ItemIndex::MAX), 0)
}

#[cfg(feature = "runtime")]
macro_rules! impl_vmfield_numeric {
    ($($t:ident),* $(,)?) => {
        $(
            impl VMField for $t {
                const HEAP_SIZE: usize = size_of::<$t>();
                fn read_field(heap: &Heap, base: HeapRef, offset: usize) -> Self {
                    $t::from_ne_bytes(field_bytes(heap, base, offset, Self::HEAP_SIZE).try_into().unwrap())
                }
                fn write_field(self, _heap: &mut Heap, buf: &mut Vec<u8>) {
                    buf.extend_from_slice(&self.to_ne_bytes());
                }
                fn free_field(_heap: &mut Heap, _base: HeapRef, _offset: usize) {}
            }
        )*
    };
}

#[cfg(feature = "runtime")]
impl_vmfield_numeric!(u8, u16, u32, u64, i8, i16, i32, i64, f32, f64);

#[cfg(feature = "runtime")]
impl VMField for bool {
    const HEAP_SIZE: usize = 1;
    fn read_field(heap: &Heap, base: HeapRef, offset: usize) -> Self {
        field_bytes(heap, base, offset, 1)[0] != 0
    }
    fn write_field(self, _heap: &mut Heap, buf: &mut Vec<u8>) {
        buf.push(self as u8);
    }
    fn free_field(_heap: &mut Heap, _base: HeapRef, _offset: usize) {}
}

#[cfg(feature = "runtime")]
impl VMField for String {
    const HEAP_SIZE: usize = size_of::<HeapRef>();
    fn read_field(heap: &Heap, base: HeapRef, offset: usize) -> Self {
        let item = read_heap_ref(heap, base, offset);
        heap.string(item).unwrap().to_string()
    }
    fn write_field(self, heap: &mut Heap, buf: &mut Vec<u8>) {
        let index = heap.alloc_copy(self.as_bytes(), ItemIndex::MAX);
        buf.extend_from_slice(&HeapRef::new(index, 0).to_ne_bytes());
    }
    fn free_field(heap: &mut Heap, base: HeapRef, offset: usize) {
        let item = read_heap_ref(heap, base, offset);
        heap.ref_item(item.index(), HeapRefOp::Free);
    }
}
