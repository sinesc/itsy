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

use crate::prelude::*;
use crate::config::FrameAddress;
use crate::StackAddress;
#[cfg(feature = "runtime")]
use crate::prelude::size_of;
#[cfg(feature = "runtime")]
use crate::{ItemIndex, VariantIndex};
#[cfg(feature = "runtime")]
use crate::bytecode::{HeapRef, HeapRefOp};
#[cfg(feature = "runtime")]
use crate::bytecode::runtime::heap::Heap;
#[cfg(feature = "runtime")]
use crate::bytecode::runtime::stack::{Stack, StackOp};
#[cfg(feature = "compiler")]
use crate::bytecode::{VMFunc, read};
#[cfg(feature = "compiler")]
use crate::shared::MetaContainer;
#[cfg(feature = "compiler")]
use crate::frontend::{ast, parser::types::ParsedModule};
#[cfg(feature = "compiler")]
use crate::bytecode::compiler::{Compiler, error::{CompileResult, OptionToCompileError}};

/// Metadata describing a top-level Itsy function, allowing the host to invoke it by name via
/// [`VM::call_typed`](crate::runtime::VM::call_typed).
#[derive(Clone, Debug)]
pub(crate) struct FunctionMeta {
    /// Bytecode address of the function's entry point.
    pub(crate) addr: StackAddress,
    /// Combined primitive size of all arguments (their on-stack footprint).
    pub(crate) arg_size: FrameAddress,
    /// Number of arguments (for argument count validation during host calls).
    pub(crate) args_count: usize,
}

/// Serializes the host-call return address and callable function table.
pub(crate) fn serialize_function_table(host_return_addr: StackAddress, functions: &Map<String, FunctionMeta>, result: &mut Vec<u8>) {
    result.extend_from_slice(&host_return_addr.to_ne_bytes()[..]);
    result.extend_from_slice(&functions.len().to_ne_bytes()[..]);
    for (name, meta) in functions {
        result.extend_from_slice(&name.len().to_ne_bytes()[..]);
        result.extend_from_slice(name.as_bytes());
        result.extend_from_slice(&meta.addr.to_ne_bytes()[..]);
        result.extend_from_slice(&meta.arg_size.to_ne_bytes()[..]);
        result.extend_from_slice(&meta.args_count.to_ne_bytes()[..]);
    }
}

/// Deserializes the host-call return address and callable function table.
pub(crate) fn deserialize_function_table(program: &mut &[u8]) -> Option<(StackAddress, Map<String, FunctionMeta>)> {
    const USIZE: usize = size_of::<usize>();
    const SA: usize = size_of::<StackAddress>();
    const FA: usize = size_of::<FrameAddress>();
    let host_return_addr = StackAddress::from_ne_bytes(read(program, SA)?.try_into().ok()?);
    let functions_count: usize = usize::from_ne_bytes(read(program, USIZE)?.try_into().ok()?);
    let mut functions: Map<String, FunctionMeta> = Map::new();
    for _ in 0..functions_count {
        let name_len: usize = usize::from_ne_bytes(read(program, USIZE)?.try_into().ok()?);
        let name = String::from_utf8(read(program, name_len)?.to_vec()).ok()?;
        let addr = StackAddress::from_ne_bytes(read(program, SA)?.try_into().ok()?);
        let arg_size = FrameAddress::from_ne_bytes(read(program, FA)?.try_into().ok()?);
        let args_count: usize = usize::from_ne_bytes(read(program, USIZE)?.try_into().ok()?);
        functions.insert(name, FunctionMeta { addr, arg_size, args_count });
    }
    Some((host_return_addr, functions))
}

/// Builds the host-callable function table from all top-level (free) functions in `modules`
/// so a host can invoke them by name via [`VM::call_typed`](crate::runtime::VM::call_typed).
/// Methods/trait functions/generators are not top-level and are intentionally excluded.
pub(crate) fn build_function_table<T>(compiler: &Compiler<T>, modules: &[ParsedModule]) -> CompileResult<Map<String, FunctionMeta>> where T: VMFunc<T>,
{
    let mut functions = Map::new();
    for module in modules {
        for statement in module.statements() {
            if let ast::Statement::Function(function) = statement {
                let function_id = compiler.constant_by_id(function.constant_id.ice()?).value.as_function_id().ice()?;
                let addr = compiler.functions.get(function_id).ice_msg("Missing function address")?;
                let func = compiler.function_by_id(function_id);
                let arg_size = func.arg_size(compiler);
                let args_count = func.arg_type_ids(compiler).len();
                functions.insert(function.shared.sig.ident.name.clone(), FunctionMeta { addr, arg_size, args_count });
            }
        }
    }
    Ok(functions)
}

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
    /// pairs in declaration order. `discriminant_type` is the Itsy primitive name (e.g. `"i8"`, `"u16"`).
    PrimitiveEnum {
        variants: Vec<(&'static str, i64)>,
        discriminant_type: &'static str,
    },
    /// A data-carrying enum, given as `(variant name, field type names)` pairs in declaration order.
    /// Variant fields are positional and keep their declaration order (unlike struct fields).
    DataEnum {
        variants: Vec<(&'static str, Vec<&'static str>)>,
    },
}

/// Structural description of a type appearing in an API function's argument or return position. Unlike
/// [ApiTypeDef] (which describes a named struct/enum to register), this describes how to *refer* to a
/// type, supporting arrays (`[ T ]`) which are anonymous and resolved structurally by the resolver.
#[cfg(feature = "compiler")]
#[derive(Clone, Debug)]
pub enum ApiType {
    /// A named type, referenced by its Itsy name (a primitive, `String`, or a registered struct/enum).
    Name(&'static str),
    /// An array type with the given element type, corresponding to Itsy `[ element ]`.
    Array(Box<ApiType>),
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
    /// Recursively increments the reference count of the stack representation and every heap object
    /// reachable through it to 1. A no-op for value types.
    ///
    /// Used to marshal a value as an argument *into* a host-initiated call: `to_stack` builds the whole
    /// structure at reference count 0 (the return-value convention, where the consuming script retains
    /// it), but a reference-typed *parameter* is released by the callee's epilogue via a recursive
    /// decrement, so every reference in the structure must arrive at count 1 to balance. This mirrors
    /// [free_stack](Self::free_stack) with an increment.
    fn retain_stack(heap: &mut Heap, raw: Self::Stack);
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
    /// Recursively increments to 1 the reference count of any heap object reachable through this field.
    /// A no-op for value types. Mirror of [free_field](Self::free_field); see
    /// [VMValue::retain_stack].
    fn retain_field(heap: &mut Heap, base: HeapRef, offset: usize);
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

/// An array maps to an anonymous Itsy `[ element ]` type. It is never registered as a named type (the
/// resolver creates it structurally from the [ApiType] descriptor), so `ITSY_NAME` is a placeholder and
/// `itsy_type_def` yields nothing; `collect_type_defs` only forwards to the element type so that custom
/// element types still get registered.
#[cfg(feature = "compiler")]
impl<T: VMType> VMType for Vec<T> {
    const ITSY_NAME: &'static str = "[ ]";
    fn collect_type_defs(out: &mut Vec<ApiTypeDef>) {
        T::collect_type_defs(out);
    }
}

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
                fn retain_field(_heap: &mut Heap, _base: HeapRef, _offset: usize) {}
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
    fn retain_field(_heap: &mut Heap, _base: HeapRef, _offset: usize) {}
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
    fn retain_field(heap: &mut Heap, base: HeapRef, offset: usize) {
        let item = read_heap_ref(heap, base, offset);
        heap.ref_item(item.index(), HeapRefOp::Inc);
    }
}

/// A `Vec<T>` marshals to an Itsy array `[ T ]`: a heap object whose data is the packed in-object
/// representation of its elements (the same layout [VMField] uses for struct/enum fields). Element
/// count is recovered from the object's byte length divided by the element's [VMField::HEAP_SIZE].
/// Nesting works because `Vec<T>` is itself a [VMField] (a reference, like any other heap type).
#[cfg(feature = "runtime")]
impl<T: VMField> VMValue for Vec<T> {
    type Stack = HeapRef;
    fn from_stack(heap: &Heap, this: HeapRef) -> Self {
        let count = heap.item(this.index()).data.len() / T::HEAP_SIZE;
        let mut result = Vec::with_capacity(count);
        let mut offset = 0usize;
        for _ in 0..count {
            result.push(T::read_field(heap, this, offset));
            offset += T::HEAP_SIZE;
        }
        result
    }
    fn to_stack(self, heap: &mut Heap) -> HeapRef {
        let mut buffer: Vec<u8> = Vec::new();
        for element in self {
            element.write_field(heap, &mut buffer);
        }
        alloc_value(heap, &buffer)
    }
    fn free_stack(heap: &mut Heap, this: HeapRef) {
        if heap.item_refs(this.index()) == 0 {
            let count = heap.item(this.index()).data.len() / T::HEAP_SIZE;
            let mut offset = 0usize;
            for _ in 0..count {
                T::free_field(heap, this, offset);
                offset += T::HEAP_SIZE;
            }
        }
        heap.ref_item(this.index(), HeapRefOp::Free);
    }
    fn retain_stack(heap: &mut Heap, this: HeapRef) {
        let count = heap.item(this.index()).data.len() / T::HEAP_SIZE;
        let mut offset = 0usize;
        for _ in 0..count {
            T::retain_field(heap, this, offset);
            offset += T::HEAP_SIZE;
        }
        heap.ref_item(this.index(), HeapRefOp::Inc);
    }
}

#[cfg(feature = "runtime")]
impl<T: VMField> VMField for Vec<T> {
    const HEAP_SIZE: usize = size_of::<HeapRef>();
    fn read_field(heap: &Heap, base: HeapRef, offset: usize) -> Self {
        let item = read_heap_ref(heap, base, offset);
        <Self as VMValue>::from_stack(heap, item)
    }
    fn write_field(self, heap: &mut Heap, buf: &mut Vec<u8>) {
        let item = <Self as VMValue>::to_stack(self, heap);
        buf.extend_from_slice(&item.to_ne_bytes());
    }
    fn free_field(heap: &mut Heap, base: HeapRef, offset: usize) {
        let item = read_heap_ref(heap, base, offset);
        <Self as VMValue>::free_stack(heap, item);
    }
    fn retain_field(heap: &mut Heap, base: HeapRef, offset: usize) {
        let item = read_heap_ref(heap, base, offset);
        <Self as VMValue>::retain_stack(heap, item);
    }
}

// --- host-initiated call marshalling (VM::call_typed / itsy_api! `callables`) ---

/// Marshals `value` onto the VM stack as an argument to a host-initiated Itsy call: allocates its heap
/// representation (reference types) via [VMValue::to_stack], then [retains](VMValue::retain_stack) the
/// whole structure to reference count 1 so the callee epilogue's recursive release of reference
/// parameters balances, and pushes it. Used by the `itsy_api!`-generated call wrappers for
/// struct/enum/array arguments.
#[cfg(feature = "runtime")]
pub fn push_call_argument<V>(value: V, stack: &mut Stack, heap: &mut Heap)
where
    V: VMValue,
    V::Stack: Copy,
    Stack: StackOp<V::Stack>,
{
    let raw = value.to_stack(heap);
    V::retain_stack(heap, raw);
    stack.push(raw);
}

/// Reads the return value of a host-initiated Itsy call off the stack top and materialises it via
/// [VMValue::from_stack], then releases the producer's reference-count-0 heap object (a no-op for value
/// types).
#[cfg(feature = "runtime")]
pub fn read_call_return<V>(stack: &Stack, heap: &mut Heap) -> V
where
    V: VMValue,
    V::Stack: Copy,
    Stack: StackOp<V::Stack>,
{
    let raw: V::Stack = stack.top();
    let value = V::from_stack(heap, raw);
    V::free_stack(heap, raw);
    value
}
