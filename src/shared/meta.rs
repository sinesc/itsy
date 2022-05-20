use crate::StackAddress;
use crate::prelude::*;
use crate::HeapAddress;
use crate::shared::typed_ids::{TypeId, FunctionId};
use crate::shared::numeric::{Numeric, Signed, Unsigned};
use crate::{VariantIndex, RustFnIndex};

/// Binding meta information.
pub struct Binding {
    pub mutable: bool,
    pub type_id: Option<TypeId>,
}

/// Information about an enum in a resolved program.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Enum {
    pub variants: Vec<(String, Vec<Option<TypeId>>)>,
    pub impl_traits: Map<TypeId, ImplTrait>,
}

/// Information about a struct in a resolved program.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Struct {
    pub fields: Map<String, Option<TypeId>>,
    pub impl_traits: Map<TypeId, ImplTrait>,
}

impl Struct {
    /// Returns the TypeId for given field name.
    pub fn type_id(self: &Self, field: &str) -> Option<TypeId> {
        *self.fields.get(field).expect("Field not found")
    }
}

/// Information about an array in a resolved program.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Array {
    pub type_id: Option<TypeId>,
}

/// Information about a trait-implementation for a type.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplTrait {
    pub functions: Map<String, Option<FunctionId>>,
}

impl ImplTrait {
    pub fn new() -> Self {
        Self { functions: Map::new() }
    }
}

/// Information about a trait definition in a resolved program.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Trait {
    pub provided: Map<String, Option<FunctionId>>,
    pub required: Map<String, Option<FunctionId>>,
}

/// Function mata information.
#[derive(Clone)]
pub struct Function {
    pub kind    : Option<FunctionKind>,
    pub arg_type: Vec<Option<TypeId>>,
    pub ret_type: Option<TypeId>,
}

impl Function {
    pub fn rust_fn_index(self: &Self) -> Option<RustFnIndex> {
        match self.kind {
            Some(FunctionKind::Rust(index)) => Some(index),
            _ => None,
        }
    }
    pub fn is_resolved(self: &Self) -> bool {
        self.ret_type.is_some() && self.kind.is_some() && self.arg_type.iter().all(|arg| arg.is_some())
    }
}

/// The kind of a function described by a FunctionInfo.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FunctionKind {
    Function,
    Method(TypeId),
    Rust(RustFnIndex),
    Intrinsic(TypeId, Intrinsic),
    Variant(TypeId, VariantIndex),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Intrinsic {
    ArrayLen,
    ArrayPush,
    ArrayPop,
    ArrayTruncate,
    ArrayRemove,
}

/// Information about a data type in a resolved program.
#[allow(non_camel_case_types)]
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    void,
    u8, u16, u32, u64,
    i8, i16, i32, i64,
    f32, f64,
    bool,
    String,
    Array(Array),
    Enum(Enum),
    Struct(Struct),
    Trait(Trait),
}

impl Debug for Type {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::void => write!(f, "void"),
            Type::u8 => write!(f, "u8"),
            Type::u16 => write!(f, "u16"),
            Type::u32 => write!(f, "u32"),
            Type::u64 => write!(f, "u64"),
            Type::i8 => write!(f, "i8"),
            Type::i16 => write!(f, "i16"),
            Type::i32 => write!(f, "i32"),
            Type::i64 => write!(f, "i64"),
            Type::f32 => write!(f, "f32"),
            Type::f64 => write!(f, "f64"),
            Type::bool => write!(f, "bool"),
            Type::String => write!(f, "String"),
            Type::Array(v) => write!(f, "{:?}", v),
            Type::Enum(v) => write!(f, "{:?}", v),
            Type::Struct(v) => write!(f, "{:?}", v),
            Type::Trait(_) => write!(f, "<Trait>"),
        }
    }
}

impl Display for Type {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::void => write!(f, "void"),
            Type::u8 => write!(f, "u8"),
            Type::u16 => write!(f, "u16"),
            Type::u32 => write!(f, "u32"),
            Type::u64 => write!(f, "u64"),
            Type::i8 => write!(f, "i8"),
            Type::i16 => write!(f, "i16"),
            Type::i32 => write!(f, "i32"),
            Type::i64 => write!(f, "i64"),
            Type::f32 => write!(f, "f32"),
            Type::f64 => write!(f, "f64"),
            Type::bool => write!(f, "bool"),
            Type::String => write!(f, "String"),
            Type::Array(_) => write!(f, "[ _ ]"),
            Type::Enum(_) => write!(f, "enum"),
            Type::Struct(_) => write!(f, "struct"),
            Type::Trait(_) => write!(f, "trait"),
        }
    }
}

impl Type {
    /// Size of the primitive type in bytes, otherwise size of the reference.
    pub const fn primitive_size(self: &Self) -> u8 {
        match self {
            Type::void                          => 0,
            Type::u8 | Type::i8 | Type::bool    => 1,
            Type::u16 | Type::i16               => 2,
            Type::u32 | Type::i32 | Type::f32   => 4,
            Type::u64 | Type::i64 | Type::f64   => 8,
            Type::String | Type::Enum(_) | Type::Struct(_) | Type::Array(_) | Type::Trait(_) => size_of::<HeapAddress>() as u8,
        }
    }
    /// Returns the type for an unsigned integer of the given byte-size.
    pub const fn unsigned(size: StackAddress) -> Self { // TODO return option once const unwrap becomes stable
        match size {
            1 => Self::u8,
            2 => Self::u16,
            4 => Self::u32,
            8 => Self::u64,
            _ => panic!("Unsupported unsigned integer size"), // unreachable! with string argument causes compile error
        }
    }
    /// Returns the type for a signed integer of the given byte-size.
    pub const fn signed(size: StackAddress) -> Self { // TODO return option once const unwrap becomes stable
        match size {
            1 => Self::i8,
            2 => Self::i16,
            4 => Self::i32,
            8 => Self::i64,
            _ => panic!("Unsupported signed integer size"), // unreachable! with string argument causes compile error
        }
    }
    /// Whether the given numeric is compatible with this type.
    pub fn is_compatible_numeric(self: &Self, value: Numeric) -> bool {
        use core::{u8, u16, u32, u64, i8, i16, i32, i64};
        if value.is_float() && self.is_float() {
            true
        } else if value.is_integer() && self.is_integer() {
            let range = match self {
                Type::u8    => (Numeric::Unsigned(u8::MIN as Unsigned), Numeric::Unsigned(u8::MAX as Unsigned)),
                Type::u16   => (Numeric::Unsigned(u16::MIN as Unsigned), Numeric::Unsigned(u16::MAX as Unsigned)),
                Type::u32   => (Numeric::Unsigned(u32::MIN as Unsigned), Numeric::Unsigned(u32::MAX as Unsigned)),
                Type::u64   => (Numeric::Unsigned(u64::MIN as Unsigned), Numeric::Unsigned(u64::MAX as Unsigned)),
                Type::i8    => (Numeric::Signed(i8::MIN as Signed), Numeric::Signed(i8::MAX as Signed)),
                Type::i16   => (Numeric::Signed(i16::MIN as Signed), Numeric::Signed(i16::MAX as Signed)),
                Type::i32   => (Numeric::Signed(i32::MIN as Signed), Numeric::Signed(i32::MAX as Signed)),
                Type::i64   => (Numeric::Signed(i64::MIN as Signed), Numeric::Signed(i64::MAX as Signed)),
                _ => unreachable!(),
            };
            range.0 <= value && range.1 >= value
        } else {
            false
        }
    }
    /// Whether the type is a reference type.
    pub const fn is_ref(self: &Self) -> bool {
        match self {
            Type::String | Type::Array(_) | Type::Enum(_) | Type::Struct(_) | Type::Trait(_) => true,
            _ => false,
        }
    }
    /// Whether the type is a copy type.
    pub const fn is_copy(self: &Self) -> bool {
        match self {
            Type::String => true,
            _ => self.is_primitive(),
        }
    }
    /// Whether the type is a primitive.
    pub const fn is_primitive(self: &Self) -> bool {
        !self.is_ref()
    }
    /// Whether the type is void.
    pub const fn is_void(self: &Self) -> bool {
        match self {
            Type::void => true,
            _ => false
        }
    }
    /// Whether the type is unsigned
    pub const fn is_unsigned(self: &Self) -> bool {
        match self {
            Type::u8 | Type::u16 |  Type::u32 |  Type::u64 => true,
            _ => false
        }
    }
    /// Whether the type is signed.
    pub const fn is_signed(self: &Self) -> bool {
        match self {
            Type::i8 |  Type::i16 |  Type::i32 |  Type::i64 => true,
            _ => false
        }
    }
    /// Whether the type is an integer.
    pub const fn is_integer(self: &Self) -> bool {
        self.is_unsigned() || self.is_signed()
    }
    /// Whether the type is a float.
    pub const fn is_float(self: &Self) -> bool {
        match self {
            Type::f32 | Type::f64 => true,
            _ => false
        }
    }
    /// Whether the type is a string.
    pub const fn is_string(self: &Self) -> bool {
        match self {
            Type::String => true,
            _ => false
        }
    }
    /// Whether the type is a simple enum.
    pub fn is_simple_enum(self: &Self) -> bool {
        match self {
            Type::Enum(e) => {
                e.impl_traits.len() == 0 && e.variants.iter().all(|(_, v)| v.len() == 0)
            },
            _ => false
        }
    }
    /// Whether the type is a concrete type. This includes all types but traits.
    /// Traits are not concrete because they can potentially be narrowed to a specific trait implementor.
    /// A binding for a concrete implementor would not accept the generic trait but the inverse is acceptable
    pub fn is_concrete(self: &Self) -> bool {
        match self {
            Type::Trait(_) => false,
            _ => true
        }
    }
    /// Returns the type as an array.
    pub const fn as_array(self: &Self) -> Option<&Array> {
        match self {
            Type::Array(array) => Some(array),
            _ => None
        }
    }
    /// Returns the type as a mutable array.
    pub fn as_array_mut(self: &mut Self) -> Option<&mut Array> {
        match self {
            Type::Array(array) => Some(array),
            _ => None
        }
    }
    /// Returns the type as a struct.
    pub const fn as_struct(self: &Self) -> Option<&Struct> {
        match self {
            Type::Struct(struct_) => Some(struct_),
            _ => None
        }
    }
    /// Returns the type as a mutable struct.
    pub fn as_struct_mut(self: &mut Self) -> Option<&mut Struct> {
        match self {
            Type::Struct(struct_) => Some(struct_),
            _ => None
        }
    }
    /// Returns the type as an enum.
    pub const fn as_enum(self: &Self) -> Option<&Enum> {
        match self {
            Type::Enum(enum_) => Some(enum_),
            _ => None
        }
    }
    /// Returns the type as a mutable enum.
    pub fn as_enum_mut(self: &mut Self) -> Option<&mut Enum> {
        match self {
            Type::Enum(enum_) => Some(enum_),
            _ => None
        }
    }
    /// Returns the type as a trait.
    pub const fn as_trait(self: &Self) -> Option<&Trait> {
        match self {
            Type::Trait(trait_def) => Some(trait_def),
            _ => None
        }
    }
    /// Returns the type as a mutable trait.
    pub fn as_trait_mut(self: &mut Self) -> Option<&mut Trait> {
        match self {
            Type::Trait(trait_def) => Some(trait_def),
            _ => None
        }
    }
    /// Returns an iterator over the type_ids of traits implemented by this type, if any.
    pub fn impl_trait_ids(self: &Self) -> Option<impl Iterator<Item=&TypeId>> {
        match self {
            Type::Struct(struct_) => Some(struct_.impl_traits.keys()),
            _ => None,
        }
    }
    /// Returns an iterator over the traits implemented by this type, if any.
    pub fn impl_traits(self: &Self) -> Option<impl Iterator<Item=(&TypeId, &ImplTrait)>> {
        match self {
            Type::Struct(struct_) => Some(struct_.impl_traits.iter()),
            _ => None,
        }
    }
}
