use crate::prelude::*;
use crate::{HeapAddress, VariantIndex, FrameAddress, RustFnIndex};
use crate::shared::{impl_as_getter, MetaContainer, typed_ids::{TypeId, FunctionId, ConstantId}, numeric::{Numeric, Signed, Unsigned}};
use crate::bytecode::builtins::BuiltinType;

/// Binding meta information.
#[derive(Debug)]
pub struct Binding {
    pub mutable: bool,
    pub type_id: Option<TypeId>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    Function(FunctionId),
    Discriminant(Numeric),
}

impl_as_getter!(ConstantValue {
    pub as_function_id: Function -> *FunctionId,
});


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constant {
    pub value: ConstantValue,
    pub type_id: Option<TypeId>,
}

/// Single variant of an enum.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EnumVariant {
    Simple(Option<Numeric>),
    Data(Vec<Option<TypeId>>),
}

impl EnumVariant {
    pub fn as_data(self: &Self) -> Option<&Vec<Option<TypeId>>> {
        match self {
            Self::Data(d) => Some(d),
            _ => None,
        }
    }
    pub fn as_simple(self: &Self) -> Option<Numeric> {
        match self {
            Self::Simple(s) => *s,
            _ => None,
        }
    }
}

/// Information about an enum in a resolved program.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Enum {
    pub variants: Vec<(String, EnumVariant)>,
    pub impl_traits: Map<TypeId, ImplTrait>,
    pub primitive: Option<(TypeId, u8)>, // note: including primitive size here so Type::primitive_size() can return the correct size (can't do a type lookup there)
}

impl Enum {
    /// Returns the numeric variant index of given named variant.
    pub fn variant_index(self: &Self, variant: &str) -> Option<VariantIndex> {
        self.variants.iter().position(|(name, _)| name == variant).map(|i| i as VariantIndex)
    }
    /// Returns the numeric variant discriminant of given named variant.
    pub fn variant_value(self: &Self, variant: &str) -> Option<Numeric> {
        self.variants.iter().find(|(n, v)| n == variant && v.as_simple().is_some()).map(|(_, v)| v.as_simple().unwrap())
    }
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
    pub fn has_field(self: &Self, field: &str) -> bool {
        self.fields.contains_key(field)
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
    pub functions: Map<String, Option<ConstantId>>,
}

impl ImplTrait {
    pub fn new() -> Self {
        Self { functions: Map::new() }
    }
}

/// Information about a trait definition in a resolved program.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Trait {
    pub provided: Map<String, Option<ConstantId>>,
    pub required: Map<String, Option<ConstantId>>,
}

/// Type-information for a function or closure.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Callable {
    pub arg_type_ids: Vec<Option<TypeId>>,
    pub ret_type_id : Option<TypeId>,
}

impl Callable {
    /// Computes the total primitive size of the callable's parameters.
    pub fn arg_size(self: &Self, container: &dyn MetaContainer) -> FrameAddress {
        let mut arg_size = 0;
        for arg in &self.arg_type_ids {
            let arg_type_id = arg.expect("Function arg is not resolved");
            arg_size += container.type_by_id(arg_type_id).primitive_size() as FrameAddress;
        }
        arg_size
    }
    /// Returns the primitive size of the callable's return type.
    pub fn ret_size(self: &Self, container: &dyn MetaContainer) -> u8 {
        let ret_type_id = self.ret_type_id.expect("Function result is not resolved");
        container.type_by_id(ret_type_id).primitive_size()
    }
}

/// A concrete function implementation, referring to a bit of Itsy or Rust code.
#[derive(Clone, Debug)]
pub struct Function {
    pub kind                : Option<FunctionKind>,
    pub signature_type_id   : TypeId,
}

impl Function {
    pub fn rust_fn_index(self: &Self) -> Option<RustFnIndex> {
        match self.kind {
            Some(FunctionKind::Rust(index)) => Some(index),
            _ => None,
        }
    }
    pub fn is_resolved(self: &Self, container: &dyn MetaContainer) -> bool {
        let callable = container.type_by_id(self.signature_type_id).as_callable().unwrap();
        callable.ret_type_id.is_some() && self.kind.is_some() && callable.arg_type_ids.iter().all(|arg| arg.is_some())
    }
    /// Returns function argument type ids.
    pub fn arg_type_ids<'a>(self: &Self, container: &'a dyn MetaContainer) -> &'a Vec<Option<TypeId>> {
        let callable = container.type_by_id(self.signature_type_id).as_callable().unwrap();
        &callable.arg_type_ids
    }
    /// Returns function return type ids.
    pub fn ret_type_id<'a>(self: &Self, container: &'a dyn MetaContainer) -> Option<TypeId> {
        let callable = container.type_by_id(self.signature_type_id).as_callable().unwrap();
        callable.ret_type_id
    }
    /// Computes the total primitive size of the function parameters.
    pub fn arg_size(self: &Self, container: &dyn MetaContainer) -> FrameAddress {
        let callable = container.type_by_id(self.signature_type_id).as_callable().unwrap();
        callable.arg_size(container)
    }
    /// Returns the primitive size of the function return type.
    pub fn ret_size(self: &Self, container: &dyn MetaContainer) -> u8 {
        let callable = container.type_by_id(self.signature_type_id).as_callable().unwrap();
        callable.ret_size(container)
    }
}

/// The kind of a function described by a Function.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FunctionKind {
    Function,
    Method(TypeId),
    Rust(RustFnIndex),
    Builtin(TypeId, BuiltinType),
    Variant(TypeId, VariantIndex),
}

/// Information about a data type in a resolved program.
#[allow(non_camel_case_types)]
#[derive(PartialEq, Eq, Hash)]
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
    Callable(Callable),
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
            Type::Callable(v) => write!(f,"{:?}", v),
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
            Type::Callable(_) => write!(f, "callable"),
        }
    }
}

impl_as_getter!(Type {
    /// Returns the type as a callable.
    pub as_callable: Callable -> &Callable,
    /// Returns the type as a mutable callable.
    pub as_callable_mut: Callable -> mut Callable,
    // Returns whether the type is a callable.
    pub is_callable: Callable ? bool,
    /// Returns the type as an array.
    pub as_array: Array -> &Array,
    /// Returns the type as a mutable array.
    pub as_array_mut: Array -> mut Array,
    /// Returns the type as a struct.
    pub as_struct: Struct -> &Struct,
    /// Returns the type as a mutable struct.
    pub as_struct_mut: Struct -> mut Struct,
    /// Returns the type as an enum.
    pub as_enum: Enum -> &Enum,
    /// Returns the type as a mutable enum.
    pub as_enum_mut: Enum -> mut Enum,
    /// Returns the type as a trait.
    pub as_trait: Trait -> & Trait,
    /// Returns the type as a mutable trait.
    pub as_trait_mut: Trait -> mut Trait,
});

impl Type {
    /// Size of the primitive type in bytes, otherwise size of the reference.
    pub const fn primitive_size(self: &Self) -> u8 {
        match self {
            Type::void                          => 0,
            Type::u8 | Type::i8 | Type::bool    => 1,
            Type::u16 | Type::i16               => 2,
            Type::u32 | Type::i32 | Type::f32   => 4,
            Type::u64 | Type::i64 | Type::f64   => 8,
            Type::Enum(Enum { primitive: Some((_, s)), .. }) => *s,
            Type::String | Type::Enum(_) | Type::Struct(_) | Type::Array(_) | Type::Trait(_) | Type::Callable(_) => size_of::<HeapAddress>() as u8,
        }
    }
    /// Returns the type for an unsigned integer of the given byte-size.
    pub const fn unsigned(size: usize) -> Self { // TODO return option once const unwrap becomes stable
        match size {
            1 => Self::u8,
            2 => Self::u16,
            4 => Self::u32,
            8 => Self::u64,
            _ => panic!("Unsupported unsigned integer size."),
        }
    }
    /// Returns the type for a signed integer of the given byte-size.
    pub const fn signed(size: usize) -> Self { // TODO return option once const unwrap becomes stable
        match size {
            1 => Self::i8,
            2 => Self::i16,
            4 => Self::i32,
            8 => Self::i64,
            _ => panic!("Unsupported signed integer size."),
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
                _ => panic!("Type is not numeric"),
            };
            range.0 <= value && range.1 >= value
        } else {
            false
        }
    }
    /// Whether the type is a reference type.
    pub const fn is_ref(self: &Self) -> bool {
        match self {
            Type::Enum(Enum { primitive: Some(_), .. }) => false,
            Type::String | Type::Array(_) | Type::Enum(_) | Type::Struct(_) | Type::Trait(_) | Type::Callable(_) => true,
            _ => false,
        }
    }
    /// Whether the type is a primitive.
    pub const fn is_primitive(self: &Self) -> bool {
        !self.is_ref()
    }
    /// Whether the type is a reference type.
    pub const fn is_simple_enum(self: &Self) -> bool {
        match self {
            Type::Enum(Enum { primitive: Some(_), .. }) => true,
            _ => false,
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
    /// Whether the type is either a float or integer.
    pub const fn is_numeric(self: &Self) -> bool {
        self.is_float() || self.is_integer()
    }
    /// Whether the type is a string.
    pub const fn is_string(self: &Self) -> bool {
        match self {
            Type::String => true,
            _ => false
        }
    }
    /// Whether the type is a boolean.
    pub const fn is_bool(self: &Self) -> bool {
        match self {
            Type::bool => true,
            _ => false
        }
    }
    /* /// Whether the type is a concrete type. This includes all types but traits.
    /// Traits are not concrete because they can potentially be narrowed to a specific trait implementor.
    /// A binding for a concrete implementor would not accept the generic trait but the inverse is acceptable
    pub fn is_concrete(self: &Self) -> bool {
        match self {
            Type::Trait(_) => false,
            _ => true
        }
    }*/
    /// Whether a type can be constructed.
    pub fn is_constructible(self: &Self) -> bool {
        match self {
            Type::Trait(_) => false,
            Type::Callable(_) => false,
            _ => !self.is_primitive(),
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
