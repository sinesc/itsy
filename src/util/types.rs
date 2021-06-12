use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use crate::util::{TypeId, BindingId, Numeric};

pub(crate) trait TypeContainer {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type;
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type;

    /// Computes the size of given type.
    fn type_size(self: &Self, ty: &Type) -> u32 {
        match ty {
            Type::Array(a)  => {
                let element_type = self.type_by_id(a.type_id.unwrap());
                let element_size = self.type_size(element_type);
                element_size * a.len.unwrap()
            },
            Type::Struct(s) => {
                s.fields.iter().fold(0, |acc, f| acc + self.type_size(self.type_by_id(f.1.unwrap())))
            },
            Type::Enum(_)   => unimplemented!("enum size"),
            _               => ty.primitive_size() as u32
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FnKind {
    User,
    Rust(u16),
    Intrinsic(Intrinsic),
}

/// A heap reference as it would appear on the stack
#[derive(Debug, Copy, Clone)]
pub struct HeapRef {
    pub index   : u32,
    pub offset  : u32,
}

impl HeapRef {
    /// Returns a clone of this heap reference offset by the given value.
    pub fn offset(self: Self, offset: u32) -> Self { // FIXME negative offsets, but can't use i32 or lose address space. enum arg OFFSET_MINUS, OFFSET_PLUS ?
        Self {
            index   : self.index,
            offset  : self.offset + offset,
        }
    }
    /// Returns a HeapSlice of this heap reference with the given length set.
    pub fn to_slice(self: Self, len: u32) -> HeapSlice {
        HeapSlice {
            index   : self.index,
            offset  : self.offset,
            len     : len,
        }
    }
    /// Returns the size of heap references.
    pub const fn size() -> u8 {
        std::mem::size_of::<Self>() as u8
    }
}

/// A heap slice as it would appear on the stack
#[derive(Debug, Copy, Clone)]
pub struct HeapSlice {
    pub len     : u32,
    pub index   : u32,
    pub offset  : u32,
}

impl HeapSlice {
    /// Returns a clone of this heap reference offset by the given value.
    pub fn offset(self: Self, offset: u32) -> Self { // FIXME negative offsets, but can't use i32 or lose address space. enum arg OFFSET_MINUS, OFFSET_PLUS ?
        Self {
            index   : self.index,
            offset  : self.offset + offset,
            len     : self.len,
        }
    }
    /// Returns a HeapSlice of this heap reference with the given length set.
    pub fn to_ref(self: Self) -> HeapRef {
        HeapRef {
            index   : self.index,
            offset  : self.offset,
        }
    }
    /// Returns the size of heap references.
    pub fn size() -> u8 {
        std::mem::size_of::<Self>() as u8
    }
}

/// Binding meta information
pub struct BindingInfo {
    pub mutable: bool,
    pub type_id: Option<TypeId>,
}

/// Program binding data.
pub struct Bindings {
    /// Maps binding ids to binding info descriptors.
    binding_map : Vec<BindingInfo>,  // HashMap<BindingId, BindingInfo>
    /// Maps type ids to types.
    type_map    : Vec<Type>,    // HashMap<TypeId, Type>
}

impl Bindings {
    pub(crate) fn new(binding_map: Vec<BindingInfo>, type_map: Vec<Type>) -> Self {
        for info in binding_map.iter() {
            info.type_id.expect("Unresolved binding type encountered.");
        }
        Self {
            binding_map,
            type_map
        }
    }
    /// Returns the TypeId of the given binding.
    pub fn binding_type_id(self: &Self, binding_id: BindingId) -> TypeId {
        let binding_index = Into::<usize>::into(binding_id);
        self.binding_map[binding_index].type_id.unwrap()
    }
    /// Returns the type of the given binding.
    pub fn binding_type(self: &Self, binding_id: BindingId) -> &Type {
        let type_id = self.binding_type_id(binding_id);
        &self.type_map[Into::<usize>::into(type_id)]
    }
    /// Returns the mutability of the given binding.
    pub fn binding_mut(self: &Self, binding_id: BindingId) -> bool {
        let binding_index = Into::<usize>::into(binding_id);
        self.binding_map[binding_index].mutable
    }
    pub fn types(self: &Self) -> &[Type] {
        &self.type_map
    }
    pub fn len(self: &Self) -> usize {
        self.binding_map.len()
    }
}

/// Support TypeContainer for Bindings so that methods that need to follow type_ids can be implemented once and be used in both
/// the Resolver where types are scored in Scopes and the Compiler where types are a stored in a Vec.
impl TypeContainer for Bindings {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type {
        let index: usize = type_id.into();
        &self.type_map[index]
    }
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type {
        let index: usize = type_id.into();
        &mut self.type_map[index]
    }
}

/// Information about an enum in a resolved program.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Enum {
    //repr: u8,
    pub keys: HashMap<usize, u64>
}

impl Hash for Enum {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (k, v) in self.keys.iter() {
            k.hash(state);
            v.hash(state);
        };
    }
}

/// Information about a struct in a resolved program.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Struct {
    pub fields: Vec<(String, Option<TypeId>)>,
}

impl Struct {
    /// Returns the TypeId for given field name.
    pub fn type_id(self: &Self, field: &str) -> Option<TypeId> {
        self.fields.iter().find(|f| &f.0 == field).expect("Field not found").1
    }
}

/// Information about an array in a resolved program.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Array {
    pub len: Option<u32>,
    pub type_id: Option<TypeId>,
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
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum TypeKind {
    Void,
    Unsigned,
    Signed,
    Float,
    Bool,
    String,
    Array,
    Enum,
    Struct,
}

impl Type {
    /// Size of the primitive type in bytes, otherwise size of the reference.
    pub fn primitive_size(self: &Self) -> u8 {
        match self {
            Type::void                          => 0,
            Type::u8 | Type::i8 | Type::bool    => 1,
            Type::u16 | Type::i16               => 2,
            Type::u32 | Type::i32 | Type::f32   => 4,
            Type::u64 | Type::i64 | Type::f64   => 8,
            _                                   => 8,
        }
    }
    /// Kind of the type, e.g. Signed or Array.
    pub(crate) fn kind(self: &Self) -> TypeKind {
        match self {
            Type::void => TypeKind::Void,
            Type::u8 | Type::u16 | Type::u32 | Type::u64 => TypeKind::Unsigned,
            Type::i8 | Type::i16 | Type::i32 | Type::i64 => TypeKind::Signed,
            Type::f32 | Type::f64 => TypeKind::Float,
            Type::bool => TypeKind::Bool,
            Type::String => TypeKind::String,
            Type::Enum(_) => TypeKind::Enum,
            Type::Struct(_) => TypeKind::Struct,
            Type::Array(_) => TypeKind::Array,
        }
    }
    /// Whether the given numeric is compatible with this type.
    pub fn is_compatible_numeric(self: &Self, value: Numeric) -> bool {
        use std::{u8, u16, u32, u64, i8, i16, i32, i64};
        use crate::util::{Signed, Unsigned};
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
    /// Whether the type is a primitive.
    pub fn is_primitive(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Unsigned | TypeKind::Signed | TypeKind::Float | TypeKind::Bool => true,
            _ => false
        }
    }
    /// Whether the type is referenced when wrapped.
    pub fn is_ref(self: &Self) -> bool {
        match self {
            Type::String | Type::Array(_) | Type::Enum(_) | Type::Struct(_) => true,
            _ => false,
        }
    }
    /// Whether the type is an array.
    pub fn is_array(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Array => true,
            _ => false
        }
    }
    /// Returns the type as an array.
    pub fn as_array(self: &Self) -> Option<&Array> {
        match self {
            Type::Array(array) => Some(array),
            _ => None
        }
    }
    /// Whether the type is a struct.
    pub fn is_struct(self: &Self) -> bool {
        match self {
            Type::Struct(_) => true,
            _ => false
        }
    }
    /// Returns the type as a struct.
    pub fn as_struct(self: &Self) -> Option<&Struct> {
        match self {
            Type::Struct(struct_) => Some(struct_),
            _ => None
        }
    }
    /// Whether the type is an integer.
    pub fn is_integer(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Unsigned | TypeKind::Signed => true,
            _ => false
        }
    }
    /// Whether the type is unsigned
    pub fn is_unsigned(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Unsigned => true,
            _ => false
        }
    }
    /// Whether the type is signed.
    pub fn is_signed(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Signed => true,
            _ => false
        }
    }
    /// Whether the type is a float.
    pub fn is_float(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Float => true,
            _ => false
        }
    }
    /// Whether the type is a string.
    pub fn is_string(self: &Self) -> bool {
        match self {
            Type::String => true,
            _ => false
        }
    }
    /// Whether the type is void.
    pub fn is_void(self: &Self) -> bool {
        match self {
            Type::void => true,
            _ => false
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Intrinsic {
    ArrayLen,
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
        match raw {
            174 => Self::Primitive,
            175 => Self::Array,
            176 => Self::Struct,
            177 => Self::String,
            index @ _ => unreachable!("Invalid constructor type {}", index),
        }
        //unsafe { ::std::mem::transmute(raw) }
    }
}