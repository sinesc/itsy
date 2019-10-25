use std::collections::HashMap;
use std::fmt::{self, Debug};
use crate::{util::{TypeId, BindingId, Numeric}, runtime::{Value, CONSTPOOL_INDEX}};

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
    pub len     : u32,
    pub offset  : u32,
}

impl HeapRef {
    /// Constructs a heap reference from const offset and length.
    pub fn from_const(const_offset: u32, const_len: u32) -> Self {
        HeapRef {
            index   : CONSTPOOL_INDEX,
            offset  : const_offset,
            len     : const_len,
        }
    }
    /// Returns a clone of this heap reference offset by the given value.
    pub fn offset(self: Self, offset: u32) -> Self { // Todo: might want to accept i32 instead
        HeapRef {
            index   : self.index,
            offset  : self.offset + offset,
            len     : self.len,
        }
    }
    /// Returns a clone of this heap reference with the given length set.
    pub fn len(self: Self, len: u32) -> Self {
        HeapRef {
            index   : self.index,
            offset  : self.offset,
            len     : len,
        }
    }
    /// Returns the size of heap references.
    pub fn size() -> u8 {
        std::mem::size_of::<HeapRef>() as u8
    }
}

/// Program binding data.
pub struct Bindings {
    /// List of bindings mapped to their TypeIds
    type_map        : Vec<TypeId>,
    /// List of registered types, effectively mapped via vector index = TypeId.
    types           : Vec<Type>,
    /// Binding mutability, effectively mapped via vector index = mutability.
    mutability_map  : Vec<bool>,
}

impl Bindings {
    pub(crate) fn new(mutability_map: Vec<bool>, type_map: Vec<TypeId>, types: Vec<Type>) -> Self {
        Self {
            type_map,
            types,
            mutability_map
        }
    }
    /// Returns the TypeId of the given binding.
    pub fn binding_type_id(self: &Self, binding_id: BindingId) -> TypeId {
        let binding_index = Into::<usize>::into(binding_id);
        self.type_map[binding_index]
    }
    /// Returns the type of the given binding.
    pub fn binding_type(self: &Self, binding_id: BindingId) -> &Type {
        let binding_index = Into::<usize>::into(binding_id);
        let type_id = self.type_map[binding_index];
        &self.types[Into::<usize>::into(type_id)]
    }
    /// Returns the mutability of the given binding.
    pub fn binding_mut(self: &Self, binding_id: BindingId) -> bool {
        let binding_index = Into::<usize>::into(binding_id);
        self.mutability_map[binding_index]
    }
    pub fn get_type(self: &Self, type_id: TypeId) -> &Type {
        let type_index = Into::<usize>::into(type_id);
        &self.types[type_index]
    }
    pub fn types(self: &Self) -> &[Type] {
        &self.types
    }
}

/// Information about an enum in a resolved program.
#[derive(Clone, Debug, PartialEq)]
pub struct Enum {
    //repr: u8,
    pub keys: HashMap<usize, u64>
}

/// Information about a struct in a resolved program.
#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub fields: Vec<(String, Option<TypeId>)>,
    pub by_ref: bool,
}

impl Struct {
    /// Returns the TypeId for given field name.
    pub fn type_id(self: &Self, field: &str) -> Option<TypeId> {
        self.fields.iter().find(|f| &f.0 == field).map(|f| &f.1).and_then(|o| *o) // todo: this lumps "not found" and "unresolved" together
    }
}

/// Information about an array in a resolved program.
#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    pub len: Option<u32>,
    pub type_id: Option<TypeId>,
}

/*
impl Debug for Array {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(len) = self.len {
            write!(f, "[ {}; {:?} ]", self.type_id.as_ref().map(|ty| format!("{:?}", ty)).unwrap_or("???".to_string()), len)
        } else {
            write!(f, "[ {} ]", self.type_id.as_ref().map(|ty| format!("{:?}", ty)).unwrap_or("???".to_string()))
        }
    }
}
*/
/// Information about a data type in a resolved program.
#[allow(non_camel_case_types)]
#[derive(Clone, PartialEq)]
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
    pub fn size(self: &Self) -> u8 {
        match self {
            Type::void                          => 0,
            Type::u8 | Type::i8 | Type::bool    => 1,
            Type::u16 | Type::i16               => 2,
            Type::u32 | Type::i32 | Type::f32   => 4,
            Type::u64 | Type::i64 | Type::f64   => 8,
            _ => HeapRef::size(),
        }
    }
    /// Size of the type in stack elements.
    pub fn quadsize(self: &Self) -> u8 {
        (self.size() + (std::mem::size_of::<Value>() as u8 - 1)) / std::mem::size_of::<Value>() as u8
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
            Type::Struct(s)     => s.by_ref, // TODO needs to move of type def (need generic & support for all types)
            Type::Array(a)      => false, // FIXME just testing ref array
            Type::String        => true,
            _                   => false,
        }
    }
    /// Whether the type is referenced while on the stack.
    pub fn is_stackref(self: &Self) -> bool {
        !self.is_primitive() || self.is_ref()
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
    _EnumStart = 173,
    Copy,           // 174 Copy(num_bytes): memcopies data
    CopyRef,        // 175 CopyRef: memcopies referenced data to new heap object (size determined from reference)
    Array,          // 176 Array(len, recursive elements): copies an array
    ArrayRef,       // 177 NYI
    Struct,         // 178 Struct(num_fields, recursive fields): copies a struct
    StructRef,      // 179 StructDynamic(num_fields, recursive fields): copies a referenced struct to new heap object
    _EnumEnd,
}

impl Constructor {
    pub fn from_u8(raw: u8) -> Constructor {
        assert!(raw > Constructor::_EnumStart as u8 && raw < Constructor::_EnumEnd as u8);
        unsafe { std::mem::transmute(raw) }
    }
}