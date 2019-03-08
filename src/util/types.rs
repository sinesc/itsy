use std::collections::HashMap;
use std::fmt::{self, Debug};
use super::TypeId;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FnKind {
    Internal,
    Rust(u16),
}

#[derive(Clone, Debug)]
pub struct FnSig {
    pub ret_type: Option<TypeId>,
    pub arg_type: Vec<TypeId>,
    pub kind    : FnKind,
}

impl FnSig {
    /*
    pub fn is_internal(self: &Self) -> bool {
        self.kind == FnKind::Internal
    }
    pub fn is_rust(self: &Self) -> bool {
        match self.kind {
            FnKind::Rust(_) => true,
            _ => false,
        }
    }
    */
    pub fn rust_fn_index(self: &Self) -> Option<u16> {
        match self.kind {
            FnKind::Rust(index) => Some(index),
            _ => None,
        }
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
    pub fields: HashMap<usize, Type>
}

#[derive(Clone, PartialEq)]
pub struct Array {
    pub len: Option<usize>,
    pub type_id: Option<TypeId>,
}

impl Debug for Array {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(len) = self.len {
            write!(f, "[ {}; {:?} ]", self.type_id.as_ref().map(|ty| format!("{:?}", ty)).unwrap_or("???".to_string()), len)
        } else {
            write!(f, "[ {} ]", self.type_id.as_ref().map(|ty| format!("{:?}", ty)).unwrap_or("???".to_string()))
        }
    }
}
/// Information about a primitive type in a resolved program.
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
pub enum TypeKind {
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
    pub fn size(self: &Self) -> Option<u8> {
        match self {
            Type::u8 | Type::i8 | Type::bool => Some(1),
            Type::u16 | Type::i16 => Some(2),
            Type::u32 | Type::i32 | Type::f32 => Some(4),
            Type::u64 | Type::i64 | Type::f64 => Some(8),
            Type::String => Some(12), // pointer+len
            _ => None,
        }
    }
    pub fn kind(self: &Self) -> TypeKind {
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
    pub fn is_array(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Array => true,
            _ => false
        }
    }
    pub fn is_primitive(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Unsigned | TypeKind::Signed | TypeKind::Float | TypeKind::Bool | TypeKind::String => true,
            _ => false
        }
    }
    pub fn is_integer(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Unsigned | TypeKind::Signed => true,
            _ => false
        }
    }
    pub fn is_unsigned(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Unsigned => true,
            _ => false
        }
    }
    pub fn is_signed(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Signed => true,
            _ => false
        }
    }
    pub fn is_float(self: &Self) -> bool {
        match self.kind() {
            TypeKind::Float => true,
            _ => false
        }
    }
}