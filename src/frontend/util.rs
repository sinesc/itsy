//! Misc. utility code.

mod repository;
pub(crate) use self::repository::Repository;

mod numeric;
pub use self::numeric::*;

mod typed_ids;
pub use self::typed_ids::*;

mod fn_sig;
pub use self::fn_sig::{FnSig, FnKind};

use std::collections::HashMap;
use std::fmt::{self, Debug};

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
    pub ty: Box<Type>,
}

impl Debug for Array {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(len) = self.len {
            write!(f, "[ {:?}, {} ]", &*self.ty, len)
        } else {
            write!(f, "[ {:?} ]", &*self.ty)
        }
    }
}
/// Information about a primitive type in a resolved program.
#[allow(non_camel_case_types)]
#[derive(Clone, PartialEq)]
pub enum Type {
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
    pub fn is_integer(self: &Self) -> bool {
        match self {
            Type::u8 | Type::u16 | Type::u32 | Type::u64 | Type::i8 | Type::i16 | Type::i32 | Type::i64 => true,
            _ => false,
        }
    }
    pub fn is_unsigned(self: &Self) -> bool {
        match self {
            Type::u8 | Type::u16 | Type::u32 | Type::u64 => true,
            _ => false,
        }
    }
    pub fn is_signed(self: &Self) -> bool {
        match self {
            Type::i8 | Type::i16 | Type::i32 | Type::i64 => true,
            _ => false,
        }
    }
    pub fn is_float(self: &Self) -> bool {
        match self {
            Type::f32 | Type::f64 => true,
            _ => false,
        }
    }
    pub fn size(self: &Self) -> u8 {
        match self {
            Type::u8 | Type::i8 | Type::bool => 1,
            Type::u16 | Type::i16 => 2,
            Type::u32 | Type::i32 | Type::f32 => 4,
            Type::u64 | Type::i64 | Type::f64 => 8,
            Type::String => 12, // pointer+len
            _ => unimplemented!(), // todo: remaining types
        }
    }
    pub fn kind(self: &Self) -> TypeKind {
        match self {
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
}