use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use crate::{StackAddress, HeapAddress};
use crate::shared::typed_ids::TypeId;
use crate::shared::numeric::{Numeric, Signed, Unsigned};

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
    pub len: Option<StackAddress>,
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
    HeapRefSize,
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
            Type::HeapRefSize => write!(f, "HeapRefSize"),
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
            Type::String | Type::Enum(_) | Type::Struct(_) | Type::Array(_) | Type::HeapRefSize => ::std::mem::size_of::<HeapAddress>() as u8,
        }
    }
    /// Whether the given numeric is compatible with this type.
    pub fn is_compatible_numeric(self: &Self, value: Numeric) -> bool {
        use std::{u8, u16, u32, u64, i8, i16, i32, i64};
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
            Type::String | Type::Array(_) | Type::Enum(_) | Type::Struct(_) => true,
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
        match self {
            Type::HeapRefSize => false,
            _ => !self.is_ref(),
        }
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
}
