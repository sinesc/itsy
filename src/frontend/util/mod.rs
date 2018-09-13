//! Misc. utility code.

mod repository;
pub(crate) use self::repository::Repository;

mod integer;
pub use self::integer::*;

mod typed_ids;
pub use self::typed_ids::*;

mod type_slot;
pub use self::type_slot::TypeSlot;

use std::collections::HashMap;


/// Information about an enum in a resolved program.
#[derive(Debug)]
pub struct Enum {
    //repr: u8,
    keys: HashMap<usize, u64>
}

/// Information about a struct in a resolved program.
#[derive(Debug)]
pub struct Struct {
    fields: HashMap<usize, Type>
}

/// Information about a primitive type in a resolved program.
#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Type {
    u8, u16, u32, u64,
    i8, i16, i32, i64,
    f32, f64,
    bool,
    String,
    Enum(Enum),
    Struct(Struct),
}

impl Type {
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
            _ => unimplemented!(), // todo: what do I want here? size of string ref and total enum/struct size?
        }
    }
}