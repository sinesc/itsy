pub mod ast;
pub mod parser;
pub mod resolver;

use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Unresolved<T> {
    Unknown,
    Maybe(T),
    Resolved(T)
}

impl<T> Unresolved<T> {
    pub fn is_unknown(self: &Self) -> bool {
        match self {
            Unresolved::Unknown => true,
            _ => false,
        }
    }
    pub fn is_maybe(self: &Self) -> bool {
        match self {
            Unresolved::Maybe(_) => true,
            _ => false,
        }
    }
    pub fn is_resolved(self: &Self) -> bool {
        match self {
            Unresolved::Resolved(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Enum<'a> {
    //repr: u8,
    keys: HashMap<&'a str, u64>
}

#[derive(Debug)]
pub struct Struct<'a> {
    fields: HashMap<&'a str, Type<'a>>
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Type<'a> {
    void,
    u8, u16, u32, u64,
    i8, i16, i32, i64,
    f32, f64,
    bool,
    String,
    Enum(Enum<'a>),
    Struct(Struct<'a>),
}

impl<'a> Type<'a> {
    pub fn primitive(ident: &'a str) -> Option<Self> {
        match ident {
            "u8"        => Some(Type::u8),
            "u16"       => Some(Type::u16),
            "u32"       => Some(Type::u32),
            "u64"       => Some(Type::u64),
            "i8"        => Some(Type::i8),
            "i16"       => Some(Type::i16),
            "i32"       => Some(Type::i32),
            "i64"       => Some(Type::i64),
            "f32"       => Some(Type::f32),
            "f64"       => Some(Type::f64),
            "bool"      => Some(Type::bool),
            "String"    => Some(Type::String),
            _           => None,
        }
    }
}