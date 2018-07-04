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