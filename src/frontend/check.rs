use std::collections::HashMap;
use frontend::ast;

#[derive(Debug)]
pub struct Enum<'a> {
    //repr: u8,
    keys: HashMap<&'a str, u64>
}

#[derive(Debug)]
pub struct Struct<'a> {
    fields: HashMap<&'a str, Type<'a>>
}

#[derive(Debug)]
pub enum Type<'a> {
    Enum(Enum<'a>),
    Struct(Struct<'a>),
    u8, u16, u32, u64,
    i8, i16, i32, i64,
    f32, f64,
    string,
}
/*
impl<'a> ResolvedType<'a> {
    pub fn from_string(ident: &'a str) -> Self {
        match ident {
            "u8"        => Type::u8,
            "i8"        => Type::i8,
            "u16"       => Type::u16,
            "i16"       => Type::i16,
            "u32"       => Type::u32,
            "i32"       => Type::i32,
            "u64"       => Type::u64,
            "i64"       => Type::i64,
            "f32"       => Type::f32,
            "f64"       => Type::f64,
            "string"    => Type::string,
            _           => Type::custom(ident),
        }
    }
}
*/

#[derive(Debug)]
pub struct ResolvedProgram<'a> {
    ast: ast::Program<'a>,
    types: HashMap<&'a str, Type<'a>>,
}

pub fn check<'a>(program: ast::Program<'a>) -> ResolvedProgram<'a> {

    ResolvedProgram {
        ast: program,
        types: HashMap::new(),
    }
}