use nom::*;
use std::collections::HashMap;

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Type<'a> {
    void,
    u8, i8,
    u16, i16,
    u32, i32,
    u64, i64,
    u128, i128,
    f32,
    f64,
    string,
    custom(&'a str),
}

impl<'a> Type<'a> {
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
            "u128"      => Type::u128,
            "i128"      => Type::i128,
            "f32"       => Type::f32,
            "f64"       => Type::f64,
            "string"    => Type::string,
            _           => Type::custom(ident),
        }
    }
}

#[derive(Debug)]
pub struct Block<'a> {
    statements: Vec<AST<'a>>,
}

#[derive(Debug)]
pub struct Structure<'a> {
    name    : &'a str,
    items   : HashMap<&'a str, Type<'a>>,
}

#[derive(Debug)]
pub struct Argument<'a> {
    name    : &'a str,
    mutable : bool,
    ty      : Type<'a>,
}

#[derive(Debug)]
pub struct Signature<'a> {
    name    : &'a str,
    args    : Vec<Argument<'a>>,
    ret     : Type<'a>,
}

#[derive(Debug)]
pub struct Function<'a> {
    sig     : Signature<'a>,
    block   : Block<'a>,
}

#[derive(Debug)]
pub enum AST<'a> {
    Block(Block<'a>),
    Function(Function<'a>),
}

// basics

named!(ident<&str, &str>, take_while!(|tw| is_alphanumeric(tw as u8))); // TODO: more like [_a-zA-Z][_a-zA-Z0-9]*
named!(ty<&str, Type>, map!(ident, |ident| Type::from_string(ident)));
named!(mutable<&str, &str>, tag!("mut"));
named!(type_assign_op<&str, &str>, tag!(":"));
named!(list_separator<&str, &str>, tag!(","));

// block

named!(block_items<&str, Block>, map!(ws!(opt!(tag!("placeholder"))), |items| Block { // TODO: implement
    statements: Vec::new()
}));

named!(block<&str, Block>, ws!(delimited!(tag!("{"), block_items, tag!("}"))));

// structure

named!(structure_item<&str, (&str, Type)>, map!(ws!(tuple!(ident, type_assign_op, ty)), |tuple| (tuple.0, tuple.2)));

named!(structure_items<&str, HashMap<&str, Type>>, map!(ws!(separated_list_complete!(list_separator, structure_item)), |list| list.into_iter().collect()));

named!(pub structure<&str, Structure>, map!(ws!(tuple!(tag!("struct"), ident, tag!("{"), structure_items, tag!("}"))), |tuple| Structure {
    name: tuple.1,
    items: tuple.3,
}));

// function

named!(argument<&str, Argument>, map!(ws!(tuple!(opt!(mutable), ident, type_assign_op, ty)), |tuple| Argument {
    name    : tuple.1,
    mutable : tuple.0.is_some(),
    ty      : tuple.3,
}));

named!(argument_list<&str, Vec<Argument>>, ws!(delimited!(tag!("("), separated_list_complete!(list_separator, argument), tag!(")"))));

named!(return_part<&str, Type>, ws!(preceded!(tag!("->"), ty)));

named!(signature<&str, Signature>, map!(ws!(tuple!(tag!("fn"), ident, argument_list, opt!(return_part))), |sig| Signature {
    name    : sig.1,
    args    : sig.2,
    ret     : sig.3.unwrap_or(Type::void),
}));

named!(pub function<&str, Function>, map!(ws!(tuple!(signature, block)), |func| Function {
    sig: func.0,
    block: func.1,
}));
