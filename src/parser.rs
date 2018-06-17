use nom::*;
use nom::types::CompleteStr as Input;
use std::collections::HashMap;

#[derive(Debug)]
pub enum AST<'a> {
    Block(Block<'a>),
    Function(Function<'a>),
}

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
pub enum Expression {
    Ident(String),                                      // TODO: would like these to be &str, caused lifetime issue in fold_many0 closure though
    Literal(String),
    Operation(Operation),
}

#[derive(Debug)]
pub enum Operation {
    Binary(String, Box<Expression>, Box<Expression>),   // TODO: would like these to be &str, caused lifetime issue in fold_many0 closure though
    Prefix(String, Box<Expression>),
    Suffix(String, Box<Expression>),
}

// basics

named!(ident<Input, Input>, take_while!(|tw| is_alphanumeric(tw as u8))); // TODO: more like [_a-zA-Z][_a-zA-Z0-9]*
named!(ty<Input, Type>, map!(ident, |ident| Type::from_string(*ident)));
named!(mutable<Input, Input>, tag!("mut"));
named!(type_assign_op<Input, Input>, tag!(":"));
named!(list_separator<Input, Input>, tag!(","));

// block

named!(block_items<Input, Block>, map!(ws!(opt!(tag!("placeholder"))), |items| Block { // TODO: implement
    statements: Vec::new()
}));

named!(block<Input, Block>, ws!(delimited!(tag!("{"), block_items, tag!("}"))));

// expression

named!(parens<Input, Expression>, ws!(delimited!(tag!("("), expression, tag!(")"))));

named!(ident_exp<Input, Expression>, map!(ident, |m| Expression::Ident(m.to_string())));

named!(operand<Input, Expression>, alt!(ident_exp | parens));

named!(term<Input, Expression>, do_parse!(
    init: operand >>
    res: fold_many0!(
        pair!(alt!(tag!("*") | tag!("/")), operand),
        init,
        |acc, (op, val): (Input, Expression)| {
            Expression::Operation(Operation::Binary(op.to_string(), Box::new(acc), Box::new(val)))
        }
    ) >>
    (res)
));

named!(pub expression<Input, Expression>, do_parse!(
    init: term >>
    res: fold_many0!(
        pair!(alt!(tag!("+") | tag!("-")), term),
        init,
        |acc, (op, val): (Input, Expression)| {
            Expression::Operation(Operation::Binary(op.to_string(), Box::new(acc), Box::new(val)))
        }
    ) >>
    (res)
));

// structure

named!(structure_item<Input, (Input, Type)>, map!(ws!(tuple!(ident, type_assign_op, ty)), |tuple| (tuple.0, tuple.2)));

named!(structure_items<Input, HashMap<&str, Type>>, map!(ws!(separated_list_complete!(list_separator, structure_item)), |list| list.into_iter().map(|item| (*item.0, item.1)).collect()));

named!(pub structure<Input, Structure>, map!(ws!(tuple!(tag!("struct"), ident, tag!("{"), structure_items, tag!("}"))), |tuple| Structure {
    name: *tuple.1,
    items: tuple.3,
}));

// function

named!(argument<Input, Argument>, map!(ws!(tuple!(opt!(mutable), ident, type_assign_op, ty)), |tuple| Argument {
    name    : *tuple.1,
    mutable : tuple.0.is_some(),
    ty      : tuple.3,
}));

named!(argument_list<Input, Vec<Argument>>, ws!(delimited!(tag!("("), separated_list_complete!(list_separator, argument), tag!(")"))));

named!(return_part<Input, Type>, ws!(preceded!(tag!("->"), ty)));

named!(signature<Input, Signature>, map!(ws!(tuple!(tag!("fn"), ident, argument_list, opt!(return_part))), |sig| Signature {
    name    : *sig.1,
    args    : sig.2,
    ret     : sig.3.unwrap_or(Type::void),
}));

named!(pub function<Input, Function>, map!(ws!(tuple!(signature, block)), |func| Function {
    sig: func.0,
    block: func.1,
}));
