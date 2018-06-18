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
    pub statements: Vec<Statement<'a>>,
    pub result: Option<Expression<'a>>,
}

#[derive(Debug)]
pub struct Structure<'a> {
    pub name    : &'a str,
    pub items   : HashMap<&'a str, Type<'a>>,
}

#[derive(Debug)]
pub struct Argument<'a> {
    pub name    : &'a str,
    pub mutable : bool,
    pub ty      : Type<'a>,
}

#[derive(Debug)]
pub struct Signature<'a> {
    pub name    : &'a str,
    pub args    : Vec<Argument<'a>>,
    pub ret     : Type<'a>,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub sig     : Signature<'a>,
    pub block   : Block<'a>,
}

#[derive(Debug)]
pub struct IdentPath<'a> {
    pub segs    : Vec<&'a str>,
}

#[derive(Debug)]
pub struct Call<'a> {
    pub path    : IdentPath<'a>,
    pub args    : Vec<Expression<'a>>,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Integer(&'a str),
    Float(&'a str),
    IdentPath(IdentPath<'a>),
    Call(Call<'a>),
    Operation(Operation<'a>),
    Block(Box<Block<'a>>),
    IfBlock(Box<IfBlock<'a>>),
}

#[derive(Debug, Copy, Clone)]
pub enum Operator {
    Add, Sub,
    Mul, Div, Rem
}

impl Operator {
    pub fn from_char(op: char) -> Option<Self> {
        match op {
            '+' => Some(Operator::Add),
            '-' => Some(Operator::Sub),
            '*' => Some(Operator::Mul),
            '/' => Some(Operator::Div),
            '%' => Some(Operator::Rem),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Operation<'a> {
    Binary(Operator, Box<Expression<'a>>, Box<Expression<'a>>),
    Prefix(Operator, Box<Expression<'a>>),
    Suffix(Operator, Box<Expression<'a>>),
}

#[derive(Debug)]
pub struct Binding<'a> {
    pub name: &'a str,
    pub expr: Expression<'a>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Function(Function<'a>),
    Structure(Structure<'a>),
    Binding(Binding<'a>),
    IfBlock(IfBlock<'a>),
}

#[derive(Debug)]
pub struct IfBlock<'a> {
    pub cond        : Expression<'a>,
    pub if_block    : Block<'a>,
    pub else_block  : Option<Block<'a>>,
}