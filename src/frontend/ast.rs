use std::collections::HashMap;
use frontend::{Unresolved, TypeId};

pub type Program<'a> = Vec<Statement<'a>>;

#[derive(Debug)]
pub enum Statement<'a> {
    Function(Function<'a>),
    Structure(Structure<'a>),
    Binding(Binding<'a>),
    IfBlock(IfBlock<'a>),
    ForLoop(ForLoop<'a>),
    Block(Block<'a>),
    Expression(Expression<'a>),
}

#[derive(Debug)]
pub struct Type<'a> {
    pub name    : IdentPath<'a>,
    pub type_id : Unresolved<TypeId>,
}

pub fn unknown_type_id() -> Unresolved<TypeId> {
    Unresolved::Unknown
}

impl<'a> Type<'a> {
    pub fn unknown(name: IdentPath<'a>) -> Self {
        Type {
            name    : name,
            type_id : unknown_type_id(),
        }
    }
}

#[derive(Debug)]
pub struct Binding<'a> {
    pub name    : &'a str,
    pub expr    : Expression<'a>,
    // TODO: also needs explicit type support ty: Option<Type<'a>>
    pub type_id : Unresolved<TypeId>,
}

#[derive(Debug)]
pub struct IfBlock<'a> {
    pub cond        : Expression<'a>,
    pub if_block    : Block<'a>,
    pub else_block  : Option<Block<'a>>,
}

#[derive(Debug)]
pub struct ForLoop<'a> {
    pub iter    : &'a str,
    pub range   : Expression<'a>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub statements  : Vec<Statement<'a>>,
    pub result      : Option<Expression<'a>>,
    pub type_id     : Unresolved<TypeId>,
}

#[derive(Debug)]
pub struct Structure<'a> {
    pub name    : &'a str,
    pub items   : HashMap<&'a str, Type<'a>>,
    pub type_id : Unresolved<TypeId>,
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
    pub ret     : Option<Type<'a>>,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub sig     : Signature<'a>,
    pub block   : Block<'a>,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
    IdentPath(IdentPath<'a>),
    Call(Call<'a>),
    Assignment(Operator, IdentPath<'a>, Box<Expression<'a>>),
    BinaryOp(Operator, Box<Expression<'a>>, Box<Expression<'a>>),
    UnaryOp(Operator, Box<Expression<'a>>),
    Block(Box<Block<'a>>),
    IfBlock(Box<IfBlock<'a>>),
}

#[derive(Debug)]
pub struct IdentPath<'a> {
    pub segs: Vec<&'a str>,
}

#[derive(Debug)]
pub struct Call<'a> {
    pub path: IdentPath<'a>,
    pub args: Vec<Expression<'a>>,
}

#[derive(Debug)]
pub enum Literal<'a> {
    String(&'a str),  // TODO: string literals
    Signed(i64),
    Unsigned(u64),
    Float(f64),
}

#[derive(Debug, Copy, Clone)]
pub enum Operator {
    // arithmetic
    Add, Sub, Mul, Div, Rem,
    // assigments
    Assign, AddAssign, SubAssign, MulAssign, DivAssign, RemAssign,
    // comparison
    Less, Greater, LessOrEq, GreaterOrEq, Equal, NotEqual,
    // boolean
    And, Or, Not,
    // special
    Range,
    // inc/dec
    IncBefore, DecBefore, // IncAfter, DecAfter, // TODO: postfix ops
}

impl Operator {
    pub fn binary_from_string(op: &str) -> Self {
        match op {

            "=" => Operator::Assign,
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Rem,

            "&&" => Operator::And,
            "||" => Operator::Or,

            "<" => Operator::Less,
            ">" => Operator::Greater,
            "<=" => Operator::LessOrEq,
            ">=" => Operator::GreaterOrEq,
            "==" => Operator::Equal,
            "!=" => Operator::NotEqual,

            "+=" => Operator::AddAssign,
            "-=" => Operator::SubAssign,
            "*=" => Operator::MulAssign,
            "/=" => Operator::DivAssign,
            "%=" => Operator::RemAssign,

            ".." => Operator::Range,

            _ => panic!(format!("parser yielded invalid binary operator \"{}\"", op)),
        }
    }
    pub fn prefix_from_string(op: &str) -> Self {
        match op {

            "!" => Operator::Not,
            "++" => Operator::IncBefore,
            "--" => Operator::DecBefore,

            _ => panic!(format!("parser yielded invalid prefix operator \"{}\"", op)),
        }
    }
    /*pub fn suffix_from_string(op: &str) -> Self { / TODO: postfix ops
        match op {

            "++" => Operator::IncAfter,
            "--" => Operator::DecAfter,

            _ => panic!(format!("parser yielded invalid prefix operator \"{}\"", op)),
        }
    }*/
}