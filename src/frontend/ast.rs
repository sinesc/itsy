use std::collections::HashMap;
use util::{BindingId, TypeId};
use frontend::Unresolved;

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
pub struct Binding<'a> {
    pub name        : &'a str,
    pub expr        : Option<Expression<'a>>,
    pub ty          : Option<Type<'a>>,
    pub type_id     : Unresolved<TypeId>,
    pub binding_id  : Option<BindingId>,
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
pub struct Function<'a> {
    pub sig     : Signature<'a>,
    pub block   : Block<'a>,
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
pub struct Type<'a> {
    pub name    : IdentPath<'a>, // TODO: ident_path is x.y.z, but should probably be something like x::y::z with namespacing supported or just an ident otherwise
    pub type_id : Unresolved<TypeId>,
}

pub fn unknown_type_id() -> Unresolved<TypeId> {
    Unresolved::Unknown
}

pub fn void_type_id() -> Unresolved<TypeId> {
    Unresolved::Resolved((0).into())
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
pub enum Expression<'a> {
    Literal(Literal<'a>),
    Variable(Variable<'a>),
    Call(Call<'a>),
    Assignment(Box<Assignment<'a>>),
    BinaryOp(Box<BinaryOp<'a>>),
    UnaryOp(Box<UnaryOp<'a>>),
    Block(Box<Block<'a>>),
    IfBlock(Box<IfBlock<'a>>),
}

impl<'a> Expression<'a> {
    pub fn get_type_id(self: &Self) -> Unresolved<TypeId> {
        match self {
            Expression::Literal(literal)        => literal.type_id,
            Expression::Variable(variable)      => variable.type_id,
            Expression::Call(call)              => call.type_id,
            Expression::Assignment(assignment)  => assignment.left.type_id,
            Expression::BinaryOp(binary_op)     => binary_op.type_id,
            Expression::UnaryOp(unary_op)       => unary_op.type_id,
            Expression::Block(block)            => block.result.as_ref().map_or(void_type_id(), |r| r.get_type_id()),
            Expression::IfBlock(if_block)       => if_block.if_block.result.as_ref().map_or(void_type_id(), |r| r.get_type_id()),
        }
    }
}

#[derive(Debug)]
pub struct Literal<'a> {
    pub value   : LiteralValue<'a>,
    pub type_id : Unresolved<TypeId>,
}

#[derive(Debug)]
pub enum LiteralValue<'a> {
    String(&'a str),  // TODO: string literals
    Signed(i64),
    Unsigned(u64),
    Float(f64),
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub path        : IdentPath<'a>,
    pub type_id     : Unresolved<TypeId>,
    pub binding_id  : Option<BindingId>,
}

#[derive(Debug)]
pub struct Call<'a> {
    pub path    : IdentPath<'a>,
    pub args    : Vec<Expression<'a>>,
    pub type_id : Unresolved<TypeId>,
}

#[derive(Debug)]
pub struct Assignment<'a> {
    pub op      : Operator,
    pub left    : Variable<'a>,
    pub right   : Expression<'a>,
}

#[derive(Debug)]
pub struct BinaryOp<'a> {
    pub op      : Operator,
    pub left    : Expression<'a>,
    pub right   : Expression<'a>,
    pub type_id : Unresolved<TypeId>,
}

#[derive(Debug)]
pub struct UnaryOp<'a> {
    pub op      : Operator,
    pub exp     : Expression<'a>,
    pub type_id : Unresolved<TypeId>,
}

#[derive(Debug)]
pub struct IdentPath<'a> {
    pub segs: Vec<&'a str>,
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