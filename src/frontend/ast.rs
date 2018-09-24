//! AST datastructures generated by the parser and processed by the resolver.

use std::collections::HashMap;
use std::fmt::{self, Debug};
use crate::frontend::util::{BindingId, FunctionId, ScopeId, Integer, TypeSlot};

#[derive(Debug)]
pub enum Statement<'a> {
    Binding(Binding<'a>),
    Function(Function<'a>),
    Structure(Structure<'a>),
    ForLoop(ForLoop<'a>),
    WhileLoop(WhileLoop<'a>),
    IfBlock(IfBlock<'a>),
    Block(Block<'a>),
    Return(Return<'a>),
    Expression(Expression<'a>),
}

impl<'a> Statement<'a> {
    /// Returns whether the statement could also be an expression. Notably, an expression could not be since Statement::Expression is ; terminated
    pub fn is_expressable(self: &Self) -> bool {
        match self {
            Statement::IfBlock(_) | Statement::Block(_) => true,
            _ => false,
        }
    }
    /// Converts the statement into an expression or panics if the conversion would be invalid.
    pub fn into_expression(self: Self) -> Expression<'a> {
        match self {
            Statement::IfBlock(if_block)        => Expression::IfBlock(Box::new(if_block)),
            Statement::Block(block)             => Expression::Block(Box::new(block)),
            Statement::Expression(expression)   => expression,
            _                                   => panic!("invalid statement to expression conversion"),
        }
    }
}

#[derive(Debug)]
pub struct Binding<'a> {
    pub name        : &'a str,
    pub mutable     : bool,
    pub expr        : Option<Expression<'a>>,
    pub ty          : Option<Type<'a>>,
    pub type_id     : TypeSlot,
    pub binding_id  : Option<BindingId>,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub sig         : Signature<'a>,
    pub block       : Block<'a>,
    pub function_id : Option<FunctionId>,
    pub scope_id    : Option<ScopeId>,
}

#[derive(Debug)]
pub struct Signature<'a> {
    pub name    : &'a str,
    pub args    : Vec<Binding<'a>>,
    pub ret     : Option<Type<'a>>,
}

impl<'a> Signature<'a> {
    pub fn ret_resolved(self: &Self) -> bool {
        self.ret.as_ref().map_or(true, |ret| !ret.type_id.is_unresolved())
    }
    pub fn args_resolved(self: &Self) -> bool {
        self.args.iter().fold(true, |acc, arg| acc && !arg.type_id.is_unresolved())
    }
}

#[derive(Debug)]
pub struct Type<'a> {
    pub name    : IdentPath<'a>,
    pub type_id : TypeSlot,
}

impl<'a> Type<'a> {
    /// Returns a type with the given name and an unresolved type-id.
    pub fn unknown(name: IdentPath<'a>) -> Self {
        Type {
            name    : name,
            type_id : TypeSlot::Unresolved,
        }
    }
}

#[derive(Debug)]
pub struct Structure<'a> {
    pub name    : &'a str,
    pub items   : HashMap<&'a str, Type<'a>>,
    pub type_id : TypeSlot,
}

#[derive(Debug)]
pub struct ForLoop<'a> {
    pub iter    : Binding<'a>,
    pub range   : Expression<'a>,
    pub block   : Block<'a>,
    pub scope_id: Option<ScopeId>,
}

#[derive(Debug)]
pub struct WhileLoop<'a> {
    pub expr    : Expression<'a>,
    pub block   : Block<'a>,
    pub scope_id: Option<ScopeId>,
}

#[derive(Debug)]
pub struct Return<'a> {
    pub expr    : Option<Expression<'a>>,
}

#[derive(Debug)]
pub struct IfBlock<'a> {
    pub cond        : Expression<'a>,
    pub if_block    : Block<'a>,
    pub else_block  : Option<Block<'a>>,
    pub scope_id    : Option<ScopeId>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub statements  : Vec<Statement<'a>>,
    pub result      : Option<Expression<'a>>,
    pub type_id     : TypeSlot,
    pub scope_id    : Option<ScopeId>,
}

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
    pub fn get_type_id(self: &Self) -> TypeSlot {
        match self {
            Expression::Literal(literal)        => literal.type_id,
            Expression::Variable(variable)      => variable.type_id,
            Expression::Call(call)              => call.type_id,
            Expression::Assignment(assignment)  => assignment.left.type_id,
            Expression::BinaryOp(binary_op)     => binary_op.type_id,
            Expression::UnaryOp(unary_op)       => unary_op.type_id,
            Expression::Block(block)            => block.result.as_ref().map_or(TypeSlot::Void, |r| r.get_type_id()),
            Expression::IfBlock(if_block)       => if_block.if_block.result.as_ref().map_or(TypeSlot::Void, |r| r.get_type_id()),
        }
    }
}

impl<'a> Debug for Expression<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Literal(literal)        => write!(f, "{:#?}", literal),
            Expression::Variable(variable)      => write!(f, "{:#?}", variable),
            Expression::Call(call)              => write!(f, "{:#?}", call),
            Expression::Assignment(assignment)  => write!(f, "{:#?}", assignment),
            Expression::BinaryOp(binary_op)     => write!(f, "{:#?}", binary_op),
            Expression::UnaryOp(unary_op)       => write!(f, "{:#?}", unary_op),
            Expression::Block(block)            => write!(f, "{:#?}", block),
            Expression::IfBlock(if_block)       => write!(f, "{:#?}", if_block),
        }
    }
}

#[derive(Debug)]
pub struct Literal<'a> {
    pub value   : LiteralValue<'a>,
    pub ty      : Option<Type<'a>>,
    pub type_id : TypeSlot,
}

pub enum LiteralValue<'a> {
    Bool(bool),
    Integer(Integer),
    Float(f64),
    String(&'a str),  // TODO: string literals
}

impl<'a> LiteralValue<'a> {
    pub fn as_string(self: &Self) -> Option<&'a str> {
        match self {
            LiteralValue::String(string) => Some(string),
            _ => None,
        }
    }
    pub fn as_float(self: &Self) -> Option<f64> {
        match self {
            LiteralValue::Float(float) => Some(*float),
            _ => None,
        }
    }
    pub fn as_integer(self: &Self) -> Option<Integer> {
        match self {
            LiteralValue::Integer(integer) => Some(*integer),
            _ => None,
        }
    }
}

impl<'a> Debug for LiteralValue<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralValue::Bool(boolean) => write!(f, "LiteralValue({:?})", boolean),
            LiteralValue::Integer(integer) => write!(f, "LiteralValue({:?})", integer),
            LiteralValue::Float(float) => write!(f, "LiteralValue(Float({:?}))", float),
            LiteralValue::String(string) => write!(f, "LiteralValue({:?})", string),
        }
    }
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub path        : IdentPath<'a>,
    pub type_id     : TypeSlot, // todo: just get from binding?
    pub binding_id  : Option<BindingId>,
}

#[derive(Debug)]
pub struct Call<'a> {
    pub path        : IdentPath<'a>,
    pub args        : Vec<Expression<'a>>,
    pub type_id     : TypeSlot, // todo: just get from function?
    pub function_id : Option<FunctionId>,
    pub rust_fn_index: Option<u16>,
}

impl<'a> Call<'a> {
    /// Returns whether the return type has been resolved.
    pub fn ret_resolved(self: &Self) -> bool {
        self.type_id.is_unresolved()
    }
    /// Returns whether the argument list has been resolved.
    pub fn args_resolved(self: &Self) -> bool {
        self.args.iter().fold(true, |acc, arg| acc && !arg.get_type_id().is_unresolved())
    }
}

#[derive(Debug)]
pub struct Assignment<'a> {
    pub op      : BinaryOperator,
    pub left    : Variable<'a>,
    pub right   : Expression<'a>,
}

#[derive(Debug)]
pub struct BinaryOp<'a> {
    pub op      : BinaryOperator,
    pub left    : Expression<'a>,
    pub right   : Expression<'a>,
    pub type_id : TypeSlot,
}

#[derive(Debug)]
pub struct UnaryOp<'a> {
    pub op      : UnaryOperator,
    pub exp     : Expression<'a>,
    pub type_id : TypeSlot,
}

pub struct IdentPath<'a>(pub Vec<&'a str>);

impl<'a> Debug for IdentPath<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IdentPath({})", self.0.join("."))
    }
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOperator {
    // boolean
    Not,
    // inc/dec
    IncBefore, DecBefore, // IncAfter, DecAfter, // TODO: postfix ops
}

impl UnaryOperator {
    pub fn prefix_from_string(op: &str) -> Self {
        match op {

            "!" => UnaryOperator::Not,
            "++" => UnaryOperator::IncBefore,
            "--" => UnaryOperator::DecBefore,

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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOperator {
    // arithmetic
    Add, Sub, Mul, Div, Rem,
    // assigments
    Assign, AddAssign, SubAssign, MulAssign, DivAssign, RemAssign,
    // comparison
    Less, Greater, LessOrEq, GreaterOrEq, Equal, NotEqual,
    // boolean
    And, Or,
    // special
    Range,
}

impl BinaryOperator {
    pub fn from_string(op: &str) -> Self {
        match op {

            "=" => BinaryOperator::Assign,
            "+" => BinaryOperator::Add,
            "-" => BinaryOperator::Sub,
            "*" => BinaryOperator::Mul,
            "/" => BinaryOperator::Div,
            "%" => BinaryOperator::Rem,

            "&&" => BinaryOperator::And,
            "||" => BinaryOperator::Or,

            "<" => BinaryOperator::Less,
            ">" => BinaryOperator::Greater,
            "<=" => BinaryOperator::LessOrEq,
            ">=" => BinaryOperator::GreaterOrEq,
            "==" => BinaryOperator::Equal,
            "!=" => BinaryOperator::NotEqual,

            "+=" => BinaryOperator::AddAssign,
            "-=" => BinaryOperator::SubAssign,
            "*=" => BinaryOperator::MulAssign,
            "/=" => BinaryOperator::DivAssign,
            "%=" => BinaryOperator::RemAssign,

            ".." => BinaryOperator::Range,

            _ => panic!(format!("parser yielded invalid binary operator \"{}\"", op)),
        }
    }
}