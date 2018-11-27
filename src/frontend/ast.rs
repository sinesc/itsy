//! AST datastructures generated by the parser and processed by the resolver.

use std::collections::HashMap;
use std::fmt::{self, Debug};
use crate::frontend::util::{BindingId, FunctionId, ScopeId, Numeric, TypeId};

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

impl<'a> Debug for Statement<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Binding(v)   => write!(f, "{:#?}", v),
            Statement::Function(v)  => write!(f, "{:#?}", v),
            Statement::Structure(v) => write!(f, "{:#?}", v),
            Statement::ForLoop(v)   => write!(f, "{:#?}", v),
            Statement::WhileLoop(v) => write!(f, "{:#?}", v),
            Statement::IfBlock(v)   => write!(f, "{:#?}", v),
            Statement::Block(v)     => write!(f, "{:#?}", v),
            Statement::Return(v)    => write!(f, "{:#?}", v),
            Statement::Expression(v)=> write!(f, "Statement{:#?}", v),
        }
    }
}

#[derive(Debug)]
pub struct Binding<'a> {
    pub name        : &'a str,
    pub mutable     : bool,
    pub expr        : Option<Expression<'a>>,
    pub type_name   : Option<TypeName<'a>>,
    pub type_id     : Option<TypeId>,
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
    pub ret     : Option<TypeName<'a>>,
}

impl<'a> Signature<'a> {
    pub fn ret_resolved(self: &Self) -> bool {
        self.ret.as_ref().map_or(true, |ret| ret.type_id.is_some())
    }
    pub fn args_resolved(self: &Self) -> bool {
        self.args.iter().fold(true, |acc, arg| acc && arg.type_id.is_some())
    }
}

#[derive(Debug)]
pub struct TypeName<'a> {
    pub name    : IdentPath<'a>,
    pub type_id : Option<TypeId>,
}

impl<'a> TypeName<'a> {
    /// Returns a type with the given name and an unresolved type-id.
    pub fn unknown(name: IdentPath<'a>) -> Self {
        TypeName {
            name    : name,
            type_id : None,
        }
    }
}

#[derive(Debug)]
pub struct Structure<'a> {
    pub name    : &'a str,
    pub items   : HashMap<&'a str, TypeName<'a>>,
    pub type_id : Option<TypeId>,
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
    pub expr            : Option<Expression<'a>>,
    pub fn_ret_type_id  : Option<TypeId>,
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
    pub type_id     : Option<TypeId>,
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
    pub fn get_type_id(self: &Self) -> Option<TypeId> {
        match self {
            Expression::Literal(literal)        => literal.type_id,
            Expression::Variable(variable)      => variable.type_id,
            Expression::Call(call)              => call.type_id,
            Expression::Assignment(assignment)  => assignment.left.type_id,
            Expression::BinaryOp(binary_op)     => binary_op.type_id,
            Expression::UnaryOp(unary_op)       => unary_op.type_id,
            Expression::Block(block)            => block.result.as_ref().map_or(Some(TypeId::void()), |e| e.get_type_id()),
            Expression::IfBlock(if_block)       => if_block.if_block.result.as_ref().map_or(Some(TypeId::void()), |e| e.get_type_id()),
        }
    }
    pub fn is_resolved(self: &Self) -> bool {
        self.get_type_id().is_some()
    }
    pub fn is_literal(self: &Self) -> bool {
        match self {
            Expression::Literal(_) => true,
            _ => false,
        }
    }
    pub fn is_variable(self: &Self) -> bool {
        match self {
            Expression::Variable(_) => true,
            _ => false,
        }
    }
    pub fn as_literal(self: &Self) -> Option<&Literal<'a>> {
        match self {
            Expression::Literal(literal) => Some(literal),
            _ => None,
        }
    }
    pub fn as_binary_op(self: &Self) -> Option<&BinaryOp<'a>> {
        match self {
            Expression::BinaryOp(literal) => Some(literal),
            _ => None,
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
    pub value       : LiteralValue<'a>,
    pub type_name   : Option<TypeName<'a>>,
    pub type_id     : Option<TypeId>,
}

pub enum LiteralValue<'a> {
    Bool(bool),
    Numeric(Numeric),
    String(&'a str),
    Array(Array<'a>),
}

#[derive(Debug)]
pub struct Array<'a> {
    pub items       : Vec<Literal<'a>>,
    pub type_id     : Option<TypeId>,
    pub binding_id  : Option<BindingId>,
}

impl<'a> LiteralValue<'a> {
    pub fn as_string(self: &Self) -> Option<&'a str> {
        match self {
            LiteralValue::String(string) => Some(string),
            _ => None,
        }
    }
    pub fn as_numeric(self: &Self) -> Option<Numeric> {
        match self {
            LiteralValue::Numeric(v) => Some(*v),
            _ => None,
        }
    }
    pub fn as_bool(self: &Self) -> Option<bool> {
        match self {
            LiteralValue::Bool(v) => Some(*v),
            _ => None,
        }
    }
}

impl<'a> Debug for LiteralValue<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralValue::Bool(v) => write!(f, "{:?}", v),
            LiteralValue::Numeric(v) => write!(f, "{:?}", v),
            LiteralValue::String(v) => write!(f, "String({:?})", v),
            LiteralValue::Array(v) => write!(f, "Array({:#?})", v),
        }
    }
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub path        : IdentPath<'a>,
    pub type_id     : Option<TypeId>, // todo: just get from binding?
    pub binding_id  : Option<BindingId>,
}

#[derive(Debug)]
pub struct Call<'a> {
    pub path        : IdentPath<'a>,
    pub args        : Vec<Expression<'a>>,
    pub type_id     : Option<TypeId>, // todo: just get from function?
    pub function_id : Option<FunctionId>,
    pub rust_fn_index: Option<u16>,
}

impl<'a> Call<'a> {
    /// Returns whether the return type has been resolved.
    pub fn ret_resolved(self: &Self) -> bool {
        self.type_id.is_some()
    }
    /// Returns whether the argument list has been resolved.
    pub fn args_resolved(self: &Self) -> bool {
        self.args.iter().fold(true, |acc, arg| acc && arg.get_type_id().is_some())
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
    pub type_id : Option<TypeId>,
}

#[derive(Debug)]
pub struct UnaryOp<'a> {
    pub op      : UnaryOperator,
    pub expr    : Expression<'a>,
    pub type_id : Option<TypeId>,
}

pub struct IdentPath<'a>(pub Vec<&'a str>);

impl<'a> Debug for IdentPath<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IdentPath({})", self.0.join("."))
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOperator {
    // boolean
    Not,
    // inc/dec
    IncBefore, DecBefore,
    IncAfter, DecAfter,
}

impl UnaryOperator {
    pub fn prefix_from_string(op: &str) -> Self {
        match op {
            "!" => UnaryOperator::Not,
            "++" => UnaryOperator::IncBefore,
            "--" => UnaryOperator::DecBefore,
            _ => panic!("parser yielded invalid prefix operator \"{}\"", op),
        }
    }
    pub fn suffix_from_string(op: &str) -> Self {
        match op {
            "++" => UnaryOperator::IncAfter,
            "--" => UnaryOperator::DecAfter,
            _ => panic!("parser yielded invalid prefix operator \"{}\"", op),
        }
    }
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
    Range, Index
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