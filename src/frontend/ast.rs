//! AST datastructures generated by the parser and processed by the resolver.

use std::{fmt::{self, Debug, Display}, collections::HashMap, cell::Cell};
use crate::util::{BindingId, FunctionId, ScopeId, Numeric, FnKind, TypeId, HeapRef};

/// BindingId handling for bindable AST structures.
pub(crate) trait Bindable {
    /// Returns a mutable reference to the binding_id.
    fn binding_id_mut(self: &mut Self) -> &mut Option<BindingId>;
    /// Returns the binding_id.
    fn binding_id(self: &Self) -> Option<BindingId>;
    /// Sets the binding_id or panics if it is already set.
    fn set_binding_id(self: &mut Self, binding_id: BindingId) {
        let current_binding_id = self.binding_id_mut();
        if current_binding_id.is_none() {
            *current_binding_id = Some(binding_id);
        } else {
            panic!("attempted to reassign binding_id");
        }
    }
}

/// Implements the Bindable trait for given structure.
macro_rules! impl_bindable {
    ($struct_name:ident) => {
        impl<'a> Bindable for $struct_name<'a> {
            fn binding_id_mut(self: &mut Self) -> &mut Option<BindingId> {
                &mut self.binding_id
            }
            fn binding_id(self: &Self) -> Option<BindingId> {
                self.binding_id
            }
        }
    };
}

/// Source code position handling for AST structures associated with a position.
pub(crate) trait Positioned {
    /// Returns the structure's position.
    fn position(self: &Self) -> u32;
}

/// Implements the Position trait for given structure.
macro_rules! impl_positioned {
    ($struct_name:ident) => {
        impl<'a> Positioned for $struct_name<'a> {
            fn position(self: &Self) -> u32 {
                self.position
            }
        }
    };
}

/// Implements the Display trait for given structure.
macro_rules! impl_display {
    ($struct_name:ident, $format:literal $(, $field:ident)*) => {
        impl<'a> Display for $struct_name<'a> {
            fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, $format $(, self.$field)*)
            }
        }
    };
}

/// Implements a match block with cases for all variants of Expression or Statement
macro_rules! impl_matchall {
    ($self:ident, Expression, $val_name:ident, $code:tt) => {
        impl_matchall!($self, Expression, $val_name, $code, Literal, Variable, Call, Member, Assignment, BinaryOp, UnaryOp, Cast, Block, IfBlock)
    };
    ($self:ident, Statement, $val_name:ident, $code:tt) => {
        impl_matchall!($self, Statement, $val_name, $code, Binding, Function, Structure, ForLoop, WhileLoop, IfBlock, Block, Return, Expression)
    };
    ($self:ident, $struct_name:ident, $val_name:ident, $code:tt $(, $field:ident)+) => {
        match $self {
            $(
                $struct_name::$field($val_name) => $code,
            )+
        }
    };
}

/// Provides information whether this AST structure causes an unconditional function return.
pub(crate) trait Returns {
    /// Returns true if this structure unconditionally causes the parent function to return.
    fn returns(self: &Self) -> bool;
}

#[derive(Debug)]
pub struct Ident<'a> {
    pub position: u32,
    pub name: &'a str,
}
impl_positioned!(Ident);
impl_display!(Ident, "{}", name);

#[derive(Debug)]
pub struct Path<'a> {
    pub position: u32,
    pub name: Vec<&'a str>,
}

impl<'a> Path<'a> {
    pub fn pop(self: &mut Self) -> &'a str {
        self.name.pop().unwrap()
    }
}

impl_positioned!(Path);

pub enum Statement<'a> {
    Binding(Binding<'a>),
    Function(Function<'a>),
    Structure(Struct<'a>),
    ForLoop(ForLoop<'a>),
    WhileLoop(WhileLoop<'a>),
    IfBlock(IfBlock<'a>),
    Block(Block<'a>),
    Return(Return<'a>),
    Expression(Expression<'a>),
}

impl<'a> Statement<'a> {
    /// Returns whether the statement could also be an expression. Notably, an expression could not be since Statement::Expression is ; terminated
    pub fn maybe_expression(self: &Self) -> bool {
        match self {
            Statement::IfBlock(_) | Statement::Block(_) => true,
            _ => false,
        }
    }
    /// Converts the statement into an expression or panics if the conversion would be invalid.
    pub fn into_expression(self: Self) -> Option<Expression<'a>> {
        match self {
            Statement::IfBlock(if_block)        => Some(Expression::IfBlock(Box::new(if_block))),
            Statement::Block(block)             => Some(Expression::Block(Box::new(block))),
            Statement::Expression(expression)   => Some(expression),
            Statement::Return(ret)              => Some(ret.expr.unwrap()),
            _                                   => None,
        }
    }
}

impl<'a> Returns for Statement<'a> {
    fn returns(self: &Self) -> bool {
        match self {
            Statement::Return(_)    => true,
            Statement::IfBlock(v)   => v.returns(),
            Statement::Block(v)     => v.returns(),
            Statement::Expression(v)=> v.returns(),
            _                       => false,
        }
    }
}

impl<'a> Positioned for Statement<'a> {
    fn position(self: &Self) -> u32 {
        impl_matchall!(self, Statement, item, { item.position() })
    }
}

impl<'a> Debug for Statement<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        impl_matchall!(self, Statement, item, { write!(f, "{:#?}", item) })
    }
}

#[derive(Debug)]
pub struct Binding<'a> {
    pub position    : u32,
    pub ident       : Ident<'a>,
    pub mutable     : bool,
    pub expr        : Option<Expression<'a>>,
    pub ty          : Option<InlineType<'a>>,
    pub binding_id  : Option<BindingId>,
}
impl_bindable!(Binding);
impl_positioned!(Binding);

#[derive(Debug)]
pub struct Function<'a> {
    pub position    : u32,
    pub sig         : Signature<'a>,
    pub block       : Block<'a>,
    pub function_id : Option<FunctionId>,
    pub scope_id    : Option<ScopeId>,
}
impl_positioned!(Function);

#[derive(Debug)]
pub struct Signature<'a> {
    pub ident   : Ident<'a>,
    pub args    : Vec<Binding<'a>>,
    pub ret     : Option<InlineType<'a>>,
}

impl<'a> Signature<'a> {
    pub fn ret_resolved(self: &Self) -> bool {
        self.ret.as_ref().map_or(true, |ret| ret.type_id().is_some())
    }
    pub fn ret_type_id(self: &Self) -> Option<TypeId> {
        self.ret.as_ref().map_or(Some(TypeId::void()), |ret| ret.type_id())
    }
    pub fn args_resolved(self: &Self) -> bool {
        self.args.iter().fold(true, |acc, arg| acc && arg.ty.as_ref().map_or(false, |type_name| type_name.type_id().is_some()))
    }
    pub fn arg_type_ids(self: &Self) -> Vec<Option<TypeId>> {
        self.args.iter().map(|arg| arg.ty.as_ref().map_or(None, |type_name| type_name.type_id())).collect()
    }
}

#[derive(Debug)]
pub struct TypeName<'a> {
    pub path    : Path<'a>,
    pub type_id : Option<TypeId>,
}

impl<'a> TypeName<'a> {
    /// Returns a type with the given name and an unresolved type-id.
    pub fn from_path(path: Path<'a>) -> Self {
        TypeName {
            path    : path,
            type_id : None,
        }
    }
    pub fn from_str(name: &'a str, position: u32) -> Self {
        TypeName {
            path    : Path { name: vec! [ name ], position: position },
            type_id : None,
        }
    }
}

impl<'a> Positioned for TypeName<'a> {
    fn position(self: &Self) -> u32 {
        self.path.position
    }
}

#[derive(Debug)]
pub enum InlineType<'a> {
    TypeName(TypeName<'a>),
    Array(Box<Array<'a>>),
}

impl<'a> InlineType<'a> {
    pub fn type_id(self: &Self) -> Option<TypeId> {
        match self {
            InlineType::TypeName(type_name) => type_name.type_id,
            InlineType::Array(array) => array.type_id,
        }
    }
}

#[derive(Debug)]
pub struct Array<'a> {
    pub position    : u32,
    pub element_type: InlineType<'a>,
    pub len         : u32,
    pub type_id     : Option<TypeId>,
}
impl_positioned!(Array);

#[derive(Debug)]
pub struct Struct<'a> {
    pub position: u32,
    pub ident   : Ident<'a>,
    pub fields  : Vec<(&'a str, InlineType<'a>)>,
    pub type_id : Option<TypeId>,
}
impl_positioned!(Struct);

#[derive(Debug)]
pub struct ForLoop<'a> {
    pub position: u32,
    pub iter    : Binding<'a>,
    pub range   : Expression<'a>,
    pub block   : Block<'a>,
    pub scope_id: Option<ScopeId>,
}
impl_positioned!(ForLoop);

#[derive(Debug)]
pub struct WhileLoop<'a> {
    pub position: u32,
    pub expr    : Expression<'a>,
    pub block   : Block<'a>,
    pub scope_id: Option<ScopeId>,
}
impl_positioned!(WhileLoop);
impl_display!(WhileLoop, "while {} {{ ... }}", expr);

#[derive(Debug)]
pub struct Return<'a> {
    pub position        : u32,
    pub expr            : Option<Expression<'a>>,
    pub fn_ret_type_id  : Option<TypeId>,
}
impl_positioned!(Return);

#[derive(Debug)]
pub struct IfBlock<'a> {
    pub position    : u32,
    pub cond        : Expression<'a>,
    pub if_block    : Block<'a>,
    pub else_block  : Option<Block<'a>>,
    pub scope_id    : Option<ScopeId>,
}
impl_positioned!(IfBlock);
impl_display!(IfBlock, "if {} {{ ... }}", cond);

impl<'a> Returns for IfBlock<'a> {
    fn returns(self: &Self) -> bool {
        if self.cond.returns() {
            true
        } else if let Some(else_block) = &self.else_block {
            self.if_block.returns() && else_block.returns()
        } else {
            false
        }
    }
}

impl<'a> Bindable for IfBlock<'a> {
    fn binding_id_mut(self: &mut Self) -> &mut Option<BindingId> {
        if let Some(result) = &mut self.if_block.result {
            result.binding_id_mut()
        } else {
            panic!("attempted to set return type of if statement")
        }
    }
    fn binding_id(self: &Self) -> Option<BindingId> {
        self.if_block.result.as_ref().map_or(None, |e| e.binding_id())
    }
}

#[derive(Debug)]
pub struct Block<'a> {
    pub position    : u32,
    pub statements  : Vec<Statement<'a>>,
    pub result      : Option<Expression<'a>>,
    pub returns     : Option<Expression<'a>>,
    pub scope_id    : Option<ScopeId>,
}
impl_positioned!(Block);
impl_display!(Block, "{{ ... }}");

impl<'a> Returns for Block<'a> {
    fn returns(self: &Self) -> bool {
        self.returns.is_some() || self.result.as_ref().map_or(false, |result| result.returns()) || self.statements.iter().any(|statement| statement.returns())
    }
}

impl<'a> Bindable for Block<'a> {
    fn binding_id_mut(self: &mut Self) -> &mut Option<BindingId> {
        if let Some(result) = &mut self.result {
            result.binding_id_mut()
        } else {
            panic!("attempted to set return binding of block statement (not an expression)")
        }
    }
    fn binding_id(self: &Self) -> Option<BindingId> {
        self.result.as_ref().map_or(None, |e| e.binding_id())
    }
}

pub enum Expression<'a> {
    Literal(Literal<'a>),
    Variable(Variable<'a>),
    Call(Call<'a>),
    Member(Member<'a>),
    Assignment(Box<Assignment<'a>>),
    BinaryOp(Box<BinaryOp<'a>>),
    UnaryOp(Box<UnaryOp<'a>>),
    Cast(Box<Cast<'a>>),
    Block(Box<Block<'a>>),
    IfBlock(Box<IfBlock<'a>>),
}

impl<'a> Returns for Expression<'a> {
    fn returns(self: &Self) -> bool {
        match self {
            Expression::Assignment(v)   => v.returns(),
            Expression::BinaryOp(v)     => v.returns(),
            Expression::UnaryOp(v)      => v.returns(),
            Expression::Cast(v)         => v.returns(),
            Expression::Block(v)        => v.returns(),
            Expression::IfBlock(v)      => v.returns(),
            _                           => false,
        }
    }
}

impl<'a> Expression<'a> {
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
    pub fn is_call(self: &Self) -> bool {
        match self {
            Expression::Call(_) => true,
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
            Expression::BinaryOp(binary_op) => Some(binary_op),
            _ => None,
        }
    }
    pub fn as_member(self: &Self) -> Option<&Member<'a>> {
        match self {
            Expression::Member(member) => Some(member),
            _ => None,
        }
    }
    pub fn as_member_mut(self: &mut Self) -> Option<&mut Member<'a>> {
        match self {
            Expression::Member(member) => Some(member),
            _ => None,
        }
    }
}

impl<'a> Bindable for Expression<'a> {
    fn binding_id_mut(self: &mut Self) -> &mut Option<BindingId> {
        impl_matchall!(self, Expression, item, { item.binding_id_mut() })
    }
    fn binding_id(self: &Self) -> Option<BindingId> {
        impl_matchall!(self, Expression, item, { item.binding_id() })
    }
}

impl<'a> Positioned for Expression<'a> {
    fn position(self: &Self) -> u32 {
        impl_matchall!(self, Expression, item, { item.position() })
    }
}

impl<'a> Debug for Expression<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        impl_matchall!(self, Expression, item, { write!(f, "{:#?}", item) })
    }
}

impl<'a> Display for Expression<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        impl_matchall!(self, Expression, item, { write!(f, "{}", item) })
    }
}

#[derive(Debug)]
pub struct Literal<'a> {
    pub position    : u32,
    pub value       : LiteralValue<'a>,
    pub type_name   : Option<TypeName<'a>>, // used in e.g. 1i8, 3.1415f32
    pub binding_id  : Option<BindingId>,
    pub heap_ref    : Cell<Option<HeapRef>>,
}
impl_bindable!(Literal);
impl_positioned!(Literal);

impl<'a> Display for Literal<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            LiteralValue::Bool(v) => write!(f, "{:?}", v),
            LiteralValue::Numeric(v) => write!(f, "{:?}", v),
            LiteralValue::String(v) => write!(f, "{:?}", v),
            LiteralValue::Array(_) => write!(f, "[ ]"),
            LiteralValue::Struct(_) => write!(f, "struct {{ ... }}"), // don't have the struct name here :-(
        }
    }
}

pub enum LiteralValue<'a> {
    Bool(bool),
    Numeric(Numeric),
    String(&'a str),
    Array(ArrayLiteral<'a>),
    Struct(StructLiteral<'a>),
}

impl<'a> LiteralValue<'a> {
    pub fn as_string(self: &Self) -> Option<&'a str> {
        match self {
            LiteralValue::String(v) => Some(v),
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
    pub fn as_array(self: &Self) -> Option<&ArrayLiteral> {
        match self {
            LiteralValue::Array(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_array_mut(self: &mut Self) -> Option<&mut ArrayLiteral<'a>> {
        match self {
            LiteralValue::Array(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_struct_mut(self: &mut Self) -> Option<&mut StructLiteral<'a>> {
        match self {
            LiteralValue::Struct(v) => Some(v),
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
            LiteralValue::Struct(v) => write!(f, "Struct({:#?})", v),
        }
    }
}

#[derive(Debug)]
pub struct ArrayLiteral<'a> {
    pub elements: Vec<Literal<'a>>, // todo: eventually need to support expressions here
}

#[derive(Debug)]
pub struct StructLiteral<'a> {
    pub fields: HashMap<&'a str, Literal<'a>>, // todo: eventually need to support expressions here
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub position    : u32,
    pub ident       : Ident<'a>,
    pub binding_id  : Option<BindingId>,
}
impl_bindable!(Variable);
impl_positioned!(Variable);
impl_display!(Variable, "{}", ident);

#[derive(Debug)]
pub struct Member<'a> {
    pub position    : u32,
    pub ident       : Ident<'a>,
    pub binding_id  : Option<BindingId>,
    pub index       : Option<u32>,
}
impl_bindable!(Member);
impl_positioned!(Member);
impl_display!(Member, "{}", ident);

#[derive(Debug)]
pub enum CallType<'a> {
    Function,
    Method(Box<Expression<'a>>), // todo: maybe box call in expression instead?
    Static(Path<'a>),
}

#[derive(Debug)]
pub struct Call<'a> {
    pub position        : u32,
    pub ident           : Ident<'a>,
    pub args            : Vec<Expression<'a>>,
    pub call_type       : CallType<'a>,
    pub call_kind       : FnKind,
    pub function_id     : Option<FunctionId>,
    pub binding_id      : Option<BindingId>,
}
impl_bindable!(Call);
impl_positioned!(Call);
impl_display!(Call, "{}({:?})", ident, args);

#[derive(Debug)]
pub struct Assignment<'a> {
    pub position: u32,
    pub op      : BinaryOperator,
    pub left    : Expression<'a>,
    pub right   : Expression<'a>,
    pub binding_id: Option<BindingId>,
}
impl_bindable!(Assignment);
impl_positioned!(Assignment);
impl_display!(Assignment, "{} {} {}", left, op, right);

impl<'a> Returns for Assignment<'a> {
    fn returns(self: &Self) -> bool {
        self.left.returns() || self.right.returns()
    }
}

#[derive(Debug)]
pub struct Cast<'a> {
    pub position    : u32,
    pub expr        : Expression<'a>,
    pub ty          : TypeName<'a>,
    pub binding_id  : Option<BindingId>,
}
impl_bindable!(Cast);
impl_positioned!(Cast);
impl_display!(Cast, "{} as {:?}", expr, ty);

impl<'a> Returns for Cast<'a> {
    fn returns(self: &Self) -> bool {
        self.expr.returns()
    }
}

#[derive(Debug)]
pub struct BinaryOp<'a> {
    pub position    : u32,
    pub op          : BinaryOperator,
    pub left        : Expression<'a>,
    pub right       : Expression<'a>,
    pub binding_id  : Option<BindingId>,
}
impl_bindable!(BinaryOp);
impl_positioned!(BinaryOp);
impl_display!(BinaryOp, "{}{}{}", left, op, right);

impl<'a> Returns for BinaryOp<'a> {
    fn returns(self: &Self) -> bool {
        self.left.returns() || self.right.returns()
    }
}

#[derive(Debug)]
pub struct UnaryOp<'a> {
    pub position    : u32,
    pub op          : UnaryOperator,
    pub expr        : Expression<'a>,
    pub binding_id  : Option<BindingId>,
}
impl_bindable!(UnaryOp);
impl_positioned!(UnaryOp);

impl<'a> Display for UnaryOp<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            UnaryOperator::DecAfter | UnaryOperator::DecBefore => write!(f, "{}{}", self.expr, self.op),
            _ => write!(f, "{}{}", self.op, self.expr),
        }
    }
}

impl<'a> Returns for UnaryOp<'a> {
    fn returns(self: &Self) -> bool {
        self.expr.returns()
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

impl Display for UnaryOperator {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            UnaryOperator::Not => "!",
            UnaryOperator::IncBefore | UnaryOperator::IncAfter => "++",
            UnaryOperator::DecBefore | UnaryOperator::DecAfter => "--",
        })
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
    // iterator
    Range, RangeInclusive,
    // data offsets
    Index, IndexWrite, Access, AccessWrite,
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
            "..=" => BinaryOperator::RangeInclusive,
            "." => BinaryOperator::Access,

            _ => panic!(format!("parser yielded invalid binary operator \"{}\"", op)),
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            BinaryOperator::Assign          => " = ",
            BinaryOperator::Add             => " + ",
            BinaryOperator::Sub             => " - ",
            BinaryOperator::Mul             => " * ",
            BinaryOperator::Div             => " / ",
            BinaryOperator::Rem             => " % ",

            BinaryOperator::And             => " && ",
            BinaryOperator::Or              => " || ",

            BinaryOperator::Less            => " < ",
            BinaryOperator::Greater         => " > ",
            BinaryOperator::LessOrEq        => " <= ",
            BinaryOperator::GreaterOrEq     => " >= ",
            BinaryOperator::Equal           => " == ",
            BinaryOperator::NotEqual        => " != ",

            BinaryOperator::AddAssign       => " += ",
            BinaryOperator::SubAssign       => " -= ",
            BinaryOperator::MulAssign       => " *= ",
            BinaryOperator::DivAssign       => " /= ",
            BinaryOperator::RemAssign       => " %= ",

            BinaryOperator::Range           => "..",
            BinaryOperator::RangeInclusive  => "..=",
            BinaryOperator::Access          => ".",
            BinaryOperator::AccessWrite     => ".",
            BinaryOperator::Index           => "[] ",
            BinaryOperator::IndexWrite      => "[] ",
        })
    }
}
