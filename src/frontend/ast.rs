//! AST datastructures generated by the parser and processed by the resolver.

use std::{fmt::{self, Debug, Display}, collections::HashMap};
use crate::{StackAddress, ItemCount};
use crate::shared::typed_ids::{BindingId, FunctionId, ScopeId, TypeId};
use crate::shared::numeric::Numeric;
use crate::shared::Progress;

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

/// TypeId handling for typeable AST structures.
pub(crate) trait Typeable {
    /// Returns a mutable reference to the type_id.
    fn type_id_mut(self: &mut Self) -> &mut Option<TypeId>;
    /// Returns the type_id.
    fn type_id(self: &Self) -> Option<TypeId>;
}

/// Implements the Typeable trait for given structure.
macro_rules! impl_typeable {
    ($struct_name:ident) => {
        impl<'a> Typeable for $struct_name<'a> {
            fn type_id_mut(self: &mut Self) -> &mut Option<TypeId> {
                &mut self.type_id
            }
            fn type_id(self: &Self) -> Option<TypeId> {
                self.type_id
            }
        }
    };
}

/// Resolvable AST structures.
pub(crate) trait Resolvable {
    /// Number of resolved and total items
    fn num_resolved(self: &Self) -> Progress;
    /// Whether the structure is fully resolved
    fn is_resolved(self: &Self) -> bool {
        self.num_resolved().done()
    }
}

/// A position in the source code.
pub(crate) type Position = u32;

/// Source code position handling for AST structures associated with a position.
pub(crate) trait Positioned {
    /// Returns the structure's position.
    fn position(self: &Self) -> Position;
}

/// Implements the Position trait for given structure.
macro_rules! impl_positioned {
    ($struct_name:ident) => {
        impl<'a> Positioned for $struct_name<'a> {
            fn position(self: &Self) -> Position {
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
        impl_matchall!($self, Statement, $val_name, $code, Binding, Function, StructDef, ImplBlock, ForLoop, WhileLoop, IfBlock, Block, Return, Expression)
    };
    ($self:ident, $enum_name:ident, $val_name:ident, $code:tt $(, $variant_name:ident)+) => {
        match $self {
            $(
                $enum_name::$variant_name($val_name) => $code,
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
    pub position: Position,
    pub name: &'a str,
}

impl<'a> Display for Ident<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl_positioned!(Ident);

#[derive(Debug)]
pub struct Path<'a> {
    pub position: Position,
    pub name: Vec<&'a str>,
}

impl<'a> Path<'a> {
    pub fn pop(self: &mut Self) -> &'a str {
        self.name.pop().unwrap()
    }
}

impl<'a> Display for Path<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name.join("::"))
    }
}

impl_positioned!(Path);

pub enum Statement<'a> {
    Binding(Binding<'a>),
    Function(Function<'a>),
    StructDef(StructDef<'a>),
    ImplBlock(ImplBlock<'a>),
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
    fn position(self: &Self) -> Position {
        impl_matchall!(self, Statement, item, { item.position() })
    }
}

impl<'a> Resolvable for Statement<'a> {
    fn num_resolved(self: &Self) -> Progress {
        impl_matchall!(self, Statement, item, { item.num_resolved() })
    }
}

impl<'a> Debug for Statement<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        impl_matchall!(self, Statement, item, { write!(f, "{:#?}", item) })
    }
}

#[derive(Debug)]
pub struct Binding<'a> {
    pub position    : Position,
    pub ident       : Ident<'a>,
    pub mutable     : bool,
    pub expr        : Option<Expression<'a>>,
    pub ty          : Option<InlineType<'a>>,
    pub binding_id  : Option<BindingId>,
}

impl_bindable!(Binding);
impl_positioned!(Binding);

impl<'a> Resolvable for Binding<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.as_ref().map_or(Progress::zero(), |expr| expr.num_resolved())
        + self.ty.as_ref().map_or(Progress::zero(), |ty| ty.num_resolved())
        + self.binding_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    pub position    : Position,
    pub sig         : Signature<'a>,
    pub block       : Block<'a>,
    pub function_id : Option<FunctionId>,
    pub scope_id    : Option<ScopeId>,
}

impl_positioned!(Function);

impl<'a> Resolvable for Function<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.sig.args.iter().fold(Progress::zero(), |acc, arg| acc + arg.num_resolved())
        + self.sig.ret.as_ref().map_or(Progress::zero(), |ret| ret.num_resolved())
        + self.block.num_resolved()
        + self.function_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct Signature<'a> {
    pub ident   : Ident<'a>,
    pub args    : Vec<Binding<'a>>,
    pub ret     : Option<InlineType<'a>>,
}

#[derive(Debug)]
pub struct TypeName<'a> {
    pub path    : Path<'a>,
    pub type_id : Option<TypeId>,
}

impl_typeable!(TypeName);

impl<'a> Resolvable for TypeName<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

impl<'a> TypeName<'a> {
    /// Returns a type with the given name and an unresolved type-id.
    pub fn from_path(path: Path<'a>) -> Self {
        TypeName {
            path    : path,
            type_id : None,
        }
    }
    pub fn from_str(name: &'a str, position: Position) -> Self {
        TypeName {
            path    : Path { name: vec! [ name ], position: position },
            type_id : None,
        }
    }
}

impl<'a> Positioned for TypeName<'a> {
    fn position(self: &Self) -> Position {
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
            Self::TypeName(typename) => typename.type_id,
            Self::Array(array) => array.type_id,
        }
    }
}

impl<'a> Typeable for InlineType<'a> {
    fn type_id(self: &Self) -> Option<TypeId> {
        match &self {
            InlineType::Array(array) => array.type_id(),
            InlineType::TypeName(type_name) => type_name.type_id(),
        }
    }
    fn type_id_mut(self: &mut Self) -> &mut Option<TypeId> {
        match self {
            InlineType::Array(array) => array.type_id_mut(),
            InlineType::TypeName(type_name) => type_name.type_id_mut(),
        }
    }
}

impl<'a> Resolvable for InlineType<'a> {
    fn num_resolved(self: &Self) -> Progress {
        match self {
            Self::TypeName(typename) => typename.num_resolved(),
            Self::Array(array) => array.num_resolved(),
        }
    }
}

#[derive(Debug)]
pub struct Array<'a> {
    pub position    : Position,
    pub element_type: InlineType<'a>,
    pub len         : StackAddress,
    pub type_id     : Option<TypeId>,
}

impl_positioned!(Array);
impl_typeable!(Array);

impl<'a> Resolvable for Array<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.element_type.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct StructDef<'a> {
    pub position: Position,
    pub ident   : Ident<'a>,
    pub fields  : Vec<(&'a str, InlineType<'a>)>,
    pub type_id : Option<TypeId>,
}
impl_positioned!(StructDef);

impl<'a> Typeable for StructDef<'a> {
    fn type_id(self: &Self) -> Option<TypeId> {
        self.type_id
    }
    fn type_id_mut(self: &mut Self) -> &mut Option<TypeId> {
        &mut self.type_id
    }
}

impl<'a> Resolvable for StructDef<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.fields.iter().fold(Progress::zero(), |acc, (_, field)| acc + field.num_resolved())
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct ImplBlock<'a> {
    pub position    : Position,
    pub functions   : Vec<Function<'a>>,
    pub scope_id    : Option<ScopeId>,
    pub ty          : InlineType<'a>,
}

impl_positioned!(ImplBlock);

impl<'a> Resolvable for ImplBlock<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.functions.iter().fold(Progress::zero(), |acc, function| acc + function.num_resolved())
        + self.ty.num_resolved()
    }
}

#[derive(Debug)]
pub struct ForLoop<'a> {
    pub position: Position,
    pub iter    : Binding<'a>,
    pub expr    : Expression<'a>,
    pub block   : Block<'a>,
    pub scope_id: Option<ScopeId>,
}

impl_positioned!(ForLoop);

impl<'a> Resolvable for ForLoop<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.iter.num_resolved()
        + self.expr.num_resolved()
        + self.block.num_resolved()
    }
}

#[derive(Debug)]
pub struct WhileLoop<'a> {
    pub position: Position,
    pub expr    : Expression<'a>,
    pub block   : Block<'a>,
    pub scope_id: Option<ScopeId>,
}

impl_positioned!(WhileLoop);
impl_display!(WhileLoop, "while {} {{ ... }}", expr);

impl<'a> Resolvable for WhileLoop<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.num_resolved()
        + self.block.num_resolved()
    }
}

#[derive(Debug)]
pub struct Return<'a> {
    pub position        : Position,
    pub expr            : Option<Expression<'a>>,
}

impl_positioned!(Return);

impl<'a> Resolvable for Return<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.as_ref().map_or(Progress::zero(), |e| e.num_resolved())
    }
}

#[derive(Debug)]
pub struct IfBlock<'a> {
    pub position    : Position,
    pub cond        : Expression<'a>,
    pub if_block    : Block<'a>,
    pub else_block  : Option<Block<'a>>,
    pub scope_id    : Option<ScopeId>,
}

impl_positioned!(IfBlock);
impl_display!(IfBlock, "if {} {{ ... }}", cond);

impl<'a> Resolvable for IfBlock<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.cond.num_resolved()
        + self.if_block.num_resolved()
        + self.else_block.as_ref().map_or(Progress::zero(), |e| e.num_resolved())
    }
}

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

impl<'a> Typeable for IfBlock<'a> {
    fn type_id(self: &Self) -> Option<TypeId> {
        self.if_block.result.as_ref().map_or(None, |e| e.type_id())
    }
    fn type_id_mut(self: &mut Self) -> &mut Option<TypeId> {
        if let Some(result) = &mut self.if_block.result {
            result.type_id_mut()
        } else {
            panic!("attempted to set return type of if statement (not an expression)")
        }
    }
}

#[derive(Debug)]
pub struct Block<'a> {
    pub position    : Position,
    pub statements  : Vec<Statement<'a>>,
    pub result      : Option<Expression<'a>>,
    pub returns     : Option<Expression<'a>>,
    pub scope_id    : Option<ScopeId>,
}

impl_positioned!(Block);
impl_display!(Block, "{{ ... }}");

impl<'a> Resolvable for Block<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.statements.iter().fold(Progress::zero(), |acc, statement| acc + statement.num_resolved())
        + self.result.as_ref().map_or(Progress::zero(), |result| result.num_resolved())
        + self.result.as_ref().map_or(Progress::zero(), |returns| returns.num_resolved())
    }
}

impl<'a> Typeable for Block<'a> {
    fn type_id(self: &Self) -> Option<TypeId> {
        self.result.as_ref().map_or(None, |e| e.type_id())
    }
    fn type_id_mut(self: &mut Self) -> &mut Option<TypeId> {
        if let Some(result) = &mut self.result {
            result.type_id_mut()
        } else {
            panic!("attempted to set return type of block statement (not an expression)")
        }
    }
}

impl<'a> Returns for Block<'a> {
    fn returns(self: &Self) -> bool {
        self.returns.is_some() || self.result.as_ref().map_or(false, |result| result.returns()) || self.statements.iter().any(|statement| statement.returns())
    }
}

pub enum Expression<'a> {
    Literal(Literal<'a>),
    Variable(Variable<'a>),
    Call(Call<'a>),
    Member(Member<'a>), //FIXME this shouldn't be here. either implement an Operand enum for BinaryOp that contains it or change to MemberAccess and store left+right (more in line with Cast)
    Assignment(Box<Assignment<'a>>),
    BinaryOp(Box<BinaryOp<'a>>),
    UnaryOp(Box<UnaryOp<'a>>),
    Cast(Box<Cast<'a>>),
    Block(Box<Block<'a>>),
    IfBlock(Box<IfBlock<'a>>),
}

impl<'a> Expression<'a> {
    pub fn is_literal(self: &Self) -> bool {
        match self {
            Self::Literal(_) => true,
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
    pub fn is_assignment(self: &Self) -> bool {
        match self {
            Expression::Assignment(_) => true,
            _ => false,
        }
    }
    pub fn as_variable(self: &Self) -> Option<&Variable<'a>> {
        match self {
            Self::Variable(variable) => Some(variable),
            _ => None,
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

impl<'a> Typeable for Expression<'a> {
    fn type_id_mut(self: &mut Self) -> &mut Option<TypeId> {
        impl_matchall!(self, Expression, item, { item.type_id_mut() })
    }
    fn type_id(self: &Self) -> Option<TypeId> {
        impl_matchall!(self, Expression, item, { item.type_id() })
    }
}

impl<'a> Resolvable for Expression<'a> {
    fn is_resolved(self: &Self) -> bool {
        impl_matchall!(self, Expression, item, { item.is_resolved() })
    }
    fn num_resolved(self: &Self) -> Progress {
        impl_matchall!(self, Expression, item, { item.num_resolved() })
    }
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

impl<'a> Positioned for Expression<'a> {
    fn position(self: &Self) -> Position {
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
    pub position    : Position,
    pub value       : LiteralValue<'a>,
    pub type_name   : Option<TypeName<'a>>, // used in e.g. 1i8, 3.1415f32
    pub type_id     : Option<TypeId>,
}

impl_typeable!(Literal);
impl_positioned!(Literal);

impl<'a> Resolvable for Literal<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
        + self.value.num_resolved()
    }
}

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
    pub fn is_const(self: &Self) -> bool {
        match self {
            LiteralValue::Array(v) => !v.elements.iter().any(|e| !e.is_literal()),
            LiteralValue::Struct(v) => !v.fields.iter().any(|(_, e)| !e.is_literal()),
            _ => true,
        }
    }
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

impl<'a> Resolvable for LiteralValue<'a> {
    fn num_resolved(self: &Self) -> Progress {
        match self {
            Self::Array(array) => array.num_resolved(),
            Self::Struct(struct_) => struct_.num_resolved(),
            _ => Progress::new(1, 1),
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
    pub elements: Vec<Expression<'a>>, // TODO: struct/array literals containing expressions should be expressions themselves instead of literals
}

impl<'a> Resolvable for ArrayLiteral<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.elements.iter().fold(Progress::zero(), |acc, element| acc + element.num_resolved())
    }
}

#[derive(Debug)]
pub struct StructLiteral<'a> {
    pub fields: HashMap<&'a str, Expression<'a>>, // TODO: struct/array literals containing expressions should be expressions themselves instead of literals
}

impl<'a> Resolvable for StructLiteral<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.fields.iter().fold(Progress::zero(), |acc, (_, field)| acc + field.num_resolved())
    }
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub position    : Position,
    pub ident       : Ident<'a>,
    pub binding_id  : Option<BindingId>,
    pub type_id     : Option<TypeId>,
}

impl_typeable!(Variable);
impl_bindable!(Variable);
impl_positioned!(Variable);
impl_display!(Variable, "{}", ident);

impl<'a> Resolvable for Variable<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
        + self.binding_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct Member<'a> {
    pub position    : Position,
    pub ident       : Ident<'a>,
    pub type_id     : Option<TypeId>,
    pub index       : Option<ItemCount>,
}

impl_typeable!(Member);
impl_positioned!(Member);
impl_display!(Member, "{}", ident);

impl<'a> Resolvable for Member<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub enum CallType<'a> {
    Function,
    Method,
    Static(Path<'a>),
}

#[derive(Debug)]
pub struct Call<'a> {
    pub position        : Position,
    pub ident           : Ident<'a>,
    pub args            : Vec<Expression<'a>>,
    pub call_type       : CallType<'a>,
    pub function_id     : Option<FunctionId>,
    pub type_id         : Option<TypeId>,
}

impl_typeable!(Call);
impl_positioned!(Call);
impl_display!(Call, "{}({:?})", ident, args);

impl<'a> Resolvable for Call<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.args.iter().fold(Progress::zero(), |acc, arg| acc + arg.num_resolved())
        + self.function_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct Assignment<'a> {
    pub position: Position,
    pub op      : BinaryOperator,
    pub left    : Expression<'a>,
    pub right   : Expression<'a>,
    pub type_id : Option<TypeId>,
}

impl_typeable!(Assignment);
impl_positioned!(Assignment);
impl_display!(Assignment, "{} {} {}", left, op, right);

impl<'a> Resolvable for Assignment<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.left.num_resolved()
        + self.right.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

impl<'a> Returns for Assignment<'a> {
    fn returns(self: &Self) -> bool {
        self.left.returns() || self.right.returns()
    }
}

#[derive(Debug)]
pub struct Cast<'a> {
    pub position    : Position,
    pub expr        : Expression<'a>,
    pub ty          : TypeName<'a>,
    pub type_id     : Option<TypeId>,
}

impl_typeable!(Cast);
impl_positioned!(Cast);
impl_display!(Cast, "{} as {:?}", expr, ty);

impl<'a> Resolvable for Cast<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

impl<'a> Returns for Cast<'a> {
    fn returns(self: &Self) -> bool {
        self.expr.returns()
    }
}

#[derive(Debug)]
pub struct BinaryOp<'a> {
    pub position    : Position,
    pub op          : BinaryOperator,
    pub left        : Expression<'a>,
    pub right       : Expression<'a>,
    pub type_id     : Option<TypeId>,
}

impl_typeable!(BinaryOp);
impl_positioned!(BinaryOp);
impl_display!(BinaryOp, "{}{}{}", left, op, right);

impl<'a> Resolvable for BinaryOp<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.left.num_resolved()
        + self.right.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

impl<'a> Returns for BinaryOp<'a> {
    fn returns(self: &Self) -> bool {
        self.left.returns() || self.right.returns()
    }
}

#[derive(Debug)]
pub struct UnaryOp<'a> {
    pub position    : Position,
    pub op          : UnaryOperator,
    pub expr        : Expression<'a>,
    pub type_id     : Option<TypeId>,
}

impl_typeable!(UnaryOp);
impl_positioned!(UnaryOp);

impl<'a> Resolvable for UnaryOp<'a> {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

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

            _ => panic!("parser yielded invalid binary operator \"{}\"", op),
        }
    }
    pub fn is_simple(self: &Self) -> bool {
        use BinaryOperator as BO;
        match self {
            BO::Add | BO::Sub | BO::Mul | BO::Div | BO::Rem => true,
            BO::Less | BO::Greater | BO::LessOrEq | BO::GreaterOrEq | BO::Equal | BO::NotEqual => true,
            _ => false,
        }
    }
    pub fn is_shortcircuit(self: &Self) -> bool {
        use BinaryOperator as BO;
        match self {
            BO::And | BO::Or => true,
            _ => false,
        }
    }
    pub fn is_offset(self: &Self) -> bool {
        use BinaryOperator as BO;
        match self {
            BO::Index | BO::IndexWrite | BO::Access | BO::AccessWrite => true,
            _ => false,
        }
    }
    pub fn is_compound(self: &Self) -> bool {
        use BinaryOperator as BO;
        match self {
            BO::AddAssign | BO::SubAssign | BO::MulAssign | BO::DivAssign | BO::RemAssign => true,
            _ => false,
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
