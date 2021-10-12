//! AST datastructures generated by the parser and processed by the resolver.

use std::{fmt::{self, Debug, Display}, collections::HashMap};
use crate::{ItemIndex, StackAddress, shared::BindingContainer};
use crate::shared::{typed_ids::{BindingId, FunctionId, ScopeId, TypeId}, numeric::Numeric, Progress, parts_to_path};

/// TypeId handling for typeable AST structures.
pub(crate) trait Typeable {
    /// Returns the type_id.
    fn type_id(self: &Self, bindings: &impl BindingContainer) -> Option<TypeId>;
    /// Returns a mutable reference to the type_id.
    fn type_id_mut<'t>(self: &'t mut Self, bindings: &'t mut impl BindingContainer) -> &'t mut Option<TypeId>;
}

/// Implements the Typeable trait for given structure.
macro_rules! impl_typeable {
    ($struct_name:ident) => {
        impl Typeable for $struct_name {
            fn type_id(self: &Self, _: &impl BindingContainer) -> Option<TypeId> {
                self.type_id
            }
            fn type_id_mut(self: &mut Self, _: &mut impl BindingContainer) -> &mut Option<TypeId> {   // FIXME: why is this compatible to the trait?
                &mut self.type_id
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
#[derive(Copy, Clone, Debug)]
pub struct Position(pub(crate) usize);

impl Position {
    /// Create a new position. The source is required since a position is internally an absolute offset from the end of a string.
    pub fn new(input: &str, offset: usize) -> Self {
        Self(input.len() - offset)
    }
    /// Compute 1-based line/column number from Position (absolute offset from end) in string.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        let offset = input.len() - self.0;
        let mut parsed = &input[0..offset];
        let mut line = 1;
        while { // can't use let parsed.lines() here as a line-break at the end is ignored
            let mut break_char = '\0';
            if let Some(nl) = parsed.find(|c| if c == '\n' || c == '\r' { break_char = c; true } else { false }) {
                parsed = &parsed[nl+1..];
                if break_char == '\r' && parsed.starts_with('\n') { // skip \n after \r on windows
                    parsed = &parsed[1..];
                }
                line += 1;
                true
            } else {
                false
            }
        } {}
        (line, parsed.len() as u32 + 1)
    }
}

/// Source code position handling for AST structures associated with a position.
pub(crate) trait Positioned {
    /// Returns the structure's position.
    fn position(self: &Self) -> Position;
}

/// Implements the Position trait for given structure.
macro_rules! impl_positioned {
    ($struct_name:ident) => {
        impl Positioned for $struct_name {
            fn position(self: &Self) -> Position {
                self.position
            }
        }
    };
}

/// Implements the Display trait for given structure.
macro_rules! impl_display {
    ($struct_name:ident, $format:literal $(, $field:ident)*) => {
        impl Display for $struct_name {
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
        impl_matchall!($self, Statement, $val_name, $code, Binding, Function, StructDef, ImplBlock, ForLoop, WhileLoop, IfBlock, Block, Return, Expression, Module, Use)
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
pub struct Ident {
    pub position: Position,
    pub name: String,
}

impl Display for Ident {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl_positioned!(Ident);

#[derive(Debug)]
pub struct Path {
    pub position: Position,
    pub name: Vec<String>,
}

impl Path {
    pub fn pop(self: &mut Self) -> String {
        self.name.pop().unwrap()
    }
}

impl Display for Path {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", parts_to_path(&self.name))
    }
}

impl_positioned!(Path);

pub enum Statement {
    Binding(Binding),
    Function(Function),
    StructDef(StructDef),
    ImplBlock(ImplBlock),
    ForLoop(ForLoop),
    WhileLoop(WhileLoop),
    IfBlock(IfBlock),
    Block(Block),
    Return(Return),
    Expression(Expression),
    Module(Module),
    Use(Use),
}

impl Statement {
    /// Returns whether the statement could also be an expression. Notably, an expression could not be since Statement::Expression is ; terminated
    pub fn is_expression(self: &Self) -> bool {
        match self {
            Statement::IfBlock(if_block) => if_block.if_block.result.is_some(),
            Statement::Block(block) => block.result.is_some(),
            _ => false,
        }
    }
    /// Converts the statement into an expression or panics if the conversion would be invalid.
    pub fn into_expression(self: Self) -> Option<Expression> {
        match self {
            Statement::IfBlock(if_block)        => Some(Expression::IfBlock(Box::new(if_block))),
            Statement::Block(block)             => Some(Expression::Block(Box::new(block))),
            Statement::Expression(expression)   => Some(expression),
            Statement::Return(ret)              => Some(ret.expr.unwrap()),
            _                                   => None,
        }
    }
}

impl Returns for Statement {
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

impl Positioned for Statement {
    fn position(self: &Self) -> Position {
        impl_matchall!(self, Statement, item, { item.position() })
    }
}

impl Resolvable for Statement {
    fn num_resolved(self: &Self) -> Progress {
        impl_matchall!(self, Statement, item, { item.num_resolved() })
    }
}

impl Debug for Statement {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        impl_matchall!(self, Statement, item, { write!(f, "{:#?}", item) })
    }
}

#[derive(Debug)]
pub struct Module {
    pub position    : Position,
    pub ident       : Ident,
}

impl_positioned!(Module);

impl Module {
    /// Returns the module name as a string
    pub fn name(self: &Self) -> &str {
        &self.ident.name
    }
}

impl Resolvable for Module {
    fn num_resolved(self: &Self) -> Progress {
        Progress::new(1, 1)
    }
}

#[derive(Debug)]
pub struct Use {
    pub position    : Position,
    pub mapping     : HashMap<String, String>,
}

impl_positioned!(Use);

impl Resolvable for Use {
    fn num_resolved(self: &Self) -> Progress {
        Progress::new(0, 1) // TODO
    }
}

#[derive(Debug)]
pub struct Binding {
    pub position    : Position,
    pub ident       : Ident,
    pub mutable     : bool,
    pub expr        : Option<Expression>,
    pub ty          : Option<InlineType>,
    pub binding_id  : Option<BindingId>,
}

impl_positioned!(Binding);

impl Typeable for Binding {
    fn type_id(self: &Self, bindings: &impl BindingContainer) -> Option<TypeId> {
        match self.binding_id {
            Some(binding_id) => bindings.binding_by_id(binding_id).type_id,
            None => None,
        }
    }
    fn type_id_mut<'t>(self: &'t mut Self, bindings: &'t mut impl BindingContainer) -> &'t mut Option<TypeId> {
        match self.binding_id {
            Some(binding_id) => &mut bindings.binding_by_id_mut(binding_id).type_id,
            None => unreachable!(),
        }
    }
}

impl Resolvable for Binding {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.as_ref().map_or(Progress::zero(), |expr| expr.num_resolved())
        + self.ty.as_ref().map_or(Progress::zero(), |ty| ty.num_resolved())
        + self.binding_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct Function {
    pub position    : Position,
    pub sig         : Signature,
    pub block       : Block,
    pub function_id : Option<FunctionId>,
    pub scope_id    : Option<ScopeId>,
}

impl_positioned!(Function);

impl Resolvable for Function {
    fn num_resolved(self: &Self) -> Progress {
        self.sig.args.iter().fold(Progress::zero(), |acc, arg| acc + arg.num_resolved())
        + self.sig.ret.as_ref().map_or(Progress::zero(), |ret| ret.num_resolved())
        + self.block.num_resolved()
        + self.function_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct Signature {
    pub ident   : Ident,
    pub args    : Vec<Binding>,
    pub ret     : Option<InlineType>,
}

impl Signature {
    pub(crate) fn ret_resolved(self: &Self, bindings: &impl BindingContainer) -> bool {
        self.ret.as_ref().map_or(true, |ret| ret.type_id(bindings).is_some())
    }
    pub(crate) fn ret_type_id(self: &Self, bindings: &impl BindingContainer) -> Option<TypeId> {
        self.ret.as_ref().map_or(Some(TypeId::void()), |ret| ret.type_id(bindings))
    }
    pub(crate) fn args_resolved(self: &Self, bindings: &impl BindingContainer) -> bool {
        !self.args.iter().any(|arg| arg.ty.as_ref().map_or(true, |type_name| type_name.type_id(bindings).is_none()))
    }
    pub(crate) fn arg_type_ids(self: &Self, bindings: &impl BindingContainer) -> Vec<Option<TypeId>> {
        self.args.iter().map(|arg| arg.ty.as_ref().map_or(None, |type_name| type_name.type_id(bindings))).collect()
    }
}

#[derive(Debug)]
pub struct TypeName {
    pub path    : Path,
    pub type_id : Option<TypeId>,
}

impl_typeable!(TypeName);

impl Resolvable for TypeName {
    fn num_resolved(self: &Self) -> Progress {
        self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

impl TypeName {
    /// Returns a type with the given name and an unresolved type-id.
    pub fn from_path(path: Path) -> Self {
        TypeName {
            path    : path,
            type_id : None,
        }
    }
    pub fn from_str(name: &str, position: Position) -> Self {
        TypeName {
            path    : Path { name: vec! [ name.to_string() ], position: position },
            type_id : None,
        }
    }
}

impl Positioned for TypeName {
    fn position(self: &Self) -> Position {
        self.path.position
    }
}

#[derive(Debug)]
pub enum InlineType {
    TypeName(TypeName),
    Array(Box<Array>),
}

impl Typeable for InlineType {
    fn type_id(self: &Self, bindings: &impl BindingContainer) -> Option<TypeId> {
        match &self {
            InlineType::Array(array) => array.type_id(bindings),
            InlineType::TypeName(type_name) => type_name.type_id(bindings),
        }
    }
    fn type_id_mut<'t>(self: &'t mut Self, bindings: &'t mut impl BindingContainer) -> &'t mut Option<TypeId> {
        match self {
            InlineType::Array(array) => array.type_id_mut(bindings),
            InlineType::TypeName(type_name) => type_name.type_id_mut(bindings),
        }
    }
}

impl Resolvable for InlineType {
    fn num_resolved(self: &Self) -> Progress {
        match self {
            Self::TypeName(typename) => typename.num_resolved(),
            Self::Array(array) => array.num_resolved(),
        }
    }
}

#[derive(Debug)]
pub struct Array {
    pub position    : Position,
    pub element_type: InlineType,
    pub len         : StackAddress,
    pub type_id     : Option<TypeId>,
}

impl_positioned!(Array);
impl_typeable!(Array);

impl Resolvable for Array {
    fn num_resolved(self: &Self) -> Progress {
        self.element_type.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct StructDef {
    pub position: Position,
    pub ident   : Ident,
    pub fields  : Vec<(String, InlineType)>,
    pub type_id : Option<TypeId>,
}
impl_positioned!(StructDef);

impl Typeable for StructDef {
    fn type_id(self: &Self, _: &impl BindingContainer) -> Option<TypeId> {
        self.type_id
    }
    fn type_id_mut(self: &mut Self, _: &mut impl BindingContainer) -> &mut Option<TypeId> {
        &mut self.type_id
    }
}

impl Resolvable for StructDef {
    fn num_resolved(self: &Self) -> Progress {
        self.fields.iter().fold(Progress::zero(), |acc, (_, field)| acc + field.num_resolved())
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct ImplBlock {
    pub position    : Position,
    pub functions   : Vec<Function>,
    pub scope_id    : Option<ScopeId>,
    pub ty          : InlineType,
}

impl_positioned!(ImplBlock);

impl Resolvable for ImplBlock {
    fn num_resolved(self: &Self) -> Progress {
        self.functions.iter().fold(Progress::zero(), |acc, function| acc + function.num_resolved())
        + self.ty.num_resolved()
    }
}

#[derive(Debug)]
pub struct ForLoop {
    pub position: Position,
    pub iter    : Binding,
    pub expr    : Expression,
    pub block   : Block,
    pub scope_id: Option<ScopeId>,
}

impl_positioned!(ForLoop);

impl Resolvable for ForLoop {
    fn num_resolved(self: &Self) -> Progress {
        self.iter.num_resolved()
        + self.expr.num_resolved()
        + self.block.num_resolved()
    }
}

#[derive(Debug)]
pub struct WhileLoop {
    pub position: Position,
    pub expr    : Expression,
    pub block   : Block,
    pub scope_id: Option<ScopeId>,
}

impl_positioned!(WhileLoop);
impl_display!(WhileLoop, "while {} {{ ... }}", expr);

impl Resolvable for WhileLoop {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.num_resolved()
        + self.block.num_resolved()
    }
}

#[derive(Debug)]
pub struct Return {
    pub position        : Position,
    pub expr            : Option<Expression>,
}

impl_positioned!(Return);

impl Resolvable for Return {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.as_ref().map_or(Progress::zero(), |e| e.num_resolved())
    }
}

#[derive(Debug)]
pub struct IfBlock {
    pub position    : Position,
    pub cond        : Expression,
    pub if_block    : Block,
    pub else_block  : Option<Block>,
    pub scope_id    : Option<ScopeId>,
}

impl_positioned!(IfBlock);
impl_display!(IfBlock, "if {} {{ ... }}", cond);

impl Resolvable for IfBlock {
    fn num_resolved(self: &Self) -> Progress {
        self.cond.num_resolved()
        + self.if_block.num_resolved()
        + self.else_block.as_ref().map_or(Progress::zero(), |e| e.num_resolved())
    }
}

impl Returns for IfBlock {
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

impl Typeable for IfBlock {
    fn type_id(self: &Self, bindings: &impl BindingContainer) -> Option<TypeId> {
        self.if_block.result.as_ref().map_or(None, |e| e.type_id(bindings))
    }
    fn type_id_mut<'t>(self: &'t mut Self, bindings: &'t mut impl BindingContainer) -> &'t mut Option<TypeId> {
        if let Some(result) = &mut self.if_block.result {
            result.type_id_mut(bindings)
        } else {
            panic!("attempted to set return type of if statement (not an expression)")
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub position    : Position,
    pub statements  : Vec<Statement>,
    pub result      : Option<Expression>,
    pub returns     : Option<Expression>,
    pub scope_id    : Option<ScopeId>,
}

impl_positioned!(Block);
impl_display!(Block, "{{ ... }}");

impl Resolvable for Block {
    fn num_resolved(self: &Self) -> Progress {
        self.statements.iter().fold(Progress::zero(), |acc, statement| acc + statement.num_resolved())
        + self.result.as_ref().map_or(Progress::zero(), |result| result.num_resolved())
        + self.result.as_ref().map_or(Progress::zero(), |returns| returns.num_resolved())
    }
}

impl Typeable for Block {
    fn type_id(self: &Self, bindings: &impl BindingContainer) -> Option<TypeId> {
        self.result.as_ref().map_or(None, |e| e.type_id(bindings))
    }
    fn type_id_mut<'t>(self: &'t mut Self, bindings: &'t mut impl BindingContainer) -> &'t mut Option<TypeId> {
        if let Some(result) = &mut self.result {
            result.type_id_mut(bindings)
        } else {
            panic!("attempted to set return type of block statement (not an expression)")
        }
    }
}

impl Returns for Block {
    fn returns(self: &Self) -> bool {
        self.returns.is_some() || self.result.as_ref().map_or(false, |result| result.returns()) || self.statements.iter().any(|statement| statement.returns())
    }
}

pub enum Expression {
    Literal(Literal),
    Variable(Variable),
    Call(Call),
    Member(Member), //FIXME this shouldn't be here. either implement an Operand enum for BinaryOp that contains it or change to MemberAccess and store left+right (more in line with Cast)
    Assignment(Box<Assignment>),
    BinaryOp(Box<BinaryOp>),
    UnaryOp(Box<UnaryOp>),
    Cast(Box<Cast>),
    Block(Box<Block>),
    IfBlock(Box<IfBlock>),
}

impl Expression {
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
    pub fn as_variable(self: &Self) -> Option<&Variable> {
        match self {
            Self::Variable(variable) => Some(variable),
            _ => None,
        }
    }
    pub fn as_literal(self: &Self) -> Option<&Literal> {
        match self {
            Expression::Literal(literal) => Some(literal),
            _ => None,
        }
    }
    pub fn as_binary_op(self: &Self) -> Option<&BinaryOp> {
        match self {
            Expression::BinaryOp(binary_op) => Some(binary_op),
            _ => None,
        }
    }
    pub fn as_member(self: &Self) -> Option<&Member> {
        match self {
            Expression::Member(member) => Some(member),
            _ => None,
        }
    }
    pub fn as_member_mut(self: &mut Self) -> Option<&mut Member> {
        match self {
            Expression::Member(member) => Some(member),
            _ => None,
        }
    }
}

impl Typeable for Expression {
    fn type_id(self: &Self, bindings: &impl BindingContainer) -> Option<TypeId> {
        impl_matchall!(self, Expression, item, { item.type_id(bindings) })
    }
    fn type_id_mut<'t>(self: &'t mut Self, bindings: &'t mut impl BindingContainer) -> &'t mut Option<TypeId> {
        impl_matchall!(self, Expression, item, { item.type_id_mut(bindings) })
    }
}

impl Resolvable for Expression {
    fn is_resolved(self: &Self) -> bool {
        impl_matchall!(self, Expression, item, { item.is_resolved() })
    }
    fn num_resolved(self: &Self) -> Progress {
        impl_matchall!(self, Expression, item, { item.num_resolved() })
    }
}

impl Returns for Expression {
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

impl Positioned for Expression {
    fn position(self: &Self) -> Position {
        impl_matchall!(self, Expression, item, { item.position() })
    }
}

impl Debug for Expression {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        impl_matchall!(self, Expression, item, { write!(f, "{:#?}", item) })
    }
}

impl Display for Expression {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        impl_matchall!(self, Expression, item, { write!(f, "{}", item) })
    }
}

#[derive(Debug)]
pub struct Literal {
    pub position    : Position,
    pub value       : LiteralValue,
    pub type_name   : Option<TypeName>, // used in e.g. 1i8, 3.1415f32
    pub type_id     : Option<TypeId>,
}

impl_typeable!(Literal);
impl_positioned!(Literal);

impl Resolvable for Literal {
    fn num_resolved(self: &Self) -> Progress {
        self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
        + self.value.num_resolved()
    }
}

impl Display for Literal {
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

pub enum LiteralValue {
    Bool(bool),
    Numeric(Numeric),
    String(String),
    Array(ArrayLiteral),
    Struct(StructLiteral),
}

impl LiteralValue {
    pub fn is_const(self: &Self) -> bool {
        match self {
            LiteralValue::Array(v) => !v.elements.iter().any(|e| !e.is_literal()),
            LiteralValue::Struct(v) => !v.fields.iter().any(|(_, e)| !e.is_literal()),
            _ => true,
        }
    }
    pub fn as_string(self: &Self) -> Option<&str> {
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
    pub fn as_array_mut(self: &mut Self) -> Option<&mut ArrayLiteral> {
        match self {
            LiteralValue::Array(v) => Some(v),
            _ => None,
        }
    }
    pub fn as_struct_mut(self: &mut Self) -> Option<&mut StructLiteral> {
        match self {
            LiteralValue::Struct(v) => Some(v),
            _ => None,
        }
    }
}

impl Resolvable for LiteralValue {
    fn num_resolved(self: &Self) -> Progress {
        match self {
            Self::Array(array) => array.num_resolved(),
            Self::Struct(struct_) => struct_.num_resolved(),
            _ => Progress::new(1, 1),
        }
    }
}

impl Debug for LiteralValue {
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
pub struct ArrayLiteral {
    pub elements: Vec<Expression>, // TODO: struct/array literals containing expressions should be expressions themselves instead of literals
}

impl Resolvable for ArrayLiteral {
    fn num_resolved(self: &Self) -> Progress {
        self.elements.iter().fold(Progress::zero(), |acc, element| acc + element.num_resolved())
    }
}

#[derive(Debug)]
pub struct StructLiteral {
    pub fields: HashMap<String, Expression>, // TODO: struct/array literals containing expressions should be expressions themselves instead of literals
}

impl Resolvable for StructLiteral {
    fn num_resolved(self: &Self) -> Progress {
        self.fields.iter().fold(Progress::zero(), |acc, (_, field)| acc + field.num_resolved())
    }
}

#[derive(Debug)]
pub struct Variable {
    pub position    : Position,
    pub ident       : Ident,
    pub binding_id  : Option<BindingId>,
}

impl_positioned!(Variable);
impl_display!(Variable, "{}", ident);

impl Typeable for Variable {
    fn type_id(self: &Self, bindings: &impl BindingContainer) -> Option<TypeId> {
        match self.binding_id {
            Some(binding_id) => bindings.binding_by_id(binding_id).type_id,
            None => None,
        }
    }
    fn type_id_mut<'t>(self: &'t mut Self, bindings: &'t mut impl BindingContainer) -> &'t mut Option<TypeId> {
        match self.binding_id {
            Some(binding_id) => &mut bindings.binding_by_id_mut(binding_id).type_id,
            None => unreachable!(),
        }
    }
}

impl Resolvable for Variable {
    fn num_resolved(self: &Self) -> Progress {
        self.binding_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct Member {
    pub position    : Position,
    pub ident       : Ident,
    pub type_id     : Option<TypeId>,
    pub index       : Option<ItemIndex>,
}

impl_typeable!(Member);
impl_positioned!(Member);
impl_display!(Member, "{}", ident);

impl Resolvable for Member {
    fn num_resolved(self: &Self) -> Progress {
        self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub enum CallType {
    Function,
    Method,
    Static(Path),
}

#[derive(Debug)]
pub struct Call {
    pub position        : Position,
    pub ident           : Ident,
    pub args            : Vec<Expression>,
    pub call_type       : CallType,
    pub function_id     : Option<FunctionId>,
    pub type_id         : Option<TypeId>,
}

impl_typeable!(Call);
impl_positioned!(Call);
impl_display!(Call, "{}({:?})", ident, args);

impl Resolvable for Call {
    fn num_resolved(self: &Self) -> Progress {
        self.args.iter().fold(Progress::zero(), |acc, arg| acc + arg.num_resolved())
        + self.function_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

#[derive(Debug)]
pub struct Assignment {
    pub position: Position,
    pub op      : BinaryOperator,
    pub left    : Expression,
    pub right   : Expression,
    pub type_id : Option<TypeId>,
}

impl_typeable!(Assignment);
impl_positioned!(Assignment);
impl_display!(Assignment, "{} {} {}", left, op, right);

impl Resolvable for Assignment {
    fn num_resolved(self: &Self) -> Progress {
        self.left.num_resolved()
        + self.right.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

impl Returns for Assignment {
    fn returns(self: &Self) -> bool {
        self.left.returns() || self.right.returns()
    }
}

#[derive(Debug)]
pub struct Cast {
    pub position    : Position,
    pub expr        : Expression,
    pub ty          : TypeName,
    pub type_id     : Option<TypeId>,
}

impl_typeable!(Cast);
impl_positioned!(Cast);
impl_display!(Cast, "{} as {:?}", expr, ty);

impl Resolvable for Cast {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

impl Returns for Cast {
    fn returns(self: &Self) -> bool {
        self.expr.returns()
    }
}

#[derive(Debug)]
pub struct BinaryOp {
    pub position    : Position,
    pub op          : BinaryOperator,
    pub left        : Expression,
    pub right       : Expression,
    pub type_id     : Option<TypeId>,
}

impl_typeable!(BinaryOp);
impl_positioned!(BinaryOp);
impl_display!(BinaryOp, "{}{}{}", left, op, right);

impl Resolvable for BinaryOp {
    fn num_resolved(self: &Self) -> Progress {
        self.left.num_resolved()
        + self.right.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

impl Returns for BinaryOp {
    fn returns(self: &Self) -> bool {
        self.left.returns() || self.right.returns()
    }
}

#[derive(Debug)]
pub struct UnaryOp {
    pub position    : Position,
    pub op          : UnaryOperator,
    pub expr        : Expression,
    pub type_id     : Option<TypeId>,
}

impl_typeable!(UnaryOp);
impl_positioned!(UnaryOp);

impl Resolvable for UnaryOp {
    fn num_resolved(self: &Self) -> Progress {
        self.expr.num_resolved()
        + self.type_id.map_or(Progress::new(0, 1), |_| Progress::new(1, 1))
    }
}

impl Display for UnaryOp {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            UnaryOperator::DecAfter | UnaryOperator::DecBefore => write!(f, "{}{}", self.expr, self.op),
            _ => write!(f, "{}{}", self.op, self.expr),
        }
    }
}

impl Returns for UnaryOp {
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
