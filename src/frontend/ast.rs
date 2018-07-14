use std::collections::HashMap;
use util::BindingId;
use frontend::integer::Integer;
use frontend::Unresolved;

pub type Program<'a> = Vec<Statement<'a>>;

#[derive(Debug)]
pub enum Statement<'a> {
    Function(Function<'a>),
    Structure(Structure<'a>),
    Binding(Binding<'a>),
    ForLoop(ForLoop<'a>),
    IfBlock(IfBlock<'a>),
    Block(Block<'a>),
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
    pub type_id     : Unresolved,
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
    pub iter    : Binding<'a>,
    pub range   : Expression<'a>,
    pub block   : Block<'a>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub statements  : Vec<Statement<'a>>,
    pub result      : Option<Expression<'a>>,
    pub type_id     : Unresolved,
}

#[derive(Debug)]
pub struct Structure<'a> {
    pub name    : &'a str,
    pub items   : HashMap<&'a str, Type<'a>>,
    pub type_id : Unresolved,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub sig     : Signature<'a>,
    pub block   : Block<'a>,
}

#[derive(Debug)]
pub struct Signature<'a> {
    pub name    : &'a str,
    pub args    : Vec<Binding<'a>>,
    pub ret     : Option<Type<'a>>,
}

#[derive(Debug)]
pub struct Type<'a> {
    pub name    : IdentPath<'a>,
    pub type_id : Unresolved,
}

impl<'a> Type<'a> {
    /// Returns a type with the given name and an unresolved type-id.
    pub fn unknown(name: IdentPath<'a>) -> Self {
        Type {
            name    : name,
            type_id : Unresolved::Unknown,
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
    pub fn get_type_id(self: &Self) -> Unresolved {
        match self {
            Expression::Literal(literal)        => literal.type_id,
            Expression::Variable(variable)      => variable.type_id,
            Expression::Call(call)              => call.type_id,
            Expression::Assignment(assignment)  => assignment.left.type_id,
            Expression::BinaryOp(binary_op)     => binary_op.type_id,
            Expression::UnaryOp(unary_op)       => unary_op.type_id,
            Expression::Block(block)            => block.result.as_ref().map_or(Unresolved::Void, |r| r.get_type_id()),
            Expression::IfBlock(if_block)       => if_block.if_block.result.as_ref().map_or(Unresolved::Void, |r| r.get_type_id()),
        }
    }
}

#[derive(Debug)]
pub struct Literal<'a> {
    pub value   : LiteralValue<'a>,
    pub type_id : Unresolved,
}

#[derive(Debug)]
pub enum LiteralValue<'a> {
    String(&'a str),  // TODO: string literals
    Integer(Integer),
    Float(f64),
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

#[derive(Debug)]
pub struct Variable<'a> {
    pub path        : IdentPath<'a>,
    pub type_id     : Unresolved,
    pub binding_id  : Option<BindingId>,
}

#[derive(Debug)]
pub struct Call<'a> {
    pub path    : IdentPath<'a>,
    pub args    : Vec<Expression<'a>>,
    pub type_id : Unresolved,
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
    pub type_id : Unresolved,
}

#[derive(Debug)]
pub struct UnaryOp<'a> {
    pub op      : UnaryOperator,
    pub exp     : Expression<'a>,
    pub type_id : Unresolved,
}

#[derive(Debug)]
pub struct IdentPath<'a>(pub Vec<&'a str>);

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

#[derive(Debug, Copy, Clone)]
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