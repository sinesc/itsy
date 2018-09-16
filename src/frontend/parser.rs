//! Nom parsers used to generate the Itsy AST.

use nom::Err as Error; // Err seems problematic given Result::Err(nom:Err::...)
use nom::{IResult, ErrorKind, is_alphabetic, is_alphanumeric, digit0, digit1};
use nom::types::CompleteStr as Input;
use std::collections::HashMap;
use frontend::util::{Integer, TypeSlot};
use frontend::ast::*;
use frontend::Program;

/// Represents the various possible parser errors.
#[repr(u32)]
pub enum ParseError {
    SyntaxLet = 1,
    InvalidNumerical = 2,
    // TODO: figure out nom error handling
}

// identifier [a-z_][a-z0-9_]*

named!(ident<Input, Input>, recognize!(tuple!(
    take_while1!(|m| is_alphabetic(m as u8) || m == '_'),
    take_while!(|m| is_alphanumeric(m as u8) || m == '_')
)));

// identifier path

named!(ident_path<Input, IdentPath>, map!(ws!(separated_nonempty_list!(char!('.'), ident)), |m| IdentPath(
    m.into_iter().map(|s| *s).collect()
)));

// block

named!(block_items<Input, Vec<Statement>>, ws!(many0!(statement)));

named!(block<Input, Block>, map!(ws!(tuple!(char!('{'), block_items, opt!(expression), char!('}'))), |mut m| {
    // move last block item into result if it could be an expression and no result was matched
    if m.2.is_none() && m.1.last().map_or(false, |l| l.is_expressable()) {
        m.2 = m.1.pop().map(|s| s.into_expression());
    }
    Block {
        statements  : m.1,
        result      : m.2,
        type_id     : TypeSlot::Unresolved,
    }
}));

// function call (expression)

named!(call_argument_list<Input, Vec<Expression>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), expression), char!(')'))));

named!(call<Input, Expression>, map!(ws!(tuple!(ident_path, call_argument_list)), |m| Expression::Call(Call {
    path        : m.0,
    args        : m.1,
    type_id     : TypeSlot::Unresolved,
    function_id : None,
    rust_fn_index: None,
})));

// literal numerical (expression)

named!(opt_sign<Input, Option<Input>>, opt!(recognize!(one_of!("+-"))));
named!(opt_fract<Input, Option<Input>>, opt!(recognize!(tuple!(tag!("."), not!(char!('.')), digit0)))); // not(.) to avoid matching ranges

fn parse_numerical(n: Input) -> IResult<Input, Expression> {

    use nom::simple_errors::Context;

    if n.contains(".") {
        if let Ok(float) = str::parse::<f64>(*n) {
            return Ok((n, Expression::Literal(Literal { value : LiteralValue::Float(float), type_id: TypeSlot::Unresolved })))
        }
    } else if n.starts_with("-") {
        if let Ok(integer) = str::parse::<i64>(*n) {
            return Ok((n, Expression::Literal(Literal { value : LiteralValue::Integer(Integer::Signed(integer)), type_id: TypeSlot::Unresolved })))
        }
    } else {
        if let Ok(integer) = str::parse::<u64>(*n) {
            return Ok((n, Expression::Literal(Literal { value : LiteralValue::Integer(Integer::Unsigned(integer)), type_id: TypeSlot::Unresolved })))
        }
    }

    Err(Error::Failure(Context::Code(n, ErrorKind::Custom(ParseError::InvalidNumerical as u32))))
}

named!(numerical<Input, Expression>, flat_map!(recognize!(tuple!(opt_sign, digit1, opt_fract)), parse_numerical));

// assignment (expression)

named!(assignment_operator<Input, BinaryOperator>, map!(alt!(tag!("=") | tag!("+=") | tag!("-=") | tag!("*=") | tag!("/=")| tag!("%=")), |o| {
    BinaryOperator::from_string(*o)
}));

named!(assignment<Input, Expression>, map!(ws!(tuple!(ident_path, assignment_operator, expression)), |m| {
    Expression::Assignment(Box::new(Assignment {
        op      : m.1,
        left    : Variable { path: m.0, type_id: TypeSlot::Unresolved, binding_id: None },
        right   : m.2,
    }))
}));

// expression

named!(parens<Input, Expression>, ws!(delimited!(char!('('), expression, char!(')'))));

named!(prefix<Input, Expression>, map!(ws!(pair!(alt!(tag!("!") | tag!("++") | tag!("--")), ident_path)),|m| {
    Expression::UnaryOp(Box::new(UnaryOp {
        op      : UnaryOperator::prefix_from_string(*m.0),
        exp     : Expression::Variable(Variable { path: m.1, type_id: TypeSlot::Unresolved, binding_id: None }),
        type_id : TypeSlot::Unresolved
    }))
}));

named!(operand<Input, Expression>, ws!(alt!(
    map!(if_block, |m| Expression::IfBlock(Box::new(m)))
    | map!(block, |m| Expression::Block(Box::new(m)))
    | parens
    | prefix
    | numerical
    | call
    | map!(ident_path, |m| Expression::Variable(Variable { path: m, type_id: TypeSlot::Unresolved, binding_id: None }))
)));

named!(prec5<Input, Expression>, ws!(do_parse!(
    init: operand >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("*") | tag!("/") | tag!("%")), |o| BinaryOperator::from_string(*o)), operand),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: TypeSlot::Unresolved }))
    ) >>
    (res)
)));

named!(prec4<Input, Expression>, ws!(do_parse!(
    init: prec5 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("+") | tag!("-")), |o| BinaryOperator::from_string(*o)), prec5),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: TypeSlot::Unresolved }))
    ) >>
    (res)
)));

named!(prec3<Input, Expression>, ws!(do_parse!(
    init: prec4 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("<=") | tag!(">=") | tag!("<") | tag!(">")), |o| BinaryOperator::from_string(*o)), prec4),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: TypeSlot::Unresolved }))
    ) >>
    (res)
)));

named!(prec2<Input, Expression>, ws!(do_parse!(
    init: prec3 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("!=") | tag!("==")), |o| BinaryOperator::from_string(*o)), prec3),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: TypeSlot::Unresolved }))
    ) >>
    (res)
)));

named!(prec1<Input, Expression>, ws!(do_parse!(
    init: prec2 >>
    res: fold_many0!(
        pair!(map!(tag!("&&"), |o| BinaryOperator::from_string(*o)), prec2),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: TypeSlot::Unresolved }))
    ) >>
    (res)
)));

named!(prec0<Input, Expression>, ws!(do_parse!(
    init: prec1 >>
    res: fold_many0!(
        pair!(map!(tag!("||"), |o| BinaryOperator::from_string(*o)), prec1),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: TypeSlot::Unresolved }))
    ) >>
    (res)
)));

named!(expression<Input, Expression>, ws!(alt!(
    assignment
    | prec0
)));

// let

named!(binding<Input, Statement>, map!(
    preceded!(ws!(tag!("let")), return_error!(
        ErrorKind::Custom(ParseError::SyntaxLet as u32),
        ws!(tuple!(opt!(tag!("mut")), ident, opt!(preceded!(char!(':'), ident_path)), opt!(preceded!(char!('='), expression)), char!(';')))
    )),
    |m| Statement::Binding(Binding {
        name        : *m.1,
        mutable     : m.0.is_some(),
        expr        : m.3,
        ty          : m.2.map(|t| Type::unknown(t)),
        type_id     : TypeSlot::Unresolved,
        binding_id  : None,
    })
));

// structure

named!(structure_item<Input, (Input, Type)>, map!(
    ws!(tuple!(ident, char!(':'), ident_path)),
    |tuple| (tuple.0, Type::unknown(tuple.2))
));

named!(structure_items<Input, HashMap<&str, Type>>, map!(ws!(separated_list_complete!(char!(','), structure_item)), |list| {
    list.into_iter().map(|item| (*item.0, item.1)).collect()
}));

named!(structure<Input, Statement>, map!(ws!(tuple!(tag!("struct"), ident, char!('{'), structure_items, char!('}'))), |tuple| Statement::Structure(Structure {
    name    : *tuple.1,
    items   : tuple.3,
    type_id : TypeSlot::Unresolved,
})));

// function

named!(signature_argument<Input, Binding>, map!(ws!(tuple!(opt!(tag!("mut")), ident, char!(':'), ident_path)), |tuple| Binding {
    name        : *tuple.1,
    expr        : None,
    mutable     : tuple.0.is_some(),
    ty          : Some(Type::unknown(tuple.3)),
    type_id     : TypeSlot::Unresolved,
    binding_id  : None,
}));

named!(signature_argument_list<Input, Vec<Binding>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), signature_argument), char!(')'))));

named!(signature_return_part<Input, IdentPath>, ws!(preceded!(tag!("->"), ident_path)));

named!(signature<Input, Signature>, map!(ws!(tuple!(tag!("fn"), ident, signature_argument_list, opt!(signature_return_part))), |sig| Signature {
    name    : *sig.1,
    args    : sig.2,
    ret     : if let Some(sig_ty) = sig.3 { Some(Type::unknown(sig_ty)) } else { None }, // TODO: might want to have void for simplicity?
}));

named!(function<Input, Statement>, map!(ws!(tuple!(signature, block)), |func| Statement::Function(Function {
    sig         : func.0,
    block       : func.1,
    function_id : None,
})));

// if

named!(block_or_if<Input, Block>, ws!(alt!(
    map!(if_block, |m| Block { statements: Vec::new(), result: Some(Expression::IfBlock(Box::new(m))), type_id: TypeSlot::Unresolved })
    | block
)));

named!(if_block<Input, IfBlock>, map!(ws!(tuple!(tag!("if"), expression, block, opt!(preceded!(tag!("else"), block_or_if)))), |m| IfBlock {
    cond        : m.1,
    if_block    : m.2,
    else_block  : m.3,
}));

// for

// TODO: simply accept "for ident in expression" and make .. an operator?

named!(for_loop_range<Input, Expression>, map!(ws!(tuple!(expression, tag!(".."), expression)), |m| {
    Expression::BinaryOp(Box::new(BinaryOp { op: BinaryOperator::Range, left: m.0, right: m.2, type_id: TypeSlot::Unresolved }))
}));

named!(for_loop<Input, ForLoop>, map!(ws!(tuple!(tag!("for"), ident, tag!("in"), alt!(for_loop_range | expression), block)), |m| ForLoop {
    iter: Binding {
        name        : *m.1,
        mutable     : true,
        expr        : None,
        ty          : None,
        type_id     : TypeSlot::Unresolved,
        binding_id  : None,
    },
    range: m.3,
    block: m.4,
}));

// while loop

named!(while_loop<Input, WhileLoop>, map!(ws!(preceded!(tag!("while"), tuple!(expression, block))), |m| WhileLoop {
    expr: m.0,
    block: m.1,
}));

// return

named!(return_statement<Input, Statement>, map!(ws!(preceded!(tag!("return"), terminated!(opt!(expression), char!(';')))),
    |m| Statement::Return(Return {
        expr        : m,
    })
));

// statement

named!(statement<Input, Statement>, alt!(
    binding
    | map!(if_block, |m| Statement::IfBlock(m))
    | function
    | structure
    | map!(for_loop, |m| Statement::ForLoop(m))
    | map!(while_loop, |m| Statement::WhileLoop(m))
    | map!(terminated!(expression, char!(';')), |m| Statement::Expression(m))
    | map!(block, |m| Statement::Block(m))
    | return_statement
));

// root

named!(program<Input, Program>, ws!(many0!(statement)));

/// Parses an Itsy source file into a program AST structure.
pub fn parse(input: &str) -> Result<Program, u32> {
    // todo: error handling!
    let result = program(Input(input));
    match result {
        Ok(result) => {
            if result.0.len() > 0 {
                Err(4) // todo: not sure what this case it. just getting the entire input back, no error
            } else {
                Ok(result.1)
            }
        },
        Err(error) => {
            Err(match error {
                Error::Incomplete(_) => 1,
                Error::Error(_) => 2,
                Error::Failure(_) => 3,
            })
        }
    }
}