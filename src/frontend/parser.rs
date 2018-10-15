//! Nom parsers used to generate the Itsy AST.

use nom::Err as Error; // Err seems problematic given Result::Err(nom:Err::...)
use nom::types::CompleteStr as Input;
use nom::verbose_errors::Context;
use nom::*;
use std::collections::HashMap;
use crate::frontend::util::Numeric;
use crate::frontend::ast::*;
use crate::frontend::Program;

/// Represents the various possible parser errors.
#[repr(u32)]
pub enum ParseError {
    SyntaxError = 1,
    SyntaxLet = 2,
    SyntaxFn = 3,
    InvalidNumerical = 4,
    // TODO: figure out nom error handling
}

// identifier [a-z_][a-z0-9_]*

named!(ident(Input<'_>) -> Input<'_>, recognize!(tuple!(
    take_while1!(|m| is_alphabetic(m as u8) || m == '_'),
    take_while!(|m| is_alphanumeric(m as u8) || m == '_')
)));

// identifier path

named!(ident_path(Input<'_>) -> IdentPath<'_>, map!(ws!(separated_nonempty_list!(char!('.'), ident)), |m| IdentPath(
    m.into_iter().map(|s| *s).collect()
)));

// block

named!(block_items(Input<'_>) -> Vec<Statement<'_>>, ws!(many0!(statement)));

named!(block(Input<'_>) -> Block<'_>, map!(ws!(tuple!(char!('{'), block_items, opt!(expression), char!('}'))), |mut m| {
    // move last block item into result if it could be an expression and no result was matched
    if m.2.is_none() && m.1.last().map_or(false, |l| l.is_expressable()) {
        m.2 = m.1.pop().map(|s| s.into_expression());
    }
    Block {
        statements  : m.1,
        result      : m.2,
        type_id     : None,
        scope_id    : None,
    }
}));

// function call (expression)

named!(call_argument_list<Input<'_>, Vec<Expression<'_>>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), expression), char!(')'))));

named!(call<Input<'_>, Expression<'_>>, map!(ws!(tuple!(ident_path, call_argument_list)), |m| Expression::Call(Call {
    path        : m.0,
    args        : m.1,
    type_id     : None,
    function_id : None,
    rust_fn_index: None,
})));

// literal numerical (expression)

named!(opt_sign<Input<'_>, Option<Input<'_>>>, opt!(recognize!(one_of!("+-"))));
named!(opt_fract<Input<'_>, Option<Input<'_>>>, opt!(recognize!(tuple!(tag!("."), not!(char!('.')), digit0)))); // not(.) to avoid matching ranges

fn parse_numerical(n: Input<'_>) -> IResult<Input<'_>, Expression<'_>> {
    if n.contains(".") {
        if let Ok(float) = str::parse::<f64>(*n) {
            return Ok((n, Expression::Literal(Literal {
                value   : LiteralValue::Numeric(Numeric::Float(float)),
                type_id : None,
                ty      : None, // todo support e.g. "23i32"
            })))
        }
    } else if n.starts_with("-") {
        if let Ok(integer) = str::parse::<i64>(*n) {
            return Ok((n, Expression::Literal(Literal {
                value   : LiteralValue::Numeric(Numeric::Signed(integer)),
                type_id : None,
                ty      : None, // todo: support e.g. "-232i32"
            })))
        }
    } else {
        if let Ok(integer) = str::parse::<u64>(*n) {
            return Ok((n, Expression::Literal(Literal {
                value   : LiteralValue::Numeric(Numeric::Unsigned(integer)),
                type_id : None,
                ty      : None, // todo: see above
            })))
        }
    }

    Err(Error::Failure(Context::Code(n, ErrorKind::Custom(ParseError::InvalidNumerical as u32))))
}

named!(numerical<Input<'_>, Expression<'_>>, flat_map!(recognize!(tuple!(opt_sign, digit1, opt_fract)), parse_numerical));

// literal boolean (expression)

named!(boolean<Input<'_>, Expression<'_>>, map!(alt!(tag!("true") | tag!("false")), |m| {
    Expression::Literal(Literal {
        value   : LiteralValue::Bool(*m == "true"),
        type_id : None,
        ty      : None,
    })
}));

// assignment (expression)

named!(assignment_operator<Input<'_>, BinaryOperator>, map!(alt!(tag!("=") | tag!("+=") | tag!("-=") | tag!("*=") | tag!("/=")| tag!("%=")), |o| {
    BinaryOperator::from_string(*o)
}));

named!(assignment<Input<'_>, Expression<'_>>, map!(ws!(tuple!(ident_path, assignment_operator, expression)), |m| {
    Expression::Assignment(Box::new(Assignment {
        op      : m.1,
        left    : Variable { path: m.0, type_id: None, binding_id: None },
        right   : m.2,
    }))
}));

// expression

named!(parens<Input<'_>, Expression<'_>>, ws!(delimited!(char!('('), expression, char!(')'))));

named!(prefix<Input<'_>, Expression<'_>>, map!(ws!(pair!(alt!(tag!("!") | tag!("++") | tag!("--")), ident_path)), |m| {
    Expression::UnaryOp(Box::new(UnaryOp {
        op      : UnaryOperator::prefix_from_string(*m.0),
        expr    : Expression::Variable(Variable { path: m.1, type_id: None, binding_id: None }),
        type_id : None
    }))
}));

named!(suffix<Input<'_>, Expression<'_>>, map!(ws!(pair!(ident_path, alt!(tag!("++") | tag!("--")))), |m| {
    Expression::UnaryOp(Box::new(UnaryOp {
        op      : UnaryOperator::suffix_from_string(*m.1),
        expr    : Expression::Variable(Variable { path: m.0, type_id: None, binding_id: None }),
        type_id : None
    }))
}));

named!(operand<Input<'_>, Expression<'_>>, ws!(alt!(
    boolean
    | map!(if_block, |m| Expression::IfBlock(Box::new(m)))
    | map!(block, |m| Expression::Block(Box::new(m)))
    | parens
    | suffix
    | prefix
    | numerical
    | call
    | map!(ident_path, |m| Expression::Variable(Variable { path: m, type_id: None, binding_id: None }))
)));

named!(prec5<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: operand >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("*") | tag!("/") | tag!("%")), |o| BinaryOperator::from_string(*o)), operand),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: None }))
    ) >>
    (res)
)));

named!(prec4<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec5 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("+") | tag!("-")), |o| BinaryOperator::from_string(*o)), prec5),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: None }))
    ) >>
    (res)
)));

named!(prec3<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec4 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("<=") | tag!(">=") | tag!("<") | tag!(">")), |o| BinaryOperator::from_string(*o)), prec4),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: None }))
    ) >>
    (res)
)));

named!(prec2<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec3 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("!=") | tag!("==")), |o| BinaryOperator::from_string(*o)), prec3),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: None }))
    ) >>
    (res)
)));

named!(prec1<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec2 >>
    res: fold_many0!(
        pair!(map!(tag!("&&"), |o| BinaryOperator::from_string(*o)), prec2),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: None }))
    ) >>
    (res)
)));

named!(prec0<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec1 >>
    res: fold_many0!(
        pair!(map!(tag!("||"), |o| BinaryOperator::from_string(*o)), prec1),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, type_id: None }))
    ) >>
    (res)
)));

named!(expression<Input<'_>, Expression<'_>>, ws!(alt!(
    assignment
    | prec0
)));

// let

named!(binding<Input<'_>, Statement<'_>>, map!(
    preceded!(ws!(tag!("let")), return_error!(
        ErrorKind::Custom(ParseError::SyntaxLet as u32),
        ws!(tuple!(opt!(tag!("mut")), ident, opt!(preceded!(char!(':'), ident_path)), opt!(preceded!(char!('='), expression)), char!(';')))
    )),
    |m| Statement::Binding(Binding {
        name        : *m.1,
        mutable     : m.0.is_some(),
        expr        : m.3,
        ty          : m.2.map(|t| Type::unknown(t)),
        type_id     : None,
        binding_id  : None,
    })
));

// structure

named!(structure_item<Input<'_>, (Input<'_>, Type<'_>)>, map!(
    ws!(tuple!(ident, char!(':'), ident_path)),
    |tuple| (tuple.0, Type::unknown(tuple.2))
));

named!(structure_items<Input<'_>, HashMap<&str, Type<'_>>>, map!(ws!(separated_list_complete!(char!(','), structure_item)), |list| {
    list.into_iter().map(|item| (*item.0, item.1)).collect()
}));

named!(structure<Input<'_>, Statement<'_>>, map!(ws!(tuple!(tag!("struct"), ident, char!('{'), structure_items, char!('}'))), |tuple| Statement::Structure(Structure {
    name    : *tuple.1,
    items   : tuple.3,
    type_id : None,
})));

// function

named!(signature_argument<Input<'_>, Binding<'_>>, map!(ws!(tuple!(opt!(tag!("mut")), ident, char!(':'), ident_path)), |tuple| Binding {
    name        : *tuple.1,
    expr        : None,
    mutable     : tuple.0.is_some(),
    ty          : Some(Type::unknown(tuple.3)),
    type_id     : None,
    binding_id  : None,
}));

named!(signature_argument_list<Input<'_>, Vec<Binding<'_>>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), signature_argument), char!(')'))));

named!(signature_return_part<Input<'_>, IdentPath<'_>>, ws!(preceded!(tag!("->"), ident_path)));

named!(signature<Input<'_>, Signature<'_>>, map!(
    preceded!(ws!(tag!("fn")), return_error!(
        ErrorKind::Custom(ParseError::SyntaxFn as u32),
        ws!(tuple!(ident, signature_argument_list, opt!(signature_return_part)))
    )), |sig| Signature {
    name    : *sig.0,
    args    : sig.1,
    ret     : if let Some(sig_ty) = sig.2 { Some(Type::unknown(sig_ty)) } else { None },
}));

named!(function<Input<'_>, Statement<'_>>, map!(ws!(tuple!(signature, block)), |func| Statement::Function(Function {
    sig         : func.0,
    block       : func.1,
    function_id : None,
    scope_id    : None,
})));

// if

named!(block_or_if<Input<'_>, Block<'_>>, ws!(alt!(
    map!(if_block, |m| Block {
        statements  : Vec::new(),
        result      : Some(Expression::IfBlock(Box::new(m))),
        type_id     : None,
        scope_id    : None,
    })
    | block
)));

named!(if_block<Input<'_>, IfBlock<'_>>, map!(ws!(tuple!(tag!("if"), expression, block, opt!(preceded!(tag!("else"), block_or_if)))), |m| IfBlock {
    cond        : m.1,
    if_block    : m.2,
    else_block  : m.3,
    scope_id    : None,
}));

// for

// TODO: simply accept "for ident in expression" and make .. an operator?

named!(for_loop_range<Input<'_>, Expression<'_>>, map!(ws!(tuple!(expression, tag!(".."), expression)), |m| {
    Expression::BinaryOp(Box::new(BinaryOp { op: BinaryOperator::Range, left: m.0, right: m.2, type_id: None }))
}));

named!(for_loop<Input<'_>, ForLoop<'_>>, map!(ws!(tuple!(tag!("for"), ident, tag!("in"), alt!(for_loop_range | expression), block)), |m| ForLoop {
    iter: Binding {
        name        : *m.1,
        mutable     : true,
        expr        : None,
        ty          : None,
        type_id     : None,
        binding_id  : None,
    },
    range   : m.3,
    block   : m.4,
    scope_id: None,
}));

// while loop

named!(while_loop<Input<'_>, WhileLoop<'_>>, map!(ws!(preceded!(tag!("while"), tuple!(expression, block))), |m| WhileLoop {
    expr    : m.0,
    block   : m.1,
    scope_id: None,
}));

// return

named!(return_statement<Input<'_>, Statement<'_>>, map!(ws!(preceded!(tag!("return"), terminated!(opt!(expression), char!(';')))),
    |m| Statement::Return(Return {
        expr        : m,
    })
));

// statement

named!(statement<Input<'_>, Statement<'_>>, alt!(
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

named!(program(Input<'_>) -> Program<'_>, return_error!(
    ErrorKind::Custom(ParseError::SyntaxError as u32),
    ws!(many0!(statement))
));

//fn

/// Parses an Itsy source file into a program AST structure.
pub fn parse(input: &str) -> Result<Program<'_>, u32> {
    // todo: error handling!
    let result = program(Input(input));
    match result {
        Ok(result) => {
            if result.0.len() > 0 {
                println!("result: {:#?}", result);
                Err(4) // todo: not sure what this case it. just getting the entire input back, no error
            } else {
                Ok(result.1)
            }
        },
        Err(error) => {
            println!("error: {:#?}", error);
            Err(match error {
                Error::Incomplete(_) => 1,
                Error::Error(_) => 2,
                Error::Failure(_) => 3,
            })
        }
    }
}