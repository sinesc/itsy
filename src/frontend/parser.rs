//! Nom parsers used to generate the Itsy AST.

use nom::Err as Error; // Err seems problematic given Result::Err(nom:Err::...)
use nom::types::CompleteStr as Input;
use nom::verbose_errors::Context;
use nom::*;
use std::collections::HashMap;
use crate::util::Numeric;
use crate::frontend::ast::*;

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

named!(ident(Input<'_>) -> &str, map!(recognize!(tuple!(take_while1!(|m| is_alphabetic(m as u8) || m == '_'), take_while!(|m| is_alphanumeric(m as u8) || m == '_'))), |s| *s));

// path

named!(path(Input<'_>) -> Vec<&'_ str>, map!(ws!(separated_nonempty_list!(tag!("::"), ident)), |m| m.into_iter().collect() ));

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
        scope_id    : None,
    }
}));

// function call

named!(call_argument_list<Input<'_>, Vec<Expression<'_>>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), expression), char!(')'))));

named!(call<Input<'_>, Call<'_>>, map!(ws!(tuple!(ident, call_argument_list)), |m| Call {
    name        : m.0,
    args        : m.1,
    function_id : None,
    rust_fn_index: None,
    binding_id  : None,
}));

// literal numerical

named!(opt_sign<Input<'_>, Option<Input<'_>>>, opt!(recognize!(one_of!("+-"))));
named!(opt_fract<Input<'_>, Option<Input<'_>>>, opt!(recognize!(tuple!(tag!("."), not!(char!('.')), digit0)))); // not(.) to avoid matching ranges
named!(opt_type<Input<'_>, Option<Input<'_>>>, opt!(recognize!(tuple!(one_of!("iuf"), alt!(tag!("8") | tag!("16") | tag!("32") | tag!("64"))))));

fn parse_numerical_suffix(n: Input<'_>) -> (&str, Option<&str>) {
    // todo: there must be something in std to simplify this
    let tail = if n.len() > 3 {
        let tail = &n[n.len() - 3 ..];
        if &tail[0..1] != "u" && &tail[0..1] != "i" && &tail[0..1] != "f" {
            &tail[1..]
        } else {
            tail
        }
    } else if n.len() > 2 {
        &n[n.len() - 2 ..]
    } else {
        ""
    };

    if tail.len() == 2 && (tail == "u8" || tail == "i8") {
        (&n[0..n.len() - 2], Some(tail))
    } else if tail.len() == 3 && (&tail[0..1] == "u" || &tail[0..1] == "i" || &tail[0..1] == "f") && (&tail[1..] == "16" || &tail[1..] == "32" || &tail[1..] == "64") && tail != "f16" {
        (&n[0..n.len() - 3], Some(tail))
    } else {
        (*n, None)
    }
}

fn parse_numerical(n: Input<'_>) -> IResult<Input<'_>, Literal<'_>> {
    let (value, type_name) = parse_numerical_suffix(n);
    if value.contains(".") || type_name == Some("f32") || type_name == Some("f64") {
        if let Ok(float) = str::parse::<f64>(value) {
            //println!("f {} {:?}", float, type_name);
            return Ok((n, Literal {
                value       : LiteralValue::Numeric(Numeric::Float(float)),
                type_name   : type_name.map(|ty| TypeName { path: vec![ ty ], type_id: None }),
                binding_id  : None,
            }))
        }
    } else if value.starts_with("-") || type_name == Some("i8") || type_name == Some("i16") || type_name == Some("i32") || type_name == Some("i64") {
        if let Ok(integer) = str::parse::<i64>(value) {
            //println!("i {} {:?}", integer, type_name);
            return Ok((n, Literal {
                value       : LiteralValue::Numeric(Numeric::Signed(integer)),
                type_name   : type_name.map(|ty| TypeName { path: vec![ ty ], type_id: None }),
                binding_id  : None,
            }))
        }
    } else {
        if let Ok(integer) = str::parse::<u64>(value) {
            //println!("u {} {:?}", integer, type_name);
            return Ok((n, Literal {
                value       : LiteralValue::Numeric(Numeric::Unsigned(integer)),
                type_name   : type_name.map(|ty| TypeName { path: vec![ ty ], type_id: None }),
                binding_id  : None,
            }))
        }
    }

    Err(Error::Failure(Context::Code(n, ErrorKind::Custom(ParseError::InvalidNumerical as u32))))
}

named!(numerical<Input<'_>, Literal<'_>>, flat_map!(recognize!(tuple!(opt_sign, digit1, opt_fract, opt_type)), parse_numerical));

// literal boolean

named!(boolean<Input<'_>, Literal<'_>>, map!(alt!(tag!("true") | tag!("false")), |m| {
    Literal {
        value       : LiteralValue::Bool(*m == "true"),
        type_name   : None,
        binding_id  : None,
    }
}));

// literal string

named!(string<Input<'_>, Literal<'_>>, map!(delimited!(char!('"'), escaped!(none_of!("\\\""), '\\', one_of!("\"n\\")), char!('"')), |m| {
    Literal {
        value       : LiteralValue::String(*m),
        type_name   : None,
        binding_id  : None,
    }
}));

// literal array

named!(array_literal_elements<Input<'_>, Vec<Literal<'_>>>, ws!(separated_list_complete!(char!(','), literal)));

named!(array_literal<Input<'_>, Literal<'_>>, map!(ws!(delimited!(char!('['), array_literal_elements, char!(']'))), |m| Literal {
    value: LiteralValue::Array(ArrayLiteral {
        elements: m,
    }),
    type_name: None,
    binding_id: None,
}));

// struct literal

named!(struct_literal_field<Input<'_>, (&str, Literal<'_>)>, map!(ws!(tuple!(ident, char!(':'), literal)),
    |tuple| (tuple.0, tuple.2)
));

named!(struct_literal_fields<Input<'_>, HashMap<&str, Literal<'_>>>, map!(ws!(separated_list_complete!(char!(','), struct_literal_field)), |list| {
    list.into_iter().map(|item| (item.0, item.1)).collect()
}));

named!(struct_literal<Input<'_>, Literal<'_>>, map!(ws!(tuple!(path, char!('{'), struct_literal_fields, opt!(char!(',')), char!('}'))), |m| Literal {
    value: LiteralValue::Struct(StructLiteral {
        fields: m.2,
    }),
    type_name: Some(TypeName::unknown(m.0)),
    binding_id: None,
}));

// general literal

named!(literal<Input<'_>, Literal<'_>>, ws!(alt!(boolean | string | array_literal | struct_literal | numerical)));

// assignment

named!(assignment_operator<Input<'_>, BinaryOperator>, map!(alt!(tag!("=") | tag!("+=") | tag!("-=") | tag!("*=") | tag!("/=")| tag!("%=")), |o| {
    BinaryOperator::from_string(*o)
}));

named!(assignment<Input<'_>, Assignment<'_>>, map!(ws!(tuple!(ident, assignment_operator, expression)), |m| {
    Assignment {
        op      : m.1,
        left    : Variable { name: m.0, binding_id: None },
        right   : m.2,
    }
}));

// expression

named!(parens<Input<'_>, Expression<'_>>, ws!(delimited!(char!('('), expression, char!(')'))));

named!(unary<Input<'_>, Expression<'_>>, map!(ws!(preceded!(tag!("!"), expression)), |m| {
    Expression::UnaryOp(Box::new(UnaryOp {
        op          : UnaryOperator::Not,
        expr        : m,
        binding_id  : None,
    }))
}));

named!(prefix<Input<'_>, Expression<'_>>, map!(ws!(pair!(alt!(tag!("!") | tag!("++") | tag!("--")), ident)), |m| { // todo: need to allow expression, check assignability in resolver
    Expression::UnaryOp(Box::new(UnaryOp {
        op          : UnaryOperator::prefix_from_string(*m.0),
        expr        : Expression::Variable(Variable { name: m.1, binding_id: None }),
        binding_id  : None,
    }))
}));

named!(suffix<Input<'_>, Expression<'_>>, map!(ws!(pair!(ident, alt!(tag!("++") | tag!("--")))), |m| {// todo: need to allow expression (e.g. a[b]), check assignability in resolver
    Expression::UnaryOp(Box::new(UnaryOp {
        op          : UnaryOperator::suffix_from_string(*m.1),
        expr        : Expression::Variable(Variable { name: m.0, binding_id: None }),
        binding_id  : None,
    }))
}));

named!(operand<Input<'_>, Expression<'_>>, ws!(alt!( // todo: this may require complete around alternatives: see "BE CAREFUL" in https://docs.rs/nom/4.1.1/nom/macro.alt.html
    map!(boolean, |m| Expression::Literal(m))
    | map!(string, |m| Expression::Literal(m))
    | map!(if_block, |m| Expression::IfBlock(Box::new(m)))
    | map!(block, |m| Expression::Block(Box::new(m)))
    | parens
    | unary // todo: unary, suffix and prefix could be changed to UnaryOp and then mapped here like bool, string, ...
    | suffix
    | prefix
    | map!(numerical, |m| Expression::Literal(m))
    | map!(call, |m| Expression::Call(m))
    | map!(ident, |m| Expression::Variable(Variable { name: m, binding_id: None }))
)));

named!(prec6<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: operand >>
    res: fold_many0!(
        alt!(
            map!(delimited!(tag!("["), expression, tag!("]")), |e| (BinaryOperator::Index, e))
            | map!(preceded!(tag!("."), ident), |n| (BinaryOperator::Access, Expression::Member(Member { name: n, binding_id: None, index: None })))
        ),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec5<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec6 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("*") | tag!("/") | tag!("%")), |o| BinaryOperator::from_string(*o)), prec6),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec4<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec5 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("+") | tag!("-")), |o| BinaryOperator::from_string(*o)), prec5),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec3<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec4 >>
    res: fold_many0!( // todo: does this work? see comment on "operand"
        pair!(map!(alt!(tag!("<=") | tag!(">=") | tag!("<") | tag!(">")), |o| BinaryOperator::from_string(*o)), prec4),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec2<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec3 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("!=") | tag!("==")), |o| BinaryOperator::from_string(*o)), prec3),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec1<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec2 >>
    res: fold_many0!(
        pair!(map!(tag!("&&"), |o| BinaryOperator::from_string(*o)), prec2),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec0<Input<'_>, Expression<'_>>, ws!(do_parse!(
    init: prec1 >>
    res: fold_many0!(
        pair!(map!(tag!("||"), |o| BinaryOperator::from_string(*o)), prec1),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(expression<Input<'_>, Expression<'_>>, ws!(alt!(
    map!(assignment, |m| Expression::Assignment(Box::new(m)))
    | map!(array_literal, |m| Expression::Literal(m))
    | map!(struct_literal, |m| Expression::Literal(m))
    | prec0
)));

// let

named!(binding<Input<'_>, Statement<'_>>, map!(
    preceded!(ws!(tag!("let")), return_error!(
        ErrorKind::Custom(ParseError::SyntaxLet as u32),
        ws!(tuple!(opt!(tag!("mut")), ident, opt!(preceded!(char!(':'), path)), opt!(preceded!(char!('='), expression)), char!(';')))
    )),
    |m| Statement::Binding(Binding {
        name        : m.1,
        mutable     : m.0.is_some(),
        expr        : m.3,
        type_name   : m.2.map(|t| TypeName::unknown(t)),
        binding_id  : None,
    })
));

// struct definition

named!(struct_item<Input<'_>, (&str, TypeName<'_>)>, map!(ws!(tuple!(ident, char!(':'), path)),
    |tuple| (tuple.0, TypeName::unknown(tuple.2))
));

named!(struct_fields<Input<'_>, Vec<(&str, TypeName<'_>)>>, map!(ws!(separated_list_complete!(char!(','), struct_item)), |list| {
    list.into_iter().map(|item| (item.0, item.1)).collect()
}));

named!(struct_<Input<'_>, Statement<'_>>, map!(ws!(tuple!(tag!("struct"), ident, char!('{'), struct_fields, opt!(char!(',')), char!('}'))), |tuple| Statement::Structure(Struct {
    name    : tuple.1,
    fields  : tuple.3,
    type_id : None,
})));

// function

named!(signature_argument<Input<'_>, Binding<'_>>, map!(ws!(tuple!(opt!(tag!("mut")), ident, char!(':'), path)), |tuple| Binding {
    name        : tuple.1,
    expr        : None,
    mutable     : tuple.0.is_some(),
    type_name   : Some(TypeName::unknown(tuple.3)),
    binding_id  : None,
}));

named!(signature_argument_list<Input<'_>, Vec<Binding<'_>>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), signature_argument), char!(')'))));

named!(signature_return_part<Input<'_>, Vec<&'_ str>>, ws!(preceded!(tag!("->"), path)));

named!(signature<Input<'_>, Signature<'_>>, map!(
    preceded!(ws!(tag!("fn")), return_error!(
        ErrorKind::Custom(ParseError::SyntaxFn as u32),
        ws!(tuple!(ident, signature_argument_list, opt!(signature_return_part)))
    )), |sig| Signature {
    name    : sig.0,
    args    : sig.1,
    ret     : if let Some(sig_ty) = sig.2 { Some(TypeName::unknown(sig_ty)) } else { None },
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
    Expression::BinaryOp(Box::new(BinaryOp { op: BinaryOperator::Range, left: m.0, right: m.2, binding_id: None }))
}));

named!(for_loop<Input<'_>, ForLoop<'_>>, map!(ws!(tuple!(tag!("for"), ident, tag!("in"), alt!(for_loop_range | expression), block)), |m| ForLoop {
    iter: Binding {
        name        : m.1,
        mutable     : true,
        expr        : None,
        type_name   : None,
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
        expr            : m,
        fn_ret_type_id  : None,
    })
));

// statement

named!(statement<Input<'_>, Statement<'_>>, alt!(
    binding
    | map!(if_block, |m| Statement::IfBlock(m))
    | function
    | struct_
    | map!(for_loop, |m| Statement::ForLoop(m))
    | map!(while_loop, |m| Statement::WhileLoop(m))
    | map!(terminated!(expression, char!(';')), |m| Statement::Expression(m))
    | map!(block, |m| Statement::Block(m))
    | return_statement
));

// root

named!(root(Input<'_>) -> Vec<Statement<'_>>, return_error!(
    ErrorKind::Custom(ParseError::SyntaxError as u32),
    ws!(many0!(statement))
));

/// Parsed program AST.
#[derive(Debug)]
pub struct ParsedProgram<'a> (pub Vec<Statement<'a>>);

/// Parses an Itsy source file into a program AST structure.
pub fn parse(input: &str) -> Result<ParsedProgram<'_>, u32> {
    // todo: error handling!
    let result = root(Input(input));
    match result {
        Ok(result) => {
            if result.0.len() > 0 {
                println!("result: {:#?}", result);
                Err(4) // todo: not sure what this case it. just getting the entire input back, no error
            } else {
                Ok(ParsedProgram(result.1))
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