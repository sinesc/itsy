//! Nom parsers used to generate the Itsy AST.

use nom::Err as Error; // Err seems problematic given Result::Err(nom:Err::...)
use nom::types::CompleteStr as Input;
use nom::*;
use std::collections::HashMap;
use crate::util::{Numeric, compute_loc};
use crate::frontend::ast::*;

/// Represents the various possible parser error-kinds.
#[repr(u32)]
#[derive(Copy, Clone, Debug)]
pub enum ParseErrorKind {
    Unknown = 1,
    UnexpectedEOF = 2,
    SyntaxError = 3,
    SyntaxLet = 4,
    SyntaxFn = 5,
    SyntaxIf = 6,
    SyntaxElse = 7,
    InvalidNumerical = 8,
    // TODO: add error handling to all parsers where it make sense
}

/// A parser error including information about the kind of error and position.
#[derive(Copy, Clone, Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    line: u32,
    column: u32,
}

impl ParseError {
    fn new(kind: ParseErrorKind, offset: u32, input: &str) -> ParseError {
        let (line, column) = compute_loc(input, offset);
        Self { kind, line, column }
    }
    /// Returns the source code location of this error.
    pub fn loc(self: &Self) -> (u32, u32) {
        (self.line, self.column)
    }
}

// comment whitespace handling

#[inline]
pub fn is_whitespace(chr: char) -> bool {
    chr == ' ' || chr == '\t' || chr == '\r' || chr == '\n'
}

#[inline]
pub fn not_eol(chr: char) -> bool {
    chr != '\r' && chr != '\n'
}

named!(space(Input<'_>) -> Input<'_>, recognize!(many1!(alt!(
    preceded!(tag!("//"), take_while!(not_eol))
    | preceded!(tag!("/*"), take_until_and_consume!("*/"))
    | take_while!(is_whitespace)
))));

macro_rules! ws (
    ($i:expr, $($args:tt)*) => ({
        use nom::{Convert, Err};
        match sep!($i, space, $($args)*) {
            Err(e) => Err(e),
            Ok((i1,o)) => {
                match space(i1) {
                    Err(e) => Err(Err::convert(e)),
                    Ok((i2,_)) => Ok((i2, o))
                }
            }
        }
    })
);

// identifier [a-z_][a-z0-9_]*

named!(label(Input<'_>) -> &str, map!(recognize!(tuple!(take_while1!(|m| is_alphabetic(m as u8) || m == '_'), take_while!(|m| is_alphanumeric(m as u8) || m == '_'))), |s| *s));

named!(ident(Input<'_>) -> Ident, do_parse!(
    position: rest_len >>
    result: map!(label, |l| Ident {
        name: l,
        position: position as u32,
    }) >>
    (result)
));

// path

named!(path(Input<'_>) -> Path, do_parse!(
    position: rest_len >>
    result: map!(ws!(separated_nonempty_list!(tag!("::"), label)), |m| Path { position: position as u32, name: m.into_iter().collect() }) >>
    (result)
));

// literal numerical

named!(opt_sign(Input<'_>) -> Option<Input<'_>>, opt!(recognize!(one_of!("+-"))));
named!(opt_fract(Input<'_>) -> Option<Input<'_>>, opt!(recognize!(tuple!(tag!("."), not!(char!('.')), digit0)))); // not(.) to avoid matching ranges
named!(opt_type(Input<'_>) -> Option<Input<'_>>, opt!(recognize!(tuple!(one_of!("iuf"), alt!(tag!("8") | tag!("16") | tag!("32") | tag!("64"))))));

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

fn parse_numerical(n: Input<'_>, position: u32) -> IResult<Input<'_>, Literal<'_>> {
    let (value, type_name) = parse_numerical_suffix(n);
    if value.contains(".") || type_name == Some("f32") || type_name == Some("f64") {
        if let Ok(float) = str::parse::<f64>(value) {
            //println!("f {} {:?}", float, type_name);
            return Ok((n, Literal {
                position    : position,
                value       : LiteralValue::Numeric(Numeric::Float(float)),
                type_name   : type_name.map(|ty| TypeName::from_str(ty, 0)),
                binding_id  : None,
            }))
        }
    } else if value.starts_with("-") || type_name == Some("i8") || type_name == Some("i16") || type_name == Some("i32") || type_name == Some("i64") {
        if let Ok(integer) = str::parse::<i64>(value) {
            //println!("i {} {:?}", integer, type_name);
            return Ok((n, Literal {
                position    : position,
                value       : LiteralValue::Numeric(Numeric::Signed(integer)),
                type_name   : type_name.map(|ty| TypeName::from_str(ty, 0)),
                binding_id  : None,
            }))
        }
    } else {
        if let Ok(integer) = str::parse::<u64>(value) {
            //println!("u {} {:?}", integer, type_name);
            return Ok((n, Literal {
                position    : position,
                value       : LiteralValue::Numeric(Numeric::Unsigned(integer)),
                type_name   : type_name.map(|ty| TypeName::from_str(ty, 0)),
                binding_id  : None,
            }))
        }
    }

    Err(Error::Failure(Context::Code(n, ErrorKind::Custom(ParseErrorKind::InvalidNumerical as u32))))
}

named!(numerical(Input<'_>) -> Literal<'_>, do_parse!(
    position: rest_len >>
    result: flat_map!(recognize!(tuple!(opt_sign, digit1, opt_fract, opt_type)), |input| parse_numerical(input, position as u32)) >>
    (result)
));

named!(static_size(Input<'_>) -> u32, map!(recognize!(digit1), |digits| str::parse::<u32>(*digits).unwrap()));

// literal boolean

named!(boolean(Input<'_>) -> Literal<'_>, do_parse!(
    position: rest_len >>
    result: map!(alt!(tag!("true") | tag!("false")), |m| {
        Literal {
            position    : position as u32,
            value       : LiteralValue::Bool(*m == "true"),
            type_name   : None,
            binding_id  : None,
        }
    }) >>
    (result)
));

// literal string

named!(string(Input<'_>) -> Literal<'_>, do_parse!(
    position: rest_len >>
    result: map!(delimited!(char!('"'), escaped!(none_of!("\\\""), '\\', one_of!("\"n\\")), char!('"')), |m| {
        Literal {
            position    : position as u32,
            value       : LiteralValue::String(*m),
            type_name   : None,
            binding_id  : None,
        }
    }) >>
    (result)
));

// literal array

named!(array_literal_elements(Input<'_>) -> Vec<Literal<'_>>, ws!(separated_list_complete!(char!(','), literal)));

named!(array_literal(Input<'_>) -> Literal<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(delimited!(char!('['), array_literal_elements, char!(']'))), |m| Literal {
        position: position as u32,
        value: LiteralValue::Array(ArrayLiteral {
            elements: m,
        }),
        type_name: None,
        binding_id: None,
    }) >>
    (result)
));

// struct literal

named!(struct_literal_field(Input<'_>) -> (&str, Literal<'_>), map!(ws!(tuple!(label, char!(':'), literal)),
    |tuple| (tuple.0, tuple.2)
));

named!(struct_literal_fields(Input<'_>) -> HashMap<&str, Literal<'_>>, map!(ws!(separated_list_complete!(char!(','), struct_literal_field)), |list| {
    list.into_iter().map(|item| (item.0, item.1)).collect()
}));

named!(struct_literal(Input<'_>) -> Literal<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(tuple!(path, char!('{'), struct_literal_fields, opt!(char!(',')), char!('}'))), |m| Literal {
        position: position as u32,
        value: LiteralValue::Struct(StructLiteral {
            fields: m.2,
        }),
        type_name: Some(TypeName::from_path(m.0)),
        binding_id: None,
    }) >>
    (result)
));

// general literal

named!(literal<Input<'_>, Literal<'_>>, ws!(alt!(boolean | string | array_literal | struct_literal | numerical)));

// assignment

named!(assignable(Input<'_>) -> Expression<'_>, ws!(do_parse!(
    var_position: rest_len >>
    init: map!(ident, |m| Expression::Variable(Variable { position: var_position as u32, ident: m, binding_id: None })) >>
    op_position: rest_len >>
    res: fold_many0!(
        alt!(
            map!(delimited!(tag!("["), expression, tag!("]")), |e| (BinaryOperator::IndexWrite, e))
            | map!(preceded!(tag!("."), ident), |i| (BinaryOperator::AccessWrite, Expression::Member(Member { position: op_position as u32, ident: i, binding_id: None, index: None })))
        ),
        init,       // todo: one of the positions must be wrong ;)
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: op_position as u32, op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(assignment_operator(Input<'_>) -> BinaryOperator, map!(alt!(tag!("=") | tag!("+=") | tag!("-=") | tag!("*=") | tag!("/=")| tag!("%=")), |o| {
    BinaryOperator::from_string(*o)
}));

named!(assignment(Input<'_>) -> Assignment<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(tuple!(assignable, assignment_operator, expression)), |m| {
        Assignment {
            position: position as u32,
            op      : m.1,
            left    : m.0,
            right   : m.2,
        }
    }) >>
    (result)
));

// call

named!(call_argument_list(Input<'_>) -> Vec<Expression<'_>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), expression), char!(')'))));

named!(call(Input<'_>) -> Call<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(tuple!(ident, call_argument_list)), |m| Call {
        position        : position as u32,
        ident           : m.0,
        args            : m.1,
        function_id     : None,
        rust_fn_index   : None,
        binding_id      : None,
    }) >>
    (result)
));

// block

named!(block_items(Input<'_>) -> Vec<Statement<'_>>, ws!(many0!(statement)));

named!(block(Input<'_>) -> Block<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(tuple!(char!('{'), block_items, opt!(expression), char!('}'))), |mut m| {
        // move last block item into result if it could be an expression and no result was matched
        if m.2.is_none() && m.1.last().map_or(false, |l| l.is_expressable()) {
            m.2 = m.1.pop().map(|s| s.into_expression());
        }
        Block {
            position        : position as u32,
            statements      : m.1,
            result          : m.2,
            scope_id        : None,
            explicit_return : false,
        }
    }) >>
    (result)
));

// if

named!(block_or_if(Input<'_>) -> Block<'_>, do_parse!(
    position: rest_len >>
    result: ws!(alt!(
        map!(if_block, |m| Block {
            position        : position as u32,
            statements      : Vec::new(),
            result          : Some(Expression::IfBlock(Box::new(m))),
            scope_id        : None,
            explicit_return : false,
        })
        | block
    )) >>
    (result)
));

named!(if_else(Input<'_>) -> Block<'_>, preceded!(tag!("else"), return_error!(
    ErrorKind::Custom(ParseErrorKind::SyntaxElse as u32),
    block_or_if
)));

named!(if_block(Input<'_>) -> IfBlock<'_>, do_parse!(
    position: rest_len >>
    result: preceded!(tag!("if"), map!(return_error!(
            ErrorKind::Custom(ParseErrorKind::SyntaxIf as u32),
            tuple!(expression, block, opt!(if_else))
        ), |m| IfBlock {
            position    : position as u32,
            cond        : m.0,
            if_block    : m.1,
            else_block  : m.2,
            scope_id    : None,
        }
    )) >>
    (result)
));

// expression

named!(parens(Input<'_>) -> Expression<'_>, ws!(delimited!(char!('('), expression, char!(')'))));

named!(prefix(Input<'_>) -> UnaryOp<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(pair!(alt!(tag!("++") | tag!("--")), assignable)), |m| UnaryOp {
        position    : position as u32,
        op          : UnaryOperator::prefix_from_string(*m.0),
        expr        : m.1,
        binding_id  : None,
    }) >>
    (result)
));

named!(suffix(Input<'_>) -> UnaryOp<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(pair!(assignable, alt!(tag!("++") | tag!("--")))), |m| UnaryOp {
        position    : position as u32,
        op          : UnaryOperator::suffix_from_string(*m.1),
        expr        : m.0,
        binding_id  : None,
    }) >>
    (result)
));

named!(unary(Input<'_>) -> Expression<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(preceded!(tag!("!"), prec6)), |m| {
        Expression::UnaryOp(Box::new(UnaryOp {
            position    : position as u32,
            op          : UnaryOperator::Not,
            expr        : m,
            binding_id  : None,
        }))
    }) >>
    (result)
));

named!(operand(Input<'_>) -> Expression<'_>, do_parse!(
    position: rest_len >>
    result: ws!(alt!( // todo: this may require complete around alternatives: see "BE CAREFUL" in https://docs.rs/nom/4.1.1/nom/macro.alt.html
        map!(boolean, |m| Expression::Literal(m))
        | map!(string, |m| Expression::Literal(m))
        | map!(if_block, |m| Expression::IfBlock(Box::new(m)))
        | map!(block, |m| Expression::Block(Box::new(m)))
        | parens
        | map!(suffix, |m| Expression::UnaryOp(Box::new(m)))
        | map!(prefix, |m| Expression::UnaryOp(Box::new(m)))
        | map!(numerical, |m| Expression::Literal(m))
        | map!(call, |m| Expression::Call(m))
        | map!(ident, |m| Expression::Variable(Variable { position: position as u32, ident: m, binding_id: None }))
    )) >>
    (result)
));

named!(prec7(Input<'_>) -> Expression<'_>, ws!(do_parse!(
    init: operand >>
    position: rest_len >>
    res: fold_many0!(
        alt!(
            map!(delimited!(tag!("["), expression, tag!("]")), |e| (BinaryOperator::Index, e))
            | map!(preceded!(tag!("."), ident), |i| (BinaryOperator::Access, Expression::Member(Member { position: position as u32, ident: i, binding_id: None, index: None })))
        ),
        init, // todo: one of the position values must be wrong :)
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position as u32, op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec6(Input<'_>) -> Expression<'_>, alt!(prec7 | unary));

named!(prec5(Input<'_>) -> Expression<'_>, ws!(do_parse!(
    init: prec6 >>
    position: rest_len >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("*") | tag!("/") | tag!("%")), |o| BinaryOperator::from_string(*o)), prec6),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position as u32, op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec4(Input<'_>) -> Expression<'_>, ws!(do_parse!(
    init: prec5 >>
    position: rest_len >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("+") | tag!("-")), |o| BinaryOperator::from_string(*o)), prec5),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position as u32, op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec3(Input<'_>) -> Expression<'_>, ws!(do_parse!(
    init: prec4 >>
    position: rest_len >>
    res: fold_many0!( // todo: does this work? see comment on "operand"
        pair!(map!(alt!(tag!("<=") | tag!(">=") | tag!("<") | tag!(">")), |o| BinaryOperator::from_string(*o)), prec4),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position as u32, op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec2(Input<'_>) -> Expression<'_>, ws!(do_parse!(
    init: prec3 >>
    position: rest_len >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("!=") | tag!("==")), |o| BinaryOperator::from_string(*o)), prec3),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position as u32, op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec1(Input<'_>) -> Expression<'_>, ws!(do_parse!(
    init: prec2 >>
    position: rest_len >>
    res: fold_many0!(
        pair!(map!(tag!("&&"), |o| BinaryOperator::from_string(*o)), prec2),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position as u32, op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(prec0(Input<'_>) -> Expression<'_>, ws!(do_parse!(
    init: prec1 >>
    position: rest_len >>
    res: fold_many0!(
        pair!(map!(tag!("||"), |o| BinaryOperator::from_string(*o)), prec1),
        init,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position as u32, op: op, left: acc, right: val, binding_id: None }))
    ) >>
    (res)
)));

named!(precn(Input<'_>) -> Expression<'_>, ws!(do_parse!(
    init: prec0 >>
    position: rest_len >>
    res: fold_many0!(
        preceded!(tag!("as"), path),
        init,
        |acc, val| Expression::Cast(Box::new(Cast { position: position as u32, expr: acc, ty: TypeName::from_path(val), binding_id: None }))
    ) >>
    (res)
)));

named!(expression(Input<'_>) -> Expression<'_>, ws!(alt!(
    map!(assignment, |m| Expression::Assignment(Box::new(m)))
    | map!(array_literal, |m| Expression::Literal(m))
    | map!(struct_literal, |m| Expression::Literal(m))
    | precn
)));

// let

named!(binding(Input<'_>) -> Statement<'_>, do_parse!(
    position: rest_len >>
    result: map!(
        preceded!(tag!("let"), return_error!(
            ErrorKind::Custom(ParseErrorKind::SyntaxLet as u32),
            ws!(tuple!(opt!(tag!("mut")), ident, opt!(preceded!(char!(':'), path)), opt!(preceded!(char!('='), expression)), char!(';')))
        )),
        |m| Statement::Binding(Binding {
            position    : position as u32,
            ident       : m.1,
            mutable     : m.0.is_some(),
            expr        : m.3,
            type_name   : m.2.map(|t| TypeName::from_path(t)),
            binding_id  : None,
        })
    ) >>
    (result)
));

// inline type
// todo: everything using TypeName needs to be switched to this
named!(inline_type(Input<'_>) -> InlineType<'_>, alt!(
    map!(path, |t| InlineType::TypeName(TypeName::from_path(t)))
    | map!(array, |a| InlineType::Array(Box::new(a)))
));

// struct definition

named!(struct_field(Input<'_>) -> (&str, InlineType<'_>), map!(ws!(tuple!(label, char!(':'), inline_type)),
    |tuple| (tuple.0, tuple.2)
));

named!(struct_fields(Input<'_>) -> Vec<(&str, InlineType<'_>)>, map!(ws!(separated_list_complete!(char!(','), struct_field)), |list| {
    list.into_iter().map(|item| (item.0, item.1)).collect()
}));

named!(struct_(Input<'_>) -> Statement<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(tuple!(tag!("struct"), ident, char!('{'), struct_fields, opt!(char!(',')), char!('}'))), |tuple| Statement::Structure(Struct {
        position: position as u32,
        ident   : tuple.1,
        fields  : tuple.3,
        type_id : None,
    })) >>
    (result)
));

// array definition

named!(array(Input<'_>) -> Array<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(delimited!(char!('['), tuple!(inline_type, char!(';'), static_size), char!(']'))), |tuple| Array {
        position    : position as u32,
        element_type: tuple.0,
        len         : tuple.2,
        type_id     : None,
    }) >>
    (result)
));

// function

named!(signature_argument(Input<'_>) -> Binding<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(tuple!(opt!(tag!("mut")), ident, char!(':'), path)), |tuple| Binding {
        position    : position as u32,
        ident       : tuple.1,
        expr        : None,
        mutable     : tuple.0.is_some(),
        type_name   : Some(TypeName::from_path(tuple.3)),
        binding_id  : None,
    }) >>
    (result)
));

named!(signature_argument_list(Input<'_>) -> Vec<Binding<'_>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), signature_argument), char!(')'))));

named!(signature_return_part(Input<'_>) -> Path, ws!(preceded!(tag!("->"), path)));

named!(signature(Input<'_>) -> Signature<'_>, map!(
    preceded!(tag!("fn"), return_error!(
        ErrorKind::Custom(ParseErrorKind::SyntaxFn as u32),
        ws!(tuple!(ident, signature_argument_list, opt!(signature_return_part)))
    )), |sig| Signature {
    ident   : sig.0,
    args    : sig.1,
    ret     : if let Some(sig_ty) = sig.2 { Some(TypeName::from_path(sig_ty)) } else { None },
}));

named!(function(Input<'_>) -> Statement<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(tuple!(signature, block)), |func| Statement::Function(Function {
        position    : position as u32,
        sig         : func.0,
        block       : func.1,
        function_id : None,
        scope_id    : None,
    })) >>
    (result)
));

// for

// TODO: accept "for ident in expression" and make .. an operator. implement iterators.

named!(for_loop_range(Input<'_>) -> Expression<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(tuple!(expression, alt!(tag!("..=") | tag!("..")), expression)), |m| {
        Expression::BinaryOp(Box::new(BinaryOp {
            position    : position as u32,
            op          : BinaryOperator::from_string(*m.1),
            left        : m.0,
            right       : m.2,
            binding_id  : None
        }))
    }) >>
    (result)
));

named!(for_loop(Input<'_>) -> ForLoop<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(tuple!(tag!("for"), ident, tag!("in"), alt!(for_loop_range | expression), block)), |m| ForLoop {
        position: position as u32,
        iter: Binding {
            position    : position as u32,
            ident       : m.1,
            mutable     : true,
            expr        : None,
            type_name   : None,
            binding_id  : None,
        },
        range   : m.3,
        block   : m.4,
        scope_id: None,
    }) >>
    (result)
));

// while loop

named!(while_loop(Input<'_>) -> WhileLoop<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(preceded!(tag!("while"), tuple!(expression, block))), |m| WhileLoop {
        position: position as u32,
        expr    : m.0,
        block   : m.1,
        scope_id: None,
    }) >>
    (result)
));

// return

named!(return_statement(Input<'_>) -> Statement<'_>, do_parse!(
    position: rest_len >>
    result: map!(ws!(preceded!(tag!("return"), terminated!(opt!(expression), char!(';')))),
        |m| Statement::Return(Return {
            position        : position as u32,
            expr            : m,
            fn_ret_type_id  : None,
        })
    ) >>
    (result)
));

// statement

named!(statement(Input<'_>) -> Statement<'_>, alt!(
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

named!(root(Input<'_>) -> Vec<Statement<'_>>, ws!(many0!(statement)));

/// Parsed program AST.
#[derive(Debug)]
pub struct ParsedProgram<'a> (pub Vec<Statement<'a>>);

/// Attempt to return the innermost custom error, otherwise the innermost error.
fn innermost_custom(input: &str, list: &[ (Input<'_>, ErrorKind) ]) -> ParseError {
    use std::mem::transmute;

    let err = list.iter().find(|(_, error_kind)| match error_kind {
        ErrorKind::Custom(_) => true,
        _ => false,
    }).or(list.last());

    let (remainder, error_kind) = err.unwrap();
    match error_kind {
        ErrorKind::Custom(custom) => {
            ParseError::new(unsafe { transmute(*custom) }, (input.len() - remainder.len()) as u32, input) // todo: figure out how to specify custom error type
        }
        _ => {
            ParseError::new(ParseErrorKind::Unknown, (input.len() - remainder.len()) as u32, input)
        }
    }
}

/// Parses an Itsy source file into a program AST structure.
pub fn parse(input: &str) -> Result<ParsedProgram<'_>, ParseError> {
    let result = root(Input(input));
    match result {
        Ok(result) => {
            if result.0.len() > 0 {
                Err(ParseError::new(ParseErrorKind::UnexpectedEOF, input.len() as u32, input))
            } else {
                Ok(ParsedProgram(result.1))
            }
        },
        Err(error) => {
            Err(match error {
                Error::Incomplete(incomplete) => panic!("Unexpected Nom Incomplete({:?})", incomplete),
                Error::Error(error) => panic!("Unexpected Nom Error({:?})", error),
                Error::Failure(failure) => {
                    #[allow(unreachable_patterns)]
                    match failure {
                        Context::List(list) => innermost_custom(input, &list[..]),
                        _ => panic!("Incompatible Nom configuration. Verbose errors need to be enabled")
                    }
                }
            })
        }
    }
}
