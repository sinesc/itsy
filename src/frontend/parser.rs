//! Nom parsers used to generate the Itsy AST.

pub(crate) mod error;
#[macro_use] mod nomutil;
use nomutil::*;

use std::collections::HashMap;
use std::fmt::Debug;
use nom::Parser;
use nom::error::ParseError as NomParseErrorTraitInScope; // need trait in scope but conflicts with our error names and we don't want to change those
use nom::character::{is_alphanumeric, is_alphabetic, complete::{none_of, one_of, digit0, char, digit1}};
use nom::bytes::complete::{take_while, take_while1, tag, escaped};
use nom::combinator::{recognize, opt, all_consuming, map, not};
use nom::multi::{separated_list0, separated_list1, many0};
use nom::branch::alt;
use nom::sequence::{tuple, pair, delimited, preceded, terminated};
use crate::util::{Numeric, FnKind, StackAddress};
use crate::frontend::ast::*;
use error::{ParseError, ParseErrorKind};

/// Parsed program AST.
#[derive(Debug)]
pub struct ParsedProgram<'a> (pub Vec<Statement<'a>>);

#[derive(Clone, Copy, Debug)]
struct ParserState {

}

impl ParserState {
    fn new() -> Self {
        ParserState {

        }
    }
}

// identifier ([a-z_][a-z0-9_]*)

fn label(i: Input<'_>) -> Output<&str> {
    map(
        recognize(tuple((
            take_while1(|m| is_alphabetic(m as u8) || m == '_'),
            take_while(|m| is_alphanumeric(m as u8) || m == '_')
        ))),
        move |l: Input<'_>| *l
    )(i)
}

fn ident(i: Input<'_>) -> Output<Ident> {
    let position = i.position();
    map(
        label,
        move |l| Ident {
            name: l,
            position: position,
        }
    )(i)
}

// path (a::b)

fn path(i: Input<'_>) -> Output<Path> {
    let position = i.position();
    map(separated_list1(ws(tag("::")), ws(label)), move |m| Path { position: position, name: m.into_iter().collect() })(i)
}

// literal numerical (-3.14f32)

fn opt_sign(i: Input<'_>) -> Output<Option<Input<'_>>> {
    opt(recognize(one_of("+-")))(i)
}

fn opt_fract(i: Input<'_>) -> Output<Option<Input<'_>>> {
    opt(recognize(tuple((tag("."), not(char('.')), digit0))))(i) // not(.) to avoid matching ranges
}

fn opt_type(i: Input<'_>) -> Output<Option<Input<'_>>> {
    opt(recognize(tuple((one_of("iuf"), alt((tag("8"), tag("16"), tag("32"), tag("64")))))))(i)
}

fn numerical(i: Input<'_>) -> Output<Literal<'_>> {

    let position = i.position();
    let (remaining, numerical) = recognize(tuple((opt_sign, digit1, opt_fract, opt_type))).parse(i.clone())?;
    let (value, type_name) = splits_numerical_suffix(numerical);

    if value.contains(".") || type_name == Some("f32") || type_name == Some("f64") {
        if let Ok(float) = str::parse::<f64>(value) {
            return Ok((remaining, Literal {
                position    : position,
                value       : LiteralValue::Numeric(Numeric::Float(float)),
                type_name   : type_name.map(|ty| TypeName::from_str(ty, 0)),
                binding_id  : None,
            }));
        }
    } else if value.starts_with("-") || type_name == Some("i8") || type_name == Some("i16") || type_name == Some("i32") || type_name == Some("i64") {
        if let Ok(integer) = str::parse::<i64>(value) {
            if check_signed_range(integer, type_name) {
                return Ok((remaining, Literal {
                    position    : position,
                    value       : LiteralValue::Numeric(Numeric::Signed(integer)),
                    type_name   : type_name.map(|ty| TypeName::from_str(ty, 0)),
                    binding_id  : None,
                }));
            }
        }
    } else {
        if let Ok(integer) = str::parse::<u64>(value) {
            if check_unsigned_range(integer, type_name) {
                return Ok((remaining, Literal {
                    position    : position,
                    value       : LiteralValue::Numeric(Numeric::Unsigned(integer)),
                    type_name   : type_name.map(|ty| TypeName::from_str(ty, 0)),
                    binding_id  : None,
                }));
            }
        }
    }

    Err(nom::Err::Error(Error::from_error_kind(i, nom::error::ErrorKind::Alpha))) // Todo
}

fn static_size(i: Input<'_>) -> Output<StackAddress> {
    ws(map(recognize(digit1), |digits: Input<'_>| str::parse::<StackAddress>(*digits).unwrap()))(i)
}

// literal boolean (true)

fn boolean(i: Input<'_>) -> Output<Literal<'_>> {
    let position = i.position();
    map(alt((tag("true"), tag("false"))), move |m: Input<'_>| {
        Literal {
            position    : position,
            value       : LiteralValue::Bool(*m == "true"),
            type_name   : None,
            binding_id  : None,
        }
    })(i)
}

// literal string ("hello world")

fn string(i: Input<'_>) -> Output<Literal<'_>> {
    let position = i.position();
    map(
        delimited(char('"'), escaped(none_of("\\\""), '\\', one_of("\"n\\")), char('"')),
        move |m: Input<'_>| {
            Literal {
                position    : position,
                value       : LiteralValue::String(*m),
                type_name   : None,
                binding_id  : None,
            }
        }
    )(i)
}

// literal array ([ 1, 2, 3 ])

fn array_literal_elements(i: Input<'_>) -> Output<Vec<Expression<'_>>> {
    ws(separated_list0(char(','), expression))(i)
}

fn array_literal(i: Input<'_>) -> Output<Literal<'_>> {
    let position = i.position();
    map(
        tuple((ws(char('[')), array_literal_elements, opt(ws(char(','))), ws(char(']')))),
        move |m| Literal {
            position    : position,
            value: LiteralValue::Array(ArrayLiteral {
                elements: m.1,
            }),
            type_name   : None,
            binding_id  : None,
        }
    )(i)
}

// literal struct (MyStruct { a: 1 })

fn struct_literal_field(i: Input<'_>) -> Output<(&str, Expression<'_>)> {
    map(
        tuple((ws(label), ws(char(':')), expression)),
        |tuple| (tuple.0, tuple.2)
    )(i)
}

fn struct_literal_fields(i: Input<'_>) -> Output<HashMap<&str, Expression<'_>>> {
    map(
        separated_list0(char(','), struct_literal_field),
        |list| {
            list.into_iter().map(|item| (item.0, item.1)).collect()
        }
    )(i)
}

fn struct_literal(i: Input<'_>) -> Output<Literal<'_>> {
    let position = i.position();
    map(
        tuple((ws(path), ws(char('{')), struct_literal_fields, opt(ws(char(','))), ws(char('}')))),
        move |m| Literal {
            position    : position,
            value: LiteralValue::Struct(StructLiteral {
                fields: m.2,
            }),
            type_name   : Some(TypeName::from_path(m.0)),
            binding_id  : None,
        }
    )(i)
}

// literal

fn literal(i: Input<'_>) -> Output<Literal<'_>> {
    ws(alt((boolean, string, array_literal, struct_literal, numerical)))(i)
}

// assignment

fn assignable(i: Input<'_>) -> Output<Expression<'_>> {
    let var_position = i.position();
    let init = map(ident, |m| Expression::Variable(Variable { position: var_position as Position, ident: m, binding_id: None }))(i)?;
    let op_position = init.0.position();
    fold_many0(
        alt((
            map(delimited(ws(tag("[")), expression, tag("]")), |e| (BinaryOperator::IndexWrite, e)),
            map(preceded(ws(tag(".")), ident), |i| (BinaryOperator::AccessWrite, Expression::Member(Member { position: op_position as Position, ident: i, binding_id: None, index: None })))
        )),
        init.1,
        |mut acc, (op, val)| {
            // update left part of the expression to ensure only the final index/access is a write operation. // TODO: find less clunky solution
            match &mut acc {
                Expression::BinaryOp(exp) => {
                    if exp.op == BinaryOperator::AccessWrite {
                        exp.op = BinaryOperator::Access;
                    } else if exp.op == BinaryOperator::IndexWrite {
                        exp.op = BinaryOperator::Index;
                    }
                }
                _ => {}
            }
            Expression::BinaryOp(Box::new(BinaryOp { position: op_position as Position, op: op, left: acc, right: val, binding_id: None }))
        }
    )(init.0)
}

fn assignment_operator(i: Input<'_>) -> Output<BinaryOperator> {
    ws(map(
        alt((tag("="), tag("+="), tag("-="), tag("*="), tag("/="), tag("%="))),
        |o: Input<'_>| {
            BinaryOperator::from_string(*o)
        }
    ))(i)
}

fn assignment(i: Input<'_>) -> Output<Assignment<'_>> {
    let position = i.position();
    ws(map(
        tuple((assignable, assignment_operator, expression)),
        move |m| {
            Assignment {
                position: position,
                op      : m.1,
                left    : m.0,
                right   : m.2,
                binding_id: None,
            }
        }
    ))(i)
}

// call

fn call_argument_list(i: Input<'_>) -> Output<Vec<Expression<'_>>> {
    delimited(ws(char('(')), separated_list0(ws(char(',')), expression), ws(char(')')))(i)
}

fn call(i: Input<'_>) -> Output<Call<'_>> {
    let position = i.position();
    ws(map(
        tuple((ident, space0, call_argument_list)),
        move |m| Call {
            position        : position,
            ident           : m.0,
            args            : m.2,
            call_type       : CallType::Function,
            call_kind       : FnKind::User,
            function_id     : None,
            binding_id      : None,
        }
    ))(i)
}


fn call_static(i: Input<'_>) -> Output<Call<'_>> {
    let position = i.position();
    map(
        tuple((path, call_argument_list)),
        move |mut m| {
            let ident = Ident { position: position, name: m.0.pop() };
            Call {
                position        : position,
                ident           : ident,
                args            : m.1,
                call_type       : CallType::Static(m.0),
                call_kind       : FnKind::User,
                function_id     : None,
                binding_id      : None,
            }
        }
    )(i)
}

// block

fn block_items(i: Input<'_>) -> Output<Vec<Statement<'_>>> {
    ws(many0(statement))(i)
}

fn block(i: Input<'_>) -> Output<Block<'_>> {
    let position = i.position();
    ws(map(
        tuple((char('{'), block_items, opt(expression), char('}'))),
        move |mut m| {
            // move last block item into result if it could be an expression and no result was matched
            if m.2.is_none() && m.1.last().map_or(false, |l| l.maybe_expression()) {
                m.2 = m.1.pop().map(|s| s.into_expression().unwrap());
            }
            Block {
                position        : position,
                statements      : m.1,
                result          : m.2,
                returns         : None,
                scope_id        : None,
            }
        }
    ))(i)
}

// if

fn if_else(i: Input<'_>) -> Output<Block<'_>> {
    let position = i.position();
    preceded(tag("else"), alt((
        sepl(map(if_block, move |m| Block {
            position        : position,
            statements      : Vec::new(),
            result          : Some(Expression::IfBlock(Box::new(m))),
            returns         : None,
            scope_id        : None,
        })),
        block
    )))(i)
}

fn if_block(i: Input<'_>) -> Output<IfBlock<'_>> {
    let position = i.position();
    ws(preceded(sepr(tag("if")), map(
        tuple((expression, block, opt(if_else))),
        move |m| IfBlock {
            position    : position,
            cond        : m.0,
            if_block    : m.1,
            else_block  : m.2,
            scope_id    : None,
        }
    )))(i)
}

// expression

fn parens(i: Input<'_>) -> Output<Expression<'_>> {
    ws(delimited(char('('), expression, char(')')))(i)
}

fn prefix(i: Input<'_>) -> Output<UnaryOp<'_>> {
    let position = i.position();
    map(
        pair(ws(alt((tag("++"), tag("--")))), ws(assignable)),
        move |m| UnaryOp {
            position    : position,
            op          : UnaryOperator::prefix_from_string(*m.0),
            expr        : m.1,
            binding_id  : None,
        }
    )(i)
}

fn suffix(i: Input<'_>) -> Output<UnaryOp<'_>> {
    let position = i.position();
    map(
        pair(ws(assignable), ws(alt((tag("++"), tag("--"))))),
        move |m| UnaryOp {
            position    : position,
            op          : UnaryOperator::suffix_from_string(*m.1),
            expr        : m.0,
            binding_id  : None,
        }
    )(i)
}

fn unary(i: Input<'_>) -> Output<Expression<'_>> {
    let position = i.position();
    map(
        preceded(ws(tag("!")), ws(prec6)),
        move |m| {
            Expression::UnaryOp(Box::new(UnaryOp {
                position    : position,
                op          : UnaryOperator::Not,
                expr        : m,
                binding_id  : None,
            }))
        }
    )(i)
}

fn operand(i: Input<'_>) -> Output<Expression<'_>> {
    let position = i.position();
    ws(alt((
        map(literal, |m| Expression::Literal(m)),
        map(if_block, |m| Expression::IfBlock(Box::new(m))),
        map(block, |m| Expression::Block(Box::new(m))),
        parens,
        map(suffix, |m| Expression::UnaryOp(Box::new(m))),
        map(prefix, |m| Expression::UnaryOp(Box::new(m))),
        map(call, |m| Expression::Call(m)),
        map(call_static, |m| Expression::Call(m)),
        map(ident, move |m| Expression::Variable(Variable { position: position, ident: m, binding_id: None }))
    )))(i)
}

fn prec7(i: Input<'_>) -> Output<Expression<'_>> {
    let init = operand(i.clone())?;
    let position = i.position();
    fold_many0(
        alt((
            map(delimited(ws(tag("[")), expression, ws(tag("]"))), |e| (BinaryOperator::Index, e)),
            map(preceded(ws(tag(".")), call), |i| (BinaryOperator::Access, Expression::Call(i))),
            map(preceded(ws(tag(".")), ident), |i| (BinaryOperator::Access, Expression::Member(Member { position: position, ident: i, binding_id: None, index: None })))
        )),
        init.1,
        move |acc, (op, mut val)| match &mut val {
            Expression::Call(call) if op == BinaryOperator::Access => {
                call.call_type = CallType::Method(Box::new(acc));
                val
            },
            _ => Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, binding_id: None }))
        }
    )(init.0)
}

fn prec6(i: Input<'_>) -> Output<Expression<'_>> {
    alt((prec7, unary))(i)
}

fn prec5(i: Input<'_>) -> Output<Expression<'_>> {
    let init = prec6(i)?;
    let position = init.0.position();
    fold_many0(
        pair(map(alt((tag("*"), tag("/"), tag("%"))), |o: Input<'_>| BinaryOperator::from_string(*o)), prec6),
        init.1,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, binding_id: None }))
    )(init.0)
}

fn prec4(i: Input<'_>) -> Output<Expression<'_>> {
    let init = prec5(i)?;
    let position = init.0.position();
    fold_many0(
        pair(map(alt((tag("+"), tag("-"))), |o: Input<'_>| BinaryOperator::from_string(*o)), prec5),
        init.1,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, binding_id: None }))
    )(init.0)
}

fn prec3(i: Input<'_>) -> Output<Expression<'_>> {
    let init = prec4(i)?;
    let position = init.0.position();
    fold_many0(
        pair(map(alt((tag("<="), tag(">="), tag("<"), tag(">"))), |o: Input<'_>| BinaryOperator::from_string(*o)), prec4),
        init.1,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, binding_id: None }))
    )(init.0)
}

fn prec2(i: Input<'_>) -> Output<Expression<'_>> {
    let init = prec3(i)?;
    let position = init.0.position();
    fold_many0(
        pair(map(alt((tag("!="), tag("=="))), |o: Input<'_>| BinaryOperator::from_string(*o)), prec3),
        init.1,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, binding_id: None }))
    )(init.0)
}

fn prec1(i: Input<'_>) -> Output<Expression<'_>> {
    let init = prec2(i)?;
    let position = init.0.position();
    fold_many0(
        pair(map(tag("&&"), |o: Input<'_>| BinaryOperator::from_string(*o)), prec2),
        init.1,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, binding_id: None }))
    )(init.0)
}

fn prec0(i: Input<'_>) -> Output<Expression<'_>> {
    let init = prec1(i)?;
    let position = init.0.position();
    fold_many0(
        pair(map(tag("||"), |o: Input<'_>| BinaryOperator::from_string(*o)), prec1),
        init.1,
        |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, binding_id: None }))
    )(init.0)
}

fn precn(i: Input<'_>) -> Output<Expression<'_>> {
    let init = prec0(i)?;
    let position = init.0.position();
    fold_many0(
        preceded(tag("as"), path),
        init.1,
        |acc, val| Expression::Cast(Box::new(Cast { position: position, expr: acc, ty: TypeName::from_path(val), binding_id: None }))
    )(init.0)
}

fn expression(i: Input<'_>) -> Output<Expression<'_>> {
    ws(alt((
        map(assignment, |m| Expression::Assignment(Box::new(m))),
        precn
    )))(i)
}

// let

fn binding(i: Input<'_>) -> Output<Statement<'_>> {
    let position = i.position();
    ws(map(
        preceded(sepr(tag("let")), tuple((opt(sepr(tag("mut"))), ident, opt(preceded(ws(char(':')), inline_type)), opt(preceded(ws(char('=')), expression)), ws(char(';'))))),
        move |m| Statement::Binding(Binding {
            position    : position,
            ident       : m.1,
            mutable     : m.0.is_some(),
            expr        : m.3,
            ty          : m.2,
            binding_id  : None,
        })
    ))(i)
}

// inline type

fn inline_type(i: Input<'_>) -> Output<InlineType<'_>> {
    ws(map(
        alt((
            map(path, |t| InlineTypeKind::TypeName(TypeName::from_path(t))),
            map(array, |a| InlineTypeKind::Array(Box::new(a)))
        )),
        |pair| InlineType { kind: pair }
    ))(i)
}

// struct definition

fn struct_field(i: Input<'_>) -> Output<(&str, InlineType<'_>)> {
    map(
        tuple((ws(label), ws(char(':')), ws(inline_type))),
        |tuple| (tuple.0, tuple.2)
    )(i)
}

fn struct_fields(i: Input<'_>) -> Output<Vec<(&str, InlineType<'_>)>> {
    map(
        separated_list1(ws(char(',')), ws(struct_field)),
        |list| {
            list.into_iter().map(|item| (item.0, item.1)).collect()
        }
    )(i)
}

fn struct_(i: Input<'_>) -> Output<Statement<'_>> {
    let position = i.position();
    ws(map(
        tuple((sepr(tag("struct")), ident, ws(char('{')), struct_fields, opt(ws(char(','))), ws(char('}')))),
        move |tuple| Statement::Structure(Struct {
            position    : position,
            ident       : tuple.1,
            fields      : tuple.3,
            binding_id  : None,
        })
    ))(i)
}

// array definition

fn array(i: Input<'_>) -> Output<Array<'_>> {
    let position = i.position();
    ws(map(
        delimited(ws(char('[')), tuple((ws(inline_type), ws(char(';')), ws(static_size))), ws(char(']'))),
        move |tuple| Array {
            position    : position,
            element_type: tuple.0,
            len         : tuple.2,
            binding_id  : None,
        }
    ))(i)
}

// function definition

fn signature_argument(i: Input<'_>) -> Output<Binding<'_>> {
    let position = i.position();
    ws(map(
        tuple((opt(sepr(tag("mut"))), ident, ws(char(':')), inline_type)),
        move |tuple| Binding {
            position    : position,
            ident       : tuple.1,
            expr        : None,
            mutable     : tuple.0.is_some(),
            ty          : Some(tuple.3),
            binding_id  : None,
        }
    ))(i)
}

fn signature_argument_list(i: Input<'_>) -> Output<Vec<Binding<'_>>> {
    delimited(ws(char('(')), separated_list0(ws(char(',')), ws(signature_argument)), ws(char(')')))(i)
}

fn signature_return_part(i: Input<'_>) -> Output<InlineType> {
    preceded(ws(tag("->")), inline_type)(i)
}

fn signature(i: Input<'_>) -> Output<Signature<'_>> {
    ws(map(
        preceded(sepr(tag("fn")), tuple((ident, ws(signature_argument_list), opt(ws(signature_return_part))))),
        |sig| Signature {
            ident   : sig.0,
            args    : sig.1,
            ret     : if let Some(sig_ty) = sig.2 { Some(sig_ty) } else { None },
        },
    ))(i)
}

fn function(i: Input<'_>) -> Output<Function<'_>> {
    let position = i.position();
    ws(map(
        tuple((signature, block)),
        move |func| Function {
            position    : position,
            sig         : func.0,
            block       : func.1,
            function_id : None,
            scope_id    : None,
        }
    ))(i)
}

// for

fn for_loop_range(i: Input<'_>) -> Output<Expression<'_>> {
    let position = i.position();
    map(
        tuple((ws(expression), ws(alt((tag("..="), tag("..")))), ws(expression))),
        move |m| Expression::BinaryOp(Box::new(BinaryOp {
            position    : position,
            op          : BinaryOperator::from_string(*m.1),
            left        : m.0,
            right       : m.2,
            binding_id  : None
        }))
    )(i)
}

fn for_loop(i: Input<'_>) -> Output<ForLoop<'_>> {
    let position = i.position();
    ws(map(
        tuple((tag("for"), sepl(ident), sepl(tag("in")), sepl(alt((for_loop_range, expression))), block)),
        move |m| ForLoop {
            position: position,
            iter: Binding {
                position    : position,
                ident       : m.1,
                mutable     : true,
                expr        : None,
                ty          : None,
                binding_id  : None,
            },
            range   : m.3,
            block   : m.4,
            scope_id: None,
        }
    ))(i)
}

// while loop

fn while_loop(i: Input<'_>) -> Output<WhileLoop<'_>> {
    let position = i.position();
    ws(map(
        preceded(tag("while"), pair(sepl(expression), block)),
        move |m| WhileLoop {
            position: position,
            expr    : m.0,
            block   : m.1,
            scope_id: None,
        }
    ))(i)
}

// return

fn return_statement(i: Input<'_>) -> Output<Statement<'_>> {
    let position = i.position();
    map(
        preceded(tag("return"), terminated(opt(sepl(expression)), ws(char(';')))),
        move |m| Statement::Return(Return {
            position    : position,
            expr        : m,
        })
    )(i)
}

// statement

fn statement(i: Input<'_>) -> Output<Statement<'_>> {
    ws(alt((
        binding,
        map(if_block, |m| Statement::IfBlock(m)),
        map(function, |m| Statement::Function(m)),
        struct_,
        map(for_loop, |m| Statement::ForLoop(m)),
        map(while_loop, |m| Statement::WhileLoop(m)),
        return_statement,
        map(terminated(expression, char(';')), |m| Statement::Expression(m)),
        map(block, |m| Statement::Block(m)),
    )))(i)
}

// root

fn root(i: Input<'_>) -> Output<Vec<Statement<'_>>> {
    all_consuming(ws(many0(statement)))(i)
}

/// Parses an Itsy source file into a program AST structure.
pub fn parse(src: &str) -> Result<ParsedProgram<'_>, ParseError> {
    let input = Input::new(src);
    let result = root(input.clone());
    match result {
        Ok(result) => {
            Ok(ParsedProgram(result.1))
        },
        Err(err) => {
            match err {
                nom::Err::Incomplete(_) => unreachable!("No parser should return Incomplete"),
                nom::Err::Failure(failure) => {
                    // a failure generated by checkpoint
                    Err(ParseError::new(failure.kind, failure.input.position()))
                }
                nom::Err::Error(_) => {
                    // nom error is useless to us, but we stored the highest parsed offset on the input which is the most likely error position
                    let error = input.max_parsed();
                    Err(ParseError::new(ParseErrorKind::SyntaxError, error.1 as Position))
                }
            }
        }
    }
}
