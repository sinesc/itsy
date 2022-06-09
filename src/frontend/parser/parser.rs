//! Nom parsers used to generate the Itsy AST.

pub mod error;
#[macro_use]
mod nomutil;
pub mod types;

use nom::Parser;
use nom::character::{is_alphanumeric, is_alphabetic, complete::{none_of, one_of, digit0, char, digit1}};
use nom::bytes::complete::{take_while, take_while1, tag, escaped};
use nom::combinator::{recognize, opt, all_consuming, map, not};
use nom::multi::{separated_list0, separated_list1, many0};
use nom::branch::alt;
use nom::sequence::{tuple, pair, delimited, preceded, terminated};
use crate::prelude::UnorderedMap;
use crate::shared::{numeric::Numeric, path_to_parts, parts_to_path};
use crate::frontend::ast::*;
use types::{Input, Output, Error, ParserState, ParsedModule, ParsedProgram};
use error::{ParseResult, ParseError, ParseErrorKind};
use nomutil::*;

fn check_state<'a, O, P, C>(mut parser: P, checker: C) -> impl FnMut(Input<'a>) -> Output<'a, O>
where
    P: Parser<Input<'a>, O, Error<'a>>,
    C: Fn(ParserState) -> Option<ParseErrorKind>
{
    move |input: Input<'_>| {
        let before = input.clone();
        let inner_result = parser.parse(input)?;

        if let Some(kind) = checker(before.state()) {
            Err(nom::Err::Failure(Error { input: before, kind: kind }))
        } else {
            Ok(inner_result)
        }
    }
}

fn with_state<'a, P: 'a, O: 'a>(s: &'a impl Fn(&mut ParserState), mut parser: P) -> impl FnMut(Input<'a>) -> Output<O> where P: FnMut(Input<'a>) -> Output<O> {
    move |input: Input<'_>| {
        let i = input.clone();
        let state = i.state();
        i.state_mut(s);
        let inner_result = parser.parse(input)?;
        i.state_mut(|s| *s = state);
        Ok(inner_result)
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
            name: l.to_string(),
            position: position,
        }
    )(i)
}

// path (a::b)

fn path(i: Input<'_>) -> Output<Path> {
    let position = i.position();
    map(separated_list1(ws(tag("::")), ident), move |name| Path { position, name })(i)
}

// literal numerical (-3.14f32)

fn numerical(i: Input<'_>) -> Output<Literal> {

    /// Splits numerical value from its type suffix (if it has any)
    fn splits_numerical_suffix(n: &str) -> (&str, Option<&str>) {
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
            (n, None)
        }
    }

    /// Returns whether the given signed numerical is within range of the named type.
    fn check_signed_range(num: i64, type_name: Option<&str>) -> bool {
        match type_name {
            Some("i8")  => num >= i8::MIN as i64 && num <= i8::MAX as i64,
            Some("i16") => num >= i16::MIN as i64 && num <= i16::MAX as i64,
            Some("i32") => num >= i32::MIN as i64 && num <= i32::MAX as i64,
            Some("i64") => true,
            None        => true,
            _           => false,
        }
    }

    /// Returns whether the given unsigned numerical is within range of the named type.
    fn check_unsigned_range(num: u64, type_name: Option<&str>) -> bool {
        match type_name {
            Some("u8")  => num >= u8::MIN as u64 && num <= u8::MAX as u64,
            Some("u16") => num >= u16::MIN as u64 && num <= u16::MAX as u64,
            Some("u32") => num >= u32::MIN as u64 && num <= u32::MAX as u64,
            Some("u64") => true,
            None        => true,
            _           => false,
        }
    }

    let position = i.position();

    let (remaining, numerical) = recognize(tuple((
        opt(recognize(one_of("+-"))),
        digit1,
        opt(recognize(tuple((tag("."), not(char('.')), digit0)))), // not(.) to avoid matching ranges
        opt(recognize(tuple((one_of("iuf"), alt((tag("8"), tag("16"), tag("32"), tag("64")))))))
    ))).parse(i.clone())?;

    let (value, type_name) = splits_numerical_suffix(*numerical);

    if value.contains(".") || type_name == Some("f32") || type_name == Some("f64") {
        if let Ok(float) = str::parse::<f64>(value) {
            return Ok((remaining, Literal {
                position    : position,
                value       : LiteralValue::Numeric(Numeric::Float(float)),
                type_name   : type_name.map(|ty| TypeName::from_str(ty, position)), // TODO: this is the position of the number, not just the type suffix
                type_id     : None,
            }));
        }
    } else if value.starts_with("-") || type_name == Some("i8") || type_name == Some("i16") || type_name == Some("i32") || type_name == Some("i64") {
        if let Ok(integer) = str::parse::<i64>(value) {
            if check_signed_range(integer, type_name) {
                return Ok((remaining, Literal {
                    position    : position,
                    value       : LiteralValue::Numeric(Numeric::Signed(integer)),
                    type_name   : type_name.map(|ty| TypeName::from_str(ty, position)), // TODO: this is the position of the number, not just the type suffix
                    type_id     : None,
                }));
            }
        }
    } else {
        if let Ok(integer) = str::parse::<u64>(value) {
            if check_unsigned_range(integer, type_name) {
                return Ok((remaining, Literal {
                    position    : position,
                    value       : LiteralValue::Numeric(Numeric::Unsigned(integer)),
                    type_name   : type_name.map(|ty| TypeName::from_str(ty, position)), // TODO: this is the position of the number, not just the type suffix
                    type_id     : None,
                }));
            }
        }
    }

    Err(nom::Err::Failure(Error { input: i, kind: ParseErrorKind::InvalidNumerical }))
}

// literal boolean (true, false)

fn boolean(i: Input<'_>) -> Output<Literal> {
    let position = i.position();
    map(alt((tag("true"), tag("false"))), move |m: Input<'_>| {
        Literal {
            position    : position,
            value       : LiteralValue::Bool(*m == "true"),
            type_name   : None,
            type_id     : None,
        }
    })(i)
}

// literal string ("hello world")

fn string(i: Input<'_>) -> Output<Literal> {
    let position = i.position();
    alt((
        // empty string (TODO this should be possible without a second mapping)
        map(tag("\"\""), move |_: Input<'_>| {
            Literal {
                position    : position,
                value       : LiteralValue::String("".to_string()),
                type_name   : None,
                type_id     : None,
            }
        }),
        // non-empty string
        map(delimited(char('"'), escaped(none_of("\\\""), '\\', one_of("\"n\\")), char('"')), move |m: Input<'_>| {
            Literal {
                position    : position,
                value       : LiteralValue::String(m.to_string()),
                type_name   : None,
                type_id     : None,
            }
        })
    ))(i)
}

// literal array ([ 1, 2, 3 ])

fn array_literal(i: Input<'_>) -> Output<Literal> {
    let position = i.position();
    map(
        tuple((ws(char('[')), separated_list0(ws(char(',')), expression), opt(ws(char(','))), ws(char(']')))),
        move |m| Literal {
            position    : position,
            value: LiteralValue::Array(ArrayLiteral {
                elements: m.1,
            }),
            type_name   : None,
            type_id     : None,
        }
    )(i)
}

// literal struct (MyStruct { a: 1 })

fn struct_literal(i: Input<'_>) -> Output<Literal> {
    fn field(i: Input<'_>) -> Output<(String, Expression)> {
        map(
            tuple((ws(label), ws(char(':')), expression)),
            |tuple| (tuple.0.to_string(), tuple.2)
        )(i)
    }
    fn fields(i: Input<'_>) -> Output<UnorderedMap<String, Expression>> {
        map(
            separated_list0(char(','), field),
            |list| {
                list.into_iter().map(|item| (item.0, item.1)).collect()
            }
        )(i)
    }
    let position = i.position();
    map(
        tuple((ws(path), ws(char('{')), fields, opt(ws(char(','))), ws(char('}')))),
        move |m| Literal {
            position    : position,
            value: LiteralValue::Struct(StructLiteral {
                fields: m.2,
            }),
            type_name   : Some(TypeName::from_path(m.0)),
            type_id     : None,
        }
    )(i)
}

// literal simple enum variant (MyEnum::MyVariant)

fn variant_literal(i: Input<'_>) -> Output<Literal> {
    let position = i.position();
    map(
        tuple((ident, ws(tag("::")), path)), // TODO: this ensures a path of at least two elements to avoid conflicts with idents but makes it impossible to `use num::Variant`. ideally the resolver would handle determining whether something is an variable or enum variant constructor
        move |(ident, _, mut path)| {
            path.unshift(ident); // prepend path with ident
            Literal {
                position    : position,
                value       : LiteralValue::Variant(VariantLiteral { ident: path.pop(), path: path }),
                type_name   : None, //Some(TypeName::from_path(m)),
                type_id     : None,
            }
        }
    )(i)
}

// literal

fn literal(i: Input<'_>) -> Output<Literal> {
    ws(alt((boolean, string, array_literal, struct_literal, numerical)))(i)
}

// assignment

fn assignable(i: Input<'_>) -> Output<Expression> {
    let var_position = i.position();
    let init = map(ident, |m| Expression::Variable(Variable { position: var_position as Position, ident: m, binding_id: None }))(i)?;
    let op_position = init.0.position();
    fold_many0(
        alt((
            map(delimited(ws(tag("[")), expression, tag("]")), |e| (BinaryOperator::IndexWrite, e)),
            map(preceded(ws(tag(".")), ident), |i| (BinaryOperator::AccessWrite, Expression::Member(Member { position: op_position as Position, ident: i, type_id: None })))
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
            Expression::BinaryOp(Box::new(BinaryOp { position: op_position as Position, op: op, left: acc, right: val, type_id: None }))
        }
    )(init.0)
}

fn assignment(i: Input<'_>) -> Output<Assignment> {
    fn assignment_operator(i: Input<'_>) -> Output<BinaryOperator> {
        ws(map(
            alt((tag("="), tag("+="), tag("-="), tag("*="), tag("/="), tag("%="))),
            |o: Input<'_>| {
                BinaryOperator::from_string(*o)
            }
        ))(i)
    }
    let position = i.position();
    ws(map(
        tuple((assignable, assignment_operator, expression)),
        move |m| {
            Assignment {
                position: position,
                op      : m.1,
                left    : m.0,
                right   : m.2,
                type_id : None,
            }
        }
    ))(i)
}

// call

fn call_argument_list(i: Input<'_>) -> Output<Vec<Expression>> {
    delimited(ws(char('(')), separated_list0(ws(char(',')), expression), ws(char(')')))(i)
}

fn call_ident(i: Input<'_>) -> Output<Call> {
    let position = i.position();
    map(
        tuple((ident, space0, call_argument_list)),
        move |m| Call {
            position    : position,
            ident       : m.0,
            args        : m.2,
            call_syntax : CallSyntax::Ident,
            function_id : None,
            type_id     : None,
        }
    )(i)
}

fn call_path(i: Input<'_>) -> Output<Call> {
    let position = i.position();
    map(
        tuple((path, space0, call_argument_list)),
        move |mut m| Call {
            position    : position,
            ident       : m.0.pop(),
            args        : m.2,
            call_syntax : CallSyntax::Path(m.0),
            function_id : None,
            type_id     : None,
        }
    )(i)
}

// block

fn block(i: Input<'_>) -> Output<Block> {
    let position = i.position();
    ws(map(
        delimited(
            ws(char('{')),
            pair(many0(statement), opt(expression)),
            ws(char('}'))
        ),
        move |mut m| {
            // move last block item into result if it is an expression and no result was matched
            if m.1.is_none() && m.0.last().map_or(false, |l| l.is_expression()) {
                m.1 = m.0.pop().map(|s| s.into_expression().unwrap());
            }
            Block {
                position        : position,
                statements      : m.0,
                result          : m.1,
                returns         : None,
                scope_id        : None,
            }
        }
    ))(i)
}

// if

fn if_block(i: Input<'_>) -> Output<IfBlock> {
    fn else_block(i: Input<'_>) -> Output<Block> {
        let position = i.position();
        ws(preceded(
            tag("else"),
            alt((
                sepl(map(if_block, move |m| Block {
                    position        : position,
                    statements      : Vec::new(),
                    result          : Some(Expression::IfBlock(Box::new(m))),
                    returns         : None,
                    scope_id        : None,
                })),
                block
            ))
        ))(i)
    }
    let position = i.position();
    ws(preceded(
        check_state(sepr(tag("if")), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalIfBlock) }),
        map(
            tuple((expression, block, opt(else_block))),
            move |m| IfBlock {
                    position    : position,
                    cond        : m.0,
                    if_block    : m.1,
                    else_block  : m.2,
                    scope_id    : None,
                }
            )
    ))(i)
}

// match

fn match_block(i: Input<'_>) -> Output<MatchBlock> {
    fn match_pattern(i: Input<'_>) -> Output<Pattern> {
        //let position = i.position();
        ws(
            //alt((
            map(variant_literal, |v| Pattern::SimpleVariant(v)),
            //))
        )(i)
    }
    fn match_case(i: Input<'_>) -> Output<Block> {
        let position = i.position();
        ws(alt((
            block,
            map(expression, move |e| Block {
                position        : position,
                statements      : Vec::new(),
                result          : Some(e),
                returns         : None,
                scope_id        : None,
            }),
        )))(i)
    }
    fn match_list(i: Input<'_>) -> Output<Vec<(Pattern, Block)>> {
        ws(separated_list1(
            char(','),
            pair(match_pattern, preceded(tag("=>"), match_case))
        ))(i)
    }
    let position = i.position();
    ws(preceded(
        check_state(sepr(tag("match")), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalIfBlock) }),
        map(
            pair(expression, delimited(char('{'), match_list, preceded(opt(char(',')), ws(char('}'))))),
            move |m| MatchBlock {
                    position    : position,
                    expr        : m.0,
                    branches    : m.1,
                    scope_id    : None,
                }
            )
    ))(i)
}

// expression

fn expression(i: Input<'_>) -> Output<Expression> {
    fn parens(i: Input<'_>) -> Output<Expression> {
        ws(delimited(char('('), expression, char(')')))(i)
    }
    fn prefix(i: Input<'_>) -> Output<UnaryOp> {
        let position = i.position();
        map(
            pair(ws(alt((tag("++"), tag("--")))), ws(assignable)),
            move |m| UnaryOp {
                position: position,
                op      : UnaryOperator::prefix_from_string(*m.0),
                expr    : m.1,
                type_id : None,
            }
        )(i)
    }
    fn suffix(i: Input<'_>) -> Output<UnaryOp> {
        let position = i.position();
        map(
            pair(ws(assignable), ws(alt((tag("++"), tag("--"))))),
            move |m| UnaryOp {
                position: position,
                op      : UnaryOperator::suffix_from_string(*m.1),
                expr    : m.0,
                type_id : None,
            }
        )(i)
    }
    fn operand(i: Input<'_>) -> Output<Expression> {
        let position = i.position();
        ws(alt((
            map(literal, |m| Expression::Literal(m)),
            map(if_block, |m| Expression::IfBlock(Box::new(m))),
            map(match_block, |m| Expression::MatchBlock(Box::new(m))),
            map(block, |m| Expression::Block(Box::new(m))),
            parens,
            map(suffix, |m| Expression::UnaryOp(Box::new(m))),
            map(prefix, |m| Expression::UnaryOp(Box::new(m))),
            map(call_ident, |m| Expression::Call(m)),
            map(call_path, |m| Expression::Call(m)),
            map(variant_literal, |m| Expression::Literal(m)),
            map(ident, move |m| Expression::Variable(Variable { position: position, ident: m, binding_id: None }))
        )))(i)
    }
    fn unary(i: Input<'_>) -> Output<Expression> {
        let position = i.position();
        map(
            preceded(ws(tag("!")), ws(prec6)),
            move |m| {
                Expression::UnaryOp(Box::new(UnaryOp {
                    position: position,
                    op      : UnaryOperator::Not,
                    expr    : m,
                    type_id : None,
                }))
            }
        )(i)
    }
    fn prec7(i: Input<'_>) -> Output<Expression> {
        let init = operand(i.clone())?;
        let position = i.position();
        fold_many0(
            alt((
                map(delimited(ws(tag("[")), expression, ws(tag("]"))), |e| (BinaryOperator::Index, e)),
                map(preceded(ws(tag(".")), ws(call_ident)), |i| (BinaryOperator::Access, Expression::Call(i))),
                map(preceded(ws(tag(".")), ws(ident)), |i| (BinaryOperator::Access, Expression::Member(Member { position: position, ident: i, type_id: None })))
            )),
            init.1,
            move |acc, (op, mut val)| match &mut val {
                Expression::Call(call) if op == BinaryOperator::Access => {
                    // prepend object argument
                    call.args.insert(0, acc);
                    call.call_syntax = CallSyntax::Method;
                    val
                },
                _ => Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, type_id: None }))
            }
        )(init.0)
    }
    fn prec6(i: Input<'_>) -> Output<Expression> {
        let position = i.position();
        ws(map(
            pair(
                alt((prec7, unary)), opt(preceded(ws(sepr(tag("as"))), path))
            ),
            move |(expr, path)| {
                if let Some(path) = path {
                    Expression::Cast(Box::new(Cast {
                        position: position,
                        expr    : expr,
                        ty      : TypeName::from_path(path),
                        type_id : None,
                    }))
                } else {
                    expr
                }
            }
        ))(i)
    }
    fn prec5(i: Input<'_>) -> Output<Expression> {
        let init = prec6(i)?;
        let position = init.0.position();
        fold_many0(
            pair(map(alt((tag("*"), tag("/"), tag("%"))), |o: Input<'_>| BinaryOperator::from_string(*o)), prec6),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, type_id: None }))
        )(init.0)
    }
    fn prec4(i: Input<'_>) -> Output<Expression> {
        let init = prec5(i)?;
        let position = init.0.position();
        fold_many0(
            pair(map(alt((tag("+"), tag("-"))), |o: Input<'_>| BinaryOperator::from_string(*o)), prec5),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, type_id: None }))
        )(init.0)
    }
    fn prec3(i: Input<'_>) -> Output<Expression> {
        let init = prec4(i)?;
        let position = init.0.position();
        fold_many0(
            pair(map(alt((tag("<="), tag(">="), tag("<"), tag(">"))), |o: Input<'_>| BinaryOperator::from_string(*o)), prec4),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, type_id: None }))
        )(init.0)
    }
    fn prec2(i: Input<'_>) -> Output<Expression> {
        let init = prec3(i)?;
        let position = init.0.position();
        fold_many0(
            pair(map(alt((tag("!="), tag("=="))), |o: Input<'_>| BinaryOperator::from_string(*o)), prec3),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, type_id: None }))
        )(init.0)
    }
    fn prec1(i: Input<'_>) -> Output<Expression> {
        let init = prec2(i)?;
        let position = init.0.position();
        fold_many0(
            pair(map(tag("&&"), |o: Input<'_>| BinaryOperator::from_string(*o)), prec2),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, type_id: None }))
        )(init.0)
    }
    fn prec0(i: Input<'_>) -> Output<Expression> {
        let init = prec1(i)?;
        let position = init.0.position();
        fold_many0(
            pair(map(tag("||"), |o: Input<'_>| BinaryOperator::from_string(*o)), prec1),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: acc, right: val, type_id: None }))
        )(init.0)
    }
    ws(alt((
        map(assignment, |m| Expression::Assignment(Box::new(m))),
        prec0
    )))(i)
}

// module

fn module(i: Input<'_>) -> Output<Module> {
    let position = i.position();
    ws(map(
        preceded(
            check_state(sepr(tag("mod")), |s| if s.in_function { Some(ParseErrorKind::IllegalModuleDef) } else { None }),
            terminated(ident, char(';'))
        ),
        move |ident| Module {
            position: position,
            ident   : ident,
        }
    ))(i)
}

// use (TODO: capture paths as AST so that we can report exact error positions)

fn use_item(i: Input<'_>) -> Output<Vec<(String, (String, bool))>> {
    ws(alt((
        map(pair(path, delimited(pair(ws(tag("::")), ws(char('{'))), separated_list1(char(','), ws(use_item)), char('}') )), |(path, list)| {
            let mut flattened = Vec::new();
            let parent = parts_to_path(&path.name);
            for elements in list  {
                for (ident, (path, _)) in elements {
                    flattened.push((ident, (parent.clone() + "::" + &path, false)));
                }
            }
            flattened
        }),
        map(pair(path, preceded(sepl(tag("as")), sepl(ident))), |(path, ident)| {
            vec![(ident.name, (parts_to_path(&path.name), false))]
        }),
        map(path, |path| {
            vec![(path.name.last().unwrap().name.clone(), (parts_to_path(&path.name), false))]
        }),
    )))(i)
}

fn use_declaration(i: Input<'_>) -> Output<Use> {
    let position = i.position();
    ws(map(
        delimited(
            check_state(sepr(tag("use")), |s| if s.in_function { Some(ParseErrorKind::IllegalModuleDef) } else { None }), // TODO: allow in function scope
            use_item,
            char(';')
        ),
        move |items| {
            Use {
                position,
                mapping: items.into_iter().collect(),
            }
        }
    ))(i)
}

// let

fn binding(i: Input<'_>) -> Output<Binding> {
    let position = i.position();
    ws(map(
        preceded(
            check_state(sepr(tag("let")), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalLetStatement) }),
            tuple((opt(sepr(tag("mut"))), ident, opt(preceded(ws(char(':')), inline_type)), opt(preceded(ws(char('=')), expression)), ws(char(';'))))
        ),
        move |m| Binding {
            position    : position,
            ident       : m.1,
            mutable     : m.0.is_some(),
            expr        : m.3,
            ty          : m.2,
            binding_id  : None,
        }
    ))(i)
}

// inline type

fn inline_type(i: Input<'_>) -> Output<InlineType> {
    ws(alt((
        map(path, |t| InlineType::TypeName(TypeName::from_path(t))),
        map(array, |a| InlineType::Array(Box::new(a)))
    )))(i)
}

// enum definition

fn enum_def(i: Input<'_>) -> Output<EnumDef> {
    fn variant(i: Input<'_>) -> Output<VariantDef> {
        let position = i.position();
        map(
            pair(
            ws(ident),
            alt((
                map(
                    delimited(ws(char('(')), separated_list0(ws(char(',')), ws(inline_type)), preceded(opt(ws(char(','))), ws(char(')')))),
                    move |fields| VariantKind::Data(None, fields)
                ),
                map(
                    opt(preceded(ws(char('=')), numerical)),
                    move |variant_value| VariantKind::Simple(variant_value)
                ),
            ))),
            move |(ident, kind)| VariantDef {
                position,
                ident,
                kind,
            }
        )(i)
    }
    let position = i.position();
    let the_cloned_clone = i.clone();
    ws(map_res(
        pair(
            terminated(opt(sepr(tag("pub"))), check_state(sepr(tag("enum")), |s| if s.in_function { Some(ParseErrorKind::IllegalEnumDef) } else { None })),
            tuple((ident, ws(char('{')), separated_list1(ws(char(',')), variant), opt(ws(char(','))), ws(char('}'))))
        ),
        move |pair| {
            let mut have_data = false;
            let mut have_value = false;
            for variant in &pair.1.2 {
                match &variant.kind {
                    VariantKind::Data(_, _) => have_data = true,
                    VariantKind::Simple(d) if d.is_some() => have_value = true,
                    _ => {}
                }
            }
            if have_data && have_value {
                Err(Error { input: the_cloned_clone.clone(), kind: ParseErrorKind::IllegalEnumDef })
            } else {
                Ok(EnumDef {
                    position: position,
                    ident   : pair.1.0,
                    variants: pair.1.2,
                    type_id : None,
                    scope_id: None,
                    vis     : if pair.0.is_some() { Visibility::Public } else { Visibility::Private },
                })
            }
        }
    ))(i)
}


// struct definition

fn struct_def(i: Input<'_>) -> Output<StructDef> {
    fn field(i: Input<'_>) -> Output<(String, InlineType)> {
        map(
            tuple((ws(label), ws(char(':')), ws(inline_type))),
            |tuple| (tuple.0.to_string(), tuple.2)
        )(i)
    }
    fn fields(i: Input<'_>) -> Output<Vec<(String, InlineType)>> {
        map(
            separated_list1(ws(char(',')), field),
            |list| {
                list.into_iter().map(|item| (item.0, item.1)).collect()
            }
        )(i)
    }
    let position = i.position();
    ws(map(
        pair(
            terminated(opt(sepr(tag("pub"))), check_state(sepr(tag("struct")), |s| if s.in_function { Some(ParseErrorKind::IllegalStructDef) } else { None })),
            tuple((ident, ws(char('{')), fields, opt(ws(char(','))), ws(char('}'))))
        ),
        move |pair| StructDef {
            position: position,
            ident   : pair.1.0,
            fields  : pair.1.2,
            type_id : None,
            vis     : if pair.0.is_some() { Visibility::Public } else { Visibility::Private },
        }
    ))(i)
}

// impl block

fn impl_block(i: Input<'_>) -> Output<ImplBlock> {
    let position = i.position();
    ws(map(
        preceded(
            check_state(sepr(tag("impl")), |s| if s.in_function { Some(ParseErrorKind::IllegalImplBlock) } else { None }),
            pair(inline_type, delimited(ws(char('{')), many0(function), ws(char('}'))))
        ),
        move |tuple| ImplBlock {
            position    : position,
            functions   : tuple.1,
            scope_id    : None,
            ty          : tuple.0,
            trt         : None,
        }
    ))(i)
}

// trait definition

fn trait_def(i: Input<'_>) -> Output<TraitDef> {
    let position = i.position();
    ws(map(
        pair(
            terminated(opt(sepr(tag("pub"))), check_state(sepr(tag("trait")), |s| if s.in_function { Some(ParseErrorKind::IllegalTraitDef) } else { None })),
            tuple((ident, ws(char('{')), many0(function), ws(char('}'))))
        ),
        move |pair| TraitDef {
            position,
            functions   : pair.1.2,
            scope_id    : None,
            ident       : pair.1.0,
            type_id     : None,
            vis         : if pair.0.is_some() { Visibility::Public } else { Visibility::Private },
        }
    ))(i)
}

// trait implementation

fn trait_impl_block(i: Input<'_>) -> Output<ImplBlock> {
    let position = i.position();
    ws(map(
        preceded(
            check_state(sepr(tag("impl")), |s| if s.in_function { Some(ParseErrorKind::IllegalImplBlock) } else { None }),
            pair(
                pair(terminated(path, sepl(tag("for"))), sepl(inline_type)),
                delimited(ws(char('{')), many0(function), ws(char('}')))
            )
        ),
        move |tuple| ImplBlock {
            position    : position,
            functions   : tuple.1,
            scope_id    : None,
            ty          : tuple.0.1,
            trt         : Some(TypeName::from_path(tuple.0.0)),
        }
    ))(i)
}

// array definition

fn array(i: Input<'_>) -> Output<Array> {
    let position = i.position();
    ws(map(
        delimited(ws(char('[')), inline_type, ws(char(']'))),
        move |ty| Array {
            position    : position,
            element_type: ty,
            type_id     : None,
        }
    ))(i)
}

// function definition

fn function(i: Input<'_>) -> Output<Function> {
    fn function_signature(i: Input<'_>) -> Output<Signature> {
        fn argument(i: Input<'_>) -> Output<Binding> {
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
        fn argument_list(i: Input<'_>) -> Output<Vec<Binding>> {
            delimited(ws(char('(')), separated_list0(ws(char(',')), ws(argument)), ws(char(')')))(i)
        }
        fn return_part(i: Input<'_>) -> Output<InlineType> {
            preceded(ws(tag("->")), inline_type)(i)
        }
        ws(map(
            pair(
                terminated(opt(sepr(tag("pub"))), check_state(sepr(tag("fn")), |s| if s.in_function { Some(ParseErrorKind::IllegalFunction) } else { None })),
                tuple((ident, ws(argument_list), opt(ws(return_part))))
            ),
            |sig| Signature {
                ident   : sig.1.0,
                args    : sig.1.1,
                ret     : if let Some(sig_ty) = sig.1.2 { Some(sig_ty) } else { None },
                vis     : if sig.0.is_some() { Visibility::Public } else { Visibility::Private },
            },
        ))(i)
    }
    let position = i.position();
    ws(map(
        tuple((function_signature, with_state(&|state: &mut ParserState| state.in_function = true, alt((
            map(block, |b| Some(b)),
            map(ws(char(';')), |_| None)
        ))))),
        move |func| Function {
            position    : position,
            sig         : func.0,
            block       : func.1,
            function_id : None,
            scope_id    : None,
        }
    ))(i.clone())
}

// for

fn for_loop(i: Input<'_>) -> Output<ForLoop> {
    fn loop_range(i: Input<'_>) -> Output<Expression> {
        let position = i.position();
        map(
            tuple((ws(expression), ws(alt((tag("..="), tag("..")))), ws(expression))),
            move |m| Expression::BinaryOp(Box::new(BinaryOp {
                position    : position,
                op          : BinaryOperator::from_string(*m.1),
                left        : m.0,
                right       : m.2,
                type_id     : None
            }))
        )(i)
    }
    let position = i.position();
    ws(map(
        preceded(
            check_state(tag("for"), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalForLoop) }),
            tuple((sepl(ident), sepl(tag("in")), sepl(alt((loop_range, expression))), block))
        ),
        move |m| ForLoop {
            position: position,
            iter: Binding {
                position    : position,
                ident       : m.0,
                mutable     : true,
                expr        : None,
                ty          : None,
                binding_id  : None,
            },
            expr   : m.2,
            block   : m.3,
            scope_id: None,
        }
    ))(i)
}

// while loop

fn while_loop(i: Input<'_>) -> Output<WhileLoop> {
    let position = i.position();
    ws(map(
        preceded(
            check_state(tag("while"), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalWhileLoop) }),
            pair(sepl(expression), block)
        ),
        move |m| WhileLoop {
            position: position,
            expr    : m.0,
            block   : m.1,
            scope_id: None,
        }
    ))(i)
}

// return

fn return_statement(i: Input<'_>) -> Output<Return> {
    let position = i.position();
    map(
        preceded(
            check_state(tag("return"), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalReturn) }),
            terminated(opt(sepl(expression)), ws(char(';')))
        ),
        move |m| Return {
            position    : position,
            expr        : m,
        }
    )(i)
}

// statement

fn statement(i: Input<'_>) -> Output<Statement> {
    ws(alt((
        map(use_declaration, |m| Statement::Use(m)),
        map(module, |m| Statement::Module(m)),
        map(function, |m| Statement::Function(m)),
        map(struct_def, |m| Statement::StructDef(m)),
        map(enum_def, |m| Statement::EnumDef(m)),
        map(trait_impl_block,|m| Statement::ImplBlock(m)),
        map(impl_block,|m| Statement::ImplBlock(m)),
        map(trait_def, |m| Statement::TraitDef(m)),

        map(binding,|m| Statement::Binding(m)),
        map(if_block, |m| Statement::IfBlock(m)),
        map(for_loop, |m| Statement::ForLoop(m)),
        map(while_loop, |m| Statement::WhileLoop(m)),
        map(return_statement, |m| Statement::Return(m)),
        map(block, |m| Statement::Block(m)),
        map(terminated(expression, char(';')), |m| Statement::Expression(m)),
    )))(i)
}

// root

fn root_items(i: Input<'_>) -> Output<Statement> {
    ws(alt((
        map(use_declaration, |m| Statement::Use(m)),
        map(module, |m| Statement::Module(m)),
        map(function, |m| Statement::Function(m)),
        map(struct_def, |m| Statement::StructDef(m)),
        map(enum_def, |m| Statement::EnumDef(m)),
        map(trait_impl_block,|m| Statement::ImplBlock(m)),
        map(impl_block,|m| Statement::ImplBlock(m)),
        map(trait_def, |m| Statement::TraitDef(m)),
    )))(i)
}

fn root(i: Input<'_>) -> Output<Vec<Statement>> {
    all_consuming(ws(many0(root_items)))(i)
}

/// Parses Itsy source code into a program AST structure.
pub fn parse_module(src: &str, module_path: &str) -> ParseResult<ParsedModule> {  // also return list of modules used by this source, rename ParsedProgram to ParsedModule or similar
    let input = Input::new(src);
    let result = root(input.clone());
    match result {
        Ok(result) => {
            Ok(ParsedModule { path: module_path.to_string(), ast: result.1, scope_id: None })
        },
        Err(err) => {
            match err {
                nom::Err::Incomplete(_) => unreachable!("No parser should return Incomplete"),
                nom::Err::Failure(failure) => {
                    // a failure generated by checkpoint
                    Err(ParseError::new(failure.kind, failure.input.position(), module_path))
                }
                nom::Err::Error(_) => {
                    // nom error is useless to us, but we stored the highest parsed offset on the input which is the most likely error position
                    let error = input.max_parsed();
                    Err(ParseError::new(ParseErrorKind::SyntaxError, Position(error.1), module_path))
                }
            }
        }
    }
}

/// Parses Itsy source files into a program AST structure. The loader receives the fully qualified module name and is
/// expected to return the parsed module.
///
/// The following example parses a module with its dependencies.
/// ```
/// use itsy::parser;
/// use std::fs;
///
/// fn main() {
///     let source_file = "itsy/examples/parse.itsy";
///     let parsed = parser::parse(|module_path| {
///         let filename = parser::module_filename(source_file, module_path);
///         let file = fs::read_to_string(filename).unwrap();
///         parser::parse_module(&file, module_path)
///     }).unwrap();
/// }
/// ```
///
/// The returned [ParsedProgram] is now ready for type resolution by [resolve](crate::resolver::resolve).
pub fn parse(mut loader: impl FnMut(&str) -> ParseResult<ParsedModule>) -> ParseResult<ParsedProgram> {
    let mut program = ParsedProgram::new();
    parse_recurse("", &mut program, &mut loader)?;
    Ok(program)
}

/// Recursively parse all submodules.
fn parse_recurse(module_path: &str, program: &mut ParsedProgram, loader: &mut impl FnMut(&str) -> ParseResult<ParsedModule>) -> ParseResult {
    let module = loader(module_path)?;
    for submodule_ast in module.modules() {
        let submodule_path = if module_path != "" { module_path.to_string() + "::" + submodule_ast.name() } else { submodule_ast.name().to_string() } ;
        parse_recurse(&submodule_path, program, loader)?;
    }
    program.add_module(module);
    Ok(())
}

/// Given the filename to an itsy program main module and an Itsy module path, returns the filename of the Itsy module.
pub fn module_filename<P: AsRef<std::path::Path>>(main_file: P, module_path: &str) -> std::path::PathBuf {
    if module_path == "" {
        main_file.as_ref().to_path_buf()
    } else {
        let mut path = main_file.as_ref().parent().expect("Invalid filename").to_path_buf();
        for module_name in path_to_parts(module_path) {
            path.push(module_name);
        }
        path.set_extension("itsy");
        path
    }
}