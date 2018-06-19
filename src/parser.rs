use nom::*;
use nom::types::CompleteStr as Input;
use std::collections::HashMap;
use ast::*;

// identifier [a-z_][a-z0-9_]*

named!(ident<Input, Input>, recognize!(tuple!(
    take_while1!(|m| is_alphabetic(m as u8) || m == '_'),
    take_while!(|m| is_alphanumeric(m as u8) || m == '_')
)));

// type name

named!(ty<Input, Type>, map!(ident, |ident| Type::from_string(*ident)));

// block

named!(block_items<Input, Vec<Statement>>, ws!(many0!(statement)));

named!(block<Input, Block>, map!(ws!(tuple!(char!('{'), block_items, opt!(expression), char!('}'))), |m| Block {
    statements: m.1,
    result: m.2,
}));

// expression

named!(parens<Input, Expression>, ws!(delimited!(char!('('), expression, char!(')'))));

named!(ident_path<Input, Expression>, map!(ws!(separated_nonempty_list!(char!('.'), ident)), |m| Expression::IdentPath(IdentPath {
    segs: m.into_iter().map(|s| *s).collect()
})));

named!(call_argument_list<Input, Vec<Expression>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), expression), char!(')'))));

named!(call<Input, Expression>, map!(ws!(tuple!(ident_path, call_argument_list)), |m| Expression::Call(Call {
    path: if let Expression::IdentPath(p) = m.0 { p } else { unreachable!() },
    args: m.1,
})));

named!(float<Input, Expression>, map!(recognize!(recognize_float), |m| Expression::Float(*m)));

named!(integer<Input, Expression>, map!(recognize!(tuple!(digit1, not!(char!('.')))), |m| Expression::Integer(*m)));

named!(operand<Input, Expression>, ws!(alt!(call | ident_path | parens | integer | float)));

named!(prec2<Input, Expression>, ws!(do_parse!(
    init: operand >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("*") | tag!("/") | tag!("%")), |o| Operator::from_string(*o)), operand),
        init,
        |acc, (op, val)| {
            Expression::Operation(Box::new(Operation::Binary(op, acc, val)))
        }
    ) >>
    (res)
)));

named!(prec1<Input, Expression>, ws!(do_parse!(
    init: prec2 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("+") | tag!("-")), |o| Operator::from_string(*o)), prec2),
        init,
        |acc, (op, val)| {
            Expression::Operation(Box::new(Operation::Binary(op, acc, val)))
        }
    ) >>
    (res)
)));

named!(prec0<Input, Expression>, ws!(do_parse!(
    init: prec1 >>
    res: fold_many0!(
        pair!(map!(alt!(tag!("=") | tag!("+=") | tag!("-=") | tag!("*=") | tag!("/=")| tag!("%=")), |o| Operator::from_string(*o)), prec1),
        init,
        |acc, (op, val)| {
            Expression::Operation(Box::new(Operation::Binary(op, acc, val)))
        }
    ) >>
    (res)
)));

named!(expression<Input, Expression>, ws!(alt!(
    map!(if_block, |m| Expression::IfBlock(Box::new(m)))
    |
    prec0
)));

// let

named!(binding<Input, Statement>, map!(preceded!(ws!(tag!("let")), return_error!(ErrorKind::Custom(1), ws!(tuple!(ident, char!('='), expression, char!(';'))))), |m| Statement::Binding(Binding {
    name: *m.0,
    expr: m.2,
})));

// structure

named!(structure_item<Input, (Input, Type)>, map!(ws!(tuple!(ident, char!(':'), ty)), |tuple| (tuple.0, tuple.2)));

named!(structure_items<Input, HashMap<&str, Type>>, map!(ws!(separated_list_complete!(char!(','), structure_item)), |list| {
    list.into_iter().map(|item| (*item.0, item.1)).collect()
}));

named!(structure<Input, Statement>, map!(ws!(tuple!(tag!("struct"), ident, char!('{'), structure_items, char!('}'))), |tuple| Statement::Structure(Structure {
    name: *tuple.1,
    items: tuple.3,
})));

// function

named!(signature_argument<Input, Argument>, map!(ws!(tuple!(opt!(tag!("mut")), ident, char!(':'), ty)), |tuple| Argument {
    name    : *tuple.1,
    mutable : tuple.0.is_some(),
    ty      : tuple.3,
}));

named!(signature_argument_list<Input, Vec<Argument>>, ws!(delimited!(char!('('), separated_list_complete!(char!(','), signature_argument), char!(')'))));

named!(signature_return_part<Input, Type>, ws!(preceded!(tag!("->"), ty)));

named!(signature<Input, Signature>, map!(ws!(tuple!(tag!("fn"), ident, signature_argument_list, opt!(signature_return_part))), |sig| Signature {
    name    : *sig.1,
    args    : sig.2,
    ret     : sig.3.unwrap_or(Type::void),
}));

named!(function<Input, Statement>, map!(ws!(tuple!(signature, block)), |func| Statement::Function(Function {
    sig: func.0,
    block: func.1,
})));

// if

named!(block_or_if<Input, Block>, ws!(alt!(
    map!(if_block, |m| Block { statements: Vec::new(), result: Some(Expression::IfBlock(Box::new(m))) })
    |
    block
)));

named!(if_block<Input, IfBlock>, map!(ws!(tuple!(tag!("if"), expression, block, opt!(preceded!(tag!("else"), block_or_if)))), |m| IfBlock {
    cond: m.1,
    if_block: m.2,
    else_block: m.3,
}));

// for

//named!(for_loop<Input, ForLoop>, ws!(tuple!(tag!("for"), )))

// statement

named!(ifblock_statement<Input, Statement>, map!(if_block, |m| Statement::IfBlock(m)));
named!(expression_statement<Input, Statement>, map!(terminated!(expression, char!(';')), |m| Statement::Expression(m)));
named!(statement<Input, Statement>, alt!(binding | ifblock_statement | function | structure | expression_statement));

// root

named!(pub parse<Input, Vec<Statement>>, ws!(many0!(statement)));