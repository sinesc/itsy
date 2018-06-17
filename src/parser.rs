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

named!(parens<Input, Expression>, ws!(delimited!(tag!("("), expression, tag!(")"))));

named!(ident_exp<Input, Expression>, map!(ident, |m| Expression::Ident(*m)));

named!(float<Input, Expression>, map!(recognize!(recognize_float), |m| Expression::Float(*m)));

named!(integer<Input, Expression>, map!(recognize!(tuple!(digit1, not!(char!('.')))), |m| Expression::Integer(*m)));

named!(operand<Input, Expression>, ws!(alt!(ident_exp | parens | integer | float)));

named!(term<Input, Expression>, ws!(do_parse!(
    init: operand >>
    res: fold_many0!(
        pair!(alt!(char!('*') | char!('/') | char!('%')), operand),
        init,
        |acc, (op, val)| {
            Expression::Operation(Operation::Binary(Operator::from_char(op).unwrap(), Box::new(acc), Box::new(val)))        //TODO:no unwrap
        }
    ) >>
    (res)
)));

named!(operation<Input, Expression>, ws!(do_parse!(
    init: term >>
    res: fold_many0!(
        pair!(alt!(char!('+') | char!('-')), term),
        init,
        |acc, (op, val)| {
            Expression::Operation(Operation::Binary(Operator::from_char(op).unwrap(), Box::new(acc), Box::new(val)))
        }
    ) >>
    (res)
)));

named!(expression<Input, Expression>, ws!(alt!(
    map!(if_block, |m| Expression::IfBlock(Box::new(m)))
    |
    operation
)));

// let

named!(binding<Input, Binding>, map!(ws!(tuple!(tag!("let"), ident, char!('='), expression)), |m| Binding {
    name: *m.1,
    expr: m.3,
}));

// structure

named!(structure_item<Input, (Input, Type)>, map!(ws!(tuple!(ident, char!(':'), ty)), |tuple| (tuple.0, tuple.2)));

named!(structure_items<Input, HashMap<&str, Type>>, map!(ws!(separated_list_complete!(char!(','), structure_item)), |list| list.into_iter().map(|item| (*item.0, item.1)).collect()));

named!(structure<Input, Structure>, map!(ws!(tuple!(tag!("struct"), ident, tag!("{"), structure_items, tag!("}"))), |tuple| Structure {
    name: *tuple.1,
    items: tuple.3,
}));

// function

named!(argument<Input, Argument>, map!(ws!(tuple!(opt!(tag!("mut")), ident, char!(':'), ty)), |tuple| Argument {
    name    : *tuple.1,
    mutable : tuple.0.is_some(),
    ty      : tuple.3,
}));

named!(argument_list<Input, Vec<Argument>>, ws!(delimited!(tag!("("), separated_list_complete!(char!(','), argument), tag!(")"))));

named!(return_part<Input, Type>, ws!(preceded!(tag!("->"), ty)));

named!(signature<Input, Signature>, map!(ws!(tuple!(tag!("fn"), ident, argument_list, opt!(return_part))), |sig| Signature {
    name    : *sig.1,
    args    : sig.2,
    ret     : sig.3.unwrap_or(Type::void),
}));

named!(function<Input, Function>, map!(ws!(tuple!(signature, block)), |func| Function {
    sig: func.0,
    block: func.1,
}));

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

// statement  TODO: avoid some of the *_statement mapper by directly returning a Statement from e.g. function. Won't do for expression.

named!(function_statement<Input, Statement>, map!(function, |m| Statement::Function(m)));
named!(structure_statement<Input, Statement>, map!(structure, |m| Statement::Structure(m)));
named!(ifblock_statement<Input, Statement>, map!(if_block, |m| Statement::IfBlock(m)));
named!(binding_statement<Input, Statement>, map!(terminated!(binding, char!(';')), |m| Statement::Binding(m)));
named!(pub statement<Input, Statement>, alt!(binding_statement | ifblock_statement | function_statement | structure_statement));