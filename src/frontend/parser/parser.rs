//! Nom parsers used to generate the Itsy AST.

pub mod error;
mod nomutil;
pub mod types;

use nom::character::{is_alphanumeric, is_alphabetic, complete::{one_of, digit0, char, digit1}};
use nom::bytes::complete::{take_while, take_while1, tag};
use nom::combinator::{recognize, opt, all_consuming, map, not, verify};
use nom::multi::{separated_list0, separated_list1, many0, fold_many0, fold_many1};
use nom::branch::alt;
use nom::sequence::{tuple, pair, delimited, preceded, terminated};
use crate::{prelude::*, internals::resolved::ids::BindingId};
use crate::shared::{numeric::Numeric, path_to_parts, parts_to_path};
use crate::frontend::ast::*;
use types::{Input, Output, Failure, ParserFlags, ParsedModule, ParsedProgram, ScopeBindingType, ScopeKind, VarDecl, LinkState};
use error::{ParseResult, ParseError, ParseErrorKind};
use nomutil::*;

/// List of keywords that are not legal identifier.
const KEYWORDS: &[ &'static str ] = &[
    "as",
    "break",
    "const",
    "continue",
    "enum",
    "false",
    "fn",
    "for",
    "if",
    "impl",
    "let",
    "match",
    "mod",
    "mut",
    "pub",
    "return",
    "struct",
    "trait",
    "true",
    "use",
    "while",
];

/// Runs the given parser. If the parser succeeds the given checker function is run. If it returns true, the parser result is returned, otherwise a failure.
fn check_flags<'a, O, P, C>(mut parser: P, checker: C) -> impl FnMut(Input<'a>) -> Output<'a, O>
where
    P: nom::Parser<Input<'a>, O, Failure<'a>>,
    C: Fn(ParserFlags) -> Option<ParseErrorKind>
{
    move |input: Input<'_>| {
        let before = input.clone();
        let inner_result = parser.parse(input)?;

        if let Some(kind) = checker(before.flags()) {
            Err(nom::Err::Failure(Failure { input: before, kind: kind }))
        } else {
            Ok(inner_result)
        }
    }
}

/// Calls the given function with a mutable reference to parser flags, then runs the given parser. Restores previous parser flags afterwards.
fn with_flags<'a, P: 'a, O: 'a>(s: &'a impl Fn(&mut ParserFlags), mut parser: P) -> impl FnMut(Input<'a>) -> Output<O> where P: FnMut(Input<'a>) -> Output<O> {
    move |input: Input<'_>| {
        use nom::Parser;
        let i = input.clone();
        let flags = i.flags();
        i.flags_mut(s);
        let inner_result = parser.parse(input);
        i.flags_mut(|s| *s = flags);
        inner_result
    }
}

/// Pushes a new scope to the scope stack before running the given parser. Pops scope afterwards.
fn with_scope<'a, P: 'a, O: 'a>(scope_type: ScopeKind, mut parser: P) -> impl FnMut(Input<'a>) -> Output<O> where P: FnMut(Input<'a>) -> Output<O> {
    move |input: Input<'_>| {
        use nom::Parser;
        let i = input.clone();
        let backup_state = i.push_scope(scope_type);
        let inner_result = parser.parse(input);
        i.pop_scope(if inner_result.is_ok() {
            None
        } else {
            Some(backup_state)
        });
        inner_result
    }
}

/// Snapshots binding id state, restores it if the inner parser fails.
fn snap<'a, P: 'a, O: 'a>(mut parser: P) -> impl FnMut(Input<'a>) -> Output<O> where P: FnMut(Input<'a>) -> Output<O> {
    move |input: Input<'_>| {
        use nom::Parser;
        let i = input.clone();
        let (next_binding_id, next_scope_id) = {
            let state = i.state.borrow();
            (state.link_state.next_binding_id, state.link_state.next_scope_id)
        };
        let inner_result = parser.parse(input);
        if inner_result.is_err() {
            let mut state = i.state.borrow_mut();
            if next_binding_id != state.link_state.next_binding_id {
                state.link_state.next_binding_id = next_binding_id;
                state.link_state.binding_ids.retain(|&binding_id| binding_id < next_binding_id);
                //state.scope_stack.last_mut().unwrap().have_bindings.retain(|_, &mut binding_id| binding_id < next_binding_id); // shouldn't be required
            }
            state.link_state.next_scope_id = next_scope_id;
        }
        inner_result
    }
}

/// Matches and returns a legal alphanumeric word and consumes optional trailing whitespace.
fn word(i: Input<'_>) -> Output<&str> {
    map(
        // ([a-z_][a-z0-9_]*)
    terminated(
            recognize(pair(
                take_while1(|m| is_alphabetic(m as u8) || m == '_'),
                take_while(|m| is_alphanumeric(m as u8) || m == '_')
            )),
            space0
        ),
        move |l: Input<'_>| *l
    )(i)
}

/// Returns a parser that matches and returns the given string and consumes optional trailing whitespace.
fn punct<'a>(p: &'static str) -> impl FnMut(Input<'a>) -> Output<&str> {
    map(terminated(tag(p), space0), |s: Input<'_>| *s)
}

// keywords, identifier and paths

/// Matches a valid identifier.
fn ident(i: Input<'_>) -> Output<Ident> {
    let position = i.position();
    map(
        verify(word, |input: &str| !KEYWORDS.contains(&input)),
        move |l| Ident {
            name: l.to_string(),
            position: position,
        }
    )(i)
}

/// Returns a parser that matches the given alphanumeric keyword. Requires the entire next alphanumeric token to match.
fn keyword<'a>(w: &'static str) -> impl FnMut(Input<'a>) -> Output<&str> {
    verify(word, move |input: &str| w == input)
}

/// Matches a variable name and declares the variable in the current scope.
fn var_decl(i: Input<'_>) -> Output<VarDecl> {
    let j = i.clone();
    map(
        ident,
        move |ident| {
            let binding_id = if j.flags().is_dead {
                BindingId::new(0)
            } else {
                j.add_binding(&ident.name)
            };
            VarDecl {
                binding_id,
                ident,
            }
        }
    )(i)
}

/// Matches a path.
fn path(i: Input<'_>) -> Output<Path> {
    let position = i.position();
    map(separated_list1(punct("::"), ident), move |name| Path { position, name })(i)
}

// module handling

fn module(i: Input<'_>) -> Output<Module> {
    let position = i.position();
    map(
        preceded(
            check_flags(keyword("mod"), |s| if s.in_function { Some(ParseErrorKind::IllegalModuleDef) } else { None }),
            terminated(ident, punct(";"))
        ),
        move |ident| Module {
            position: position,
            ident   : ident,
        }
    )(i)
}

fn use_decl(i: Input<'_>) -> Output<UseDecl> {
    fn use_item(i: Input<'_>) -> Output<Vec<(String, (String, bool))>> {
        alt((
            map(pair(path, delimited(pair(punct("::"), punct("{")), separated_list1(punct(","), use_item), punct("}") )), |(path, list)| {
                let mut flattened = Vec::new();
                let parent = parts_to_path(&path.name);
                for elements in list  {
                    for (ident, (path, _)) in elements {
                        flattened.push((ident, (parent.clone() + "::" + &path, false)));
                    }
                }
                flattened
            }),
            map(pair(path, preceded(keyword("as"), ident)), |(path, ident)| {
                vec![(ident.name, (parts_to_path(&path.name), false))]
            }),
            map(path, |path| {
                vec![(path.name.last().unwrap().name.clone(), (parts_to_path(&path.name), false))]
            }),
        ))(i)
    }
    let position = i.position();
    map(
        delimited(
            keyword("use"),
            use_item,
            punct(";")
        ),
        move |items| {
            UseDecl {
                position,
                mapping: items.into_iter().collect(),
            }
        }
    )(i)
}

// type definitions

fn inline_type(i: Input<'_>) -> Output<InlineType> {
    alt((
        map(path, |t| InlineType::TypeName(TypeName::from_path(t))),
        map(snap(callable_type), |f| InlineType::CallableDef(Box::new(f))),
        map(array_type, |a| InlineType::ArrayDef(Box::new(a))),
    ))(i)
}

fn callable_type(i: Input<'_>) -> Output<CallableDef> {
    fn type_list(i: Input<'_>) -> Output<Vec<InlineType>> {
        delimited(punct("("), separated_list0(punct(","), inline_type), punct(")"))(i)
    }
    fn return_part(i: Input<'_>) -> Output<InlineType> {
        preceded(punct("->"), inline_type)(i)
    }
    let position = i.position();
    map(
        tuple((keyword("fn"), type_list, opt(return_part))),
        move |sig| CallableDef {
            position,
            args    : sig.1,
            ret     : if let Some(sig_ty) = sig.2 { Some(sig_ty) } else { None },
            type_id : None,
        },
    )(i)
}

fn enum_type(i: Input<'_>) -> Output<EnumDef> {
    fn variant(i: Input<'_>) -> Output<VariantDef> {
        let position = i.position();
        map(
            pair(
            ident,
            alt((
                map(
                    delimited(punct("("), separated_list0(punct(","), inline_type), preceded(opt(punct(",")), punct(")"))),
                    move |fields| VariantKind::Data(None, fields)
                ),
                map(
                    opt(preceded(punct("="), numeric_literal)),
                    move |variant_value| VariantKind::Simple(None, variant_value)
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
    let j = i.clone();
    map_result(
        pair(
            terminated(opt(keyword("pub")), check_flags(keyword("enum"), |s| if s.in_function { Some(ParseErrorKind::IllegalEnumDef) } else { None })),
            tuple((ident, punct("{"), separated_list1(punct(","), variant), opt(punct(",")), punct("}")))
        ),
        move |pair| {
            let mut have_data = false;
            let mut have_value = false;
            for variant in &pair.1.2 {
                match &variant.kind {
                    VariantKind::Data(_, _) => have_data = true,
                    VariantKind::Simple(_, d) if d.is_some() => have_value = true,
                    _ => {}
                }
            }
            if have_data && have_value {
                Err(Failure { input: j.clone(), kind: ParseErrorKind::IllegalVariantMix })
            } else {
                Ok(EnumDef {
                    position: position,
                    ident   : pair.1.0,
                    variants: pair.1.2,
                    type_id : None,
                    scope_id: j.scope_id(),
                    vis     : if pair.0.is_some() { Visibility::Public } else { Visibility::Private },
                })
            }
        }
    )(i)
}

fn struct_type(i: Input<'_>) -> Output<StructDef> {
    fn field(i: Input<'_>) -> Output<(String, InlineType)> {
        map(
            tuple((word, punct(":"), inline_type)),
            |tuple| (tuple.0.to_string(), tuple.2)
        )(i)
    }
    fn fields(i: Input<'_>) -> Output<Vec<(String, InlineType)>> {
        map(
            separated_list1(punct(","), field),
            |list| {
                list.into_iter().map(|item| (item.0, item.1)).collect()
            }
        )(i)
    }
    let position = i.position();
    map(
        pair(
            terminated(opt(keyword("pub")), check_flags(keyword("struct"), |s| if s.in_function { Some(ParseErrorKind::IllegalStructDef) } else { None })),
            tuple((ident, punct("{"), fields, opt(punct(",")), punct("}")))
        ),
        move |pair| StructDef {
            position: position,
            ident   : pair.1.0,
            fields  : pair.1.2,
            type_id : None,
            vis     : if pair.0.is_some() { Visibility::Public } else { Visibility::Private },
        }
    )(i)
}

fn trait_type(i: Input<'_>) -> Output<TraitDef> {
    let position = i.position();
    let j = i.clone();
    with_scope(ScopeKind::Module, map(
        pair(
            terminated(opt(keyword("pub")), check_flags(keyword("trait"), |s| if s.in_function { Some(ParseErrorKind::IllegalTraitDef) } else { None })),
            tuple((ident, punct("{"), with_flags(&|flags| flags.in_trait = true, many0(function)), punct("}")))
        ),
        move |pair| TraitDef {
            position,
            functions   : pair.1.2,
            scope_id    : j.scope_id(),
            ident       : pair.1.0,
            type_id     : None,
            vis         : if pair.0.is_some() { Visibility::Public } else { Visibility::Private },
        }
    ))(i)
}

fn array_type(i: Input<'_>) -> Output<ArrayDef> {
    let position = i.position();
    map(
        delimited(punct("["), inline_type, punct("]")),
        move |ty| ArrayDef {
            position    : position,
            element_type: ty,
            type_id     : None,
        }
    )(i)
}

// type implementations

fn impl_block(i: Input<'_>) -> Output<ImplBlock> {
    let position = i.position();
    let j = i.clone();
    with_scope(ScopeKind::Module, map(
        preceded(
            check_flags(keyword("impl"), |s| if s.in_function { Some(ParseErrorKind::IllegalImplBlock) } else { None }),
            pair(inline_type, delimited(punct("{"), many0(function), punct("}")))
        ),
        move |tuple| ImplBlock {
            position    : position,
            functions   : tuple.1,
            scope_id    : j.scope_id(),
            ty          : tuple.0,
            trt         : None,
        }
    ))(i)
}

fn trait_impl_block(i: Input<'_>) -> Output<ImplBlock> {
    let position = i.position();
    let j = i.clone();
    with_scope(ScopeKind::Module, map(
        preceded(
            check_flags(keyword("impl"), |s| if s.in_function { Some(ParseErrorKind::IllegalImplBlock) } else { None }),
            pair(
                pair(terminated(path, keyword("for")), inline_type),
                delimited(punct("{"), many0(function), punct("}"))
            )
        ),
        move |tuple| ImplBlock {
            position    : position,
            functions   : tuple.1,
            scope_id    : j.scope_id(),
            ty          : tuple.0.1,
            trt         : Some(TypeName::from_path(tuple.0.0)),
        }
    ))(i)
}

// function/closure/return

fn function_return_part(i: Input<'_>) -> Output<InlineType> {
    preceded(punct("->"), inline_type)(i)
}

fn function_parameter_list(i: Input<'_>) -> Output<Vec<LetBinding>> {
    fn parameter(i: Input<'_>) -> Output<LetBinding> {
        let position = i.position();
        map(
            tuple((opt(keyword("mut")), var_decl, punct(":"), inline_type)),
            move |tuple| LetBinding {
                position    : position,
                binding_id  : tuple.1.binding_id,
                ident       : tuple.1.ident,
                expr        : None,
                mutable     : tuple.0.is_some(),
                ty          : Some(tuple.3),
            }
        )(i)
    }
    separated_list0(punct(","), parameter)(i)
}

fn function_transform_result(block: &mut Block, position: Position) {
    // transform block result to return statement
    if let Some(returns) = block.result.take() {
        block.statements.push(Statement::Return(Return {
            position: returns.position(),
            expr: returns,
        }));
    }
    // if the last block statement is still not a return statement, return void
    if block.control_flow() != Some(ControlFlowType::Return) {
        block.statements.push(Statement::Return(Return {
            expr: Expression::void(position),
            position: position,
        }));
    }
}

fn function(i: Input<'_>) -> Output<Function> {
    fn signature(i: Input<'_>) -> Output<Signature> {
        map(
            tuple((
                terminated(opt(keyword("pub")), check_flags(keyword("fn"), |s| if s.in_function { Some(ParseErrorKind::IllegalFunctionDef) } else { None })),
                terminated(ident, punct("(")), terminated(function_parameter_list, punct(")")), opt(function_return_part)
            )),
            |sig| Signature {
                ident   : sig.1,
                args    : sig.2,
                ret     : if let Some(sig_ty) = sig.3 { Some(sig_ty) } else { None },
                vis     : if sig.0.is_some() { Visibility::Public } else { Visibility::Private },
            },
        )(i)
    }
    let position = i.position();
    let j = i.clone();
    with_scope(ScopeKind::Function, map(
        tuple((signature, with_flags(&|flags| flags.in_function = true, alt((
            map(block, |b| Some(b)),
            map(check_flags(punct(";"), |f| if !f.in_trait { Some(ParseErrorKind::IllegalFunctionDecl) } else { None }), |_| None)
        ))))),
        move |(sig, mut block)| {
            if let Some(block) = &mut block {
                function_transform_result(block, position);
            }
            Function {
                shared: FunctionShared {
                    position,
                    sig,
                    block,
                    scope_id: j.scope_id(),
                },
                constant_id: None,
            }
        }
    ))(i)
}

fn anonymous_function(i: Input<'_>) -> Output<Function> {
    fn signature(i: Input<'_>) -> Output<Signature> {
        let position = i.position();
        map(
            preceded(
                check_flags(keyword("fn"), |s| if !s.in_function { Some(ParseErrorKind::IllegalClosure) } else { None }),
                pair(preceded(punct("("), function_parameter_list), preceded(punct(")"), opt(function_return_part)))
            ),
            move |sig| Signature {
                ident   : Ident { name: format!("anonymous@{}", position), position },
                args    : sig.0,
                ret     : if let Some(sig_ty) = sig.1 { Some(sig_ty) } else { None },
                vis     : Visibility::Private,
            },
        )(i)
    }
    let position = i.position();
    let j = i.clone();
    with_scope(ScopeKind::Function, map(
        pair(signature, block),
        move |(sig, mut block)| {
            function_transform_result(&mut block, position);
            Function {
                shared: FunctionShared {
                    position,
                    sig,
                    block: Some(block),
                    scope_id: j.scope_id(),
                },
                constant_id: None,
            }
        }
    ))(i)
}

fn closure(i: Input<'_>) -> Output<Closure> {
    fn signature(i: Input<'_>) -> Output<Signature> {
        let position = i.position();
        map(
            preceded(
                check_flags(punct("|"), |s| if !s.in_function { Some(ParseErrorKind::IllegalClosure) } else { None }),
                pair(terminated(function_parameter_list, punct("|")), opt(function_return_part))
            ),
            move |sig| {
                Signature {
                    ident   : Ident { name: format!("closure[{}]", position), position },
                    args    : sig.0,
                    ret     : if let Some(sig_ty) = sig.1 { Some(sig_ty) } else { None },
                    vis     : Visibility::Private,
                }
            }
        )(i)
    }
    let position = i.position();
    let j = i.clone();
    with_scope(ScopeKind::Closure, map(
        pair(signature, expression),
        move |(sig, expr)| {
            let required_bindings = j.take_required_bindings();
            // convert to block to allow for more reuse of Function specific code
            let mut block = if let Expression::Block(block) = expr {
                *block
            } else {
                j.push_scope(ScopeKind::Closure);
                let scope_id = j.scope_id();
                j.pop_scope(None);
                Block {
                    position    : expr.position(),
                    statements  : Vec::new(),
                    result      : Some(expr),
                    scope_id,
                }
            };
            function_transform_result(&mut block, position);
            Closure {
                shared          : FunctionShared { position, sig, block: Some(block), scope_id: j.scope_id() },
                required_bindings,
                function_id     : None,
                type_id         : None,
                struct_type_id  : None,
            }
        }
    ))(i)
}

fn return_statement(i: Input<'_>) -> Output<Return> {
    let position = i.position();
    map(
        preceded(
            check_flags(keyword("return"), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalReturn) }),
            terminated(opt(expression), punct(";"))
        ),
        move |m| Return {
            position,
            expr: m.unwrap_or_else(|| Expression::void(position)),
        }
    )(i)
}

// literals/expression

fn numeric_literal(i: Input<'_>) -> Output<Literal> {
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
    let j = i.clone();

    let (remaining, numerical) = terminated(
        recognize(tuple((
            opt(recognize(alt((punct("+"), punct("-"))))),
            digit1,
            opt(recognize(tuple((tag("."), not(punct(".")), digit0)))), // not(.) to avoid matching ranges
            opt(recognize(tuple((one_of("iuf"), alt((tag("8"), tag("16"), tag("32"), tag("64")))))))
        ))),
        space0
    )(i)?;

    let (value, type_name) = splits_numerical_suffix(*numerical);

    if value.contains(".") || type_name == Some("f32") || type_name == Some("f64") {
        if let Ok(float) = str::parse::<f64>(&value.replace(" ", "")) {
            return Ok((remaining, Literal {
                position    : position,
                value       : LiteralValue::Numeric(Numeric::Float(float)),
                type_name   : type_name.map(|ty| TypeName::from_str(ty, position)), // TODO: this is the position of the number, not just the type suffix
                type_id     : None,
            }));
        }
    } else if value.starts_with("-") || type_name == Some("i8") || type_name == Some("i16") || type_name == Some("i32") || type_name == Some("i64") {
        if let Ok(integer) = str::parse::<i64>(&value.replace(" ", "")) {
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

    Err(nom::Err::Failure(Failure { input: j, kind: ParseErrorKind::InvalidNumerical }))
}

fn bool_literal(i: Input<'_>) -> Output<Literal> {
    let position = i.position();
    map(alt((keyword("true"), keyword("false"))), move |m| {
        Literal {
            position    : position,
            value       : LiteralValue::Bool(m == "true"),
            type_name   : None,
            type_id     : None,
        }
    })(i)
}

fn string_literal(i: Input<'_>) -> Output<Expression> {

    let position = i.position();

    enum StringFragment<'a> {
        Literal(&'a str),
        EscapedChar(char),
        Interpolation(Expression),
    }

    fn fragments(i: Input<'_>) -> Output<Vec<StringFragment<'_>>> {
        // gather fragments into a vector
        fold_many0(
            alt((
                map(parse_literal, |l| StringFragment::Literal(&l.data)),
                map(parse_escaped_char, |e| StringFragment::EscapedChar(e)),
                map(delimited(punct("{"), expression, char('}')), |i| StringFragment::Interpolation(i)), // intentionally not tailing punct("}") to not consume whitespace in the string
            )),
            Vec::new,
            |mut vec, fragment| {
                vec.push(fragment);
                vec
            },
        )(i)
    }

    map(delimited(char('"'), fragments, punct("\"")), move |mut fragments| {  // intentionally not leading punct("\"") to not consume whitespace in the string

        let add = |left, right| Expression::BinaryOp(Box::new(BinaryOp {
            position,
            op: BinaryOperator::Add,
            left: BinaryOperand::Expression(left),
            right: BinaryOperand::Expression(right),
            type_id: None,
        }));

        let lit = |string| Expression::Literal(Literal {
            position,
            value: LiteralValue::String(string),
            type_name: None,
            type_id: None,
        });

        let cast = |left| Expression::BinaryOp(Box::new(BinaryOp {
            position,
            op: BinaryOperator::Cast,
            left: BinaryOperand::Expression(left),
            right: BinaryOperand::TypeName(TypeName::from_str("String", position)),
            type_id: None,
        }));

        let mut current_string = "".to_string();
        let mut ast: Option<Expression> = None;

        // construct AST from fragments
        for fragment in fragments.drain(..) {
            match fragment {
                StringFragment::Literal(l) => current_string.push_str(l),
                StringFragment::EscapedChar(c) => current_string.push(c),
                StringFragment::Interpolation(interpolation) => {
                    let current = if current_string.len() == 0 {
                        cast(interpolation)
                    } else {
                        add(
                            lit(replace(&mut current_string, "".to_string())),
                            cast(interpolation)
                        )
                    };
                    if let Some(prev) = ast {
                        ast = Some(add(prev, current));
                    } else {
                        ast = Some(current);
                    }
                },
            }
        }

        if current_string.len() > 0 {
            if let Some(prev) = ast {
                ast = Some(add(prev, lit(current_string)));
            } else {
                ast = Some(lit(current_string));
            }
        }

        ast.unwrap_or_else(|| lit("".to_string()))

    })(i)
}

fn array_literal(i: Input<'_>) -> Output<Literal> {
    let position = i.position();
    map(
        tuple((punct("["), separated_list0(punct(","), expression), opt(punct(",")), punct("]"))),
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

fn struct_literal(i: Input<'_>) -> Output<Literal> {
    fn field(i: Input<'_>) -> Output<(String, Expression)> {
        map(
            tuple((word, punct(":"), expression)),
            |tuple| (tuple.0.to_string(), tuple.2)
        )(i)
    }
    fn fields(i: Input<'_>) -> Output<UnorderedMap<String, Expression>> {
        map(
            separated_list1(punct(","), field),
            |list| {
                list.into_iter().map(|item| (item.0, item.1)).collect()
            }
        )(i)
    }
    let position = i.position();
    map(
        tuple((path, punct("{"), fields, opt(punct(",")), punct("}"))),
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

fn literal(i: Input<'_>) -> Output<Expression> {
    alt((
        map(bool_literal, |l| Expression::Literal(l)),
        string_literal,
        map(array_literal, |l| Expression::Literal(l)),
        map(struct_literal, |l| Expression::Literal(l)),
        map(numeric_literal, |l| Expression::Literal(l)),
    ))(i)
}

fn expression(i: Input<'_>) -> Output<Expression> {
    fn argument_list(i: Input<'_>) -> Output<ArgumentList> {
        let position = i.position();
        map(
            delimited(punct("("), separated_list0(punct(","), expression), punct(")")),
            move |args| ArgumentList {
                position,
                args,
            }
        )(i)
    }
    fn parens(i: Input<'_>) -> Output<Expression> {
        delimited(punct("("), expression, punct(")"))(i)
    }
    fn operand(i: Input<'_>) -> Output<Expression> {
        let j = i.clone();
        alt((
            literal,
            map(if_block, |m| Expression::IfBlock(Box::new(m))),
            map(match_block, |m| Expression::MatchBlock(Box::new(m))),
            map(block, |m| Expression::Block(Box::new(m))),
            parens,
            map(path, move |mut m| {
                // single element paths may be variables if their names exist in the current scope
                if m.name.len() == 1 {
                    match j.has_binding(&m.name[0].name) {
                        // local variable binding
                        ScopeBindingType::Local(binding_id) => return Expression::Variable(Variable { position: m.position, ident: m.name.pop().unwrap(), binding_id }),
                        // a binding originating in a parent of the current closure
                        ScopeBindingType::Parent(binding_id) => {
                            j.require_binding(binding_id);
                            return Expression::Variable(Variable { position: m.position, ident: m.name.pop().unwrap(), binding_id });
                        },
                        // not a binding, fall through to handle name.len() != 1 case as well
                        _ => {},
                    }
                }
                Expression::Constant(Constant { position: m.position, path: m, constant_id: None })
            }),
            map(snap(anonymous_function), move |m| Expression::AnonymousFunction(Box::new(m))),
            map(closure, move |m| Expression::Closure(Box::new(m))),
        ))(i)
    }
    fn prec7(i: Input<'_>) -> Output<Expression> {
        let init = operand(i.clone())?;
        let position = i.position();
        fold_many0_mut(
            alt((
                map(delimited(punct("["), expression, punct("]")), |e| (BinaryOperator::Index, BinaryOperand::Expression(e))),
                map(preceded(punct("."), ident), |i| (BinaryOperator::Access, BinaryOperand::Member(Member { position: position, ident: i, type_id: None, constant_id: None }))),
                map(snap(argument_list), |l| (BinaryOperator::Call, BinaryOperand::ArgumentList(l))),
            )),
            init.1,
            move |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position, op, left: BinaryOperand::Expression(acc), right: val, type_id: None }))
        )(init.0)
    }
    fn unary(i: Input<'_>) -> Output<Expression> {
        let position = i.position();
        map(
            pair(
                opt(alt((
                    // count number of ! to handle them all here
                    fold_many1(punct("!"), || ("!", 0usize), |mut acc, _| { acc.1 += 1; acc }),
                    // allow only one +/-, ensure not followed by numeric to avoid parsing e.g -128i8 (valid) as unary(-, 128i8) which would
                    // be an overflowing numeric literal
                    map(
                        alt((
                            terminated(punct("+"), not(one_of("+.0123456789"))),
                            terminated(punct("-"), not(one_of("-.0123456789")))
                        )),
                        |c| (c, 1)
                    )
                ))),
                prec7
            ),
            move |m| {
                if let Some(unary) = m.0 {
                    let op = match unary.0 {
                        "!" => UnaryOperator::Not,
                        "+" => UnaryOperator::Plus,
                        "-" => UnaryOperator::Minus,
                        _ => unreachable!("Parser only accepts !, + and - operator."),
                    };
                    // nest required number of unary repetitons
                    let mut acc = m.1;
                    for _ in 0..unary.1 {
                        acc = Expression::UnaryOp(Box::new(UnaryOp {
                            position: position,
                            op      : op,
                            expr    : acc,
                            type_id : None,
                        }))
                    }
                    acc
                } else {
                    m.1
                }
            }
        )(i)
    }
    fn prec6(i: Input<'_>) -> Output<Expression> {
        let position = i.position();
        map(
            pair(
                unary, opt(preceded(keyword("as"), path))
            ),
            move |(expr, path)| {
                if let Some(path) = path {
                    Expression::BinaryOp(Box::new(BinaryOp {
                        position: position,
                        op      : BinaryOperator::Cast,
                        left    : BinaryOperand::Expression(expr),
                        right   : BinaryOperand::TypeName(TypeName::from_path(path)),
                        type_id : None,
                    }))
                } else {
                    expr
                }
            }
        )(i)
    }
    fn prec5(i: Input<'_>) -> Output<Expression> {
        let init = prec6(i)?;
        let position = init.0.position();
        fold_many0_mut(
            pair(map(alt((punct("*"), punct("/"), punct("%"))), |o| BinaryOperator::from_string(o)), prec6),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: BinaryOperand::Expression(acc), right: BinaryOperand::Expression(val), type_id: None }))
        )(init.0)
    }
    fn prec4(i: Input<'_>) -> Output<Expression> {
        let init = prec5(i)?;
        let position = init.0.position();
        fold_many0_mut(
            pair(map(alt((punct("+"), punct("-"))), |o| BinaryOperator::from_string(o)), prec5),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: BinaryOperand::Expression(acc), right: BinaryOperand::Expression(val), type_id: None }))
        )(init.0)
    }
    fn prec3(i: Input<'_>) -> Output<Expression> {
        let init = prec4(i)?;
        let position = init.0.position();
        fold_many0_mut(
            pair(map(alt((punct("<="), punct(">="), punct("<"), punct(">"))), |o| BinaryOperator::from_string(o)), prec4),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: BinaryOperand::Expression(acc), right: BinaryOperand::Expression(val), type_id: None }))
        )(init.0)
    }
    fn prec2(i: Input<'_>) -> Output<Expression> {
        let init = prec3(i)?;
        let position = init.0.position();
        fold_many0_mut(
            pair(map(alt((punct("!="), punct("=="))), |o| BinaryOperator::from_string(o)), prec3),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: BinaryOperand::Expression(acc), right: BinaryOperand::Expression(val), type_id: None }))
        )(init.0)
    }
    fn prec1(i: Input<'_>) -> Output<Expression> {
        let init = prec2(i)?;
        let position = init.0.position();
        fold_many0_mut(
            pair(map(punct("&&"), |o| BinaryOperator::from_string(o)), prec2),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op: op, left: BinaryOperand::Expression(acc), right: BinaryOperand::Expression(val), type_id: None }))
        )(init.0)
    }
    fn prec0(i: Input<'_>) -> Output<Expression> {
        let init = prec1(i)?;
        let position = init.0.position();
        fold_many0_mut(
            pair(map(punct("||"), |o| BinaryOperator::from_string(o)), prec1),
            init.1,
            |acc, (op, val)| Expression::BinaryOp(Box::new(BinaryOp { position: position, op, left: BinaryOperand::Expression(acc), right: BinaryOperand::Expression(val), type_id: None }))
        )(init.0)
    }
    alt((
        map(assignment, |m| Expression::Assignment(Box::new(m))),
        prec0
    ))(i)
}

// let/assignment

fn assignment(i: Input<'_>) -> Output<Assignment> {
    fn assignable(i: Input<'_>) -> Output<Expression> {
        let var_position = i.position();
        let j = i.clone();
        let init = map(ident, |m| {
            if let Some(binding_id) = j.has_binding(&m.name).binding_id() {
                Expression::Variable(Variable {
                    position    : var_position,
                    binding_id  : binding_id,
                    ident       : m,
                })
            } else {
                // We don't have a binding id - it is not defined. But we cannot error yet because
                // there's a really high chance this isn't even supposed to be an assignment and the parent
                // parser is just going through alt() options.
                // We have to wait until the whole assignment is parsed and identified to actually be an
                // assignment before generating an error.
                // There's probably a way to handle this with nom error handling but I can't figure it out,
                // so instead we'll just return a constant and check for that somewhere downstream.
                Expression::Constant(Constant {
                    position    : var_position,
                    constant_id : None,
                    path        : Path { name: vec![ m ], position: var_position },
                })
            }
        })(i)?;
        let op_position = init.0.position();
        fold_many0_mut(
            alt((
                map(delimited(punct("["), expression, punct("]")), |e| (BinaryOperator::IndexWrite, BinaryOperand::Expression(e))),
                map(preceded(punct("."), ident), |i| (BinaryOperator::AccessWrite, BinaryOperand::Member(Member { position: op_position, ident: i, type_id: None, constant_id: None })))
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
                Expression::BinaryOp(Box::new(BinaryOp { position: op_position, op: op, left: BinaryOperand::Expression(acc), right: val, type_id: None }))
            }
        )(init.0)
    }
    fn assignment_operator(i: Input<'_>) -> Output<BinaryOperator> {
        map(
            alt((punct("="), punct("+="), punct("-="), punct("*="), punct("/="), punct("%="))),
            |o| {
                BinaryOperator::from_string(o)
            }
        )(i)
    }
    let position = i.position();
    map(
        tuple((assignable, assignment_operator, expression)),
        move |m| {
            // TODO: check that left is not a constant
            Assignment {
                position: position,
                op      : m.1,
                left    : m.0,
                right   : m.2,
                type_id : None,
            }
        }
    )(i)
}

fn let_binding(i: Input<'_>) -> Output<LetBinding> {
    let position = i.position();
    map(
        preceded(
            check_flags(keyword("let"), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalLetStatement) }),
            tuple((opt(keyword("mut")), var_decl, opt(preceded(punct(":"), inline_type)), opt(preceded(punct("="), expression)), punct(";")))
        ),
        move |m| LetBinding {
            position    : position.clone(),
            binding_id  : m.1.binding_id,
            ident       : m.1.ident,
            mutable     : m.0.is_some(),
            expr        : m.3,
            ty          : m.2,
        }
    )(i)
}

// blocks

fn block(i: Input<'_>) -> Output<Block> {
    let position = i.position();
    let j = i.clone();
    with_scope(ScopeKind::Block, map(
        delimited(
            punct("{"),
            // contain flags here but don't set any yet. required by return statement dead code marker
            with_flags(&|_| (), pair(many0(statement), opt(expression))),
            punct("}")
        ),
        move |m| {
            Block::new(position, j.scope_id(), m.0, m.1)
        }
    ))(i)
}

fn if_block(i: Input<'_>) -> Output<IfBlock> {
    fn else_block(i: Input<'_>) -> Output<Block> {
        let position = i.position();
        let j = i.clone();
        preceded(
            keyword("else"),
            alt((
                map(if_block, move |m| Block::new(position, j.scope_id(), Vec::new(), Some(Expression::IfBlock(Box::new(m))))),
                block
            ))
        )(i)
    }
    let position = i.position();
    let j = i.clone();
    preceded(
        check_flags(keyword("if"), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalIfBlock) }),
        map(
            tuple((expression, block, opt(else_block))),
            move |m| IfBlock {
                    position    : position,
                    cond        : m.0,
                    if_block    : m.1,
                    else_block  : m.2,
                    scope_id    : j.scope_id(),
                }
            )
    )(i)
}

fn match_block(i: Input<'_>) -> Output<MatchBlock> {
    /*fn variant_literal(i: Input<'_>) -> Output<Literal> {
        let position = i.position();
        map(
            tuple((ident, punct("::"), path)), // TODO: this ensures a path of at least two elements to avoid conflicts with idents but makes it impossible to `use num::Variant`. ideally the resolver would handle determining whether something is an variable or enum variant constructor
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
    }*/
    fn match_pattern(i: Input<'_>) -> Output<Pattern> {
        //let position = i.position();
        //alt((
            map(path, |v| Pattern::SimpleVariant(v))
        //))
        (i)
    }
    fn match_case(i: Input<'_>) -> Output<Block> {
        let position = i.position();
        let j = i.clone();
        alt((
            block,
            map(expression, move |e| Block::new(position, j.scope_id(), Vec::new(), Some(e))),
        ))(i)
    }
    fn match_list(i: Input<'_>) -> Output<Vec<(Pattern, Block)>> {
        separated_list1(
            punct(","),
            pair(match_pattern, preceded(punct("=>"), match_case))
        )(i)
    }
    let position = i.position();
    let j = i.clone();
    preceded(
        check_flags(keyword("match"), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalIfBlock) }),
        map(
            pair(expression, delimited(punct("{"), match_list, preceded(opt(punct(",")), punct("}")))),
            move |m| MatchBlock {
                position    : position,
                expr        : m.0,
                branches    : m.1,
                scope_id    : j.scope_id(),
            }
        )
    )(i)
}

// loops/loop control

fn for_loop(i: Input<'_>) -> Output<ForLoop> {
    fn loop_range(i: Input<'_>) -> Output<Expression> {
        let position = i.position();
        map(
            tuple((expression, alt((punct("..="), punct(".."))), expression)),
            move |m| Expression::BinaryOp(Box::new(BinaryOp {
                position    : position,
                op          : BinaryOperator::from_string(m.1),
                left        : BinaryOperand::Expression(m.0),
                right       : BinaryOperand::Expression(m.2),
                type_id     : None
            }))
        )(i)
    }
    let position = i.position();
    let j = i.clone();
    with_scope(ScopeKind::Block, map(
        preceded(
            check_flags(keyword("for"), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalForLoop) }),
            tuple((var_decl, keyword("in"), alt((loop_range, expression)), with_flags(&|flags| flags.in_loop = true, block)))
        ),
        move |m| ForLoop {
            position: position,
            iter: LetBinding {
                position    : position,
                binding_id  : m.0.binding_id,
                ident       : m.0.ident,
                mutable     : true,
                expr        : None,
                ty          : None,
            },
            expr: m.2,
            block: m.3,
            scope_id: j.scope_id(),
        }
    ))(i)
}

fn while_loop(i: Input<'_>) -> Output<WhileLoop> {
    let position = i.position();
    let j = i.clone();
    map(
        preceded(
            check_flags(keyword("while"), |s| if s.in_function { None } else { Some(ParseErrorKind::IllegalWhileLoop) }),
            pair(expression, with_flags(&|flags| flags.in_loop = true, block))
        ),
        move |m| WhileLoop {
            position: position,
            expr    : m.0,
            block   : m.1,
            scope_id: j.scope_id(),
        }
    )(i)
}

fn break_statement(i: Input<'_>) -> Output<Break> {
    let position = i.position();
    map(
        preceded(
            check_flags(keyword("break"), |s| if s.in_loop { None } else { Some(ParseErrorKind::IllegalBreak) }),
            punct(";")
        ),
        move |_| Break { position }
    )(i)
}

fn continue_statement(i: Input<'_>) -> Output<Continue> {
    let position = i.position();
    map(
        preceded(
            check_flags(keyword("continue"), |s| if s.in_loop { None } else { Some(ParseErrorKind::IllegalContinue) }),
            punct(";")
        ),
        move |_| Continue { position }
    )(i)
}

// statement

fn statement(i: Input<'_>) -> Output<Statement> {
    let j = i.clone();
    let output = alt((
        map(use_decl, |m| Statement::UseDecl(m)),
        map(let_binding,|m| Statement::LetBinding(m)),
        map(if_block, |m| Statement::IfBlock(m)),
        map(for_loop, |m| Statement::ForLoop(m)),
        map(while_loop, |m| Statement::WhileLoop(m)),
        map(return_statement, |m| Statement::Return(m)),
        map(break_statement, |m| Statement::Break(m)),
        map(continue_statement, |m| Statement::Continue(m)),
        map(block, |m| Statement::Block(m)),
        map(snap(terminated(expression, punct(";"))), |m| Statement::Expression(m)),
    ))(i);
    if let Ok((_, s)) = &output {
        if s.identify_control_flow(true).is_some() {
            j.flags_mut(|f| f.is_dead = true);
        }
    }
    output
}

// root

fn root_items(i: Input<'_>) -> Output<Statement> {
    alt((
        map(use_decl, |m| Statement::UseDecl(m)),
        map(module, |m| Statement::Module(m)),
        map(function, |m| Statement::Function(m)),
        map(struct_type, |m| Statement::StructDef(m)),
        map(enum_type, |m| Statement::EnumDef(m)),
        map(trait_impl_block,|m| Statement::ImplBlock(m)),
        map(impl_block,|m| Statement::ImplBlock(m)),
        map(trait_type, |m| Statement::TraitDef(m)),
    ))(i)
}

/// Parses Itsy source code into a program AST structure.
pub fn parse_module(link_state: &mut LinkState, src: &str, module_path: &str) -> ParseResult<ParsedModule> {
    let input = Input::new(src, replace(link_state, LinkState::new()));
    let module_scope_id = link_state.next_scope_id;
    let result = all_consuming(preceded(space0, with_scope(ScopeKind::Module, many0(root_items))))(input.clone());
    *link_state = input.take_linkstate();
    match result {
        Ok(result) => {
            Ok(ParsedModule::new(module_scope_id, module_path, result.1))
        },
        Err(err) => {
            match err {
                nom::Err::Incomplete(_) => unreachable!("Parser does not return Incomplete."),
                nom::Err::Failure(failure) => {
                    // a failure generated by checkpoint
                    Err(ParseError::new(failure.kind, failure.input.position(), module_path))
                }
                nom::Err::Error(_) => {
                    // nom error is useless to us, but we stored the highest parsed offset on the input which is the most likely error position
                    let error = input.max_parsed();
                    Err(ParseError::new(ParseErrorKind::SyntaxError, Position(src.len() - error.1), module_path))
                }
            }
        }
    }
}

/// Parses Itsy source files into a program AST structure. The loader receives the fully qualified module name and is
/// expected to return the parsed module.
///
/// # Examples
///
/// The following example parses a module with its dependencies.
/// ```
/// use itsy::parser;
/// use std::fs;
///
/// fn main() {
///     let source_file = "itsy/rustdoc/parse.itsy";
///     let parsed = parser::parse(|module_path, state| {
///         let filename = parser::module_filename(source_file, module_path, false);
///         let file = fs::read_to_string(filename)?;
///         parser::parse_module(state, &file, module_path)
///     }).unwrap();
/// }
/// ```
///
/// The returned [ParsedProgram] is now ready for type resolution by [resolve](crate::resolver::resolve).
pub fn parse(mut loader: impl FnMut(&str, &mut LinkState) -> ParseResult<ParsedModule>) -> ParseResult<ParsedProgram> {
    let mut program = ParsedProgram::new();
    let mut state = LinkState::new();
    parse_recurse("", &mut state, &mut program, &mut loader)?;
    program.set_link_state(state);
    Ok(program)
}

/// Recursively parse all submodules.
fn parse_recurse(module_path: &str, state: &mut LinkState, program: &mut ParsedProgram, loader: &mut impl FnMut(&str, &mut LinkState) -> ParseResult<ParsedModule>) -> ParseResult {
    let module = loader(module_path, state)?;
    for submodule_ast in module.modules() {
        let submodule_path = if module_path != "" { module_path.to_string() + "::" + submodule_ast.name() } else { submodule_ast.name().to_string() };
        match parse_recurse(&submodule_path, state, program, loader) {
            Ok(module) => Ok(module),
            Err(err) => Err(match err.kind {
                ParseErrorKind::IOError(_) => ParseError::new(ParseErrorKind::ModuleNotFound(submodule_path), submodule_ast.position, module_path),
                _ => err,
            }),
        }?;
    }
    program.add_module(module);
    Ok(())
}

/// Given the filename to an itsy program main module and an Itsy module path, returns the filename of the Itsy module.
pub fn module_filename<P: Into<std::path::PathBuf>>(main_file: P, module_path: &str, subdirectory: bool) -> std::path::PathBuf {
    if module_path == "" {
        main_file.into()
    } else {
        let mut path: std::path::PathBuf = main_file.into();
        if path.pop() {
            for module_name in path_to_parts(module_path) {
                path.push(module_name);
            }
            if subdirectory {
                path.push("mod");
            }
        } else {
            panic!("Invalid filename.");
        }
        path.set_extension("itsy");
        path
    }
}