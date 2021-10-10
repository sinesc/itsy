use std::cell::Cell;
use std::rc::Rc;
use std::ops::Deref;
use crate::frontend::{ast::{Statement, Position, Module}, parser::error::{ParseErrorKind}};

/// Parsed source-file AST.
#[derive(Debug)]
pub struct ParsedModule {
    pub(crate) path: String,
    pub(crate) ast: Vec<Statement>,
}

impl ParsedModule {
    /// Returns an iterator over modules referenced by the source.
    pub fn modules<'a>(self: &'a Self) -> impl Iterator<Item=&Module> {
        self.ast
            .iter()
            .filter_map(|s| match s {
                Statement::Module(m) => Some(m),
                _ => None,
            })
    }
    /// Returns an iterator over all statements in the source.
    pub fn iter<'a>(self: &'a Self) -> impl Iterator<Item=&Statement> {
        self.ast.iter()
    }
}

pub struct ParsedProgram (pub(crate) Vec<ParsedModule>);

impl ParsedProgram {
    /// Returns an empty ParsedProgram instance.
    pub fn new() -> Self {
        Self(Vec::new())
    }
    /// Adds a module to the instance.
    pub fn add_module(self: &mut Self, module: ParsedModule) {
        self.0.push(module);
    }
    /// Returns an iterator over all modules and their paths in the program.
    pub fn modules(self: &Self) -> impl Iterator<Item=&ParsedModule> {
        self.0.iter()
    }
}

/// Interal parser state, tracked via RC through the Input type.
#[derive(Clone, Copy, Debug)]
pub(super)struct ParserState {
    pub in_function: bool,
}

impl ParserState {
    fn new() -> Self {
        ParserState {
            in_function: false,
        }
    }
}

/// Parser input
#[derive(Clone, Debug)]
pub(super) struct Input<'a> {
    pub data: &'a str,
    pub max_parsed: Rc<Cell<(Option<ParseErrorKind>, usize)>>,
    pub state: Rc<Cell<ParserState>>,
}

impl<'a> Input<'a> {
    pub fn new(data: &'a str) -> Self {
        Input {
            data        : data,
            max_parsed  : Rc::new(Cell::new((None, 0))),
            state       : Rc::new(Cell::new(ParserState::new())),
        }
    }
    pub fn position(self: &Self) -> Position {
        Position(self.data.len())
    }
    pub fn max_parsed(self: &Self) -> (Option<ParseErrorKind>, usize) {
        self.max_parsed.get()
    }
    pub fn max_parsed_mut(self: &Self, inner: impl Fn(&mut (Option<ParseErrorKind>, usize))) {
        let mut max_parsed = self.max_parsed.get();
        inner(&mut max_parsed);
        self.max_parsed.set(max_parsed);
    }

    pub fn state_mut(self: &Self, inner: impl Fn(&mut ParserState)) {
        let mut state = self.state.get();
        inner(&mut state);
        self.state.set(state);
    }
    pub fn state(self: &Self) -> ParserState {
        self.state.get()
    }
    pub fn from_str(self: &Self, data: &'a str) -> Self {
        Input {
            data        : data,
            max_parsed  : self.max_parsed.clone(),
            state       : self.state.clone(),
        }
    }
}

impl<'a> Deref for Input<'a> {
    type Target = &'a str;
    fn deref(self: &Self) -> &Self::Target {
        &self.data
    }
}

impl<'a> PartialEq for Input<'a> {
    fn eq(self: &Self, other: &Self) -> bool {
        self.data == other.data
    }
}

/// Parser output
pub(super) type Output<'a, O> = nom::IResult<Input<'a>, O, Error<'a>>;

/// Parser error
#[derive(Debug)]
pub(super) struct Error<'a> {
    pub input: Input<'a>,
    pub kind: ParseErrorKind,
}