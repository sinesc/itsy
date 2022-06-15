use crate::prelude::*;
use crate::frontend::{ast::{Statement, Position, Module}, parser::error::{ParseErrorKind}};
use crate::shared::typed_ids::ScopeId;
use std::rc::Rc;
use std::cell::Cell;

/// Parsed sourcecode of a single Itsy module.
#[derive(Debug)]
pub struct ParsedModule {
    pub(crate) path: String,
    pub(crate) ast: Vec<Statement>,
    pub(crate) scope_id: Option<ScopeId>,
}

impl ParsedModule {
    /// Returns an iterator over all submodules of the module.
    pub fn modules<'a>(self: &'a Self) -> impl Iterator<Item=&Module> {
        self.ast
            .iter()
            .filter_map(|s| match s {
                Statement::Module(m) => Some(m),
                _ => None,
            })
    }
    /// Returns an iterator over all use mappings in the module.
    pub fn using<'a>(self: &'a Self) -> impl Iterator<Item=(&String, &String)> {
        self.ast
            .iter()
            .filter_map(|s| match s {
                Statement::Use(u) => Some(u.mapping.iter().map(|(k, v)| (k, &v.0))),
                _ => None,
            })
            .flatten()
    }
    /// Returns an iterator over all statements in the source.
    pub fn iter<'a>(self: &'a Self) -> impl Iterator<Item=&Statement> {
        self.ast.iter()
    }
}

/// Parsed sourcecode of an Itsy program.
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
    /// Returns an iterator over all modules in the program.
    pub fn modules(self: &Self) -> impl Iterator<Item=&ParsedModule> {
        self.0.iter()
    }
}

/// Interal parser state, tracked via RC through the Input type.
#[derive(Clone, Copy, Debug)]
pub(super)struct ParserState {
    pub in_function: bool,
    pub in_loop: bool,
}

impl ParserState {
    fn new() -> Self {
        ParserState {
            in_function: false,
            in_loop: false,
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
pub(super) type Output<'a, O> = nom::IResult<Input<'a>, O, Failure<'a>>;

/// Parser failure. Converted to ParseError on return of the parse() function to remove the lifetime bound.
#[derive(Debug)]
pub(super) struct Failure<'a> {
    pub input: Input<'a>,
    pub kind: ParseErrorKind,
}