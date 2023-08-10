use crate::prelude::*;
use crate::frontend::{ast::{Statement, Position, Module}, parser::error::ParseErrorKind};
use crate::shared::typed_ids::ScopeId;
use std::rc::Rc;
use std::cell::{RefCell, Cell};

/// Parsed sourcecode of a single Itsy module.
#[derive(Debug)]
pub struct ParsedModule {
    pub(crate) path: String,
    pub(crate) ast: Vec<Statement>,
    pub(crate) scope_id: Option<ScopeId>,
}

impl ParsedModule {
    /// Creates a new ParsedModule from a vector of Statements.
    pub fn new<S: Into<String>>(path: S, ast: Vec<Statement>) -> Self {
        Self {
            path: path.into(),
            ast,
            scope_id: None,
        }
    }
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
    pub fn statements<'a>(self: &'a Self) -> impl Iterator<Item=&Statement> {
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

/// Parse flags, used to determine if some input is currently legal.
#[derive(Clone, Copy, Debug)]
pub(super) struct ParserFlags {
    pub in_function: bool,
    pub in_loop: bool,
}

impl ParserFlags {
    fn new() -> Self {
        ParserFlags {
            in_function: false,
            in_loop: false,
        }
    }
}

/// Interal parser state, tracked via RC through the Input type.
pub(super) struct ParserState { // cannot implement clone because cell requires inner to be copy to be able to be cloned
    len: usize,
    max_parsed: Cell<(Option<ParseErrorKind>, usize)>,
    flags: Cell<ParserFlags>,
    scopes: RefCell<Vec<UnorderedSet<String>>>,
}

/// Parser input
#[derive(Clone)]
pub(super) struct Input<'a> {
    pub data: &'a str,
    pub state: Rc<ParserState>,
}

impl<'a> Debug for Input<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let max_parsed = self.max_parsed();
        f.debug_struct("Input")
         .field("data", &self.data)
         .field("max_parsed", &max_parsed)
         .field("flags", &self.state.flags)
         .finish()
    }
}

impl<'a> Input<'a> {
    pub fn new(data: &'a str) -> Self {
        Input {
            data: data,
            state: Rc::new(ParserState {
                len         : data.len(),
                max_parsed  : Cell::new((None, 0)),
                flags       : Cell::new(ParserFlags::new()),
                scopes      : RefCell::new(Vec::new()),
            })
        }
    }
    pub fn from_str(self: &Self, data: &'a str) -> Self {
        Input {
            data    : data,
            state   : self.state.clone(),
        }
    }
    pub fn position(self: &Self) -> Position {
        Position(self.state.len - self.data.len())
    }
    pub fn max_parsed_mut(self: &Self, inner: impl Fn(&mut (Option<ParseErrorKind>, usize))) {
        let mut max_parsed = self.state.max_parsed.take();
        inner(&mut max_parsed);
        self.state.max_parsed.set(max_parsed);
    }
    pub fn max_parsed(self: &Self) -> (Option<ParseErrorKind>, usize) {
        let max_parsed = self.state.max_parsed.take();
        let result = max_parsed.clone();
        self.state.max_parsed.set(max_parsed);
        result
    }
    pub fn flags_mut(self: &Self, inner: impl Fn(&mut ParserFlags)) {
        let mut state = self.state.flags.get();
        inner(&mut state);
        self.state.flags.set(state);
    }
    pub fn flags(self: &Self) -> ParserFlags {
        self.state.flags.get()
    }
    pub fn push_scope(self: &Self) {
        self.state.scopes.borrow_mut().push(UnorderedSet::new());
    }
    pub fn pop_scope(self: &Self) {
        self.state.scopes.borrow_mut().pop();
    }
    pub fn add_binding(self: &Self, name: String) {
        self.state.scopes.borrow_mut().last_mut().unwrap().insert(name);
    }
    pub fn has_binding(self: &Self, name: &str) -> bool {
        let scopes = self.state.scopes.borrow_mut();
        let dbg=scopes.iter().rev().find(|s| s.contains(name)).is_some();
        //println!("has_binding {name}: {dbg}");
        return dbg;
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