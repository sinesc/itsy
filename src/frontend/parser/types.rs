
use std::fmt::{self, Display};
use std::cell::Cell;
use std::rc::Rc;
use std::ops::Deref;
use crate::frontend::ast::{Statement, Position};
use crate::util::compute_loc;

/// Parsed program AST.
#[derive(Debug)]
pub struct ParsedProgram<'a> (pub Vec<Statement<'a>>);

/// Represents the various possible parser error-kinds.
#[derive(Copy, Clone, Debug)]
pub enum ParseErrorKind {
    SyntaxError,
    InvalidNumerical,
    IllegalReturn,
    IllegalFunction,
    IllegalForLoop,
    IllegalWhileLoop,
    IllegalStructDef,
    IllegalIfBlock,
    IllegalLetStatement,
}

/// An error reported by the parser (e.g. syntax error).
#[derive(Copy, Clone, Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    position: Position, // this is the position from the end of the input
}

impl ParseError {
    pub(crate) fn new(kind: ParseErrorKind, offset: Position) -> ParseError {
        Self { kind: kind, position: offset }
    }
    /// Computes and returns the source code location of this error.
    pub fn loc(self: &Self, input: &str) -> (Position, Position) {
        compute_loc(input, input.len() as Position - self.position)
    }
}

impl Display for ParseError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParseErrorKind::SyntaxError => write!(f, "Syntax error"),
            ParseErrorKind::InvalidNumerical => write!(f, "Invalid numeric value"),
            // Todo: handle the others
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

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
        self.data.len() as Position
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