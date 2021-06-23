
use std::fmt::{self, Display};
use crate::frontend::ast::Position;
use crate::util::compute_loc;

/// Represents the various possible parser error-kinds.
#[repr(u32)]
#[derive(Copy, Clone, Debug)]
pub enum ParseErrorKind {
    Unknown = 1,
    UnexpectedEOF,
    SyntaxError,
    SyntaxLet,
    SyntaxFn,
    SyntaxIf,
    SyntaxElse,
    SyntaxInlineType,
    InvalidNumerical,
    // TODO: add error handling to all parsers where it make sense
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