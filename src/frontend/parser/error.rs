use std::fmt::{self, Display};
use crate::frontend::ast::Position;

/// Represents the various possible parser error-kinds.
#[derive(Copy, Clone, Debug)]
pub enum ParseErrorKind {
    SyntaxError,
    InvalidNumerical,
    IllegalReturn,
    IllegalFunction,
    IllegalImplBlock,
    IllegalForLoop,
    IllegalWhileLoop,
    IllegalStructDef,
    IllegalIfBlock,
    IllegalLetStatement,
    IllegalModuleDef,
}

/// An error reported by the parser (e.g. syntax error).
#[derive(Copy, Clone, Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    position: Position,
}

impl ParseError {
    pub(crate) fn new(kind: ParseErrorKind, position: Position) -> ParseError {
        Self { kind, position }
    }
    /// Compute 1-based line/column number from Position (absolute offset from end) in string.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        self.position.loc(input)
    }
    /// The kind of the error.
    pub fn kind(self: &Self) -> &ParseErrorKind {
        &self.kind
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