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
    DisabledFeature(&'static str),
}

/// An error reported by the parser (e.g. syntax error).
#[derive(Clone, Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    position: Position,
    module_path: String,
}

impl ParseError {
    pub(crate) fn new(kind: ParseErrorKind, position: Position, module_path: &str) -> ParseError {
        Self { kind, position, module_path: module_path.to_string() }
    }
    /// Compute 1-based line/column number from Position (absolute offset from end) in string.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        self.position.loc(input)
    }
    /// The kind of the error.
    pub fn kind(self: &Self) -> &ParseErrorKind {
        &self.kind
    }
    /// Path to the module where the error occured.
    pub fn module_path(self: &Self) -> &str {
        &self.module_path
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