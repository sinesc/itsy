use std::fmt::{self, Display};
use crate::frontend::parser::error::ParseError;
use crate::frontend::resolver::error::ResolveError;
use crate::bytecode::compiler::error::CompileError;

/// An error generated during program compilation or execution.
#[derive(Clone, Debug)]
pub enum Error {
    ParseError(ParseError),
    ResolveError(ResolveError),
    CompileError(CompileError),
    RuntimeError, // TODO: details
}

impl Error {
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        match self {
            Self::ParseError(e) => e.loc(input),
            Self::ResolveError(e) => e.loc(input),
            Self::CompileError(e) => e.loc(input),
            Self::RuntimeError => (0, 0),
        }
    }
}

impl Display for Error {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParseError(e) => write!(f, "Parser error: {}", e),
            Self::ResolveError(e) => write!(f, "Resolver error: {}", e),
            Self::CompileError(e) => write!(f, "Compiler error: {}", e),
            Self::RuntimeError => write!(f, "Runtime error"),
        }
    }
}

impl From<ParseError> for Error {
    fn from(error: ParseError) -> Error {
        Error::ParseError(error)
    }
}

impl From<ResolveError> for Error {
    fn from(error: ResolveError) -> Error {
        Error::ResolveError(error)
    }
}

impl From<CompileError> for Error {
    fn from(error: CompileError) -> Error {
        Error::CompileError(error)
    }
}