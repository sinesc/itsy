use std::fmt::{self, Display};
#[cfg(feature="compiler")]
use crate::frontend::{parser::error::ParseError, resolver::error::ResolveError};
#[cfg(feature="compiler")]
use crate::bytecode::compiler::error::CompileError;

/// An error generated during program compilation or execution.
#[derive(Clone, Debug)]
pub enum Error {
    #[cfg(feature="compiler")]
    ParseError(ParseError),
    #[cfg(feature="compiler")]
    ResolveError(ResolveError),
    #[cfg(feature="compiler")]
    CompileError(CompileError),
    RuntimeError, // TODO: details
}

#[cfg(feature="compiler")]
impl Error {
    /// Compute 1-based line/column number from Position (absolute offset from end) in string.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        match self {
            Self::ParseError(e) => e.loc(input),
            Self::ResolveError(e) => e.loc(input),
            Self::CompileError(e) => e.loc(input),
            Self::RuntimeError => (0, 0),
        }
    }
    /// Path to the module where the error occured.
    pub fn module_path(self: &Self) -> &str {
        match self {
            Self::ParseError(e) => e.module_path(),
            Self::ResolveError(e) => e.module_path(),
            Self::CompileError(e) => e.module_path(),
            Self::RuntimeError => "",
        }
    }
}

impl Display for Error {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            #[cfg(feature="compiler")]
            Self::ParseError(e) => write!(f, "Parser error: {}", e),
            #[cfg(feature="compiler")]
            Self::ResolveError(e) => write!(f, "Resolver error: {}", e),
            #[cfg(feature="compiler")]
            Self::CompileError(e) => write!(f, "Compiler error: {}", e),
            Self::RuntimeError => write!(f, "Runtime error"),
        }
    }
}

#[cfg(feature="compiler")]
impl From<ParseError> for Error {
    fn from(error: ParseError) -> Error {
        Error::ParseError(error)
    }
}

#[cfg(feature="compiler")]
impl From<ResolveError> for Error {
    fn from(error: ResolveError) -> Error {
        Error::ResolveError(error)
    }
}

#[cfg(feature="compiler")]
impl From<CompileError> for Error {
    fn from(error: CompileError) -> Error {
        Error::CompileError(error)
    }
}