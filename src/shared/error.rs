#[cfg(feature="compiler")]
use std::path::{Path, PathBuf};
use crate::prelude::*;
#[cfg(feature="compiler")]
use crate::frontend::{parser::error::{ParseError, ParseErrorKind}, resolver::error::ResolveError};
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
    /// Compute 1-based line/column number in string.
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

/// An `Error`-wrapper returned by `build()`.
#[cfg(feature="compiler")]
#[derive(Clone, Debug)]
pub struct BuildError {
    pub(crate) error: Error,
    pub(crate) filename: PathBuf,
    pub(crate) source: String,
}

#[cfg(feature="compiler")]
impl BuildError {
    /// Returns the error being wrapped by this build error.
    pub fn error(self: &Self) -> &Error {
        &self.error
    }
    /// Returns the name of the file the error occured in.
    pub fn filename(self: &Self) -> &Path {
        &self.filename.as_path()
    }
    /// Returns the contents of the file the error occured in.
    pub fn source(self: &Self) -> &str {
        &self.source
    }
    /// Compute 1-based line/column number in string.
    pub fn loc(self: &Self) -> (u32, u32) {
        match &self.error {
            Error::ParseError(e) => e.loc(&self.source),
            Error::ResolveError(e) => e.loc(&self.source),
            Error::CompileError(e) => e.loc(&self.source),
            _ => (0, 0),
        }
    }
    /// Path to the module where the error occured.
    pub fn module_path(self: &Self) -> &str {
        self.error.module_path()
    }
}

#[cfg(feature="compiler")]
impl Display for BuildError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let loc = self.loc();
        match self.error {
            Error::ParseError(ParseError { kind: ParseErrorKind::IOError(_), .. }) => write!(f, "{}", self.error),
            _ => write!(f, "{} in line {}, column {} in file {}", self.error, loc.0, loc.1, self.filename.to_string_lossy()),
        }
    }
}