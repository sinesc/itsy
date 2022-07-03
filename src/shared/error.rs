use std::path::{Path, PathBuf};
use crate::prelude::*;
use crate::frontend::{parser::error::{ParseError, ParseErrorKind}, resolver::error::ResolveError};
use crate::bytecode::compiler::error::CompileError;
#[cfg(feature="runtime")]
use crate::bytecode::runtime::error::RuntimeError;

/// An error generated during program compilation or execution.
#[derive(Clone, Debug)]
pub enum Error {
    ParseError(ParseError),
    ResolveError(ResolveError),
    CompileError(CompileError),
    #[cfg(feature="runtime")]
    RuntimeError(RuntimeError),
}

#[cfg(feature="compiler")]
impl Error {
    /// Compute 1-based line/column number in string.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        match self {
            Self::ParseError(e) => e.loc(input),
            Self::ResolveError(e) => e.loc(input),
            Self::CompileError(e) => e.loc(input),
            #[cfg(feature="runtime")]
            Self::RuntimeError(_) => (0, 0),
        }
    }
    /// Path to the module where the error occured.
    pub fn module_path(self: &Self) -> &str {
        match self {
            Self::ParseError(e) => e.module_path(),
            Self::ResolveError(e) => e.module_path(),
            Self::CompileError(e) => e.module_path(),
            #[cfg(feature="runtime")]
            Self::RuntimeError(_) => "",
        }
    }
}

impl Display for Error {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParseError(e) => write!(f, "Parser error: {}", e),
            Self::ResolveError(e) => write!(f, "Resolver error: {}", e),
            Self::CompileError(e) => write!(f, "Compiler error: {}", e),
            #[cfg(feature="runtime")]
            Self::RuntimeError(e) => write!(f, "Runtime error: {}", e),
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

#[cfg(feature="runtime")]
impl From<RuntimeError> for Error {
    fn from(error: RuntimeError) -> Error {
        Error::RuntimeError(error)
    }
}

/// An `Error`-wrapper returned by `build()`.
#[derive(Clone, Debug)]
pub struct BuildError {
    pub(crate) error: Error,
    pub(crate) filename: PathBuf,
    pub(crate) source: String,
}

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
            #[cfg(feature="runtime")]
            Error::RuntimeError(e) => (0, e.offset() as u32),
        }
    }
    /// Path to the module where the error occured.
    pub fn module_path(self: &Self) -> &str {
        self.error.module_path()
    }
}

impl Display for BuildError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let loc = self.loc();
        match &self.error {
            Error::ParseError(ParseError { kind: ParseErrorKind::IOError(_), .. }) => write!(f, "{}", self.error),
            _ => write!(f, "{} in line {}, column {} in file {}", self.error, loc.0, loc.1, self.filename.to_string_lossy()),
        }
    }
}