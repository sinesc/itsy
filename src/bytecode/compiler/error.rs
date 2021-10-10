use std::fmt::{self, Display};
use crate::frontend::ast::{Positioned, Position};

/// Represents the various possible compiler error-kinds.
#[derive(Copy, Clone, Debug)]
pub enum CompileErrorKind {
    Uninitialized,
    Internal,
}

/// An error reported by the compiler.
#[derive(Clone, Debug)]
pub struct CompileError {
    kind: CompileErrorKind,
    position: Position,
    module_path: String,
}

impl CompileError {
    pub(crate) fn new(item: &impl Positioned, kind: CompileErrorKind, module_path: &str) -> CompileError {
        Self { kind, position: item.position(), module_path: module_path.to_string() }
    }
    /// Compute 1-based line/column number from Position (absolute offset from end) in string.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        self.position.loc(input)
    }
    /// The kind of the error.
    pub fn kind(self: &Self) -> &CompileErrorKind {
        &self.kind
    }
    /// Path to the module where the error occured.
    pub fn module_path(self: &Self) -> &str {
        &self.module_path
    }
}

impl Display for CompileError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            CompileErrorKind::Internal => write!(f, "Internal error"),
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

pub type CompileResult = Result<(), CompileError>;