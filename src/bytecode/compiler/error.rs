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
}

impl CompileError {
    pub(super) fn new(item: &impl Positioned, kind: CompileErrorKind) -> CompileError {
        Self { kind, position: item.position() }
    }
    /// Compute 1-based line/column number from Position (absolute offset from end) in string.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        self.position.loc(input)
    }
    /// The kind of the error.
    pub fn kind(self: &Self) -> &CompileErrorKind {
        &self.kind
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