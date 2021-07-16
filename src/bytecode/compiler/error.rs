use std::fmt::{self, Display};
use crate::shared::compute_loc;
use crate::frontend::ast::{Positioned, Position};

/// Represents the various possible compiler error-kinds.
#[derive(Clone, Debug)]
pub enum CompileErrorKind {
    Error
}

/// An error reported by the compiler.
#[derive(Clone, Debug)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    position: Position, // this is the position from the end of the input
}

impl CompileError {
    pub(super) fn new(item: &impl Positioned, kind: CompileErrorKind) -> CompileError {
        Self { kind: kind, position: item.position() }
    }
    /// Computes and returns the source code location of this error. Since the AST only stores byte
    /// offsets, the original source is required to recover line and column information.
    pub fn loc(self: &Self, input: &str) -> (Position, Position) {
        compute_loc(input, input.len() as Position - self.position)
    }
}

impl Display for CompileError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            CompileErrorKind::Error => write!(f, "Internal error"),
            //_ => write!(f, "{:?}", self.kind),
        }
    }
}

pub type CompileResult = Result<(), CompileError>;