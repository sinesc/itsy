use crate::prelude::*;
use crate::frontend::ast::{Positioned, Position};

/// Represents the various possible compiler error-kinds.
#[derive(Clone, Debug)]
pub enum CompileErrorKind {
    IncompatibleTraitMethod(String),
    Uninitialized(String),
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
        #[allow(unreachable_patterns)]
        match &self.kind {
            CompileErrorKind::IncompatibleTraitMethod(method) => write!(f, "Incompatible trait method implementation for method '{}'", method),
            CompileErrorKind::Uninitialized(variable) => write!(f, "Uninitialized variable '{}'", variable),
            CompileErrorKind::Internal => write!(f, "Internal compiler error"),
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

pub type CompileResult = Result<(), CompileError>;