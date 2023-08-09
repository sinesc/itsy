use crate::prelude::*;
use crate::frontend::ast::{Positioned, Position};

/// Represents the various possible compiler error-kinds.
#[derive(Clone, Debug)]
pub enum CompileErrorKind {
    IncompatibleTraitMethod(String),
    Uninitialized(String),
    MaybeInitialized(String),
    Internal(String),
}

/// An error reported by the compiler.
#[derive(Clone, Debug)]
pub struct CompileError {
    kind: CompileErrorKind,
    position: Position,
    pub(super) module_path: String,
}

impl CompileError {
    pub(crate) fn new(item: &dyn Positioned, kind: CompileErrorKind, module_path: &str) -> CompileError {
        Self { kind, position: item.position(), module_path: module_path.to_string() }
    }
    #[cfg_attr(feature="ice_panics", allow(dead_code))]
    pub(crate) fn ice(message: String) -> CompileError {
        Self { kind: CompileErrorKind::Internal(message), position: Position(0), module_path: "".to_string() }
    }
    /// Compute 1-based line/column number in string.
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
            CompileErrorKind::IncompatibleTraitMethod(method) => write!(f, "Incompatible trait method implementation for method '{method}'"),
            CompileErrorKind::Uninitialized(variable) => write!(f, "Uninitialized variable '{variable}'"),
            CompileErrorKind::MaybeInitialized(variable) => write!(f, "Variable '{variable}' might not be initialized"),
            CompileErrorKind::Internal(msg) => write!(f, "Internal compiler error: {msg}"),
        }
    }
}

pub type CompileResult<T = ()> = Result<T, CompileError>;

/// Trait to convert an Option to a Result compatible with ResolveResult
pub(super) trait OptionToCompileError<T> {
    fn ice_msg(self: Self, message: &str) -> CompileResult<T>;
    fn ice(self: Self) -> CompileResult<T>;
}

impl<T> OptionToCompileError<T> for Option<T> {
    fn ice_msg(self: Self, message: &str) -> CompileResult<T> {
        if let Some(result) = self {
            Ok(result)
        } else {
            #[cfg(feature="ice_panics")]
            panic!("Internal compiler error: {}", message);
            #[cfg(not(feature="ice_panics"))]
            Err(CompileError {
                kind: CompileErrorKind::Internal(message.to_string()),
                position: Position(0),
                module_path: "".to_string(),
            })
        }
    }
    fn ice(self: Self) -> CompileResult<T> {
        if let Some(result) = self {
            Ok(result)
        } else {
            #[cfg(feature="ice_panics")]
            panic!("Internal compiler error: Expectation failed.");
            #[cfg(not(feature="ice_panics"))]
            Err(CompileError {
                kind: CompileErrorKind::Internal("Expectation failed.".to_string()),
                position: Position(0),
                module_path: "".to_string(),
            })
        }
    }
}