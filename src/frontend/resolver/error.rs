
use std::fmt::{self, Display};
use crate::frontend::ast::{Position, Positioned};
use crate::util::{Type, Numeric, compute_loc, ItemCount};

/// Represents the various possible resolver error-kinds.
#[derive(Clone, Debug)]
pub enum ResolveErrorKind {
    TypeMismatch(Type, Type),
    NonPrimitiveCast(Type),
    IncompatibleNumeric(Type, Numeric),
    UnknownValue(String),
    NumberOfArguments(ItemCount, ItemCount),
    MutabilityEscalation,
    AssignToImmutable,
    Internal,
}

/// An error reported by the resolver (e.g. unknown/mismatching types).
#[derive(Clone, Debug)]
pub struct ResolveError {
    pub kind: ResolveErrorKind,
    position: Position, // this is the position from the end of the input
}

impl ResolveError {
    pub(crate) fn new(item: &impl Positioned, kind: ResolveErrorKind) -> ResolveError {
        Self { kind: kind, position: item.position() }
    }
    /// Computes and returns the source code location of this error. Since the AST only stores byte
    /// offsets, the original source is required to recover line and column information.
    pub fn loc(self: &Self, input: &str) -> (Position, Position) {
        compute_loc(input, input.len() as Position - self.position)
    }
}

impl Display for ResolveError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ResolveErrorKind::TypeMismatch(t1, t2) => write!(f, "Incompatible types {:?} and {:?}", t1, t2),
            ResolveErrorKind::IncompatibleNumeric(t, n) => write!(f, "Incompatible numeric {:?} for expected type {:?}", n, t),
            ResolveErrorKind::NumberOfArguments(e, g) => write!(f, "Expected {} arguments, got {}", e, g),
            ResolveErrorKind::MutabilityEscalation => write!(f, "Cannot re-bind immutable reference as mutable"),
            ResolveErrorKind::AssignToImmutable => write!(f, "Cannot assign to immutable binding"),
            // Todo: handle the others
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

pub type ResolveResult = Result<(), ResolveError>;

/// Trait to convert an Option to a Result compatible with ResolveResult
pub(crate) trait SomeOrResolveError<T> {
    fn some_or(self: Self, item: &impl Positioned, kind: ResolveErrorKind) -> Result<T, ResolveError>;
}

impl<T> SomeOrResolveError<T> for Option<T> {
    fn some_or(self: Self, item: &impl Positioned, kind: ResolveErrorKind) -> Result<T, ResolveError> {
        if let Some(result) = self {
            Ok(result)
        } else {
            Err(ResolveError::new(item, kind))
        }
    }
}
