
use std::fmt::{self, Display};
use crate::ItemCount;
use crate::frontend::ast::{Position, Positioned};
use crate::shared::types::Type;
use crate::shared::numeric::Numeric;
use crate::shared::compute_loc;

pub const ICE: &'static str = "Internal compiler error";

/// Represents the various possible resolver error-kinds.
#[derive(Clone, Debug)]
pub enum ResolveErrorKind {
    TypeMismatch(Type, Type),
    NonPrimitiveCast(Type),
    IncompatibleNumeric(Type, Numeric),
    UnknownValue(String),
    UnknownMember(String),
    NumberOfArguments(ItemCount, ItemCount),
    MutabilityEscalation,
    AssignToImmutable,
    CannotResolve(String),
    InvalidOperation(String),
    Internal(String),
}

/// An error reported by the resolver (e.g. unknown/mismatching types).
#[derive(Clone, Debug)]
pub struct ResolveError {
    pub kind: ResolveErrorKind,
    pub(crate) position: Position, // this is the position from the end of the input
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
    fn unwrap_or_err(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind) -> Result<T, ResolveError>;
    fn unwrap_or_ice(self: Self, message: &str) -> Result<T, ResolveError>;
    fn some_or_err(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind) -> Result<Option<T>, ResolveError>;
    fn some_or_ice(self: Self, message: &str) -> Result<Option<T>, ResolveError>;
}

impl<T> SomeOrResolveError<T> for Option<T> {
    fn unwrap_or_err(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind) -> Result<T, ResolveError> {
        if let Some(result) = self {
            Ok(result)
        } else {
            Err(ResolveError {
                kind: kind,
                position: item.map_or(0, |i| i.position())
            })
        }
    }
    fn unwrap_or_ice(self: Self, message: &str) -> Result<T, ResolveError> {
        self.unwrap_or_err(None, ResolveErrorKind::Internal(message.to_string()))
    }
    fn some_or_err(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind) -> Result<Option<T>, ResolveError> {
        if self.is_some() {
            Ok(self)
        } else {
            Err(ResolveError {
                kind: kind,
                position: item.map_or(0, |i| i.position())
            })
        }
    }
    fn some_or_ice(self: Self, message: &str) -> Result<Option<T>, ResolveError> {
        self.some_or_err(None, ResolveErrorKind::Internal(message.to_string()))
    }
}

/// Returns an internal compiler error with positional information.
pub(crate) fn ice<T>(message: &str) -> Result<T, ResolveError> {
    Err(ResolveError {
        kind: ResolveErrorKind::Internal(message.to_string()),
        position: 0
    })
}