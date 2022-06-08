
use crate::prelude::*;
use crate::ItemIndex;
use crate::frontend::ast::{Position, Positioned};
use crate::shared::meta::Type;
use crate::shared::numeric::Numeric;

pub const ICE: &'static str = "Internal compiler error";

/// Represents the various possible resolver error-kinds.
#[derive(Clone, Debug)]
pub enum ResolveErrorKind {
    TypeMismatch(String, String),
    NonPrimitiveCast(String),
    IncompatibleNumeric(Type, Numeric),
    UndefinedVariable(String),
    UndefinedMember(String),
    NumberOfArguments(String, ItemIndex, ItemIndex),
    MutabilityEscalation,
    AssignToImmutable,
    CannotResolve(String),
    UndefinedFunction(String),
    UndefinedType(String),
    UndefinedItem(String),
    /// Enum variant value is not an integer.
    InvalidVariantValue(Numeric),
    /// Enum variant literal is not numeric.
    InvalidVariantLiteral,
    InvalidOperation(String),
    NotATraitMethod(String, String),
    Internal(String),
    Unsupported(String),
}

/// An error reported by the resolver (e.g. unknown/mismatching types).
#[derive(Clone, Debug)]
pub struct ResolveError {
    kind: ResolveErrorKind,
    position: Position,
    module_path: String,
}

impl ResolveError {
    pub(crate) fn new(item: &impl Positioned, kind: ResolveErrorKind, module_path: &str) -> ResolveError {
        Self { kind, position: item.position(), module_path: module_path.to_string() }
    }
    /// Compute 1-based line/column number in string.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        self.position.loc(input)
    }
    /// The kind of the error.
    pub fn kind(self: &Self) -> &ResolveErrorKind {
        &self.kind
    }
    /// Path to the module where the error occured.
    pub fn module_path(self: &Self) -> &str {
        &self.module_path
    }
}

impl Display for ResolveError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[allow(unreachable_patterns)]
        match &self.kind {
            ResolveErrorKind::TypeMismatch(g, e) => write!(f, "Expected type {}, got {}", e, g),
            ResolveErrorKind::NonPrimitiveCast(t1) => write!(f, "Non-primitive cast of type {}", t1),
            ResolveErrorKind::IncompatibleNumeric(t, n) => write!(f, "Incompatible numeric {} for expected type {}", n, t),
            ResolveErrorKind::UndefinedVariable(v) => write!(f, "Undefined variable '{}'", v),
            ResolveErrorKind::UndefinedMember(m) => write!(f, "Undefined struct member '{}'", m),
            ResolveErrorKind::NumberOfArguments(func, e, g) => write!(f, "Invalid number of arguments. Function '{}' expects {} argument, got {}", func, e, g),
            ResolveErrorKind::MutabilityEscalation => write!(f, "Cannot re-bind immutable reference as mutable"),
            ResolveErrorKind::AssignToImmutable => write!(f, "Cannot assign to immutable binding"),
            ResolveErrorKind::CannotResolve(b) => write!(f, "Cannot resolve {}", b), // fallback case
            ResolveErrorKind::UndefinedFunction(func) => write!(f, "Undefined function '{}'", func),
            ResolveErrorKind::UndefinedType(t) => write!(f, "Undefined type '{}'", t),
            ResolveErrorKind::UndefinedItem(i) => write!(f, "Undefined item '{}'", i),
            ResolveErrorKind::InvalidOperation(o) => write!(f, "Invalid operation: {}", o),
            ResolveErrorKind::NotATraitMethod(m, t) => write!(f, "Method '{}' may not be implemented for trait '{}' because the trait does not define it", m, t),
            ResolveErrorKind::Internal(msg) => write!(f, "Internal error: {}", msg),
            ResolveErrorKind::Unsupported(msg) => write!(f, "Unsupported: {}", msg),
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

pub type ResolveResult = Result<(), ResolveError>;

/// Trait to convert an Option to a Result compatible with ResolveResult
pub(crate) trait SomeOrResolveError<T> {
    fn unwrap_or_err(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind, module_path: &str) -> Result<T, ResolveError>;
    fn unwrap_or_ice(self: Self, message: &str) -> Result<T, ResolveError>;
    fn some_or_err(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind, module_path: &str) -> Result<Option<T>, ResolveError>;
    fn some_or_ice(self: Self, message: &str) -> Result<Option<T>, ResolveError>;
}

impl<T> SomeOrResolveError<T> for Option<T> {
    fn unwrap_or_err(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind, module_path: &str) -> Result<T, ResolveError> {
        if let Some(result) = self {
            Ok(result)
        } else {
            Err(ResolveError {
                kind: kind,
                position: item.map_or(Position(0), |i| i.position()),
                module_path: module_path.to_string(),
            })
        }
    }
    fn unwrap_or_ice(self: Self, message: &str) -> Result<T, ResolveError> {
        self.unwrap_or_err(None, ResolveErrorKind::Internal(message.to_string()), "")
    }
    fn some_or_err(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind, module_path: &str) -> Result<Option<T>, ResolveError> {
        if self.is_some() {
            Ok(self)
        } else {
            Err(ResolveError {
                kind: kind,
                position: item.map_or(Position(0), |i| i.position()),
                module_path: module_path.to_string(),
            })
        }
    }
    fn some_or_ice(self: Self, message: &str) -> Result<Option<T>, ResolveError> {
        self.some_or_err(None, ResolveErrorKind::Internal(message.to_string()), "")
    }
}

/// Returns an internal compiler error with positional information.
pub(crate) fn ice<T>(message: &str) -> Result<T, ResolveError> {
    Err(ResolveError {
        kind: ResolveErrorKind::Internal(message.to_string()),
        position: Position(0),
        module_path: "".to_string(),
    })
}