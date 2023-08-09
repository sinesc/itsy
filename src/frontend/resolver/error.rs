
use crate::prelude::*;
use crate::ItemIndex;
use crate::frontend::ast::{Position, Positioned};
use crate::shared::numeric::Numeric;

/// Represents the various possible resolver error-kinds.
#[derive(Clone, Debug)]
pub enum ResolveErrorKind {
    UndefinedIdentifier(String),
    UndefinedMember(String),
    UndefinedFunction(String),
    UndefinedType(String),
    UndefinedItem(String),
    TypeMismatch(String, String),
    InvalidCast(String, String),
    IncompatibleNumeric(String, Numeric),
    NumberOfArguments(String, ItemIndex, ItemIndex),
    MutabilityEscalation,
    AssignToImmutable,
    /// Duplicate discriminant.
    DuplicateVariantValue(Numeric),
    /// Enum variant value is not an integer.
    InvalidVariantValue(Numeric),
    /// Enum variant literal is not numeric. TODO: non-numeric currently intercepted by the parser. eventually consts should be allowed here which the parser wouldn't catch
    InvalidVariantLiteral,
    NotATraitMethod(String, String),
    NotIterable(String),
    NotCallable(String),
    CannotResolve(String),
    InvalidOperation(String),
    Internal(String),
    Unsupported(String),
}

/// An error reported by the resolver (e.g. unknown/mismatching types).
#[derive(Clone, Debug)]
pub struct ResolveError {
    kind: ResolveErrorKind,
    position: Position,
    pub(super) module_path: String,
}
impl ResolveError {
    pub(crate) fn new(item: &impl Positioned, kind: ResolveErrorKind, module_path: &str) -> ResolveError {
        Self { kind, position: item.position(), module_path: module_path.to_string() }
    }
    pub(crate) fn ice(message: String) -> ResolveError {
        Self { kind: ResolveErrorKind::Internal(message), position: Position(0), module_path: "".to_string() }
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
            ResolveErrorKind::TypeMismatch(g, e) => write!(f, "Expected type {e}, got {g}"),
            ResolveErrorKind::InvalidCast(t1, t2) => write!(f, "Invalid cast from {t1} to {t2}"),
            ResolveErrorKind::IncompatibleNumeric(t, n) => write!(f, "Incompatible numeric {n} for expected type {t}"),
            ResolveErrorKind::UndefinedIdentifier(v) => write!(f, "Undefined identifier '{v}'"),
            ResolveErrorKind::UndefinedMember(m) => write!(f, "Undefined struct member '{m}'"),
            ResolveErrorKind::NumberOfArguments(func, e, g) => write!(f, "Invalid number of arguments. Function '{func}' expects {e} argument, got {g}"),
            ResolveErrorKind::MutabilityEscalation => write!(f, "Cannot re-bind immutable reference as mutable"),
            ResolveErrorKind::AssignToImmutable => write!(f, "Cannot assign to immutable binding"),
            ResolveErrorKind::UndefinedFunction(func) => write!(f, "Undefined function '{}'", func),
            ResolveErrorKind::UndefinedType(t) => write!(f, "Undefined type '{}'", t),
            ResolveErrorKind::UndefinedItem(i) => write!(f, "Undefined item '{}'", i),
            ResolveErrorKind::DuplicateVariantValue(numeric) => write!(f, "Duplicate enum variant discriminant {numeric}"),
            ResolveErrorKind::InvalidVariantValue(numeric) => write!(f, "Invalid enum variant discriminant {numeric}. Discriminants must be integer types"),
            ResolveErrorKind::InvalidVariantLiteral => write!(f, "Invalid enum variant literal"),
            ResolveErrorKind::NotATraitMethod(m, t) => write!(f, "Method '{}' may not be implemented for trait '{}' because the trait does not define it", m, t),
            ResolveErrorKind::NotIterable(t) => write!(f, "Type {t} is not iterable"),
            ResolveErrorKind::NotCallable(t) => write!(f, "Type {t} is not callable"),
            ResolveErrorKind::CannotResolve(b) => write!(f, "Cannot resolve {b}"), // fallback case
            ResolveErrorKind::InvalidOperation(o) => write!(f, "Invalid operation: {}", o),
            ResolveErrorKind::Internal(msg) => write!(f, "Internal error: {}", msg),
            ResolveErrorKind::Unsupported(msg) => write!(f, "Unsupported: {}", msg),
        }
    }
}

pub type ResolveResult<T = ()> = Result<T, ResolveError>;

/// Trait to convert an Option to a Result compatible with ResolveResult
pub(crate) trait OptionToResolveError<T> {
    fn usr(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind) -> ResolveResult<T>;
    fn ice_msg(self: Self, message: &str) -> ResolveResult<T>;
    fn ice(self: Self) -> ResolveResult<T>;
}

impl<T> OptionToResolveError<T> for Option<T> {
    fn usr(self: Self, item: Option<&dyn Positioned>, kind: ResolveErrorKind) -> ResolveResult<T> {
        if let Some(result) = self {
            Ok(result)
        } else {
            Err(ResolveError {
                kind: kind,
                position: item.map_or(Position(0), |i| i.position()),
                module_path: "".to_string(),
            })
        }
    }
    fn ice_msg(self: Self, message: &str) -> ResolveResult<T> {
        #[cfg(feature="ice_panics")]
        if self.is_none() {
            panic!("Internal compiler error: {}", message);
        }
        self.usr(None, ResolveErrorKind::Internal(message.to_string()))
    }
    fn ice(self: Self) -> ResolveResult<T> {
        #[cfg(feature="ice_panics")]
        if self.is_none() {
            panic!("Internal compiler error: Expectation failed.");
        }
        self.usr(None, ResolveErrorKind::Internal("Expectation failed.".to_string()))
    }
}
