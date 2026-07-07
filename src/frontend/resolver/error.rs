
use crate::prelude::*;
use crate::ItemIndex;
use crate::frontend::ast::{Position, Positioned};
use crate::shared::numeric::Numeric;

/// Represents the various possible resolver error-kinds.
#[derive(Clone, Debug)]
pub enum ResolveErrorKind {
    UndefinedIdentifier(String),
    UndefinedMember(String),
    /// A method (or builtin) was accessed on a receiver that does not provide it. First field is the
    /// member name, second is the receiver type name. Used for non-struct receivers (traits, trait bounds,
    /// enums, scalars), whose missing methods cannot be pinned to a single owning trait/impl, so the
    /// message names the receiver type rather than guessing a trait.
    UndefinedMethod(String, String),
    UndefinedFunction(String),
    UndefinedType(String),
    UndefinedItem(String),
    /// Expected one type, got another. Expected type is second argument.
    TypeMismatch(String, String),
    InvalidCast(String, String),
    /// A cast to a type backed by an intrinsic conversion trait (e.g. `String` via `ToString`) was attempted
    /// on a type that does not implement that trait. Arguments are the source type and the trait name.
    MissingTraitImplementation(String, String),
    /// A `Result` constructor (`Ok`/`Err`) or the `?` operator was used outside a `Result` context, so its
    /// type could not be determined. First field: whether the construct was a desugared `?`. Second field:
    /// the type the surrounding context expects instead (if known).
    ResultOutsideResultContext(bool, Option<String>),
    /// A `Some`/`None` was used outside an `Option` context so its type could not be determined.
    /// Field: the expected type if one was known.
    OptionOutsideOptionContext(Option<String>),
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
    /// A type used in a multiple-trait bound (`A + B`) is not a trait.
    NotATrait(String),
    /// A multiple-trait bound (`A + B`) lists the same trait more than once.
    DuplicateTraitBound(String),
    /// A match block does not cover all possible values; the string is a witness pattern that is left uncovered.
    NonExhaustiveMatch(String),
    /// An if-expression used as a value lacks an else branch; the string is the branch's result type.
    MissingElseBranch(String),
    NotIterable(String),
    /// A `for key[, value] in ..` loop iterates keys/indices of something that is neither a map nor an
    /// array; the string is the iterated type.
    NotKeyValueIterable(String),
    NotCallable(String),
    /// A struct or enum contains itself by value (directly or transitively), which would result in an infinitely-sized type.
    RecursiveType(String),
    /// Compound assignment through `[]` on a custom `Index` type requires `get` and `set` to share
    /// the same value type, but they differ.
    IndexCompoundAssignMismatch(String, String, String),
    /// A trait impl defines a const not declared by the trait.
    TraitConstNotDeclared(String, String),
    /// A trait impl does not define a required trait const.
    RequiredTraitConstMissing(String, String),
    /// A trait impl const type does not match the trait declaration.
    TraitConstTypeMismatch(String, String, String),
    CannotResolve(String),
    InvalidOperation(String),
    Internal(String),
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
    #[cfg(not(feature="ice_panics"))]
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
            ResolveErrorKind::TypeMismatch(g, e) => write!(f, "Expected type `{e}`, got `{g}`"),
            ResolveErrorKind::InvalidCast(t1, t2) => write!(f, "Invalid cast from `{t1}` to `{t2}`"),
            ResolveErrorKind::MissingTraitImplementation(ty, trt) => write!(f, "`{ty}` does not implement required trait `{trt}`"),
            ResolveErrorKind::ResultOutsideResultContext(from_try, expected) => {
                let context = match expected {
                    Some(ty) => format!(", but the enclosing function returns `{ty}`"),
                    None => String::new(),
                };
                if *from_try {
                    write!(f, "the `?` operator can only be used in a function that returns `Result<T>`{context}")
                } else {
                    write!(f, "`Ok`/`Err` can only be used where a `Result<T>` value is expected{context}")
                }
            },
            ResolveErrorKind::OptionOutsideOptionContext(expected) => {
                let context = match expected {
                    Some(ty) => format!(", but the enclosing context expects `{ty}`"),
                    None => String::new(),
                };
                write!(f, "`Some`/`None` can only be used where an `Option<T>` value is expected{context}")
            },
            ResolveErrorKind::IncompatibleNumeric(t, n) => write!(f, "Incompatible numeric `{n}` for expected type `{t}`"),
            ResolveErrorKind::UndefinedIdentifier(v) => write!(f, "Undefined identifier `{v}`"),
            ResolveErrorKind::UndefinedMember(m) => write!(f, "Undefined struct member `{m}`"),
            ResolveErrorKind::UndefinedMethod(m, t) => write!(f, "No method `{m}` found for type `{t}`"),
            ResolveErrorKind::NumberOfArguments(func, e, g) => write!(f, "Invalid number of arguments. Function `{func}` expects `{e}` argument, got `{g}`"),
            ResolveErrorKind::MutabilityEscalation => write!(f, "Cannot re-bind immutable reference as mutable"),
            ResolveErrorKind::AssignToImmutable => write!(f, "Cannot assign to immutable binding"),
            ResolveErrorKind::UndefinedFunction(func) => write!(f, "Undefined function `{func}`"),
            ResolveErrorKind::UndefinedType(t) => write!(f, "Undefined type `{t}`"),
            ResolveErrorKind::UndefinedItem(i) => write!(f, "Undefined item `{i}`"),
            ResolveErrorKind::DuplicateVariantValue(numeric) => write!(f, "Duplicate enum variant discriminant `{numeric}`"),
            ResolveErrorKind::InvalidVariantValue(numeric) => write!(f, "Invalid enum variant discriminant `{numeric}`. Discriminants must be integer types"),
            ResolveErrorKind::InvalidVariantLiteral => write!(f, "Invalid enum variant literal"),
            ResolveErrorKind::NotATraitMethod(m, t) => write!(f, "Method `{m}` may not be implemented for trait `{t}`. The trait does not define it"),
            ResolveErrorKind::NotATrait(t) => write!(f, "Type `{t}` used in a trait bound is not a trait"),
            ResolveErrorKind::DuplicateTraitBound(t) => write!(f, "Trait `{t}` appears more than once in a trait bound"),
            ResolveErrorKind::NonExhaustiveMatch(witness) => write!(f, "Non-exhaustive match: pattern `{witness}` not covered"),
            ResolveErrorKind::MissingElseBranch(t) => write!(f, "If-expression evaluating to type `{t}` requires an else branch"),
            ResolveErrorKind::NotIterable(t) => write!(f, "Type `{t}` is not iterable"),
            ResolveErrorKind::NotKeyValueIterable(t) => write!(f, "Cannot iterate keys/indices of `{t}`: key/index iteration (`for key, _ in ..` / `for key, value in ..`) requires a map or array"),
            ResolveErrorKind::NotCallable(t) => write!(f, "Type `{t}` is not callable"),
            ResolveErrorKind::RecursiveType(name) => write!(f, "Recursive type `{name}` has infinite size. Recursive types are not supported"),
            ResolveErrorKind::IndexCompoundAssignMismatch(ty, get_ret, set_val) => write!(f, "compound assignment through `[]` on `{ty}` requires `Index::get` to return the same type `Index::set` accepts (got `{get_ret}` vs `{set_val}`)"),
            ResolveErrorKind::TraitConstNotDeclared(name, trait_name) => write!(f, "Const `{name}` is not declared by trait `{trait_name}`"),
            ResolveErrorKind::RequiredTraitConstMissing(name, trait_name) => write!(f, "Required const `{name}` is not defined by impl of trait `{trait_name}`"),
            ResolveErrorKind::TraitConstTypeMismatch(name, expected, got) => write!(f, "Const `{name}` type mismatch in trait impl: expected `{expected}`, got `{got}`"),
            ResolveErrorKind::CannotResolve(b) => write!(f, "Cannot resolve `{b}`"), // fallback case
            ResolveErrorKind::InvalidOperation(o) => write!(f, "Invalid operation: {o}"),
            ResolveErrorKind::Internal(msg) => write!(f, "Internal error: {msg}"),
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
