//! Compiler-known intrinsic traits (casts and operators) and the bookkeeping for synthesized
//! `Result<T>` enums. These describe traits the resolver registers and recognizes specially; see the
//! registration loop in `resolve` and the cast/operator handling in `Resolver`.

use crate::frontend::ast;
use crate::shared::meta::Type;
use crate::shared::typed_ids::{TypeId, ConstantId};

/// Describes an intrinsic conversion trait that backs an `as` cast to a particular target type.
/// See the registration loop in `resolve` and `Resolver::resolve_cast`
pub(super) struct IntrinsicCastTrait {
    /// Name of the trait scripts implement, e.g. `ToString`.
    pub(super) trait_name  : &'static str,
    /// The trait's single required method, e.g. `to_string`.
    pub(super) method      : &'static str,
    /// The cast target type the trait converts to, e.g. `String`.
    pub(super) target      : Type,
}

/// The set of intrinsic conversion traits known to the compiler. Add a row to make a new trait drive
/// a cast (e.g. a future `Display` backing some target type).
pub(super) const INTRINSIC_CAST_TRAITS: &[IntrinsicCastTrait] = &[
    IntrinsicCastTrait { trait_name: "ToString", method: "to_string", target: Type::String },
];

/// A resolved intrinsic cast: the trait that backs a cast to a particular target type and the trait
/// method to dispatch to. Built from [IntrinsicCastTrait] once the trait has been registered.
#[derive(Copy, Clone)]
pub(super) struct IntrinsicCast {
    /// Type id of the registered trait (e.g. `ToString`).
    pub(super) trait_type_id   : TypeId,
    /// The trait's required method (e.g. `to_string`).
    pub(super) method          : &'static str,
}

/// Return type of an intrinsic operator trait's required method: either the implementing type itself
/// (`Self`, e.g. `Add::add(self, rhs) -> Self`) or a fixed primitive type (e.g. `Eq::eq(self, rhs) -> bool`).
pub(super) enum IntrinsicResult {
    /// The method returns `Self` (the implementing type), like the arithmetic operator traits.
    SelfType,
    /// The method returns the given (primitive) type, like `Eq` returning `bool`.
    Type(Type),
}

/// Describes an intrinsic operator trait that backs a binary operator for custom types. Each has a single
/// required method `fn <method>(self: Self, rhs: Self) -> <result>` whose return type is given by `result`
/// (`Self` for the arithmetic traits, `bool` for `Eq`). See the registration loop in `resolve`,
/// Resolver::resolve_binary_op and Resolver::resolve_assignment.
pub(super) struct IntrinsicOpTrait {
    /// The operator this trait backs, e.g. `Add` for `+`. `Eq` is keyed under `Equal` and backs both
    /// `==` and `!=` (see `Resolver::resolve_binary_op`).
    pub(super) op          : ast::BinaryOperator,
    /// Name of the trait scripts implement, e.g. `Add`.
    pub(super) trait_name  : &'static str,
    /// The trait's single required method, e.g. `add`.
    pub(super) method      : &'static str,
    /// Return type of the required method (`Self` or a fixed primitive).
    pub(super) result      : IntrinsicResult,
}

/// The set of intrinsic operator traits known to the compiler. These let custom types implement the
/// binary operators; an operator applied to a type that has no built-in meaning for it is lowered to a
/// call of the corresponding trait method (e.g. `a + b` to `a.add(b)`, `a == b` to `a.eq(b)`).
pub(super) const INTRINSIC_OP_TRAITS: &[IntrinsicOpTrait] = &[
    IntrinsicOpTrait { op: ast::BinaryOperator::Add, trait_name: "Add", method: "add", result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Sub, trait_name: "Sub", method: "sub", result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Mul, trait_name: "Mul", method: "mul", result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Div, trait_name: "Div", method: "div", result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Rem, trait_name: "Rem", method: "rem", result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Equal, trait_name: "Eq", method: "eq", result: IntrinsicResult::Type(Type::bool) },
];

/// A resolved intrinsic operator: the trait that backs an operator and the trait method to dispatch to.
/// Built from [IntrinsicOpTrait] once the trait has been registered.
#[derive(Copy, Clone)]
pub(super) struct IntrinsicOp {
    /// Type id of the registered trait (e.g. `Add`).
    pub(super) trait_type_id   : TypeId,
    /// The trait's required method (e.g. `add`).
    pub(super) method          : &'static str,
}

/// Bookkeeping for a synthesized `Result<T>` data enum, keyed by its (deduplicated) enum type id. Lets
/// the resolver recognize a type as a `Result` and bind `Ok`/`Err` calls to its variant constructors.
#[derive(Copy, Clone)]
pub(super) struct ResultTypeInfo {
    /// Success type `T` (the `Ok` payload). Read by the `?` operator.
    #[allow(dead_code)]
    pub(super) ok_type_id      : TypeId,
    /// Constructor for the `Ok(T)` variant (`FunctionKind::Variant`, index 0).
    pub(super) ok_constructor  : ConstantId,
    /// Constructor for the `Err(Error)` variant (`FunctionKind::Variant`, index 1).
    pub(super) err_constructor : ConstantId,
}
