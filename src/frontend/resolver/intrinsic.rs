//! Compiler-known intrinsic traits (casts and operators) and the bookkeeping for synthesized
//! `Result<T>` enums. These describe traits the resolver registers and recognizes specially; see the
//! registration loop in `resolve` and the cast/operator handling in `Resolver`.

use crate::frontend::ast;
use crate::shared::meta::Type;
use crate::shared::typed_ids::{TypeId, ConstantId};

/// Describes an intrinsic conversion trait that backs one or more `as` cast targets.
///
/// The trait's required method returns `method_return` (a single, fixed type). When the cast
/// target matches `method_return` exactly (e.g. `ToString` → `String`), the cast is lowered to
/// just the method call. When the target is narrower (e.g. `ToUnsigned` returns `u64` but the
/// cast is `as u8`), the method call is followed by a trailing primitive numeric cast that
/// truncates the value to the requested width.
///
/// See the registration loop in `resolve` and `Resolver::resolve_cast`.
pub(super) struct IntrinsicCastTrait {
    /// Name of the trait scripts implement, e.g. `ToString`.
    pub(super) trait_name  : &'static str,
    /// The trait's single required method, e.g. `to_string`.
    pub(super) method      : &'static str,
    /// Declared return type of the required method (e.g. `String`, `i64`, `u64`).
    pub(super) method_return : Type,
    /// Cast target types this trait backs. The method result is cast to the requested
    /// target via an ordinary primitive numeric cast when it differs from `method_return`.
    pub(super) targets       : &'static [Type],
}

/// The set of intrinsic conversion traits known to the compiler. Add a row to make a new trait drive
/// a cast (e.g. a future `Display` or `ToFloat` backing some target type).
pub(super) const INTRINSIC_CAST_TRAITS: &[IntrinsicCastTrait] = &[
    IntrinsicCastTrait { trait_name: "ToString",    method: "to_string",    method_return: Type::String, targets: &[ Type::String ] },
    IntrinsicCastTrait { trait_name: "ToUnsigned",  method: "to_unsigned",  method_return: Type::u64,    targets: &[ Type::u8, Type::u16, Type::u32, Type::u64 ] },
    IntrinsicCastTrait { trait_name: "ToSigned",    method: "to_signed",    method_return: Type::i64,    targets: &[ Type::i8, Type::i16, Type::i32, Type::i64 ] },
    IntrinsicCastTrait { trait_name: "ToFloat",     method: "to_float",     method_return: Type::f64,    targets: &[ Type::f32, Type::f64 ] },
];

/// A resolved intrinsic cast: the trait that backs a cast to a particular target type, the trait
/// method to dispatch to, and the return type of that method. Built from [IntrinsicCastTrait] once
/// the trait has been registered.
#[derive(Copy, Clone)]
pub(super) struct IntrinsicCast {
    /// Type id of the registered trait (e.g. `ToString`).
    pub(super) trait_type_id         : TypeId,
    /// The trait's required method (e.g. `to_string`).
    pub(super) method                : &'static str,
    /// Type id of the method's declared return type (e.g. `String`, `i64`, `u64`).
    pub(super) method_return_type_id : TypeId,
}

/// Return type of an intrinsic operator trait's required method: either the implementing type itself
/// (`Self`, e.g. `Add::add(self, rhs) -> Self`), a fixed primitive type (e.g. `Eq::eq(self, rhs) -> bool`),
/// or the built-in `Ordering` enum (e.g. `Ord::cmp(self, rhs) -> Ordering`).
pub(super) enum IntrinsicResult {
    /// The method returns `Self` (the implementing type), like the arithmetic operator traits.
    SelfType,
    /// The method returns the given (primitive) type, like `Eq` returning `bool`.
    Type(Type),
    /// The method returns the built-in `Ordering` enum (marker; resolved to real type id during registration).
    Ordering,
}

/// Describes an intrinsic operator trait that backs a binary operator for custom types. Each has a single
/// required method whose signature is determined by `rhs` and `result`:
/// `fn <method>(self: Self, rhs: <rhs>) -> <result>`. The right operand type defaults to `Self` (arithmetic,
/// bitwise, equality) but can be a fixed primitive for shift operators (`Shl`/`Shr` take `i64`).
/// See the registration loop in `resolve`, Resolver::resolve_binary_op and Resolver::resolve_assignment.
pub(super) struct IntrinsicOpTrait {
    /// The operator this trait backs, e.g. `Add` for `+`. `Eq` is keyed under `Equal` and backs both
    /// `==` and `!=` (see `Resolver::resolve_binary_op`).
    pub(super) op          : ast::BinaryOperator,
    /// Name of the trait scripts implement, e.g. `Add`.
    pub(super) trait_name  : &'static str,
    /// The trait's single required method, e.g. `add`.
    pub(super) method      : &'static str,
    /// Type of the right-hand operand (`Self` or a fixed primitive).
    pub(super) rhs         : IntrinsicResult,
    /// Return type of the required method (`Self` or a fixed primitive).
    pub(super) result      : IntrinsicResult,
}

/// The set of intrinsic operator traits known to the compiler. These let custom types implement the
/// binary operators; an operator applied to a type that has no built-in meaning for it is lowered to a
/// call of the corresponding trait method (e.g. `a + b` to `a.add(b)`, `a == b` to `a.eq(b)`).
/// `Eq` is keyed under `Equal` and backs both `==` and `!=`; `Ord` is keyed under `Less` and backs
/// `<`, `>`, `<=` and `>=`.
pub(super) const INTRINSIC_OP_TRAITS: &[IntrinsicOpTrait] = &[
    IntrinsicOpTrait { op: ast::BinaryOperator::Add, trait_name: "Add", method: "add", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Sub, trait_name: "Sub", method: "sub", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Mul, trait_name: "Mul", method: "mul", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Div, trait_name: "Div", method: "div", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Rem, trait_name: "Rem", method: "rem", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::BitAnd, trait_name: "BitAnd", method: "bitand", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::BitOr, trait_name: "BitOr", method: "bitor", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::BitXor, trait_name: "BitXor", method: "bitxor", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Shl, trait_name: "Shl", method: "shl", rhs: IntrinsicResult::Type(Type::i64), result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Shr, trait_name: "Shr", method: "shr", rhs: IntrinsicResult::Type(Type::i64), result: IntrinsicResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Equal, trait_name: "Eq", method: "eq", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::Type(Type::bool) },
    IntrinsicOpTrait { op: ast::BinaryOperator::Less, trait_name: "Ord", method: "cmp", rhs: IntrinsicResult::SelfType, result: IntrinsicResult::Ordering },
];

/// A resolved intrinsic operator: the trait that backs an operator and the trait method to dispatch to.
/// Built from [IntrinsicOpTrait] once the trait has been registered.
#[derive(Copy, Clone)]
pub(super) struct IntrinsicOp {
    /// Type id of the registered trait (e.g. `Add`).
    pub(super) trait_type_id   : TypeId,
    /// Type id of the right-hand operand (e.g. `Self` for arithmetic, `i64` for shift).
    pub(super) rhs_type_id     : TypeId,
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

/// Bookkeeping for a synthesized `Option<T>` enum, keyed by its (deduplicated) enum type id. Lets the
/// resolver recognize a type as an `Option` and bind `Some(x)` calls / bare `None` to its variants.
#[derive(Copy, Clone)]
pub(super) struct OptionTypeInfo {
    /// The `Some` payload type `T`.
    #[allow(dead_code)]
    pub(super) some_type_id   : TypeId,
    /// Constructor function for the `Some(T)` data variant (`FunctionKind::Variant`, index 0).
    pub(super) some_constructor : ConstantId,
    /// Unit-variant constant for the `None` simple variant (`ConstantValue::Discriminant`, index 1).
    pub(super) none_constant  : ConstantId,
}

/// Describes the intrinsic `Index` trait that backs overloadable `[]` index access on custom types.
/// Unlike operator traits (fixed `rhs`/`result`), the index and value types are impl-defined,
/// so signatures are read from the impl at resolution time.
pub(super) struct IntrinsicIndexTrait {
    /// Name of the trait scripts implement.
    pub(super) trait_name : &'static str,
    /// Name of the read method (`fn get(self: Self, index: <K>) -> <V>`).
    pub(super) get_method : &'static str,
    /// Name of the write method (`fn set(self: Self, index: <K>, value: <V>)`).
    pub(super) set_method : &'static str,
}

/// The set of intrinsic index traits known to the compiler. Currently only `Index`.
pub(super) const INTRINSIC_INDEX_TRAITS: &[IntrinsicIndexTrait] = &[
    IntrinsicIndexTrait { trait_name: "Index", get_method: "get", set_method: "set" },
];

/// A resolved intrinsic `Index` trait: the trait type id and the method names. The concrete
/// signatures (index type, value type) are read from the impl at resolution time.
#[derive(Copy, Clone)]
pub(super) struct IntrinsicIndex {
    /// Type id of the registered trait (`Index`).
    pub(super) trait_type_id : TypeId,
    /// Name of the read method.
    pub(super) get_method    : &'static str,
    /// Name of the write method.
    pub(super) set_method    : &'static str,
}
