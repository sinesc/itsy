//! Compiler-known intrinsic traits (casts and operators).
//!
//! These describe traits the resolver registers and recognizes specially; see the registration loop
//! in `resolve` and the cast/operator handling in `Resolver`.

use crate::prelude::*;
use crate::frontend::ast;
use crate::frontend::resolver::error::{OptionToResolveError, ResolveResult};
use crate::shared::meta::{Type, Trait, Enum, EnumVariant, FunctionKind, ConstantValue};
use crate::shared::typed_ids::{TypeId, ConstantId};
use crate::shared::numeric::Numeric;
use super::scopes::Scopes;

/// The set of intrinsic conversion traits known to the compiler. Add a row to make a new trait drive
/// a cast (e.g. a future `Display` or `ToFloat` backing some target type).
pub(super) const INTRINSIC_CAST_TRAITS: &[IntrinsicCastTrait] = &[
    IntrinsicCastTrait { trait_name: "ToString",    method: "to_string",    method_return: Type::String, targets: &[ Type::String ] },
    IntrinsicCastTrait { trait_name: "ToUnsigned",  method: "to_unsigned",  method_return: Type::u64,    targets: &[ Type::u8, Type::u16, Type::u32, Type::u64 ] },
    IntrinsicCastTrait { trait_name: "ToSigned",    method: "to_signed",    method_return: Type::i64,    targets: &[ Type::i8, Type::i16, Type::i32, Type::i64 ] },
    IntrinsicCastTrait { trait_name: "ToFloat",     method: "to_float",     method_return: Type::f64,    targets: &[ Type::f32, Type::f64 ] },
];

/// The set of intrinsic operator traits known to the compiler. These let custom types implement the
/// binary operators; an operator applied to a type that has no built-in meaning for it is lowered to a
/// call of the corresponding trait method (e.g. `a + b` to `a.add(b)`, `a == b` to `a.eq(b)`).
/// `Eq` is keyed under `Equal` and backs both `==` and `!=`; `Ord` is keyed under `Less` and backs
/// `<`, `>`, `<=` and `>=`.
pub(super) const INTRINSIC_OP_TRAITS: &[IntrinsicOpTrait] = &[
    IntrinsicOpTrait { op: ast::BinaryOperator::Add, trait_name: "Add", method: "add", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Sub, trait_name: "Sub", method: "sub", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Mul, trait_name: "Mul", method: "mul", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Div, trait_name: "Div", method: "div", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Rem, trait_name: "Rem", method: "rem", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::BitAnd, trait_name: "BitAnd", method: "bitand", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::BitOr, trait_name: "BitOr", method: "bitor", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::BitXor, trait_name: "BitXor", method: "bitxor", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Shl, trait_name: "Shl", method: "shl", rhs: IntrinsicOpResult::Type(Type::i64), result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Shr, trait_name: "Shr", method: "shr", rhs: IntrinsicOpResult::Type(Type::i64), result: IntrinsicOpResult::SelfType },
    IntrinsicOpTrait { op: ast::BinaryOperator::Equal, trait_name: "Eq", method: "eq", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::Type(Type::bool) },
    IntrinsicOpTrait { op: ast::BinaryOperator::Less, trait_name: "Ord", method: "cmp", rhs: IntrinsicOpResult::SelfType, result: IntrinsicOpResult::Ordering },
];

/// The set of intrinsic unary operator traits known to the compiler. These let custom types implement the
/// prefix operators; a unary operator applied to a type that has no built-in meaning for it is lowered to a
/// call of the corresponding trait method (e.g. `-a` to `a.neg()`, `!a` to `a.not()`).
pub(super) const INTRINSIC_UNARY_OP_TRAITS: &[IntrinsicUnaryOpTrait] = &[
    IntrinsicUnaryOpTrait { op: ast::UnaryOperator::Minus, trait_name: "Neg", method: "neg" },
    IntrinsicUnaryOpTrait { op: ast::UnaryOperator::Not, trait_name: "Not", method: "not" },
];

/// The set of intrinsic index traits known to the compiler. Currently only `Index`.
pub(super) const INTRINSIC_INDEX_TRAITS: &[IntrinsicIndexTrait] = &[
    IntrinsicIndexTrait { trait_name: "Index", get_method: "get", set_method: "set" },
];

/// Register all intrinsic traits and the `Ordering` enum.
///
/// This covers:
/// - Conversion traits (`ToString`, `ToSigned`, `ToUnsigned`, `ToFloat`) and their cast targets,
/// - The `Ordering` enum (return type of `Ord::cmp`),
/// - Operator traits (`Add`, `Sub`, `Mul`, …, `Eq`, `Ord`),
/// - The `Error` trait,
/// - The `Index` trait.
///
/// All of these are ordinary traits/enums that scripts can implement; the compiler recognizes them
/// specially to lower casts and operators to trait-method calls.
pub(super) fn register_intrinsics(scopes: &mut Scopes, primitives: &UnorderedMap<&Type, TypeId>) -> ResolveResult<IntrinsicRegistration> {

    let mut trait_names = Vec::new();

    // Register intrinsic cast traits and the cast targets they back. These are ordinary traits that scripts
    // implement via `impl ToString for MyType { ... }`.
    let mut casts = UnorderedMap::new();
    for intrinsic in INTRINSIC_CAST_TRAITS {
        let return_type_id = *primitives.get(&intrinsic.method_return).ice()?;
        let trait_type_id = scopes.insert_type(Some(intrinsic.trait_name), Type::Trait(Trait::new()));
        // required method `fn <method>(self: Self) -> <method_return>`, registered exactly as the resolver would
        // for a source-defined trait method so impl-matching and vtable construction pick it up.
        let constant_id = scopes.insert_function(
            &format!("{}::{}", intrinsic.trait_name, intrinsic.method),
            Some(return_type_id),
            vec![ Some(trait_type_id) ],
            Some(FunctionKind::Method(trait_type_id)),
        );
        scopes.type_mut(trait_type_id).as_trait_mut().ice()?.required.insert(intrinsic.method.to_string(), Some(constant_id));
        trait_names.push(intrinsic.trait_name.to_string());
        for target in intrinsic.targets {
            let target_type_id = *primitives.get(target).ice()?;
            casts.insert(target_type_id, IntrinsicCast { trait_type_id, method: intrinsic.method, method_return_type_id: return_type_id });
        }
    }

    // Register the built-in `Ordering` enum. This is the return type of `Ord::cmp`.
    let i8_type_id = *primitives.get(&Type::i8).ice()?;
    let ordering_type_id = scopes.insert_type(Some("Ordering"), Type::Enum(Enum {
        primitive: Some((i8_type_id, Type::i8.primitive_size())),
        variants: vec![
            ("Less".to_string(), EnumVariant::Simple(Some(Numeric::Signed(-1)))),
            ("Equal".to_string(), EnumVariant::Simple(Some(Numeric::Signed(0)))),
            ("Greater".to_string(), EnumVariant::Simple(Some(Numeric::Signed(1)))),
        ],
        impl_traits: Map::new(),
    }));
    let ordering_less_constant = scopes.insert_constant("Ordering::Less", ordering_type_id, Some(ordering_type_id), ConstantValue::Discriminant(Numeric::Signed(-1)));
    let ordering_equal_constant = scopes.insert_constant("Ordering::Equal", ordering_type_id, Some(ordering_type_id), ConstantValue::Discriminant(Numeric::Signed(0)));
    let ordering_greater_constant = scopes.insert_constant("Ordering::Greater", ordering_type_id, Some(ordering_type_id), ConstantValue::Discriminant(Numeric::Signed(1)));
    let ordering = OrderingInfo {
        type_id: ordering_type_id,
        less_constant: ordering_less_constant,
        equal_constant: ordering_equal_constant,
        greater_constant: ordering_greater_constant,
    };
    trait_names.push("Ordering".to_string());

    // Register intrinsic operator traits (e.g. `Add`, `Eq`).
    let mut ops = UnorderedMap::new();
    for intrinsic in INTRINSIC_OP_TRAITS {
        let trait_type_id = scopes.insert_type(Some(intrinsic.trait_name), Type::Trait(Trait::new()));
        // required method `fn <method>(self: Self, rhs: <rhs>) -> <result>`, where rhs defaults to Self
        // (arithmetic, bitwise, equality) but can be a fixed primitive for shift operators.
        let rhs_type_id = match &intrinsic.rhs {
            IntrinsicOpResult::SelfType => trait_type_id,
            IntrinsicOpResult::Type(ty) => *primitives.get(ty).ice()?,
            IntrinsicOpResult::Ordering => ordering.type_id,
        };
        let result_type_id = match &intrinsic.result {
            IntrinsicOpResult::SelfType => trait_type_id,
            IntrinsicOpResult::Type(ty) => *primitives.get(ty).ice()?,
            IntrinsicOpResult::Ordering => ordering.type_id,
        };
        let constant_id = scopes.insert_function(
            &format!("{}::{}", intrinsic.trait_name, intrinsic.method),
            Some(result_type_id),
            vec![ Some(trait_type_id), Some(rhs_type_id) ],
            Some(FunctionKind::Method(trait_type_id)),
        );
        scopes.type_mut(trait_type_id).as_trait_mut().ice()?.required.insert(intrinsic.method.to_string(), Some(constant_id));
        trait_names.push(intrinsic.trait_name.to_string());
        ops.insert(intrinsic.op, IntrinsicOp { trait_type_id, rhs_type_id, method: intrinsic.method });
    }

    // Register intrinsic unary operator traits (`Neg`, `Not`). Each has a single required method
    // `fn <method>(self: Self) -> Self` (no rhs, result is the implementing type).
    let mut unary_ops = UnorderedMap::new();
    for intrinsic in INTRINSIC_UNARY_OP_TRAITS {
        let trait_type_id = scopes.insert_type(Some(intrinsic.trait_name), Type::Trait(Trait::new()));
        let constant_id = scopes.insert_function(
            &format!("{}::{}", intrinsic.trait_name, intrinsic.method),
            Some(trait_type_id),
            vec![ Some(trait_type_id) ],
            Some(FunctionKind::Method(trait_type_id)),
        );
        scopes.type_mut(trait_type_id).as_trait_mut().ice()?.required.insert(intrinsic.method.to_string(), Some(constant_id));
        trait_names.push(intrinsic.trait_name.to_string());
        unary_ops.insert(intrinsic.op, IntrinsicUnaryOp { trait_type_id, method: intrinsic.method });
    }

    // Register the built-in `Error` trait.
    let string_type_id = *primitives.get(&Type::String).ice()?;
    let error_trait = scopes.insert_type(Some("Error"), Type::Trait(Trait::new()));
    let error_description_id = scopes.insert_function(
        "Error::description",
        Some(string_type_id),
        vec![ Some(error_trait) ],
        Some(FunctionKind::Method(error_trait)),
    );
    scopes.type_mut(error_trait).as_trait_mut().ice()?.required.insert("description".to_string(), Some(error_description_id));
    trait_names.push("Error".to_string());

    // Register the intrinsic `Index` trait.
    // Unlike operator traits (fixed rhs/result types), Index's `get`/`set` signatures are
    // impl-defined: the index type and value type vary per implementor, so we cannot bake a fixed
    // signature into the required methods. We still register each required method with a placeholder
    // function constant (as `Error::description` and the operator traits do), because a trait whose
    // `required` entry maps to `None` is treated as forever-unresolved by `check_type_id`, which would
    // in turn stall every type that implements it. The placeholder is only a vtable-slot identity: it is
    // never called and never used for type checking. Index access never goes through virtual dispatch;
    // it is rewritten to a direct call of the concrete impl's `get`/`set` (see `intrinsic_index_methods`),
    // whose real signatures are read off the impl. The placeholder's own signature is therefore
    // irrelevant beyond being fully resolved (`fn (self: Self) -> void`).
    let mut index: Option<IntrinsicIndex> = None;
    if let Some(intrinsic) = INTRINSIC_INDEX_TRAITS.first() {
        let trait_type_id = scopes.insert_type(Some(intrinsic.trait_name), Type::Trait(Trait::new_impl_defined()));
        let get_constant_id = scopes.insert_function(
            &format!("{}::{}", intrinsic.trait_name, intrinsic.get_method),
            Some(TypeId::VOID),
            vec![ Some(trait_type_id) ],
            Some(FunctionKind::Method(trait_type_id)),
        );
        let set_constant_id = scopes.insert_function(
            &format!("{}::{}", intrinsic.trait_name, intrinsic.set_method),
            Some(TypeId::VOID),
            vec![ Some(trait_type_id) ],
            Some(FunctionKind::Method(trait_type_id)),
        );
        let trt = scopes.type_mut(trait_type_id).as_trait_mut().ice()?;
        trt.required.insert(intrinsic.get_method.to_string(), Some(get_constant_id));
        trt.required.insert(intrinsic.set_method.to_string(), Some(set_constant_id));
        trait_names.push(intrinsic.trait_name.to_string());
        index = Some(IntrinsicIndex { trait_type_id, get_method: intrinsic.get_method, set_method: intrinsic.set_method });
    }

    Ok(IntrinsicRegistration {
        trait_names,
        casts,
        ops,
        unary_ops,
        index,
        error_trait,
        ordering,
    })
}

/// All values produced by registering intrinsic traits into the scope repository.
pub(super) struct IntrinsicRegistration {
    /// Human-readable names of every intrinsic trait and type registered (e.g. `ToString`, `Add`,
    /// `Ordering`, `Error`, `Index`). Used to create aliases in each module scope.
    pub(super) trait_names     : Vec<String>,
    /// Maps a cast target type id to the intrinsic conversion trait that backs it.
    pub(super) casts           : UnorderedMap<TypeId, IntrinsicCast>,
    /// Maps a binary operator to the intrinsic operator trait that backs it.
    pub(super) ops             : UnorderedMap<ast::BinaryOperator, IntrinsicOp>,
    /// Maps a unary operator to the intrinsic operator trait that backs it.
    pub(super) unary_ops       : UnorderedMap<ast::UnaryOperator, IntrinsicUnaryOp>,
    /// The intrinsic `Index` trait that backs overloadable `[]` on custom types.
    pub(super) index           : Option<IntrinsicIndex>,
    /// Type id of the built-in `Error` trait.
    pub(super) error_trait     : TypeId,
    /// Type id and variant constants of the built-in `Ordering` enum.
    pub(super) ordering        : OrderingInfo,
}

/// Bookkeeping for the built-in `Ordering` enum: its type id and the constant ids for each variant.
#[derive(Copy, Clone)]
#[allow(dead_code)]
pub(super) struct OrderingInfo {
    pub(super) type_id         : TypeId,
    pub(super) less_constant   : ConstantId,
    pub(super) equal_constant  : ConstantId,
    pub(super) greater_constant: ConstantId,
}

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
    pub(super) rhs         : IntrinsicOpResult,
    /// Return type of the required method (`Self` or a fixed primitive).
    pub(super) result      : IntrinsicOpResult,
}

/// Return type of an intrinsic operator trait's required method: either the implementing type itself
/// (`Self`, e.g. `Add::add(self, rhs) -> Self`), a fixed primitive type (e.g. `Eq::eq(self, rhs) -> bool`),
/// or the built-in `Ordering` enum (e.g. `Ord::cmp(self, rhs) -> Ordering`).
pub(super) enum IntrinsicOpResult {
    /// The method returns `Self` (the implementing type), like the arithmetic operator traits.
    SelfType,
    /// The method returns the given (primitive) type, like `Eq` returning `bool`.
    Type(Type),
    /// The method returns the built-in `Ordering` enum (marker; resolved to real type id during registration).
    Ordering,
}

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

/// Describes an intrinsic unary operator trait that backs a prefix operator for custom types. Each has a
/// single required method `fn <method>(self: Self) -> Self` (no right operand; the result is the
/// implementing type). See the registration loop in `register_intrinsics` and `Resolver::resolve_unary_op`.
pub(super) struct IntrinsicUnaryOpTrait {
    /// The unary operator this trait backs, e.g. `Minus` for `-`.
    pub(super) op          : ast::UnaryOperator,
    /// Name of the trait scripts implement, e.g. `Neg`.
    pub(super) trait_name  : &'static str,
    /// The trait's single required method, e.g. `neg`.
    pub(super) method      : &'static str,
}


/// A resolved intrinsic unary operator: the trait that backs a prefix operator and the trait method to
/// dispatch to. Built from [IntrinsicUnaryOpTrait] once the trait has been registered.
#[derive(Copy, Clone)]
pub(super) struct IntrinsicUnaryOp {
    /// Type id of the registered trait (e.g. `Neg`).
    pub(super) trait_type_id   : TypeId,
    /// The trait's required method (e.g. `neg`).
    pub(super) method          : &'static str,
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
