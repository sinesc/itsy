//! Compiler-known intrinsic traits (casts and operators).
//!
//! These describe traits the resolver registers and recognizes specially; see the registration loop
//! in `resolve` and the cast/operator handling in `Resolver`.

use crate::prelude::*;
use crate::frontend::ast::{self, Typeable};
use crate::frontend::resolver::error::{OptionToResolveError, ResolveResult, ResolveError, ResolveErrorKind};
use crate::shared::meta::{Type, Trait, Enum, EnumVariant, FunctionKind, ConstantValue};
use crate::shared::typed_ids::{TypeId, ConstantId};
use crate::shared::numeric::Numeric;
use crate::shared::MetaContainer;
use super::scopes::Scopes;
use super::Resolver;

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

// -----------------------------------------------------------------------------------------------
// Resolver helper methods for intrinsic trait dispatch
// -----------------------------------------------------------------------------------------------

impl<'ctx> Resolver<'ctx> {
    
    /// Resolves a compound index-assignment `a[i] += v` on a type implementing the intrinsic `Index`
    /// trait. Sets up `CompoundDispatch::IndexMethod` with the get/set method constants and temp bindings.
    pub(super) fn resolve_index_compound_assign(self: &mut Self, item: &mut ast::Assignment, recv_type_id: TypeId, intrinsic: &IntrinsicIndex, expected_result: Option<TypeId>) -> ResolveResult {
        use ast::BinaryOperator::*;
        let base_op = item.op.arithmetic_assign_base().ice()?;

        // Look up the get and set methods.
        let (get_id, set_id) = match self.intrinsic_index_methods(recv_type_id, intrinsic) {
            Some(pair) => pair,
            None => {
                // Methods not yet resolved; defer to a later pass.
                // Resolve the right side without type constraints.
                self.resolve_expression(&mut item.right, None)?;
                item.type_id = Some(TypeId::VOID);
                return self.types_resolved(item, expected_result);
            }
        };

        // Get the get method's return type, index param type, and set method's value param type.
        // Extract all values before any mutable borrows.
        let get_function_id = self.scopes.constant_function_id(get_id).ice()?;
        let get_ret_type_id = self.scopes.function_ref(get_function_id).ret_type_id(self).ice()?;
        let get_index_type_id = self.scopes.function_ref(get_function_id).arg_type_ids(self).get(1).and_then(|t| *t).ice()?;

        let set_function_id = self.scopes.constant_function_id(set_id).ice()?;
        // set's value parameter is the third (index 2: self, index, value)
        let set_value_type_id = self.scopes.function_ref(set_function_id).arg_type_ids(self).get(2).and_then(|t| *t).ice()?;

        // For compound assignment, get's return type must match set's value type.
        if !self.type_equals(get_ret_type_id, set_value_type_id) {
            if self.stage.must_resolve() {
                let type_name = self.type_name(recv_type_id);
                let get_ret_name = self.type_name(get_ret_type_id);
                let set_val_name = self.type_name(set_value_type_id);
                return Err(ResolveError::new(item, ResolveErrorKind::IndexCompoundAssignMismatch(type_name, get_ret_name, set_val_name), self.module_path));
            }
            self.resolve_expression(&mut item.right, None)?;
            item.type_id = Some(TypeId::VOID);
            return self.types_resolved(item, expected_result);
        }

        // Resolve the right-hand value expression against the value type.
        self.resolve_expression(&mut item.right, Some(set_value_type_id))?;

        // Determine the operator dispatch: is the value type a custom type needing trait dispatch,
        // or a built-in primitive?
        let value_type = self.type_by_id(set_value_type_id);
        let is_bitwise_shift = matches!(base_op, BitAnd | BitOr | BitXor | Shl | Shr);
        let is_builtin_value = if is_bitwise_shift {
            value_type.is_integer()
        } else {
            value_type.is_numeric() || value_type.is_string()
        };
        // A built-in value type uses the arithmetic/bitwise opcodes directly; a custom value type must
        // implement the matching operator trait (e.g. `Add`), dispatched like a standalone `a + b`.
        let op_method = if is_builtin_value {
            None
        } else {
            let op_intrinsic = self.intrinsic_ops.get(&base_op).copied().ice()?;
            match self.intrinsic_op_method_constant(set_value_type_id, &op_intrinsic) {
                Some(constant_id) => Some(constant_id),
                None if self.stage.must_resolve() => {
                    let type_name = self.type_name(set_value_type_id);
                    let trait_name = self.type_name(op_intrinsic.trait_type_id);
                    return Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path));
                },
                None => {
                    // operator-trait impl not resolved yet; defer to a later pass.
                    self.resolve_expression(&mut item.right, None)?;
                    item.type_id = Some(TypeId::VOID);
                    return self.types_resolved(item, expected_result);
                },
            }
        };

        // Type the temp bindings (recv and idx).
        let bin = item.left.as_binary_op_mut().ice()?;
        // Type the receiver expression.
        self.set_type_id(bin.left.as_expression_mut().ice()?, recv_type_id)?;
        // Resolve and type the index expression against get's index parameter.
        self.resolve_expression(bin.right.as_expression_mut().ice()?, Some(get_index_type_id))?;
        self.set_type_id(bin.right.as_expression_mut().ice()?, get_index_type_id)?;
        // Type the `IndexWrite` node itself with the indexed value type (get's return == set's value), so the
        // assignment's `left` operand counts as resolved (see BinaryOp::resolvables).
        let bin = item.left.as_binary_op_mut().ice()?;
        bin.type_id = Some(set_value_type_id);

        // Set the temp binding types.
        if let Some(recv_binding) = item.temp_recv {
            self.scopes.binding_mut(recv_binding).type_id = Some(recv_type_id);
        }
        if let Some(idx_binding) = item.temp_idx {
            self.scopes.binding_mut(idx_binding).type_id = Some(get_index_type_id);
        }

        // Set up the dispatch.
        item.op_dispatch = Some(ast::CompoundDispatch::IndexMethod {
            get_method: get_id,
            set_method: set_id,
            op_method,
            recv: item.temp_recv.ice()?,
            idx: item.temp_idx.ice()?,
        });

        item.type_id = Some(TypeId::VOID);
        self.types_resolved(item, expected_result)
    }

    /// Looks up the constant for an intrinsic operator trait's method on the given target type, used to
    /// dispatch a compound assignment in-place. Concrete implementors resolve to their own impl method
    /// (static dispatch); a trait-object target resolves to the trait's required method (virtual dispatch).
    /// Returns `None` while the implementation is not yet known.
    pub(super) fn intrinsic_op_method_constant(self: &Self, type_id: TypeId, intrinsic: &IntrinsicOp) -> Option<ConstantId> {
        if let Some(impl_traits) = self.type_by_id(type_id).impl_traits_map() {
            if let Some(impl_trait) = impl_traits.get(&intrinsic.trait_type_id) {
                if let Some(Some(constant_id)) = impl_trait.functions.get(intrinsic.method) {
                    return Some(*constant_id);
                }
            }
        }
        if self.type_by_id(type_id).is_trait_object() {
            if let Some(trt) = self.type_by_id(intrinsic.trait_type_id).as_trait() {
                if let Some(Some(constant_id)) = trt.required.get(intrinsic.method) {
                    return Some(*constant_id);
                }
            }
        }
        None
    }

    /// Looks up the `get` and `set` method constants for the intrinsic `Index` trait on the given
    /// target type. Returns both constants when the impl is fully resolved.
    pub(super) fn intrinsic_index_methods(self: &Self, type_id: TypeId, intrinsic: &IntrinsicIndex) -> Option<(ConstantId, ConstantId)> {
        if let Some(impl_traits) = self.type_by_id(type_id).impl_traits_map() {
            if let Some(impl_trait) = impl_traits.get(&intrinsic.trait_type_id) {
                if let (Some(Some(get_id)), Some(Some(set_id))) = (
                    impl_trait.functions.get(intrinsic.get_method),
                    impl_trait.functions.get(intrinsic.set_method),
                ) {
                    return Some((*get_id, *set_id));
                }
            }
        }
        None
    }

    /// Builds an unresolved method-call expression `receiver.method(args...)` for the resolver to lower
    /// intrinsic-trait operations (casts, operators) onto the regular method-call machinery.
    pub(super) fn make_method_call(receiver: ast::Expression, method: &str, args: Vec<ast::Expression>, position: ast::Position) -> ast::Expression {
        let access = ast::Expression::BinaryOp(Box::new(ast::BinaryOp {
            position,
            op: ast::BinaryOperator::Access,
            left: ast::BinaryOperand::Expression(receiver),
            right: ast::BinaryOperand::Member(ast::Member {
                position,
                ident: ast::Ident { position, name: method.to_string() },
                type_id: None,
                constant_id: None,
            }),
            type_id: None,
            op_resolved: false,
        }));
        ast::Expression::BinaryOp(Box::new(ast::BinaryOp {
            position,
            op: ast::BinaryOperator::Call,
            left: ast::BinaryOperand::Expression(access),
            right: ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args }),
            type_id: None,
            op_resolved: false,
        }))
    }

    /// Attempts to dispatch a unary operator on a custom type through its intrinsic operator trait.
    /// Returns `true` (after rewriting `item` to the trait-method call) once the operand type is
    /// known to implement the trait; `false` while the implementation is not yet known (so resolution
    /// retries on a later pass). In the final `must_resolve` pass a still-missing impl is an error.
    pub(super) fn dispatch_unary_op(self: &mut Self, item: &mut ast::Expression, op: ast::UnaryOperator, operand_type_id: TypeId, expected_type: Option<TypeId>) -> ResolveResult<bool> {
        let intrinsic = self.intrinsic_unary_ops.get(&op).copied().ice()?;
        if self.type_accepted_for(operand_type_id, intrinsic.trait_type_id) {
            self.rewrite_unary_op_to_method(item, intrinsic.method, expected_type)?;
            Ok(true)
        } else if self.stage.must_resolve() {
            let type_name = self.type_name(operand_type_id);
            let trait_name = self.type_name(intrinsic.trait_type_id);
            Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path))
        } else {
            Ok(false)
        }
    }

    /// Rewrites a cast expression `expr as T` into a method call `expr.<method>()` and resolves it. Used to
    /// dispatch casts backed by an intrinsic conversion trait (e.g. `ToString`) through the regular method
    /// call machinery, which handles both static dispatch and dynamic dispatch on trait objects.
    pub(super) fn rewrite_cast_to_method(self: &mut Self, item: &mut ast::Expression, method: &str, expected_result: Option<TypeId>) -> ResolveResult {
        let cast = item.as_cast_mut().ice()?;
        let position = cast.position;
        let receiver = std::mem::replace(&mut cast.expr, ast::Expression::void(position));
        *item = Self::make_method_call(receiver, method, Vec::new(), position);
        self.resolve_expression(item, expected_result)
    }

    /// Rewrites a unary operation `OP expr` into a method call `expr.<method>()` and resolves it. Used to
    /// dispatch a prefix operator backed by an intrinsic unary operator trait (e.g. `Neg`) through the
    /// regular method-call machinery, reusing its static and dynamic dispatch. Mirrors
    /// `rewrite_cast_to_method` (the receiver is the operand and the method takes no arguments).
    pub(super) fn rewrite_unary_op_to_method(self: &mut Self, item: &mut ast::Expression, method: &str, expected_result: Option<TypeId>) -> ResolveResult {
        let unary = item.as_unary_op_mut().ice()?;
        let position = unary.position;
        let receiver = std::mem::replace(&mut unary.expr, ast::Expression::void(position));
        *item = Self::make_method_call(receiver, method, Vec::new(), position);
        self.resolve_expression(item, expected_result)
    }

    /// Lowers `expr as <intN>` for a type whose intrinsic conversion trait returns a wider integer
    /// (`ToSigned`/`ToUnsigned`/`ToFloat`). Replaces the cast's operand with the trait method call (`expr.to_signed()`
    /// and reclassifies the surviving `Cast` as a built-in primitive numeric cast from the method's
    /// return type to the requested width. Mirrors `rewrite_cast_to_method` but keeps the cast node.
    pub(super) fn rewrite_cast_via_method(self: &mut Self, item: &mut ast::Expression, method: &str, to_type_id: TypeId) -> ResolveResult {
        let cast = item.as_cast_mut().ice()?;
        let position = cast.position;
        let receiver = std::mem::replace(&mut cast.expr, ast::Expression::void(position));
        cast.expr = Self::make_method_call(receiver, method, Vec::new(), position);
        self.resolve_expression(&mut item.as_cast_mut().ice()?.expr, None)?;   // resolves to i64/u64
        let cast = item.as_cast_mut().ice()?;
        cast.set_type_id(self, to_type_id);
        cast.kind = Some(ast::CastKind::Primitive);
        Ok(())
    }

    /// Rewrites an arithmetic binary operation `a OP b` into a method call `a.<method>(b)` and resolves it.
    /// Used to dispatch operators backed by an intrinsic operator trait (e.g. `Add`) through the regular
    /// method-call machinery, reusing its static and dynamic dispatch.
    pub(super) fn rewrite_binary_op_to_method(self: &mut Self, item: &mut ast::BinaryOp, method: &str, expected_result: Option<TypeId>) -> ResolveResult {
        let position = item.position;
        let receiver = match std::mem::replace(&mut item.left, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator left operand is not an expression"),
        };
        let rhs = match std::mem::replace(&mut item.right, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator right operand is not an expression"),
        };
        match Self::make_method_call(receiver, method, vec![ rhs ], position) {
            ast::Expression::BinaryOp(call) => *item = *call,
            _ => return Self::ice("make_method_call did not produce a binary operation"),
        }
        self.resolve_binary_op(item, expected_result)
    }

    /// Rewrites an equality comparison `a == b` / `a != b` on a type implementing the intrinsic `Eq` trait
    /// into a call of its `eq` method and resolves it. `==` lowers to `a.eq(b)`; `!=` lowers to the negated
    /// form `a.eq(b) == false`, which the built-in boolean comparison then evaluates. Mirrors
    /// `rewrite_binary_op_to_method` (used for the arithmetic operator traits) but accounts for `eq`
    /// returning `bool` rather than `Self`.
    pub(super) fn rewrite_eq_op_to_method(self: &mut Self, item: &mut ast::BinaryOp, method: &str, negate: bool, expected_result: Option<TypeId>) -> ResolveResult {
        let position = item.position;
        let receiver = match std::mem::replace(&mut item.left, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator left operand is not an expression"),
        };
        let rhs = match std::mem::replace(&mut item.right, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator right operand is not an expression"),
        };
        let call = Self::make_method_call(receiver, method, vec![ rhs ], position);
        if negate {
            // `a != b` becomes `a.eq(b) == false`: comparing the boolean `eq` result against `false`
            // negates it through the built-in boolean comparison (the `false` operand is a primitive, so
            // this re-resolves as a regular comparison and is not itself dispatched through `Eq`).
            let false_literal = ast::Expression::Literal(ast::Literal {
                position,
                value: ast::LiteralValue::Bool(false),
                type_name: None,
                type_id: None,
            });
            item.op = ast::BinaryOperator::Equal;
            item.left = ast::BinaryOperand::Expression(call);
            item.right = ast::BinaryOperand::Expression(false_literal);
            item.type_id = None;
            item.op_resolved = false;
        } else {
            match call {
                ast::Expression::BinaryOp(call) => *item = *call,
                _ => return Self::ice("make_method_call did not produce a binary operation"),
            }
        }
        self.resolve_binary_op(item, expected_result)
    }

    /// Rewrites an ordering comparison (`<`, `>`, `<=`, `>=`) on a type implementing the intrinsic `Ord`
    /// trait into `a.cmp(b) <op> Ordering::<variant>`. The lowering is:
    /// - `a < b`  -> `a.cmp(b) == Ordering::Less`
    /// - `a > b`  -> `a.cmp(b) == Ordering::Greater`
    /// - `a <= b` -> `a.cmp(b) != Ordering::Greater`
    /// - `a >= b` -> `a.cmp(b) != Ordering::Less`
    /// The `Ordering::Variant` constant is built as an AST Constant with `constant_id` preset (so
    /// `resolve_constant` short-circuits), then the outer `==`/`!=` is re-resolved as a regular
    /// comparison against the primitive enum `Ordering`.
    pub(super) fn rewrite_ord_op_to_method(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        let position = item.position;
        let receiver = match std::mem::replace(&mut item.left, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator left operand is not an expression"),
        };
        let rhs = match std::mem::replace(&mut item.right, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator right operand is not an expression"),
        };
        let cmp_call = Self::make_method_call(receiver, "cmp", vec![ rhs ], position);
        // Determine the target comparison operator and Ordering variant constant based on the original op
        let (cmp_op, variant_name, constant_id) = match item.op {
            ast::BinaryOperator::Less => (ast::BinaryOperator::Equal, "Less", self.ordering.less_constant),
            ast::BinaryOperator::Greater => (ast::BinaryOperator::Equal, "Greater", self.ordering.greater_constant),
            ast::BinaryOperator::LessOrEq => (ast::BinaryOperator::NotEqual, "Greater", self.ordering.greater_constant),
            ast::BinaryOperator::GreaterOrEq => (ast::BinaryOperator::NotEqual, "Less", self.ordering.less_constant),
            _ => return Self::ice("rewrite_ord_op_to_method called with non-ordering operator"),
        };
        // Build the Ordering::<variant> constant expression with preset constant_id
        let ordering_constant = ast::Expression::Constant(ast::Constant {
            position,
            path: ast::Path {
                position,
                segments: vec![
                    ast::Ident { position, name: "Ordering".to_string() },
                    ast::Ident { position, name: variant_name.to_string() },
                ],
            },
            constant_id: Some(constant_id),
            type_parameter: None,
        });
        item.op = cmp_op;
        item.left = ast::BinaryOperand::Expression(cmp_call);
        item.right = ast::BinaryOperand::Expression(ordering_constant);
        item.type_id = None;
        item.op_resolved = false;
        self.resolve_binary_op(item, expected_result)
    }

    /// Rewrites a shift operation `a << b` / `a >> b` on a type implementing the intrinsic `Shl`/`Shr` trait
    /// into a call of its `shl`/`shr` method. Mirrors `rewrite_binary_op_to_method` (used for the arithmetic
    /// operator traits) but the shift amount (right operand) is already resolved as an independent integer type.
    pub(super) fn rewrite_shift_op_to_method(self: &mut Self, item: &mut ast::BinaryOp, method: &str, expected_result: Option<TypeId>) -> ResolveResult {
        let position = item.position;
        let receiver = match std::mem::replace(&mut item.left, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator left operand is not an expression"),
        };
        let rhs = match std::mem::replace(&mut item.right, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator right operand is not an expression"),
        };
        match Self::make_method_call(receiver, method, vec![ rhs ], position) {
            ast::Expression::BinaryOp(call) => *item = *call,
            _ => return Self::ice("make_method_call did not produce a binary operation"),
        }
        self.resolve_binary_op(item, expected_result)
    }

    /// Rewrites an index read `a[i]` on a type implementing the intrinsic `Index` trait into a call of
    /// its `get` method: `a.get(i)`. Mirrors `rewrite_binary_op_to_method` but uses the Index trait.
    pub(super) fn rewrite_index_read_to_method(self: &mut Self, item: &mut ast::BinaryOp, intrinsic: IntrinsicIndex, expected_result: Option<TypeId>) -> ResolveResult {
        let position = item.position;
        let receiver = match std::mem::replace(&mut item.left, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator left operand is not an expression"),
        };
        let idx = match std::mem::replace(&mut item.right, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator right operand is not an expression"),
        };
        match Self::make_method_call(receiver, intrinsic.get_method, vec![ idx ], position) {
            ast::Expression::BinaryOp(call) => *item = *call,
            _ => return Self::ice("make_method_call did not produce a binary operation"),
        }
        self.resolve_binary_op(item, expected_result)
    }

    /// Rewrites a plain index-write assignment `a[i] = v` on a type implementing the intrinsic `Index`
    /// trait into a call of its `set` method: `a.set(i, v)`. The whole `Assignment` expression is
    /// replaced with the method-call expression.
    pub(super) fn rewrite_index_write_to_method(self: &mut Self, item: &mut ast::Expression, intrinsic: IntrinsicIndex, expected_result: Option<TypeId>) -> ResolveResult {
        let assignment = item.as_assignment_mut().ice()?;
        let position = assignment.position;
        // Extract receiver and index from the IndexWrite left side.
        let bin = assignment.left.as_binary_op_mut().ice()?;
        let receiver = match std::mem::replace(&mut bin.left, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("index-write left operand is not an expression"),
        };
        let idx = match std::mem::replace(&mut bin.right, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("index-write right operand is not an expression"),
        };
        let value = std::mem::replace(&mut assignment.right, ast::Expression::void(position));
        // Replace the entire assignment expression with `a.set(i, v)`.
        *item = Self::make_method_call(receiver, intrinsic.set_method, vec![ idx, value ], position);
        // Re-resolve the new call expression.
        self.resolve_expression(item, expected_result)
    }
}
