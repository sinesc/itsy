//! Synthesis of compiler-generated types: `Result<T>`, `Option<T>`, and `Generator<K, V>`.
//!
//! These types are not declared in source; the resolver creates their underlying enum/struct
//! definitions on first use and deduplicates structurally identical instantiations.

use crate::prelude::*;
use crate::frontend::ast::{self, Typeable, Resolvable};
use crate::frontend::resolver::error::{ResolveResult, ResolveError, ResolveErrorKind, OptionToResolveError};
use crate::shared::meta::{Type, Enum, EnumVariant, Struct, FunctionKind, ConstantValue};
use crate::shared::typed_ids::{TypeId, ConstantId};
use crate::shared::numeric::Numeric;
use crate::shared::MetaContainer;
use crate::VariantIndex;
use super::Resolver;

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

/// Core type synthesis methods: create the anonymous enum/struct definitions for `Result<T>`,
/// `Option<T>`, and `Generator<K, V>` on first use, deduplicating structurally identical types.
impl<'ctx> Resolver<'ctx> {

    /// Returns the type id of the `Result<T>` data enum for the given success type, synthesizing it on
    /// first use. `Result<T>` is represented as an anonymous data enum with variants `Ok(T)` and
    /// `Err(Error)`; structurally identical results (same `T`) dedupe to a single type id. On first
    /// synthesis the two variant constructors are created and recorded in `result_types`.
    fn synthesize_result_type(self: &mut Self, ok_type_id: TypeId) -> TypeId {
        let error_trait_type_id = self.error_trait_type_id;
        let variants = vec![
            ("Ok".to_string(), EnumVariant::Data(vec![ Some(ok_type_id) ])),
            ("Err".to_string(), EnumVariant::Data(vec![ Some(error_trait_type_id) ])),
        ];
        // primitive: None -> data-carrying (heap) enum, like a source enum with data variants
        let ty = Type::Enum(Enum { primitive: None, variants, impl_traits: Map::new() });
        let result_type_id = self.scopes.insert_anonymous_type(true, ty);
        if !self.result_types.contains_key(&result_type_id) {
            // names embed the enum type id to stay unique across distinct `Result<T>`; they are never
            // looked up by name (callers bind to the constructor's constant id directly).
            let raw = result_type_id.into_usize();
            let ok_constructor = self.scopes.insert_function(
                &format!("Result#{raw}::Ok"), Some(result_type_id), vec![ Some(ok_type_id) ],
                Some(FunctionKind::Variant(result_type_id, 0)),
            );
            let err_constructor = self.scopes.insert_function(
                &format!("Result#{raw}::Err"), Some(result_type_id), vec![ Some(error_trait_type_id) ],
                Some(FunctionKind::Variant(result_type_id, 1)),
            );
            self.result_types.insert(result_type_id, ResultTypeInfo { ok_type_id, ok_constructor, err_constructor });
        }
        result_type_id
    }

    /// Returns the type id of the `Option<T>` enum for the given inner type, synthesizing it on first
    /// use. `Option<T>` is represented as an anonymous mixed enum with variants `Some(T)` (data) and
    /// `None` (simple/unit); structurally identical options (same `T`) dedupe to a single type id.
    /// On first synthesis the `Some` constructor function and the `None` unit-variant constant are
    /// created and recorded in `option_types`.
    pub(crate) fn synthesize_option_type(self: &mut Self, some_type_id: TypeId) -> TypeId {
        let variants = vec![
            ("Some".to_string(), EnumVariant::Data(vec![ Some(some_type_id) ])),
            ("None".to_string(), EnumVariant::Simple(None)),
        ];
        // primitive: None -> data-carrying (heap) enum, since the Some variant carries data
        let ty = Type::Enum(Enum { primitive: None, variants, impl_traits: Map::new() });
        let option_type_id = self.scopes.insert_anonymous_type(true, ty);
        if !self.option_types.contains_key(&option_type_id) {
            // mangled, never looked up by name: callers bind to the constant/constructor id directly
            let raw = option_type_id.into_usize();
            let some_constructor = self.scopes.insert_function(
                &format!("Option#{raw}::Some"), Some(option_type_id), vec![ Some(some_type_id) ],
                Some(FunctionKind::Variant(option_type_id, 0)),
            );
            // None is a unit variant: a constant, like a user enum's `VariantKind::Simple`. The numeric is
            // a placeholder (reference-enum codegen tags by variant index, ignoring it).
            let none_constant = self.scopes.insert_constant(
                &format!("Option#{raw}::None"), option_type_id, Some(option_type_id),
                ConstantValue::Discriminant(Numeric::Unsigned(1)),
            );
            self.option_types.insert(option_type_id, OptionTypeInfo {
                some_type_id,
                some_constructor,
                none_constant,
            });
        }
        option_type_id
    }

    /// Returns the type id of the `Generator<V>` / `Generator<K, V>` carrier for the given key/value
    /// types, synthesizing it on first use. The carrier is an anonymous struct with sentinel fields
    /// `$value` (and `$key` for the keyed form); the `$` keeps the field names unwritable by user
    /// code, so the structural `generator_signature` check can identify it. Structurally identical
    /// generators (same key/value) dedupe to a single type id, like `Result<T>`. The frozen frame the
    /// carrier will hold at runtime is *not* modelled as typed fields here - that stays opaque bytes.
    fn synthesize_generator_type(self: &mut Self, key_type_id: Option<TypeId>, value_type_id: TypeId) -> TypeId {
        let mut fields = Map::new();
        fields.insert("$value".to_string(), Some(value_type_id));
        if let Some(key_type_id) = key_type_id {
            fields.insert("$key".to_string(), Some(key_type_id));
        }
        let ty = Type::Struct(Struct { fields, impl_traits: Map::new() });
        self.scopes.insert_anonymous_type(true, ty)
    }

    /// Resolves a result definition `Result<T>` into its synthesized two-variant data enum `Ok(T)` / `Err(Error)`.
    pub(super) fn resolve_result_type(self: &mut Self, item: &mut ast::ResultDef) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        if let (None, Some(ok_type_id)) = (item.type_id, self.resolve_inline_type(&mut item.ok_type)?) {
            item.type_id = Some(self.synthesize_result_type(ok_type_id));
        }
        self.types_resolved(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves `Option<T>` into its synthesized `Some(T)` / `None` enum.
    pub(super) fn resolve_option_type(self: &mut Self, item: &mut ast::OptionDef) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        if let (None, Some(some_type_id)) = (item.type_id, self.resolve_inline_type(&mut item.some_type)?) {
            item.type_id = Some(self.synthesize_option_type(some_type_id));
        }
        self.types_resolved(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves a generator definition `Generator<V>` / `Generator<K, V>` into its synthesized struct carrier.
    pub(super) fn resolve_generator_type(self: &mut Self, item: &mut ast::GeneratorDef) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        // resolve the value type and the optional key type first
        let value_type_id = self.resolve_inline_type(&mut item.value_type)?;
        let key_type_id = match &mut item.key_type {
            Some(key_type) => self.resolve_inline_type(key_type)?,
            None => None,
        };
        // synthesize once the value (and the key, if this generator has one) are resolved
        if item.type_id.is_none() {
            if let Some(value_type_id) = value_type_id {
                if item.key_type.is_none() || key_type_id.is_some() {
                    item.type_id = Some(self.synthesize_generator_type(key_type_id, value_type_id));
                }
            }
        }
        self.types_resolved(item, None)?;
        Ok(item.type_id)
    }

    /// If the call target is a bare, still-unbound `Ok`/`Err`, bind it to the matching `Result<T>` variant
    /// constructor so the generic call resolution below handles argument checking and the result type.
    /// `T` comes from the expected result type when available, otherwise (for `Ok`) is inferred from the
    /// argument. `Err` without an expected `Result` type cannot infer `T` and is left unbound (reported as
    /// an undefined identifier in the final stage).
    pub(super) fn try_bind_result_constructor(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        let variant = match item.left.as_expression().and_then(|e| e.as_constant()) {
            Some(c) if c.constant_id.is_none() && c.path.segments.len() == 1 => match c.path.segments[0].name.as_str() {
                "Ok" => 0 as VariantIndex,
                "Err" => 1 as VariantIndex,
                _ => return Ok(()),
            },
            _ => return Ok(()),
        };
        // an expected result type pins T directly; this is the common case (`return Ok(x)`, match arms, ...)
        let expected_result_type = expected_result.filter(|t| self.result_types.contains_key(t));
        let result_type_id = match (variant, expected_result_type) {
            (_, Some(result_type_id)) => Some(result_type_id),
            // Ok(x): infer T from the argument's type
            (0, None) => {
                if let Some(arg) = item.right.as_argument_list_mut().ice()?.args.first_mut() {
                    self.resolve_expression(arg, None)?;
                    arg.type_id(self).map(|ok_type_id| self.synthesize_result_type(ok_type_id))
                } else {
                    None
                }
            },
            // Err(e): cannot determine T from the error alone; wait for / require context
            (_, None) => None,
        };
        if let Some(result_type_id) = result_type_id {
            let info = *self.result_types.get(&result_type_id).ice()?;
            let constructor = if variant == 0 { info.ok_constructor } else { info.err_constructor };
            item.left.as_expression_mut().ice()?.as_constant_mut().ice()?.constant_id = Some(constructor);
        } else if self.stage.must_resolve() {
            // the constructor could not be bound to any `Result<T>`: there is no result type to infer from
            // (e.g. `?` or a bare `Err`/`Ok` in a function that does not return `Result`). Report this
            // directly instead of letting the unbound `Ok`/`Err` surface as a misleading "undefined identifier".
            let from_try = item.right.as_argument_list().and_then(|a| a.args.first())
                .and_then(|arg| arg.as_variable())
                .map_or(false, |var| var.ident.name == "try$err" || var.ident.name == "try$ok");
            let expected = expected_result.map(|type_id| self.type_name(type_id));
            return Err(ResolveError::new(item, ResolveErrorKind::ResultOutsideResultContext(from_try, expected), self.module_path));
        }
        Ok(())
    }

    /// If the call target is a bare, still-unbound `Some`, bind it to the matching `Option<T>` variant
    /// constructor so the generic call resolution below handles argument checking and the result type.
    /// `T` comes from the expected result type when available, otherwise is inferred from the argument.
    /// `None` is a bare constant (not a call) and is handled in `resolve_constant`.
    pub(super) fn try_bind_option_constructor(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        // only `Some(x)` is a call; `None` is a bare unit-variant constant, bound in resolve_constant
        let is_some = matches!(
            item.left.as_expression().and_then(|e| e.as_constant()),
            Some(c) if c.constant_id.is_none() && c.path.segments.len() == 1 && c.path.segments[0].name == "Some"
        );
        if !is_some {
            return Ok(());
        }
        // an expected Option type pins T directly; otherwise infer T from the argument
        let option_type_id = match expected_result.filter(|t| self.option_types.contains_key(t)) {
            Some(option_type_id) => Some(option_type_id),
            None => {
                if let Some(arg) = item.right.as_argument_list_mut().ice()?.args.first_mut() {
                    self.resolve_expression(arg, None)?;
                    arg.type_id(self).map(|some_type_id| self.synthesize_option_type(some_type_id))
                } else {
                    None
                }
            }
        };
        if let Some(option_type_id) = option_type_id {
            let info = *self.option_types.get(&option_type_id).ice()?;
            item.left.as_expression_mut().ice()?.as_constant_mut().ice()?.constant_id = Some(info.some_constructor);
        }
        Ok(())
    }
}
