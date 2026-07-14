//! Resolution of binary operations: arithmetic, comparison, logical, indexing, member access, and calls.
//!
//! Each binary operator category gets its own method; the main `resolve_binary_op` dispatches to them.

use crate::frontend::ast::{self, Typeable};
use crate::frontend::resolver::error::{ResolveResult, ResolveError, ResolveErrorKind, OptionToResolveError};
use crate::shared::meta::{Type, MapType, Array};
use crate::shared::typed_ids::TypeId;
use crate::shared::MetaContainer;
use super::Resolver;

/// Core binary-operation resolution methods.
impl<'ctx> Resolver<'ctx> {

    /// Resolve both operands of a binary op, compute the common type, and apply it to the node and both operands.
    /// Returns the common type id if available.
    pub(super) fn resolve_binary_operands(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult<Option<TypeId>> {
        self.resolve_expression(item.left.as_expression_mut().ice()?, expected_result)?;
        let left_type_id = item.left.type_id(self);
        self.resolve_expression(item.right.as_expression_mut().ice()?, left_type_id)?;
        let right_type_id = item.right.type_id(self);
        let common_type_id = item.type_id(self).or(left_type_id).or(right_type_id);
        if let Some(ct) = common_type_id {
            self.set_type_id(item, ct)?;
            self.set_type_id(item.left.as_expression_mut().ice()?, ct)?;
            self.set_type_id(item.right.as_expression_mut().ice()?, ct)?;
        }
        Ok(common_type_id)
    }

    /// Check if `common_type` uses a builtin operator or dispatches through a trait.
    /// Returns `true` if the op is fully resolved (builtin path).
    pub(super) fn check_op_dispatch(self: &mut Self, item: &mut ast::BinaryOp, common_type_id: TypeId, is_builtin: bool) -> ResolveResult<bool> {
        if is_builtin {
            item.op_resolved = true;
            Ok(true)
        } else {
            let intrinsic = self.intrinsic_ops.get(&item.op).copied().ice()?;
            if self.type_accepted_for(common_type_id, intrinsic.trait_type_id) {
                self.rewrite_binary_op_to_method(item, intrinsic.method, None)?;
                return Ok(true);
            } else if self.stage.must_resolve() {
                let type_name = self.type_name(common_type_id);
                let trait_name = self.type_name(intrinsic.trait_type_id);
                Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path))
            } else {
                Ok(false)
            }
        }
    }

    /// Resolve `&&` / `||` — both operands must be `bool`, result is `bool`.
    pub(super) fn resolve_logical_op(self: &mut Self, item: &mut ast::BinaryOp, _expected_result: Option<TypeId>) -> ResolveResult {
        let bool_type = self.primitive_type_id(Type::bool)?;
        self.resolve_expression(item.left.as_expression_mut().ice()?, Some(bool_type))?;
        self.resolve_expression(item.right.as_expression_mut().ice()?, Some(bool_type))?;
        item.set_type_id(self, bool_type);
        Ok(())
    }

    /// Resolve `<`, `>`, `<=`, `>=`, `==`, `!=`.
    pub(super) fn resolve_comparison_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        self.resolve_expression(item.left.as_expression_mut().ice()?, None)?;
        let left_type_id = item.left.type_id(self);
        self.resolve_expression(item.right.as_expression_mut().ice()?, left_type_id)?;
        let right_type_id = item.right.type_id(self);
        item.set_type_id(self, self.primitive_type_id(Type::bool)?);

        // Propagate inner types between operands so a partially inferred compound type
        // (e.g. `[ ? ]`) is completed from the other side before the equality check below
        if let (Some(left_type_id), Some(right_type_id)) = (left_type_id, right_type_id) {
            self.unify_type_ids(left_type_id, right_type_id);
        }

        if let Some(common_type_id) = left_type_id.or(right_type_id) {
            self.set_type_id(item.left.as_expression_mut().ice()?, common_type_id)?;
            self.set_type_id(item.right.as_expression_mut().ice()?, common_type_id)?;

            // operator overloading for `==`/`!=`: types implementing the intrinsic `Eq` trait are
            // lowered to a call of its `eq` method (overriding the built-in deep comparison). The `Eq`
            // trait backs both operators and is keyed under `Equal` in `intrinsic_ops`; `!=` negates
            // the `eq` result. Types that do not implement `Eq` keep the built-in comparison, so a
            // missing impl is not an error here.
            if matches!(item.op, ast::BinaryOperator::Equal | ast::BinaryOperator::NotEqual) {
                let intrinsic = self.intrinsic_ops.get(&ast::BinaryOperator::Equal).copied().ice()?;
                if self.type_accepted_for(common_type_id, intrinsic.trait_type_id) {
                    let negate = item.op == ast::BinaryOperator::NotEqual;
                    return self.rewrite_eq_op_to_method(item, intrinsic.method, negate, expected_result);
                }
            }

            // operator overloading for `<`, `>`, `<=`, `>=`: types implementing the intrinsic `Ord`
            // trait are lowered to `self.cmp(other) == Ordering::<variant>` comparisons. The `Ord`
            // trait is keyed under `Less` in `intrinsic_ops`. Built-in numeric/string types use
            // native comparison opcodes; custom types without `Ord` get a dedicated error.
            if matches!(item.op, ast::BinaryOperator::Less | ast::BinaryOperator::Greater
                         | ast::BinaryOperator::LessOrEq | ast::BinaryOperator::GreaterOrEq)
            {
                let common_type = self.type_by_id(common_type_id);
                let is_builtin = common_type.is_numeric() || common_type.is_string();
                if is_builtin {
                    item.op_resolved = true;
                } else {
                    let intrinsic = self.intrinsic_ops.get(&ast::BinaryOperator::Less).copied().ice()?;
                    if self.type_accepted_for(common_type_id, intrinsic.trait_type_id) {
                        return self.rewrite_ord_op_to_method(item, expected_result);
                    } else if self.stage.must_resolve() {
                        let type_name = self.type_name(common_type_id);
                        let trait_name = self.type_name(intrinsic.trait_type_id);
                        return Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path));
                    }
                }
            }
        }
        Ok(())
    }

    /// Resolve `+`, `-`, `*`, `/`, `%`.
    pub(super) fn resolve_arithmetic_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        if let Some(common_type_id) = self.resolve_binary_operands(item, expected_result)? {
            let common_type = self.type_by_id(common_type_id);
            let is_builtin = common_type.is_numeric() || (item.op == ast::BinaryOperator::Add && common_type.is_string());
            if !self.check_op_dispatch(item, common_type_id, is_builtin)? {
                // trait dispatch not yet resolved; defer
            }
        }
        Ok(())
    }

    /// Resolve `..` / `..=` range operators.
    pub(super) fn resolve_range_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        if let Some(common_type_id) = self.resolve_binary_operands(item, expected_result)? {
            self.check_op_dispatch(item, common_type_id, true)?;
        }
        Ok(())
    }

    /// Resolve `&`, `|`, `^` bitwise operators (integer-only, trait-dispatchable).
    pub(super) fn resolve_bitwise_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        if let Some(common_type_id) = self.resolve_binary_operands(item, expected_result)? {
            let common_type = self.type_by_id(common_type_id);
            let is_builtin = common_type.is_integer();
            if is_builtin {
                self.check_op_dispatch(item, common_type_id, true)?;
            } else if common_type.is_primitive() {
                // primitive type that is not an integer (e.g. float, bool, string) - reject early
                return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires integer operands", item.op)), self.module_path));
            } else {
                self.check_op_dispatch(item, common_type_id, false)?;
            }
        }
        Ok(())
    }

    /// Resolve `<<`, `>>` shift operators.
    pub(super) fn resolve_shift_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        // Result type follows the left operand; the right operand (shift amount) is an
        // independent integer type (cast to u32 at compile time), so it is not unified with the left;
        // custom types dispatch through the corresponding operator trait
        self.resolve_expression(item.left.as_expression_mut().ice()?, expected_result)?;
        let left_type_id = item.left.type_id(self);
        let type_id = item.type_id(self);
        let mut is_builtin = false;
        if let Some(common_type_id) = type_id.or(left_type_id) {
            self.set_type_id(item, common_type_id)?;
            self.set_type_id(item.left.as_expression_mut().ice()?, common_type_id)?;
            let common_type = self.type_by_id(common_type_id);
            is_builtin = common_type.is_integer();
            if is_builtin {
                item.op_resolved = true;
            } else if common_type.is_primitive() {
                // primitive type that is not an integer (e.g. float, bool, string) - reject early
                return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires an integer left operand", item.op)), self.module_path));
            } else {
                let intrinsic = self.intrinsic_ops.get(&item.op).copied().ice()?;
                if self.type_accepted_for(common_type_id, intrinsic.trait_type_id) {
                    return self.rewrite_shift_op_to_method(item, intrinsic.method, expected_result);
                } else if self.stage.must_resolve() {
                    let type_name = self.type_name(common_type_id);
                    let trait_name = self.type_name(intrinsic.trait_type_id);
                    return Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path));
                }
            }
        }
        self.resolve_expression(item.right.as_expression_mut().ice()?, None)?;
        // integer checks only apply to the builtin path; custom types go through trait dispatch
        if is_builtin {
            if let Some(left_type_id) = item.type_id(self) {
                if !self.type_by_id(left_type_id).is_integer() {
                    return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires an integer left operand", item.op)), self.module_path));
                }
            }
            if let Some(right_type_id) = item.right.type_id(self) {
                if !self.type_by_id(right_type_id).is_integer() {
                    return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires integer shift amount", item.op)), self.module_path));
                }
            }
        }
        Ok(())
    }

    /// Resolve `[]` / `[]=` index operators (map, array, custom Index trait, view).
    pub(super) fn resolve_index_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        self.resolve_expression(item.left.as_expression_mut().ice()?, None)?;
        let left_type_id = item.left.type_id(self);
        let is_map = left_type_id.map_or(false, |id| self.type_by_id(id).as_map().is_some());
        let is_array = left_type_id.map_or(false, |id| self.type_by_id(id).as_array().is_some());
        let is_view = left_type_id.map_or(false, |id| self.type_by_id(id).as_view().is_some());

        if is_view {
            return self.resolve_index_op_view(item, left_type_id.ice()?, expected_result);
        }
        if !is_map && !is_array {
            return self.resolve_index_op_custom(item, left_type_id, expected_result);
        }
        if is_map {
            self.resolve_index_op_map(item, expected_result)
        } else {
            self.resolve_index_op_array(item, expected_result)
        }
    }

    /// Resolve index on a view type: set the element type so the compiler can handle it.
    fn resolve_index_op_view(self: &mut Self, item: &mut ast::BinaryOp, view_type_id: TypeId, _expected_result: Option<TypeId>) -> ResolveResult {
        // Resolve the index expression
        self.resolve_expression(item.right.as_expression_mut().ice()?, Some(self.primitive_type_id(crate::STACK_ADDRESS_TYPE)?))?;

        let view_ty = self.type_by_id(view_type_id).as_view().ice()?;
        let element_type_id = view_ty.element_type_id;

        // Set the result type to the element type
        item.set_type_id(self, element_type_id);
        Ok(())
    }

    /// Handle index on a non-map, non-array type: custom `Index` trait or error.
    fn resolve_index_op_custom(self: &mut Self, item: &mut ast::BinaryOp, left_type_id: Option<TypeId>, expected_result: Option<TypeId>) -> ResolveResult {
        if let (Some(left_type_id), Some(intrinsic)) = (left_type_id, self.intrinsic_index) {
            if self.type_accepted_for(left_type_id, intrinsic.trait_type_id) {
                // Custom Index type detected. For read, rewrite to `a.get(i)`.
                if item.op == ast::BinaryOperator::Index {
                    return self.rewrite_index_read_to_method(item, intrinsic, expected_result);
                }
                // IndexWrite: resolve the index operand (against the get method's param type)
                // so the node is typed enough for resolve_assignment to consume.
                if let Some((get_id, _)) = self.intrinsic_index_methods(left_type_id, &intrinsic) {
                    let function_id = self.scopes.constant_function_id(get_id);
                    if let Some(function_id) = function_id {
                        let func = self.scopes.function_ref(function_id);
                        // get's second parameter (index) type
                        if let Some(Some(index_type_id)) = func.arg_type_ids(self).get(1) {
                            self.resolve_expression(item.right.as_expression_mut().ice()?, Some(*index_type_id))?;
                            return Ok(());
                        }
                    }
                }
                // Methods not yet resolved; defer
                self.resolve_expression(item.right.as_expression_mut().ice()?, None)?;
                return Ok(());
            }
        }
        // Not a map, array, or custom Index type
        if let Some(left_type_id) = left_type_id {
            return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} ({}) does not implement index access", &item.left, self.type_name(left_type_id))), self.module_path));
        }
        Ok(())
    }

    /// Resolve index on a map: infer key/value types and propagate.
    fn resolve_index_op_map(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        // map index: the index operand is the key type, the result is the value type
        let key_type_id = if let Some(&Type::Map(MapType { key_type_id, .. })) = self.item_type(&item.left) { key_type_id } else { None };
        self.resolve_expression(item.right.as_expression_mut().ice()?, key_type_id)?;

        // infer the map key type from the index operand if still unknown
        if key_type_id.is_none() {
            if let (Some(index_type_id), Some(left_type_id)) = (item.right.type_id(self), item.left.type_id(self)) {
                if let Some(map_ty) = self.type_by_id_mut(left_type_id).as_map_mut() {
                    map_ty.key_type_id = Some(index_type_id);
                }
            }
        }
        // if we expect a particular result type, set it now
        if let Some(expected_result) = expected_result {
            self.set_type_id(item, expected_result)?;
        }
        // if we know the result type, propagate it to the map value type
        if let Some(result_type_id) = item.type_id(self) {
            let value_type_id = if let Some(&Type::Map(MapType { value_type_id, .. })) = self.item_type(&item.left) { value_type_id } else { None };
            if let Some(value_type_id) = value_type_id {
                self.check_type_accepted_for(item, result_type_id, value_type_id)?;
            } else if let Some(left_type_id) = item.left.type_id(self) {
                if let Some(map_ty) = self.type_by_id_mut(left_type_id).as_map_mut() {
                    map_ty.value_type_id = Some(result_type_id);
                }
            }
        }
        // if we know the map value type, set the result type to that. The index operator
        // `map[key]` yields the bare value and traps on a missing key (mirroring array
        // indexing); the fallible `map.get(key)` returns an `Option` instead.
        if let Some(&Type::Map(MapType { value_type_id: Some(value_type_id), .. })) = self.item_type(&item.left) {
            self.set_type_id(item, value_type_id)?;
        }
        Ok(())
    }

    /// Resolve index on an array: resolve right operand as stack address, propagate element type.
    fn resolve_index_op_array(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        self.resolve_expression(item.right.as_expression_mut().ice()?, Some(self.primitive_type_id(crate::STACK_ADDRESS_TYPE)?))?;
        self.set_type_id(item.right.as_expression_mut().ice()?, self.primitive_type_id(crate::STACK_ADDRESS_TYPE)?)?;

        // if we expect the result to be of a particular type, set it now
        if let Some(expected_result) = expected_result {
            self.set_type_id(item, expected_result)?;
        }
        // if we know the result type, set the array element type to that
        if let Some(result_type_id) = item.type_id(self) {
            if let Some(Type::Array(array)) = self.item_type(&item.left) {
                if let Some(array_type_id) = array.type_id {
                    self.check_type_accepted_for(item, result_type_id, array_type_id)?;
                } else {
                    // TODO: this is pretty ugly, referencing left type again. working around mutable borrow issues when directly borrowing mutably in above if let
                    let ty = item.left.type_id(self).map(|type_id| self.type_by_id_mut(type_id));
                    if let Some(Type::Array(array)) = ty {
                        array.type_id = Some(result_type_id);
                    }
                }
            }
        }
        // if we know the array element type, set the result type to that
        if let Some(&Type::Array(Array { type_id: Some(element_type_id), .. })) = self.item_type(&item.left) {
            self.set_type_id(item, element_type_id)?;
        }
        Ok(())
    }

    /// Resolve `.member` / `.member=` access operators (builtins, struct fields, trait methods).
    pub(super) fn resolve_access_op(self: &mut Self, item: &mut ast::BinaryOp, _expected_result: Option<TypeId>) -> ResolveResult {
        self.resolve_expression(item.left.as_expression_mut().ice()?, None)?;
        if let Some(left_type_id) = item.left.type_id(self) {
            // check if a built-in method is accessed
            if item.right.type_id(self).is_none() {
                self.resolve_access_builtin(item, left_type_id)?;
            }
            // check if a struct field or method is accessed
            if item.right.type_id(self).is_none() {
                self.resolve_access_struct_field(item, left_type_id)?;
            }
            if let Some(type_id) = item.right.type_id(self) {
                self.set_type_id(item, type_id)?; // ignored if member is constant
            }
        }
        Ok(())
    }

    /// Lookup a built-in method on the receiver type (generator, array, map, scalar, trait-bound, or plain type).
    fn resolve_access_builtin(self: &mut Self, item: &mut ast::BinaryOp, left_type_id: TypeId) -> ResolveResult {
        let owning_type_id = item.left.type_id(self).ice()?;
        let member_name = &item.right.as_member().ice()?.ident.name;
        let left_ty = self.type_by_id(left_type_id);
        let is_array = left_ty.as_array().is_some();
        let is_map = left_ty.as_map().is_some();
        let is_scalar = left_ty.is_float() || left_ty.is_integer() || left_ty.is_string();
        let is_generator = self.generator_signature(left_type_id).is_some();
        let is_view = left_ty.as_view().is_some();
        let trait_bound_ids = left_ty.as_trait_bound().cloned();

        let constant_id = if is_generator {
            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                .or(self.try_create_generator_builtin(member_name, owning_type_id)?)
        } else if is_array {
            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                .or(self.try_create_array_builtin(item.left.as_expression().ice()?, member_name, owning_type_id)?)
        } else if is_map {
            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                .or(self.try_create_map_builtin(item.left.as_expression().ice()?, member_name, owning_type_id)?)
        } else if is_view {
            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                .or(self.try_create_view_builtin(member_name, owning_type_id)?)
        } else if is_scalar {
            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                .or(self.try_create_scalar_builtin(member_name, owning_type_id)?)
        } else if let Some(trait_bound_ids) = trait_bound_ids {
            self.lookup_trait_bound_method(&trait_bound_ids, member_name)
        } else {
            self.lookup_type_method(owning_type_id, member_name)
        };

        if let Some(constant_id) = constant_id {
            let function_id = self.scopes.constant_function_id(constant_id).ice()?;
            let function = self.scopes.function_ref(function_id);
            let member = item.right.as_member_mut().ice()?;
            member.constant_id = Some(constant_id);
            self.set_type_id(member, function.callable_type_id)?;
        }
        Ok(())
    }

    /// Search each constituent trait in a multiple-trait bound (`A + B`) for the named method.
    fn lookup_trait_bound_method(self: &mut Self, trait_bound_ids: &[TypeId], member_name: &str) -> Option<crate::shared::typed_ids::ConstantId> {
        for trait_id in trait_bound_ids {
            let type_name = self.type_flat_name(*trait_id)?.clone();
            let path = self.make_path(&[ &type_name, member_name ]);
            if let Some(constant_id) = self.scopes.constant_id(self.scope_id, &path, *trait_id) {
                return Some(constant_id);
            }
        }
        None
    }

    /// Lookup a method on a concrete type, checking its own methods, trait defaults, and trait-provided methods.
    fn lookup_type_method(self: &mut Self, owning_type_id: TypeId, member_name: &str) -> Option<crate::shared::typed_ids::ConstantId> {
        let type_name = self.type_flat_name(owning_type_id)?.clone();
        let path = self.make_path(&[ type_name, member_name.to_string() ]);
        // For trait types, also check the trait's own provided/required methods
        let trait_method_id = self.type_by_id(owning_type_id).as_trait().and_then(|trt| {
            trt.provided.get(member_name).copied()
                .or(trt.required.get(member_name).copied())
                .and_then(|id| id)
        });
        self.scopes.constant_id(self.scope_id, &path, owning_type_id)
            .or(self.scopes.constant_id(self.scope_id, &path, TypeId::VOID))
            .or(trait_method_id)
            .or(self.scopes.trait_provided_constant_id(member_name, owning_type_id))
    }

    /// Resolve struct field access; report undefined member for non-struct receivers.
    fn resolve_access_struct_field(self: &mut Self, item: &mut ast::BinaryOp, left_type_id: TypeId) -> ResolveResult {
        match self.type_by_id(left_type_id) {
            Type::Struct(struct_) => {
                let field = item.right.as_member_mut().ice_msg("Member access using a non-field")?;
                let field_type_id = *struct_.fields.get(&field.ident.name).usr(Some(field), ResolveErrorKind::UndefinedMember(field.ident.name.clone()))?;
                if let Some(field_type_id) = field_type_id {
                    self.set_type_id(field, field_type_id)?;
                }
            },
            Type::Array(_) | Type::Map(_) => {
                // can't ice here since builtin resolve above might temporarily fail while
                // the array/map inner types are not yet resolved
            },
            _ => {
                let member = item.right.as_member().ice_msg("Member access using a non-member")?;
                let member_name = member.ident.name.clone();
                let type_name = self.type_name(left_type_id);
                return Err(ResolveError::new(member, ResolveErrorKind::UndefinedMethod(member_name, type_name), self.module_path));
            },
        }
        Ok(())
    }

    /// Resolve `func(args)` call expressions (including method calls, Result/Option constructors).
    pub(super) fn resolve_call_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        self.try_bind_result_constructor(item, expected_result)?;
        self.try_bind_option_constructor(item, expected_result)?;
        self.try_bind_view_constructor(item, expected_result)?;

        let call_func = item.left.as_expression_mut().ice()?;
        self.resolve_expression(call_func, None)?;

        // Check for mutating method calls on const receivers BEFORE the method call rewrite,
        // because after the rewrite the Access is replaced with a Constant and the receiver
        // is moved to the argument list, making the check impossible.
        let mutating_on_const = if let Some(binary_op) = call_func.as_binary_op()
            && binary_op.op == ast::BinaryOperator::Access
            && let Some(member) = binary_op.right.as_member()
            && Self::is_mutating_method(&member.ident.name)
            && let ast::BinaryOperand::Expression(ref receiver) = binary_op.left
            && let Some(const_name) = self.find_const_in_receiver_chain(receiver)
        {
            Some((const_name, member.ident.name.clone()))
        } else {
            None
        };
        if let Some((const_name, method_name)) = mutating_on_const {
            return Err(ResolveError::new(item, ResolveErrorKind::MutateConst(const_name, method_name), self.module_path));
        }

        let call_args = item.right.as_argument_list_mut().ice()?;
        self.try_infer_array_element_from_builtin_args(call_func, call_args)?;
        self.try_infer_map_types_from_builtin_args(call_func, call_args)?;
        self.rewrite_method_call_to_constant_call(call_func, call_args)?;

        let callee_constant_id = call_func.as_constant().and_then(|c| c.constant_id);
        let num_args = call_args.args.len();

        if let Some(function_type_id) = call_func.type_id(self) {
            let func = self.validate_callable(item, function_type_id)?;
            self.resolve_call_return_type(item, &func, expected_result)?;
            self.resolve_call_arguments(item, &func, num_args)?;
            self.check_const_args_to_mutating_params(item, callee_constant_id)?;
        }

        self.apply_expected_result_type(item, expected_result)
    }

    /// Validate that the callee type is callable and return the callable signature.
    fn validate_callable(self: &mut Self, item: &mut ast::BinaryOp, function_type_id: TypeId) -> ResolveResult<crate::shared::meta::Callable> {
        match self.type_by_id(function_type_id).as_callable() {
            Some(callable) => Ok(callable.clone()), //FIXME borrow
            None => {
                let type_name = self.type_name(function_type_id);
                Err(ResolveError::new(item, ResolveErrorKind::NotCallable(type_name), self.module_path))
            }
        }
    }

    /// Resolve the call's return type from the callee signature, propagating `Self` returns.
    fn resolve_call_return_type(self: &mut Self, item: &mut ast::BinaryOp, func: &crate::shared::meta::Callable, expected_result: Option<TypeId>) -> ResolveResult {
        // A trait method declared to return `Self` records its return type as the declaring
        // trait. When invoked on a trait-object receiver whose static type is broader (e.g. a
        // multiple-trait bound `A + B`), propagate the receiver's type to the result so methods
        // from the other constituent traits remain callable on the returned value.
        let ret_type_id = func.ret_type_id.map(|ret_type_id| {
            let receiver_type_id = item.left.as_expression()
                .and_then(|e| e.as_constant())
                .and_then(|c| c.constant_id)
                .and_then(|cid| self.scopes.constant_function_id(cid))
                .filter(|&fid| matches!(self.scopes.function_ref(fid).kind, Some(crate::shared::meta::FunctionKind::Method(declaring_id)) if declaring_id == ret_type_id))
                .and(item.right.as_argument_list().and_then(|a| a.args.first()))
                .and_then(|receiver| receiver.type_id(self));
            match receiver_type_id {
                Some(receiver_type_id) if receiver_type_id != ret_type_id && self.type_accepted_for(receiver_type_id, ret_type_id) => receiver_type_id,
                _ => ret_type_id,
            }
        });
        if let Some(ret_type_id) = ret_type_id {
            self.set_type_id(item, ret_type_id)?;
        }
        if let (Some(item_type_id), Some(expected_result)) = (item.type_id(self), expected_result) {
            self.check_type_accepted_for(item, item_type_id, expected_result)?;
        }
        Ok(())
    }

    /// Resolve and type-check each call argument against the callee signature.
    fn resolve_call_arguments(self: &mut Self, item: &mut ast::BinaryOp, func: &crate::shared::meta::Callable, num_args: usize) -> ResolveResult {
        if func.arg_type_ids.len() != num_args {
            let function_name = format!("{}", item.left.as_expression().ice()?);
            return Err(ResolveError::new(item, ResolveErrorKind::NumberOfArguments(function_name, func.arg_type_ids.len() as crate::ItemIndex, num_args as crate::ItemIndex), self.module_path));
        }
        let arguments = item.right.as_argument_list_mut().ice()?;
        for (index, &expected_type_id) in func.arg_type_ids.iter().enumerate() {
            self.resolve_expression(&mut arguments.args[index], expected_type_id)?;
            let actual_type_id = arguments.args[index].type_id(self);
            // infer arguments
            if actual_type_id.is_none() && expected_type_id.is_some() {
                if let ast::Expression::Constant(x) = &arguments.args[index] {
                    if x.constant_id.is_none() {
                        continue; // FIXME: omfg, get this out of here
                    }
                }
                self.set_type_id(&mut arguments.args[index], expected_type_id.ice()?)?;
            } else if let (Some(actual_type_id), Some(expected_type_id)) = (actual_type_id, expected_type_id) {
                self.check_type_accepted_for(&arguments.args[index], actual_type_id, expected_type_id)?;
            }
        }
        Ok(())
    }

    /// Check if any from_const argument is passed to a mutating parameter.
    fn check_const_args_to_mutating_params(self: &mut Self, item: &mut ast::BinaryOp, callee_constant_id: Option<crate::shared::typed_ids::ConstantId>) -> ResolveResult {
        if let Some(call_const_id) = callee_constant_id && let Some(callee_func_id) = self.scopes.constant_function_id(call_const_id) {
            let callee = self.scopes.function_ref(callee_func_id);
            if !callee.mutated_params.is_empty() {
                let arguments = item.right.as_argument_list().ice()?;
                for (arg_index, arg_expr) in arguments.args.iter().enumerate() {
                    if callee.mutated_params.contains(&arg_index) && self.expr_references_user_const(arg_expr) {
                        let func_name = item.left.as_expression().ice()?.as_constant().ice()?.path.to_string(0);
                        return Err(ResolveError::new(
                            item,
                            ResolveErrorKind::ConstArgToMutatingParam(func_name, arg_index),
                            self.module_path,
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    /// Apply the expected result type to the call node, handling trait-object receivers.
    fn apply_expected_result_type(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        if let Some(expected_result) = expected_result {
            // a concrete result passed where a trait is expected stays concrete (coerced to a
            // trait object at compile time); only its acceptance is verified, not its identity
            if self.type_by_id(expected_result).is_trait_object() {
                if let Some(item_type_id) = item.type_id(self) {
                    self.check_type_accepted_for(item, item_type_id, expected_result)?;
                }
            } else {
                self.set_type_id(item, expected_result)?;
            }
        }
        Ok(())
    }
}
