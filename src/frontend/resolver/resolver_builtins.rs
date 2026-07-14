use crate::StackAddress;
use crate::frontend::ast::{self, Typeable};
use crate::frontend::resolver::error::{OptionToResolveError, ResolveResult, ResolveError, ResolveErrorKind};
use crate::shared::{MetaContainer, meta::{Array, FunctionKind}};
use crate::shared::typed_ids::{ScopeId, TypeId, ConstantId};
use crate::bytecode::builtins::builtin_types;
use crate::frontend::resolver::Resolver;

/// General utility methods.
impl<'ctx> Resolver<'ctx> {

    /// Insert a resolved builtin function signature into the root scope.
    pub(super) fn insert_builtin_fn(self: &mut Self, name: &str, type_id: TypeId, builtin_type: crate::bytecode::builtins::BuiltinType, result_type_id: TypeId, arg_type_ids: Vec<TypeId>) -> ConstantId {
        self.scopes.insert_function(
            name,
            Some(result_type_id),
            arg_type_ids.into_iter().map(Some).collect(),
            Some(FunctionKind::Builtin(type_id, builtin_type)),
        )
    }

    /// Try to create concrete array builtin function signature for the given array type
    pub(super) fn try_create_array_builtin(self: &mut Self, item: &ast::Expression, name: &str, type_id: TypeId) -> ResolveResult<Option<ConstantId>> {

        if let Some(constant_id) = self.scopes.constant_id(ScopeId::ROOT, name, type_id) {
            return Ok(Some(constant_id));
        }

        let array_ty = self.type_by_id(type_id).as_array().ice()?;
        if let &Array { type_id: element_type_id @ Some(_) } = array_ty {
            Ok(match builtin_types::Array::resolve(self, name, type_id, element_type_id) {
                None => None,
                Some((builtin_type, result_type_id, arg_type_ids)) => {
                    Some(self.insert_builtin_fn(name, type_id, builtin_type, result_type_id, arg_type_ids))
                },
            })
        } else if self.stage.must_resolve() {
            Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(format!("{}", &item)), self.module_path))
        } else {
            Ok(None)
        }
    }

    /// Try to create a concrete map builtin function signature for the given map type. Uses the
    /// macro-generated [`builtin_types::Map::resolve`] function, registered as [`FunctionKind::Builtin`].
    pub(super) fn try_create_map_builtin(self: &mut Self, item: &ast::Expression, name: &str, type_id: TypeId) -> ResolveResult<Option<ConstantId>> {

        if let Some(constant_id) = self.scopes.constant_id(ScopeId::ROOT, name, type_id) {
            return Ok(Some(constant_id));
        }

        let map_ty = self.type_by_id(type_id).as_map().ice()?;
        let (key_type_id, _value_type_id) = match (map_ty.key_type_id, map_ty.value_type_id) {
            (Some(key_type_id), Some(value_type_id)) => (key_type_id, value_type_id),
            _ => return if self.stage.must_resolve() {
                Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(format!("{}", &item)), self.module_path))
            } else {
                Ok(None)
            },
        };

        Ok(match builtin_types::Map::resolve(self, name, type_id, Some(key_type_id)) {
            None => None,
            Some((builtin_type, result_type_id, arg_type_ids)) => {
                Some(self.insert_builtin_fn(name, type_id, builtin_type, result_type_id, arg_type_ids))
            },
        })
    }

    /// Create a generator method signature (`next`/`value`/`key`). Uses the macro-generated
    /// [`builtin_types::Generator::resolve`] function, registered as [`FunctionKind::Builtin`].
    pub(super) fn try_create_generator_builtin(self: &mut Self, name: &str, type_id: TypeId) -> ResolveResult<Option<ConstantId>> {

        if let Some(constant_id) = self.scopes.constant_id(ScopeId::ROOT, name, type_id) {
            return Ok(Some(constant_id));
        }

        // Pass type_id as inner_type_id so that GenValue/GenKey pseudo-types can resolve via
        // generator_signature, and the require_type check falls through to Type::void.
        Ok(match builtin_types::Generator::resolve(self, name, type_id, Some(type_id)) {
            None => None,
            Some((builtin_type, result_type_id, arg_type_ids)) => {
                Some(self.insert_builtin_fn(name, type_id, builtin_type, result_type_id, arg_type_ids))
            },
        })
    }

    /// Try to create a concrete view builtin function signature (currently just `len()`).
    pub(super) fn try_create_view_builtin(self: &mut Self, name: &str, type_id: TypeId) -> ResolveResult<Option<ConstantId>> {

        if let Some(constant_id) = self.scopes.constant_id(ScopeId::ROOT, name, type_id) {
            return Ok(Some(constant_id));
        }

        let view_ty = self.type_by_id(type_id).as_view().ice()?;
        let _packed_size = view_ty.packed_size as StackAddress;
        let stack_addr_type_id = self.primitive_type_id(crate::STACK_ADDRESS_TYPE)?;

        // len() returns StackAddress (u64), takes self (the view type) as argument
        if name == "len" {
            return Ok(Some(self.insert_builtin_fn(
                name, type_id,
                crate::bytecode::builtins::BuiltinType::View(crate::bytecode::builtins::builtin_types::View::len),
                stack_addr_type_id,
                vec![type_id],
            )));
        }

        Ok(None)
    }

    /// Create integer/float/string builtin function signature.
    pub(super) fn try_create_scalar_builtin(self: &mut Self, name: &str, type_id: TypeId) -> ResolveResult<Option<ConstantId>> {

        if let Some(constant_id) = self.scopes.constant_id(ScopeId::ROOT, name, type_id) {
            return Ok(Some(constant_id));
        }

        let resolved = match self.type_by_id(type_id) {
            ty @ _ if ty.is_integer()   => builtin_types::Integer::resolve(self, name, type_id, None),
            ty @ _ if ty.is_float()     => builtin_types::Float::resolve(self, name, type_id, None),
            ty @ _ if ty.is_string()    => builtin_types::String::resolve(self, name, type_id, None),
            _ => None,
        };

        Ok(match resolved {
            None => None,
            Some((builtin_type, result_type_id, arg_type_ids)) => {
                Some(self.insert_builtin_fn(name, type_id, builtin_type, result_type_id, arg_type_ids))
            },
        })
    }

    /// For a call like `arr.push(x)` whose receiver is an array of still-unknown element type, infer that
    /// element type from the value argument.
    /// Note: method names/parameter index for inference are hardcoded in value_index match block and may need to be updated for new array builtins.
    pub(super) fn try_infer_array_element_from_builtin_args(self: &mut Self, call_exp: &mut ast::Expression, call_args: &mut ast::ArgumentList) -> ResolveResult {
        // identify `<array>.<method>` access whose array element type is still unresolved
        let (array_type_id, value_index) = {
            let binary_op = match call_exp.as_binary_op_mut() {
                Some(binary_op) if binary_op.op == ast::BinaryOperator::Access => binary_op,
                _ => return Ok(()),
            };
            // which (pre-transform, so the receiver is not yet prepended) argument carries the Element value
            let value_index = match binary_op.right.as_member().map(|member| member.ident.name.as_str()) {
                Some("push") => 0,
                Some("insert") => 1,
                _ => return Ok(()),
            };
            match binary_op.left.type_id(self).map(|type_id| self.type_by_id(type_id).as_array()) {
                Some(Some(&Array { type_id: None })) => (binary_op.left.type_id(self).ice()?, value_index),
                _ => return Ok(()),
            }
        };
        if let Some(arg) = call_args.args.get_mut(value_index) {
            self.resolve_expression(arg, None)?;
            if let Some(element_type_id) = arg.type_id(self) {
                self.type_by_id_mut(array_type_id).as_array_mut().ice()?.type_id = Some(element_type_id);
            }
        }
        Ok(())
    }

    /// For a call like `map.insert(k, v)` (or `get`/`remove`) whose receiver map has a still-unknown key or
    /// value type, infer those types from the relevant arguments. Required so methods can be called on an empty
    /// map literal `[ => ]` whose types are not yet determined.
    pub(super) fn try_infer_map_types_from_builtin_args(self: &mut Self, call_exp: &mut ast::Expression, call_args: &mut ast::ArgumentList) -> ResolveResult {
        // identify `<map>.<method>` access and which (pre-transform, receiver not yet prepended) arguments
        // carry the key and value
        let (map_type_id, key_index, value_index) = {
            let binary_op = match call_exp.as_binary_op_mut() {
                Some(binary_op) if binary_op.op == ast::BinaryOperator::Access => binary_op,
                _ => return Ok(()),
            };
            let (key_index, value_index) = match binary_op.right.as_member().map(|member| member.ident.name.as_str()) {
                Some("insert") => (Some(0), Some(1)),
                Some("get") | Some("remove") => (Some(0), None),
                _ => return Ok(()),
            };
            match binary_op.left.type_id(self).map(|type_id| self.type_by_id(type_id).as_map()) {
                Some(Some(_)) => (binary_op.left.type_id(self).ice()?, key_index, value_index),
                _ => return Ok(()),
            }
        };
        if let Some(key_index) = key_index {
            if self.type_by_id(map_type_id).as_map().ice()?.key_type_id.is_none() {
                if let Some(arg) = call_args.args.get_mut(key_index) {
                    self.resolve_expression(arg, None)?;
                    if let Some(key_type_id) = arg.type_id(self) {
                        self.type_by_id_mut(map_type_id).as_map_mut().ice()?.key_type_id = Some(key_type_id);
                    }
                }
            }
        }
        if let Some(value_index) = value_index {
            if self.type_by_id(map_type_id).as_map().ice()?.value_type_id.is_none() {
                if let Some(arg) = call_args.args.get_mut(value_index) {
                    self.resolve_expression(arg, None)?;
                    if let Some(value_type_id) = arg.type_id(self) {
                        self.type_by_id_mut(map_type_id).as_map_mut().ice()?.value_type_id = Some(value_type_id);
                    }
                }
            }
        }
        Ok(())
    }
}