//! AST type checker and resolver.

mod scopes;
pub(crate) mod error;

use std::marker::PhantomData;
use std::collections::HashMap;
use crate::frontend::ast::{self, Bindable, Positioned, Returns, CallType};
use crate::frontend::resolver::error::{SomeOrResolveError, ResolveResult, ResolveError as Error, ResolveErrorKind as ErrorKind, ice, ICE};
use crate::util::{StackAddress, Array, BindingId, Bindings, FnKind, FunctionId, Intrinsic, ItemCount, Numeric, ScopeId, Struct, Type, TypeContainer, TypeId, STACK_ADDRESS_TYPE};
use crate::runtime::VMFunc;

/// Parsed program AST with all types, bindings and other language structures resolved.
pub struct ResolvedProgram<'ast, T> where T: VMFunc<T> {
    /// Programs are generic over their Rust API
    ty: PhantomData<T>,
    /// Program AST with resolved `BindingId`s.
    pub ast: super::ParsedProgram<'ast>,
    /// Type and mutability data for each binding.
    pub bindings: Bindings,
    /// `FunctionId` of the entry/main function.
    pub entry_fn: FunctionId,
}


/// Temporary internal state during program type/binding resolution.
struct Resolver<'ctx> {
    /// Scope id this state operates in.
    scope_id        : ScopeId,
    /// Repository of all scopes.
    scopes          : &'ctx mut scopes::Scopes,
    /// Set to true once resolution failed proceed, causes unresolved numeric literals to be assumed as their default types.
    infer_literals  : bool,
    /// Set to true once resolution failed to proceed again after numeric literal types have been defaulted, causes next resolution failure to trigger a resolution error.
    must_resolve    : bool,
    /// Primitive to type id mapping.
    primitives      : &'ctx HashMap<&'ctx Type, TypeId>,
    /// Count unresolved calls. These are not trackable by Scopes
    unresolved_calls: usize,
    /// Current fully qualified path
    path            : &'ctx mut Vec<String>,
}

/// Resolves types within the given program AST structure.
#[allow(invalid_type_param_default)]
pub fn resolve<'ast, T>(mut program: super::ParsedProgram<'ast>, entry: &str) -> Result<ResolvedProgram<'ast, T>, Error> where T: VMFunc<T> {

    // create root scope and insert primitives
    let mut scopes = scopes::Scopes::new();
    let root_scope_id = scopes::Scopes::root_id();

    let mut primitives = HashMap::new();
    primitives.insert(&Type::void, scopes.insert_type(root_scope_id, None, Type::void));
    primitives.insert(&Type::bool, scopes.insert_type(root_scope_id, Some("bool"), Type::bool));
    primitives.insert(&Type::u8, scopes.insert_type(root_scope_id, Some("u8"), Type::u8));
    primitives.insert(&Type::u16, scopes.insert_type(root_scope_id, Some("u16"), Type::u16));
    primitives.insert(&Type::u32, scopes.insert_type(root_scope_id, Some("u32"), Type::u32));
    primitives.insert(&Type::u64, scopes.insert_type(root_scope_id, Some("u64"), Type::u64));
    primitives.insert(&Type::i8, scopes.insert_type(root_scope_id, Some("i8"), Type::i8));
    primitives.insert(&Type::i16, scopes.insert_type(root_scope_id, Some("i16"), Type::i16));
    primitives.insert(&Type::i32, scopes.insert_type(root_scope_id, Some("i32"), Type::i32));
    primitives.insert(&Type::i64, scopes.insert_type(root_scope_id, Some("i64"), Type::i64));
    primitives.insert(&Type::f32, scopes.insert_type(root_scope_id, Some("f32"), Type::f32));
    primitives.insert(&Type::f64, scopes.insert_type(root_scope_id, Some("f64"), Type::f64));
    primitives.insert(&Type::String, scopes.insert_type(root_scope_id, Some("String"), Type::String));

    // insert intrinsics // todo tie methods to their struct type
    scopes.insert_function(root_scope_id, "len", Some(*primitives.get(&STACK_ADDRESS_TYPE).unwrap_or_ice(ICE)?), Vec::new(), Some(FnKind::Intrinsic(Intrinsic::ArrayLen)));

    // insert rust functions into root scope
    for (name, info) in T::call_info().iter() {
        let (index, ret_type_name, arg_type_names) = info;
        let ret_type = if *ret_type_name == "" {
            Some(*primitives.get(&Type::void).unwrap_or_ice(ICE)?)
        } else {
            Some(scopes.type_id(root_scope_id, ret_type_name).unwrap_or_ice(&format!("Unknown type '{}' encountered in rust fn '{}' return position", ret_type_name, name))?)
        };
        let arg_type_id: Result<Vec<_>, _> = arg_type_names
            .iter()
            .map(|arg_type_name| {
                let arg_type_name = if &arg_type_name[0..2] == "& " { &arg_type_name[2..] } else { &arg_type_name[..] };
                scopes.type_id(root_scope_id, if arg_type_name == "str" { "String" } else { arg_type_name }) // todo: fix string hack
                    .some_or_ice(&format!("Unknown type '{}' encountered in rust fn '{}' argument position", arg_type_name, name))
            })
            .collect();
        scopes.insert_function(root_scope_id, *name, ret_type, arg_type_id?, Some(FnKind::Rust(*index)));
    }

    // repeatedly try to resolve items until no more progress is made
    let mut now_unresolved = (0, 0);
    let mut prev_unresolved;
    let mut infer_literals = false;
    let mut must_resolve = false;
    let mut path = Vec::new();

    loop {
        let unresolved_calls = {
            let mut resolver = Resolver {
                scope_id        : root_scope_id,
                scopes          : &mut scopes,
                infer_literals  : infer_literals,
                must_resolve    : must_resolve,
                primitives      : &primitives,
                unresolved_calls: 0,
                path            : &mut path,
            };
            for mut statement in program.0.iter_mut() {
                resolver.resolve_statement(&mut statement)?; // todo: want a vec of errors here
            }
            resolver.unresolved_calls // scopes doesn't track calls, so we do it
        };

        prev_unresolved = now_unresolved;
        now_unresolved = scopes.state(unresolved_calls);

        if now_unresolved.0 > 0 && (now_unresolved == prev_unresolved) {
            if !infer_literals {
                // no additional items resolved, set flag to assumen default types for literals and try again
                infer_literals = true
            } else if !must_resolve {
                // no additional items resolved after literals were inferred to default types, set flag that next run must either resolve or error immediately.
                must_resolve = true;
            } else if must_resolve {
                return ice("Unresolved types remaining but no errors were triggered in error run");
            }
        } else if now_unresolved.0 == 0 {
            break;
        }
    }

    let entry_fn = scopes.lookup_function_id(root_scope_id, entry).unwrap_or_err(None, ErrorKind::CannotResolve(format!("Cannot resolve entry function '{}'", entry)))?;

    Ok(ResolvedProgram {
        ty              : PhantomData,
        ast             : program,
        entry_fn        : entry_fn,
        bindings        : scopes.into(),
    })
}

/// Utility methods to update a typeslot with a resolved type and increase the resolution counter.
impl<'ast, 'ctx> Resolver<'ctx> where 'ast: 'ctx {

    /// Creates new or enters existing scope and returns the original/parent scope id.
    fn try_create_scope(self: &mut Self, scope_id: &mut Option<ScopeId>) -> ScopeId {
        let parent_scope_id = self.scope_id;
        if let &mut Some(scope_id) = scope_id {
            self.scope_id = scope_id;
        } else {
            self.scope_id = self.scopes.create_scope(parent_scope_id);
            *scope_id = Some(self.scope_id);
        }
        parent_scope_id
    }

    /// Return existing binding-id or create and return new binding-id.
    fn try_create_anon_binding(self: &mut Self, item: &mut impl Bindable) -> BindingId {
        if let Some(binding_id) = item.binding_id() {
            binding_id
        } else {
            let binding_id = self.scopes.insert_binding(self.scope_id, None, true, None);
            *item.binding_id_mut() = Some(binding_id);
            binding_id
        }
    }

    /// Sets given TypeId for the given binding, generating a BindingId if required.
    fn binding_set_type_id(self: &mut Self, item: &mut (impl Bindable+Positioned), new_type_id: TypeId) -> ResolveResult  {
        let binding_id = self.try_create_anon_binding(item);
        let type_id = self.scopes.binding_type_id_mut(binding_id);
        if type_id.is_none() {
            *type_id = Some(new_type_id);
        } else {
            let type_id = *type_id;
            self.check_types_match(item, type_id, Some(new_type_id))?;
        }
        Ok(())
    }

    /// Returns TypeId for the given binding.
    fn binding_type_id(self: &Self, item: &impl Bindable) -> Option<TypeId> {
        item.binding_id().and_then(|binding_id| self.scopes.binding_type_id(binding_id))
    }

    /// Returns a reference to the type of the given binding.
    fn binding_type(self: &mut Self, item: &impl Bindable) -> Option<&Type> {
        self.binding_type_id(item).map(move |type_id| self.type_by_id(type_id))
    }

    /// Returns the type name for a given binding.
    fn binding_type_name(self: &Self, item: &impl Bindable) -> Option<&str> {
        let type_id = self.binding_type_id(item)?;
        Some(self.scopes.lookup_type_name(type_id)?)
    }

    /// Sets mutability for given binding.
    fn binding_set_mutable(self: &mut Self, item: &mut impl Bindable, value: bool) {
        let binding_id = self.try_create_anon_binding(item);
        *self.scopes.binding_mutable_mut(binding_id) = value;
    }

    /// Returns mutability for given binding.
    fn binding_mutable(self: &Self, item: &impl Bindable) -> Option<bool> {
        item.binding_id().map(|binding_id| self.scopes.binding_mutable(binding_id))
    }

    /// Returns TypeId of a type suitable to represent the given numeric. Will only consider i32, i64 and f32.
    fn classify_numeric(self: &Self, value: Numeric) -> Result<Option<TypeId>, Error> {
        Ok(if value.is_integer() {
            if Type::i32.is_compatible_numeric(value) {
                Some(self.primitive_type_id(Type::i32)?)
            } else if Type::i64.is_compatible_numeric(value) {
                Some(self.primitive_type_id(Type::i64)?)
            } else {
                None
            }
        } else if value.is_float() {
            Some(self.primitive_type_id(Type::f64)?)
        } else {
            None
        })
    }

    /// Returns whether the given type_ids refer to matching or fully compatible types.
    fn types_match(self: &Self, type_id_a: Option<TypeId>, type_id_b: Option<TypeId>) -> Result<bool, Error> {
        Ok(if type_id_a == type_id_b {
            true
        } else {
            let type_a = self.type_by_id(type_id_a.unwrap_or_ice(ICE)?);
            let type_b = self.type_by_id(type_id_b.unwrap_or_ice(ICE)?);
            match (type_a, type_b) {
                (&Type::Array(Array { len: a_len, type_id: Some(a_type_id) }), &Type::Array(Array { len: b_len, type_id: Some(b_type_id) })) => {
                    a_len == b_len && self.types_match(Some(a_type_id), Some(b_type_id))?
                }
                _ => false,
            }
        })
    }

    /// Returns Ok if the given type_ids refer to matching or fully compatible types. Otherwise a TypeMismatch error.
    fn check_types_match(self: &Self, item: &impl Positioned, type_id_a: Option<TypeId>, type_id_b: Option<TypeId>) -> ResolveResult  {
        if !self.types_match(type_id_a, type_id_b)? {
            let error_kind = ErrorKind::TypeMismatch(self.type_by_id(type_id_a.unwrap_or_ice(ICE)?).clone(), self.type_by_id(type_id_b.unwrap_or_ice(ICE)?).clone());
            Err(Error::new(item, error_kind))
        } else {
            Ok(())
        }
    }

    /// Returns whether an expression is/may become mutable or not.
    fn expression_may_become_mutable(self: &Self, item: &ast::Expression) -> Result<Option<bool>, Error> { // TODO: weird result. single caller defaults none to true. remove option?
        Ok(if item.is_literal() || item.is_call() {
            // literals and call results accept any mutability
            Some(true)
        } else if let Some(expr_type_id) = self.binding_type_id(item) {
            // primitives or mutable references may become/are mutable
            Some(self.type_by_id(expr_type_id).is_primitive() || self.binding_mutable(item).unwrap_or_ice(ICE)?)
        } else {
            None
        })
    }

    /// Returns the type-id for given primitive.
    fn primitive_type_id(self: &Self, ty: Type) -> Result<TypeId, Error> {
        self.primitives.get(&ty).cloned().unwrap_or_ice(ICE)
    }

    /// Returns Ok if must_resolve flag is not set or Err if must_resolve is set and the item is not resolved.
    fn resolved_or_err(self: &Self, item: &(impl Bindable+Positioned), expected_result: Option<TypeId>) -> ResolveResult {
        if self.must_resolve {
            let binding_type_id = self.binding_type_id(item);
            if binding_type_id.is_none() {
                Err(Error::new(item, ErrorKind::CannotResolve("Cannot resolve binding".to_string())))
            } else if expected_result.is_some() {
                self.check_types_match(item, expected_result, binding_type_id)
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    }
}

/// Methods to resolve individual AST structures.
impl<'ast, 'ctx> Resolver<'ctx> where 'ast: 'ctx {

    /// Resolves types and bindings used in a statement.
    fn resolve_statement(self: &mut Self, item: &mut ast::Statement<'ast>) -> ResolveResult  {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.resolve_function(function, None),
            S::StructDef(structure)     => self.resolve_struct_def(structure),
            S::ImplBlock(impl_block)     => self.resolve_impl_block(impl_block),
            S::Binding(binding)         => self.resolve_binding(binding),
            S::IfBlock(if_block)        => self.resolve_if_block(if_block, None), // accept any type for these, result is discarded
            S::ForLoop(for_loop)        => self.resolve_for_loop(for_loop),
            S::WhileLoop(while_loop)    => self.resolve_while_loop(while_loop),
            S::Block(block)             => self.resolve_block(block, None),
            S::Return(ret)              => self.resolve_return(ret),
            S::Expression(expression)   => self.resolve_expression(expression, None),
        }
    }

    /// Resolves types and bindings used in an expression.
    fn resolve_expression(self: &mut Self, item: &mut ast::Expression<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        use self::ast::Expression as E;
        match item { // todo: these all need to check expected_result since the caller might depend on an error result on type mismatch
            E::Literal(literal)         => self.resolve_literal(literal, expected_result),
            E::Variable(variable)       => self.resolve_variable(variable, expected_result),
            E::Call(call)               => self.resolve_call(call, expected_result),
            E::Member(_)                => { Ok(()) /* nothing to do here */ },
            E::Assignment(assignment)   => self.resolve_assignment(assignment),
            E::BinaryOp(binary_op)      => self.resolve_binary_op(binary_op, expected_result),
            E::UnaryOp(unary_op)        => self.resolve_unary_op(unary_op, expected_result),
            E::Cast(cast)               => self.resolve_cast(cast, expected_result),
            E::Block(block)             => self.resolve_block(block, expected_result),
            E::IfBlock(if_block)        => self.resolve_if_block(if_block, expected_result),
        }
    }

    // Resolves an inline type definition.
    fn resolve_inline_type(self: &mut Self, item: &mut ast::InlineType<'ast>) -> Result<Option<TypeId>, Error> {
        use ast::InlineTypeKind as ITK;
        match &mut item.kind {
            ITK::TypeName(type_name) => self.resolve_type(type_name, None), // todo: not sure about this one. inline-type is defining, so if it differs, the other side should be wrong
            ITK::Array(array) => self.resolve_array(array),
        }
    }

    /// Resolves a type (name) to a type_id.
    fn resolve_type(self: &mut Self, item: &mut ast::TypeName<'ast>, expected_result: Option<TypeId>) -> Result<Option<TypeId>, Error> {
        if self.binding_type_id(item).is_none() {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &item.path.qualified(self.path)) {
                self.binding_set_type_id(item, new_type_id)?;
            }
        }
        let item_type_id = self.binding_type_id(item);
        if item_type_id.is_some() && expected_result.is_some() {
            self.check_types_match(item, expected_result, item_type_id)?;
        }
        self.resolved_or_err(item, expected_result)?;
        Ok(item_type_id)
    }

    /// Resolves an array definition
    fn resolve_array(self: &mut Self, item: &mut ast::Array<'ast>) -> Result<Option<TypeId>, Error> {
        let inner_type_id = self.resolve_inline_type(&mut item.element_type)?;
        if self.binding_type_id(item).is_none() {
            let ty = Type::Array(Array {
                len     : Some(item.len),
                type_id : inner_type_id,
            });
            let new_type_id = self.scopes.insert_type(self.scope_id, None, ty);
            self.binding_set_type_id(item, new_type_id)?;
        }
        self.resolved_or_err(item, None)?;
        Ok(self.binding_type_id(item))
    }

    /// Resolves a struct definition.
    fn resolve_struct_def(self: &mut Self, item: &mut ast::StructDef<'ast>) -> ResolveResult {
        for (_, field) in &mut item.fields {
            self.resolve_inline_type(field)?;
        }
        if self.binding_type_id(item).is_none() {
            let mut fields = Vec::new();
            for (_, (field_name, field_type)) in item.fields.iter().enumerate() {
                let field_type_id = self.binding_type_id(field_type);
                fields.push((field_name.to_string(), field_type_id));
            }
            let ty = Type::Struct(Struct { fields: fields });
            let item_type_id = self.scopes.insert_type(self.scope_id, Some(item.ident.name), ty);
            self.binding_set_type_id(item, item_type_id)?;
        }
        self.resolved_or_err(item, None)
    }

    /// Resolves a struct definition.
    fn resolve_impl_block(self: &mut Self, item: &mut ast::ImplBlock<'ast>) -> ResolveResult {
        if self.binding_type_id(item).is_none() {
            if let Some(type_id) = self.scopes.lookup_type_id(self.scope_id, &item.ident.qualified(self.path, None)) {
                self.binding_set_type_id(item, type_id)?;
            }
        }
        if let Some(binding_id) = item.binding_id() {
            let parent_scope_id = self.try_create_scope(&mut item.scope_id);

            if let Some(type_id) = self.scopes.binding_type_id(binding_id) {
                if self.scopes.type_id(self.scope_id, "Self").is_none() {
                    self.scopes.alias_type(self.scope_id, "Self", type_id);
                }
                let type_name = self.binding_type_name(item).unwrap_or_ice(ICE)?.to_string();
                for function in &mut item.functions {
                    self.resolve_function(function, Some((parent_scope_id, type_name.clone())))?;
                }
            }

            self.scope_id = parent_scope_id;
        }
        Ok(())
    }

    /// Resolves a function signature.
    fn resolve_signature(self: &mut Self, item: &mut ast::Signature<'ast>) -> ResolveResult {
        // resolve arguments
        for arg in item.args.iter_mut() {
            self.resolve_binding(arg)?; // checks resolved_or_err
        }
        // resolve return type
        if let Some(ret) = &mut item.ret {
            self.resolve_inline_type(ret)?; // checks resolved_or_err
        }
        Ok(())
    }

    /// Resolves a function defintion.
    fn resolve_function(self: &mut Self, item: &mut ast::Function<'ast>, struct_scope: Option<(ScopeId, String)>) -> ResolveResult {

        fn ret_resolved<'ast>(me: &mut Resolver, item: &ast::Signature<'ast>) -> bool {
            item.ret.as_ref().map_or(true, |ret| me.binding_type_id(ret).is_some())
        }
        fn ret_type_id<'ast>(me: &Resolver, item: &ast::Signature<'ast>) -> Option<TypeId> {
            item.ret.as_ref().map_or(Some(TypeId::void()), |ret| me.binding_type_id(ret))
        }
        fn args_resolved<'ast>(me: &mut Resolver, item: &ast::Signature<'ast>) -> bool {
            //item.args.iter().fold(true, |acc, arg| acc && arg.ty.as_ref().map_or(false, |type_name| me.binding_type_id(type_name).is_some()))
            // todo: test this instead:
            !item.args.iter().any(|arg| arg.ty.as_ref().map_or(true, |type_name| me.binding_type_id(type_name).is_none()))
        }
        fn arg_type_ids<'ast>(me: &Resolver, item: &ast::Signature<'ast>) -> Vec<Option<TypeId>> {
            item.args.iter().map(|arg| arg.ty.as_ref().map_or(None, |type_name| me.binding_type_id(type_name))).collect()
        }

        let parent_scope_id = self.try_create_scope(&mut item.scope_id);

        self.resolve_signature(&mut item.sig)?;

        //let signature_scope_id = self.try_create_scope(&mut item.scope_id); // todo: put body into separate scope so signature can't access body
        if item.function_id.is_none() && ret_resolved(self, &item.sig) && args_resolved(self, &item.sig) {
            let result_type_id = ret_type_id(self, &item.sig);
            let arg_type_ids: Vec<_> = arg_type_ids(self, &item.sig);
            let (target_scope_id, type_name) = if let Some(scope) = struct_scope {
                (scope.0, Some(scope.1))
            } else {
                (parent_scope_id, None)
            };
            let function_id = self.scopes.insert_function(target_scope_id, item.sig.ident.qualified(self.path, type_name), result_type_id, arg_type_ids, Some(FnKind::User));
            item.function_id = Some(function_id);
            self.scopes.set_scopefunction_id(self.scope_id, function_id);
        }
        if let Some(function_id) = item.function_id {
            let ret_type = self.scopes.function_ref(function_id).ret_type;
            self.resolve_block(&mut item.block, ret_type)?;
        }
        self.scope_id = parent_scope_id;
        if self.must_resolve && (!args_resolved(self, &item.sig) || !ret_resolved(self, &item.sig)) {
            Err(Error::new(item, ErrorKind::CannotResolve(format!("Cannot resolve arguments/return types for '{}'", item.sig.ident.name))))
        } else {
            Ok(())
        }
    }

    /// Resolves a return statement.
    fn resolve_return(self: &mut Self, item: &mut ast::Return<'ast>) -> ResolveResult {
        let function_id = self.scopes.lookup_scopefunction_id(self.scope_id).unwrap_or_err(Some(item), ErrorKind::InvalidOperation("Encountered return outside of function".to_string()))?;
        let ret_type_id = self.scopes.function_ref(function_id).ret_type;
        if let Some(expr) = &mut item.expr {
            self.resolved_or_err(expr, None)?;
            self.resolve_expression(expr, ret_type_id)?;
            // check return type matches function result type
            let expression_type_id = self.binding_type_id(expr);
            if expression_type_id.is_some() && ret_type_id.is_some() {
                self.check_types_match(item, ret_type_id, expression_type_id)?;
            }
            Ok(())
        } else {
            // no return expression, function result type must be void
            self.check_types_match(item, Some(TypeId::void()), ret_type_id)
        }
    }

    /// Resolves an occurance of a function call.
    fn resolve_call(self: &mut Self, item: &mut ast::Call<'ast>, expected_result: Option<TypeId>) -> ResolveResult {

        // locate function definition
        if item.function_id.is_none() {
            let path = match &item.call_type { // TODO: wow this got ugly
                CallType::Method => {
                    if item.args.len() >= 1 {
                        if self.binding_type_id(&item.args[0]) == None {
                            self.resolve_expression(&mut item.args[0], None)?;
                        }
                        self.binding_type_name(&item.args[0]).unwrap().to_string()
                    } else {
                        return Err(Error::new(item, ErrorKind::Internal(ICE.to_string())));
                    }
                },
                CallType::Static(path) => path.qualified(self.path),
                CallType::Function => self.path.join("::"),
            };
            let fn_name = if path != "" { vec![&path[..], item.ident.name].join("::") } else { item.ident.name.to_string() }; // TODO: hot garbage
            item.function_id = self.scopes.lookup_function_id(self.scope_id, &fn_name);
            if item.function_id.is_none() {
                if self.must_resolve {
                    return Err(Error::new(item, ErrorKind::CannotResolve(format!("Cannot resolve function '{}'", fn_name))));
                } else {
                    self.unresolved_calls += 1
                }
            }
        }

        // found a function, resolve return type and arguments
        if let Some(function_id) = item.function_id {

            // return value
            let function_info = self.scopes.function_ref(function_id).clone();
            if let Some(ret_type_id) = function_info.ret_type {
                self.binding_set_type_id(item, ret_type_id)?;
            }

            if expected_result.is_some() {
                let actual_result = self.binding_type_id(item);
                if actual_result.is_some() {
                    self.check_types_match(item, expected_result, actual_result)?;
                }
            }

            // argument count
            if function_info.arg_type.len() != item.args.len() {
                return Err(Error::new(item, ErrorKind::NumberOfArguments(function_info.arg_type.len() as ItemCount, item.args.len() as ItemCount)));
            }

            // arguments
            for (index, &expected_type) in function_info.arg_type.iter().enumerate() {
                self.resolve_expression(&mut item.args[index], expected_type)?;
                let actual_type = self.binding_type_id(&item.args[index]);
                // infer arguments // FIXME: probably don't want to infer parameter types from arguments
                if actual_type.is_none() && expected_type.is_some() {
                    self.binding_set_type_id(&mut item.args[index], expected_type.unwrap_or_ice(ICE)?)?;
                } else if actual_type.is_some() {
                    self.check_types_match(&item.args[index], expected_type, actual_type)?;
                }
            }

            // rustcall?
            if let Some(rust_fn_index) = function_info.rust_fn_index() {
                item.call_kind = FnKind::Rust(rust_fn_index);
            }
        }

        self.resolved_or_err(item, expected_result)
    }

    /// Resolves an occurance of a variable.
    fn resolve_variable(self: &mut Self, item: &mut ast::Variable<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        // resolve binding
        if item.binding_id.is_none() {
            item.binding_id = self.scopes.lookup_binding_id(self.scope_id, item.ident.name);
            if item.binding_id.is_none() {
                return Err(Error::new(item, ErrorKind::UnknownValue(item.ident.name.to_string())));
            }
        }
        // set expected type, if any
        if let Some(expected_result) = expected_result {
            self.binding_set_type_id(item, expected_result)?;
        }
        self.resolved_or_err(item, expected_result)
    }

    /// Resolves an if block.
    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        let mut result_mutability = None;
        // resolve condition and block
        self.resolve_expression(&mut item.cond, Some(self.primitive_type_id(Type::bool)?))?;
        self.resolve_block(&mut item.if_block, expected_result)?;
        // optionally resolve else block
        if let Some(else_block) = &mut item.else_block {
            self.resolve_block(else_block, expected_result)?;
            let if_type_id = self.binding_type_id(&item.if_block);
            let else_type_id = self.binding_type_id(else_block);
            if if_type_id.is_some() && else_type_id.is_some() {
                result_mutability = Some(self.binding_mutable(&item.if_block).unwrap_or_ice(ICE)? && self.binding_mutable(else_block).unwrap_or_ice(ICE)?);
                self.check_types_match(item, if_type_id, else_type_id)?;
            }
        } else if let Some(if_type_id) = self.binding_type_id(&item.if_block) {
            // if block with a non-void result but no else block
            self.check_types_match(item, Some(if_type_id), Some(TypeId::void()))?; // Todo: meh, using check_types_match to generate an error when we already know there is an error.
            result_mutability = Some(self.binding_mutable(&item.if_block).unwrap_or_ice(ICE)?);
        }
        if let Some(result_mutability) = result_mutability {
            self.binding_set_mutable(item, result_mutability);
        }
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a for loop.
    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop<'ast>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        // create binding for the iterator variable
        self.resolve_binding(&mut item.iter)?;
        // resolve the range type using the type of the iterator variable, if possible, then apply range type back to iter
        let type_id = self.binding_type_id(&item.iter);
        self.resolve_expression(&mut item.range, type_id)?;
        if let Some(type_id) = self.binding_type_id(&item.range) {
            self.binding_set_type_id(&mut item.iter, type_id)?;
        }
        // handle block
        self.resolve_block(&mut item.block, None)?;
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a while loop.
    fn resolve_while_loop(self: &mut Self, item: &mut ast::WhileLoop<'ast>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        self.resolve_expression(&mut item.expr, None)?;
        self.resolve_block(&mut item.block, None)?;
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a block.
    fn resolve_block(self: &mut Self, item: &mut ast::Block<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);

        // check for unconditional returns, code below those is unreachable, remove
        let mut return_index = None;
        for (index, statement) in item.statements.iter_mut().enumerate() {
            if statement.returns() {
                return_index = Some(index);
                break;
            }
        }
        // remove code after return, move return to returns
        if let Some(return_index) = return_index {
            item.statements.truncate(return_index + 1);
            let returns = item.statements.pop().unwrap_or_ice(ICE)?;
            item.returns = Some(returns.into_expression().unwrap_or_ice(ICE)?);
            item.result = None;
        }
        // check if the result returns, if so move to returns
        if item.result.as_ref().map_or(false, |r| r.returns()) {
            item.returns = item.result.take();
        }

        // resolve statments, result and returns
        for statement in item.statements.iter_mut() {
            self.resolve_statement(statement)?;
        }
        if let Some(ref mut result) = item.result {
            self.resolve_expression(result, expected_result)?;
        }
        if let Some(ref mut returns) = item.returns {
            let function_id = self.scopes.lookup_scopefunction_id(self.scope_id).unwrap_or_err(Some(returns), ErrorKind::InvalidOperation("Encountered return outside of function".to_string()))?;
            let ret_type_id = self.scopes.function_ref(function_id).ret_type;
            self.resolve_expression(returns, ret_type_id)?;
        }

        // check result type matches expected type unless block is returned from before ever resulting
        if item.returns.is_none() && expected_result.is_some() {
            if item.result.is_none() { // no result = void
                self.check_types_match(item, expected_result, Some(self.primitive_type_id(Type::void)?))?;
            } else if let Some(result_expression) = &item.result {
                let result_type_id = self.binding_type_id(result_expression);
                if result_type_id.is_some() {
                    self.check_types_match(item, expected_result, result_type_id)?;
                }
            }
        }
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves an assignment expression.
    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment<'ast>) -> ResolveResult {
        let right_type_id = self.binding_type_id(&item.right);
        self.resolve_expression(&mut item.left, right_type_id)?;
        let left_type_id = self.binding_type_id(&item.left);
        self.resolve_expression(&mut item.right, left_type_id)?;
        self.binding_set_type_id(item, TypeId::void())?;
        if let Some(false) = self.binding_mutable(&item.left) {
            Err(Error::new(item, ErrorKind::AssignToImmutable))
        } else {
            Ok(())
        }
    }

    /// Resolves an assignment expression.
    fn resolve_cast(self: &mut Self, item: &mut ast::Cast<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        self.resolve_type(&mut item.ty/*, 0*/, expected_result)?; // FIXME hardcoded ref arg
        self.resolve_expression(&mut item.expr, None)?;
        if let Some(type_id) = self.binding_type_id(&item.ty) {
            self.binding_set_type_id(item, type_id)?;
            let ty = self.type_by_id(type_id);
            if !ty.is_primitive() && !ty.is_string() {
                return Err(Error::new(item, ErrorKind::NonPrimitiveCast(ty.clone())));
            }
        }
        if let Some(ty) = self.binding_type(&mut item.expr) {
            if !ty.is_primitive() && !ty.is_string() {
                return Err(Error::new(item, ErrorKind::NonPrimitiveCast(ty.clone())));
            }
        }
        Ok(())
    }

    /// Resolves a binary operation.
    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::BinaryOperator as O;

        match item.op {
            O::And | O::Or => {
                self.resolve_expression(&mut item.left, Some(self.primitive_type_id(Type::bool)?))?;
                self.resolve_expression(&mut item.right, Some(self.primitive_type_id(Type::bool)?))?;
                self.binding_set_type_id(item, self.primitive_type_id(Type::bool)?)?;
            }
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual => {
                self.resolve_expression(&mut item.left, None)?;
                let left_type_id = self.binding_type_id(&item.left);
                self.resolve_expression(&mut item.right, left_type_id)?;
                let right_type_id = self.binding_type_id(&item.right);
                self.binding_set_type_id(item, self.primitive_type_id(Type::bool)?)?;
                if let Some(common_type_id) = left_type_id.or(right_type_id) {
                    self.binding_set_type_id(&mut item.left, common_type_id)?;
                    self.binding_set_type_id(&mut item.right, common_type_id)?;
                }
            }
            O::Add | O::Sub | O::Mul | O::Div | O::Rem | O::Range | O::RangeInclusive => {
                self.resolve_expression(&mut item.left, expected_result)?;
                let left_type_id = self.binding_type_id(&item.left);
                self.resolve_expression(&mut item.right, left_type_id)?;
                let type_id = self.binding_type_id(item);
                let right_type_id = self.binding_type_id(&item.right);
                if let Some(common_type_id) = type_id.or(left_type_id).or(right_type_id) {
                    self.binding_set_type_id(item, common_type_id)?;
                    self.binding_set_type_id(&mut item.left, common_type_id)?;
                    self.binding_set_type_id(&mut item.right, common_type_id)?;
                }
            }
            O::Index | O::IndexWrite => {
                self.resolve_expression(&mut item.left, None)?;
                self.resolve_expression(&mut item.right, Some(self.primitive_type_id(STACK_ADDRESS_TYPE)?))?;
                // left[right] : item
                self.binding_set_type_id(&mut item.right, self.primitive_type_id(STACK_ADDRESS_TYPE)?)?;
                self.try_create_anon_binding(item);
                // if we know the result type, set the array element type to that
                if let Some(result_type_id) = self.binding_type_id(item) {
                    // get mutable reference to type
                    let ty = item.left.binding_id()
                        .and_then(|binding_id| self.scopes.binding_type_id(binding_id))
                        .map(|type_id| self.type_by_id_mut(type_id));

                    if let Some(Type::Array(array)) = ty {
                        array.type_id = Some(result_type_id); // todo: check that array.type_id is either None or equals binding_type_id
                    }
                    if let Some(false) = self.binding_mutable(&item.left) {
                        self.binding_set_mutable(item, false);
                    }
                }
                // if we know the array element type, set the result type to that
                if let Some(&Type::Array(Array { type_id: Some(element_type_id), .. })) = self.binding_type(&item.left) {
                    self.binding_set_type_id(item, element_type_id)?;
                }
            }
            O::Access | O::AccessWrite => {
                self.resolve_expression(&mut item.left, None)?;
                self.resolve_expression(&mut item.right, None)?;
                // left.right : item
                if let Some(type_id) = self.binding_type_id(&item.left) {
                    let ty = self.type_by_id(type_id);
                    let struct_ = ty.as_struct().unwrap_or_err(Some(item), ErrorKind::InvalidOperation("Member access on a non-struct".to_string()))?;
                    let field = item.right.as_member_mut().unwrap_or_ice("Member access using a non-field")?;
                    if field.index.is_none() {
                        let index = struct_.fields.iter().position(|f| f.0 == field.ident.name)
                            .unwrap_or_err(Some(field), ErrorKind::UnknownMember(format!("Unknown struct member '{}'", field.ident.name)))?;
                        field.index = Some(index as ItemCount);
                    }
                    if let Some(type_id) = struct_.fields[field.index.unwrap_or_ice(ICE)? as usize].1 {
                        self.binding_set_type_id(item, type_id)?;
                        self.binding_set_type_id(&mut item.right, type_id)?;
                    }
                    if let Some(false) = self.binding_mutable(&item.left) {
                        self.binding_set_mutable(item, false);
                    }
                }
            }
            O::Assign | O::AddAssign | O::SubAssign | O::MulAssign | O::DivAssign | O::RemAssign => {
                return ice("Unexpected operator in resolve_binary_op");
            }
        }

        Ok(())
    }

    /// Resolves a binding created by let, for or a signature.
    fn resolve_binding(self: &mut Self, item: &mut ast::Binding<'ast>) -> ResolveResult {

        // check if a type is specified
        let explicit = match item.ty {
            Some(ref mut ty) => {
                self.resolve_inline_type(ty)?
            },
            None => None,
        };

        // create binding id if we don't have one yet
        if item.binding_id().is_none() {
            // this binding ast node wasn't processed yet. if the binding name already exists we're shadowing - which is NYI
            if self.scopes.binding_id(self.scope_id, item.ident.name).is_some() {
                unimplemented!("cannot shadow {}", item.ident.name); // todo: support shadowing
            }
            let binding_id = self.scopes.insert_binding(self.scope_id, Some(item.ident.name), item.mutable, None);
            *item.binding_id_mut() = Some(binding_id);
        }

        // apply explicit type if we got one
        if let Some(explicit) = explicit {
            self.binding_set_type_id(item, explicit)?;
        }

        // resolve right hand side
        if let Some(expr) = &mut item.expr {
            self.resolve_expression(expr, explicit)?;
        }

        // if we have a binding type, apply it to the expression
        let lhs = self.binding_type_id(item);

        if let (Some(lhs), Some(expr)) = (lhs, &mut item.expr) {
            self.binding_set_type_id(expr, lhs)?;
        }

        // if the expression has a type, apply it back to the binding
        if let Some(expr) = &mut item.expr {
            let expr_type_id = self.binding_type_id(expr);
            if let Some(expr_type_id) = expr_type_id {
                self.binding_set_type_id(item, expr_type_id)?;
            }
        };

        // for mutable binding, expression needs to be a primitive or a literal (both copied/constructed) or mutable itself
        if let Some(expr) = &item.expr {
            if item.mutable && !self.expression_may_become_mutable(expr)?.unwrap_or(true) {
                return Err(Error::new(item, ErrorKind::MutabilityEscalation));
            }
            self.resolved_or_err(expr, None)?;
        }

        self.resolved_or_err(item, None)?;
        Ok(())
    }

    /// Resolves a unary operation.
    fn resolve_unary_op(self: &mut Self, item: &mut ast::UnaryOp<'ast>, expected_type: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::UnaryOperator as UO;
        self.resolve_expression(&mut item.expr, expected_type)?;
        match item.op {
            UO::Not => {
                if expected_type.is_some() {
                    self.check_types_match(item, expected_type, Some(self.primitive_type_id(Type::bool)?))?;
                }
                self.binding_set_type_id(item, self.primitive_type_id(Type::bool)?)?;
            },
            UO::IncBefore | UO::DecBefore | UO::IncAfter | UO::DecAfter => {
                if let Some(type_id) = self.binding_type_id(&item.expr) {
                    self.binding_set_type_id(item, type_id)?;
                }
            },
        }
        self.resolved_or_err(&item.expr, None)?;
        self.resolved_or_err(item, expected_type)
    }

    /// Resolves a literal type if it is annotated, otherwise let the parent expression pick a concrete type.
    fn resolve_literal(self: &mut Self, item: &mut ast::Literal<'ast>, expected_type: Option<TypeId>) -> ResolveResult {
        use self::ast::LiteralValue as LV;

        if let LV::Bool(_) = item.value { // todo: all of these need to check expected type if any
            if expected_type.is_some() {
                self.check_types_match(item, expected_type, Some(self.primitive_type_id(Type::bool)?))?;
            }
            self.binding_set_type_id(item, self.primitive_type_id(Type::bool)?)?;
        } else if let LV::String(_) = item.value {
            if expected_type.is_some() {
                self.check_types_match(item, expected_type, Some(self.primitive_type_id(Type::String)?))?;
            }
            self.binding_set_type_id(item, self.primitive_type_id(Type::String)?)?;
        } else if let LV::Array(_) = item.value {
            self.resolve_array_literal(item, expected_type)?;
        } else if let LV::Struct(_) = item.value {
            self.resolve_struct_literal(item)?;
        } else if let Some(type_name) = &mut item.type_name {
            // literal has explicit type, use it
            if let Some(explicit_type_id) = self.resolve_type(type_name, expected_type)? {
                self.binding_set_type_id(item, explicit_type_id)?;
                if expected_type.is_some() {
                    self.check_types_match(item, expected_type, Some(explicit_type_id))?;
                }
            }
        } else if let (&LV::Numeric(numeric), Some(expected_type)) = (&item.value, expected_type) {
            let ty = self.type_by_id(expected_type);
            if !ty.is_compatible_numeric(numeric) {
                return Err(Error::new(item, ErrorKind::IncompatibleNumeric(ty.clone(), numeric)));
            }
            self.binding_set_type_id(item, expected_type)?;
        } else if self.infer_literals {
            // numerics, once normal resolution has failed
            if let LV::Numeric(value) = item.value {
                if let Some(type_id) = self.classify_numeric(value)? {
                    self.binding_set_type_id(item, type_id)?;
                }
            }
        } else {
            self.try_create_anon_binding(item);
        }
        self.resolved_or_err(item, expected_type)
    }

    /// Resolves an struct literal and creates the required field types.
    fn resolve_struct_literal(self: &mut Self, item: &mut ast::Literal<'ast>) -> ResolveResult {

        self.try_create_anon_binding(item);

        // resolve type from name

        let type_name = item.type_name.as_mut().unwrap_or_ice(ICE)?;
        let type_id = self.resolve_type(type_name, None)?;

        // resolve fields from field definition

        if let Some(type_id) = type_id {
            self.binding_set_type_id(item, type_id)?;
            let struct_def = self.type_by_id(type_id).as_struct().unwrap_or_err(Some(item), ErrorKind::Internal("Tried to resolve a struct but got different type".to_string()))?.clone();
            let struct_ = item.value.as_struct_mut().unwrap_or_ice(ICE)?;
            for (name, field) in &mut struct_.fields {
                self.resolve_expression(field, struct_def.type_id(name))?;
                self.resolved_or_err(field, None)?;
            }
        }

        self.resolved_or_err(item, None)
    }

    /// Resolves an array literal and creates the required array types.
    fn resolve_array_literal(self: &mut Self, item: &mut ast::Literal<'ast>, expected_type: Option<TypeId>) -> ResolveResult {

        let binding_id = self.try_create_anon_binding(item);

        // apply expected type if known
        if let Some(expected_type) = expected_type {
            self.binding_set_type_id(item, expected_type)?;
        }

        let array = item.value.as_array_mut().unwrap_or_ice("Expected array type, got something else")?;

        // apply the same binding id to all elements
        let element_binding_id = self.try_create_anon_binding(array.elements.first_mut().unwrap_or_ice(ICE)?);
        let mut elements_type_id = None;

        for element in &mut array.elements {
            *element.binding_id_mut() = Some(element_binding_id);
            self.resolve_expression(element, None)?;
            if elements_type_id == None {
                elements_type_id = self.binding_type_id(element);
            } else {
                let this_type_id = self.binding_type_id(element);
                if this_type_id.is_some() {
                    self.check_types_match(element, elements_type_id, this_type_id)?;
                }
            }
        }

        // create this level's type based on the inner type
        if self.scopes.binding_type_id(binding_id).is_none() {
            let new_type_id = self.scopes.insert_type(self.scope_id, None, Type::Array(Array {
                len     : Some(array.elements.len() as StackAddress),
                type_id : elements_type_id,
            }));
            *self.scopes.binding_type_id_mut(binding_id) = Some(new_type_id);
        }

        // set inner type based on this level's type
        if let Some(type_id) = self.scopes.binding_type_id(binding_id) {
            let ty = self.type_by_id(type_id);
            if let Some(elements_type_id) = ty.as_array().unwrap_or_ice(ICE)?.type_id {
                self.binding_set_type_id(array.elements.first_mut().unwrap_or_ice(ICE)?, elements_type_id)?; // all elements have the same binding id, so set only first item type
            }
        }

        self.resolved_or_err(item, expected_type)
    }
}

/// Support TypeContainer for Scopes so that methods that need to follow type_ids can be implemented once and be used in both
/// the Resolver where types are stored in Scopes and the Compiler where types are a stored in a Vec.
impl<'ctx> TypeContainer for Resolver<'ctx> {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type {
        self.scopes.type_ref(type_id)
    }
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type {
        self.scopes.type_mut(type_id)
    }
}