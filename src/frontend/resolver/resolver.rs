//! AST type checker and resolver.

#[path="scopes/scopes.rs"]
mod scopes;
pub mod error;

use std::marker::PhantomData;
use std::collections::HashMap;
use crate::{StackAddress, ItemCount, STACK_ADDRESS_TYPE};
use crate::frontend::ast::{self, Positioned, Returns, Typeable, Resolvable, CallType};
use crate::frontend::resolver::error::{SomeOrResolveError, ResolveResult, ResolveError as Error, ResolveErrorKind as ErrorKind, ice, ICE};
use crate::shared::{Progress, TypeContainer, BindingContainer};
use crate::shared::bindings::Bindings;
use crate::shared::infos::{BindingInfo, FunctionKind, Intrinsic};
use crate::shared::types::{Array, Struct, Type};
use crate::shared::typed_ids::{BindingId, FunctionId, ScopeId, TypeId};
use crate::shared::numeric::Numeric;

use crate::bytecode::VMFunc;
use crate::frontend::parser::types::ParsedProgram;

/// Parsed program AST with all types, bindings and other language structures resolved.
pub struct ResolvedProgram<'ast, T> where T: VMFunc<T> {
    /// Programs are generic over their Rust API
    ty: PhantomData<T>,
    /// Program AST with resolved `BindingId`s.
    pub ast: ParsedProgram<'ast>,
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
    /// Current fully qualified path
    path            : &'ctx mut Vec<String>,
}

/// Resolves types within the given program AST structure.
#[allow(invalid_type_param_default)]
pub fn resolve<'ast, T>(mut program: ParsedProgram<'ast>, entry: &str) -> Result<ResolvedProgram<'ast, T>, Error> where T: VMFunc<T> {

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
    scopes.insert_function(root_scope_id, "len", Some(*primitives.get(&STACK_ADDRESS_TYPE).unwrap_or_ice(ICE)?), Vec::new(), Some(FunctionKind::Intrinsic(Intrinsic::ArrayLen)));

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
        scopes.insert_function(root_scope_id, *name, ret_type, arg_type_id?, Some(FunctionKind::Rust(*index)));
    }

    // repeatedly try to resolve items until no more progress is made
    let mut now_resolved = Progress::zero();
    let mut prev_resolved;
    let mut infer_literals = false;
    let mut must_resolve = false;
    let mut path = Vec::new();

    loop {
        let mut resolver = Resolver {
            scope_id        : root_scope_id,
            scopes          : &mut scopes,
            infer_literals  : infer_literals,
            must_resolve    : must_resolve,
            primitives      : &primitives,
            path            : &mut path,
        };

        for mut statement in program.0.iter_mut() {
            resolver.resolve_statement(&mut statement)?; // todo: want a vec of errors here
        }

        prev_resolved = now_resolved;
        now_resolved = scopes.resolved() + program.0.iter().fold(Progress::zero(), |acc, statement| acc + statement.num_resolved());

        if !now_resolved.done() && now_resolved == prev_resolved {
            if !infer_literals {
                // no additional items resolved, set flag to assumen default types for literals and try again
                infer_literals = true
            } else if !must_resolve {
                // no additional items resolved after literals were inferred to default types, set flag that next run must either resolve or error immediately.
                must_resolve = true;
            } else if must_resolve {
                return ice("Unresolved types remaining but no errors were triggered in error run");
            }
        } else if now_resolved.done() {
            break;
        }
    }

    let entry_fn = scopes.lookup_function_id(root_scope_id, (entry, TypeId::void())).unwrap_or_err(None, ErrorKind::CannotResolve(format!("Cannot resolve entry function '{}'", entry)))?;

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

    /// Returns the type-id for given primitive.
    fn primitive_type_id(self: &Self, ty: Type) -> Result<TypeId, Error> {
        self.primitives.get(&ty).cloned().unwrap_or_ice(ICE)
    }

    /// Returns Err if item and expected type are resolved but do not match as well as if must_resolve is set and item is not resolved.
    fn resolved_or_err(self: &Self, item: &(impl Typeable+Positioned+Resolvable), expected_result: Option<TypeId>) -> ResolveResult {
        if self.must_resolve {
            if item.is_resolved() {
                if expected_result.is_some() {
                    self.check_types_match(item, expected_result, item.type_id(self))
                } else {
                    Ok(())
                }
            } else {
                Err(Error::new(item, ErrorKind::CannotResolve("Cannot resolve binding".to_string())))
            }
        } else {
            if item.type_id(self).is_some() && expected_result.is_some() {
                self.check_types_match(item, expected_result, item.type_id(self))
            } else {
                Ok(())
            }
        }
    }

    /// Sets type if for given AST item if it is not set yet, otherwise checks that new type matches existing type.
    fn set_type_id(self: &mut Self, item: &mut (impl Typeable+Positioned), new_type_id: TypeId) -> ResolveResult {
        if item.type_id(self).is_some() {
            self.check_types_match(item, item.type_id(self), Some(new_type_id))?;
        }
        *item.type_id_mut(self) = Some(new_type_id);
        Ok(())
    }

    /// Returns the type of given AST item.
    fn item_type(self: &Self, item: &impl Typeable) -> Option<&Type> {
        match item.type_id(self) {
            None => None,
            Some(type_id) => Some(self.type_by_id(type_id))
        }
    }

    /// Returns a path string built from current and additional path segments.
    fn make_path(self: &Self, append: &[ &str ]) -> String {
        let mut result = self.path.clone();
        for &part in append {
            result.push(part.to_string());
        }
        result.join("::")
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
        match item {
            ast::InlineType::TypeName(type_name) => self.resolve_type(type_name, None), // todo: not sure about this one. inline-type is defining, so if it differs, the other side should be wrong
            ast::InlineType::Array(array) => self.resolve_array(array),
        }
    }

    /// Resolves a type (name) to a type_id.
    fn resolve_type(self: &mut Self, item: &mut ast::TypeName<'ast>, expected_result: Option<TypeId>) -> Result<Option<TypeId>, Error> {
        if item.type_id.is_none() {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &self.make_path(&item.path.name)) {
                item.type_id = Some(new_type_id);
            }
        }
        if item.type_id.is_some() && expected_result.is_some() {
            self.check_types_match(item, expected_result, item.type_id)?;
        }
        self.resolved_or_err(item, expected_result)?;
        Ok(item.type_id)
    }

    /// Resolves an array definition
    fn resolve_array(self: &mut Self, item: &mut ast::Array<'ast>) -> Result<Option<TypeId>, Error> {
        let inner_type_id = self.resolve_inline_type(&mut item.element_type)?;
        if item.type_id.is_none() { //FIXME needs to update inner type if it is none
            let ty = Type::Array(Array {
                len     : Some(item.len),
                type_id : inner_type_id,
            });
            let new_type_id = self.scopes.insert_type(self.scope_id, None, ty);
            item.type_id = Some(new_type_id);
        }
        self.resolved_or_err(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves a struct definition.
    fn resolve_struct_def(self: &mut Self, item: &mut ast::StructDef<'ast>) -> ResolveResult {
        if item.is_resolved() {
            Ok(())
        } else {
            // resolve ast fields
            for (_, field) in &mut item.fields {
                self.resolve_inline_type(field)?;
            }
            // assemble type field list
            let mut fields = Vec::new();
            for (_, (field_name, field_type)) in item.fields.iter().enumerate() {
                fields.push((field_name.to_string(), field_type.type_id(self)));
            }
            // insert or update type
            if let Some(type_id) = item.type_id {
                let ty = self.scopes.type_mut(type_id);
                ty.as_struct_mut().unwrap().fields = fields;
            } else {
                let ty = Type::Struct(Struct { fields: fields });
                item.type_id = Some(self.scopes.insert_type(self.scope_id, Some(item.ident.name), ty));
            }
            self.resolved_or_err(item, None)
        }
    }

    /// Resolves a struct definition.
    fn resolve_impl_block(self: &mut Self, item: &mut ast::ImplBlock<'ast>) -> ResolveResult {
        if item.ty.type_id(self).is_none() {
            self.resolve_inline_type(&mut item.ty)?;
        }
        if let Some(type_id) = item.ty.type_id(self) {
            let parent_scope_id = self.try_create_scope(&mut item.scope_id);

            if self.scopes.type_id(self.scope_id, "Self").is_none() {
                self.scopes.alias_type(self.scope_id, "Self", type_id);
            }
            for function in &mut item.functions {
                self.resolve_function(function, Some((parent_scope_id, type_id)))?;
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
    fn resolve_function(self: &mut Self, item: &mut ast::Function<'ast>, struct_scope: Option<(ScopeId, TypeId)>) -> ResolveResult {

        let parent_scope_id = self.try_create_scope(&mut item.scope_id);

        self.resolve_signature(&mut item.sig)?;

        //let signature_scope_id = self.try_create_scope(&mut item.scope_id); // todo: put body into separate scope so signature can't access body
        if item.function_id.is_none() && item.sig.ret_resolved(self) && item.sig.args_resolved(self) {
            let result_type_id = item.sig.ret_type_id(self);
            let arg_type_ids: Vec<_> = item.sig.arg_type_ids(self);
            // function/method switch
            let (target_scope_id, function_kind, qualified) = if let Some(scope) = struct_scope {
                let type_name = self.scopes.lookup_type_name(scope.1).unwrap();
                let path = self.make_path(&[ type_name, item.sig.ident.name ]);
                if item.sig.args.len() == 0 || item.sig.args[0].ident.name != "self" {
                    (scope.0, FunctionKind::Function, path)
                } else {
                    (scope.0, FunctionKind::Method(scope.1), path)
                }
            } else {
                let path = self.make_path(&[ item.sig.ident.name ]);
                (parent_scope_id, FunctionKind::Function, path)
            };
            let function_id = self.scopes.insert_function(target_scope_id, &qualified, result_type_id, arg_type_ids, Some(function_kind));
            item.function_id = Some(function_id);
            self.scopes.set_scopefunction_id(self.scope_id, function_id);
        }
        if let Some(function_id) = item.function_id {
            let ret_type = self.scopes.function_ref(function_id).ret_type;
            self.resolve_block(&mut item.block, ret_type)?;
        }
        self.scope_id = parent_scope_id;
        if self.must_resolve && (!item.sig.args_resolved(self) || !item.sig.ret_resolved(self)) {
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
            self.resolved_or_err(expr, ret_type_id)
        } else {
            // no return expression, function result type must be void
            self.check_types_match(item, Some(TypeId::void()), ret_type_id)
        }
    }

    /// Resolves an occurance of a function call.
    fn resolve_call(self: &mut Self, item: &mut ast::Call<'ast>, expected_result: Option<TypeId>) -> ResolveResult {

        // locate function definition
        if item.function_id.is_none() {
            let (path, type_id) = match &item.call_type {
                CallType::Method => {
                    let arg = item.args.get_mut(0).unwrap_or_ice(ICE)?;
                    self.resolve_expression(arg, None)?;
                    let type_id = arg.type_id(self).unwrap_or_ice("Unresolved self binding in method call")?;
                    let type_name = self.scopes.lookup_type_name(type_id).unwrap_or_ice("Unnamed type")?;
                    (self.make_path(&[ type_name, item.ident.name ]), type_id)
                },
                CallType::Function => {
                    (self.make_path(&[ item.ident.name ]), TypeId::void())
                },
                CallType::Static(path) => {
                    (self.make_path(&[ &path.name.join("::"), item.ident.name ]), TypeId::void())
                },
            };
            item.function_id = self.scopes.lookup_function_id(self.scope_id, (&path, type_id));
            if item.function_id.is_none() && self.must_resolve {
                return Err(Error::new(item, ErrorKind::CannotResolve(format!("Cannot resolve function '{}'", &path))));
            }
        }

        // found a function, resolve return type and arguments
        if let Some(function_id) = item.function_id {

            // return value
            let function_info = self.scopes.function_ref(function_id).clone();

            if let Some(ret_type_id) = function_info.ret_type {
                self.set_type_id(item, ret_type_id)?;
            }

            if expected_result.is_some() && item.type_id(self).is_some() {
                self.check_types_match(item, expected_result, item.type_id(self))?;
            }

            // argument count
            if function_info.arg_type.len() != item.args.len() {
                return Err(Error::new(item, ErrorKind::NumberOfArguments(function_info.arg_type.len() as ItemCount, item.args.len() as ItemCount)));
            }

            // arguments
            for (index, &expected_type) in function_info.arg_type.iter().enumerate() {
                self.resolve_expression(&mut item.args[index], expected_type)?;
                let actual_type = item.args[index].type_id(self);
                // infer arguments // FIXME: probably don't want to infer parameter types from arguments
                if actual_type.is_none() && expected_type.is_some() {
                    *item.args[index].type_id_mut(self) = expected_type;
                } else if actual_type.is_some() {
                    self.check_types_match(&item.args[index], expected_type, actual_type)?;
                }
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
            self.set_type_id(item, expected_result)?;
        }
        self.resolved_or_err(item, expected_result)
    }

    /// Resolves an if block.
    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        // resolve condition and block
        self.resolve_expression(&mut item.cond, Some(self.primitive_type_id(Type::bool)?))?;
        self.resolve_block(&mut item.if_block, expected_result)?;
        // optionally resolve else block
        if let Some(else_block) = &mut item.else_block {
            self.resolve_block(else_block, expected_result)?;
            let if_type_id = item.if_block.type_id(self);
            let else_type_id = else_block.type_id(self);
            if if_type_id.is_some() && else_type_id.is_some() {
                self.check_types_match(item, if_type_id, else_type_id)?;
            }
        } else if let Some(if_type_id) = item.if_block.type_id(self) {
            // if block with a non-void result but no else block
            self.check_types_match(item, Some(if_type_id), Some(TypeId::void()))?; // Todo: meh, using check_types_match to generate an error when we already know there is an error.
        }
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a for loop.
    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop<'ast>) -> ResolveResult {
        use ast::{Expression, BinaryOperator as Op};
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        // create binding for the iterator variable
        self.resolve_binding(&mut item.iter)?;
        match &item.expr { // NOTE: these need to match Compiler::compile_for_loop
            Expression::BinaryOp(bo) if bo.op == Op::Range || bo.op == Op::RangeInclusive => {
                let type_id = item.iter.type_id(self);
                self.resolve_expression(&mut item.expr, type_id)?;
                if let Some(type_id) = item.expr.type_id(self) {
                    self.set_type_id(&mut item.iter, type_id)?;
                }
            },
            Expression::Block(_) | Expression::Call(_) | Expression::IfBlock(_) | Expression::Literal(_) | Expression::Variable(_) => {
                self.resolve_expression(&mut item.expr, None)?;
                if let Some(&Type::Array(Array { type_id: Some(elements_type_id), ..})) = self.item_type(&item.expr) {
                    self.set_type_id(&mut item.iter, elements_type_id)?;
                }
            },
            _ => return Err(Error::new(&item.iter, ErrorKind::InvalidOperation("Unsupported for in operand".to_string()))),
        };
        // handle block
        self.resolve_block(&mut item.block, Some(self.primitive_type_id(Type::void)?))?;
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
                let result_type_id = result_expression.type_id(self);
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
        let right_type_id = item.right.type_id(self);
        self.resolve_expression(&mut item.left, right_type_id)?;
        let left_type_id = item.left.type_id(self);
        self.resolve_expression(&mut item.right, left_type_id)?;
        item.type_id = Some(TypeId::void());
        Ok(())
    }

    /// Resolves an assignment expression.
    fn resolve_cast(self: &mut Self, item: &mut ast::Cast<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        self.resolve_type(&mut item.ty/*, 0*/, expected_result)?; // FIXME hardcoded ref arg
        self.resolve_expression(&mut item.expr, None)?;
        if let Some(type_id) = item.ty.type_id {
            *item.type_id_mut(self) = Some(type_id);
            let ty = self.type_by_id(type_id);
            if !ty.is_primitive() && !ty.is_string() {
                return Err(Error::new(item, ErrorKind::NonPrimitiveCast(ty.clone())));
            }
        }
        if let Some(ty) = self.item_type(&item.expr) {
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
                *item.type_id_mut(self) = Some(self.primitive_type_id(Type::bool)?);
            }
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual => {
                self.resolve_expression(&mut item.left, None)?;
                let left_type_id = item.left.type_id(self);
                self.resolve_expression(&mut item.right, left_type_id)?;
                let right_type_id = item.right.type_id(self);
                *item.type_id_mut(self) = Some(self.primitive_type_id(Type::bool)?);
                if let Some(common_type_id) = left_type_id.or(right_type_id) {
                    self.set_type_id(&mut item.left, common_type_id)?;
                    self.set_type_id(&mut item.right, common_type_id)?;
                }
            }
            O::Add | O::Sub | O::Mul | O::Div | O::Rem | O::Range | O::RangeInclusive => {
                self.resolve_expression(&mut item.left, expected_result)?;
                let left_type_id = item.left.type_id(self);
                self.resolve_expression(&mut item.right, left_type_id)?;
                let type_id = item.type_id(self);
                let right_type_id = item.right.type_id(self);
                if let Some(common_type_id) = type_id.or(left_type_id).or(right_type_id) {
                    self.set_type_id(item, common_type_id)?;
                    self.set_type_id(&mut item.left, common_type_id)?;
                    self.set_type_id(&mut item.right, common_type_id)?;
                }
            }
            O::Index | O::IndexWrite => {
                self.resolve_expression(&mut item.left, None)?;
                self.resolve_expression(&mut item.right, Some(self.primitive_type_id(STACK_ADDRESS_TYPE)?))?;
                // left[right] : item
                self.set_type_id(&mut item.right, self.primitive_type_id(STACK_ADDRESS_TYPE)?)?;
                // if we expect the result to be of a particular type, set it now
                if let Some(expected_result) = expected_result {
                    self.set_type_id(item, expected_result)?;
                }
                // if we know the result type, set the array element type to that
                if let Some(result_type_id) = item.type_id(self) {
                    if let Some(Type::Array(array)) = self.item_type(&item.left) {
                        if array.type_id.is_some() {
                            self.check_types_match(item, array.type_id, Some(result_type_id))?;
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
            }
            O::Access | O::AccessWrite => {
                self.resolve_expression(&mut item.left, None)?;
                self.resolve_expression(&mut item.right, None)?;
                // left.right : item
                if let Some(ty) = self.item_type(&item.left) {
                    let struct_ = ty.as_struct().unwrap_or_err(Some(item), ErrorKind::InvalidOperation("Member access on a non-struct".to_string()))?;
                    let field = item.right.as_member_mut().unwrap_or_ice("Member access using a non-field")?;
                    if field.index.is_none() {
                        let index = struct_.fields.iter().position(|f| f.0 == field.ident.name)
                            .unwrap_or_err(Some(field), ErrorKind::UnknownMember(format!("Unknown struct member '{}'", field.ident.name)))?;
                        field.index = Some(index as ItemCount);
                    }
                    if let Some(type_id) = struct_.fields[field.index.unwrap_or_ice(ICE)? as usize].1 {
                        self.set_type_id(item, type_id)?;
                        self.set_type_id(&mut item.right, type_id)?;
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

        // create binding id if we don't have one yet
        if item.binding_id.is_none() {
            // this binding ast node wasn't processed yet. if the binding name already exists we're shadowing - which is NYI
            if self.scopes.binding_id(self.scope_id, item.ident.name).is_some() {
                unimplemented!("cannot shadow {}", item.ident.name); // todo: support shadowing
            }
            let binding_id = self.scopes.insert_binding(self.scope_id, Some(item.ident.name), item.mutable, None);
            item.binding_id = Some(binding_id);
        }

        // check if a type is specified
        if let Some(inline_type) = &mut item.ty {
            if let Some(inline_type_id) = self.resolve_inline_type(inline_type)? {
                self.set_type_id(item, inline_type_id)?;
            }
        }

        // if we have a type, apply it to the expression
        let lhs = item.type_id(self);

        // resolve right hand side
        if let Some(expr) = &mut item.expr {
            self.resolve_expression(expr, lhs)?;
        }

        // if the expression has a type, apply it back to the binding
        if let Some(expr) = &mut item.expr {
            if let Some(expr_type_id) = expr.type_id(self) {
                self.set_type_id(item, expr_type_id)?;
            }
        };

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
                self.set_type_id(item, self.primitive_type_id(Type::bool)?)?;
            },
            UO::IncBefore | UO::DecBefore | UO::IncAfter | UO::DecAfter => {
                if let Some(type_id) = item.expr.type_id(self) {
                    self.set_type_id(item, type_id)?;
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
            self.set_type_id(item, self.primitive_type_id(Type::bool)?)?;
        } else if let LV::String(_) = item.value {
            if expected_type.is_some() {
                self.check_types_match(item, expected_type, Some(self.primitive_type_id(Type::String)?))?;
            }
            self.set_type_id(item, self.primitive_type_id(Type::String)?)?;
        } else if let LV::Array(_) = item.value {
            self.resolve_array_literal(item, expected_type)?;
        } else if let LV::Struct(_) = item.value {
            self.resolve_struct_literal(item)?;
        } else if let Some(type_name) = &mut item.type_name {
            // literal has explicit type, use it
            if let Some(explicit_type_id) = self.resolve_type(type_name, expected_type)? {
                self.set_type_id(item, explicit_type_id)?;
                if expected_type.is_some() {
                    self.check_types_match(item, expected_type, Some(explicit_type_id))?;
                }
            }
        } else if let (&LV::Numeric(numeric), Some(expected_type)) = (&item.value, expected_type) {
            let ty = self.type_by_id(expected_type);
            if !ty.is_compatible_numeric(numeric) {
                return Err(Error::new(item, ErrorKind::IncompatibleNumeric(ty.clone(), numeric)));
            }
            self.set_type_id(item, expected_type)?;
        } else if self.infer_literals {
            // numerics, once normal resolution has failed
            if let LV::Numeric(value) = item.value {
                if let Some(type_id) = self.classify_numeric(value)? {
                    self.set_type_id(item,type_id)?;
                }
            }
        }
        self.resolved_or_err(item, expected_type)
    }

    /// Resolves an struct literal and creates the required field types.
    fn resolve_struct_literal(self: &mut Self, item: &mut ast::Literal<'ast>) -> ResolveResult {

        // resolve type from name

        let type_name = item.type_name.as_mut().unwrap_or_ice(ICE)?;
        let type_id = self.resolve_type(type_name, None)?;

        // resolve fields from field definition

        if let Some(type_id) = type_id {
            self.set_type_id(item, type_id)?;
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
    fn resolve_array_literal(self: &mut Self, item: &mut ast::Literal<'ast>, expected_type_id: Option<TypeId>) -> ResolveResult {

        let mut elements_type_id = None;

        // apply expected type if known. if we already have a known type, set_type_id will check that it matches the one we're trying to set
        if let Some(expected_type_id) = expected_type_id {
            self.set_type_id(item, expected_type_id)?;
        }

        // if we have a type for the array, check if we also have an inner type. if so we want to apply it to all contained literals later.
        if let Some(&Type::Array(Array { type_id: Some(type_id), ..})) = self.item_type(item) {
            elements_type_id = Some(type_id);
        }

        // mutably borrow literal as array literal
        let type_id = item.type_id(self);
        let array_literal = item.value.as_array_mut().unwrap_or_ice("Expected array type, got something else")?;

        // try to get inner type from element literals
        if elements_type_id.is_none() {
            for element in &mut array_literal.elements {
                self.resolve_expression(element, None)?;
                if element.type_id(self).is_some() {
                    elements_type_id = element.type_id(self);
                    break;
                }
            }
        }

        // if we have a known inner type, resolve contained literals with it to help with their inference
        if elements_type_id.is_some() {
            for element in &mut array_literal.elements {
                self.resolve_expression(element, elements_type_id)?;
            }
        }

        // if we don't yet have one, create type based on the inner type, otherwise check types all match
        if type_id.is_none() {
            let new_type_id = self.scopes.insert_type(self.scope_id, None, Type::Array(Array {
                len     : Some(array_literal.elements.len() as StackAddress),
                type_id : elements_type_id,
            }));
            *item.type_id_mut(self) = Some(new_type_id);
        } else if elements_type_id.is_some() {
            let array_type_id = item.type_id(self).unwrap_or_ice(ICE)?;
            let array_ty = self.scopes.type_mut(array_type_id).as_array_mut().unwrap_or_ice(ICE)?;
            if let Some(current_element_type_id) = array_ty.type_id {
                self.check_types_match(item, Some(current_element_type_id), elements_type_id)?;
            } else {
                array_ty.type_id = elements_type_id;
            }
        }

        self.resolved_or_err(item, expected_type_id)
    }
}

// TODO: move this onto scopes, or alternatively move the implementation for Bindings to Compiler instead? might run into mutability issues

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

/// A container holding binding id to BindingInfo mappings
impl<'ctx> BindingContainer for Resolver<'ctx> {
    fn binding_by_id(self: &Self, binding_id: BindingId) -> &BindingInfo {
        self.scopes.bindings.value_by_id(binding_id)
    }
    fn binding_by_id_mut(self: &mut Self, binding_id: BindingId) -> &mut BindingInfo {
        self.scopes.bindings.value_by_id_mut(binding_id)
    }
}