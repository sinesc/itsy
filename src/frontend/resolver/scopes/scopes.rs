mod repository;

use crate::prelude::*;
use crate::frontend::resolver::resolved::Resolved;
use crate::shared::typed_ids::{TypeId, ScopeId, BindingId, FunctionId, ConstantId};
use crate::shared::meta::{Type, Function, FunctionKind, Binding, Callable, Constant, ConstantValue};
use crate::shared::{TypeContainer, Progress};
use repository::Repository;

/// Flat lists of types and bindings and which scope the belong to.
pub(crate) struct Scopes {
    /// Flat type data, lookup via TypeId or ScopeId and name
    types           : Repository<String, TypeId, Type>,
    /// Flat binding data, lookup via BindingId or ScopeId and name
    bindings        : Repository<String, BindingId, Binding>,
    /// Flat constant data, lookup via ConstantId or ScopeId and name
    constants       : Repository<(String, TypeId), ConstantId, Constant>,
    /// Flat function data, lookup via FunctionId or ScopeId and name
    functions       : Repository<(String, TypeId), FunctionId, Function>,
    /// Function scopes (the function containing this scope), required to typecheck return statements
    scopefunction   : UnorderedMap<ScopeId, Option<FunctionId>>,
    /// Maps ScopeId => Parent ScopeId (using vector as usize=>usize map)
    parent_map      : Vec<ScopeId>, // ScopeId => ScopeId
}

impl Into<Resolved> for Scopes {
    /// convert scopes into type vector
    fn into(self: Self) -> Resolved {
        let binding_map = self.bindings.into();
        let type_map = self.types.into();
        let constant_map = self.constants.into();
        let function_map = self.functions.into();
        Resolved::new(binding_map, type_map, constant_map, function_map)
    }
}

impl Scopes {

    /// Creates and returns a new Scopes instance.
    pub fn new() -> Self {
        Scopes {
            types           : Repository::new(),
            bindings        : Repository::new(),
            constants       : Repository::new(),
            functions       : Repository::new(),
            scopefunction   : UnorderedMap::new(),
            parent_map      : vec![ ScopeId::ROOT ], // set root-scope's parent to itself. used by parent_id() to detect that we hit the root
        }
    }

    /// Returns the number of resolved and total items in the Scopes.
    // TODO: remove this and TypeContainer from Scopes, have ast trait Resolvable take a type container
    pub fn resolved(self: &Self) -> Progress {
        Progress::new(
            // resolved counts
            self.bindings.values().fold(0, |acc, b| acc + b.type_id.is_some() as usize)
            + self.types.values().fold(0, |acc, t| acc + match t {
                Type::Array(array) => if array.type_id.is_some() { 1 } else { 0 },
                _ => 1,
            })
            + self.functions.values().fold(0, |acc, f| acc + f.is_resolved(self) as usize),

            // total counts
            self.bindings.len()
            + self.types.len()
            + self.functions.len(),
        )
    }

    /// Returns the parent scope id of the given scope id.
    pub fn parent_id(self: &Self, scope_id: ScopeId) -> Option<ScopeId> {
        let parent_scope_id = self.parent_map[Into::<usize>::into(scope_id)];
        if parent_scope_id == scope_id { None } else { Some(parent_scope_id) }
    }

    /// Creates a new scope within the parent and returns its id.
    pub fn create_scope(self: &mut Self, parent: ScopeId) -> ScopeId {
        let index = self.parent_map.len();
        self.parent_map.push(parent);
        index.into()
    }
}

/// Function-scope handling
impl Scopes {

    /// Sets the id of the function containing this scope.
    pub fn set_scopefunction_id(self: &mut Self, scope_id: ScopeId, function_id: FunctionId) {
        self.scopefunction.insert(scope_id, Some(function_id));
    }

    /// Registers that this scope has a function attached to it but its id is not known yet.
    /// Does nothing if the scope already has a known function id.
    pub fn register_scopefunction(self: &mut Self, scope_id: ScopeId) {
        self.scopefunction.entry(scope_id).or_insert(None);
    }

    /// Finds the id of the closest function containing this scope.
    pub fn lookup_scopefunction_id(self: &Self, mut scope_id: ScopeId) -> Option<FunctionId> {
        loop {
            // Check if we have a resolved or unresolved scope id. Don't look in parent if we have one that is just not resolved yet.
            if let Some(&function_id) = self.scopefunction.get(&scope_id) {
                return function_id;
            } else if let Some(parent_scope_id) = self.parent_id(scope_id) {
                scope_id = parent_scope_id;
            } else {
                return None;
            }
        }
    }
}

/// Function handling
impl Scopes {

    /// Insert a function into the given scope, returning a function id. Its types might not be resolved yet.
    pub fn insert_function(self: &mut Self, scope_id: ScopeId, name: &str, result_type_id: Option<TypeId>, arg_type_ids: Vec<Option<TypeId>>, kind: Option<FunctionKind>) -> ConstantId {
        let type_id = match kind {
            Some(FunctionKind::Method(type_id)) => type_id,
            Some(FunctionKind::Builtin(type_id, _)) => type_id,
            _ => TypeId::VOID,
        };
        let signature_type_id = self.insert_anonymous_type(true, Type::Callable(Callable { ret_type_id: result_type_id, arg_type_ids }));
        let function_id = self.functions.insert(scope_id, Some((name.into(), type_id)), Function { signature_type_id, kind });
        self.insert_constant(scope_id, name, type_id, Some(signature_type_id), ConstantValue::Function(function_id))
    }

    /// Looks up an existing constant_id for the given function_id.
    pub fn function_constant_id(self: &Self, function_id: FunctionId) -> Option<ConstantId> {
        self.constants.id_search(|c| match c.value {
            ConstantValue::Function(f) => f == function_id,
            _ => false,
        })
    }

    /// Finds the id of the named function within the scope or its parent scopes.
    pub fn lookup_function_id(self: &Self, mut scope_id: ScopeId, name: (&str, TypeId)) -> Option<FunctionId> { // todo: rename to function_id()
        loop {
            if let Some(index) = self.functions.id_by_name(scope_id, (name.0.to_string(), name.1)) {
                return Some(index);
            } else if let Some(parent_scope_id) = self.parent_id(scope_id) {
                scope_id = parent_scope_id;
            } else {
                return None;
            }
        }
    }

    /// Returns the id of the named constant implemented by a trait for the given type_id.
    pub fn trait_function_id(self: &Self, _scope_id: ScopeId, name: &str, type_id: TypeId) -> Option<ConstantId> {
        // todo: think about scoping
        let ty = self.types.value_by_id(type_id);
        if let Some(trait_type_ids) = ty.impl_trait_ids() {
            for &trait_type_id in trait_type_ids {
                let trt = self.types.value_by_id(trait_type_id).as_trait().expect("Implemented type expected to be a trait, got something else");
                if let Some(&constant_id) = trt.provided.get(name) {
                    return constant_id;
                }
            }
        }
        None
    }

    /// Returns a reference to the signature of the given function id.
    pub fn function_ref(self: &Self, function_id: FunctionId) -> &Function {
        self.functions.value_by_id(function_id)
    }
}

/// Binding handling
impl Scopes {

    /// Insert a binding into the given scope, returning a binding id. Its type might not be resolved yet.
    pub fn insert_binding(self: &mut Self, scope_id: ScopeId, name: Option<&str>, mutable: bool, type_id: Option<TypeId>) -> BindingId {
        self.bindings.insert(scope_id, name.map(|n| n.into()), Binding { mutable, type_id })
    }

    /// Returns the id of the named binding originating in exactly this scope.
    pub fn local_binding_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<BindingId> {
        self.bindings.id_by_name(scope_id, name.to_string())
    }

    /// Finds the id of the named binding within the scope or its parent scopes.
    pub fn lookup_binding_id(self: &Self, mut scope_id: ScopeId, name: &str) -> Option<BindingId> { // todo: rename to binding_id()
        loop {
            if let Some(index) = self.local_binding_id(scope_id, name) {
                return Some(index);
            } else if let Some(parent_scope_id) = self.parent_id(scope_id) {
                scope_id = parent_scope_id;
            } else {
                return None;
            }
        }
    }

    /// Returns a mutable reference to the binding info of the given binding id.
    pub fn binding_mut(self: &mut Self, binding_id: BindingId) -> &mut Binding {
        self.bindings.value_by_id_mut(binding_id)
    }

    /// Returns a reference to the binding info of the given binding id.
    pub fn binding_ref(self: &Self, binding_id: BindingId) -> &Binding {
        self.bindings.value_by_id(binding_id)
    }
}

/// Constant handling
impl Scopes {

    /// Insert a constant into the given scope, returning a constant id. Its type might not be resolved yet.
    pub fn insert_constant(self: &mut Self, scope_id: ScopeId, name: &str, owning_type_id: TypeId, value_type_id: Option<TypeId>, value: ConstantValue) -> ConstantId {
        self.constants.insert(scope_id, Some((name.into(), owning_type_id)), Constant { value, type_id: value_type_id })
    }

    /// Aliases an existing constant into the given scope, returning a constant id.
    pub fn alias_constant(self: &mut Self, scope_id: ScopeId, name: &str, constant_id: ConstantId) -> ConstantId {
        self.constants.alias(scope_id, (name.into(), TypeId::VOID), constant_id) // FIXME: VOID seems wrong here
    }

    /// Returns the id of the named constant originating in exactly this scope.
    pub fn local_constant_id(self: &Self, scope_id: ScopeId, name: &str, owning_type_id: TypeId) -> Option<ConstantId> {
        self.constants.id_by_name(scope_id, (name.to_string(), owning_type_id))
    }

    /// Finds the id of the named constant within the scope or its parent scopes.
    pub fn constant_id(self: &Self, mut scope_id: ScopeId, name: &str, owning_type_id: TypeId) -> Option<ConstantId> {
        loop {
            if let Some(index) = self.local_constant_id(scope_id, name, owning_type_id) {
                return Some(index);
            } else if let Some(parent_scope_id) = self.parent_id(scope_id) {
                scope_id = parent_scope_id;
            } else {
                return None;
            }
        }
    }

    /// Returns a mutable reference to the constant info of the given constant id.
    pub fn constant_mut(self: &mut Self, constant_id: ConstantId) -> &mut Constant {
        self.constants.value_by_id_mut(constant_id)
    }

    /// Returns a reference to the constant info of the given constant id.
    pub fn constant_ref(self: &Self, constant_id: ConstantId) -> &Constant {
        self.constants.value_by_id(constant_id)
    }

    pub fn constant_function_id(self: &Self, constant_id: ConstantId) -> Option<FunctionId> {
        let constant = self.constant_ref(constant_id);
        match constant.value {
            ConstantValue::Function(function_id) => Some(function_id),
            _ => None,
        }
    }
}

/// Type handling
impl Scopes {

    /// Insert a type into the given scope, returning a type id.
    pub fn insert_type(self: &mut Self, scope_id: ScopeId, name: Option<&str>, ty: Type) -> TypeId {
        self.types.insert(scope_id, name.map(|n| n.into()), ty)
    }

    /// Inserts an anonymous type into the root scope and returns a type id.
    pub fn insert_anonymous_type(self: &mut Self, reuse_existing: bool, ty: Type) -> TypeId {
        if let (true, Some(type_id)) = (reuse_existing, self.types.id_by_value(&ty)) {
            type_id
        } else {
            self.types.insert(ScopeId::ROOT, None, ty)
        }
    }

    /// Aliases an existing type into the given scope, returning a type id.
    pub fn alias_type(self: &mut Self, scope_id: ScopeId, name: &str, type_id: TypeId) -> TypeId {
        self.types.alias(scope_id, name.into(), type_id)
    }

    /// Returns the id of the named type originating in exactly this scope.
    pub fn local_type_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<TypeId> {
        self.types.id_by_name(scope_id, name.to_string())
    }

    /// Finds the id of the named type within the scope or its parent scopes.
    pub fn lookup_type_id(self: &Self, mut scope_id: ScopeId, name: &str) -> Option<TypeId> {
        loop {
            if let Some(index) = self.local_type_id(scope_id, name) {
                return Some(index);
            } else if let Some(parent_scope_id) = self.parent_id(scope_id) {
                scope_id = parent_scope_id;
            } else {
                return None;
            }
        }
    }

    /// Returns the name of the given type id.
    pub fn type_name(self: &Self, type_id: TypeId) -> Option<&String> {
        self.types.name_by_id(type_id, "Self".to_string())
    }

    /// Returns a mutable reference to the type of the given type id.
    pub fn type_mut(self: &mut Self, type_id: TypeId) -> &mut Type {
        self.types.value_by_id_mut(type_id)
    }

    /// Returns a reference to the type of the given type id.
    pub fn type_ref(self: &Self, type_id: TypeId) -> &Type {
        self.types.value_by_id(type_id)
    }
}

impl TypeContainer for Scopes {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type {
        self.type_ref(type_id)
    }
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type {
        self.type_mut(type_id)
    }
    fn type_flat_name(self: &Self, type_id: TypeId) -> Option<&String> {
        self.type_name(type_id)
    }
}