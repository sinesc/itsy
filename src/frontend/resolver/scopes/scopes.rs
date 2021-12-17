mod repository;

use crate::prelude::*;
use crate::shared::Progress;
use crate::frontend::resolver::resolved::IdMappings;
use crate::shared::typed_ids::{TypeId, ScopeId, BindingId, FunctionId};
use crate::shared::infos::{BindingInfo, FunctionInfo, FunctionKind};
use crate::shared::types::Type;
use repository::Repository;

/// Flat lists of types and bindings and which scope the belong to.
pub(crate) struct Scopes {
    /// Flat bytecode type data, lookup via TypeId or ScopeId and name
    types           : Repository<String, TypeId, Type>,
    /// Flat binding data, lookup via BindingId or ScopeId and name
    bindings        : Repository<String, BindingId, BindingInfo>,
    /// Flat function data, lookup via FunctionId or ScopeId and name
    functions       : Repository<(String, TypeId), FunctionId, FunctionInfo>,
    /// Function scopes (the function containing this scope)
    scopefunction   : UnorderedMap<ScopeId, Option<FunctionId>>,
    /// Maps ScopeId => Parent ScopeId (using vector as usize=>usize map)
    parent_map      : Vec<ScopeId>, // ScopeId => ScopeId
}

impl Into<IdMappings> for Scopes {
    /// convert scopes into type vector
    fn into(self: Self) -> IdMappings {
        let binding_map = self.bindings.into();
        let type_map = self.types.into();
        let function_map = self.functions.into();
        IdMappings::new(binding_map, type_map, function_map)
    }
}

impl Scopes {

    /// Creates and returns a new Scopes instance.
    pub fn new() -> Self {
        let root_id = Self::root_id();
        Scopes {
            types           : Repository::new(),
            bindings        : Repository::new(),
            functions       : Repository::new(),
            scopefunction   : UnorderedMap::new(),
            parent_map      : vec![ root_id ], // set root-scope's parent to itself. used by parent_id() to detect that we hit the root
        }
    }

    /// Returns the number of resolved and total items in the Scopes.
    pub fn resolved(self: &Self) -> Progress {
        Progress::new(
            // resolved counts
            self.bindings.values().fold(0, |acc, b| acc + b.type_id.is_some() as usize)
            + self.types.values().fold(0, |acc, t| acc + match t {
                Type::Array(array) => if array.len.is_some() && array.type_id.is_some() { 1 } else { 0 },
                _ => 1,
            })
            + self.functions.values().fold(0, |acc, f| acc + f.is_resolved() as usize),

            // total counts
            self.bindings.len()
            + self.types.len()
            + self.functions.len(),
        )
    }

    /// Returns the root scope id.
    pub fn root_id() -> ScopeId {
        (0).into()
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

    /// Gets the id of the function containing this scope.
    pub fn scopefunction_id(self: &Self, scope_id: ScopeId) -> Option<FunctionId> {
        *self.scopefunction.get(&scope_id).unwrap_or(&None)
    }

    /// Finds the id of the closest function containing this scope.
    pub fn lookup_scopefunction_id(self: &Self, mut scope_id: ScopeId) -> Option<FunctionId> {
        loop {
            if let Some(function_id) = self.scopefunction_id(scope_id) {
                return Some(function_id);
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
    pub fn insert_function(self: &mut Self, scope_id: ScopeId, name: &str, result_type_id: Option<TypeId>, arg_type_ids: Vec<Option<TypeId>>, kind: Option<FunctionKind>) -> FunctionId {
        let type_id = match kind {
            Some(FunctionKind::Method(type_id)) => type_id,
            _ => TypeId::void(),
        };
        self.functions.insert(scope_id, Some((name.into(), type_id)), FunctionInfo { ret_type: result_type_id, arg_type: arg_type_ids, kind: kind })
    }

    /// Aliases an existing function into the given scope, returning a function id.
    pub fn alias_function(self: &mut Self, scope_id: ScopeId, name: &str, function_id: FunctionId) -> FunctionId {
        self.functions.alias(scope_id, (name.into(), TypeId::void()), function_id)
    }

    /// Returns the id of the named function originating in exactly this scope.
    pub fn function_id(self: &Self, scope_id: ScopeId, name: (&str, TypeId)) -> Option<FunctionId> {
        self.functions.id_by_name(scope_id, (name.0.to_string(), name.1))
    }

    /// Finds the id of the named function within the scope or its parent scopes.
    pub fn lookup_function_id(self: &Self, mut scope_id: ScopeId, name: (&str, TypeId)) -> Option<FunctionId> {
        loop {
            if let Some(index) = self.function_id(scope_id, name) {
                return Some(index);
            } else if let Some(parent_scope_id) = self.parent_id(scope_id) {
                scope_id = parent_scope_id;
            } else {
                return None;
            }
        }
    }

    /// Returns the id of the named function implemented by a trait for the given type_id.
    pub fn trait_function_id(self: &Self, _scope_id: ScopeId, name: &str, type_id: TypeId) -> Option<FunctionId> {
        // todo: think about scoping
        let ty = self.types.value_by_id(type_id);
        if let Some(trait_type_ids) = ty.impl_trait_ids() {
            for &trait_type_id in trait_type_ids {
                let trt = self.types.value_by_id(trait_type_id).as_trait().expect("Implemented type expected to be a trait, got something else");
                if let Some(&function_id) = trt.provided.get(name) {
                    return function_id;
                }
            }
        }
        None
    }

    /// Returns a reference to the signature of the given function id.
    pub fn function_ref(self: &Self, function_id: FunctionId) -> &FunctionInfo {
        self.functions.value_by_id(function_id)
    }
}

/// Binding handling
impl Scopes {

    /// Insert a binding into the given scope, returning a binding id. Its type might not be resolved yet.
    pub fn insert_binding(self: &mut Self, scope_id: ScopeId, name: Option<&str>, mutable: bool, type_id: Option<TypeId>) -> BindingId {
        self.bindings.insert(scope_id, name.map(|n| n.into()), BindingInfo { mutable, type_id })
    }

    /// Returns the id of the named binding originating in exactly this scope.
    pub fn binding_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<BindingId> {
        self.bindings.id_by_name(scope_id, name.to_string())
    }

    /// Finds the id of the named binding within the scope or its parent scopes.
    pub fn lookup_binding_id(self: &Self, mut scope_id: ScopeId, name: &str) -> Option<BindingId> {
        loop {
            if let Some(index) = self.binding_id(scope_id, name) {
                return Some(index);
            } else if let Some(parent_scope_id) = self.parent_id(scope_id) {
                scope_id = parent_scope_id;
            } else {
                return None;
            }
        }
    }

    /// Returns a mutable reference to the binding info of the given binding id.
    pub fn binding_mut(self: &mut Self, binding_id: BindingId) -> &mut BindingInfo {
        self.bindings.value_by_id_mut(binding_id)
    }

    /// Returns a reference to the binding info of the given binding id.
    pub fn binding_ref(self: &Self, binding_id: BindingId) -> &BindingInfo {
        self.bindings.value_by_id(binding_id)
    }
}

/// Type handling
impl Scopes {

    /// Insert a type into the given scope, returning a type id.
    pub fn insert_type(self: &mut Self, scope_id: ScopeId, name: Option<&str>, ty: Type) -> TypeId {
        self.types.insert(scope_id, name.map(|n| n.into()), ty)
    }

    /// Aliases an existing type into the given scope, returning a type id.
    pub fn alias_type(self: &mut Self, scope_id: ScopeId, name: &str, type_id: TypeId) -> TypeId {
        self.types.alias(scope_id, name.into(), type_id)
    }

    /// Returns the id of the named type originating in exactly this scope.
    pub fn type_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<TypeId> {
        self.types.id_by_name(scope_id, name.to_string())
    }

    /// Finds the id of the named type within the scope or its parent scopes.
    pub fn lookup_type_id(self: &Self, mut scope_id: ScopeId, name: &str) -> Option<TypeId> {
        loop {
            if let Some(index) = self.type_id(scope_id, name) {
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