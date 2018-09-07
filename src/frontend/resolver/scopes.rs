use util::{Repository, TypeId, ScopeId, BindingId, FunctionId, TypeSlot};
use super::Type;

/// Flat lists of types and bindings and which scope the belong to.
pub struct Scopes<'a> {
    /// Flat bytecode type data, lookup via TypeId or ScopeId and name
    types       : Repository<Type, TypeId, (ScopeId, &'a str)>,
    /// Flat binding data, lookup via BindingId or ScopeId and name
    bindings    : Repository<TypeSlot, BindingId, (ScopeId, &'a str)>,
    /// Flat function data, lookup via FunctionId or ScopeId and name
    functions   : Repository<(TypeSlot, Vec<TypeSlot>), FunctionId, (ScopeId, &'a str)>,
    /// Current scope id, incremented when scopes are added
    current     : ScopeId,
    /// Maps ScopeId => Parent ScopeId (using vector as usize=>usize map)
    parent_map  : Vec<ScopeId>, // ScopeId => ScopeId
}

impl<'a> Scopes<'a> {

    pub fn new() -> Self {
        let root_id = Self::root_id();
        Scopes {
            types       : Repository::new(),
            bindings    : Repository::new(),
            functions   : Repository::new(),
            current     : root_id,
            parent_map  : vec![ root_id ],
        }
    }

    pub fn root_id() -> ScopeId {
        (0).into()
    }

    pub fn create_scope(self: &mut Self, parent: ScopeId) -> ScopeId {
        let index = self.parent_map.len();
        self.parent_map.push(parent);
        index.into()
    }
}

impl<'a> Scopes<'a> {

    /// Insert a function into the given scope, returning a function id. Its types might not be resolved yet.
    pub fn insert_function(self: &mut Self, scope_id: ScopeId, name: &'a str, result_type_id: TypeSlot, arg_type_ids: Vec<TypeSlot>) -> FunctionId {
        self.functions.insert((scope_id, name), (result_type_id, arg_type_ids))
    }

    /// Returns the id of the named function originating in exactly this scope.
    pub fn function_id(self: &Self, scope_id: ScopeId, name: &'a str) -> Option<FunctionId> {
        self.functions.index_of(&(scope_id, name))
    }

    /// Finds the id of the named function within the scope or its parent scopes.
    pub fn lookup_function_id(self: &Self, scope_id: ScopeId, name: &'a str) -> Option<FunctionId> {
        if let Some(index) = self.functions.index_of(&(scope_id, name)) {
            Some(index)
        } else {
            // TODO: non recursive solution, ran into multiple mut borrow issues using a while loop
            let parent_scope_id = self.parent_map[Into::<usize>::into(scope_id)];
            if parent_scope_id != scope_id {
                self.lookup_function_id(parent_scope_id, name)
            } else {
                None
            }
        }
    }

    /// Returns the signature of the given function id.
    pub fn function_type(self: &Self, function_id: FunctionId) -> &(TypeSlot, Vec<TypeSlot>) {
        self.functions.index(function_id)
    }
}

impl<'a> Scopes<'a> {

    /// Insert a binding into the given scope, returning a binding id. Its type might not be resolved yet.
    pub fn insert_binding(self: &mut Self, scope_id: ScopeId, name: &'a str, type_id: TypeSlot) -> BindingId {
        self.bindings.insert((scope_id, name), type_id)
    }

    /// Returns the id of the named binding originating in exactly this scope.
    pub fn binding_id(self: &Self, scope_id: ScopeId, name: &'a str) -> Option<BindingId> {
        self.bindings.index_of(&(scope_id, name))
    }

    /// Finds the id of the named binding within the scope or its parent scopes.
    pub fn lookup_binding_id(self: &Self, scope_id: ScopeId, name: &'a str) -> Option<BindingId> {
        if let Some(index) = self.bindings.index_of(&(scope_id, name)) {
            Some(index)
        } else {
            // TODO: non recursive solution, ran into multiple mut borrow issues using a while loop
            let parent_scope_id = self.parent_map[Into::<usize>::into(scope_id)];
            if parent_scope_id != scope_id {
                self.lookup_binding_id(parent_scope_id, name)
            } else {
                None
            }
        }
    }

    /// Returns a mutable reference to the type of the given binding id.
    pub fn binding_type_mut(self: &mut Self, binding_id: BindingId) -> &mut TypeSlot {
        self.bindings.index_mut(binding_id)
    }

    /// Returns a copy of the type of the given binding id.
    pub fn binding_type(self: &Self, binding_id: BindingId) -> TypeSlot {
        *self.bindings.index(binding_id)
    }
}

impl<'a> Scopes<'a> {

    /// Insert a type into the given scope, returning a type id.
    pub fn insert_type(self: &mut Self, scope_id: ScopeId, name: &'a str, ty: Type) -> TypeId {
        self.types.insert((scope_id, name), ty)
    }

    /// Finds the id of the named type within the scope or its parent scopes.
    pub fn lookup_type_id(self: &Self, scope_id: ScopeId, name: &'a str) -> Option<TypeId> {
        if let Some(index) = self.types.index_of(&(scope_id, name)) {
            Some(index)
        } else {
            // TODO: non recursive solution, ran into multiple mut borrow issues using a while loop
            let parent_scope_id = self.parent_map[Into::<usize>::into(scope_id)];
            if parent_scope_id != scope_id {
                self.lookup_type_id(parent_scope_id, name)
            } else {
                None
            }
        }
    }
}

impl<'a> Into<Vec<Type>> for Scopes<'a> {
    /// convert scopes into type vector
    fn into(self: Self) -> Vec<Type> {
        self.types.into()
    }
}