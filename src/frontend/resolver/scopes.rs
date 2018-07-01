use util::{TypeId, ScopeId, BindingId, Repository};
use frontend::{Unresolved, Type};

/// Flat lists of types and bindings and which scope the belong to.
pub struct Scopes<'a> {
    /// Flat type data, lookup via TypeId or ScopeId and name
    types       : Repository<Type<'a>, TypeId, (ScopeId, &'a str)>,
    /// Flat binding data, lookup via TypeId or ScopeId and name
    bindings    : Repository<Unresolved<TypeId>, BindingId, (ScopeId, &'a str)>,
    /// Current scope id, incremented when scopes are added
    current     : ScopeId,
    /// Maps ScopeId => Parent ScopeId (using vector as usize=>usize map)
    parent_map  : Vec<ScopeId>, // ScopeId => ScopeId
}

impl<'a> Scopes<'a> {

    pub fn new() -> Self {
        let root_id = (0).into();
        Scopes {
            types       : Repository::new(),
            bindings    : Repository::new(),
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

    /// Insert a binding into the given scope, returning a binding id. Its type may not be resolved yet.
    pub fn insert_binding(self: &mut Self, scope_id: ScopeId, name: &'a str, type_id: Unresolved<TypeId>) -> BindingId {
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

    /// Returns a mutable reference to the type of the given binding id
    pub fn binding_type_mut(self: &mut Self, binding_id: BindingId) -> &mut Unresolved<TypeId> {
        self.bindings.index_mut(binding_id)
    }

    /// Returns a mutable reference to the type of the given binding id
    pub fn binding_type(self: &Self, binding_id: BindingId) -> Unresolved<TypeId> {
        *self.bindings.index(binding_id)
    }

    /// Insert a type into the given scope, returning a type id.
    pub fn insert_type(self: &mut Self, scope_id: ScopeId, name: &'a str, ty: Type<'a>) -> TypeId {
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

impl<'a> Into<Vec<Type<'a>>> for Scopes<'a> {
    /// convert scopes into type vector
    fn into(self: Self) -> Vec<Type<'a>> {
        self.types.into()
    }
}