// todo: remove
#![allow(dead_code)]
use std::collections::HashMap;
use crate::frontend::util::{Repository, TypeId, Type, ScopeId, BindingId, FunctionId, FnSig, FnKind};

/// Flat lists of types and bindings and which scope the belong to.
pub struct Scopes<'a> {
    /// Flat bytecode type data, lookup via TypeId or ScopeId and name
    types           : Repository<Type, TypeId, (ScopeId, &'a str)>,
    /// Flat binding data, lookup via BindingId or ScopeId and name
    bindings        : Repository<Option<TypeId>, BindingId, (ScopeId, &'a str)>,
    /// Flat function data, lookup via FunctionId or ScopeId and name
    functions       : Repository<FnSig, FunctionId, (ScopeId, &'a str)>,
    /// Function scopes (the function containing this scope)
    scopefunction   : HashMap<ScopeId, Option<FunctionId>>,
    /// Maps ScopeId => Parent ScopeId (using vector as usize=>usize map)
    parent_map      : Vec<ScopeId>, // ScopeId => ScopeId
}

impl<'a> Scopes<'a> {

    pub fn new() -> Self {
        let root_id = Self::root_id();
        Scopes {
            types           : Repository::new(),
            bindings        : Repository::new(),
            functions       : Repository::new(),
            scopefunction   : HashMap::new(),
            parent_map      : vec![ root_id ],
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

    pub fn dump_parents(self: &Self, start_scope_id: ScopeId) {
        print!("self({:?})", start_scope_id);
        let mut scope_id = start_scope_id;
        loop {
            let parent_scope_id = self.parent_map[Into::<usize>::into(scope_id)];
            if parent_scope_id == scope_id {
                break;
            }
            print!(" <- {:?}", parent_scope_id);
            scope_id = parent_scope_id;
        }
        println!("");
    }
}

/// Function-scope handling
impl<'a> Scopes<'a> {
    /// Sets the id of the function containing this scope.
    pub fn set_scopefunction_id(self: &mut Self, scope_id: ScopeId, function_id: FunctionId) {
        self.scopefunction.insert(scope_id, Some(function_id));
    }
    /// Gets the id of the function containing this scope.
    pub fn scopefunction_id(self: &Self, scope_id: ScopeId) -> Option<FunctionId> {
        *self.scopefunction.get(&scope_id).unwrap_or(&None)
    }
    /// Finds the id of the closest function containing this scope.
    pub fn lookup_scopefunction_id(self: &Self, scope_id: ScopeId) -> Option<FunctionId> {
        if let Some(function_id) = self.scopefunction_id(scope_id) {
            Some(function_id)
        } else {
            // TODO: non recursive solution, ran into multiple mut borrow issues using a while loop
            let parent_scope_id = self.parent_map[Into::<usize>::into(scope_id)];
            if parent_scope_id != scope_id {
                self.lookup_scopefunction_id(parent_scope_id)
            } else {
                None
            }
        }
    }
}

/// Function handling
impl<'a> Scopes<'a> {

    /// Insert a function into the given scope, returning a function id. Its types might not be resolved yet.
    pub fn insert_function(self: &mut Self, scope_id: ScopeId, name: &'a str, result_type_id: Option<TypeId>, arg_type_ids: Vec<TypeId>) -> FunctionId {
        self.functions.insert((scope_id, name), FnSig { ret_type: result_type_id, arg_type: arg_type_ids, kind: FnKind::Internal })
    }

    /// Insert a function into the given scope, returning a function id. Its types might not be resolved yet.
    pub fn insert_rustfn(self: &mut Self, scope_id: ScopeId, name: &'a str, fn_index: u16, result_type_id: Option<TypeId>, arg_type_ids: Vec<TypeId>) -> FunctionId {
        self.functions.insert((scope_id, name), FnSig { ret_type: result_type_id, arg_type: arg_type_ids, kind: FnKind::Rust(fn_index) })
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
    pub fn function_type(self: &Self, function_id: FunctionId) -> &FnSig {
        self.functions.index(function_id)
    }
}

/// Binding handling
impl<'a> Scopes<'a> {

    /// Insert a binding into the given scope, returning a binding id. Its type might not be resolved yet.
    pub fn insert_binding(self: &mut Self, scope_id: ScopeId, name: &'a str, type_id: Option<TypeId>) -> BindingId {
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
    pub fn binding_type_mut(self: &mut Self, binding_id: BindingId) -> &mut Option<TypeId> {
        self.bindings.index_mut(binding_id)
    }

    /// Returns a copy of the type of the given binding id.
    pub fn binding_type(self: &Self, binding_id: BindingId) -> Option<TypeId> {
        *self.bindings.index(binding_id)
    }
}

/// Type handling
impl<'a> Scopes<'a> {

    /// Insert a type into the given scope, returning a type id.
    pub fn insert_type(self: &mut Self, scope_id: ScopeId, name: &'a str, ty: Type) -> TypeId {
        self.types.insert((scope_id, name), ty)
    }

    /// Returns the id of the named type originating in exactly this scope.
    pub fn type_id(self: &Self, scope_id: ScopeId, name: &'a str) -> Option<TypeId> {
        self.types.index_of(&(scope_id, name))
    }

    pub fn void_type(self: &Self) -> TypeId {
        0.into() // todo: this is a little bit hacky
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

    pub fn lookup_type(self: &Self, type_id: TypeId) -> &Type {
        self.types.index(type_id)
    }
}

impl<'a> Into<Vec<Type>> for Scopes<'a> {
    /// convert scopes into type vector
    fn into(self: Self) -> Vec<Type> {
        self.types.into()
    }
}