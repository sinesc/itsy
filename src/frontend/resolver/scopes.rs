mod repository;

use std::{collections::HashMap, convert::Into};
use self::repository::Repository;
use crate::util::{TypeId, Type, ScopeId, BindingId, FunctionId, FnKind, Intrinsic, Bindings};

#[derive(Clone, Debug)]
pub struct FnSig {
    pub ret_type: Option<TypeId>,
    pub arg_type: Vec<TypeId>,
    pub kind    : FnKind,
}

impl FnSig {
    pub fn rust_fn_index(self: &Self) -> Option<u16> {
        match self.kind {
            FnKind::Rust(index) => Some(index),
            _ => None,
        }
    }
}

struct BindingInfo {
    mutable: bool,
    type_id: Option<TypeId>,
}

/// Flat lists of types and bindings and which scope the belong to.
pub struct Scopes {
    /// Flat bytecode type data, lookup via TypeId or ScopeId and name
    types           : Repository<TypeId, Type>,
    /// Flat binding data, lookup via BindingId or ScopeId and name
    bindings        : Repository<BindingId, BindingInfo>,
    /// Flat function data, lookup via FunctionId or ScopeId and name
    functions       : Repository<FunctionId, FnSig>,
    /// Function scopes (the function containing this scope)
    scopefunction   : HashMap<ScopeId, Option<FunctionId>>,
    /// Maps ScopeId => Parent ScopeId (using vector as usize=>usize map)
    parent_map      : Vec<ScopeId>, // ScopeId => ScopeId
}

impl Scopes {

    /// Creates and returns a new Scopes instance.
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

    /// Returns the number of unresolved and total items in the Scopes.
    pub fn state(self: &Self) -> (u32, u32) {
        (
            self.bindings.values().fold(0, |acc, x| acc + if x.type_id.is_some() { 0 } else { 1 })
            + self.types.values().fold(0, |acc, x| acc + match x {
                Type::Array(array) => if array.len.is_some() && array.type_id.is_some() { 0 } else { 1 },
                _ => 0,
            }), // todo: consider functions as well

            self.bindings.len() as u32
            + self.types.len() as u32,
        )
    }

    /// Returns the root scope id.
    pub fn root_id() -> ScopeId {
        (0).into()
    }

    /// Creates a new scope within the parent and returns its id.
    pub fn create_scope(self: &mut Self, parent: ScopeId) -> ScopeId {
        let index = self.parent_map.len();
        self.parent_map.push(parent);
        index.into()
    }

    #[allow(dead_code)]
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
impl Scopes {

    /// Insert a function into the given scope, returning a function id. Its types might not be resolved yet.
    pub fn insert_function(self: &mut Self, scope_id: ScopeId, name: impl Into<String>, result_type_id: Option<TypeId>, arg_type_ids: Vec<TypeId>) -> FunctionId {
        self.functions.insert(scope_id, Some(name.into()), FnSig { ret_type: result_type_id, arg_type: arg_type_ids, kind: FnKind::User })
    }

    /// Insert a function into the given scope, returning a function id.
    pub fn insert_rustfn(self: &mut Self, scope_id: ScopeId, name: impl Into<String>, fn_index: u16, result_type_id: TypeId, arg_type_ids: Vec<TypeId>) -> FunctionId {
        self.functions.insert(scope_id, Some(name.into()), FnSig { ret_type: Some(result_type_id), arg_type: arg_type_ids, kind: FnKind::Rust(fn_index) })
    }

    /// Insert an intrinsic function into the given scope, returning a function id.
    pub fn insert_intrinsic(self: &mut Self, scope_id: ScopeId, name: impl Into<String>, intrinsic: Intrinsic, result_type_id: TypeId, arg_type_ids: Vec<TypeId>) -> FunctionId {
        self.functions.insert(scope_id, Some(name.into()), FnSig { ret_type: Some(result_type_id), arg_type: arg_type_ids, kind: FnKind::Intrinsic(intrinsic) })
    }

    /*/// Returns the id of the named function originating in exactly this scope.
    pub fn function_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<FunctionId> {
        self.functions.id_of(scope_id, name)
    }*/

    /// Finds the id of the named function within the scope or its parent scopes. // todo: generalize lookup functions, then integrate into resolver
    pub fn lookup_function_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<FunctionId> {
        if let Some(index) = self.functions.id_of(scope_id, name) {
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
        self.functions.by_id(function_id)
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
        self.bindings.id_of(scope_id, name)
    }

    /// Finds the id of the named binding within the scope or its parent scopes.
    pub fn lookup_binding_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<BindingId> {
        if let Some(index) = self.bindings.id_of(scope_id, name) {
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

    /// Returns a mutable reference to the type-id of the given binding id.
    pub fn binding_type_id_mut(self: &mut Self, binding_id: BindingId) -> &mut Option<TypeId> {
        &mut self.bindings.by_id_mut(binding_id).type_id
    }

    /// Returns a copy of the type-id of the given binding id.
    pub fn binding_type_id(self: &Self, binding_id: BindingId) -> Option<TypeId> {
        self.bindings.by_id(binding_id).type_id
    }

    pub fn binding_mutable_mut(self: &mut Self, binding_id: BindingId) -> &mut bool {
        &mut self.bindings.by_id_mut(binding_id).mutable
    }

    pub fn binding_mutable(self: &Self, binding_id: BindingId) -> bool {
        self.bindings.by_id(binding_id).mutable
    }
}

/// Type handling
impl Scopes {

    /// Insert a type into the given scope, returning a type id.
    pub fn insert_type(self: &mut Self, scope_id: ScopeId, name: Option<&str>, ty: Type) -> TypeId {
        self.types.insert(scope_id, name.map(|n| n.into()), ty)
    }

    /// Returns the id of the named type originating in exactly this scope.
    pub fn type_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<TypeId> {
        self.types.id_of(scope_id, name)
    }

    /// Finds the id of the named type within the scope or its parent scopes.
    pub fn lookup_type_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<TypeId> {
        if let Some(index) = self.types.id_of(scope_id, name) {
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

    /// Returns a mutable reference to the type of the given type id.
    pub fn type_mut(self: &mut Self, type_id: TypeId) -> &mut Type {
        self.types.by_id_mut(type_id)
    }

    /// Returns a reference to the type of the given type id.
    pub fn type_ref(self: &Self, type_id: TypeId) -> &Type {
        self.types.by_id(type_id)
    }

    /// Returns the id of type void.
    pub fn void_type(self: &Self) -> TypeId {
        0.into() // todo: this is a little bit hacky
    }
}

impl Into<Bindings> for Scopes {
    /// convert scopes into type vector
    fn into(self: Self) -> Bindings {
        let types: Vec<Type> = self.types.into();
        let type_map = self.bindings.values().map(|info| info.type_id.expect("Unresolved binding type while creating type map")).collect();
        let mutability_map = self.bindings.values().map(|info| info.mutable).collect();
        Bindings::new(mutability_map, type_map, types)
    }
}
