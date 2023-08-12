mod repository;

use crate::prelude::*;
use crate::frontend::resolver::resolved::Resolved;
use crate::shared::typed_ids::{TypeId, ScopeId, BindingId, FunctionId, ConstantId};
use crate::shared::meta::{Type, Function, FunctionKind, Binding, Callable, Constant, ConstantValue};
use crate::shared::{TypeContainer, Progress};
use repository::Repository;

/// Flat lists of types and bindings and which scope they belong to.
pub(crate) struct Scopes {
    /// Aliases point from a scoped name to an unscoped path (e.g. Self -> mymod::MyStruct)
    aliases         : UnorderedMap<(ScopeId, String), String>,
    /// Constants point from an unscoped path or a type id and a name to an item.
    constants       : Repository<(String, TypeId), ConstantId, Constant>,
    /// List of types. Vector index represents TypeId
    types           : Repository<String, TypeId, Type>,
    /// List of functions. Vector index represents FunctionId
    functions       : Vec<Function>,
    /// Scoped bindings.
    bindings        : Repository<(ScopeId, String), BindingId, Binding>,
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
        Resolved::new(binding_map, type_map, constant_map, self.functions)
    }
}

impl Scopes {

    /// Creates and returns a new Scopes instance.
    pub fn new() -> Self {
        Scopes {
            aliases         : UnorderedMap::new(),
            types           : Repository::new(),
            bindings        : Repository::new(),
            constants       : Repository::new(),
            functions       : Vec::new(),
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
            + self.functions.iter().fold(0, |acc, f| acc + f.is_resolved(self) as usize),

            // total counts
            self.bindings.len()
            + self.types.len()
            + self.functions.len(),
        )
    }

    /// Returns the parent scope id of the given scope id.
    fn parent_id(self: &Self, scope_id: ScopeId) -> Option<ScopeId> {
        let parent_scope_id = self.parent_map[Into::<usize>::into(scope_id)];
        if parent_scope_id == scope_id { None } else { Some(parent_scope_id) }
    }

    /// Creates a new scope within the parent and returns its id.
    pub fn create_scope(self: &mut Self, parent: ScopeId) -> ScopeId {
        let index = self.parent_map.len();
        self.parent_map.push(parent);
        let result: ScopeId = index.into();
        result
    }

    /// Inserts a general alias into the given scope.
    pub fn insert_alias<T: AsRef<str>, U: AsRef<str>>(self: &mut Self, scope_id: ScopeId, name: T, alias: U) {
        //self.print(&format!("insert_alias {} -> {}", alias.as_ref().to_string(), name.as_ref()), scope_id);
        self.aliases.insert((scope_id, alias.as_ref().to_string()), name.as_ref().to_string());
    }

    /// Resolves given alias.
    pub fn alias<T: AsRef<str>>(self: &Self, mut scope_id: ScopeId, alias: T) -> Option<&str> {
        loop {
            if let Some(name) = self.aliases.get(&(scope_id, alias.as_ref().to_string())) {
                return Some(name);
            } else if let Some(parent_scope_id) = self.parent_id(scope_id) {
                scope_id = parent_scope_id;
            } else {
                return None;
            }
        }
    }
}

/// Function-scope handling
impl Scopes {

    /// Sets the id of the function containing this scope. Used to check return types against function signature.
    pub fn scope_set_function_id(self: &mut Self, scope_id: ScopeId, function_id: FunctionId) {
        self.scopefunction.insert(scope_id, Some(function_id));
    }

    /* /// Registers that this scope has a function attached to it but its id is not known yet.
    /// Does nothing if the scope already has a known function id.
    pub fn scope_register_function(self: &mut Self, scope_id: ScopeId) {
        self.scopefunction.entry(scope_id).or_insert(None);
    }*/

    /// Finds the id of the closest function containing this scope.
    pub fn scope_function_id(self: &Self, mut scope_id: ScopeId) -> Option<FunctionId> {
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
    pub fn insert_function(self: &mut Self, name: &str, result_type_id: Option<TypeId>, arg_type_ids: Vec<Option<TypeId>>, kind: Option<FunctionKind>) -> ConstantId {
        let type_id = match kind {
            Some(FunctionKind::Method(type_id)) => type_id,
            Some(FunctionKind::Builtin(type_id, _)) => type_id,
            _ => TypeId::VOID,
        };
        let signature_type_id = self.insert_anonymous_type(true, Type::Callable(Callable { ret_type_id: result_type_id, arg_type_ids }));
        let function_id = FunctionId::from(self.functions.len());
        self.functions.push(Function { signature_type_id, kind });
        self.insert_constant(name, type_id, Some(signature_type_id), ConstantValue::Function(function_id))
    }

    /// Looks up an existing constant_id for the given function_id.
    pub fn function_constant_id(self: &Self, function_id: FunctionId) -> Option<ConstantId> {
        self.constants.id_search(|c| match c.value {
            ConstantValue::Function(f) => f == function_id,
            //_ => false,
        })
    }

    /// Returns the function id for the given constant id, if the constant represents a function.
    pub fn constant_function_id(self: &Self, constant_id: ConstantId) -> Option<FunctionId> {
        let constant = self.constant_ref(constant_id);
        match constant.value {
            ConstantValue::Function(function_id) => Some(function_id),
            //_ => None,
        }
    }

    /// Returns the id of the named constant implemented by a trait for the given type_id.
    pub fn trait_provided_constant_id(self: &Self, name: &str, type_id: TypeId) -> Option<ConstantId> {
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
        &self.functions[function_id.into_usize()]
    }
}

/// Binding handling
impl Scopes {

    /// Insert a binding into the given scope, returning a binding id. Its type might not be resolved yet.
    pub fn insert_binding(self: &mut Self, scope_id: ScopeId, name: &str, mutable: bool, type_id: Option<TypeId>) -> BindingId {
        self.bindings.insert(Some((scope_id, name.into())), Binding { mutable, type_id })
    }

    /// Returns the id of the named binding originating in exactly this scope.
    pub fn local_binding_id(self: &Self, scope_id: ScopeId, name: &str) -> Option<BindingId> {
        self.bindings.id_by_name(&(scope_id, name.to_string()))
    }

    /// Finds the id of the named binding within the scope or its parent scopes.
    pub fn binding_id(self: &Self, mut scope_id: ScopeId, name: &str) -> Option<BindingId> {
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
    pub fn insert_constant(self: &mut Self, name: &str, owning_type_id: TypeId, value_type_id: Option<TypeId>, value: ConstantValue) -> ConstantId {
        self.constants.insert(Some((name.into(), owning_type_id)), Constant { value, type_id: value_type_id })
    }

    /// Finds the id of the named constant within the scope or its parent scopes.
    pub fn constant_id<T: AsRef<str>>(self: &Self, scope_id: ScopeId, name: T, owning_type_id: TypeId) -> Option<ConstantId> {
        let name = self.alias(scope_id, name.as_ref()).unwrap_or_else(|| name.as_ref()).to_string();
        self.constants.id_by_name(&(name, owning_type_id))
    }

    /// Returns a mutable reference to the constant info of the given constant id.
    pub fn constant_mut(self: &mut Self, constant_id: ConstantId) -> &mut Constant {
        self.constants.value_by_id_mut(constant_id)
    }

    /// Returns a reference to the constant info of the given constant id.
    pub fn constant_ref(self: &Self, constant_id: ConstantId) -> &Constant {
        self.constants.value_by_id(constant_id)
    }
}

/// Type handling
impl Scopes {

    /// Insert a type into the root scope, returning a type id.
    pub fn insert_type(self: &mut Self, name: Option<&str>, ty: Type) -> TypeId {
        self.types.insert(name.map(|n| n.into()), ty)
    }

    /// Inserts an anonymous type into the root scope and returns a type id.
    pub fn insert_anonymous_type(self: &mut Self, reuse_existing: bool, ty: Type) -> TypeId {
        if let (true, Some(type_id)) = (reuse_existing, self.types.id_by_value(&ty)) {
            type_id
        } else {
            self.types.insert(None, ty)
        }
    }

    /// Returns the id of the named type or alias (the scope id is required for alias resolution).
    pub fn type_id<T: AsRef<str>>(self: &Self, scope_id: ScopeId, name: T) -> Option<TypeId> {
        let name = self.alias(scope_id, name.as_ref()).unwrap_or_else(|| name.as_ref()).to_string();
        self.types.id_by_name(&name)
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
        self.types.name_by_id(type_id)
    }
}