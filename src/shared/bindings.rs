use crate::shared::{TypeContainer, types::Type, infos::BindingInfo, typed_ids::{TypeId, BindingId}};

/// Program binding data. // FIXME: naming is bad. this is type information
pub struct Bindings {
    /// Maps binding ids to binding info descriptors.
    binding_map : Vec<BindingInfo>,  // HashMap<BindingId, BindingInfo>
    /// Maps type ids to types.
    type_map    : Vec<Type>,    // HashMap<TypeId, Type>
}

impl Bindings {
    pub(crate) fn new(binding_map: Vec<BindingInfo>, type_map: Vec<Type>) -> Self {
        for info in binding_map.iter() {
            info.type_id.expect("Unresolved binding type encountered.");
        }
        Self {
            binding_map,
            type_map
        }
    }
    /// Returns the TypeId of the given binding.
    pub fn binding_type_id(self: &Self, binding_id: BindingId) -> TypeId {
        let binding_index = Into::<usize>::into(binding_id);
        self.binding_map[binding_index].type_id.unwrap()
    }
    /// Returns the type of the given binding.
    pub fn binding_type(self: &Self, binding_id: BindingId) -> &Type {
        let type_id = self.binding_type_id(binding_id);
        &self.type_map[Into::<usize>::into(type_id)]
    }
    /*/// Returns the mutability of the given binding.
    pub fn binding_mutable(self: &Self, binding_id: BindingId) -> bool {
        let binding_index = Into::<usize>::into(binding_id);
        self.binding_map[binding_index].mutable
    }*/
    pub fn types(self: &Self) -> &[Type] {
        &self.type_map
    }
    pub fn len(self: &Self) -> usize {
        self.binding_map.len()
    }
}

/// Support TypeContainer for Bindings so that methods that need to follow type_ids can be implemented once and be used in both
/// the Resolver where types are scored in Scopes and the Compiler where types are a stored in a Vec.
impl TypeContainer for Bindings {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type {
        let index: usize = type_id.into();
        &self.type_map[index]
    }
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type {
        let index: usize = type_id.into();
        &mut self.type_map[index]
    }
}