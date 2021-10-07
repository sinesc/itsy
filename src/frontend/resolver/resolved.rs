
use std::marker::PhantomData;
use crate::shared::types::Type;
use crate::shared::{infos::{BindingInfo, FunctionInfo}, typed_ids::{BindingId, TypeId, FunctionId}};
use crate::frontend::parser::types::ParsedModule;

/// Parsed program AST with all types, bindings and other language structures resolved.
pub struct ResolvedProgram<T> {
    /// Programs are generic over their Rust API
    pub(crate) ty: PhantomData<T>,
    /// Program AST with resolved `BindingId`s.
    pub ast: ParsedModule,
    /// Maps typed ids to program meta data.
    pub id_mappings: IdMappings,
    /// `FunctionId` of the entry/main function.
    pub entry_fn: FunctionId,
}

/// Resolved program meta data.
pub struct IdMappings {
    /// Maps binding ids to binding info descriptors.
    pub(crate) binding_map : Vec<BindingInfo>,
    /// Maps type ids to types.
    pub(crate) type_map    : Vec<Type>,
    /// Maps function ids to functions.
    pub(crate) function_map: Vec<FunctionInfo>,
}

impl IdMappings {
    pub(crate) fn new(binding_map: Vec<BindingInfo>, type_map: Vec<Type>, function_map: Vec<FunctionInfo>) -> Self {
        for info in binding_map.iter() {
            info.type_id.expect("Unresolved binding type encountered.");
        }
        for info in function_map.iter() {
            if !info.is_resolved() { panic!("Unresolved binding type encountered."); }
        }
        Self {
            binding_map,
            type_map,
            function_map,
        }
    }
    /// Returns function information for given function id.
    pub fn function(self: &Self, function_id: FunctionId) -> &FunctionInfo {
        &self.function_map[Into::<usize>::into(function_id)]
    }
    /// Returns binding information for given binding id.
    pub fn binding(self: &Self, binding_id: BindingId) -> &BindingInfo {
        &self.binding_map[Into::<usize>::into(binding_id)]
    }
    /// Returns type information for given type id.
    pub fn ty(self: &Self, type_id: TypeId) -> &Type {
        &self.type_map[Into::<usize>::into(type_id)]
    }
}
