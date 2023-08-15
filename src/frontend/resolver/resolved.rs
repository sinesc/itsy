
use crate::prelude::*;
use crate::shared::meta::{Type, Trait, ImplTrait, Function, Binding, Constant};
use crate::shared::typed_ids::{BindingId, TypeId, FunctionId, ConstantId};
use crate::frontend::parser::types::ParsedModule;

/// Parsed program AST with all types, bindings and other language structures resolved.
pub struct ResolvedProgram<T> {
    /// Programs are generic over their Rust API.
    pub(crate) ty: PhantomData<T>,
    /// AST of all program modules.
    pub modules: Vec<ParsedModule>,
    /// Maps typed ids to program meta data.
    pub resolved: Resolved,
    /// `FunctionId` of the entry/main function.
    pub entry_fn: FunctionId,
}

/// Resolved program meta data.
pub struct Resolved {
    /// Maps binding ids to binding info descriptors.
    binding_map : Vec<Binding>,
    /// Maps type ids to types.
    type_map    : Vec<Type>,
    /// Maps constant ids to constants.
    constant_map: Vec<Constant>,
    /// Maps function ids to functions.
    function_map: Vec<Function>,
}

impl Debug for Resolved {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Resolved")
            .field("binding_map", &self.bindings().collect::<Map<_, _>>())
            .field("constant_map", &self.constants().collect::<Map<_, _>>())
            .field("type_map", &self.types().collect::<Map<_, _>>())
            .field("function_map", &self.functions().collect::<Map<_, _>>())
            .finish()
    }
}

impl Resolved {
    pub(crate) fn new(binding_map: Vec<Binding>, type_map: Vec<Type>, constant_map: Vec<Constant>, function_map: Vec<Function>) -> Self {
        Self {
            binding_map,
            type_map,
            constant_map,
            function_map,
        }
    }
    /// Returns an iterator over the function mapping.
    pub fn functions(self: &Self) -> impl Iterator<Item=(FunctionId, &Function)> {
        self.function_map.iter().enumerate().map(|(index, info)| (FunctionId::from(index), info))
    }
    /// Returns an iterator over the type mapping.
    pub fn types(self: &Self) -> impl Iterator<Item=(TypeId, &Type)> {
        self.type_map.iter().enumerate().map(|(index, info)| (TypeId::from(index), info))
    }
    /// Returns an iterator over the type mapping returning only traits.
    pub fn traits(self: &Self) -> impl Iterator<Item=(TypeId, &Trait)> {
        self.type_map.iter()
            .enumerate()
            .filter_map(|(type_id, ty)| ty.as_trait().map(|trt| (TypeId::from(type_id), trt)))
    }
    /// Returns an iterator over the type mapping returning only traits implementors.
    pub fn implementors(self: &Self) -> impl Iterator<Item=(TypeId, &Map<TypeId, ImplTrait>)> {
        // TODO: currently only structs support traits. this will have to be extended to at least String and enums once those exist.
        self.type_map.iter()
            .enumerate()
            .filter_map(|(type_id, ty)| ty.as_struct().map(|struct_| (TypeId::from(type_id), struct_)))
            .filter_map(|i| if i.1.impl_traits.len() > 0 { Some((i.0, &i.1.impl_traits)) } else { None })
    }
    /// Returns an iterator over the binding mapping.
    pub fn bindings(self: &Self) -> impl Iterator<Item=(BindingId, &Binding)> {
        self.binding_map.iter().enumerate().map(|(index, info)| (BindingId::from(index), info))
    }
    /// Returns an iterator over the constant mapping.
    pub fn constants(self: &Self) -> impl Iterator<Item=(ConstantId, &Constant)> {
        self.constant_map.iter().enumerate().map(|(index, info)| (ConstantId::from(index), info))
    }
    /// Returns function information for given function id.
    pub fn function(self: &Self, function_id: FunctionId) -> &Function {
        &self.function_map[Into::<usize>::into(function_id)]
    }
    /// Returns binding information for given binding id.
    pub fn binding(self: &Self, binding_id: BindingId) -> &Binding {
        &self.binding_map[Into::<usize>::into(binding_id)]
    }
    /// Returns type information for given type id.
    pub fn ty(self: &Self, type_id: TypeId) -> &Type {
        &self.type_map[Into::<usize>::into(type_id)]
    }
    /// Returns constant information for given constant id.
    pub fn constant(self: &Self, constant_id: ConstantId) -> &Constant {
        &self.constant_map[Into::<usize>::into(constant_id)]
    }
}
