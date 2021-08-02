use crate::shared::typed_ids::TypeId;

/// Binding meta information
pub struct BindingInfo {
    pub mutable: bool,
    pub type_id: Option<TypeId>,
}

#[derive(Clone)]
pub struct FunctionInfo {
    pub kind    : Option<FunctionKind>,
    pub arg_type: Vec<Option<TypeId>>,
    pub ret_type: Option<TypeId>,
}

impl FunctionInfo {
    pub fn rust_fn_index(self: &Self) -> Option<u16> {
        match self.kind {
            Some(FunctionKind::Rust(index)) => Some(index),
            _ => None,
        }
    }
    pub fn is_resolved(self: &Self) -> bool {
        self.ret_type.is_some() && self.kind.is_some() && !self.arg_type.iter().any(|arg| arg.is_none())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FunctionKind {
    Function,
    Method(TypeId),
    Rust(u16),
    Intrinsic(Intrinsic),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Intrinsic {
    ArrayLen,
}