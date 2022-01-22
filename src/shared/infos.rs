use crate::shared::typed_ids::TypeId;
use crate::RustFnIndex;

/// Binding meta information.
pub struct BindingInfo {
    pub mutable: bool,
    pub type_id: Option<TypeId>,
}

/// Function mata information.
#[derive(Clone)]
pub struct FunctionInfo {
    pub kind    : Option<FunctionKind>,
    pub arg_type: Vec<Option<TypeId>>,
    pub ret_type: Option<TypeId>,
}

impl FunctionInfo {
    pub fn rust_fn_index(self: &Self) -> Option<RustFnIndex> {
        match self.kind {
            Some(FunctionKind::Rust(index)) => Some(index),
            _ => None,
        }
    }
    pub fn is_resolved(self: &Self) -> bool {
        self.ret_type.is_some() && self.kind.is_some() && self.arg_type.iter().all(|arg| arg.is_some())
    }
}

/// The kind of a function described by a FunctionInfo.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FunctionKind {
    Function,
    Method(TypeId),
    Rust(RustFnIndex),
    Intrinsic(TypeId, Intrinsic),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Intrinsic {
    ArrayLen,
    ArrayPush,
    ArrayPop,
    ArrayTruncate,
}