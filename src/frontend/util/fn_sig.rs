use super::TypeId;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FnKind {
    Internal,
    Rust(u16),
}

#[derive(Clone, Debug)]
pub struct FnSig {
    pub ret_type: Option<TypeId>,
    pub arg_type: Vec<TypeId>,
    pub kind    : FnKind,
}

impl FnSig {
    /*
    pub fn is_internal(self: &Self) -> bool {
        self.kind == FnKind::Internal
    }
    pub fn is_rust(self: &Self) -> bool {
        match self.kind {
            FnKind::Rust(_) => true,
            _ => false,
        }
    }
    */
    pub fn rust_fn_index(self: &Self) -> Option<u16> {
        match self.kind {
            FnKind::Rust(index) => Some(index),
            _ => None,
        }
    }
}