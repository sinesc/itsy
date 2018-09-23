use crate::frontend::util::TypeId;
use std::fmt::{self, Debug};

/// Used in the AST to represent an unresolved or resolved Type or the lack of a type (void).
#[derive(Copy, Clone, PartialEq)]
pub enum TypeSlot {
    Unresolved,
    Void,
    TypeId(TypeId)
}

impl TypeSlot {
    pub fn is_unresolved(self: &Self) -> bool {
        match self {
            TypeSlot::Unresolved => true,
            _ => false,
        }
    }
    pub fn is_void(self: &Self) -> bool {
        match self {
            TypeSlot::Void => true,
            _ => false,
        }
    }
    pub fn is_type(self: &Self) -> bool {
        match self {
            TypeSlot::TypeId(_) => true,
            _ => false,
        }
    }
    pub fn unwrap(self: &Self) -> TypeId {
        match self {
            TypeSlot::TypeId(id) => *id,
            TypeSlot::Void => panic!("Tried to unwrap Void TypeSlot"),
            TypeSlot::Unresolved => panic!("Tried to unwrap Unresolved TypeSlot"),
        }
    }
}

impl Debug for TypeSlot {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeSlot::Unresolved => write!(f, "Unresolved"),
            TypeSlot::Void => write!(f, "Void"),
            TypeSlot::TypeId(type_id) => write!(f, "{:?}", type_id),
        }
    }
}