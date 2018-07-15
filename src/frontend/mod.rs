pub mod ast;
pub mod parser;
pub mod resolver;
pub mod integer;

use util::TypeId;
use std::fmt::{self, Debug};

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
}

impl Debug for TypeSlot {
    fn fmt(self: &Self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeSlot::Unresolved => write!(f, "Unresolved"),
            TypeSlot::Void => write!(f, "Void"),
            TypeSlot::TypeId(type_id) => write!(f, "TypeId({:?})", type_id),
        }
    }
}