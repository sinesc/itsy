pub mod ast;
pub mod parser;
pub mod resolver;
pub mod integer;

use util::TypeId;
use std::fmt::{self, Debug};

#[derive(Copy, Clone, PartialEq)]
pub enum Unresolved {
    Unknown,
    Void,
    Resolved(TypeId)
}

impl Unresolved {
    pub fn is_unknown(self: &Self) -> bool {
        match self {
            Unresolved::Unknown => true,
            _ => false,
        }
    }
    pub fn is_void(self: &Self) -> bool {
        match self {
            Unresolved::Void => true,
            _ => false,
        }
    }
    pub fn is_resolved(self: &Self) -> bool {
        match self {
            Unresolved::Resolved(_) => true,
            _ => false,
        }
    }
}

impl Debug for Unresolved {
    fn fmt(self: &Self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Unresolved::Unknown => write!(f, "Unknown"),
            Unresolved::Void => write!(f, "Void"),
            Unresolved::Resolved(type_id) => write!(f, "Resolved({:?})", type_id),
        }
    }
}