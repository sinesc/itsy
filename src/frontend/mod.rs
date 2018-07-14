pub mod ast;
pub mod parser;
pub mod resolver;
pub mod integer;

use util::TypeId;

#[derive(Copy, Clone, Debug, PartialEq)]
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
