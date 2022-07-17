//! Shared code for frontend and bytecode

pub mod numeric;
pub mod typed_ids;
pub mod meta;
pub mod error;

use crate::shared::{typed_ids::TypeId, meta::{Type, Array}};
use crate::{prelude::*, shared::{typed_ids::BindingId, meta::Binding}};

/// A container holding type id to type mappings
pub trait TypeContainer {
    /// Returns a reference to the type.
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type;
    /// Returns a mutable reference to the type.
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type;
    /// Returns the flat type name or None if that cannot represent a type (e.g. an array)
    fn type_flat_name(self: &Self, type_id: TypeId) -> Option<&String>;
    /// Returns whether given_type_id is acceptable to a binding of the accepted_type_id, e.g. [ u8; 3 ] is acceptable to a [ u8 ] binding, but not the inverse.
    fn type_accepted_for(self: &Self, given_type_id: TypeId, accepted_type_id: TypeId) -> bool {
        if given_type_id == accepted_type_id {
            true
        } else {
            match (self.type_by_id(given_type_id), self.type_by_id(accepted_type_id)) {
                (&Type::Array(Array { type_id: Some(given_type_id), .. }), &Type::Array(Array { type_id: Some(accepted_type_id), .. })) => {
                    self.type_accepted_for(given_type_id, accepted_type_id)
                },
                (Type::Struct(struct_), Type::Trait(_)) => {
                    struct_.impl_traits.contains_key(&accepted_type_id)
                },
                _ => false,
            }
        }
    }
    /// Returns whether the given types are the same. (Anonymous types may have differing type ids but still be the same type)
    fn type_equals(self: &Self, first_type_id: TypeId, second_type_id: TypeId) -> bool {
        if first_type_id == second_type_id {
            true
        } else {
            match (self.type_by_id(first_type_id), self.type_by_id(second_type_id)) {
                (&Type::Array(Array { type_id: Some(first_type_id), .. }), &Type::Array(Array { type_id: Some(second_type_id), .. })) => {
                    self.type_equals(first_type_id, second_type_id)
                },
                _ => false,
            }
        }
    }
    // Returns recursively resolved type name.
    fn type_name(self: &Self, type_id: TypeId) -> String {
        let ty = self.type_by_id(type_id);
        if let Some(type_name) = self.type_flat_name(type_id) {
            type_name.to_string()
        } else {
            match ty {
                Type::void => "void".to_string(), // void is explicitly not a named type so we have to manually give it a name here
                &Type::Array(Array { type_id: Some(type_id) }) => {
                    format!("[ {} ]", self.type_name(type_id))
                }
                Type::Array(Array { type_id: None }) => {
                    "[ _ ]".to_string()
                }
                _ => "?".to_string()
            }
        }
    }
}

/// A container holding binding id to Binding mappings
pub trait BindingContainer {
    /// Returns a reference to the type id.
    fn binding_by_id(self: &Self, binding_id: BindingId) -> &Binding;
    /// Returns a mutable reference to the type id.
    fn binding_by_id_mut(self: &mut Self, binding_id: BindingId) -> &mut Binding;
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) struct Progress {
    pub current: usize,
    pub total: usize,
}

impl Progress {
    pub fn new(current: usize, total: usize) -> Self {
        Self { current, total }
    }
    pub fn zero() -> Self {
        Self { current: 0, total: 0 }
    }
    pub fn done(self: &Self) -> bool {
        self.current == self.total
    }
}

impl fmt::Debug for Progress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.current, self.total)
    }
}

impl Add for Progress {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            current: self.current + other.current,
            total: self.total + other.total,
        }
    }
}

/// Splits a path string into its constituent parts.
pub fn path_to_parts<T: AsRef<str>>(path: T) -> Vec<String> {
    let path = path.as_ref();
    match path {
        "" => Vec::new(),
        _ => path.split("::").map(|s| s.to_string()).collect()
    }
}

/// Joins parts of a path into a string.
pub fn parts_to_path<T: AsRef<str>>(parts: &[T]) -> String {
    let parts = parts.iter().map(|p| p.as_ref()).collect::<Vec<_>>(); // todo: lame to have to collect first
    parts.join("::")
}
