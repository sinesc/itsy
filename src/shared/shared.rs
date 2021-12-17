//! Shared code for frontend and bytecode

pub mod numeric;
pub mod typed_ids;
pub mod types;
pub mod error;
#[cfg(feature="compiler")]
pub mod infos;

use crate::prelude::*;
use crate::shared::{typed_ids::TypeId, types::{Type, Array}};
#[cfg(feature="compiler")]
use crate::shared::{infos::BindingInfo, typed_ids::BindingId};

/// A container holding type id to type mappings
pub(crate) trait TypeContainer {
    /// Returns a reference to the type.
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type;
    /// Returns a mutable reference to the type.
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type;
    /// Returns whether type_id is acceptable by the type expectation for expected_type_id (but not necessarily the inverse).
    fn types_match(self: &Self, type_id: TypeId, expected_type_id: TypeId) -> bool {
        if type_id == expected_type_id {
            true
        } else {
            match (self.type_by_id(type_id), self.type_by_id(expected_type_id)) {
                (&Type::Array(Array { len: a_len, type_id: Some(a_type_id) }), &Type::Array(Array { len: b_len, type_id: Some(b_type_id) })) => {
                    a_len == b_len && self.types_match(a_type_id, b_type_id)
                },
                (Type::Struct(struct_), Type::Trait(_)) => {
                    struct_.impl_traits.contains_key(&expected_type_id)
                },
                _ => false,
            }
        }
    }
}

/// A container holding binding id to BindingInfo mappings
#[cfg(feature="compiler")]
pub(crate) trait BindingContainer {
    /// Returns a reference to the type id.
    fn binding_by_id(self: &Self, binding_id: BindingId) -> &BindingInfo;
    /// Returns a mutable reference to the type id.
    fn binding_by_id_mut(self: &mut Self, binding_id: BindingId) -> &mut BindingInfo;
}

#[derive(Copy, Clone, PartialEq)]
#[cfg(feature="compiler")]
pub(crate) struct Progress {
    pub current: usize,
    pub total: usize,
}

#[cfg(feature="compiler")]
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

#[cfg(feature="compiler")]
impl fmt::Debug for Progress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.current, self.total)
    }
}

#[cfg(feature="compiler")]
impl Add for Progress {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            current: self.current + other.current,
            total: self.total + other.total,
        }
    }
}

/// References two elements of a slice mutably
pub fn index_twice<T>(slice: &mut [T], a: usize, b: usize) -> (&mut T, &mut T) {
    if a == b {
        panic!("tried to index element {} twice", a);
    } else if a >= slice.len() || b >= slice.len() {
        panic!("index ({}, {}) out of bounds", a, b);
    } else {
        if a > b {
            let (left, right) = slice.split_at_mut(a);
            (&mut right[0], &mut left[b])
        } else {
            let (left, right) = slice.split_at_mut(b);
            (&mut left[a], &mut right[0])
        }
    }
}

/// Splits a path string into its constituent parts.
#[cfg(feature="compiler")]
pub fn path_to_parts<T: AsRef<str>>(path: T) -> Vec<String> {
    let path = path.as_ref();
    match path {
        "" => Vec::new(),
        _ => path.split("::").map(|s| s.to_string()).collect()
    }
}

/// Joins parts of a path into a string.
#[cfg(feature="compiler")]
pub fn parts_to_path<T: AsRef<str>>(parts: &[T]) -> String {
    let parts = parts.iter().map(|p| p.as_ref()).collect::<Vec<_>>(); // todo: lame to have to collect first
    parts.join("::")
}

#[test]
fn test_index_twice() {
    let mut data = [ 1i32, 2, 3, 4, 5];

    {
        let (a, b) = index_twice(&mut data, 0, 4);  // b > a at bounds
        *a = -1;
        *b = -5;
    }
    assert!(data == [ -1i32, 2, 3, 4, -5 ]);

    {
        let (a, b) = index_twice(&mut data, 4, 0);  // a > b at bounds
        *a = -10;
        *b = -5;
    }
    assert!(data == [ -5i32, 2, 3, 4, -10 ]);

    {
        let (a, b) = index_twice(&mut data, 3, 2);  // a > b adjacent
        *a = 11;
        *b = 22;
    }
    assert!(data == [ -5i32, 2, 22, 11, -10 ]);

    {
        let (a, b) = index_twice(&mut data, 1, 2);  // b > a adjacent
        *a = 33;
        *b = 44;
    }
    assert!(data == [ -5i32, 33, 44, 11, -10 ]);
}
