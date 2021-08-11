//! Shared code for frontend and bytecode

pub mod numeric;
pub mod typed_ids;
pub mod types;
pub mod error;
pub mod infos;
pub mod bindings;

use std::ops::Add;
use std::fmt;
use crate::frontend::ast::Position;
use crate::{StackAddress, shared::{typed_ids::TypeId, types::Type}};

/// A container holding type id to type mappings
pub(crate) trait TypeContainer {
    /// Returns a reference to the type id.
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type;
    /// Returns a mutable reference to the type id.
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type;
    /// Computes the size of given type.
    fn type_size(self: &Self, ty: &Type) -> StackAddress { // FIXME: remove this and its usages. cannot be correct anymore due to type storage changes
        match ty {
            Type::Array(a)  => {
                let element_type = self.type_by_id(a.type_id.unwrap());
                let element_size = self.type_size(element_type);
                element_size * a.len.unwrap()
            },
            Type::Struct(s) => {
                s.fields.iter().fold(0, |acc, f| acc + self.type_size(self.type_by_id(f.1.unwrap())))
            },
            Type::Enum(_)   => unimplemented!("enum size"),
            _               => ty.primitive_size() as StackAddress
        }
    }
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

/// Compute line/column number from absolute offset in string
pub fn compute_loc(input: &str, offset: Position) -> (Position, Position) {
    let mut parsed = &input[0..offset as usize];
    let mut line = 1;
    while { // can't use let parsed.lines() here as a line-break at the end is ignored
        let mut break_char = '\0';
        if let Some(nl) = parsed.find(|c| if c == '\n' || c == '\r' { break_char = c; true } else { false }) {
            parsed = &parsed[nl+1..];
            if break_char == '\r' && parsed.starts_with('\n') { // skip \n after \r on windows
                parsed = &parsed[1..];
            }
            line += 1;
            true
        } else {
            false
        }
    } {}
    (line as Position, parsed.len() as Position + 1)
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
