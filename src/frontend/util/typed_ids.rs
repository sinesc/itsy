//! Various new-typed usize unique identifiers.

use std::fmt::{self, Debug};

/// Unique numeric id of a type.
#[derive(Copy, Clone, Default, PartialEq, PartialOrd)]
pub struct TypeId(usize);

impl From<TypeId> for usize {
    fn from(input: TypeId) -> usize {
        input.0
    }
}

impl From<usize> for TypeId {
    fn from(input: usize) -> TypeId {
        TypeId(input)
    }
}

impl Debug for TypeId {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeId({})", self.0)
    }
}

/// Unique numeric id of a scope.
#[derive(Copy, Clone, Default, Eq, Hash, PartialEq)]
pub struct ScopeId(usize);

impl From<ScopeId> for usize {
    fn from(input: ScopeId) -> usize {
        input.0
    }
}

impl From<usize> for ScopeId {
    fn from(input: usize) -> ScopeId {
        ScopeId(input)
    }
}

impl Debug for ScopeId {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ScopeId({})", self.0)
    }
}

/// Unique numeric id of a variable binding.
#[derive(Copy, Clone, Default, Eq, Hash, PartialEq)]
pub struct BindingId(usize);

impl From<BindingId> for usize {
    fn from(input: BindingId) -> usize {
        input.0
    }
}

impl From<usize> for BindingId {
    fn from(input: usize) -> BindingId {
        BindingId(input)
    }
}

impl Debug for BindingId {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BindingId({})", self.0)
    }
}

/// Unique numeric id of a function.
#[derive(Copy, Clone, Default, Eq, Hash, PartialEq)]
pub struct FunctionId(usize);

impl From<FunctionId> for usize {
    fn from(input: FunctionId) -> usize {
        input.0
    }
}

impl From<usize> for FunctionId {
    fn from(input: usize) -> FunctionId {
        FunctionId(input)
    }
}

impl Debug for FunctionId {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FunctionId({})", self.0)
    }
}