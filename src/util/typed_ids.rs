/// Unique numeric type id.
#[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
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

/// Unique numeric id of a scope.
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
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

/// Unique numeric id of a binding.
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
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