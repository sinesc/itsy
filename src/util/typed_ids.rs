/// Unique numeric type id.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct TypeId(usize);

impl TypeId {
    pub fn void() -> Self {
        TypeId(0)
    }
}

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


/// Unique numeric scope id.
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