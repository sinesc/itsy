pub mod ast;
pub mod parser;
pub mod check;

#[derive(Debug)]
pub enum Unresolved<T> {
    Unknown,
    Maybe(T),
    Resolved(T)
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
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