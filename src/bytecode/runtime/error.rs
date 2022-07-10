use crate::prelude::*;
use crate::StackAddress;

/// Represents the various possible runtime error-kinds.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RuntimeErrorKind {
    NotReady,
    UnexpectedReady,
    HeapCorruption,
    Panic,
}

/// An error reported by the runtime.
#[derive(Clone, Debug)]
pub struct RuntimeError {
    kind: RuntimeErrorKind,
    offset: StackAddress, // TODO: pc/sp here
}

impl RuntimeError {
    pub(crate) fn new(offset: StackAddress, kind: RuntimeErrorKind) -> RuntimeError {
        Self { kind, offset }
    }
    /// The kind of the error.
    pub fn kind(self: &Self) -> &RuntimeErrorKind {
        &self.kind
    }
    /// Opcode offset of the error.
    pub fn offset(self: &Self) -> StackAddress {
        self.offset
    }
}

impl Display for RuntimeError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[allow(unreachable_patterns)]
        match &self.kind {
            RuntimeErrorKind::NotReady => write!(f, "VM state is not ready."),
            RuntimeErrorKind::HeapCorruption => write!(f, "Heap elements remaining after program termination."),
            RuntimeErrorKind::Panic => write!(f, "Panic at opcode offset {}.", self.offset),
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;