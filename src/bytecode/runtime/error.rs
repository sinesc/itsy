use crate::prelude::*;
use crate::StackAddress;

/// Represents the various possible runtime error-kinds.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RuntimeErrorKind {
    NotReady,
    CannotClear,
    UnexpectedReady,
    HeapCorruption,
    Panic,
    IntegerOverflow,
    DivisionByZero,
    InvalidArgument,
}

/// An error reported by the runtime.
#[derive(Clone, Debug)]
pub struct RuntimeError {
    kind: RuntimeErrorKind,
    offset: StackAddress, // TODO: pc/sp here
    opcode: Option<String>,
}

impl RuntimeError {
    pub(crate) fn new(offset: StackAddress, kind: RuntimeErrorKind, opcode: Option<String>) -> RuntimeError {
        Self { kind, offset, opcode }
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
        let opcode = if let Some(opcode) = &self.opcode {
            opcode.clone()
        } else {
            self.offset.to_string()
        };
        #[allow(unreachable_patterns)]
        match &self.kind {
            RuntimeErrorKind::NotReady => write!(f, "VM is not ready."),
            RuntimeErrorKind::CannotClear => write!(f, "Cannot clear VM error. VM is not in a resumable state."),
            RuntimeErrorKind::UnexpectedReady => write!(f, "VM returned before reaching an exit or yld instruction."),
            RuntimeErrorKind::HeapCorruption => write!(f, "Heap elements remaining after program termination."),
            RuntimeErrorKind::Panic => write!(f, "Panic at opcode {opcode}"),
            RuntimeErrorKind::IntegerOverflow => write!(f, "Integer overflow at opcode {opcode}"),
            RuntimeErrorKind::DivisionByZero => write!(f, "Divison by zero at opcode {opcode}"),
            RuntimeErrorKind::InvalidArgument => write!(f, "Invalid argument at opcode {opcode}"),
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;