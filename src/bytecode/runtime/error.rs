use crate::prelude::*;
use crate::StackAddress;

/// Represents the various possible runtime error-kinds.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RuntimeErrorKind {
    NotReady,
    CannotClear,
    UnexpectedReady,
    HeapCorruption,
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
        match &self.kind {
            RuntimeErrorKind::NotReady => write!(f, "VM is not ready."),
            RuntimeErrorKind::CannotClear => write!(f, "Cannot clear VM error. VM is not in a resumable state."),
            RuntimeErrorKind::UnexpectedReady => write!(f, "VM returned before reaching an exit or yld instruction."),
            RuntimeErrorKind::HeapCorruption => write!(f, "Heap elements remaining after program termination."),
            RuntimeErrorKind::IntegerOverflow => write!(f, "Integer overflow at opcode {opcode}."),
            RuntimeErrorKind::DivisionByZero => write!(f, "Divison by zero at opcode {opcode}."),
            RuntimeErrorKind::InvalidArgument => write!(f, "Invalid argument at opcode {opcode}."),
        }
    }
}

pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;

/// An error reported by [`VM::call_function`](crate::runtime::VM::call_function).
#[derive(Clone, Debug)]
pub enum CallError {
    /// No top-level function with the given name exists in the program.
    FunctionNotFound(String),
    /// The number of supplied arguments does not match the function's parameter count.
    ArgumentCountMismatch { expected: usize, got: usize },
    /// The argument at `index` could not be marshalled to the expected Itsy type.
    ArgumentTypeMismatch { index: usize, expected: &'static str },
    /// An argument or return type is not yet supported for host calls (e.g. arrays, structs, enums).
    UnsupportedType { expected: &'static str },
    /// The function triggered a runtime error while executing.
    Runtime(RuntimeError),
}

impl Display for CallError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CallError::FunctionNotFound(name) => write!(f, "No callable function named '{name}'."),
            CallError::ArgumentCountMismatch { expected, got } => write!(f, "Expected {expected} argument(s), got {got}."),
            CallError::ArgumentTypeMismatch { index, expected } => write!(f, "Argument {index} could not be marshalled to expected type '{expected}'."),
            CallError::UnsupportedType { expected } => write!(f, "Type '{expected}' is not supported for host calls."),
            CallError::Runtime(err) => write!(f, "Runtime error during call: {err}"),
        }
    }
}

pub type CallResult<T = Box<dyn std::any::Any>> = Result<T, CallError>;