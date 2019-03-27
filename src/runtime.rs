//! Bytecode interpretation.

#[macro_use]
mod vm;
mod stack;
mod heap;
mod traits;

pub use self::vm::{VM, VMState};
pub use self::stack::*;
pub use self::heap::*;
pub use self::traits::*;

/// A stack value.
pub(crate) type Value = i32;
pub(crate) type Value64 = i64;