//! Bytecode interpretation.

#[macro_use]
mod vm;
mod stack;
mod heap;
mod traits;

pub use self::vm::VM;
pub use self::vm::VMState;
pub(crate) use self::vm::CopyTarget;
pub use self::stack::*;
pub use self::heap::*;
pub use self::traits::*;
pub use crate::util::{HeapRef, HeapSlice};
