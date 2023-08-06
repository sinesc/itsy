//! Bytecode macros.

pub mod impl_opcodes;
pub(crate) use impl_opcodes::impl_opcodes;

pub mod impl_builtins;
pub(crate) use impl_builtins::impl_builtins;