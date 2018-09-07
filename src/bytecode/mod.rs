pub mod gen;
pub mod interp;
#[macro_use]
mod macros;

pub use self::macros::*;

opcode!(op_const, const_id: u8);
opcode!(op_const_long, const_id: u16);