//! Compiler frontend. Generates a type-checked and resolved AST.

pub mod ast;

mod parser;
pub use frontend::parser::{parse, ParseError};

mod resolver;
pub use frontend::resolver::{resolve, ResolvedProgram};

pub(crate)mod util;

pub mod lookup {
    //! Newtypes for ids representing items in the AST.
    pub use super::util::{BindingId, FunctionId, TypeId};
}

pub mod resolved {
    //! Resolved information to be used by the bytecode generator.
    pub use super::util::{Type, Struct, Enum};
}