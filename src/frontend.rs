//! Compiler frontend. Generates a type-checked and resolved AST.

pub mod ast;

/// Parsed program AST.
pub type Program<'a> = Vec<ast::Statement<'a>>;

mod parser;
pub use crate::frontend::parser::{parse, ParseError};

mod resolver;
pub use crate::frontend::resolver::{resolve, ResolvedProgram};

pub(crate)mod util;

pub mod lookup {
    //! Newtypes for ids representing items in the AST.
    pub use super::util::{BindingId, FunctionId, TypeId};
}

pub mod resolved {
    //! Resolved information to be used by the bytecode generator.
    pub use super::util::{Type, Struct, Enum};
}