//! Type resolution and AST generation.

pub mod ast;

mod parser;
pub use crate::frontend::parser::{parse, ParseError, ParseErrorKind, ParsedProgram};

mod resolver;
pub use crate::frontend::resolver::{resolve, ResolvedProgram, ResolveError};

pub mod lookup {
    //! Newtypes for ids representing items in the AST.
    pub use crate::util::{BindingId, FunctionId, TypeId};
}

pub mod resolved {
    //! Resolved information to be used by the bytecode generator.
    pub use crate::util::{Type, Struct, Enum};
}