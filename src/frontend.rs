//! Type resolution and AST generation.

pub mod ast;

mod parser;
pub use crate::frontend::parser::{parse, ParsedProgram, ParseError, ParseErrorKind};

mod resolver;
pub use crate::frontend::resolver::{resolve, ResolvedProgram, error::ResolveError, error::ResolveErrorKind};

pub mod resolved {
    //! Resolved information to be used by the bytecode generator.
    pub use crate::util::{Bindings, BindingId, FunctionId, TypeId, ScopeId};
    pub mod types {
        //! Resolved program type information.
        pub use crate::util::{Type, Struct, Enum, Array, Numeric};
    }
}

pub use crate::util::Numeric;