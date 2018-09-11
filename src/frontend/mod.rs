//! Compiler frontend. Generates a type-checked and resolved AST.

pub mod ast;

mod parser;
pub use frontend::parser::{parse, ParseError};

mod resolver;
pub use frontend::resolver::{resolve, ResolvedProgram};

pub mod util;
