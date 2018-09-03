//! Compiler frontend. Generates a type-checked and resolved AST.

pub mod ast;
mod parser;
mod resolver;

pub use frontend::resolver::{resolve, ResolvedProgram};
pub use frontend::parser::{parse, ParseError};