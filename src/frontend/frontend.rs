//! Type resolution and AST generation.

pub mod ast;

#[path="parser/parser.rs"]
pub mod parser;

#[path="resolver/resolver.rs"]
pub mod resolver;