//! Type resolution and AST generation.

pub mod ast;

#[path="parser/parser.rs"]
pub mod parser;

#[path="resolver/resolver.rs"]
pub mod resolver;

#[path="ast_visitor.rs"]
pub mod ast_visitor;