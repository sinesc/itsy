use crate::prelude::*;
use crate::frontend::ast::Position;
use std::{rc::Rc, io::Error as IOError};

/// Represents the various possible parser error-kinds.
#[derive(Clone, Debug)]
pub enum ParseErrorKind {
    SyntaxError,
    InvalidNumerical,
    IllegalVariantMix,
    IllegalImplBlock,
    IllegalStructDef,
    IllegalModuleDef,
    IllegalTraitDef,
    IllegalEnumDef,
    IllegalFunction,
    IllegalReturn,
    IllegalIfBlock,
    IllegalLetStatement,
    IllegalForLoop,
    IllegalWhileLoop,
    IllegalBreak,
    IllegalContinue,
    IllegalClosure,
    DisabledFeature(&'static str),
    IOError(Rc<IOError>), // io::Error fails to implement clone
}


/// An error reported by the parser (e.g. syntax error).
#[derive(Clone, Debug)]
pub struct ParseError {
    pub(crate) kind: ParseErrorKind,
    position: Position,
    module_path: String,
}

impl From<IOError> for ParseError {
    fn from(error: IOError) -> ParseError {
        ParseError {
            kind: ParseErrorKind::IOError(Rc::new(error)), // io::Error is still not cloneable
            position: Position(0),
            module_path: "".to_string(),
        }
    }
}

impl ParseError {
    pub(crate) fn new(kind: ParseErrorKind, position: Position, module_path: &str) -> ParseError {
        Self { kind, position, module_path: module_path.to_string() }
    }
    /// Compute 1-based line/column number in string.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        self.position.loc(input)
    }
    /// The kind of the error.
    pub fn kind(self: &Self) -> &ParseErrorKind {
        &self.kind
    }
    /// Path to the module where the error occured.
    pub fn module_path(self: &Self) -> &str {
        &self.module_path
    }
}

impl Display for ParseError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParseErrorKind::SyntaxError => write!(f, "Syntax error"),
            ParseErrorKind::InvalidNumerical => write!(f, "Invalid numeric value"),
            ParseErrorKind::IllegalVariantMix => write!(f, "Illegal mix of enum data and value variants"),
            ParseErrorKind::IllegalImplBlock => write!(f, "Impl-blocks are not allowed in this position"),
            ParseErrorKind::IllegalStructDef => write!(f, "Struct definitions are not allowed in this position"),
            ParseErrorKind::IllegalModuleDef => write!(f, "Module definitions are not allowed in this position"),
            ParseErrorKind::IllegalTraitDef => write!(f, "Trait definitions are not allowed in this position"),
            ParseErrorKind::IllegalEnumDef => write!(f, "Enum definitions are not allowed in this position"),
            ParseErrorKind::IllegalFunction => write!(f, "Function definitions are not allowed in this position"),
            ParseErrorKind::IllegalReturn => write!(f, "Return-statements are not allowed outside of functions"),
            ParseErrorKind::IllegalIfBlock => write!(f, "If-blocks are not allowed outside of functions"),
            ParseErrorKind::IllegalLetStatement => write!(f, "Let-statements are not allowed outside of functions"),
            ParseErrorKind::IllegalForLoop => write!(f, "For-loops are not allowed outside of functions"),
            ParseErrorKind::IllegalWhileLoop => write!(f, "While-loops are not allowed outside of functions"),
            ParseErrorKind::IllegalBreak => write!(f, "Break-statements are not allowed outside of loops"),
            ParseErrorKind::IllegalContinue => write!(f, "Continue-statements are not allowed outside of loops"),
            ParseErrorKind::IllegalClosure => write!(f, "Closure not allowed in this position"),
            ParseErrorKind::DisabledFeature(s) => write!(f, "{}", s),
            ParseErrorKind::IOError(e) => write!(f, "{}", e),
        }
    }
}

pub type ParseResult<T = ()> = Result<T, ParseError>;