//! AST type checker and resolver.

mod scopes;
mod state;
mod primitives;

use util::{Integer, Signed, Unsigned};
use frontend::ast;
use frontend::resolver::primitives::IntegerRange;

use std::collections::HashMap;

#[derive(Debug)]
pub struct Enum {
    //repr: u8,
    keys: HashMap<usize, u64>
}

#[derive(Debug)]
pub struct Struct {
    fields: HashMap<usize, Type>
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Type {
    void,
    u8, u16, u32, u64,
    i8, i16, i32, i64,
    f32, f64,
    bool,
    String,
    Enum(Enum),
    Struct(Struct),
}

/// Wrapper containing an AST structure with all types resolved and a map of those types.
#[derive(Debug)]
pub struct ResolvedProgram<'a> {
    pub ast     : ast::Program<'a>,
    pub types   : Vec<Type>,
}

/// Resolves types within the given program AST structure.
pub fn resolve<'a>(mut program: ast::Program<'a>) -> ResolvedProgram<'a> {

    use std::{u8, u16, u32, u64, i8, i16, i32, i64};

    let mut scopes = scopes::Scopes::new();
    let root_scope_id = scopes::Scopes::root_id();

    // insert primitive types into root scope
    // todo: primitives should be statically defined. scopes should than be filled with looked up values of primitive ids, not
    //  the other way around like here

    let primitives = primitives::Primitives {
        bool: scopes.insert_type(root_scope_id, "bool", Type::bool),
        string: scopes.insert_type(root_scope_id, "String", Type::String),
        unsigned: [
            IntegerRange {
                type_id: scopes.insert_type(root_scope_id, "u8", Type::u8),
                min: Integer::Unsigned(u8::MIN as Unsigned),
                max: Integer::Unsigned(u8::MAX as Unsigned),
            },
            IntegerRange {
                type_id: scopes.insert_type(root_scope_id, "u16", Type::u8),
                min: Integer::Unsigned(u16::MIN as Unsigned),
                max: Integer::Unsigned(u16::MAX as Unsigned),
            },
            IntegerRange {
                type_id: scopes.insert_type(root_scope_id, "u32", Type::u8),
                min: Integer::Unsigned(u32::MIN as Unsigned),
                max: Integer::Unsigned(u32::MAX as Unsigned),
            },
            IntegerRange {
                type_id: scopes.insert_type(root_scope_id, "u64", Type::u8),
                min: Integer::Unsigned(u64::MIN as Unsigned),
                max: Integer::Unsigned(u64::MAX as Unsigned),
            },
        ],
        signed: [
            IntegerRange {
                type_id: scopes.insert_type(root_scope_id, "i8", Type::u8),
                min: Integer::Signed(i8::MIN as Signed),
                max: Integer::Signed(i8::MAX as Signed),
            },
            IntegerRange {
                type_id: scopes.insert_type(root_scope_id, "i16", Type::u8),
                min: Integer::Signed(i16::MIN as Signed),
                max: Integer::Signed(i16::MAX as Signed),
            },
            IntegerRange {
                type_id: scopes.insert_type(root_scope_id, "i32", Type::u8),
                min: Integer::Signed(i32::MIN as Signed),
                max: Integer::Signed(i32::MAX as Signed),
            },
            IntegerRange {
                type_id: scopes.insert_type(root_scope_id, "i64", Type::u8),
                min: Integer::Signed(i64::MIN as Signed),
                max: Integer::Signed(i64::MAX as Signed),
            },
        ],
        float: [
            scopes.insert_type(root_scope_id, "f32", Type::f32),
            scopes.insert_type(root_scope_id, "f64", Type::f64),
        ],
    };

    // keep resolving until the number of resolved items no longer increases.
    // perf: ideally we count the number of unresolved items during parsing and then count it down to 0 here. This
    // would avoid the additional "have we resolved everything" check afterwards.

    let mut num_resolved = 0;
    let mut num_resolved_before;

    loop {
        num_resolved_before = num_resolved;
        for mut statement in program.iter_mut() {
            let mut state = state::State {
                counter     : &mut num_resolved,
                scope_id    : root_scope_id,
                scopes      : &mut scopes,
                primitives  : &primitives,
            };
            state.resolve_statement(&mut statement);
        }
        if num_resolved == num_resolved_before {
            break;
        }
    }

    ResolvedProgram {
        ast     : program,
        types   : scopes.into(),
    }
}
