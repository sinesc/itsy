mod scopes;
mod state;
mod consts;

use frontend::{Unresolved, Type, ast};

#[derive(Debug)]
pub struct ResolvedProgram<'a> {
    ast     : ast::Program<'a>,
    types   : Vec<Type<'a>>,
}

/// Resolves types within the given program
pub fn resolve<'a>(mut program: ast::Program<'a>) -> ResolvedProgram<'a> {

    let mut scopes = scopes::Scopes::new();
    let root_scope_id = scopes::Scopes::root_id();

    // insert primitive types into root scope

    scopes.insert_type(root_scope_id, "", Type::void);
    scopes.insert_type(root_scope_id, "bool", Type::bool);
    scopes.insert_type(root_scope_id, "String", Type::String);

    let unsigned = [
        scopes.insert_type(root_scope_id, "u8", Type::u8),
        scopes.insert_type(root_scope_id, "u16", Type::u16),
        scopes.insert_type(root_scope_id, "u32", Type::u32),
        scopes.insert_type(root_scope_id, "u64", Type::u64),
    ];

    let signed = [
        scopes.insert_type(root_scope_id, "i8", Type::i8),
        scopes.insert_type(root_scope_id, "i16", Type::i16),
        scopes.insert_type(root_scope_id, "i32", Type::i32),
        scopes.insert_type(root_scope_id, "i64", Type::i64),
    ];

    let float = [
        scopes.insert_type(root_scope_id, "f32", Type::f32),
        scopes.insert_type(root_scope_id, "f64", Type::f64),
    ];

    // keep resolving until the number of resolved items no longer increases.
    // perf: ideally we count the number of unresolved items during parsing and then count it down to 0 here. This
    // would avoid the additional "have we resolved everything" check afterwards.

    let mut num_resolved = 0;
    let mut num_resolved_before;

    loop {
        num_resolved_before = num_resolved;
        for mut statement in program.iter_mut() {
            let mut state = state::State {
                counter : &mut num_resolved,
                scope_id: root_scope_id,
                scopes  : &mut scopes,
                unsigned: &unsigned,
                signed  : &signed,
                float   : &float,
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