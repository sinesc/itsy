mod scopes;
mod resolver_state;
mod consts;

use util::{BindingId, TypeId};
use super::{Unresolved, Type, ast};
use self::resolver_state::ResolverState;
use self::scopes::Scopes;

/// Resolves types within the given program
pub fn resolve<'a>(program: ast::Program<'a>) -> ResolvedProgram<'a> {
    ResolvedProgram::new(program)
}

#[derive(Debug)]
pub struct ResolvedProgram<'a> {
    ast     : ast::Program<'a>,
    types   : Vec<Type<'a>>,
}

// todo: all the resolve_* methods should probably go into resolver_state (and rename that to resolver)

impl<'a, 'b> ResolvedProgram<'a> {

    fn new(mut program: ast::Program<'a>) -> ResolvedProgram<'a> {

        let mut scopes = Scopes::new();
        let root_scope_id = Scopes::root_id();

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
                let mut state = ResolverState {
                    counter : &mut num_resolved,
                    scope_id: root_scope_id,
                    scopes  : &mut scopes,
                    unsigned: &unsigned,
                    signed  : &signed,
                    float   : &float,
                };
                Self::resolve_statement(&mut statement, &mut state);
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

    fn is_unsigned(type_id: TypeId, state: &ResolverState<'a, 'b>) -> bool {
        type_id >= *state.unsigned.first().unwrap() && type_id <= *state.unsigned.last().unwrap()
    }

    fn is_signed(type_id: TypeId, state: &ResolverState<'a, 'b>) -> bool {
        type_id >= *state.signed.first().unwrap() && type_id <= *state.signed.last().unwrap()
    }

    fn is_float(type_id: TypeId, state: &ResolverState<'a, 'b>) -> bool {
        type_id >= *state.float.first().unwrap() && type_id <= *state.float.last().unwrap()
    }

    fn is_valid_cast(from_type_id: TypeId, to_type_id: TypeId, state: &ResolverState<'a, 'b>) -> bool {
        if Self::is_unsigned(from_type_id, state) {
            if Self::is_unsigned(to_type_id, state) && from_type_id <= to_type_id {
                // unsigned -> unsigned: valid if target has equal or more bits (primitive type_ids are ordered)
                true
            } else if Self::is_signed(to_type_id, state) {
                // unsigned -> signed: valid if target has more bits (compare index within group of signed / unsigned types)
                let from_size: usize = Into::<usize>::into(from_type_id) - Into::<usize>::into(*state.unsigned.first().unwrap());
                let to_size: usize = Into::<usize>::into(to_type_id) - Into::<usize>::into(*state.signed.first().unwrap());
                from_size < to_size
            } else {
                // unsigend -> everything else: invalid
                false
            }
        } else if Self::is_signed(from_type_id, state) && Self::is_signed(to_type_id, state) && from_type_id <= to_type_id {
            // signed -> signed: valid if target has equal or more bits (primitive type_ids are ordered)
            true
        } else if Self::is_float(from_type_id, state) && Self::is_float(to_type_id, state) && from_type_id <= to_type_id {
            true
        } else {
            // everything else: invalid
            false
        }
    }

    fn try_cast(type_id_a: TypeId, type_id_b: TypeId, state: &ResolverState<'a, 'b>) -> Option<TypeId> {
        if Self::is_valid_cast(type_id_a, type_id_b, state) {
            Some(type_id_b)
        } else if Self::is_valid_cast(type_id_b, type_id_a, state) {
            Some(type_id_a)
        } else {
            None
        }
    }

    /// Sets given type_id for the given binding. Increases resolution counter if a change was made.
    fn binding_set_type_id(binding_id: BindingId, new_type_id: TypeId, state: &mut ResolverState<'a, 'b>) {
        let mut type_id = state.scopes.binding_type(binding_id);
        Self::set_type_id(&mut type_id, new_type_id, state); // todo: get borrowck to accept a binding_type_mut instead of the temp copy?
        *state.scopes.binding_type_mut(binding_id) = type_id;
    }

    /// Sets given type_id for the given unresolved type. Increases resolution counter if a change was made.
    fn set_type_id(type_id: &mut Unresolved<TypeId>, new_type_id: TypeId, state: &mut ResolverState<'a, 'b>) {
        if type_id.is_maybe() || type_id.is_unknown() {
            *type_id = Unresolved::Resolved(new_type_id);
            *state.counter += 1
        } else if let Unresolved::Resolved(type_id) = type_id {
            if *type_id != new_type_id {
                panic!("attempted to change already resolved type");
            }
        }
    }

    /// Resolves and sets named type for the given unresolved type. Increases resolution counter if a change was made.
    fn set_type(type_id: &mut Unresolved<TypeId>, new_type: &[&'a str], state: &mut ResolverState<'a, 'b>) {
        if type_id.is_maybe() || type_id.is_unknown() {
            if let Some(new_type_id) = state.scopes.lookup_type_id(state.scope_id, &new_type[0]) { // fixme: handle path segments
                *type_id = Unresolved::Resolved(new_type_id);
                *state.counter += 1
            }
        }
        // todo: error if type already resolved and new type differs?
    }

    /// Resolves types and bindings used in a statement.
    fn resolve_statement(item: &mut ast::Statement<'a>, state: &mut ResolverState<'a, 'b>) {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => Self::resolve_function(function, state),
            S::Structure(structure)     => Self::resolve_structure(structure, state),
            S::Binding(binding)         => Self::resolve_binding(binding, state),
            S::IfBlock(if_block)        => Self::resolve_if_block(if_block, state),
            S::ForLoop(for_loop)        => Self::resolve_for_loop(for_loop, state),
            S::Block(block)             => Self::resolve_block(block, state),
            S::Expression(expression)   => Self::resolve_expression(expression, state),
        }
    }

    /// Resolves types and bindings used in an expression.
    fn resolve_expression(item: &mut ast::Expression<'a>, state: &mut ResolverState<'a, 'b>) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => Self::resolve_literal(literal, state),
            E::Variable(variable)       => Self::resolve_variable(variable, state),
            E::Call(call)               => { }
            E::Assignment(assignment)   => Self::resolve_assignment(assignment, state),
            E::BinaryOp(binary_op)      => Self::resolve_binary_op(binary_op, state),
            E::UnaryOp(unary_op)        => Self::resolve_unary_op(unary_op, state),
            E::Block(block)             => { }
            E::IfBlock(if_block)        => { }
        };
    }

    fn resolve_function(item: &mut ast::Function<'a>, state: &mut ResolverState<'a, 'b>) {

    }

    fn resolve_structure(item: &mut ast::Structure<'a>, state: &mut ResolverState<'a, 'b>) {

    }

    fn resolve_type(item: &mut ast::Type<'a>, state: &mut ResolverState<'a, 'b>) {
        Self::set_type(&mut item.type_id, &item.name.0, state);
    }

    fn resolve_binding(item: &mut ast::Binding<'a>, state: &mut ResolverState<'a, 'b>) {

        // create binding
        let binding_id = if let Some(binding_id) = item.binding_id {
            binding_id
        } else {
            // this binding ast node wasn't processed yet. if the binding name already exists we're shadowing - which is NYI
            if state.scopes.binding_id(state.scope_id, item.name).is_some() {
                panic!("shadowing NYI"); // todo: support shadowing
            }
            let binding_id = state.scopes.insert_binding(state.scope_id, item.name, Unresolved::Unknown); // todo: a &mut would be nicer than the index
            item.binding_id = Some(binding_id);
            binding_id
        };

        // has an explicit type, determine id
        let lhs = match item.ty {
            Some(ref mut ty) => {
                Self::resolve_type(ty, state);
                Some(ty.type_id)
            },
            None => None,
        };

        // has an initial value
        let rhs = match item.expr {
            Some(ref mut expr) => {
                Self::resolve_expression(expr, state);
                Some(expr.get_type_id())
            },
            None => None,
        };

        // have explicit type and a resolved type for right hand side, check that they match
        if let (Some(Unresolved::Resolved(lhs)), Some(Unresolved::Resolved(rhs))) = (lhs, rhs) {
            if lhs != rhs && !Self::is_valid_cast(rhs, lhs, state) {
                panic!("invalid cast from {:?} to {:?}", lhs, rhs); // TODO: error handling
            }
        }

        // have explicit type and/or resolved expression
        if let Some(Unresolved::Resolved(lhs)) = lhs {
            Self::set_type_id(&mut item.type_id, lhs, state);
            Self::binding_set_type_id(binding_id, lhs, state);
        } else if let Some(Unresolved::Resolved(rhs)) = rhs {
            Self::set_type_id(&mut item.type_id, rhs, state);
            Self::binding_set_type_id(binding_id, rhs, state);
        }
    }

    fn resolve_if_block(item: &mut ast::IfBlock<'a>, state: &mut ResolverState<'a, 'b>) {

        Self::resolve_block(&mut item.if_block, state);

        if let Some(ref mut else_block) = item.else_block {
            Self::resolve_block(else_block, state);

            if item.if_block.type_id != else_block.type_id { // TODO: used complete here
                panic!("if/else return type mismatch"); // TODO: error handling
            }
        }
    }

    fn resolve_for_loop(item: &mut ast::ForLoop<'a>, state: &mut ResolverState<'a, 'b>) {

    }

    fn resolve_block(item: &mut ast::Block<'a>, state: &mut ResolverState<'a, 'b>) {

        for mut statement in item.statements.iter_mut() {
            Self::resolve_statement(&mut statement, state);
        }

        if let Some(ref mut result) = item.result {
            Self::resolve_expression(result, state);
            if let Unresolved::Resolved(type_id) = result.get_type_id() {
                Self::set_type_id(&mut item.type_id, type_id, state);
            }
        }
    }

    fn resolve_variable(item: &mut ast::Variable<'a>, state: &mut ResolverState<'a, 'b>) {
        // resolve binding
        if item.binding_id.is_none() {
            item.binding_id = state.scopes.lookup_binding_id(state.scope_id, item.path.0[0]);      // fixme: need full path here
            if item.binding_id.is_none() {
                panic!("unknown binding"); // todo: error handling
            }
        }
        // try to resolve type from binding
        if let Some(binding_id) = item.binding_id {
            if let Unresolved::Resolved(binding_type_id) = state.scopes.binding_type(binding_id) {
                Self::set_type_id(&mut item.type_id, binding_type_id, state);
            }
        }
    }

    fn resolve_assignment(item: &mut ast::Assignment<'a>, state: &mut ResolverState<'a, 'b>) {
        Self::resolve_variable(&mut item.left, state);
        Self::resolve_expression(&mut item.right, state);
        // todo: implement
    }

    fn resolve_binary_op(item: &mut ast::BinaryOp<'a>, state: &mut ResolverState<'a, 'b>) {

        use self::ast::BinaryOperator as O;

        Self::resolve_expression(&mut item.left, state);
        Self::resolve_expression(&mut item.right, state);

        match item.op {
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual | O::And | O::Or => {
                Self::set_type(&mut item.type_id, &[ "bool" ], state);
            },
            O::Add | O::Sub | O::Div | O::Mul => {
                if let (Unresolved::Resolved(left_type_id), Unresolved::Resolved(right_type_id)) = (item.left.get_type_id(), item.right.get_type_id()) {
                    if let Some(type_id) = Self::try_cast(left_type_id, right_type_id, state) {
                        Self::set_type_id(&mut item.type_id, type_id, state);
                    } else {
                        panic!("cannot cast between {:?} and {:?}", left_type_id, right_type_id);
                    }
                }
            },
            _ => { }, // fixme: remove and add missing
        }
        // todo: implement
    }

    fn resolve_unary_op(item: &mut ast::UnaryOp<'a>, state: &mut ResolverState<'a, 'b>) {

        use self::ast::UnaryOperator as O;

        Self::resolve_expression(&mut item.exp, state);

        match item.op {
            O::Not => {
                Self::set_type(&mut item.type_id, &[ "bool" ], state);
            },
            _ => { } // fixme: remove and add missing
        }
        // todo: implement
    }

    fn resolve_literal(item: &mut ast::Literal<'a>, state: &mut ResolverState<'a, 'b>) {
        use self::ast::LiteralValue as L;
        use self::consts::*;
        match item.value {
            L::Signed(v) => {
                if v >= MIN_I8 && v <= MAX_I8 {
                    Self::set_type(&mut item.type_id, &[ "i8" ], state);
                } else if v >= MIN_I16 && v <= MAX_I16 {
                    Self::set_type(&mut item.type_id, &[ "i16" ], state);
                } else if v >= MIN_I32 && v <= MAX_I32 {
                    Self::set_type(&mut item.type_id, &[ "i32" ], state);
                } else if v >= MIN_I64 && v <= MAX_I64 {
                    Self::set_type(&mut item.type_id, &[ "i64" ], state);
                }
            },
            L::Unsigned(v) => {
                if v >= MIN_U8 && v <= MAX_U8 {
                    Self::set_type(&mut item.type_id, &[ "u8" ], state);
                } else if v >= MIN_U16 && v <= MAX_U16 {
                    Self::set_type(&mut item.type_id, &[ "u16" ], state);
                } else if v >= MIN_U32 && v <= MAX_U32 {
                    Self::set_type(&mut item.type_id, &[ "u32" ], state);
                } else if v >= MIN_U64 && v <= MAX_U64 {
                    Self::set_type(&mut item.type_id, &[ "u64" ], state);
                }
            },
            L::Float(float) => {

            },
            L::String(string) => { },
        };
    }
}
