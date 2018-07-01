use util::{BindingId, ScopeId, TypeId};
use frontend::{Unresolved, Type, ast};

mod scopes;

/// Internal state of the ResolvedProgram during type/binding resolution.
/// Not a member of ResolvedProgram since this data is no longer useful after resolution.
struct ResolverState<'a, 'b> where 'a: 'b {
    counter : &'b mut u32,
    scope_id: ScopeId,
    scopes  : &'b mut scopes::Scopes<'a>,
}

#[derive(Debug)]
pub struct ResolvedProgram<'a> {
    ast     : ast::Program<'a>,
    types   : Vec<Type<'a>>,
}

impl<'a, 'b> ResolvedProgram<'a> {

    fn new(mut program: ast::Program<'a>) -> ResolvedProgram<'a> {

        let mut scopes = scopes::Scopes::new();
        let scope_id = scopes::Scopes::root_id();
        Self::define_primitives(scope_id, &mut scopes);

        let mut num_resolved = 0;
        let mut num_resolved_before;

        // keep resolving until the number of resolved items no longer increases.
        // perf: ideally we count the number of unresolved items during parsing and then count it down to 0 here. This
        // would avoid the additional "have we resolved everything" check afterwards.

        loop {
            num_resolved_before = num_resolved;
            for mut statement in program.iter_mut() {
                let mut state = ResolverState { counter: &mut num_resolved, scope_id, scopes: &mut scopes };
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

    /// Sets up basic primitive types in the given scope.
    fn define_primitives(scope_id: ScopeId, scopes: &mut scopes::Scopes<'a>) {
        scopes.insert_type(scope_id, "", Type::void);
        scopes.insert_type(scope_id, "u8", Type::u8);
        scopes.insert_type(scope_id, "u16", Type::u16);
        scopes.insert_type(scope_id, "u32", Type::u32);
        scopes.insert_type(scope_id, "u64", Type::u64);
        scopes.insert_type(scope_id, "i8", Type::i8);
        scopes.insert_type(scope_id, "i16", Type::i16);
        scopes.insert_type(scope_id, "i32", Type::i32);
        scopes.insert_type(scope_id, "i64", Type::i64);
        scopes.insert_type(scope_id, "f32", Type::f32);
        scopes.insert_type(scope_id, "f64", Type::f64);
        scopes.insert_type(scope_id, "bool", Type::bool);
        scopes.insert_type(scope_id, "String", Type::String);
    }

    fn resolve_function(item: &mut ast::Function<'a>, state: &mut ResolverState<'a, 'b>) {

    }

    fn resolve_structure(item: &mut ast::Structure<'a>, state: &mut ResolverState<'a, 'b>) {

    }

    fn resolve_type(item: &mut ast::Type<'a>, state: &mut ResolverState<'a, 'b>) {
        Self::set_type(&mut item.type_id, &item.name.segs, state);
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
            if lhs != rhs {
                panic!("types don't match!"); // TODO: error handling
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
        println!("resolve_var");
        // resolve binding
        if item.binding_id.is_none() {
            item.binding_id = state.scopes.lookup_binding_id(state.scope_id, item.path.segs[0]);      // fixme: need full path here
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

        use self::ast::Operator as O;

        Self::resolve_expression(&mut item.left, state);
        Self::resolve_expression(&mut item.right, state);

        match item.op {
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual | O::And | O::Or => {
                Self::set_type(&mut item.type_id, &[ "bool" ], state);
            },
            _ => { }
        }
        // todo: implement
    }

    fn resolve_unary_op(item: &mut ast::UnaryOp<'a>, state: &mut ResolverState<'a, 'b>) {

        use self::ast::Operator as O;

        Self::resolve_expression(&mut item.exp, state);

        match item.op {
            O::Not => {
                Self::set_type(&mut item.type_id, &[ "bool" ], state);
            },
            _ => { }
        }
        // todo: implement
    }

    fn resolve_expression(item: &mut ast::Expression<'a>, state: &mut ResolverState<'a, 'b>) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => { }
            E::Variable(variable)       => Self::resolve_variable(variable, state),
            E::Call(call)               => { }
            E::Assignment(assignment)   => Self::resolve_assignment(assignment, state),
            E::BinaryOp(binary_op)      => Self::resolve_binary_op(binary_op, state),
            E::UnaryOp(unary_op)        => Self::resolve_unary_op(unary_op, state),
            E::Block(block)             => { }
            E::IfBlock(if_block)        => { }
        };
    }

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
}

/// Resolves types within the given program
pub fn resolve<'a>(program: ast::Program<'a>) -> ResolvedProgram<'a> {
    ResolvedProgram::new(program)
}
