use util::{TypeId, ScopeId, BindingId, Repository};
use super::{Unresolved, Type, ast};

#[derive(Debug)]
pub struct ResolvedProgram<'a> {
    ast     : ast::Program<'a>,
    types   : Vec<Type<'a>>,
}

struct ScopeTree<'a> {
    /// Flat type data, lookup via TypeId or ScopeId and name
    types       : Repository<Type<'a>, TypeId, (ScopeId, &'a str)>,
    /// Flat binding data, lookup via TypeId or ScopeId and name
    bindings    : Repository<Unresolved<TypeId>, BindingId, (ScopeId, &'a str)>,
    /// Current scope id, incremented when scopes are added
    current     : ScopeId,
    /// Maps ScopeId => Parent ScopeId (using vector as usize=>usize map)
    parent_map  : Vec<ScopeId>, // ScopeId => ScopeId
}

impl<'a> ScopeTree<'a> {

    pub fn new() -> Self {
        let root_id = (0).into();
        ScopeTree {
            types       : Repository::new(),
            bindings    : Repository::new(),
            current     : root_id,
            parent_map  : vec![ root_id ],
        }
    }

    pub fn root_id() -> ScopeId {
        (0).into()
    }

    pub fn create_scope(self: &mut Self, parent: ScopeId) -> ScopeId {
        let index = self.parent_map.len();
        self.parent_map.push(parent);
        index.into()
    }

    /// Insert a binding into the given scope, returning a binding id. Its type may not be resolved yet.
    pub fn insert_binding(self: &mut Self, scope_id: ScopeId, name: &'a str, type_id: Unresolved<TypeId>) -> BindingId {
        self.bindings.insert((scope_id, name), type_id)
    }

    /// Finds the id of the named binding within the scope or its parent scopes.
    pub fn lookup_binding_id(self: &mut Self, scope_id: ScopeId, name: &'a str) -> Option<BindingId> {
        if let Some(index) = self.bindings.index_of(&(scope_id, name)) {
            Some(index)
        } else {
            // TODO: non recursive solution, ran into multiple mut borrow issues using a while loop
            let parent_scope_id = self.parent_map[Into::<usize>::into(scope_id)];
            if parent_scope_id != scope_id {
                self.lookup_binding_id(parent_scope_id, name)
            } else {
                None
            }
        }
    }

    /// Insert a type into the given scope, returning a type id.
    pub fn insert_type(self: &mut Self, scope_id: ScopeId, name: &'a str, ty: Type<'a>) -> TypeId {
        self.types.insert((scope_id, name), ty)
    }

    /// Finds the id of the named type within the scope or its parent scopes.
    pub fn lookup_type_id(self: &mut Self, scope_id: ScopeId, name: &'a str) -> Option<TypeId> {
        if let Some(index) = self.types.index_of(&(scope_id, name)) {
            Some(index)
        } else {
            // TODO: non recursive solution, ran into multiple mut borrow issues using a while loop
            let parent_scope_id = self.parent_map[Into::<usize>::into(scope_id)];
            if parent_scope_id != scope_id {
                self.lookup_type_id(parent_scope_id, name)
            } else {
                None
            }
        }
    }

    /* TODO: maybe rather add function to get Type from TypeId? do I even need this?
    pub fn type_lookup(self: &mut Self, scope_id: ScopeId, name: &'a str) -> Option<&'a mut Type> {
        if let Some(type_id) = self.lookup_type_id(scope_id, name) {
            Some(self.types.index_mut(type_id))
        } else {
            None
        }
    }
    */
}

impl<'a> ResolvedProgram<'a> {

    fn new(mut program: ast::Program<'a>) -> ResolvedProgram<'a> {

        let mut scopes = ScopeTree::new();
        let scope_id = ScopeTree::root_id();

        Self::define_primitives(scope_id, &mut scopes);

        // TODO: count number of un/resolved items (maybe return that instead of bool?)
        // loop until the number stops changing (stuck, needs more annotations) or all is resolved
        for i in 0..5 {
            for mut statement in program.iter_mut() {
                Self::resolve_statement(&mut statement, scope_id, &mut scopes);
            }
        }

        ResolvedProgram {
            ast     : program,
            types   : scopes.types.into(),
        }
    }

    fn define_primitives(scope_id: ScopeId, scopes: &mut ScopeTree<'a>) {
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

    fn resolve_function(item: &mut ast::Function<'a>, scope_id: ScopeId, scopes: &mut ScopeTree<'a>) -> bool {
        false
    }

    fn resolve_structure(item: &mut ast::Structure<'a>, scope_id: ScopeId, scopes: &mut ScopeTree<'a>) -> bool {
        false
    }

    fn resolve_type(item: &mut ast::Type<'a>, scope_id: ScopeId, scopes: &mut ScopeTree<'a>) -> bool {

        if item.type_id.is_resolved() {
            return true;
        }

        if let Some(type_id) = scopes.lookup_type_id(scope_id, &item.name.segs[0]) { // TODO: handle path segments
            // not yet resolved, resolve
            item.type_id = Unresolved::Resolved(type_id);
            true
        } else {
            // TODO: error handling, custom type lookup
            panic!(format!("unknown type {:?}", item.name.segs));
        }
    }

    fn resolve_binding(item: &mut ast::Binding<'a>, scope_id: ScopeId, scopes: &mut ScopeTree<'a>) -> bool {

        // has an explicit type, determine id
        let lhs = match item.ty {
            Some(ref mut ty) => {
                Self::resolve_type(ty, scope_id, scopes);
                Some(ty.type_id)
            },
            None => None,
        };

        // has an initial value
        let rhs = match item.expr {
            Some(ref mut expr) => {
                Self::resolve_expression(expr, scope_id, scopes);
                Some(expr.get_type_id())
            },
            None => None,
        };

        // have explicit type and a resolved type for right hand side, check that they match
        if let (Some(Unresolved::Resolved(lhs)), Some(Unresolved::Resolved(rhs))) = (lhs, rhs) {
            if lhs != rhs {
                panic!("types don't match!"); // TODO: obviously
            }
        }

        // have explicit type and/or resolved expression
        if let Some(Unresolved::Resolved(lhs)) = lhs {
            item.type_id = Unresolved::Resolved(lhs);
            scopes.insert_binding(scope_id, item.name, item.type_id);
            rhs.is_none() || rhs.unwrap().is_resolved() // TODO: return value handling needs to be more obvious
        } else if let Some(Unresolved::Resolved(rhs)) = rhs {
            item.type_id = Unresolved::Resolved(rhs);
            scopes.insert_binding(scope_id, item.name, item.type_id);
            true
        } else {
            false
        }
    }

    fn resolve_if_block(item: &mut ast::IfBlock<'a>, scope_id: ScopeId, scopes: &mut ScopeTree<'a>) -> bool {

        let mut complete = Self::resolve_block(&mut item.if_block, scope_id, scopes);

        if let Some(ref mut else_block) = item.else_block {
            complete &= Self::resolve_block(else_block, scope_id, scopes);

            if complete && item.if_block.type_id != else_block.type_id {
                panic!("if/else return type mismatch"); // TODO: error handling
            }
        }

        complete
    }

    fn resolve_for_loop(item: &mut ast::ForLoop<'a>, scope_id: ScopeId, scopes: &mut ScopeTree<'a>) -> bool {
        false
    }

    fn resolve_block(item: &mut ast::Block<'a>, scope_id: ScopeId, scopes: &mut ScopeTree<'a>) -> bool {

        let mut complete = true;

        for mut statement in item.statements.iter_mut() {
            complete &= Self::resolve_statement(&mut statement, scope_id, scopes);
        }

        if let Some(ref mut result) = item.result {
            complete &= Self::resolve_expression(result, scope_id, scopes);
            item.type_id = result.get_type_id();
        }

        complete
    }

    fn resolve_expression(item: &mut ast::Expression<'a>, scope_id: ScopeId, scopes: &mut ScopeTree<'a>) -> bool {

        use self::ast::Expression as E;
        use self::ast::Operator as O;

        match item {
            E::Literal(literal)        => { }
            E::Variable(variable)      => { }
            E::Call(call)              => { }
            E::Assignment(assignment)  => { }
            E::BinaryOp(binary_op)     => match binary_op.op {
                O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual | O::And | O::Or => {
                    binary_op.type_id = Unresolved::Resolved(scopes.lookup_type_id(scope_id, "bool").unwrap());
                },
                _ => { }
            }
            E::UnaryOp(unary_op) => match unary_op.op {
                O::Not => {
                    unary_op.type_id = Unresolved::Resolved(scopes.lookup_type_id(scope_id, "bool").unwrap());
                },
                _ => { }
            }
            E::Block(block)            => { }
            E::IfBlock(if_block)       => { }
        };

        false
    }

    fn resolve_statement(item: &mut ast::Statement<'a>, scope_id: ScopeId, scopes: &mut ScopeTree<'a>) -> bool {

        use self::ast::Statement as S;

        match item {
            S::Function(function)       => Self::resolve_function(function, scope_id, scopes),
            S::Structure(structure)     => Self::resolve_structure(structure, scope_id, scopes),
            S::Binding(binding)         => Self::resolve_binding(binding, scope_id, scopes),
            S::IfBlock(if_block)        => Self::resolve_if_block(if_block, scope_id, scopes),
            S::ForLoop(for_loop)        => Self::resolve_for_loop(for_loop, scope_id, scopes),
            S::Block(block)             => Self::resolve_block(block, scope_id, scopes),
            S::Expression(expression)   => Self::resolve_expression(expression, scope_id, scopes),
        }
    }
}

/// Resolves types within the given program
pub fn resolve<'a>(program: ast::Program<'a>) -> ResolvedProgram<'a> {
    ResolvedProgram::new(program)
}
