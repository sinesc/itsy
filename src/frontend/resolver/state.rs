use util::{ScopeId, TypeId, BindingId};
use frontend::Unresolved;
use frontend::resolver::{scopes, primitives, ast};

/// Internal state of the ResolvedProgram during type/binding resolution.
pub struct State<'a, 'b> where 'a: 'b {
    /// Counts resolved items.
    pub counter     : &'b mut u32,
    /// Scope id this state operates in.
    pub scope_id    : ScopeId,
    /// Repository of all scopes.
    pub scopes      : &'b mut scopes::Scopes<'a>,
    /// Grouped primitive types
    pub primitives  : &'b primitives::Primitives,
}

impl<'a, 'b> State<'a, 'b> {

    /// Sets given type_id for the given binding. Increases resolution counter if a change was made.
    fn bindingtype_from_id(self: &mut Self, binding_id: BindingId, new_type_id: TypeId) {
        let mut type_id = self.scopes.binding_type(binding_id);
        self.type_from_id(&mut type_id, new_type_id); // todo: get borrowck to accept a binding_type_mut instead of the temp copy?
        *self.scopes.binding_type_mut(binding_id) = type_id;
    }

    /// Sets given type_id for the given unresolved type. Increases resolution counter if a change was made.
    fn type_from_id(self: &mut Self, type_id: &mut Unresolved, new_type_id: TypeId) {
        if type_id.is_unknown() {
            *type_id = Unresolved::Resolved(new_type_id);
            *self.counter += 1
        } else if let Unresolved::Resolved(type_id) = type_id {
            if *type_id != new_type_id {
                panic!("attempted to change already resolved type");
            }
        }
    }

    fn type_from_void(self: &mut Self, type_id: &mut Unresolved) {
        if type_id.is_unknown() {
            *type_id = Unresolved::Void;
            *self.counter += 1
        } else if !type_id.is_void() {
            panic!("attempted to change already resolved type");
        }
    }

    /// Resolves and sets named type for the given unresolved type. Increases resolution counter if a change was made.
    fn type_from_name(self: &mut Self, type_id: &mut Unresolved, new_type: &[&'a str]) {
        if type_id.is_unknown() {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &new_type[0]) { // fixme: handle path segments
                *type_id = Unresolved::Resolved(new_type_id);
                *self.counter += 1
            }
        }
        // todo: error if type already resolved and new type differs?
    }

    /// Resolves types and bindings used in a statement.
    pub fn resolve_statement(self: &mut Self, item: &mut ast::Statement<'a>) {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.resolve_function(function),
            S::Structure(structure)     => self.resolve_structure(structure),
            S::Binding(binding)         => self.resolve_binding(binding),
            S::IfBlock(if_block)        => self.resolve_if_block(if_block),
            S::ForLoop(for_loop)        => self.resolve_for_loop(for_loop),
            S::Block(block)             => self.resolve_block(block),
            S::Expression(expression)   => self.resolve_expression(expression),
        }
    }

    /// Resolves types and bindings used in an expression.
    fn resolve_expression(self: &mut Self, item: &mut ast::Expression<'a>) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.resolve_literal(literal),
            E::Variable(variable)       => self.resolve_variable(variable),
            E::Call(call)               => { }
            E::Assignment(assignment)   => self.resolve_assignment(assignment),
            E::BinaryOp(binary_op)      => self.resolve_binary_op(binary_op),
            E::UnaryOp(unary_op)        => self.resolve_unary_op(unary_op),
            E::Block(block)             => self.resolve_block(block),
            E::IfBlock(if_block)        => { }
        };
    }

    fn resolve_function(self: &mut Self, item: &mut ast::Function<'a>) {
        self.resolve_signature(&mut item.sig);
    }

    fn resolve_signature(self: &mut Self, item: &mut ast::Signature<'a>) {
        // resolve arguments // todo: create scope here
        for arg in item.args.iter_mut() {
            self.try_create_binding(arg);
            self.resolve_type(arg.ty.as_mut().unwrap());
            if let Unresolved::Resolved(type_id) = arg.ty.as_ref().unwrap().type_id {
                self.type_from_id(&mut arg.type_id, type_id);
    }
        }
        // resolve return type
        if let Some(ret) = &mut item.ret {
            self.resolve_type(ret);
        }
    }

    fn resolve_structure(self: &mut Self, item: &mut ast::Structure<'a>) {

    }

    fn resolve_type(self: &mut Self, item: &mut ast::Type<'a>) {
        self.type_from_name(&mut item.type_id, &item.name.0);
    }

    /// Return existing binding-id or create and return new binding-id.
    fn try_create_binding(self: &mut Self, item: &mut ast::Binding<'a>) -> BindingId {
        if let Some(binding_id) = item.binding_id {
            binding_id
        } else {
            // this binding ast node wasn't processed yet. if the binding name already exists we're shadowing - which is NYI
            if self.scopes.binding_id(self.scope_id, item.name).is_some() {
                panic!("shadowing NYI"); // todo: support shadowing
            }
            let binding_id = self.scopes.insert_binding(self.scope_id, item.name, Unresolved::Unknown); // todo: a &mut would be nicer than the index
            item.binding_id = Some(binding_id);
            binding_id
        }
    }

    fn resolve_binding(self: &mut Self, item: &mut ast::Binding<'a>) {
        // create binding
        let binding_id = self.try_create_binding(item);
        // has an explicit type, determine id
        let lhs = match item.ty {
            Some(ref mut ty) => {
                self.resolve_type(ty);
                Some(ty.type_id)
            },
            None => None,
        };
        // has an initial value
        let rhs = match item.expr {
            Some(ref mut expr) => {
                self.resolve_expression(expr);
                Some(expr.get_type_id())
            },
            None => None,
        };
        // have explicit type and a resolved type for right hand side, check that they match
        if let (Some(Unresolved::Resolved(lhs)), Some(Unresolved::Resolved(rhs))) = (lhs, rhs) {
            if lhs != rhs && !self.primitives.is_valid_cast(rhs, lhs) {
                panic!("invalid cast from {:?} to {:?}", lhs, rhs); // TODO: error handling
            }
        }
        // have explicit type and/or resolved expression
        if let Some(Unresolved::Resolved(lhs)) = lhs {
            self.type_from_id(&mut item.type_id, lhs);
            self.bindingtype_from_id(binding_id, lhs);
        } else if let Some(Unresolved::Resolved(rhs)) = rhs {
            self.type_from_id(&mut item.type_id, rhs);
            self.bindingtype_from_id(binding_id, rhs);
        }
    }

    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock<'a>) {
        self.resolve_block(&mut item.if_block);
        if let Some(ref mut else_block) = item.else_block {
            self.resolve_block(else_block);
            if item.if_block.type_id != else_block.type_id { // TODO: used complete here
                panic!("if/else return type mismatch"); // TODO: error handling, attempt cast
            }
        }
    }

    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop<'a>) {
        // create binding for iterator var
        let binding_id = self.try_create_binding(&mut item.iter);
        // resolve the range type
        self.resolve_expression(&mut item.range);
        // set binding type to range type
        if let Unresolved::Resolved(range) = item.range.get_type_id() {
            self.type_from_id(&mut item.iter.type_id, range);
            self.bindingtype_from_id(binding_id, range);
        }
        // handle block
        self.resolve_block(&mut item.block);
    }

    fn resolve_block(self: &mut Self, item: &mut ast::Block<'a>) {
        for mut statement in item.statements.iter_mut() {
            self.resolve_statement(&mut statement);
        }
        if let Some(ref mut result) = item.result {
            self.resolve_expression(result);
            if let Unresolved::Resolved(type_id) = result.get_type_id() {
                self.type_from_id(&mut item.type_id, type_id);
            }
        } else {
            self.type_from_void(&mut item.type_id);
        }
    }

    fn resolve_variable(self: &mut Self, item: &mut ast::Variable<'a>) {
        // resolve binding
        if item.binding_id.is_none() {
            item.binding_id = self.scopes.lookup_binding_id(self.scope_id, item.path.0[0]);      // fixme: need full path here
            if item.binding_id.is_none() {
                panic!("unknown binding"); // todo: error handling
            }
        }
        // try to resolve type from binding
        if let Some(binding_id) = item.binding_id {
            if let Unresolved::Resolved(binding_type_id) = self.scopes.binding_type(binding_id) {
                self.type_from_id(&mut item.type_id, binding_type_id);
            }
        }
    }

    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment<'a>) {
        self.resolve_variable(&mut item.left);
        self.resolve_expression(&mut item.right);
        // todo: implement
    }

    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp<'a>) {

        use frontend::ast::BinaryOperator as O;
        use frontend::ast::Expression as E;

        if let (E::Literal(ast::Literal { value: value_a, .. }), E::Literal(ast::Literal { value: value_b, .. })) = (&item.left, &item.right) {
            // todo: both sides are const, could compute here
        }

        self.resolve_expression(&mut item.left);
        self.resolve_expression(&mut item.right);

        match item.op {
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual | O::And | O::Or => {
                let selfbool = self.primitives.bool; // todo: fixed with nll
                self.type_from_id(&mut item.type_id, selfbool);
            },
            O::Add | O::Sub | O::Mul | O::Div | O::Rem | O::Range => {
                if let (E::Literal(literal_a), E::Literal(literal_b)) = (&item.left, &item.right) {
                    // both sides are literals, determine type that suits both
                    if let Some(type_id) = self.primitives.cast_literal(literal_a, literal_b) {
                        self.type_from_id(&mut item.type_id, type_id);
                    } else {
                        panic!("cannot cast between {:?} and {:?}", literal_a, literal_b);
                    }
                } else if let (Unresolved::Resolved(left_type_id), Unresolved::Resolved(right_type_id)) = (item.left.get_type_id(), item.right.get_type_id()) {
                    // cast to common type or try to widen type
                    if let Some(type_id) = self.primitives.cast(left_type_id, right_type_id) {
                        self.type_from_id(&mut item.type_id, type_id);
                    } else {
                        panic!("cannot cast between {:?} and {:?}", left_type_id, right_type_id);
                    }
                }
            },
            _ => { }, // fixme: remove and add missing
        }
        // todo: implement
    }

    fn resolve_unary_op(self: &mut Self, item: &mut ast::UnaryOp<'a>) {
        use frontend::ast::UnaryOperator as O;
        self.resolve_expression(&mut item.exp);
        match item.op {
            O::Not => {
                let type_id = self.primitives.bool; // todo: nll fixes this
                self.type_from_id(&mut item.type_id, type_id);
            },
            _ => { } // fixme: remove and add missing
        }
        // todo: implement
    }

    fn resolve_literal(self: &mut Self, item: &mut ast::Literal<'a>) {
        use frontend::ast::LiteralValue as L;
        match item.value {
            L::Integer(ref v) => {
                let type_id = self.primitives.integer_type_id(*v).unwrap();
                self.type_from_id(&mut item.type_id, type_id);
            },
            L::Float(ref float) => {

            },
            L::String(ref string) => { },
        };
    }
}