use util::{ScopeId, TypeId, BindingId};
use frontend::resolver::scopes;
use frontend::resolver::{Unresolved, ast};

/// Internal state of the ResolvedProgram during type/binding resolution.
pub struct State<'a, 'b> where 'a: 'b {
    pub counter : &'b mut u32,
    pub scope_id: ScopeId,
    pub scopes  : &'b mut scopes::Scopes<'a>,
    pub unsigned: &'b [ TypeId; 4 ],
    pub signed  : &'b [ TypeId; 4 ],
    pub float   : &'b [ TypeId; 2 ],
}

impl<'a, 'b> State<'a, 'b> {

    fn is_unsigned(self: &Self, type_id: TypeId) -> bool {
        type_id >= *self.unsigned.first().unwrap() && type_id <= *self.unsigned.last().unwrap()
    }

    fn is_signed(self: &Self, type_id: TypeId) -> bool {
        type_id >= *self.signed.first().unwrap() && type_id <= *self.signed.last().unwrap()
    }

    fn is_float(self: &Self, type_id: TypeId) -> bool {
        type_id >= *self.float.first().unwrap() && type_id <= *self.float.last().unwrap()
    }

    fn is_valid_cast(self: &Self, from_type_id: TypeId, to_type_id: TypeId) -> bool {
        if self.is_unsigned(from_type_id) {
            if self.is_unsigned(to_type_id) && from_type_id <= to_type_id {
                // unsigned -> unsigned: valid if target has equal or more bits (primitive type_ids are ordered)
                true
            } else if self.is_signed(to_type_id) {
                // unsigned -> signed: valid if target has more bits (compare index within group of signed / unsigned types)
                let from_size: usize = Into::<usize>::into(from_type_id) - Into::<usize>::into(*self.unsigned.first().unwrap());
                let to_size: usize = Into::<usize>::into(to_type_id) - Into::<usize>::into(*self.signed.first().unwrap());
                from_size < to_size
            } else {
                // unsigend -> everything else: invalid
                false
            }
        } else if self.is_signed(from_type_id) && self.is_signed(to_type_id) && from_type_id <= to_type_id {
            // signed -> signed: valid if target has equal or more bits (primitive type_ids are ordered)
            true
        } else if self.is_float(from_type_id) && self.is_float(to_type_id) && from_type_id <= to_type_id {
            true
        } else {
            // everything else: invalid
            false
        }
    }

    fn try_cast(self: &Self, type_id_a: TypeId, type_id_b: TypeId) -> Option<TypeId> {
        if self.is_valid_cast(type_id_a, type_id_b) {
            Some(type_id_b)
        } else if self.is_valid_cast(type_id_b, type_id_a) {
            Some(type_id_a)
        } else {
            None
        }
    }

    /// Sets given type_id for the given binding. Increases resolution counter if a change was made.
    fn binding_set_type_id(self: &mut Self, binding_id: BindingId, new_type_id: TypeId) {
        let mut type_id = self.scopes.binding_type(binding_id);
        self.set_type_id(&mut type_id, new_type_id); // todo: get borrowck to accept a binding_type_mut instead of the temp copy?
        *self.scopes.binding_type_mut(binding_id) = type_id;
    }

    /// Sets given type_id for the given unresolved type. Increases resolution counter if a change was made.
    fn set_type_id(self: &mut Self, type_id: &mut Unresolved<TypeId>, new_type_id: TypeId) {
        if type_id.is_maybe() || type_id.is_unknown() {
            *type_id = Unresolved::Resolved(new_type_id);
            *self.counter += 1
        } else if let Unresolved::Resolved(type_id) = type_id {
            if *type_id != new_type_id {
                panic!("attempted to change already resolved type");
            }
        }
    }

    /// Resolves and sets named type for the given unresolved type. Increases resolution counter if a change was made.
    fn set_type(self: &mut Self, type_id: &mut Unresolved<TypeId>, new_type: &[&'a str]) {
        if type_id.is_maybe() || type_id.is_unknown() {
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
            E::Block(block)             => { }
            E::IfBlock(if_block)        => { }
        };
    }

    fn resolve_function(self: &mut Self, item: &mut ast::Function<'a>) {

    }

    fn resolve_structure(self: &mut Self, item: &mut ast::Structure<'a>) {

    }

    fn resolve_type(self: &mut Self, item: &mut ast::Type<'a>) {
        self.set_type(&mut item.type_id, &item.name.0);
    }

    fn resolve_binding(self: &mut Self, item: &mut ast::Binding<'a>) {

        // create binding
        let binding_id = if let Some(binding_id) = item.binding_id {
            binding_id
        } else {
            // this binding ast node wasn't processed yet. if the binding name already exists we're shadowing - which is NYI
            if self.scopes.binding_id(self.scope_id, item.name).is_some() {
                panic!("shadowing NYI"); // todo: support shadowing
            }
            let binding_id = self.scopes.insert_binding(self.scope_id, item.name, Unresolved::Unknown); // todo: a &mut would be nicer than the index
            item.binding_id = Some(binding_id);
            binding_id
        };

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
            if lhs != rhs && !self.is_valid_cast(rhs, lhs) {
                panic!("invalid cast from {:?} to {:?}", lhs, rhs); // TODO: error handling
            }
        }

        // have explicit type and/or resolved expression
        if let Some(Unresolved::Resolved(lhs)) = lhs {
            self.set_type_id(&mut item.type_id, lhs);
            self.binding_set_type_id(binding_id, lhs);
        } else if let Some(Unresolved::Resolved(rhs)) = rhs {
            self.set_type_id(&mut item.type_id, rhs);
            self.binding_set_type_id(binding_id, rhs);
        }
    }

    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock<'a>) {

        self.resolve_block(&mut item.if_block);

        if let Some(ref mut else_block) = item.else_block {
            self.resolve_block(else_block);

            if item.if_block.type_id != else_block.type_id { // TODO: used complete here
                panic!("if/else return type mismatch"); // TODO: error handling
            }
        }
    }

    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop<'a>) {

    }

    fn resolve_block(self: &mut Self, item: &mut ast::Block<'a>) {

        for mut statement in item.statements.iter_mut() {
            self.resolve_statement(&mut statement);
        }

        if let Some(ref mut result) = item.result {
            self.resolve_expression(result);
            if let Unresolved::Resolved(type_id) = result.get_type_id() {
                self.set_type_id(&mut item.type_id, type_id);
            }
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
                self.set_type_id(&mut item.type_id, binding_type_id);
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

        self.resolve_expression(&mut item.left);
        self.resolve_expression(&mut item.right);

        match item.op {
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual | O::And | O::Or => {
                self.set_type(&mut item.type_id, &[ "bool" ]);
            },
            O::Add | O::Sub | O::Div | O::Mul => {
                if let (Unresolved::Resolved(left_type_id), Unresolved::Resolved(right_type_id)) = (item.left.get_type_id(), item.right.get_type_id()) {
                    if let Some(type_id) = self.try_cast(left_type_id, right_type_id) {
                        self.set_type_id(&mut item.type_id, type_id);
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
                self.set_type(&mut item.type_id, &[ "bool" ]);
            },
            _ => { } // fixme: remove and add missing
        }
        // todo: implement
    }

    fn resolve_literal(self: &mut Self, item: &mut ast::Literal<'a>) {
        use frontend::ast::LiteralValue as L;
        use frontend::resolver::consts::*;
        match item.value {
            L::Signed(v) => {
                if v >= MIN_I8 && v <= MAX_I8 {
                    self.set_type(&mut item.type_id, &[ "i8" ]);
                } else if v >= MIN_I16 && v <= MAX_I16 {
                    self.set_type(&mut item.type_id, &[ "i16" ]);
                } else if v >= MIN_I32 && v <= MAX_I32 {
                    self.set_type(&mut item.type_id, &[ "i32" ]);
                } else if v >= MIN_I64 && v <= MAX_I64 {
                    self.set_type(&mut item.type_id, &[ "i64" ]);
                }
            },
            L::Unsigned(v) => {
                if v >= MIN_U8 && v <= MAX_U8 {
                    self.set_type(&mut item.type_id, &[ "u8" ]);
                } else if v >= MIN_U16 && v <= MAX_U16 {
                    self.set_type(&mut item.type_id, &[ "u16" ]);
                } else if v >= MIN_U32 && v <= MAX_U32 {
                    self.set_type(&mut item.type_id, &[ "u32" ]);
                } else if v >= MIN_U64 && v <= MAX_U64 {
                    self.set_type(&mut item.type_id, &[ "u64" ]);
                }
            },
            L::Float(float) => {

            },
            L::String(string) => { },
        };
    }
}