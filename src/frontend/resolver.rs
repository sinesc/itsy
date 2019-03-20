//! AST type checker and resolver.

mod scopes;
mod primitives;
mod repository;

use std::marker::PhantomData;
use crate::frontend::ast::{self, Bindable};
use crate::util::{ScopeId, TypeId, BindingId, FunctionId, Type, Array, Struct};
use crate::{VMFunc, Standalone};

/// Parsed program AST with all types, bindings and other language structures resolved.
#[derive(Debug)]
pub struct ResolvedProgram<'a, T> where T: VMFunc<T> {
    ty: PhantomData<T>,
    /// Program AST with resolved `BindingId`s.
    pub ast: super::ParsedProgram<'a>,
    /// Mapping from `BindingId` (vector index) to `TypeId`.
    pub bindingtype_ids: Vec<TypeId>,
    /// Mapping from `TypeId` (vector index) to primitive type.
    pub types: Vec<Type>,
    /// `FunctionId` of the entry/main function.
    pub entry_fn: FunctionId,
}

/// Internal state during program type/binding resolution.
struct Resolver<'a, 'b> where 'a: 'b {
    stop_bugging_me_about_this_lifetime_every_fkn_refactoring: PhantomData<&'a bool>,
    /// Scope id this state operates in.
    scope_id        : ScopeId,
    /// Repository of all scopes.
    scopes          : &'b mut scopes::Scopes,
    /// Grouped primitive types.
    primitives      : &'b primitives::Primitives,
    /// Set to true once resolution cannot proceed any further, causes numeric literals to be set to their default types.
    infer_literals  : bool,
}

/// Resolves types within the given program AST structure.
#[allow(invalid_type_param_default)]
pub fn resolve<'a, T=Standalone>(mut program: super::ParsedProgram<'a>, entry: &str) -> ResolvedProgram<'a, T> where T: VMFunc<T> {

    // create root scope and insert primitives
    let mut scopes = scopes::Scopes::new();
    let root_scope_id = scopes::Scopes::root_id();
    let primitives = primitives::Primitives::new(&mut scopes, root_scope_id);

    // insert rust functions into root scope
    for (name, info) in T::call_info().iter() {
        let (index, ret_type_name, arg_type_names) = info;
        let ret_type = if *ret_type_name == "" {
            Some(scopes.void_type())
        } else {
            Some(scopes.type_id(root_scope_id, ret_type_name).expect("Unknown return type encountered"))
        };
        let arg_type_id = arg_type_names
            .iter()
            .map(|arg_type_name| {
                let arg_type_name = if &arg_type_name[0..2] == "& " { &arg_type_name[2..] } else { &arg_type_name[..] };
                scopes
                    .type_id(root_scope_id, if arg_type_name == "str" { "String" } else { arg_type_name }) // todo: fix string hack
                    .expect(&format!("Unknown argument type encountered: {}", &arg_type_name))
            })
            .collect();
        scopes.insert_rustfn(root_scope_id, *name, *index, ret_type, arg_type_id);
    }

    let mut now_unresolved = 0;
    let mut prev_unresolved;
    let mut infer_literals = false;

    loop {
        prev_unresolved = now_unresolved;

        for mut statement in program.0.iter_mut() {
            let mut resolver = Resolver {
                stop_bugging_me_about_this_lifetime_every_fkn_refactoring: PhantomData,
                scope_id        : root_scope_id,
                scopes          : &mut scopes,
                primitives      : &primitives,
                infer_literals  : infer_literals,
            };
            resolver.resolve_statement(&mut statement);
        }

        now_unresolved = scopes.num_unresolved();

        if now_unresolved == 0 || (now_unresolved == prev_unresolved && infer_literals) {
            break;
        }

        infer_literals = now_unresolved == prev_unresolved && !infer_literals;
    }

    let entry_fn = scopes.lookup_function_id(root_scope_id, entry).expect("Failed to resolve entry function");
    let (bindingtype_ids, types) = scopes.into();

    ResolvedProgram {
        ty              : PhantomData,
        ast             : program,
        entry_fn        : entry_fn,
        types           : types,
        bindingtype_ids : bindingtype_ids,
    }
}

/// Utility methods to update a typeslot with a resolved type and increase the resolution counter.
impl<'a, 'b> Resolver<'a, 'b> {

    /// Creates new or enters existing scope and returns the original/parent scope id.
    fn try_create_scope(self: &mut Self, scope_id: &mut Option<ScopeId>) -> ScopeId {
        let parent_scope_id = self.scope_id;
        if let &mut Some(scope_id) = scope_id {
            self.scope_id = scope_id;
        } else {
            self.scope_id = self.scopes.create_scope(parent_scope_id);
            *scope_id = Some(self.scope_id);
        }
        parent_scope_id
    }

    /// Return existing binding-id or create and return new binding-id.
    fn try_create_binding(self: &mut Self, item: &mut impl Bindable, name: &str) -> BindingId {
        if let Some(binding_id) = item.binding_id() {
            binding_id
        } else {
            // this binding ast node wasn't processed yet. if the binding name already exists we're shadowing - which is NYI
            if self.scopes.binding_id(self.scope_id, name).is_some() {
                unimplemented!("cannot shadow {}", name); // todo: support shadowing
            }
            let binding_id = self.scopes.insert_binding(self.scope_id, Some(name), None);
            *item.binding_id_mut() = Some(binding_id);
            binding_id
        }
    }

    /// Return existing binding-id or create and return new binding-id.
    fn try_create_anon_binding(self: &mut Self, item: &mut impl Bindable) -> BindingId {
        if let Some(binding_id) = item.binding_id() {
            binding_id
        } else {
            let binding_id = self.scopes.insert_binding(self.scope_id, None, None); // todo: a &mut would be nicer than the index
            *item.binding_id_mut() = Some(binding_id);
            binding_id
        }
    }

    /// Sets given TypeId for the given binding, generating a BindingId if required.
    fn set_bindingtype_id<T>(self: &mut Self, item: &mut T, new_type_id: TypeId) where T: Bindable {
        let binding_id = self.try_create_anon_binding(item);
        let type_id = self.scopes.binding_type_id_mut(binding_id);
        if let Some(type_id) = *type_id {
            if type_id != new_type_id {
                panic!("attempted to change already resolved type {:?} to {:?}", self.scopes.type_ref(type_id), self.scopes.type_ref(new_type_id));
            }
        }
        *type_id = Some(new_type_id);
    }

    // Returns TypeId for the given binding.
    fn bindingtype_id<T>(self: &Self, item: &mut T) -> Option<TypeId> where T: Bindable {
        item.binding_id().and_then(|binding_id| self.scopes.binding_type_id(binding_id))
    }

    // Returns a mutable reference to the type of the given binding.
    fn bindingtype<T>(self: &mut Self, item: &T) -> Option<&Type> where T: Bindable {
        item.binding_id()
            .and_then(|binding_id| self.scopes.binding_type_id(binding_id))
            .map(move |type_id| self.scopes.type_ref(type_id))
    }

    // Returns a mutable reference to the type of the given binding.
    fn bindingtype_mut<T>(self: &mut Self, item: &mut T) -> Option<&mut Type> where T: Bindable {
        item.binding_id()
            .and_then(|binding_id| self.scopes.binding_type_id(binding_id))
            .map(move |type_id| self.scopes.type_mut(type_id))
    }

    /* /// Format a type-name for debug output
    fn format_type(self: &Self, type_id: Option<TypeId>) -> String {
        if let Some(type_id) = type_id {
            format!("{:?}", self.scopes.type_ref(type_id))
        } else {
            "???".to_string()
        }
    } */
}

/// Methods to resolve individual AST structures.
impl<'a, 'b> Resolver<'a, 'b> {

    /// Resolves types and bindings used in a statement.
    fn resolve_statement(self: &mut Self, item: &mut ast::Statement<'a>) {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.resolve_function(function),
            S::Structure(structure)     => self.resolve_structure(structure),
            S::Binding(binding)         => self.resolve_binding(binding),
            S::IfBlock(if_block)        => self.resolve_if_block(if_block),
            S::ForLoop(for_loop)        => self.resolve_for_loop(for_loop),
            S::WhileLoop(while_loop)    => self.resolve_while_loop(while_loop),
            S::Block(block)             => self.resolve_block(block, None),
            S::Return(ret)              => self.resolve_return(ret),
            S::Expression(expression)   => self.resolve_expression(expression, None),
        }
    }

    /// Resolves types and bindings used in an expression.
    fn resolve_expression(self: &mut Self, item: &mut ast::Expression<'a>, expected_result: Option<TypeId>) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.resolve_literal(literal, expected_result),
            E::Variable(variable)       => self.resolve_variable(variable),
            E::Call(call)               => self.resolve_call(call),
            E::Member(_)                => { /* nothing to do here */ },
            E::Assignment(assignment)   => self.resolve_assignment(assignment),
            E::BinaryOp(binary_op)      => { // fixme: disabled for now
                //if binary_op.left.is_literal() && binary_op.right.is_literal() {
                //    self.precompute_expression_binary_op(item);
                //} else {
                    self.resolve_binary_op(binary_op);
                //}
            }
            E::UnaryOp(unary_op)        => self.resolve_unary_op(unary_op),
            E::Block(block)             => self.resolve_block(block, expected_result),
            E::IfBlock(if_block)        => self.resolve_if_block(if_block),
        };
    }

    /*
    /// Computes the result of a literal x literal binary expression and alters the item variant to literal.
    fn precompute_expression_binary_op(self: &mut Self, item: &mut ast::Expression<'a>) {

        use crate::frontend::ast::BinaryOperator as O;

        let binary_op = item.as_binary_op().expect("precompute_expression_binary_op received non-literal expression");
        let left_type_id = self.scopes.binding_type_id(binary_op.left.binding_id().expect("missing binding id"));
        let right_type_id = self.scopes.binding_type_id(binary_op.right.binding_id().expect("missing binding id"));

        match binary_op.op {
            O::And | O::Or => {
                if left_type_id != right_type_id || left_type_id.unwrap() != self.primitives.bool {
                    panic!("Logical {:?} expects both operands to be boolean, got {:?} and {:?}", binary_op.op, self.format_type(left_type_id), self.format_type(right_type_id));
                }
                let lval = binary_op.left.as_literal().unwrap().value.as_bool().unwrap();
                let rval = binary_op.right.as_literal().unwrap().value.as_bool().unwrap();
                let result = if binary_op.op == O::And {
                    lval && rval
                } else {
                    lval || rval
                };
                *item = ast::Expression::Literal(ast::Literal {
                    value       : ast::LiteralValue::Bool(result),
                    type_name   : None,
                    binding_id  : None,
                });
                self.set_bindingtype_id(item, self.primitives.bool);
            }
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual => {
                let result;
                if binary_op.left.as_literal().unwrap().value.as_bool().is_some() {
                    let lval = binary_op.left.as_literal().unwrap().value.as_bool().unwrap();
                    let rval = binary_op.right.as_literal().unwrap().value.as_bool().unwrap();
                    result = match binary_op.op {
                        O::Less         => lval < rval,
                        O::Greater      => lval > rval,
                        O::LessOrEq     => lval <= rval,
                        O::GreaterOrEq  => lval >= rval,
                        O::Equal        => lval == rval,
                        O::NotEqual     => lval != rval,
                        _ => unreachable!(),
                    };
                } else if binary_op.left.as_literal().unwrap().value.as_numeric().is_some() {
                    let lval = binary_op.left.as_literal().unwrap().value.as_numeric().unwrap();
                    let rval = binary_op.right.as_literal().unwrap().value.as_numeric().unwrap();
                    result = match binary_op.op {
                        O::Less         => lval < rval,
                        O::Greater      => lval > rval,
                        O::LessOrEq     => lval <= rval,
                        O::GreaterOrEq  => lval >= rval,
                        O::Equal        => lval == rval,
                        O::NotEqual     => lval != rval,
                        _ => unreachable!(),
                    };
                } else {
                    let lval = binary_op.left.as_literal().unwrap().value.as_string().unwrap();
                    let rval = binary_op.right.as_literal().unwrap().value.as_string().unwrap();
                    result = match binary_op.op {
                        O::Less         => lval < rval,
                        O::Greater      => lval > rval,
                        O::LessOrEq     => lval <= rval,
                        O::GreaterOrEq  => lval >= rval,
                        O::Equal        => lval == rval,
                        O::NotEqual     => lval != rval,
                        _ => unreachable!(),
                    };
                }
                *item = ast::Expression::Literal(ast::Literal {
                    value       : ast::LiteralValue::Bool(result),
                    type_name   : None,
                    binding_id  : None,
                });
                self.set_bindingtype_id(item, self.primitives.bool);
            },
            O::Add | O::Sub | O::Mul | O::Div | O::Rem => {
                if binary_op.left.as_literal().unwrap().value.as_numeric().is_some() {
                    let lval = binary_op.left.as_literal().unwrap().value.as_numeric().unwrap();
                    let rval = binary_op.right.as_literal().unwrap().value.as_numeric().unwrap();
                    let result = match binary_op.op {
                        O::Add  => lval + rval,
                        O::Sub  => lval - rval,
                        O::Mul  => lval * rval,
                        O::Div  => lval / rval,
                        O::Rem  => lval % rval,
                        _ => unreachable!(),
                    };
                    *item = ast::Expression::Literal(ast::Literal {
                        value       : ast::LiteralValue::Numeric(result),
                        type_name   : None,
                        binding_id  : None,
                    });
                }
            },
            O::Range | O::Index => {
                unimplemented!("range/index");
            },
            O::Assign | O::AddAssign | O::SubAssign | O::MulAssign | O::DivAssign | O::RemAssign => {
                unimplemented!("assignments");
            },
        }
    }
    */

    /// Resolves a function signature.
    fn resolve_signature(self: &mut Self, item: &mut ast::Signature<'a>) {
        // resolve arguments
        for arg in item.args.iter_mut() {
            self.resolve_binding(arg);
        }
        // resolve return type
        if let Some(ret) = &mut item.ret {
            self.resolve_type(ret);
        }
    }

    /// Resolves a function defintion.
    fn resolve_function(self: &mut Self, item: &mut ast::Function<'a>) {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        self.resolve_signature(&mut item.sig);
        //let signature_scope_id = self.try_create_scope(&mut item.scope_id); // todo: put body into separate scope so signature can't access body
        if item.function_id.is_none() && item.sig.ret_resolved() && item.sig.args_resolved() {
            let result_type_id = item.sig.ret_type_id();
            let arg_type_ids: Vec<_> = item.sig.arg_type_ids().iter().map(|arg| arg.unwrap()).collect();
            let function_id = self.scopes.insert_function(parent_scope_id, item.sig.name, result_type_id, arg_type_ids);

            item.function_id = Some(function_id);
            self.scopes.set_scopefunction_id(self.scope_id, function_id);
            // todo: needs to check that block return type matches
        }
        if let Some(function_id) = item.function_id {
            let function_type = self.scopes.function_type(function_id);
            self.resolve_block(&mut item.block, function_type.ret_type);
        }
        self.scope_id = parent_scope_id;
    }

    /// Resolves a struct definition.
    fn resolve_structure(self: &mut Self, item: &mut ast::Struct<'a>) {

        for (_, field) in &mut item.fields {
            self.resolve_type(field);
        }

        if item.type_id.is_none() {

            let mut fields = Vec::new();

            for (_, (field_name, field_type)) in item.fields.iter().enumerate() {
                fields.push((field_name.to_string(), field_type.type_id));
            }

            let ty = Type::Struct(Struct { fields });
            item.type_id = Some(self.scopes.insert_type(self.scope_id, Some(item.name), ty));
        }
    }

    /// Resolves a return statement.
    fn resolve_return(self: &mut Self, item: &mut ast::Return<'a>) {
        let function_id = self.scopes.lookup_scopefunction_id(self.scope_id).expect("Encountered return outside of function");
        let ret_type_id = self.scopes.function_type(function_id).ret_type;
        if ret_type_id.is_some() && item.expr.is_none() {
            panic!("Expected return value");
        } else if ret_type_id.is_none() && item.expr.is_some() {
            panic!("Unexpected return value");
        }
        if let Some(expr) = &mut item.expr {
            item.fn_ret_type_id = ret_type_id;
            self.resolve_expression(expr, ret_type_id);
            let expression_type_id = self.bindingtype_id(expr);
            if expression_type_id.is_some() && ret_type_id != expression_type_id {
                panic!("Expected return type {:?}, got {:?}", self.scopes.type_ref(ret_type_id.unwrap()), self.scopes.type_ref(expression_type_id.unwrap()));
            }
        }
    }

    /// Resolves an occurance of a function call.
    fn resolve_call(self: &mut Self, item: &mut ast::Call<'a>) {

        // locate function definition
        if item.function_id.is_none() {
            item.function_id = self.scopes.lookup_function_id(self.scope_id, item.name);
        }

        // found a function, resolve return type and arguments
        if let Some(function_id) = item.function_id {

            // return value
            let function_types = self.scopes.function_type(function_id).clone();
            if let Some(ret_type_id) = function_types.ret_type {
                self.set_bindingtype_id(item, ret_type_id);
            } else {
                self.set_bindingtype_id(item, TypeId::void());
            }

            // arguments
            for (index, &expected_type) in function_types.arg_type.iter().enumerate() {
                self.resolve_expression(&mut item.args[index], Some(expected_type));
                let actual_type = self.bindingtype_id(&mut item.args[index]);
                // infer arguments
                if actual_type.is_none() {
                    self.set_bindingtype_id(&mut item.args[index], expected_type);
                } else if actual_type.is_some() && actual_type.unwrap() != expected_type  {
                    panic!("Function {}, argument {}: Expected {:?}, got {:?}.", item.name, index + 1, self.scopes.type_ref(expected_type), self.scopes.type_ref(actual_type.unwrap()));
                }
            }

            // rustcall?
            if let Some(rust_fn_index) = function_types.rust_fn_index() {
                item.rust_fn_index = Some(rust_fn_index);
            }
        }
    }

    /// Resolves a type (name) to a type_id.
    fn resolve_type(self: &Self, item: &mut ast::TypeName<'a>) -> Option<TypeId> {
        if item.type_id.is_none() {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &item.path[0]) { // fixme: handle path segments
                item.type_id = Some(new_type_id);
            }
        } else if let Some(type_id) = item.type_id {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &item.path[0]) { // fixme: handle path segments
                if type_id != new_type_id {
                    panic!("type resolution result changed, aka 'this should never happen'"); // todo: remove this whole else branch
                }
            }
        }
        item.type_id
    }

    /// Resolves an occurance of a variable.
    fn resolve_variable(self: &mut Self, item: &mut ast::Variable<'a>) {
        // resolve binding
        if item.binding_id.is_none() {
            item.binding_id = self.scopes.lookup_binding_id(self.scope_id, item.name);
            if item.binding_id.is_none() {
                panic!("unknown binding {:?} in scope {:?}", item.name, self.scope_id); // todo: error handling
            }
        }
    }

    /// Resolves an if block.
    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock<'a>) {
        // resolve condition and block
        self.resolve_expression(&mut item.cond, None); // todo: pass in expected type
        self.resolve_block(&mut item.if_block, None);
        // optionally resolve else block
        if let Some(else_block) = &mut item.else_block {
            self.resolve_block(else_block, None); // todo: pass in expected type
            if let (Some(if_type_id), Some(else_type_id)) = (self.bindingtype_id(&mut item.if_block), self.bindingtype_id(else_block)) {
                if if_type_id != else_type_id {
                    panic!("if/else return type mismatch {:?} vs {:?}", if_type_id, else_type_id); // TODO: error handling, attempt cast
                }
            }
        }
    }

    /// Resolves a for loop.
    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop<'a>) {

        // resolve the range type
        self.resolve_expression(&mut item.range, None);

        if let Some(type_id) = self.bindingtype_id(&mut item.range) {

            let parent_scope_id = self.try_create_scope(&mut item.scope_id);

            // create binding and set type to range type
            if self.scopes.binding_id(self.scope_id, item.iter.name).is_none() {
                self.scopes.insert_binding(self.scope_id, Some(item.iter.name), Some(type_id));
            }

            //self.set_type_from_id(&mut item.iter.type_id, type_id);

            // handle block
            self.resolve_block(&mut item.block, None);

            self.scope_id = parent_scope_id;
        }
    }

    /// Resolves a while loop.
    fn resolve_while_loop(self: &mut Self, item: &mut ast::WhileLoop<'a>) {
        self.resolve_expression(&mut item.expr, None);
        self.resolve_block(&mut item.block, None);
    }

    /// Resolves a block.
    fn resolve_block(self: &mut Self, item: &mut ast::Block<'a>, expected_result: Option<TypeId>) {
        for mut statement in item.statements.iter_mut() {
            self.resolve_statement(&mut statement);
        }
        if let Some(ref mut result) = item.result {
            self.resolve_expression(result, expected_result);
        }
    }

    /// Resolves an assignment expression.
    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment<'a>) {
        self.resolve_variable(&mut item.left);
        let left_type_id = self.bindingtype_id(&mut item.left);
        self.resolve_expression(&mut item.right, left_type_id);
        if item.left.binding_id.is_some() && self.bindingtype_id(&mut item.left).is_none() && self.bindingtype_id(&mut item.right).is_some() {
            let right_type_id = self.bindingtype_id(&mut item.right).unwrap();
            self.set_bindingtype_id(&mut item.left, right_type_id);
        }
    }

    /// Resolves a binary operation.
    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp<'a>) {

        use crate::frontend::ast::BinaryOperator as O;

        self.resolve_expression(&mut item.left, None); // todo: set expected type
        self.resolve_expression(&mut item.right, None);

        let type_id = self.bindingtype_id(item);
        let left_type_id = self.bindingtype_id(&mut item.left);
        let right_type_id = self.bindingtype_id(&mut item.right);

        // todo: enable again, but not for index
        /*if left_type_id.is_some() && right_type_id.is_some() && left_type_id != right_type_id {
            panic!("Binary operator {:?} expects both operands to be of same type, got {:?} and {:?} on {:?}", item.op, self.format_type(left_type_id), self.format_type(right_type_id), item);
        }*/

        match item.op {
            O::And | O::Or => {
                /*if left_type_id != right_type_id || left_type_id.unwrap() != self.primitives.bool {
                    panic!("Logical {:?} expects both operands to be boolean, got {:?} and {:?}", item.op, self.format_type(left_type_id), self.format_type(right_type_id));
                }*/
                self.set_bindingtype_id(item, self.primitives.bool);
                self.set_bindingtype_id(&mut item.left, self.primitives.bool);
                self.set_bindingtype_id(&mut item.right, self.primitives.bool);
            }
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual => {
                self.set_bindingtype_id(item, self.primitives.bool);
                if let Some(common_type_id) = left_type_id.or(right_type_id) {
                    self.set_bindingtype_id(&mut item.left, common_type_id);
                    self.set_bindingtype_id(&mut item.right, common_type_id);
                }
            }
            O::Add | O::Sub | O::Mul | O::Div | O::Rem => {
                if let Some(common_type_id) = type_id.or(left_type_id).or(right_type_id) {
                    self.set_bindingtype_id(item, common_type_id);
                    self.set_bindingtype_id(&mut item.left, common_type_id);
                    self.set_bindingtype_id(&mut item.right, common_type_id);
                }
            }
            O::Index => {
                // left[right] : item
                self.set_bindingtype_id(&mut item.right, self.primitives.unsigned[2].type_id); // u32
                self.try_create_anon_binding(item);

                // if we know the result type, set the array element type to that
                if let Some(result_type_id) = self.bindingtype_id(item) {
                    let ty = self.bindingtype_mut(&mut item.left);
                    if let Some(Type::Array(array)) = ty {
                        array.type_id = Some(result_type_id); // todo: check that array.type_id is either None or equals binding_type_id
                    }
                }

                // if we know the array element type, set the result type to that
                if let Some(&Type::Array(Array { type_id: Some(element_type_id), .. })) = self.bindingtype(&item.left) {
                    self.set_bindingtype_id(item, element_type_id);
                }
            }
            O::Access => {
                // left.right : item
                if let Some(ty) = self.bindingtype(&item.left) {
                    let struct_ = ty.as_struct().expect("Member access on a non-struct");
                    let field = item.right.as_member_mut().expect("Internal error: Member access using a non-field");
                    if field.index.is_none() {
                        field.index = Some(struct_.fields.iter().position(|f| f.0 == field.name).expect("Unknown struct member") as u32);
                    }
                    if let Some(type_id) = struct_.fields[field.index.unwrap() as usize].1 {
                        self.set_bindingtype_id(&mut item.right, type_id);
                    }
                }
            }
            O::Range => {
                unimplemented!("range");
            }
            O::Assign | O::AddAssign | O::SubAssign | O::MulAssign | O::DivAssign | O::RemAssign => {
                unimplemented!("assignments");
            }
        }
    }

    /// Resolves a binding created by let, for or a signature.
    fn resolve_binding(self: &mut Self, item: &mut ast::Binding<'a>) {

        // check if a type is specified
        let explicit = match item.type_name {
            Some(ref mut ty) => {
                self.resolve_type(ty)
            },
            None => None,
        };

        // create binding id if we don't have one yet
        let binding_id = self.try_create_binding(item, item.name);

        // apply explicit type if we got one
        if let Some(explicit) = explicit {
            self.set_bindingtype_id(item, explicit);
        }

        // resolve right hand side, link bindings
        if let Some(expr) = &mut item.expr {
            *expr.binding_id_mut() = Some(binding_id);
            self.resolve_expression(expr, explicit);
        }

        // if we have a binding type, apply it to the expression
        let lhs = self.bindingtype_id(item);

        if let (Some(lhs), Some(expr)) = (lhs, &mut item.expr) {
            self.set_bindingtype_id(expr, lhs);
        }

        // if the expression has a type, apply it back to the binding
        if let Some(expr) = &mut item.expr {
            if let Some(expr_type) = self.bindingtype_id(expr) {
                self.set_bindingtype_id(item, expr_type);
            }
        };
    }

    /// Resolves a unary operation.
    fn resolve_unary_op(self: &mut Self, item: &mut ast::UnaryOp<'a>) {
        use crate::frontend::ast::UnaryOperator as UO;
        self.resolve_expression(&mut item.expr, None);
        match item.op {
            UO::Not => {
                self.set_bindingtype_id(item, self.primitives.bool);
            },
            UO::IncBefore | UO::DecBefore | UO::IncAfter | UO::DecAfter => {
                if let Some(type_id) = self.bindingtype_id(&mut item.expr) {
                    self.set_bindingtype_id(item, type_id);
                }
            },
        }
    }

    /// Resolves a literal type if it is annotated, otherwise let the parent expression pick a concrete type.
    fn resolve_literal(self: &mut Self, item: &mut ast::Literal<'a>, expected_type: Option<TypeId>) {
        use self::ast::LiteralValue as LV;

        if let LV::Bool(_) = item.value { // todo: all of these need to check expected type if any
            self.set_bindingtype_id(item, self.primitives.bool);
        } else if let LV::String(_) = item.value {
            self.set_bindingtype_id(item, self.primitives.string);
        } else if let LV::Array(_/*ref mut array*/) = item.value { // bck fail
            let binding_id = self.try_create_anon_binding(item);
            self.resolve_literal_array(item.value.as_array_mut().unwrap(), binding_id);
        } else if let LV::Struct(_) = item.value {
            self.resolve_literal_struct(item);
        } else if let Some(type_name) = &mut item.type_name {
            // literal has explicit type, use it
            if let Some(explicit_type_id) = self.resolve_type(type_name) {
                self.set_bindingtype_id(item, explicit_type_id);
                if expected_type.is_some() && expected_type.unwrap() != explicit_type_id {
                    panic!("type mismatch");
                }
            }
        } else if let (LV::Numeric(_), Some(expected_type)) = (&item.value, expected_type) {
            self.set_bindingtype_id(item, expected_type);
        } else if self.infer_literals {
            // numerics, once normal resolution has failed
            // todo: re-enable
            /*if let LV::Numeric(value) = item.value {
                if let Some(type_id) = self.primitives.classify_numeric(value) {
                    self.set_bindingtype_id(item, type_id);
                }
            }*/
        } else {
            self.try_create_anon_binding(item);
        }


        // todo: typehint
        /* else if let Some(type_hint) = type_hint {
            // we got a type hint, try to match it
            match item.value {
                LV::Bool(_) => if type_hint != self.primitives.bool { panic!("Inferred type is incompatible to this boolean literal") },
                LV::Numeric(n) => if !self.primitives.is_compatible_numeric(n) { panic!("Inferred type is incompatible to this numeric literal") },
                LV::String(_) => if type_hint != self.primitives.string { panic!("Inferred type is incompatible to this string literal") }
                LV::Array(ref n) => unimplemented!("Array literal {:?}", n) // todo: how to approach this?
            }
            self.set_bindingtype_id(item);
        } */
    }

    /// Resolves an struct literal and creates the required field types.
    fn resolve_literal_struct(self: &mut Self, item: &mut ast::Literal<'a>) {

        self.try_create_anon_binding(item);

        // resolve type from name

        let type_name = item.type_name.as_mut().unwrap();
        self.resolve_type(type_name);

        // resolve fields from field definition

        if let Some(type_id) = type_name.type_id {
            let struct_def = self.scopes.type_ref(type_id).as_struct().unwrap().clone(); // todo: this sucks
            let struct_ = item.value.as_struct_mut().unwrap();
            for (name, field) in &mut struct_.fields {
                self.try_create_anon_binding(field);
                self.resolve_literal(field, struct_def.type_id(name));
            }
        }
    }

    /// Resolves an array literal and creates the required array types.
    fn resolve_literal_array(self: &mut Self, array: &mut ast::ArrayLiteral<'a>, binding_id: BindingId) {

        // apply the same binding id to all elements
        let element_binding_id = self.try_create_anon_binding(array.elements.first_mut().unwrap());
        let mut element_type_id = None;

        for element in &mut array.elements {
            *element.binding_id_mut() = Some(element_binding_id);
            self.resolve_literal(element, None);
            if element_type_id == None {
                element_type_id = self.bindingtype_id(element);
            } else if element_type_id != None && element_type_id != self.bindingtype_id(element) {
                panic!("array element type mismatch {:?} - {:?}", self.scopes.type_ref(element_type_id.unwrap()), self.scopes.type_ref(self.bindingtype_id(element).unwrap()));
            }
        }

        // create this level's type based on the inner type
        if self.scopes.binding_type_id(binding_id).is_none() {
            let new_type_id = self.scopes.insert_type(self.scope_id, None, Type::Array(Array {
                len     : Some(array.elements.len()),
                type_id : element_type_id,
            }));
            *self.scopes.binding_type_id_mut(binding_id) = Some(new_type_id);
        }

        // set inner type based on this level's type
        if let Some(type_id) = self.scopes.binding_type_id(binding_id) {
            let ty = self.scopes.type_ref(type_id);
            if let Some(element_type_id) = ty.as_array().unwrap().type_id {
                self.set_bindingtype_id(array.elements.first_mut().unwrap(), element_type_id); // all elements have the same binding id, so set only first item type
            }
        }
    }
}