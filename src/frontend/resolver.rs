//! AST type checker and resolver.

mod scopes;
mod primitives;

use std::{cell::Cell, collections::HashMap, marker::PhantomData};
use crate::frontend::ast::{self, Typeable, Bindable};
use crate::frontend::util::{ScopeId, TypeId, BindingId, FunctionId, Type, Array};
use crate::{ExternRust, Standalone};
use std::fmt::Debug;

/// Parsed program AST with all types, bindings and other language structures resolved.
#[derive(Debug)]
pub struct ResolvedProgram<'a, T> where T: ExternRust<T> {
    ty: PhantomData<T>,
    /// Program AST with types and bindings resolved.
    pub ast: super::Program<'a>,
    /// Mapping from BindingId (vector index) to TypeId
    pub(crate) type_ids: Vec<TypeId>,
    /// Mapping from TypeId (vector index) to primitive type.
    pub(crate) types: Vec<Type>,
    /// Function id of the entry/main function.
    pub(crate) entry_fn: FunctionId,
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
}

/// Resolves types within the given program AST structure.
#[allow(invalid_type_param_default)]
pub fn resolve<'a, T=Standalone>(mut program: super::Program<'a>, entry: &str) -> ResolvedProgram<'a, T> where T: ExternRust<T> {

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
                scopes
                    .type_id(root_scope_id, if arg_type_name == &"& str" { "String" } else { arg_type_name })
                    .expect(&format!("Unknown argument type encountered: {}", &arg_type_name))
            })
            .collect();
        scopes.insert_rustfn(root_scope_id, *name, *index, ret_type, arg_type_id);
    }

    let mut now_unresolved = 0;
    let mut prev_unresolved;

    loop {
        prev_unresolved = now_unresolved;
        for mut statement in program.iter_mut() {
            let mut resolver = Resolver {
                stop_bugging_me_about_this_lifetime_every_fkn_refactoring: PhantomData,
                scope_id        : root_scope_id,
                scopes          : &mut scopes,
                primitives      : &primitives,
            };
            resolver.resolve_statement(&mut statement);
        }
        now_unresolved = scopes.num_unresolved();
        println!("num unresolved: {}", now_unresolved);
        if now_unresolved == 0 || now_unresolved == prev_unresolved {
            break;
        }
    }

    println!("{:#?}", program);

    let entry_fn = scopes.lookup_function_id(root_scope_id, entry).expect("Failed to resolve entry function");
    let (type_ids, types) = scopes.into();

    ResolvedProgram {
        ty      : PhantomData,
        ast     : program,
        entry_fn: entry_fn,
        types   : types,
        type_ids: type_ids,
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
                unimplemented!("shadowing"); // todo: support shadowing
            }
            let binding_id = self.scopes.insert_binding(self.scope_id, name, None); // todo: a &mut would be nicer than the index
            *item.binding_id_mut() = Some(binding_id);
            println!("binding {} = {:?}", name, binding_id);
            binding_id
        }
    }

    /// Return existing binding-id or create and return new binding-id.
    fn try_create_anon_binding(self: &mut Self, item: &mut impl Bindable) -> BindingId {
        if let Some(binding_id) = item.binding_id() {
            binding_id
        } else {
            let binding_id = self.scopes.insert_anon_binding(self.scope_id, None); // todo: a &mut would be nicer than the index
            *item.binding_id_mut() = Some(binding_id);
            binding_id
        }
    }

    /// Sets given TypeId for the given binding.
    fn set_bindingtype_id<T>(self: &mut Self, item: &mut T, new_type_id: TypeId) where T: Bindable {
        let binding_id = self.try_create_anon_binding(item);
        let type_id = self.scopes.binding_type_mut(binding_id);
        if let Some(type_id) = *type_id {
            if type_id != new_type_id {
                panic!("attempted to change already resolved type {:?} to {:?}", self.get_type(type_id), self.get_type(new_type_id));
            }
        }
        *type_id = Some(new_type_id);
    }

    // Returns TypeId for the given binding.
    fn bindingtype_id<T>(self: &mut Self, item: &mut T) -> Option<TypeId> where T: Bindable+Debug {
        let binding_id = self.try_create_anon_binding(item);
        self.scopes.binding_type(binding_id)
    }

    /// Returns the type for given TypeId
    fn get_type(self: &Self, type_id: TypeId) -> &Type {
        self.scopes.lookup_type(type_id)
    }

    /// Format a type-name for debug output
    fn format_type(self: &Self, type_id: Option<TypeId>) -> String {
        if let Some(type_id) = type_id {
            format!("{:?}", self.get_type(type_id))
        } else {
            "<Unresolved>".to_string()
        }
    }
}

/// Methods to resolve individual AST structures.
impl<'a, 'b> Resolver<'a, 'b> {

    /// Resolves types and bindings used in a statement.
    fn resolve_statement(self: &mut Self, item: &mut ast::Statement<'a>) {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.resolve_function(function),
            S::Structure(_structure)     => { }, // todo: handle structures
            S::Binding(binding)         => self.resolve_binding(binding),
            S::IfBlock(if_block)        => self.resolve_if_block(if_block, None),
            S::ForLoop(for_loop)        => self.resolve_for_loop(for_loop),
            S::WhileLoop(while_loop)    => self.resolve_while_loop(while_loop),
            S::Block(block)             => self.resolve_block(block, None),
            S::Return(ret)              => self.resolve_return(ret),
            S::Expression(expression)   => self.resolve_expression(expression, None),
        }
    }

    /// Resolves types and bindings used in an expression.
    fn resolve_expression(self: &mut Self, item: &mut ast::Expression<'a>, type_hint: Option<TypeId>) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.resolve_literal(literal, type_hint),
            E::Variable(variable)       => self.resolve_variable(variable, type_hint),
            E::Call(call)               => self.resolve_call(call, type_hint),
            E::Assignment(assignment)   => self.resolve_assignment(assignment, type_hint),
            E::BinaryOp(binary_op)      => { // fixme: disabled for now
                //if binary_op.left.is_literal() && binary_op.right.is_literal() {
                //    self.precompute_expression_binary_op(item, type_hint);
                //} else {
                    self.resolve_binary_op(binary_op, type_hint);
                //}
            }
            E::UnaryOp(unary_op)        => self.resolve_unary_op(unary_op, type_hint),
            E::Block(block)             => self.resolve_block(block, type_hint),
            E::IfBlock(if_block)        => self.resolve_if_block(if_block, type_hint),
        };
    }

    /* /// Computes the result of a literal x literal binary expression and alters the item variant to literal.
    fn precompute_expression_binary_op(self: &mut Self, item: &mut ast::Expression<'a>, _type_hint: Option<TypeId>) {

        use crate::frontend::ast::BinaryOperator as O;

        let binary_op = item.as_binary_op().expect("precompute_expression_binary_op received non-literal expression");
        let left_type_id = binary_op.left.type_id();
        let right_type_id = binary_op.right.type_id();

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
                //self.flag_rewritten();
                *item = ast::Expression::Literal(ast::Literal {
                    value       : ast::LiteralValue::Bool(result),
                    type_name   : None,
                    type_id     : Some(self.primitives.bool),
                    binding_id  : None,
                });
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
                //self.flag_rewritten();
                *item = ast::Expression::Literal(ast::Literal {
                    value       : ast::LiteralValue::Bool(result),
                    type_name   : None,
                    type_id     : Some(self.primitives.bool),
                    binding_id  : None,
                });
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
                    //self.flag_rewritten();
                    *item = ast::Expression::Literal(ast::Literal {
                        value       : ast::LiteralValue::Numeric(result),
                        type_name   : None,
                        type_id     : None,
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
    } */

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
                panic!("Expected return type {:?}, got {:?}", self.get_type(ret_type_id.unwrap()), self.get_type(expression_type_id.unwrap()));
            }
        }
    }

    /// Resolves an occurance of a function call.
    fn resolve_call(self: &mut Self, item: &mut ast::Call<'a>, _type_hint: Option<TypeId>) {

        // locate function definition
        if item.function_id.is_none() {
            item.function_id = self.scopes.lookup_function_id(self.scope_id, item.path.0[0]);      // fixme: need full path here
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
                if actual_type.is_some() && actual_type.unwrap() != expected_type  {
                    panic!("Function {}, argument {}: Expected {:?}, got {:?}.", item.path.0[0], index + 1, self.get_type(expected_type), self.get_type(actual_type.unwrap()));
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
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &item.name.0[0]) { // fixme: handle path segments
                item.type_id = Some(new_type_id);
                //self.inc_resolved()
            }
        } else if let Some(type_id) = item.type_id {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &item.name.0[0]) {
                if type_id != new_type_id {
                    panic!("type resolution result changed, aka 'this should never happen'"); // todo: remove this whole else branch
                }
            }
        }
        item.type_id
    }

    fn resolve_array_literal_binding(self: &mut Self, item: &mut ast::Array<'a>, binding_name: &str) {
        let array_name = format!("{}[]", binding_name);
        self.try_create_binding(item, &array_name);

        // recursively descend resolve contained arrays
        if let ast::Literal { value: ast::LiteralValue::Array(ref mut array), .. } = item.items[0] {
            self.resolve_array_literal_binding(array, &array_name);
        }
    }

    /// Resolves a binding created by let, for or a signature
    fn resolve_binding(self: &mut Self, item: &mut ast::Binding<'a>) {

        // ensure we have a BindingId
        let binding_id = self.try_create_binding(item, item.name);

        // bind contained array literals // todo: should happen in resolve_array but we need the binding name and don't have parent access
        if let Some(ast::Expression::Literal(ast::Literal { value: ast::LiteralValue::Array(ref mut array), ..})) = item.expr {
            self.resolve_array_literal_binding(array, item.name);
        }

        // get types for lhs(ty) and rhs(expr)
        let lhs = match item.type_name {
            Some(ref mut ty) => {
                self.resolve_type(ty)
            },
            None => None,
        };
        let rhs = match item.expr {
            Some(ref mut expr) => {
                self.resolve_expression(expr, lhs.or(self.scopes.binding_type(binding_id)));
                self.bindingtype_id(expr)
            },
            None => None,
        };

        // have explicit type and a resolved type for right hand side, check that they match
        if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
            if lhs != rhs && !self.primitives.is_compatible(rhs, lhs) {
                panic!("incompatible types {:?} and {:?}", self.get_type(lhs), self.get_type(rhs)); // TODO: error handling

                //todo: don't just check here, SET the type and binding for the right hand side


            }
        }

        // apply type to binding
        if let Some(lhs) = lhs {
            // have explicit type: use it
            self.set_bindingtype_id(item, lhs);
        } else if let Some(rhs) = rhs {
            // expression has type: use that
            self.set_bindingtype_id(item, rhs);
        }
    }

    /// Resolves an occurance of a variable.
    fn resolve_variable(self: &mut Self, item: &mut ast::Variable<'a>, type_hint: Option<TypeId>) {
        // resolve binding
        if item.binding_id.is_none() {
            item.binding_id = self.scopes.lookup_binding_id(self.scope_id, item.path.0[0]);      // fixme: need full path here
            if item.binding_id.is_none() {
                panic!("unknown binding {:?} in scope {:?}", item.path.0, self.scope_id); // todo: error handling
            }
        }
/*        // resolve type
        if let Some(binding_id) = item.binding_id {
            if let Some(binding_type_id) = self.scopes.binding_type(binding_id) {
                // from binding
                //self.set_type_from_id(&mut item.type_id, binding_type_id);
            } else if let Some(type_hint_id) = type_hint {
                // from type hint
                //self.set_type_from_id(&mut item.type_id, type_hint_id);
                self.set_bindingtype_id(item, type_hint_id);
            }
        }  */
    }

    /// Resolves an if block.
    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock<'a>, _type_hint: Option<TypeId>) {
        // resolve condition and block
        self.resolve_expression(&mut item.cond, None);
        self.resolve_block(&mut item.if_block, None);
        // optionally resolve else block
        if let Some(else_block) = &mut item.else_block {
            self.resolve_block(else_block, None);
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
                self.scopes.insert_binding(self.scope_id, item.iter.name, Some(type_id));
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
    fn resolve_block(self: &mut Self, item: &mut ast::Block<'a>, type_hint: Option<TypeId>) {
        for mut statement in item.statements.iter_mut() {
            self.resolve_statement(&mut statement);
        }
        if let Some(ref mut result) = item.result {
            self.resolve_expression(result, type_hint);
        }
    }

    /// Resolves an assignment expression.
    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment<'a>, _type_hint: Option<TypeId>) {
        self.resolve_variable(&mut item.left, None);
        let left_type_id = self.bindingtype_id(&mut item.left);
        self.resolve_expression(&mut item.right, left_type_id);
        if item.left.binding_id.is_some() && self.bindingtype_id(&mut item.left).is_none() && self.bindingtype_id(&mut item.right).is_some() {
            let right_type_id = self.bindingtype_id(&mut item.right).unwrap();
            self.set_bindingtype_id(&mut item.left, right_type_id);
        }
    }

    /// Resolves a binary operation.
    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp<'a>, type_hint: Option<TypeId>) {

        use crate::frontend::ast::BinaryOperator as O;

        let right_type_id = self.bindingtype_id(&mut item.right);
        self.resolve_expression(&mut item.left, type_hint.or(right_type_id));

        let left_type_id = self.bindingtype_id(&mut item.left);
        self.resolve_expression(&mut item.right, type_hint.or(left_type_id));

        let left_type_id = self.bindingtype_id(&mut item.left);
        let right_type_id = self.bindingtype_id(&mut item.right);

        if left_type_id.is_some() && right_type_id.is_some() {

            if left_type_id != right_type_id {
                panic!("Binary operator {:?} expects both operands to be of same type, got {:?} and {:?} on {:?}", item.op, self.format_type(left_type_id), self.format_type(right_type_id), item);
            }

            match item.op {
                O::And | O::Or => {
                    if left_type_id != right_type_id || left_type_id.unwrap() != self.primitives.bool {
                        panic!("Logical {:?} expects both operands to be boolean, got {:?} and {:?}", item.op, self.format_type(left_type_id), self.format_type(right_type_id));
                    }
                    self.set_bindingtype_id(item, self.primitives.bool);
                }
                O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual => {
                    self.set_bindingtype_id(item, self.primitives.bool);
                },
                O::Add | O::Sub | O::Mul | O::Div | O::Rem => {
                    self.set_bindingtype_id(item, left_type_id.unwrap());
                },
                O::Range => {
                    unimplemented!("range");
                },
                O::Index => {
                    if item.binding_id.is_none() {
                        // todo: resolve item binding from left bindind_id. cannot just use name because it might be an expression
                    }
                    //unimplemented!("index");
                }
                O::Assign | O::AddAssign | O::SubAssign | O::MulAssign | O::DivAssign | O::RemAssign => {
                    unimplemented!("assignments");
                },
            }
        }
    }

    /// Resolves a unary operation.
    fn resolve_unary_op(self: &mut Self, item: &mut ast::UnaryOp<'a>, _type_hint: Option<TypeId>) {
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

    /* /// Resolves literal to its possible default type if all other type resolution failed.
    fn resolve_literal_default(self: &Self, item: &mut ast::Literal<'a>) {
        item.type_id = if let ast::LiteralValue::Numeric(value) = item.value {
            self.primitives.classify_numeric(value)
        } else {
            None
        };
    } */

    /* /// Resolves an array literal and creates the required array types.
    fn resolve_literal_array(self: &mut Self, item: &mut ast::Literal<'a>) {

        use crate::frontend::ast::{Literal, LiteralValue};

        let mut current_item = &mut *item;

        while let Literal { value: LiteralValue::Array(ref mut a), type_id: ref mut array_type_id, .. } = current_item {

            let mut item_type_id = None;

            for literal in a.items.iter_mut() {
                self.resolve_literal(literal, None);
                if item_type_id == None {
                    item_type_id = literal.type_id;
                } else if literal.type_id != None && item_type_id != literal.type_id {
                    panic!("array element type mismatch");
                }
            }

            a.type_id = item_type_id;

            if item_type_id.is_some() {
                let ty = Type::Array(Array {
                    len: Some(a.items.len()),
                    ty: Box::new(self.scopes.lookup_type(item_type_id.unwrap()).clone()),
                });
                let type_name = format!("{:?}", ty);
                *array_type_id = Some(self.scopes.insert_type(self.scope_id, type_name, ty));
            }

            if let Some(next_level) = a.items.get_mut(0) {
                current_item = next_level;
            } else {
                break;
            }
        }
    } */

    /// Resolves a literal type if it is annotated, otherwise let the parent expression pick a concrete type.
    fn resolve_literal(self: &mut Self, item: &mut ast::Literal<'a>, type_hint: Option<TypeId>) {
        use self::ast::LiteralValue as LV;
        if let Some(type_name) = &mut item.type_name {
            // literal has explicit type, use it
            self.resolve_type(type_name);
            // todo: compare against literal
        } else if let LV::Bool(_) = item.value {
            // literal boolean
            self.set_bindingtype_id(item, self.primitives.bool);
        } else if let LV::String(_) = item.value {
            // literal string
            self.set_bindingtype_id(item, self.primitives.string);
        } else if let LV::Array(_) = item.value {
            //self.resolve_literal_array(item);
        } else if let Some(type_hint) = type_hint {
            // we got a type hint, try to match it
            match item.value {
                LV::Bool(_) => if type_hint != self.primitives.bool { panic!("Inferred type is incompatible to this boolean literal") },
                LV::Numeric(n) => if !self.primitives.is_compatible_numeric(n, type_hint) { panic!("Inferred type is incompatible to this numeric literal") },
                LV::String(_) => if type_hint != self.primitives.string { panic!("Inferred type is incompatible to this string literal") }
                LV::Array(ref n) => unimplemented!("Array literal {:?}", n) // todo: how to approach this?
            }
            self.set_bindingtype_id(item, type_hint);
        }
    }
}