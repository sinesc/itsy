//! AST type checker and resolver.

mod scopes;
mod primitives;

use std::marker::PhantomData;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::frontend::ast;
use crate::frontend::util::{ScopeId, TypeId, BindingId, Type, FunctionId};
use crate::{ExternRust, Standalone};

/// Parsed program AST with all types, bindings and other language structures resolved.
#[derive(Debug)]
pub struct ResolvedProgram<'a, T> where T: ExternRust<T> {
    ty: PhantomData<T>,
    /// Program AST with types and bindings resolved.
    pub ast         : super::Program<'a>,
    /// Mapping from TypeId (vector index) to primitive type.
    pub(crate) types: Vec<Type>,
    /// Function id of the entry/main function.
    pub(crate) entry_fn: FunctionId,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum BindingFlag {
    //None,
    MustResolve,
    //ExpectedType(TypeId),
}

/// Internal state during program type/binding resolution.
struct Resolver<'a, 'b> where 'a: 'b {
    /// Counts resolved items.
    num_resolved: &'b RefCell<u32>,
    /// true if parts of the AST have been rewritten.
    did_rewrite : &'b RefCell<bool>,
    /// Scope id this state operates in.
    scope_id    : ScopeId,
    /// Repository of all scopes.
    scopes      : &'b mut scopes::Scopes<'a>,
    /// Grouped primitive types.
    primitives  : &'b primitives::Primitives,
    /// WIP todo: document or remove
    binding_flags   : &'b mut HashMap<BindingId, BindingFlag>,
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
        scopes.insert_rustfn(root_scope_id, name, *index, ret_type, arg_type_id);
    }

    // keep resolving until the number of resolved items no longer increases.
    // perf: ideally we count the number of unresolved items during parsing and then count it down to 0 here. This
    // would avoid the additional "have we resolved everything" check afterwards.

    let num_resolved = RefCell::new(0);
    let did_rewrite = RefCell::new(false);
    let mut num_resolved_before: u32;
    let mut binding_flags = HashMap::new();

    loop {
        num_resolved_before = *num_resolved.borrow();
        for mut statement in program.iter_mut() {
            let mut resolver = Resolver {
                num_resolved    : &num_resolved,
                did_rewrite     : &did_rewrite,
                scope_id        : root_scope_id,
                scopes          : &mut scopes,
                primitives      : &primitives,
                binding_flags   : &mut binding_flags,
            };
            resolver.resolve_statement(&mut statement);
        }
        if *did_rewrite.borrow() {
            *did_rewrite.borrow_mut() = false;
            *num_resolved.borrow_mut() = 0;
        } else if *num_resolved.borrow() == num_resolved_before {
            break;
        }
    }

    ResolvedProgram {
        ty      : PhantomData,
        ast     : program,
        entry_fn: scopes.lookup_function_id(root_scope_id, entry).expect("Failed to resolve entry function"),
        types   : scopes.into(),
    }
}

/// Utility methods to update a typeslot with a resolved type and increase the resolution counter.
impl<'a, 'b> Resolver<'a, 'b> {

    /// Increment resolution counter to signal that something has been resolved and another resolution pass should happen.
    fn inc_resolved(self: &Self) {
        *self.num_resolved.borrow_mut() += 1;
    }

    /// Mark AST as rewritten to ensure another resolution pass.
    fn flag_rewritten(self: &Self) {
        *self.did_rewrite.borrow_mut() = true;
    }

    /// Sets given type_id for the given binding. Increases resolution counter if a change was made.
    fn set_bindingtype_from_id(self: &mut Self, binding_id: BindingId, new_type_id: TypeId) {
        let mut type_id = self.scopes.binding_type(binding_id);
        self.set_type_from_id(&mut type_id, new_type_id); // todo: get borrowck to accept a binding_type_mut instead of the temp copy?
        *self.scopes.binding_type_mut(binding_id) = type_id;
    }

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
    fn try_create_binding(self: &mut Self, item: &mut ast::Binding<'a>) -> BindingId {
        if let Some(binding_id) = item.binding_id {
            binding_id
        } else {
            // this binding ast node wasn't processed yet. if the binding name already exists we're shadowing - which is NYI
            if self.scopes.binding_id(self.scope_id, item.name).is_some() {
                panic!("shadowing NYI"); // todo: support shadowing
            }
            let binding_id = self.scopes.insert_binding(self.scope_id, item.name, None); // todo: a &mut would be nicer than the index
            item.binding_id = Some(binding_id);
            binding_id
        }
    }

    /// Sets given type_id for the given unresolved type. Increases resolution counter if a change was made.
    fn set_type_from_id(self: &Self, type_id: &mut Option<TypeId>, new_type_id: TypeId) {
        if type_id.is_none() {
            *type_id = Some(new_type_id);
            self.inc_resolved()
        } else if let Some(type_id) = type_id {
            if *type_id != new_type_id {
                panic!("attempted to change already resolved type");
            }
        }
    }

    /// Sets given unresolved type to void.
    fn set_type_from_void(self: &Self, type_id: &mut Option<TypeId>) {
        if type_id.is_none() {
            *type_id = Some(TypeId::void());
            self.inc_resolved()
        } else if *type_id != Some(TypeId::void()) {
            panic!("attempted to change already resolved type");
        }
    }

    /// Resolves and sets named type for the given unresolved type. Increases resolution counter if a change was made.
    fn set_type_from_name(self: &Self, type_id: &mut Option<TypeId>, new_type: &[&'a str]) {
        if type_id.is_none() {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &new_type[0]) { // fixme: handle path segments
                *type_id = Some(new_type_id);
                self.inc_resolved()
            }
        } else if let Some(type_id) = type_id {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &new_type[0]) {
                if *type_id != new_type_id {
                    panic!("type resolution result changed, aka 'this should never happen'"); // todo: remove this whole else branch
                }
            }
        }
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
            E::BinaryOp(binary_op)      => {
                if binary_op.left.is_literal() && binary_op.right.is_literal() {
                    self.precompute_expression_binary_op(item, type_hint);
                } else {
                    self.resolve_binary_op(binary_op, type_hint);
                }
            }
            E::UnaryOp(unary_op)        => self.resolve_unary_op(unary_op, type_hint),
            E::Block(block)             => self.resolve_block(block, type_hint),
            E::IfBlock(if_block)        => self.resolve_if_block(if_block, type_hint),
        };
    }

    /// Computes the result of a literal x literal binary expression and alters the item variant to literal.
    fn precompute_expression_binary_op(self: &mut Self, item: &mut ast::Expression<'a>, _type_hint: Option<TypeId>) {

        use crate::frontend::ast::BinaryOperator as O;

        let binary_op = item.as_binary_op().expect("precompute_expression_binary_op received non-literal expression");
        let left_type_id = binary_op.left.get_type_id();
        let right_type_id = binary_op.right.get_type_id();

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
                self.flag_rewritten();
                *item = ast::Expression::Literal(ast::Literal {
                    value   : ast::LiteralValue::Bool(result),
                    ty      : None,
                    type_id : Some(self.primitives.bool),
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
                        _ => panic!(),
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
                        _ => panic!(),
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
                        _ => panic!(),
                    };
                }
                self.flag_rewritten();
                *item = ast::Expression::Literal(ast::Literal {
                    value   : ast::LiteralValue::Bool(result),
                    ty      : None,
                    type_id : Some(self.primitives.bool),
                });
            },
            O::Add | O::Sub | O::Mul | O::Div | O::Rem | O::Range => {
                if binary_op.left.as_literal().unwrap().value.as_numeric().is_some() {
                    let lval = binary_op.left.as_literal().unwrap().value.as_numeric().unwrap();
                    let rval = binary_op.right.as_literal().unwrap().value.as_numeric().unwrap();
                    let result = match binary_op.op {
                        O::Add  => lval + rval,
                        O::Sub  => lval - rval,
                        O::Mul  => lval * rval,
                        O::Div  => lval / rval,
                        O::Rem  => lval % rval,
                        //O::Range=> lval != rval,
                        _ => panic!(),
                    };
                    self.flag_rewritten();
                    *item = ast::Expression::Literal(ast::Literal {
                        value   : ast::LiteralValue::Numeric(result),
                        ty      : None,
                        type_id : None,
                    });
                }
            },
            O::Assign | O::AddAssign | O::SubAssign | O::MulAssign | O::DivAssign | O::RemAssign => {
                panic!("nyi");
            },
        }
    }

    /// Resolves a function signature.
    fn resolve_signature(self: &mut Self, item: &mut ast::Signature<'a>) {
        // resolve arguments // todo: create scope here
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
        if item.function_id.is_none() && item.sig.ret_resolved() && item.sig.args_resolved() {
            let result_type_id = item.sig.ret.as_ref().map_or(Some(TypeId::void()), |ret| ret.type_id);
            let arg_type_ids: Vec<_> = item.sig.args.iter().map(|arg| arg.type_id.unwrap()).collect();
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
            let expression_type_id = expr.get_type_id();
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
            if function_types.ret_type.is_some() {
                self.set_type_from_id(&mut item.type_id, function_types.ret_type.unwrap());
            } else {
                self.set_type_from_void(&mut item.type_id);
            }

            // arguments
            for (index, &expected_type) in function_types.arg_type.iter().enumerate() {
                self.resolve_expression(&mut item.args[index], Some(expected_type));
                let actual_type = item.args[index].get_type_id();
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
    fn resolve_type(self: &Self, item: &mut ast::Type<'a>) {
        self.set_type_from_name(&mut item.type_id, &item.name.0);
    }

    /// Resolves a binding created by let, for or a signature
    fn resolve_binding(self: &mut Self, item: &mut ast::Binding<'a>) {

        // ensure we have a BindingId
        let binding_id = self.try_create_binding(item);

        // get types for lhs(ty) and rhs(expr)
        let lhs = match item.ty {
            Some(ref mut ty) => {
                self.resolve_type(ty);
                ty.type_id
            },
            None => None,
        };
        let rhs = match item.expr {
            Some(ref mut expr) => {
                self.resolve_expression(expr, lhs.or(self.scopes.binding_type(binding_id)));
                expr.get_type_id()
            },
            None => None,
        };

        // have explicit type and a resolved type for right hand side, check that they match
        if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
            if lhs != rhs && !self.primitives.is_compatible(rhs, lhs) {
                panic!("incompatible types {:?} and {:?}", self.get_type(lhs), self.get_type(rhs)); // TODO: error handling
            }
        }

        // apply type to ast and binding
        if let Some(lhs) = lhs {
            // have explicit type: use it
            self.set_type_from_id(&mut item.type_id, lhs);
            self.set_bindingtype_from_id(binding_id, lhs);
        } else if let Some(rhs) = rhs {
            // expression has type: use that
            self.set_type_from_id(&mut item.type_id, rhs);
            self.set_bindingtype_from_id(binding_id, rhs);
        } else if let Some(&flag) = self.binding_flags.get(&binding_id) {
            // someone in a previous iteration encountered this binding and needs it to be resolved. see if we can come up with a default type
            if flag == BindingFlag::MustResolve {
                if let Some(ast::Expression::Literal(literal)) = &mut item.expr {
                    self.resolve_literal_default(literal);
                    self.set_bindingtype_from_id(binding_id, literal.type_id.expect("Failed to resolve default literal type"));
                } else {
                    panic!("encountered BindingFlag::MustResolve on non-literal binding.");
                }
            }
        } else if let Some(binding_type_id) = self.scopes.binding_type(binding_id) {
            // someone else set the binding type, accept it

            if rhs.is_some() && !self.primitives.is_compatible(rhs.unwrap(), binding_type_id) {
                panic!("incompatible types {:?} and {:?}", self.get_type(rhs.unwrap()), self.get_type(binding_type_id)); // TODO: error handling
            }
            self.set_type_from_id(&mut item.type_id, binding_type_id);
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
        // resolve type
        if let Some(binding_id) = item.binding_id {
            if let Some(binding_type_id) = self.scopes.binding_type(binding_id) {
                // from binding
                self.set_type_from_id(&mut item.type_id, binding_type_id);
            } else if let Some(type_hint_id) = type_hint {
                // from type hint
                self.set_type_from_id(&mut item.type_id, type_hint_id);
                self.set_bindingtype_from_id(binding_id, type_hint_id);
            }
        }
    }

    /// Resolves an if block.
    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock<'a>, _type_hint: Option<TypeId>) {
        self.resolve_expression(&mut item.cond, None);
        self.resolve_block(&mut item.if_block, None);
        if let Some(ref mut else_block) = item.else_block {
            self.resolve_block(else_block, None);
            if item.if_block.type_id != else_block.type_id
                && item.if_block.type_id.is_some() && else_block.type_id.is_some()
            { // TODO: used complete here
                panic!("if/else return type mismatch {:?} vs {:?}", item.if_block.type_id, else_block.type_id); // TODO: error handling, attempt cast
            }
        }
    }

    /// Resolves a for loop.
    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop<'a>) {

        // resolve the range type
        self.resolve_expression(&mut item.range, None);

        if let Some(type_id) = item.range.get_type_id() {

            let parent_scope_id = self.try_create_scope(&mut item.scope_id);

            // create binding and set type to range type
            if self.scopes.binding_id(self.scope_id, item.iter.name).is_none() {
                self.scopes.insert_binding(self.scope_id, item.iter.name, Some(type_id));
            }
            self.set_type_from_id(&mut item.iter.type_id, type_id);

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
            if let Some(type_id) = result.get_type_id() {
                self.set_type_from_id(&mut item.type_id, type_id);
            }
        } else {
            self.set_type_from_void(&mut item.type_id);
        }
    }

    /// Resolves an assignment expression.
    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment<'a>, _type_hint: Option<TypeId>) {
        self.resolve_variable(&mut item.left, None);
        self.resolve_expression(&mut item.right, item.left.type_id);
        if item.left.binding_id.is_some() && item.left.type_id.is_none() && item.right.get_type_id().is_some() {
            //self.set_type_from_id(&mut item.left.type_id, item.right.get_type_id().unwrap());
            self.set_bindingtype_from_id(item.left.binding_id.unwrap(), item.right.get_type_id().unwrap());
        }
    }

    fn require_resolved(self: &mut Self, item: &mut ast::Expression<'a>) {

        if let ast::Expression::Variable(var) = item {
            // binding type not resolved. since we can't directly access the binding here, set a flag to force it to resolve next iteration
            let binding_id = var.binding_id.expect("Unresolved binding id when trying to schedule binding-type resolution");
            self.binding_flags.insert(binding_id, BindingFlag::MustResolve);
            self.inc_resolved();
        }
    }


    /// Resolves a binary operation.
    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp<'a>, type_hint: Option<TypeId>) {

        use crate::frontend::ast::BinaryOperator as O;
        //use crate::frontend::ast::Expression as E;

        self.resolve_expression(&mut item.left, type_hint.or(item.right.get_type_id()));
        self.resolve_expression(&mut item.right, type_hint.or(item.left.get_type_id()));

        if !item.left.is_resolved() && !item.right.is_resolved() {
            self.require_resolved(&mut item.left);
            self.require_resolved(&mut item.right);// fixme: don't default-infer the second operand, have normal inferance handle this (add type_hint on resolve_variable and handle it)

        }

        if item.left.is_resolved() && item.right.is_resolved() {

            let left_type_id = item.left.get_type_id();
            let right_type_id = item.right.get_type_id();

            if left_type_id != right_type_id {
                panic!("Binary operator {:?} expects both operands to be of same type, got {:?} and {:?} on {:?}", item.op, self.format_type(left_type_id), self.format_type(right_type_id), item);
            }

            match item.op {
                O::And | O::Or => {
                    if left_type_id != right_type_id || left_type_id.unwrap() != self.primitives.bool {
                        panic!("Logical {:?} expects both operands to be boolean, got {:?} and {:?}", item.op, self.format_type(left_type_id), self.format_type(right_type_id));
                    }
                    self.set_type_from_id(&mut item.type_id, self.primitives.bool);
                }
                O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual => {
                    self.set_type_from_id(&mut item.type_id, self.primitives.bool);
                },
                O::Add | O::Sub | O::Mul | O::Div | O::Rem | O::Range => {
                    self.set_type_from_id(&mut item.type_id, left_type_id.unwrap());
                },
                O::Assign | O::AddAssign | O::SubAssign | O::MulAssign | O::DivAssign | O::RemAssign => {
                    panic!("nyi");
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
                self.set_type_from_id(&mut item.type_id, self.primitives.bool);
            },
            UO::IncBefore | UO::DecBefore | UO::IncAfter | UO::DecAfter => {
                if let Some(type_id) = item.expr.get_type_id() {
                    self.set_type_from_id(&mut item.type_id, type_id);
                }
            },
        }
    }

    /// Resolves literal to its possible default type if all other type resolution failed.
    fn resolve_literal_default(self: &Self, item: &mut ast::Literal<'a>) {
        item.type_id = if let ast::LiteralValue::Numeric(value) = item.value {
            self.primitives.classify_numeric(value)
        } else {
            None
        };
    }

    /// Resolves a literal type if it is annotated, otherwise let the parent expression pick a concrete type.
    fn resolve_literal(self: &Self, item: &mut ast::Literal<'a>, type_hint: Option<TypeId>) {
        use self::ast::LiteralValue as LV;
        if let Some(ty) = &mut item.ty {
            // numerical literal has explicit type, use it
            self.resolve_type(ty);
            self.set_type_from_id(&mut item.type_id, ty.type_id.unwrap());
        } else if let ast::LiteralValue::Bool(_) = item.value {
            // literal boolean
            self.set_type_from_id(&mut item.type_id, self.primitives.bool);
        } else if let ast::LiteralValue::String(_) = item.value {
            // literal string
            self.set_type_from_id(&mut item.type_id, self.primitives.string);
        } else if let Some(type_hint) = type_hint {
            // we got a type hint, try to match it
            match item.value {
                LV::Bool(_) => if type_hint != self.primitives.bool { panic!("Inferred type is incompatible to this boolean literal") },
                LV::Numeric(n) => if !self.primitives.is_compatible_numeric(n, type_hint) { panic!("Inferred type is incompatible to this numeric literal") },
                LV::String(_) => if type_hint != self.primitives.string { panic!("Inferred type is incompatible to this string literal") }
            }
            self.set_type_from_id(&mut item.type_id, type_hint);
        }
    }
}