//! AST type checker and resolver.

mod scopes;
mod primitives;

use std::marker::PhantomData;
use std::cell::RefCell;
use crate::frontend::ast;
use crate::frontend::util::{ScopeId, TypeId, BindingId, TypeSlot, Type, FunctionId};
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

/// Internal state during program type/binding resolution.
struct Resolver<'a, 'b> where 'a: 'b {
    /// Counts resolved items.
    pub counter     : &'b RefCell<u32>,
    /// Scope id this state operates in.
    pub scope_id    : ScopeId,
    /// Repository of all scopes.
    pub scopes      : &'b mut scopes::Scopes<'a>,
    /// Grouped primitive types.
    pub primitives  : &'b primitives::Primitives,
    /// Name of the entry/main function.
    pub entry_fn    : &'b str,
}

/// Resolves types within the given program AST structure.
#[allow(invalid_type_param_default)]
pub fn resolve<'a, T=Standalone>(mut program: super::Program<'a>, entry: &str) -> ResolvedProgram<'a, T> where T: ExternRust<T> {

    // create root scope and insert primitives
    let mut scopes = scopes::Scopes::new();
    let root_scope_id = scopes::Scopes::root_id();
    let primitives = primitives::Primitives::new(&mut scopes, root_scope_id);

    // keep resolving until the number of resolved items no longer increases.
    // perf: ideally we count the number of unresolved items during parsing and then count it down to 0 here. This
    // would avoid the additional "have we resolved everything" check afterwards.

    let num_resolved = RefCell::new(0);
    let mut num_resolved_before: u32;

    for (name, info) in T::call_info().iter() {
        let (index, ret_type_name, arg_type_name) = info;
        let ret_type = if *ret_type_name == "" {
            TypeSlot::Void
        } else {
            TypeSlot::TypeId(scopes.type_id(root_scope_id, ret_type_name).expect("Unknown return type encountered"))
        };
        let arg_type = arg_type_name
            .iter()
            .map(|arg_type| TypeSlot::TypeId(scopes.type_id(root_scope_id, arg_type).expect("Unknown argument type encountered")))
            .collect();
        scopes.insert_rustfn(root_scope_id, name, *index, ret_type, arg_type);
    }

    // todo: convert rust_fn signature type strings here

    loop {
        num_resolved_before = *num_resolved.borrow();
        for mut statement in program.iter_mut() {
            let mut resolver = Resolver {
                counter     : &num_resolved,
                scope_id    : root_scope_id,
                scopes      : &mut scopes,
                primitives  : &primitives,
                entry_fn    : entry,
            };
            resolver.resolve_statement(&mut statement);
        }
        if *num_resolved.borrow() == num_resolved_before {
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
            let binding_id = self.scopes.insert_binding(self.scope_id, item.name, TypeSlot::Unresolved); // todo: a &mut would be nicer than the index
            item.binding_id = Some(binding_id);
            binding_id
        }
    }

    /// Sets given type_id for the given unresolved type. Increases resolution counter if a change was made.
    fn set_type_from_id(self: &Self, type_id: &mut TypeSlot, new_type_id: TypeId) {
        if type_id.is_unresolved() {
            *type_id = TypeSlot::TypeId(new_type_id);
            *self.counter.borrow_mut() += 1
        } else if let TypeSlot::TypeId(type_id) = type_id {
            if *type_id != new_type_id {
                panic!("attempted to change already resolved type");
            }
        }
    }

    /// Sets given unresolved type to void.
    fn set_type_from_void(self: &Self, type_id: &mut TypeSlot) {
        if type_id.is_unresolved() {
            *type_id = TypeSlot::Void;
            *self.counter.borrow_mut() += 1
        } else if !type_id.is_void() {
            panic!("attempted to change already resolved type");
        }
    }

    /// Resolves and sets named type for the given unresolved type. Increases resolution counter if a change was made.
    fn set_type_from_name(self: &Self, type_id: &mut TypeSlot, new_type: &[&'a str]) {
        if type_id.is_unresolved() {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &new_type[0]) { // fixme: handle path segments
                *type_id = TypeSlot::TypeId(new_type_id);
                *self.counter.borrow_mut() += 1
            }
        } else if let TypeSlot::TypeId(type_id) = type_id {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &new_type[0]) {
                if *type_id != new_type_id {
                    panic!("type resolution result changed, aka 'this should never happen'"); // todo: remove this whole else branch
                }
            }
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
            S::IfBlock(if_block)        => self.resolve_if_block(if_block),
            S::ForLoop(for_loop)        => self.resolve_for_loop(for_loop),
            S::WhileLoop(while_loop)    => self.resolve_while_loop(while_loop),
            S::Block(block)             => self.resolve_block(block),
            S::Return(ret)              => self.resolve_return(ret),
            S::Expression(expression)   => self.resolve_expression(expression, None),
        }
    }

    /// Resolves types and bindings used in an expression.
    fn resolve_expression(self: &mut Self, item: &mut ast::Expression<'a>, type_hint: Option<TypeSlot>) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.resolve_literal(literal, type_hint),
            E::Variable(variable)       => self.resolve_variable(variable),
            E::Call(call)               => self.resolve_call(call),
            E::Assignment(assignment)   => self.resolve_assignment(assignment),
            E::BinaryOp(binary_op)      => self.resolve_binary_op(binary_op),
            E::UnaryOp(unary_op)        => self.resolve_unary_op(unary_op),
            E::Block(block)             => self.resolve_block(block),
            E::IfBlock(if_block)        => self.resolve_if_block(if_block),
        };
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
        self.resolve_block(&mut item.block);
        if item.function_id.is_none() && item.sig.ret_resolved() && item.sig.args_resolved() {
            let result = item.sig.ret.as_ref().map_or(TypeSlot::Void, |ret| ret.type_id);
            let args: Vec<_> = item.sig.args.iter().map(|arg| arg.type_id).collect();
            item.function_id = Some(self.scopes.insert_function(parent_scope_id, item.sig.name, result, args));
        }
        self.scope_id = parent_scope_id;
    }

    /// Resolves an occurance of a function call.
    fn resolve_call(self: &mut Self, item: &mut ast::Call<'a>) {

        // locate function definition
        if item.function_id.is_none() {
            item.function_id = self.scopes.lookup_function_id(self.scope_id, item.path.0[0]);      // fixme: need full path here
        }

        // found a function, resolve return type and arguments
        if let Some(function_id) = item.function_id {

            // return value
            let function_types = self.scopes.function_type(function_id).clone();
            if function_types.ret_type.is_type() {
                self.set_type_from_id(&mut item.type_id, function_types.ret_type.unwrap());
            } else {
                self.set_type_from_void(&mut item.type_id);
            }

            // arguments
            for (index, &arg_type) in function_types.arg_type.iter().enumerate() {
                self.resolve_expression(&mut item.args[index], Some(arg_type));
                if arg_type != item.args[index].get_type_id() {
                    panic!("type mismatch: {:#?}", item);
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
                self.resolve_expression(expr, lhs);
                Some(expr.get_type_id())
            },
            None => None,
        };
        // have explicit type and a resolved type for right hand side, check that they match
        if let (Some(TypeSlot::TypeId(lhs)), Some(TypeSlot::TypeId(rhs))) = (lhs, rhs) {
            if lhs != rhs && !self.primitives.is_valid_cast(rhs, lhs) {
                panic!("invalid cast from {:?} to {:?}", lhs, rhs); // TODO: error handling
            }
        }
        // have explicit type and/or resolved expression
        if let Some(TypeSlot::TypeId(lhs)) = lhs {
            self.set_type_from_id(&mut item.type_id, lhs);
            self.set_bindingtype_from_id(binding_id, lhs);
        } else if let Some(TypeSlot::TypeId(rhs)) = rhs {
            self.set_type_from_id(&mut item.type_id, rhs);
            self.set_bindingtype_from_id(binding_id, rhs);
        } else if let TypeSlot::TypeId(binding_type_id) = self.scopes.binding_type(binding_id) {
            // someone else set the binding type, accept it
            self.set_type_from_id(&mut item.type_id, binding_type_id);
        }
    }

    /// Resolves an occurance of a variable.
    fn resolve_variable(self: &Self, item: &mut ast::Variable<'a>) {
        // resolve binding
        if item.binding_id.is_none() {
            item.binding_id = self.scopes.lookup_binding_id(self.scope_id, item.path.0[0]);      // fixme: need full path here
            if item.binding_id.is_none() {
                panic!("unknown binding {:?} in scope {:?}", item.path.0, self.scope_id); // todo: error handling
            }
        }
        // try to resolve type from binding
        if let Some(binding_id) = item.binding_id {
            if let TypeSlot::TypeId(binding_type_id) = self.scopes.binding_type(binding_id) {
                self.set_type_from_id(&mut item.type_id, binding_type_id);
            }
        }
    }

    /// Resolves an if block.
    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock<'a>) {
        self.resolve_expression(&mut item.cond, None);
        self.resolve_block(&mut item.if_block);
        if let Some(ref mut else_block) = item.else_block {
            self.resolve_block(else_block);
            if item.if_block.type_id != else_block.type_id
                && item.if_block.type_id.is_type() && else_block.type_id.is_type()
            { // TODO: used complete here
                println!("{:#?}", else_block);
                panic!("if/else return type mismatch {:?} vs {:?}", item.if_block.type_id, else_block.type_id); // TODO: error handling, attempt cast
            }
        }
    }

    /// Resolves a for loop.
    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop<'a>) {
        // create binding for iterator var
        let binding_id = self.try_create_binding(&mut item.iter);
        // resolve the range type
        self.resolve_expression(&mut item.range, None);
        // set binding type to range type
        if let TypeSlot::TypeId(range) = item.range.get_type_id() {
            self.set_type_from_id(&mut item.iter.type_id, range);
            self.set_bindingtype_from_id(binding_id, range);
        }
        // handle block
        self.resolve_block(&mut item.block);
    }

    /// Resolves a while loop.
    fn resolve_while_loop(self: &mut Self, item: &mut ast::WhileLoop<'a>) {
        self.resolve_expression(&mut item.expr, None);
        self.resolve_block(&mut item.block);
    }

    /// Resolves a block.
    fn resolve_block(self: &mut Self, item: &mut ast::Block<'a>) {
        for mut statement in item.statements.iter_mut() {
            self.resolve_statement(&mut statement);
        }
        if let Some(ref mut result) = item.result {
            self.resolve_expression(result, None);
            if let TypeSlot::TypeId(type_id) = result.get_type_id() {
                self.set_type_from_id(&mut item.type_id, type_id);
            }
        } else {
            self.set_type_from_void(&mut item.type_id);
        }
    }

    /// Resolves a return statement.
    fn resolve_return(self: &mut Self, item: &mut ast::Return<'a>) {
        if let Some(expr) = &mut item.expr {
            self.resolve_expression(expr, None);
        }
    }

    /// Resolves an assignment expression.
    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment<'a>) {
        self.resolve_variable(&mut item.left);
        self.resolve_expression(&mut item.right, Some(item.left.type_id));
        if item.left.binding_id.is_some() && item.left.type_id.is_unresolved() && item.right.get_type_id().is_type() {
            //self.set_type_from_id(&mut item.left.type_id, item.right.get_type_id().unwrap());
            self.set_bindingtype_from_id(item.left.binding_id.unwrap(), item.right.get_type_id().unwrap());
        }
    }

    /*
    /// Try to set a concrete type for a literal without explicit type annotation.
    fn try_pin_literal(self: &Self, item: &mut ast::Literal<'a>, other: &TypeSlot) -> TypeSlot {
        if item.ty.is_none() && item.type_id.is_unresolved() && other.is_type() {
            self.set_type_from_id(&mut item.type_id, other.unwrap());
        }
        item.type_id
    }
    */

    /// Resolves a binary operation.
    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp<'a>) {

        use crate::frontend::ast::BinaryOperator as O;
        use crate::frontend::ast::Expression as E;

        self.resolve_expression(&mut item.left, Some(item.right.get_type_id()));
        self.resolve_expression(&mut item.right, Some(item.left.get_type_id()));

        // try pinning literals to concrete types
        /*if let E::Literal(literal) = &mut item.left {
            self.try_pin_literal(literal, &item.right.get_type_id());
        }
        if let E::Literal(literal) = &mut item.right {
            self.try_pin_literal(literal, &item.left.get_type_id());
        }*/

        match item.op {
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual | O::And | O::Or => {
                self.set_type_from_id(&mut item.type_id, self.primitives.bool);
            },
            O::Add | O::Sub | O::Mul | O::Div | O::Rem | O::Range => {
                if let (E::Literal(literal_a), E::Literal(literal_b)) = (&item.left, &item.right) {
                    // both sides are literals, determine type that suits both
                    if let Some(type_id) = self.primitives.cast_literal(literal_a, literal_b) {
                        self.set_type_from_id(&mut item.type_id, type_id);
                    } else {
                        panic!("cannot cast between {:?} and {:?}", literal_a, literal_b);
                    }
                } else if let (TypeSlot::TypeId(left_type_id), TypeSlot::TypeId(right_type_id)) = (item.left.get_type_id(), item.right.get_type_id()) {
                    // cast to common type or try to widen type
                    if let Some(type_id) = self.primitives.cast(left_type_id, right_type_id) {
                        self.set_type_from_id(&mut item.type_id, type_id);
                    } else {
                        panic!("cannot cast between {:?} and {:?}", left_type_id, right_type_id);
                    }
                }
            },
            _ => { }, // fixme: remove and add missing
        }
        // todo: implement
    }

    /// Resolves a unary operation.
    fn resolve_unary_op(self: &mut Self, item: &mut ast::UnaryOp<'a>) {
        use crate::frontend::ast::UnaryOperator as O;
        self.resolve_expression(&mut item.exp, None);
        match item.op {
            O::Not => {
                let type_id = self.primitives.bool; // todo: nll fixes this
                self.set_type_from_id(&mut item.type_id, type_id);
            },
            _ => { } // fixme: remove and add missing
        }
        // todo: implement
    }

    /// Resolves a literal type if it is annotated, otherwise let the parent expression pick a concrete type.
    fn resolve_literal(self: &Self, item: &mut ast::Literal<'a>, type_hint: Option<TypeSlot>) {
        if let Some(ty) = &mut item.ty {
            // literal has explicit type, use it
            self.resolve_type(ty);
            self.set_type_from_id(&mut item.type_id, ty.type_id.unwrap());
        } else if let Some(type_hint) = type_hint {
            // we got a type hint, try to match it
            if let TypeSlot::TypeId(type_id) = type_hint {
                self.set_type_from_id(&mut item.type_id, type_id);
            }
        }
        /*
        match item.value {
            L::Integer(ref v) => {
                let type_id = self.primitives.integer_type_id(*v).unwrap();
                self.set_type_from_id(&mut item.type_id, type_id);
            },
            L::Float(_) => {
                // todo: pick correct float type
                let type_id = self.primitives.float[0];
                self.set_type_from_id(&mut item.type_id, type_id);
            },
            L::String(_) => {
                // todo: implement
            },
        };
        */
    }
}