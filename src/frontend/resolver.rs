//! AST type checker and resolver.

mod scopes;

use std::fmt::{self, Display};
use std::marker::PhantomData;
use crate::frontend::ast::{self, Bindable, Positioned, Returns, CallType};
use crate::util::{ScopeId, TypeId, BindingId, FunctionId, Type, Array, Struct, Numeric, FnKind, Intrinsic, compute_loc};
use crate::runtime::VMFunc;

/// Parsed program AST with all types, bindings and other language structures resolved.
#[derive(Debug)]
pub struct ResolvedProgram<'ast, T> where T: VMFunc<T> {
    /// Programs are generic over their Rust API
    ty: PhantomData<T>,
    /// Program AST with resolved `BindingId`s.
    pub ast: super::ParsedProgram<'ast>,
    /// Mapping from `BindingId` (vector index) to `TypeId`.
    pub bindingtype_ids: Vec<TypeId>,
    /// Mapping from `TypeId` (vector index) to primitive type.
    pub types: Vec<Type>,
    /// `FunctionId` of the entry/main function.
    pub entry_fn: FunctionId,
}

/// Represents the various possible resolver error-kinds.
#[derive(Clone, Debug)]
pub enum ResolveErrorKind {
    TypeMismatch(Type, Type),
    NonPrimitiveCast(Type),
    IncompatibleNumeric(Type, Numeric),
    UnknownValue(String),
    NumberOfArguments(u32, u32),
}

/// A type resolution error.
#[derive(Clone, Debug)]
pub struct ResolveError {
    pub kind: ResolveErrorKind,
    position: u32, // this is the position from the end of the input
}

impl ResolveError {
    fn new(item: &impl Positioned, kind: ResolveErrorKind) -> ResolveError {
        Self { kind: kind, position: item.position() }
    }
    /// Computes and returns the source code location of this error. Since the AST only stores byte
    /// offsets, the original source is required to recover line and column information.
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        compute_loc(input, input.len() as u32 - self.position)
    }
}

impl Display for ResolveError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ResolveErrorKind::TypeMismatch(t1, t2) => write!(f, "Incompatible types {:?} and {:?}", t1, t2),
            ResolveErrorKind::IncompatibleNumeric(t, n) => write!(f, "Incompatible numeric {:?} for expected type {:?}", n, t),
            ResolveErrorKind::NumberOfArguments(e, g) => write!(f, "Expected {} arguments, got {}", e, g),
            // Todo: handle the others
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

type ResolveResult = Result<(), ResolveError>;

/// Temporary internal state during program type/binding resolution.
struct Resolver<'ctx> {
    /// Scope id this state operates in.
    scope_id        : ScopeId,
    /// Repository of all scopes.
    scopes          : &'ctx mut scopes::Scopes,
    /// Grouped primitive types.
    primitives      : &'ctx Primitives,
    /// Set to true once resolution cannot proceed any further, causes numeric literals to be set to their default types.
    infer_literals  : bool,
}

/// Utility structure to handle primitive type information and casting.
struct Primitives {
    /// Void type
    pub void    : TypeId,
    /// Boolean type
    pub bool    : TypeId,
    /// Unsigned types ordered by bit count.
    pub unsigned: [ TypeId; 4 ],
    /// Signed types ordered by bit count.
    pub signed  : [ TypeId; 4 ],
    /// Floating point types ordered by bit count.
    pub float   : [ TypeId; 2 ],
    /// String type
    pub string  : TypeId,
}

/// Resolves types within the given program AST structure.
#[allow(invalid_type_param_default)]
pub fn resolve<'ast, T>(mut program: super::ParsedProgram<'ast>, entry: &str) -> Result<ResolvedProgram<'ast, T>, ResolveError> where T: VMFunc<T> {

    // create root scope and insert primitives
    let mut scopes = scopes::Scopes::new();
    let root_scope_id = scopes::Scopes::root_id();

    let primitives = Primitives {
        void: scopes.insert_type(root_scope_id, None, Type::void),
        bool: scopes.insert_type(root_scope_id, Some("bool"), Type::bool),
        unsigned: [
            scopes.insert_type(root_scope_id, Some("u8"), Type::u8),
            scopes.insert_type(root_scope_id, Some("u16"), Type::u16),
            scopes.insert_type(root_scope_id, Some("u32"), Type::u32),
            scopes.insert_type(root_scope_id, Some("u64"), Type::u64),
        ],
        signed: [
            scopes.insert_type(root_scope_id, Some("i8"), Type::i8),
            scopes.insert_type(root_scope_id, Some("i16"), Type::i16),
            scopes.insert_type(root_scope_id, Some("i32"), Type::i32),
            scopes.insert_type(root_scope_id, Some("i64"), Type::i64),
        ],
        float: [
            scopes.insert_type(root_scope_id, Some("f32"), Type::f32),
            scopes.insert_type(root_scope_id, Some("f64"), Type::f64),
        ],
        string: scopes.insert_type(root_scope_id, Some("String"), Type::String),
    };

    // insert intrinsics // todo tie methods to their struct type
    scopes.insert_intrinsic(root_scope_id, "len", Intrinsic::ArrayLen, primitives.unsigned[2], Vec::new());

    // insert rust functions into root scope
    for (name, info) in T::call_info().iter() {
        let (index, ret_type_name, arg_type_names) = info;
        let ret_type = if *ret_type_name == "" {
            scopes.void_type()
        } else {
            scopes.type_id(root_scope_id, ret_type_name).expect("Unknown return type encountered")
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

    let mut now_unresolved = (0, 0);
    let mut prev_unresolved;
    let mut infer_literals = false;

    loop {
        for mut statement in program.0.iter_mut() {
            let mut resolver = Resolver {
                scope_id        : root_scope_id,
                scopes          : &mut scopes,
                primitives      : &primitives,
                infer_literals  : infer_literals,
            };
            resolver.resolve_statement(&mut statement)?; // FIXME: want a vec of errors here
        }

        prev_unresolved = now_unresolved;
        now_unresolved = scopes.state();

        if now_unresolved.0 == 0 || (now_unresolved == prev_unresolved && infer_literals) {
            break;
        }

        infer_literals = now_unresolved == prev_unresolved && !infer_literals;
    }

    let entry_fn = scopes.lookup_function_id(root_scope_id, entry).expect("Failed to resolve entry function");
    let (bindingtype_ids, types) = scopes.into();

    Ok(ResolvedProgram {
        ty              : PhantomData,
        ast             : program,
        entry_fn        : entry_fn,
        types           : types,
        bindingtype_ids : bindingtype_ids,
    })
}

/// Utility methods to update a typeslot with a resolved type and increase the resolution counter.
impl<'ast, 'ctx> Resolver<'ctx> where 'ast: 'ctx {

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
    fn try_create_anon_binding(self: &mut Self, item: &mut impl Bindable) -> BindingId {
        if let Some(binding_id) = item.binding_id() {
            binding_id
        } else {
            let binding_id = self.scopes.insert_binding(self.scope_id, None, None);
            *item.binding_id_mut() = Some(binding_id);
            binding_id
        }
    }

    /// Sets given TypeId for the given binding, generating a BindingId if required.
    fn binding_set_type_id(self: &mut Self, item: &mut (impl Bindable+Positioned), new_type_id: TypeId) -> Result<(), ResolveError>  {
        let binding_id = self.try_create_anon_binding(item);
        let type_id = self.scopes.binding_type_id_mut(binding_id);
        if type_id.is_none() {
            *type_id = Some(new_type_id);
        } else {
            let type_id = *type_id; // todo: directly deref in args if bck ever figures this one out
            self.check_types_match(item, type_id, Some(new_type_id))?;
        }
        Ok(())
    }

    /// Returns TypeId for the given binding.
    fn binding_type_id(self: &Self, item: &impl Bindable) -> Option<TypeId> {
        item.binding_id().and_then(|binding_id| self.scopes.binding_type_id(binding_id))
    }

    /// Returns a reference to the type of the given binding.
    fn binding_type(self: &mut Self, item: &impl Bindable) -> Option<&Type> {
        item.binding_id()
            .and_then(|binding_id| self.scopes.binding_type_id(binding_id))
            .map(move |type_id| self.scopes.type_ref(type_id))
    }

    /// Returns TypeId of a type suitable to represent the given numeric. Will only consider i32, i64 and f32.
    fn classify_numeric(self: &Self, value: Numeric) -> Option<TypeId> {
        if value.is_integer() {
            if Type::i32.is_compatible_numeric(value) {
                Some(self.primitives.signed[2])
            } else if Type::i64.is_compatible_numeric(value) {
                Some(self.primitives.signed[3])
            } else {
                None
            }
        } else if value.is_float() {
            Some(self.primitives.float[1])
        } else {
            None
        }
    }

    /// Returns whether the given type_ids refer to matching or fully compatible types.
    fn types_match(self: &Self, type_id_a: Option<TypeId>, type_id_b: Option<TypeId>) -> bool {
        if type_id_a == type_id_b {
            true
        } else {
            let type_a = self.scopes.type_ref(type_id_a.unwrap());
            let type_b = self.scopes.type_ref(type_id_b.unwrap());
            match (type_a, type_b) {
                (&Type::Array(Array { len: a_len, type_id: Some(a_type_id) }), &Type::Array(Array { len: b_len, type_id: Some(b_type_id) })) => {
                    a_len == b_len && self.types_match(Some(a_type_id), Some(b_type_id))
                }
                _ => false,
            }
        }
    }

    /// Returns Ok if the given type_ids refer to matching or fully compatible types. Otherwise a TypeMismatch error.
    fn check_types_match(self: &Self, item: &impl Positioned, type_id_a: Option<TypeId>, type_id_b: Option<TypeId>) -> ResolveResult  {
        if !self.types_match(type_id_a, type_id_b) {
            let error_kind = ResolveErrorKind::TypeMismatch(self.scopes.type_ref(type_id_a.unwrap()).clone(), self.scopes.type_ref(type_id_b.unwrap()).clone());
            Err(ResolveError::new(item, error_kind))
        } else {
            Ok(())
        }
    }
}

/// Methods to resolve individual AST structures.
impl<'ast, 'ctx> Resolver<'ctx> where 'ast: 'ctx {

    /// Resolves types and bindings used in a statement.
    fn resolve_statement(self: &mut Self, item: &mut ast::Statement<'ast>) -> ResolveResult  {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.resolve_function(function),
            S::Structure(structure)     => self.resolve_structure(structure),
            S::Binding(binding)         => self.resolve_binding(binding),
            S::IfBlock(if_block)        => self.resolve_if_block(if_block, None), // accept any type for these, result is discarded
            S::ForLoop(for_loop)        => self.resolve_for_loop(for_loop),
            S::WhileLoop(while_loop)    => self.resolve_while_loop(while_loop),
            S::Block(block)             => self.resolve_block(block, None),
            S::Return(ret)              => self.resolve_return(ret),
            S::Expression(expression)   => self.resolve_expression(expression, None),
        }
    }

    /// Resolves types and bindings used in an expression.
    fn resolve_expression(self: &mut Self, item: &mut ast::Expression<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        use self::ast::Expression as E;
        match item { // todo: these all need to check expected_result since the caller might depend on an error result on type mismatch
            E::Literal(literal)         => self.resolve_literal(literal, expected_result),
            E::Variable(variable)       => self.resolve_variable(variable, expected_result),
            E::Call(call)               => self.resolve_call(call, expected_result),
            E::Member(_)                => { Ok(()) /* nothing to do here */ },
            E::Assignment(assignment)   => self.resolve_assignment(assignment),
            E::BinaryOp(binary_op)      => self.resolve_binary_op(binary_op, expected_result),
            E::UnaryOp(unary_op)        => self.resolve_unary_op(unary_op, expected_result),
            E::Cast(cast)               => self.resolve_cast(cast, expected_result),
            E::Block(block)             => self.resolve_block(block, expected_result),
            E::IfBlock(if_block)        => self.resolve_if_block(if_block, expected_result),
        }
    }

    // Resolves an inline type definition.
    fn resolve_inline_type(self: &mut Self, item: &mut ast::InlineType<'ast>) -> Result<Option<TypeId>, ResolveError> {
        use ast::InlineType as IT;
        match item {
            IT::TypeName(type_name) => self.resolve_type(type_name, None), // todo: not sure about this one. inline-type is usually defining, so if it differs, the other side should be wrong
            IT::Array(array) => self.resolve_array(array),
        }
    }

    /// Resolves an array definition
    fn resolve_array(self: &mut Self, item: &mut ast::Array<'ast>) -> Result<Option<TypeId>, ResolveError> {
        let inner_type_id = self.resolve_inline_type(&mut item.element_type)?;
        if item.type_id.is_none() {
            let ty = Type::Array(Array {
                len     : Some(item.len),
                type_id : inner_type_id,
            });
            item.type_id = Some(self.scopes.insert_type(self.scope_id, None, ty));
        }
        Ok(item.type_id)
    }

    /// Resolves a struct definition.
    fn resolve_structure(self: &mut Self, item: &mut ast::Struct<'ast>) -> ResolveResult {
        for (_, field) in &mut item.fields {
            self.resolve_inline_type(field)?;
        }
        if item.type_id.is_none() {
            let mut fields = Vec::new();
            for (_, (field_name, field_type)) in item.fields.iter().enumerate() {
                fields.push((field_name.to_string(), field_type.type_id()));
            }
            let ty = Type::Struct(Struct { fields });
            item.type_id = Some(self.scopes.insert_type(self.scope_id, Some(item.ident.name), ty));
        }
        Ok(())
    }

    /// Resolves a function signature.
    fn resolve_signature(self: &mut Self, item: &mut ast::Signature<'ast>) -> ResolveResult {
        // resolve arguments
        for arg in item.args.iter_mut() {
            self.resolve_binding(arg)?;
        }
        // resolve return type
        if let Some(ret) = &mut item.ret {
            self.resolve_inline_type(ret)?;
        }
        Ok(())
    }

    /// Resolves a function defintion.
    fn resolve_function(self: &mut Self, item: &mut ast::Function<'ast>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        self.resolve_signature(&mut item.sig)?;
        //let signature_scope_id = self.try_create_scope(&mut item.scope_id); // todo: put body into separate scope so signature can't access body
        if item.function_id.is_none() && item.sig.ret_resolved() && item.sig.args_resolved() {
            let result_type_id = item.sig.ret_type_id();
            let arg_type_ids: Vec<_> = item.sig.arg_type_ids().iter().map(|arg| arg.unwrap()).collect();
            let function_id = self.scopes.insert_function(parent_scope_id, item.sig.ident.name, result_type_id, arg_type_ids);
            item.function_id = Some(function_id);
            self.scopes.set_scopefunction_id(self.scope_id, function_id);
        }
        if let Some(function_id) = item.function_id {
            let ret_type = self.scopes.function_type(function_id).ret_type;
            self.resolve_block(&mut item.block, ret_type)?;
        }
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a return statement.
    fn resolve_return(self: &mut Self, item: &mut ast::Return<'ast>) -> ResolveResult {
        let function_id = self.scopes.lookup_scopefunction_id(self.scope_id).expect("Encountered return outside of function");
        let ret_type_id = self.scopes.function_type(function_id).ret_type;
        if let Some(expr) = &mut item.expr {
            item.fn_ret_type_id = ret_type_id;
            self.resolve_expression(expr, ret_type_id)?;
            // check return type matches function result type // todo: would be unnecessary if resolve_expression would always check expected_type
            let expression_type_id = self.binding_type_id(expr);
            if expression_type_id.is_some() {
                self.check_types_match(item, ret_type_id, expression_type_id)?;
            }
        } else if ret_type_id != Some(TypeId::void()) {
            // no return expression, function result type must be void
            self.check_types_match(item, Some(TypeId::void()), ret_type_id)?; // Todo: meh, using check_types_match to generate an error when we already know there is an error.
        }
        Ok(())
    }

    /// Resolves an occurance of a function call.
    fn resolve_call(self: &mut Self, item: &mut ast::Call<'ast>, expected_result: Option<TypeId>) -> ResolveResult {

        // locate function definition
        if item.function_id.is_none() {
            item.function_id = self.scopes.lookup_function_id(self.scope_id, item.ident.name);
        }

        // found a function, resolve return type and arguments
        if let Some(function_id) = item.function_id {

            // return value
            let function_types = self.scopes.function_type(function_id).clone();
            if let Some(ret_type_id) = function_types.ret_type { // fixme: probably should be Some(Void) instead
                self.binding_set_type_id(item, ret_type_id)?;
            } else {
                self.binding_set_type_id(item, TypeId::void())?;
            }

            if expected_result.is_some() {
                let actual_result = self.binding_type_id(item);
                if actual_result.is_some() {
                    self.check_types_match(item, expected_result, actual_result)?;
                }
            }

            // argument count
            if function_types.arg_type.len() != item.args.len() {
                return Err(ResolveError::new(item, ResolveErrorKind::NumberOfArguments(function_types.arg_type.len() as u32, item.args.len() as u32)));
            }

            // arguments
            for (index, &expected_type) in function_types.arg_type.iter().enumerate() {
                self.resolve_expression(&mut item.args[index], Some(expected_type))?;
                let actual_type = self.binding_type_id(&item.args[index]);
                // infer arguments
                if actual_type.is_none() {
                    self.binding_set_type_id(&mut item.args[index], expected_type)?;
                } else if actual_type.is_some() {
                    self.check_types_match(&item.args[index], Some(expected_type), actual_type)?;
                }
            }

            // rustcall?
            if let Some(rust_fn_index) = function_types.rust_fn_index() {
                item.call_kind = FnKind::Rust(rust_fn_index);
            }

            // is this a method? resolve self argument
            if let CallType::Method(self_arg) = &mut item.call_type {
                self.resolve_expression(self_arg, None)?;
                if let Some(ty) = self.binding_type(&**self_arg) { // todo: intrinsics needs a more generic approach. can't implement them all individually here
                    if ty.is_array() && item.ident.name == "len" { // need to be in scope like rust_fns
                        item.call_kind = FnKind::Intrinsic(Intrinsic::ArrayLen);
                    }
                }
            }
        }

        Ok(())
    }

    /// Resolves a type (name) to a type_id.
    fn resolve_type(self: &Self, item: &mut ast::TypeName<'ast>, expected_result: Option<TypeId>) -> Result<Option<TypeId>, ResolveError> {
        if item.type_id.is_none() {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &item.path.name[0]) { // fixme: handle path segments
                item.type_id = Some(new_type_id);
            }
        }
        if item.type_id.is_some() && expected_result.is_some() {
            self.check_types_match(item, expected_result, item.type_id)?;
        }
        Ok(item.type_id)
    }

    /// Resolves an occurance of a variable.
    fn resolve_variable(self: &mut Self, item: &mut ast::Variable<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        // resolve binding
        if item.binding_id.is_none() {
            item.binding_id = self.scopes.lookup_binding_id(self.scope_id, item.ident.name);
            if item.binding_id.is_none() {
                return Err(ResolveError::new(item, ResolveErrorKind::UnknownValue(item.ident.name.to_string())));
            }
        }
        // set expected type, if any
        if let Some(expected_result) = expected_result {
            self.binding_set_type_id(item, expected_result)?;
        }
        Ok(())
    }

    /// Resolves an if block.
    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        // resolve condition and block
        self.resolve_expression(&mut item.cond, Some(self.primitives.bool))?;
        self.resolve_block(&mut item.if_block, expected_result)?;
        // optionally resolve else block
        if let Some(else_block) = &mut item.else_block {
            self.resolve_block(else_block, expected_result)?;
            let if_type_id = self.binding_type_id(&item.if_block);
            let else_type_id = self.binding_type_id(else_block);
            if if_type_id.is_some() && else_type_id.is_some() {
                self.check_types_match(item, if_type_id, else_type_id)?;
            }
        } else if let Some(if_type_id) = self.binding_type_id(&item.if_block) {
            // if block with a non-void result but no else block
            self.check_types_match(item, Some(if_type_id), Some(TypeId::void()))?; // Todo: meh, using check_types_match to generate an error when we already know there is an error.
        }
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a for loop.
    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop<'ast>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        // create binding for the iterator variable
        self.resolve_binding(&mut item.iter)?;
        // resolve the range type using the type of the iterator variable, if possible, then apply range type back to iter
        let type_id = self.binding_type_id(&item.iter);
        self.resolve_expression(&mut item.range, type_id)?;
        if let Some(type_id) = self.binding_type_id(&item.range) {
            self.binding_set_type_id(&mut item.iter, type_id)?;
        }
        // handle block
        self.resolve_block(&mut item.block, None)?;
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a while loop.
    fn resolve_while_loop(self: &mut Self, item: &mut ast::WhileLoop<'ast>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        self.resolve_expression(&mut item.expr, None)?;
        self.resolve_block(&mut item.block, None)?;
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a block.
    fn resolve_block(self: &mut Self, item: &mut ast::Block<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);

        // check for unconditional returns, code below those is unreachable, remove
        let mut return_index = None;
        for (index, statement) in item.statements.iter_mut().enumerate() {
            if statement.returns() {
                return_index = Some(index);
                break;
            }
        }
        // remove code after return, move return to returns
        if let Some(return_index) = return_index {
            item.statements.truncate(return_index + 1);
            let returns = item.statements.pop().unwrap();
            item.returns = Some(returns.into_expression().unwrap());
            item.result = None;
        }
        // check if the result returns, if so move to returns
        if item.result.as_ref().map_or(false, |r| r.returns()) {
            item.returns = item.result.take();
        }

        // resolve statments, result and returns
        for statement in item.statements.iter_mut() {
            self.resolve_statement(statement)?;
        }
        if let Some(ref mut result) = item.result {
            self.resolve_expression(result, expected_result)?;
        }
        if let Some(ref mut returns) = item.returns {
            let function_id = self.scopes.lookup_scopefunction_id(self.scope_id).expect("Encountered return outside of function");
            let ret_type_id = self.scopes.function_type(function_id).ret_type;
            self.resolve_expression(returns, ret_type_id)?;
        }

        // check result type matches expected type unless block is returned from before ever resulting
        if item.returns.is_none() && expected_result.is_some() {
            if item.result.is_none() { // no result = void
                self.check_types_match(item, expected_result, Some(self.primitives.void))?;
            } else if let Some(result_expression) = &item.result {
                let result_type_id = self.binding_type_id(result_expression);
                if result_type_id.is_some() {
                    self.check_types_match(item, expected_result, result_type_id)?;
                }
            }
        }
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves an assignment expression.
    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment<'ast>) -> ResolveResult {
        let right_type_id = self.binding_type_id(&item.right);
        self.resolve_expression(&mut item.left, right_type_id)?;
        let left_type_id = self.binding_type_id(&item.left);
        self.resolve_expression(&mut item.right, left_type_id)?;
        self.binding_set_type_id(item, TypeId::void())?;
        Ok(())
    }

    /// Resolves an assignment expression.
    fn resolve_cast(self: &mut Self, item: &mut ast::Cast<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        self.resolve_type(&mut item.ty, expected_result)?;
        self.resolve_expression(&mut item.expr, None)?;
        if let Some(type_id) = item.ty.type_id {
            self.binding_set_type_id(item, type_id)?;
            let ty = self.scopes.type_ref(type_id);
            if !ty.is_primitive() {
                return Err(ResolveError::new(item, ResolveErrorKind::NonPrimitiveCast(ty.clone())));
            }
        }
        if let Some(ty) = self.binding_type(&mut item.expr) {
            if !ty.is_primitive() {
                return Err(ResolveError::new(item, ResolveErrorKind::NonPrimitiveCast(ty.clone())));
            }
        }
        Ok(())
    }

    /// Resolves a binary operation.
    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::BinaryOperator as O;

        match item.op {
            O::And | O::Or => {
                self.resolve_expression(&mut item.left, Some(self.primitives.bool))?;
                self.resolve_expression(&mut item.right, Some(self.primitives.bool))?;
                self.binding_set_type_id(item, self.primitives.bool)?;
            }
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual => {
                self.resolve_expression(&mut item.left, None)?;
                let left_type_id = self.binding_type_id(&item.left);
                self.resolve_expression(&mut item.right, left_type_id)?;
                let right_type_id = self.binding_type_id(&item.right);
                self.binding_set_type_id(item, self.primitives.bool)?;
                if let Some(common_type_id) = left_type_id.or(right_type_id) {
                    self.binding_set_type_id(&mut item.left, common_type_id)?;
                    self.binding_set_type_id(&mut item.right, common_type_id)?;
                }
            }
            O::Add | O::Sub | O::Mul | O::Div | O::Rem | O::Range | O::RangeInclusive => {
                self.resolve_expression(&mut item.left, expected_result)?;
                let left_type_id = self.binding_type_id(&item.left);
                self.resolve_expression(&mut item.right, left_type_id)?;
                let type_id = self.binding_type_id(item);
                let right_type_id = self.binding_type_id(&item.right);
                if let Some(common_type_id) = type_id.or(left_type_id).or(right_type_id) {
                    self.binding_set_type_id(item, common_type_id)?;
                    self.binding_set_type_id(&mut item.left, common_type_id)?;
                    self.binding_set_type_id(&mut item.right, common_type_id)?;
                }
            }
            O::Index | O::IndexWrite => {
                self.resolve_expression(&mut item.left, None)?;
                self.resolve_expression(&mut item.right, Some(self.primitives.unsigned[2]))?; // u32
                // left[right] : item
                self.binding_set_type_id(&mut item.right, self.primitives.unsigned[2])?; // u32
                self.try_create_anon_binding(item);
                // if we know the result type, set the array element type to that
                if let Some(result_type_id) = self.binding_type_id(item) {
                    // get mutable reference to type
                    let ty = item.left.binding_id()
                        .and_then(|binding_id| self.scopes.binding_type_id(binding_id))
                        .map(|type_id| self.scopes.type_mut(type_id));

                    if let Some(Type::Array(array)) = ty {
                        array.type_id = Some(result_type_id); // todo: check that array.type_id is either None or equals binding_type_id
                    }
                }
                // if we know the array element type, set the result type to that
                if let Some(&Type::Array(Array { type_id: Some(element_type_id), .. })) = self.binding_type(&item.left) {
                    self.binding_set_type_id(item, element_type_id)?;
                }
            }
            O::Access | O::AccessWrite => {
                self.resolve_expression(&mut item.left, None)?;
                self.resolve_expression(&mut item.right, None)?;
                // left.right : item
                if let Some(ty) = self.binding_type(&item.left) {
                    let struct_ = ty.as_struct().expect("Member access on a non-struct");
                    let field = item.right.as_member_mut().expect("Internal error: Member access using a non-field");
                    if field.index.is_none() {
                        field.index = Some(struct_.fields.iter().position(|f| f.0 == field.ident.name).expect("Unknown struct member") as u32);
                    }
                    if let Some(type_id) = struct_.fields[field.index.unwrap() as usize].1 {
                        self.binding_set_type_id(item, type_id)?;
                        self.binding_set_type_id(&mut item.right, type_id)?;
                    }
                }
            }
            O::Assign | O::AddAssign | O::SubAssign | O::MulAssign | O::DivAssign | O::RemAssign => {
                panic!("this operator should not be handled here");
            }
        }

        Ok(())
    }

    /// Resolves a binding created by let, for or a signature.
    fn resolve_binding(self: &mut Self, item: &mut ast::Binding<'ast>) -> ResolveResult {

        // check if a type is specified
        let explicit = match item.ty {
            Some(ref mut ty) => {
                self.resolve_inline_type(ty)?
            },
            None => None,
        };

        // create binding id if we don't have one yet
        if item.binding_id().is_none() {
            // this binding ast node wasn't processed yet. if the binding name already exists we're shadowing - which is NYI
            if self.scopes.binding_id(self.scope_id, item.ident.name).is_some() {
                unimplemented!("cannot shadow {}", item.ident.name); // todo: support shadowing
            }
            let binding_id = self.scopes.insert_binding(self.scope_id, Some(item.ident.name), None);
            *item.binding_id_mut() = Some(binding_id);
        }

        // apply explicit type if we got one
        if let Some(explicit) = explicit {
            self.binding_set_type_id(item, explicit)?;
        }

        // resolve right hand side
        if let Some(expr) = &mut item.expr {
            self.resolve_expression(expr, explicit)?;
        }

        // if we have a binding type, apply it to the expression
        let lhs = self.binding_type_id(item);

        if let (Some(lhs), Some(expr)) = (lhs, &mut item.expr) {
            self.binding_set_type_id(expr, lhs)?;
        }

        // if the expression has a type, apply it back to the binding
        if let Some(expr) = &mut item.expr {
            let expr_type_id = self.binding_type_id(expr);
            if let Some(expr_type_id) = expr_type_id {
                self.binding_set_type_id(item, expr_type_id)?;
            }
        };

        Ok(())
    }

    /// Resolves a unary operation.
    fn resolve_unary_op(self: &mut Self, item: &mut ast::UnaryOp<'ast>, expected_type: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::UnaryOperator as UO;
        self.resolve_expression(&mut item.expr, expected_type)?;
        match item.op {
            UO::Not => {
                if expected_type.is_some() {
                    self.check_types_match(item, expected_type, Some(self.primitives.bool))?;
                }
                self.binding_set_type_id(item, self.primitives.bool)?;
            },
            UO::IncBefore | UO::DecBefore | UO::IncAfter | UO::DecAfter => {
                if let Some(type_id) = self.binding_type_id(&item.expr) {
                    self.binding_set_type_id(item, type_id)?;
                }
            },
        }
        Ok(())
    }

    /// Resolves a literal type if it is annotated, otherwise let the parent expression pick a concrete type.
    fn resolve_literal(self: &mut Self, item: &mut ast::Literal<'ast>, expected_type: Option<TypeId>) -> ResolveResult {
        use self::ast::LiteralValue as LV;

        if let LV::Bool(_) = item.value { // todo: all of these need to check expected type if any
            if expected_type.is_some() {
                self.check_types_match(item, expected_type, Some(self.primitives.bool))?;
            }
            self.binding_set_type_id(item, self.primitives.bool)?;
        } else if let LV::String(_) = item.value {
            if expected_type.is_some() {
                self.check_types_match(item, expected_type, Some(self.primitives.string))?;
            }
            self.binding_set_type_id(item, self.primitives.string)?;
        } else if let LV::Array(_) = item.value {
            self.resolve_array_literal(item, expected_type)?;
        } else if let LV::Struct(_) = item.value {
            self.resolve_struct_literal(item)?;
        } else if let Some(type_name) = &mut item.type_name {
            // literal has explicit type, use it
            if let Some(explicit_type_id) = self.resolve_type(type_name, expected_type)? {
                self.binding_set_type_id(item, explicit_type_id)?;
                if expected_type.is_some() {
                    self.check_types_match(item, expected_type, Some(explicit_type_id))?;
                }
            }
        } else if let (&LV::Numeric(numeric), Some(expected_type)) = (&item.value, expected_type) {
            let ty = self.scopes.type_ref(expected_type);
            if !ty.is_compatible_numeric(numeric) {
                return Err(ResolveError::new(item, ResolveErrorKind::IncompatibleNumeric(ty.clone(), numeric)));
            }
            self.binding_set_type_id(item, expected_type)?;
        } else if self.infer_literals {
            // numerics, once normal resolution has failed
            if let LV::Numeric(value) = item.value {
                if let Some(type_id) = self.classify_numeric(value) {
                    self.binding_set_type_id(item, type_id)?;
                }
            }
        } else {
            self.try_create_anon_binding(item);
        }
        Ok(())
    }

    /// Resolves an struct literal and creates the required field types.
    fn resolve_struct_literal(self: &mut Self, item: &mut ast::Literal<'ast>) -> ResolveResult {

        self.try_create_anon_binding(item);

        // resolve type from name

        let type_name = item.type_name.as_mut().unwrap();
        let type_id = self.resolve_type(type_name, None)?;

        // resolve fields from field definition

        if let Some(type_id) = type_id {
            self.binding_set_type_id(item, type_id)?;
            let struct_def = self.scopes.type_ref(type_id).as_struct().unwrap().clone(); // todo: this sucks
            let struct_ = item.value.as_struct_mut().unwrap();
            for (name, field) in &mut struct_.fields {
                self.resolve_literal(field, struct_def.type_id(name))?;
            }
        }

        Ok(())
    }

    /// Resolves an array literal and creates the required array types.
    fn resolve_array_literal(self: &mut Self, item: &mut ast::Literal<'ast>, expected_type: Option<TypeId>) -> ResolveResult {

        let binding_id = self.try_create_anon_binding(item);

        // apply expected type if known
        if let Some(expected_type) = expected_type {
            self.binding_set_type_id(item, expected_type)?;
        }

        let array = item.value.as_array_mut().unwrap();

        // apply the same binding id to all elements
        let element_binding_id = self.try_create_anon_binding(array.elements.first_mut().unwrap());
        let mut elements_type_id = None;

        for element in &mut array.elements {
            *element.binding_id_mut() = Some(element_binding_id);
            self.resolve_literal(element, None)?;
            if elements_type_id == None {
                elements_type_id = self.binding_type_id(element);
            } else {
                let this_type_id = self.binding_type_id(element);
                if this_type_id.is_some() {
                    self.check_types_match(element, elements_type_id, this_type_id)?;
                }
            }
        }

        // create this level's type based on the inner type
        if self.scopes.binding_type_id(binding_id).is_none() {
            let new_type_id = self.scopes.insert_type(self.scope_id, None, Type::Array(Array {
                len     : Some(array.elements.len() as u32),
                type_id : elements_type_id,
            }));
            *self.scopes.binding_type_id_mut(binding_id) = Some(new_type_id);
        }

        // set inner type based on this level's type
        if let Some(type_id) = self.scopes.binding_type_id(binding_id) {
            let ty = self.scopes.type_ref(type_id);
            if let Some(elements_type_id) = ty.as_array().unwrap().type_id {
                self.binding_set_type_id(array.elements.first_mut().unwrap(), elements_type_id)?; // all elements have the same binding id, so set only first item type
            }
        }

        Ok(())
    }
}