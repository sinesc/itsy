//! AST type checker and resolver.

#[path="scopes/scopes.rs"]
mod scopes;
mod stage;
pub mod error;
pub mod resolved;

use crate::{prelude::*, VariantIndex};
use crate::{ItemIndex, STACK_ADDRESS_TYPE};
use crate::frontend::parser::types::ParsedProgram;
use crate::frontend::ast::{self, Visibility, Positioned, Typeable, Resolvable};
use crate::frontend::resolver::error::{OptionToResolveError, ResolveResult, ResolveError, ResolveErrorKind};
use crate::frontend::resolver::resolved::ResolvedProgram;
use crate::shared::{Progress, TypeContainer, BindingContainer, parts_to_path};
use crate::shared::meta::{Array, Struct, Enum, EnumVariant, Trait, ImplTrait, Type, FunctionKind, Binding, Constant, Callable};
use crate::shared::typed_ids::{BindingId, ScopeId, TypeId, ConstantId};
use crate::shared::numeric::Numeric;
use crate::bytecode::{VMFunc, builtins::builtin_types};

/// Temporary internal state during program type/binding resolution.
pub(crate) struct Resolver<'ctx> {
    /// Resolution stage.
    stage           : stage::Stage,
    /// Scope id this state operates in.
    scope_id        : ScopeId,
    /// Repository of all scopes.
    scopes          : &'ctx mut scopes::Scopes,
    /// Primitive to type id mapping.
    primitives      : &'ctx UnorderedMap<&'ctx Type, TypeId>,
    /// Paths of known modules in the program.
    module_paths    : &'ctx Set<String>,
    /// Current module base path.
    module_path     : &'ctx str,
}

/// Resolves types within the given program AST structure.
///
/// # Examples
///
/// The following example parses a string into a module, constructs a program with it and resolves the program.
/// If the program were to be compiled and run, execution would start at the "main" function.
/// ```
/// use itsy::{itsy_api, parser, resolver};
///
/// // Define an API of Rust functions that are callable from the Itsy script.
/// itsy_api!(MyAPI<()> {
///     fn print(&mut context, value: &str) {
///         println!("print: {}", value);
///     }
///     // ... more functions ...
/// });
///
/// fn main() {
///     let module = parser::parse_module("
///         // An Itsy program that calls the Rust 'print' function.
///         fn main() {
///             MyAPI::print(\"Hello from Itsy!\");
///         }
///     ", "").unwrap();
///     let mut parsed = parser::ParsedProgram::new();
///     parsed.add_module(module);
///     let resolved = resolver::resolve::<MyAPI>(parsed, "main").unwrap();
/// }
/// ```
///
/// The returned [ResolvedProgram] is now ready for compilation by [compile](crate::compiler::compile).
#[allow(invalid_type_param_default)]
pub fn resolve<T>(mut program: ParsedProgram, entry_function: &str) -> ResolveResult<ResolvedProgram<T>> where T: VMFunc<T> {

    // create root scope and insert primitives
    let mut scopes = scopes::Scopes::new(
        replace(&mut program.scope_parent_map, UnorderedMap::new()),
        replace(&mut program.binding_map, Set::new())
    );
    let mut primitives = UnorderedMap::new();
    primitives.insert(&Type::void, scopes.insert_type(None, Type::void));
    primitives.insert(&Type::bool, scopes.insert_type(Some("bool"), Type::bool));
    primitives.insert(&Type::u8, scopes.insert_type(Some("u8"), Type::u8));
    primitives.insert(&Type::u16, scopes.insert_type(Some("u16"), Type::u16));
    primitives.insert(&Type::u32, scopes.insert_type(Some("u32"), Type::u32));
    primitives.insert(&Type::u64, scopes.insert_type(Some("u64"), Type::u64));
    primitives.insert(&Type::i8, scopes.insert_type(Some("i8"), Type::i8));
    primitives.insert(&Type::i16, scopes.insert_type(Some("i16"), Type::i16));
    primitives.insert(&Type::i32, scopes.insert_type(Some("i32"), Type::i32));
    primitives.insert(&Type::i64, scopes.insert_type(Some("i64"), Type::i64));
    primitives.insert(&Type::f32, scopes.insert_type(Some("f32"), Type::f32));
    primitives.insert(&Type::f64, scopes.insert_type(Some("f64"), Type::f64));
    primitives.insert(&Type::String, scopes.insert_type(Some("String"), Type::String));

    // insert rust functions into root scope
    for (name, (index, ret_type_name, arg_type_names)) in T::resolve_info().into_iter() {
        let ret_type = if ret_type_name == "" {
            Some(*primitives.get(&Type::void).ice()?)
        } else {
            Some(scopes.type_id(ScopeId::ROOT, ret_type_name).ice_msg(&format!("Unknown type '{}' encountered in rust fn '{}' return position", ret_type_name, name))?)
        };
        let arg_type_id: ResolveResult<Vec<_>> = arg_type_names
            .iter()
            .map(|arg_type_name| {
                let arg_type_name = if &arg_type_name[0..2] == "& " { &arg_type_name[2..] } else { &arg_type_name[..] };
                let type_id = scopes.type_id(ScopeId::ROOT, if arg_type_name == "str" { "String" } else { arg_type_name });
                if type_id.is_some() {
                    Ok(type_id)
                } else {
                    Err(ResolveError::ice(format!("Unknown type '{}' encountered in rust fn '{}' argument position", arg_type_name, name)))
                }
            })
            .collect();
        scopes.insert_function(name, ret_type, arg_type_id?, Some(FunctionKind::Rust(index)));
    }

    // create scopes for each module, import builtin types into module namespace
    for module in &mut program.modules {
        let module_path = module.path.clone() + "::";
        for (_, &type_id) in &primitives {
            if let Some(name) = scopes.type_flat_name(type_id) {
                let name = name.clone();
                scopes.insert_alias(module.scope_id, &name, &(module_path.clone() + &name));
            }
        }
    }

    // assemble set of module paths to check use declarations against
    let module_paths = program.modules().map(|m| m.path.clone()).collect::<Set<_>>();

    // repeatedly try to resolve items until no more progress is made
    let mut now_resolved = Progress::zero();
    let mut prev_resolved;
    let mut stage = stage::Stage::new();

    loop {
        let mut resolver = Resolver {
            stage,
            scope_id        : ScopeId::ROOT,
            scopes          : &mut scopes,
            primitives      : &primitives,
            module_paths    : &module_paths,
            module_path     : "",
        };

        for module in &mut program.modules {
            resolver.module_path = &module.path;
            resolver.scope_id = module.scope_id;
            for mut statement in module.ast.iter_mut() {
                if let Err(mut err) = resolver.resolve_statement(&mut statement) {
                    err.module_path = module.path.clone();
                    return Err(err);
                }
            }
        }

        prev_resolved = now_resolved;
        now_resolved = scopes.resolved() + program.modules().flat_map(|m| m.statements()).fold(Progress::zero(), |acc, statement| acc + statement.num_resolved(&scopes));

        if !now_resolved.done() {
            if now_resolved == prev_resolved {
                if !stage.must_resolve() {
                    stage.next();
                } else {
                    return Resolver::ice("Unresolved types remaining but no errors were triggered during must_resolve stage");
                }
            } else {
                stage.reset();
            }
        } else if now_resolved.done() {
            break;
        }
    }

    // find entry module (empty path) and main function within
    let entry_scope_id = program.modules()
        .find(|&m| m.path == "").ice()?
        .scope_id;

    let entry_fn = match scopes.constant_id(entry_scope_id, &entry_function, TypeId::VOID) {
        Some(constant_id) => scopes.constant_function_id(constant_id),
        None => None,
    };

    Ok(ResolvedProgram {
        ty              : PhantomData,
        modules         : program.modules,
        entry_fn        : entry_fn.usr(None, ResolveErrorKind::UndefinedFunction(entry_function.to_string()))?,
        resolved        : scopes.into(),
    })
}

/// Utility methods to update a typeslot with a resolved type and increase the resolution counter.
impl<'ast, 'ctx> Resolver<'ctx> where 'ast: 'ctx {

    /// Try to create concrete array builtin function signature for the given array type
    fn try_create_array_builtin(self: &mut Self, item: &ast::Expression, name: &str, type_id: TypeId) -> ResolveResult<Option<ConstantId>> {

        if let Some(constant_id) = self.scopes.constant_id(ScopeId::ROOT, name, type_id) {
            return Ok(Some(constant_id));
        }

        let array_ty = self.type_by_id(type_id).as_array().ice()?;
        if let &Array { type_id: element_type_id @ Some(_) } = array_ty {
            Ok(match builtin_types::Array::resolve(self, name, type_id, element_type_id) {
                None => None,
                Some((builtin_type, result_type_id, arg_type_ids)) => {
                    Some(self.scopes.insert_function(
                        name,
                        Some(result_type_id),
                        arg_type_ids.iter().map(|id| Some(*id)).collect::<Vec<Option<TypeId>>>(),
                        Some(FunctionKind::Builtin(type_id, builtin_type))
                    ))
                },
            })
        } else if self.stage.must_resolve() {
            Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(format!("{}", &item)), self.module_path))
        } else {
            Ok(None)
        }
    }

    /// Create integer/float/string builtin function signature.
    fn try_create_scalar_builtin(self: &mut Self, name: &str, type_id: TypeId) -> ResolveResult<Option<ConstantId>> {

        if let Some(constant_id) = self.scopes.constant_id(ScopeId::ROOT, name, type_id) {
            return Ok(Some(constant_id));
        }

        let resolved = match self.type_by_id(type_id) {
            ty @ _ if ty.is_integer()   => builtin_types::Integer::resolve(self, name, type_id, None),
            ty @ _ if ty.is_float()     => builtin_types::Float::resolve(self, name, type_id, None),
            ty @ _ if ty.is_string()    => builtin_types::String::resolve(self, name, type_id, None),
            _ => None,
        };

        Ok(match resolved {
            None => None,
            Some((builtin_type, result_type_id, arg_type_ids)) => {
                Some(self.scopes.insert_function(
                    name,
                    Some(result_type_id),
                    arg_type_ids.iter().map(|id| Some(*id)).collect::<Vec<Option<TypeId>>>(),
                    Some(FunctionKind::Builtin(type_id, builtin_type))
                ))
            },
        })
    }

    /// Returns TypeId of a type suitable to represent the given numeric. Will only consider i32, i64 and f32.
    fn classify_numeric(self: &Self, value: Numeric) -> ResolveResult<Option<TypeId>> {
        Ok(if value.is_integer() {
            if Type::i32.is_compatible_numeric(value) {
                Some(self.primitive_type_id(Type::i32)?)
            } else if Type::i64.is_compatible_numeric(value) {
                Some(self.primitive_type_id(Type::i64)?)
            } else {
                None
            }
        } else if value.is_float() {
            Some(self.primitive_type_id(Type::f64)?)
        } else {
            None
        })
    }

    /// Returns Ok if given_type_id is acceptable to a binding of the accepted_type_id, otherwise a TypeMismatch error.
    fn check_type_accepted_for(self: &Self, item: &impl Positioned, given_type_id: TypeId, accepted_type_id: TypeId) -> ResolveResult  {
        if !self.type_accepted_for(given_type_id, accepted_type_id) {
            let name_given = self.type_name(given_type_id);
            let name_accepted = self.type_name(accepted_type_id);
            let error_kind = ResolveErrorKind::TypeMismatch(name_given, name_accepted);
            Err(ResolveError::new(item, error_kind, self.module_path))
        } else {
            Ok(())
        }
    }

    /// Returns Ok if given types are the same, otherwise a TypeMismatch error.
    fn check_type_equals(self: &Self, item: &impl Positioned, first_type_id: TypeId, second_type_id: TypeId) -> ResolveResult  {
        if !self.type_equals(first_type_id, second_type_id) {
            let name_first = self.type_name(first_type_id);
            let name_second = self.type_name(second_type_id);
            let error_kind = ResolveErrorKind::TypeMismatch(name_first, name_second);
            Err(ResolveError::new(item, error_kind, self.module_path))
        } else {
            Ok(())
        }
    }

    /// Returns the type-id for given primitive.
    pub(crate) fn primitive_type_id(self: &Self, ty: Type) -> ResolveResult<TypeId> {
        self.primitives.get(&ty).cloned().ice()
    }

    /// Returns an internal compiler error with positional information.
    fn ice<R>(message: &str) -> ResolveResult<R> {
        #[cfg(feature="ice_panics")]
        panic!("Internal compiler error: {}", message);
        #[cfg(not(feature="ice_panics"))]
        Err(ResolveError::ice(message.to_string()))
    }

    /// Returns Err if item and expected type are resolved but do not match as well as if must_resolve is set and item is not resolved.
    fn resolved(self: &Self, item: &(impl Positioned+Resolvable)) -> ResolveResult {
        if self.stage.must_resolve() && !item.is_resolved(self) {
            Err(ResolveError::new(item, item.unresolved_error(), self.module_path))
        } else {
            Ok(())
        }
    }

    /// Returns Err if item and expected type are resolved but do not match as well as if must_resolve is set and item is not resolved.
    fn types_resolved(self: &Self, item: &(impl Typeable+Positioned+Resolvable), expected_result: Option<TypeId>) -> ResolveResult {
        if self.stage.must_resolve() {
            if item.is_resolved(self) {
                match expected_result {
                    Some(expected_result) => self.check_type_accepted_for(item, item.type_id(self).ice()?, expected_result),
                    None => Ok(()),
                }
            } else {
                Err(ResolveError::new(item, item.unresolved_error(), self.module_path))
            }
        } else {
            match (item.type_id(self), expected_result) {
                (Some(item_type_id), Some(expected_result)) => self.check_type_accepted_for(item, item_type_id, expected_result),
                _ => Ok(()),
            }
        }
    }

    /// Sets type if for given AST item if it is not set yet, otherwise checks that new type equals existing type.
    fn set_type_id(self: &mut Self, item: &mut (impl Typeable+Positioned), new_type_id: TypeId) -> ResolveResult {
        if let Some(item_type_id) = item.type_id(self) {
            self.check_type_equals(item, item_type_id, new_type_id)?;
        }
        item.set_type_id(self, new_type_id);
        Ok(())
    }

    /*fn try_set_type_id(self: &mut Self, item: &mut (impl Typeable+Positioned), new_type_id: TypeId) -> ResolveResult {
        if let Some(item_type_id) = item.type_id(self) {
            self.check_type_accepted_for(item, new_type_id, item_type_id)?;
        }
        if self.stage.infer_as_concrete() || self.type_by_id(new_type_id).is_concrete() {
            *item.type_id_mut(self) = Some(new_type_id);
        }
        Ok(())
    }*/

    /// Returns the type of given AST item.
    fn item_type(self: &Self, item: &impl Typeable) -> Option<&Type> {
        match item.type_id(self) {
            None => None,
            Some(type_id) => Some(self.type_by_id(type_id)),
        }
    }

    /// Returns an absolute path built from current module and additional path segments.
    fn make_path<T: AsRef<str>>(self: &Self, parts: &[ T ]) -> String {
        let parts_str = parts_to_path(&parts);
        if self.module_path == "" || parts_str == "Self"  {
            parts_str
        } else {
            self.module_path.to_string() + "::" + &parts_str
        }
    }

    /// Transforms a method call from a.b.c() format to c(a.b) if c is a function name (i.e. not a function reference)
    fn transform_constant_call(self: &mut Self, call_exp: &mut ast::Expression, call_args: &mut ast::ArgumentList) -> ResolveResult {
        let mut constant = None;
        if let Some(binary_op) = call_exp.as_binary_op_mut() {
            if binary_op.op == ast::BinaryOperator::Access {
                if let Some(member) = binary_op.right.as_member() {
                    if member.constant_id.is_some() {
                        // last member item is a function name, convert to const
                        constant = Some(
                            ast::Constant {
                                position: member.position,
                                path: ast::Path { position: member.ident.position, name: vec![ member.ident.clone() ]},
                                constant_id: member.constant_id,
                            }
                        );
                    }
                }
            }
        }
        // move left part before function name into arguments
        if let Some(constant) = constant {
            let binary_op = call_exp.as_binary_op_mut().ice()?;
            let arg = std::mem::replace(binary_op.left.as_expression_mut().ice()?, ast::Expression::void(ast::Position(0)));
            *call_exp = ast::Expression::Constant(constant);
            call_args.args.insert(0, arg);
        }
        Ok(())
    }
}

/// Methods to resolve individual AST structures.
impl<'ast, 'ctx> Resolver<'ctx> where 'ast: 'ctx {

    /// Resolves types and bindings used in a statement.
    fn resolve_statement(self: &mut Self, item: &mut ast::Statement) -> ResolveResult  {
        use self::ast::Statement::*;
        match item {
            Function(function) => self.resolve_function(function, None),
            StructDef(structure) => self.resolve_struct_def(structure),
            ImplBlock(impl_block) => self.resolve_impl_block(impl_block),
            TraitDef(trait_def) => self.resolve_trait_def(trait_def),
            LetBinding(binding) => self.resolve_let_binding(binding),
            IfBlock(if_block) => self.resolve_if_block(if_block, None), // accept any type for these, result is discarded
            ForLoop(for_loop) => self.resolve_for_loop(for_loop),
            WhileLoop(while_loop) => self.resolve_while_loop(while_loop),
            Block(block) => self.resolve_block(block, None),
            Return(ret) => self.resolve_return(ret),
            Expression(expression) => self.resolve_expression(expression, None),
            Use(use_declaration) => self.resolve_use_decl(use_declaration),
            EnumDef(enum_def) => self.resolve_enum_def(enum_def),
            Module(_) | Break(_) | Continue(_) => { Ok(()) /* nothing to do here */ },
        }
    }

    /// Resolves use declarations.
    fn resolve_use_decl(self: &mut Self, item: &mut ast::UseDecl) -> ResolveResult  {
        let mut unresolved = None;
        for (name, (ref path, resolved)) in &mut item.mapping.iter_mut().filter(|(_, (_, r))| !r) {
            if self.module_paths.contains(path) {
                *resolved = true;
                self.scopes.insert_alias(self.scope_id, path, name);
            } else if let Some(_) = self.scopes.type_id(self.scope_id, path) { // FIXME scopes.type_id() considers aliases, probably shouldn't do that here
                *resolved = true;
                self.scopes.insert_alias(self.scope_id, path, name);
            } else if let Some(_) = self.scopes.constant_id(self.scope_id, path, TypeId::VOID) { // FIXME: same as type_id, should not consider aliases
                self.scopes.insert_alias(self.scope_id, path, name);
                *resolved = true;
            } else if unresolved == None {
                unresolved = Some(path.clone());
            }
        }
        if !self.stage.must_resolve() || unresolved.is_none() {
            Ok(())
        } else {
            Err(ResolveError::new(item, ResolveErrorKind::UndefinedItem(unresolved.ice()?), self.module_path))
        }
    }

    /// Resolves types and bindings used in an expression.
    fn resolve_expression(self: &mut Self, item: &mut ast::Expression, expected_result: Option<TypeId>) -> ResolveResult {
        use ast::Expression::*;
        match item { // todo: these all need to check expected_result since the caller might depend on an error result on type mismatch
            Literal(literal) => self.resolve_literal(literal, expected_result),
            Variable(variable) => self.resolve_variable(variable, expected_result),
            Constant(constant) => self.resolve_constant(constant, expected_result),
            Assignment(assignment) => self.resolve_assignment(assignment),
            BinaryOp(binary_op) => self.resolve_binary_op(binary_op, expected_result),
            UnaryOp(unary_op) => self.resolve_unary_op(unary_op, expected_result),
            Block(block) => self.resolve_block(block, expected_result),
            IfBlock(if_block) => self.resolve_if_block(if_block, expected_result),
            MatchBlock(match_block) => self.resolve_match_block(match_block, expected_result),
            AnonymousFunction(anonymous_function) => self.resolve_function(anonymous_function, None),
            Closure(closure) => self.resolve_closure(closure, expected_result),
        }
    }

    // Resolves an inline type definition.
    fn resolve_inline_type(self: &mut Self, item: &mut ast::InlineType) -> ResolveResult<Option<TypeId>> {
        use ast::InlineType::*;
        match item {
            TypeName(type_name) => self.resolve_type_name(type_name, None), // todo: not sure about this one. inline-type is defining, so if it differs, the other side should be wrong
            ArrayDef(array) => self.resolve_array_def(array),
            CallableDef(callable) => self.resolve_callable_def(callable),
        }
    }

    /// Resolves a struct definition.
    fn resolve_callable_def(self: &mut Self, item: &mut ast::CallableType) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        // init container type
        if item.type_id.is_none() {
            let ty = Type::Callable(Callable {
                arg_type_ids: Vec::new(),
                ret_type_id: None,
            });
            let new_type_id = self.scopes.insert_type(None, ty);
            item.type_id = Some(new_type_id);
        }
        // resolve args and result
        if let Some(ret) = &mut item.ret {
            self.resolve_inline_type(ret)?;
        }
        for field in &mut item.args {
            self.resolve_inline_type(field)?;
        }
        // assemble type list first to avoid borrow issues
        let ret_type_id = item.ret.as_ref().map_or(Some(TypeId::VOID), |ret| ret.type_id(self));
        let arg_type_ids: Vec<_> = item.args.iter()
            .map(|field| field.type_id(self))
            .collect();
        // assign type ids to definition
        let callable = self.type_by_id_mut(item.type_id.ice()?).as_callable_mut().ice()?;
        callable.arg_type_ids = arg_type_ids;
        callable.ret_type_id = ret_type_id;
        // ensure type is resolved during must-resolve stage
        self.types_resolved(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves a type (name) to a type_id.
    fn resolve_type_name(self: &mut Self, item: &mut ast::TypeName, expected_result: Option<TypeId>) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        if item.type_id.is_none() {
            item.type_id = self.scopes.type_id(self.scope_id, &self.make_path(&item.path.name));
        }
        if let (Some(item_type_id), Some(expected_result)) = (item.type_id, expected_result) {
            self.check_type_accepted_for(item, item_type_id, expected_result)?;
        }
        self.types_resolved(item, expected_result)?;
        Ok(item.type_id)
    }

    /// Resolves an array definition.
    fn resolve_array_def(self: &mut Self, item: &mut ast::ArrayType) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        if let (None, inner_type_id @ Some(_)) = (item.type_id, self.resolve_inline_type(&mut item.element_type)?) {
            let ty = Type::Array(Array {
                type_id : inner_type_id,
            });
            let new_type_id = self.scopes.insert_type(None, ty);
            item.type_id = Some(new_type_id);
        }
        self.types_resolved(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves a struct definition.
    fn resolve_struct_def(self: &mut Self, item: &mut ast::StructType) -> ResolveResult {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(());
        }
        // resolve struct fields
        for (_, field) in &mut item.fields {
            self.resolve_inline_type(field)?;
        }
        // assemble type field list
        let fields: Map<_, _> = item.fields.iter()
            .map(|(field_name, field)| (field_name.clone(), field.type_id(self)))
            .collect();
        // insert or update type
        if let Some(type_id) = item.type_id {
            self.type_by_id_mut(type_id).as_struct_mut().ice()?.fields = fields;
        } else {
            let qualified = self.make_path(&[ &item.ident.name ]);
            let type_id = self.scopes.insert_type(Some(&qualified), Type::Struct(Struct { fields, impl_traits: Map::new() }));
            item.type_id = Some(type_id);
            if item.vis == Visibility::Public {
                // TODO: public visibility
                //self.scopes.alias_type(ScopeId::ROOT, &qualified, type_id);
            }
        }
        self.types_resolved(item, None)
    }

    /// Resolves an enum definition.
    fn resolve_enum_def(self: &mut Self, item: &mut ast::EnumType) -> ResolveResult {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(());
        }

        // check whether at least one enum variant specifies a type, default to i32 if none do
        let simple_type_id = if let Some(named_type) = item.named_type() {
            named_type.type_id.or(self.scopes.type_id(self.scope_id, &self.make_path(&named_type.path.name))).ice_msg("Invalid variant type")?
        } else {
            self.primitive_type_id(Type::i32)?
        };

        // resolve enum variant fields, assemble variant field lists. TODO: fix ugly mess. should add a const stage and handle discriminant values there
        let mut variants = Vec::new();
        let mut next_discriminant = Numeric::Unsigned(0);
        let mut seen_discriminants = Vec::new();
        for variant in &mut item.variants {
            match &mut variant.kind {
                ast::VariantKind::Data(_, fields) => {
                    let mut field_type_ids = Vec::new();
                    for field in fields.iter_mut() {
                        self.resolve_inline_type(field)?;
                        field_type_ids.push(field.type_id(self));
                    }
                    variants.push((variant.ident.name.clone(), EnumVariant::Data(field_type_ids)));
                },
                ast::VariantKind::Simple(value_literal) => {
                    if let Some(value_literal) = value_literal {
                        self.resolve_literal(value_literal, Some(simple_type_id))?;
                        if let Some(numeric) = value_literal.value.as_numeric() {
                            if numeric.is_integer() {
                                if seen_discriminants.contains(&numeric) {
                                    return Err(ResolveError::new(variant, ResolveErrorKind::DuplicateVariantValue(numeric), self.module_path));
                                }
                                seen_discriminants.push(numeric);
                                next_discriminant = numeric.inc();
                                variants.push((variant.ident.name.clone(), EnumVariant::Simple(Some(numeric))));
                            } else {
                                return Err(ResolveError::new(variant, ResolveErrorKind::InvalidVariantValue(numeric), self.module_path));
                            }
                        } else {
                            return Err(ResolveError::new(variant, ResolveErrorKind::InvalidVariantLiteral, self.module_path))
                        }
                    } else {
                        if seen_discriminants.contains(&next_discriminant) {
                            return Err(ResolveError::new(variant, ResolveErrorKind::DuplicateVariantValue(next_discriminant), self.module_path));
                        }
                        seen_discriminants.push(next_discriminant);
                        variants.push((variant.ident.name.clone(), EnumVariant::Simple(Some(next_discriminant))));
                        next_discriminant = next_discriminant.inc();
                    }
                },
            }
        }

        // insert or update type
        if let Some(type_id) = item.type_id {
            self.type_by_id_mut(type_id).as_enum_mut().ice()?.variants = variants;
        } else {
            let qualified = self.make_path(&[ &item.ident.name ]);
            let primitive = if item.is_primitive() {
                Some((simple_type_id, self.type_by_id(simple_type_id).primitive_size()))
            } else {
                None
            };
            let type_id = self.scopes.insert_type(Some(&qualified), Type::Enum(Enum { primitive, variants, impl_traits: Map::new() }));
            item.type_id = Some(type_id);
            if item.vis == Visibility::Public {
                // TODO: public visibility
                //self.scopes.alias_type(ScopeId::ROOT, &qualified, type_id);
            }
        }
        // create variant constructors
        //let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        for (index, variant) in item.variants.iter_mut().enumerate() {
            if variant.is_resolved(self) {
                match &mut variant.kind {
                    ast::VariantKind::Data(function_id @ None, fields) => {
                        let arg_type_ids: Vec<_> = fields.iter().map(|field| field.type_id(self)).collect::<Vec<_>>();
                        let path = self.make_path(&[ &item.ident.name, &variant.ident.name ]);
                        let kind = FunctionKind::Variant(item.type_id.ice()?, index as VariantIndex);
                        *function_id = Some(self.scopes.insert_function(&path, item.type_id, arg_type_ids, Some(kind)));
                    },
                    _ => { },
                }
            }
        }
        //self.scope_id = parent_scope_id;
        self.types_resolved(item, None)
    }

    /// Resolves an implementation block.
    fn resolve_impl_block(self: &mut Self, item: &mut ast::ImplBlock) -> ResolveResult {
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        if item.ty.type_id(self).is_none() {
            self.resolve_inline_type(&mut item.ty)?;
        }
        let trait_type_id = match &mut item.trt {
            Some(trt) => {
                self.resolve_type_name(trt, None)?;
                Some(trt.type_id)
            },
            None => None,
        };
        if let Some(type_id) = item.ty.type_id(self) {
            if self.scopes.alias(self.scope_id, "Self").is_none() {
                let type_name = self.type_name(type_id);
                self.scopes.insert_alias(self.scope_id, &type_name, "Self");
            }
            for function in &mut item.functions {
                self.resolve_function(function, Some(type_id))?;
                // if this is a trait impl and the trait is resolved
                if let Some(Some(trait_type_id)) = trait_type_id {
                    let trt = self.type_by_id(trait_type_id).as_trait().ice()?;
                    let function_name = &function.shared.sig.ident.name;
                    // check if function is defined in trait
                    if trt.provided.get(function_name).is_none() && trt.required.get(function_name).is_none() {
                        let trait_name = item.trt.as_ref().ice()?.path.to_string();
                        return Err(ResolveError::new(function, ResolveErrorKind::NotATraitMethod(function_name.clone(), trait_name), self.module_path));
                    }
                    if let Some(constant_id) = function.constant_id {
                        if let Some(struct_) = self.type_by_id_mut(type_id).as_struct_mut() {
                            let impl_trait = struct_.impl_traits.entry(trait_type_id).or_insert(ImplTrait::new());
                            impl_trait.functions.insert(function_name.clone(), Some(constant_id));
                        }
                    }
                }
            }
        }
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a trait definition block.
    fn resolve_trait_def(self: &mut Self, item: &mut ast::TraitType) -> ResolveResult {
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        // ensure trait exists
        if item.type_id.is_none() {
            let mut trt = Trait { provided: Map::new(), required: Map::new() };
            for function in &mut item.functions {
                let name = function.shared.sig.ident.name.clone();
                match function.shared.block {
                    Some(_) => trt.provided.insert(name, None),
                    None => trt.required.insert(name, None),
                };
            }
            let qualified = self.make_path(&[ &item.ident.name ]);
            let type_id = self.scopes.insert_type(Some(&qualified), Type::Trait(trt));
            item.type_id = Some(type_id);
            // create aliases
            self.scopes.insert_alias(self.scope_id, &qualified, "Self");
            if item.vis == Visibility::Public {
                // TODO: public visibility
                //self.scopes.alias_type(ScopeId::ROOT, &qualified, type_id);
            }
        }
        // try to resolve functions
        for function in &mut item.functions {
            self.resolve_function(function, Some(item.type_id.ice()?))?;
        }
        // update trait
        for function in &item.functions {
            if let Some(constant_id) = function.constant_id {
                let name = &function.shared.sig.ident.name;
                let trt = self.type_by_id_mut(item.type_id.ice()?).as_trait_mut().ice()?;
                match function.shared.block {
                    Some(_) => *trt.provided.get_mut(name).ice()? = Some(constant_id),
                    None => *trt.required.get_mut(name).ice()? = Some(constant_id),
                };
            }
        }
        self.scope_id = parent_scope_id;
        self.types_resolved(item, None)
    }

    /// Resolves a function signature.
    fn resolve_signature(self: &mut Self, item: &mut ast::Signature) -> ResolveResult {
        if item.is_resolved(self) {
            return Ok(());
        }
        // resolve arguments
        for arg in item.args.iter_mut() {
            self.resolve_let_binding(arg)?; // checks resolved_or_err
        }
        // resolve return type
        if let Some(ret) = &mut item.ret {
            self.resolve_inline_type(ret)?; // checks resolved_or_err
        }
        self.resolved(item)
    }

    /// Resolves a closure defintion. // TODO dedup with Function
    fn resolve_closure(self: &mut Self, item: &mut ast::Closure, _expected_result: Option<TypeId>) -> ResolveResult {

        let parent_scope_id = self.scope_id;
        self.scope_id = item.shared.scope_id;
        self.scopes.scope_register_function(self.scope_id);

        // resolve closed over bindings
        if item.struct_type_id.is_none() {
            let closure_type_ids: Map<_, _> = item.required_bindings.iter().map(|&b| ("".to_string(), self.binding_by_id(b).type_id)).collect();
            if closure_type_ids.iter().all(|t| t.1.is_some()) {
                //let mangled_type_name = format!("{}::{}", &self.module_path, &item.shared.sig.ident.name);
                let type_id = self.scopes.insert_type(None, Type::Struct(Struct { fields: closure_type_ids, impl_traits: Map::new() }));
                item.struct_type_id = Some(type_id);
            }
        }

        // resolve signature and expression using signature return type, if any
        self.resolve_signature(&mut item.shared.sig)?;
        self.resolve_block(item.shared.block.as_mut().ice()?, item.shared.sig.ret.as_ref().map(|r| r.type_id(self)).flatten())?;

        // if the closure has an explicit return type // TODO: why set explicit type from non-explicit expression?
        if let Some(ret) = &mut item.shared.sig.ret {
            if item.shared.block.as_ref().ice()?.is_resolved(self) && !ret.is_resolved(self) {
                ret.set_type_id(self, item.shared.block.as_ref().ice()?.type_id(self).ice()?);
            }
        }

        // if signature has a return type, use that, otherwise try to infer from expression
        let ret_type_id = if let Some(ret) = &item.shared.sig.ret {
            ret.type_id(self)
        } else if let Some(type_id) = item.shared.block.as_ref().ice()?.type_id(self) {
            Some(type_id)
        } else {
            None
        };

        if item.function_id.is_none() && item.shared.sig.ret_resolved(self) && item.shared.sig.args_resolved(self) && ret_type_id.is_some() {
            let arg_type_ids = item.shared.sig.arg_type_ids(self);
            // insert function
            let function_id = self.scopes.insert_closure(ret_type_id, arg_type_ids.clone());
            item.function_id = Some(function_id);
            self.scopes.scope_set_function_id(self.scope_id, function_id);
            // insert function type
            let closure_ty = Type::Callable(Callable { arg_type_ids, ret_type_id });
            let closure_type_id = self.scopes.insert_anonymous_type(true, closure_ty);
            item.type_id = Some(closure_type_id);
        }

        self.scope_id = parent_scope_id;

        if self.stage.must_resolve() && (!item.shared.sig.args_resolved(self) || !item.shared.sig.ret_resolved(self)) {
            Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(format!("signature for '{}'", item.shared.sig.ident.name)), self.module_path))
        } else {
            Ok(())
        }
    }

    /// Resolves a function defintion.
    fn resolve_function(self: &mut Self, item: &mut ast::Function, struct_scope: Option<TypeId>) -> ResolveResult {

        let parent_scope_id = self.scope_id;
        self.scope_id = item.shared.scope_id;
        self.resolve_signature(&mut item.shared.sig)?;

        if item.constant_id.is_none() && item.shared.sig.ret_resolved(self) && item.shared.sig.args_resolved(self) {
            let result_type_id = item.shared.sig.ret_type_id(self);
            let arg_type_ids: Vec<_> = item.shared.sig.arg_type_ids(self);
            // function/method switch
            let (function_kind, qualified, alias) = if let Some(type_id) = struct_scope {
                let type_name = self.type_flat_name(type_id).ice()?;
                let path = self.make_path(&[ type_name, &item.shared.sig.ident.name ]);
                if item.shared.sig.args.len() == 0 || item.shared.sig.args[0].ident.name != "self" { // FIXME ugh
                    let alias = self.make_path(&[ "Self", &item.shared.sig.ident.name ]);
                    (FunctionKind::Function, path, Some(alias))
                } else {
                    (FunctionKind::Method(type_id), path, None)
                }
            } else {
                let path = self.make_path(&[ &item.shared.sig.ident.name ]);
                (FunctionKind::Function, path, None)
            };
            let constant_id = self.scopes.insert_function(&qualified, result_type_id, arg_type_ids, Some(function_kind));
            if let Some(alias) = alias {
                self.scopes.insert_alias(parent_scope_id, qualified, alias);
            }
            if item.shared.sig.vis == Visibility::Public {
                // TODO: public visibility
                //self.scopes.alias_constant(ScopeId::ROOT, &qualified, constant_id);
            }
            let function_id = self.scopes.constant_function_id(constant_id).ice()?;
            item.constant_id = Some(constant_id);
            self.scopes.scope_set_function_id(self.scope_id, function_id);
        }
        if let Some(constant_id) = item.constant_id {
            let function_id = self.scopes.constant_function_id(constant_id).ice()?;
            let ret_type = self.scopes.function_ref(function_id).ret_type_id(self);
            if let Some(block) = &mut item.shared.block {
                self.resolve_block(block, ret_type)?;
            }
        }
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a return statement.
    fn resolve_return(self: &mut Self, item: &mut ast::Return) -> ResolveResult {
        let function_id = self.scopes.scope_function_id(self.scope_id).usr(Some(item), ResolveErrorKind::InvalidOperation("Use of return outside of function".to_string()))?;
        let ret_type_id = if let Some(function_id) = function_id {
            let ret_type_id = self.scopes.function_ref(function_id).ret_type_id(self);
            self.types_resolved(&mut item.expr, None)?;
            self.resolve_expression(&mut item.expr, ret_type_id)?;
            ret_type_id
        } else {
            None
        };
        // check return type matches function result type
        self.types_resolved(&mut item.expr, ret_type_id)
    }

    /// Resolves a variable name to a variable.
    fn resolve_variable(self: &mut Self, item: &mut ast::Variable, expected_result: Option<TypeId>) -> ResolveResult {
        // set expected type, if it isn't a trait or we're in final resolver stage
        if item.type_id(self).is_none() {
            if let Some(expected_result) = expected_result {
                if self.stage.infer_as_concrete() || !self.type_by_id(expected_result).as_trait().is_some() {
                    self.set_type_id(item, expected_result)?;
                }
            }
        }
        self.types_resolved(item, expected_result)
    }

    /// Resolves a constant name to a constant.
    fn resolve_constant(self: &mut Self, item: &mut ast::Constant, expected_result: Option<TypeId>) -> ResolveResult {
        // resolve constant
        if item.constant_id.is_none() {
            let path = self.make_path(&item.path.name);
            item.constant_id = self.scopes.constant_id(self.scope_id, &path, TypeId::VOID);
            // builtin function
            if item.path.name.len() == 2 {
                if let Some(type_id) = self.scopes.type_id(ScopeId::ROOT, &item.path.name[0]) {
                    if let Some(constant_id) = self.try_create_scalar_builtin(&item.path.name[1].name, type_id)? {
                        item.constant_id = Some(constant_id);
                    }
                }
            }
            if item.constant_id.is_none() && self.stage.must_exist() {
                return Err(ResolveError::new(item, ResolveErrorKind::UndefinedIdentifier(item.path.to_string()), self.module_path));
            }
        }
        // set expected type, if it isn't a trait or we're in final resolver stage
        if item.type_id(self).is_none() && item.constant_id.is_some() {
            if let Some(expected_result) = expected_result {
                if self.stage.infer_as_concrete() || !self.type_by_id(expected_result).as_trait().is_some() {
                    self.set_type_id(item, expected_result)?;
                }
            }
        }
        self.types_resolved(item, expected_result)
    }

    /// Resolves an if block.
    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        // resolve condition and block
        self.resolve_expression(&mut item.cond, Some(self.primitive_type_id(Type::bool)?))?;
        self.resolve_block(&mut item.if_block, expected_result)?;
        // optionally resolve else block
        if let Some(else_block) = &mut item.else_block {
            self.resolve_block(else_block, expected_result)?;
            if let (Some(if_type_id), Some(else_type_id)) = (item.if_block.type_id(self), else_block.type_id(self)) {
                self.check_type_accepted_for(item, else_type_id, if_type_id)?;
            }
        } else if let Some(if_type_id) = item.if_block.type_id(self) {
            // if block with a non-void result but no else block
            self.check_type_accepted_for(item, if_type_id, TypeId::VOID)?; // Todo: meh, using this to generate an error when we already know there is an error.
        }
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a match expression.
    fn resolve_match_block(self: &mut Self, item: &mut ast::MatchBlock, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        self.resolve_expression(&mut item.expr, None)?;
        for (_, block) in &mut item.branches {
            self.resolve_block(block, expected_result)?;
        }
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a for loop.
    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop) -> ResolveResult {
        use ast::{Expression::*, BinaryOperator as Op};
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        // create binding for the iterator variable
        self.resolve_let_binding(&mut item.iter)?;
        match &item.expr { // NOTE: these need to match Compiler::compile_for_loop
            BinaryOp(bo) if bo.op == Op::Range || bo.op == Op::RangeInclusive => {
                let type_id = item.iter.type_id(self);
                self.resolve_expression(&mut item.expr, type_id)?;
                if let Some(type_id) = item.expr.type_id(self) {
                    self.set_type_id(&mut item.iter, type_id)?;
                }
            },
            Literal(_) | Constant(_) | Variable(_) | Block(_) | IfBlock(_) | MatchBlock(_)  => {
                self.resolve_expression(&mut item.expr, None)?;
                if let Some(&Type::Array(Array { type_id: Some(elements_type_id) })) = self.item_type(&item.expr) {
                    // infer iter type from array element type
                    self.set_type_id(&mut item.iter, elements_type_id)?;
                } else if let (Some(iter_type_id), Some(array_type_id)) = (item.iter.type_id(self), item.expr.type_id(self)) {
                    // infer array element type from iter
                    if let Some(array) = self.type_by_id_mut(array_type_id).as_array_mut() {
                        array.type_id = Some(iter_type_id);
                    }
                } else if self.item_type(&item.expr).map_or(false, |expr| expr.as_array().is_none()) {
                    let type_name = self.type_name(item.expr.type_id(self).ice()?);
                    return Err(ResolveError::new(&item.expr, ResolveErrorKind::NotIterable(type_name), self.module_path));
                }
            },
            _ => return Err(ResolveError::new(&item.iter, ResolveErrorKind::InvalidOperation("Unsupported for in operand".to_string()), self.module_path)),
        };
        // handle block
        self.resolve_block(&mut item.block, Some(self.primitive_type_id(Type::void)?))?;
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a while loop.
    fn resolve_while_loop(self: &mut Self, item: &mut ast::WhileLoop) -> ResolveResult {
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        self.resolve_expression(&mut item.expr, None)?;
        self.resolve_block(&mut item.block, None)?;
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a block.
    fn resolve_block(self: &mut Self, item: &mut ast::Block, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        // resolve statments, result and returns
        for statement in item.statements.iter_mut() {
            self.resolve_statement(statement)?;
        }
        if let Some(ref mut result) = item.result {
            self.resolve_expression(result, expected_result)?;
        }
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves an assignment expression.
    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment) -> ResolveResult {
        let right_type_id = item.right.type_id(self);
        self.resolve_expression(&mut item.left, right_type_id)?;
        let left_type_id = item.left.type_id(self);
        self.resolve_expression(&mut item.right, left_type_id)?;
        item.type_id = Some(TypeId::VOID);
        self.resolved(item)
    }

    /// Resolves a binary operation.
    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::BinaryOperator::*;
        match item.op {
            And | Or => {
                self.resolve_expression(item.left.as_expression_mut().ice()?, Some(self.primitive_type_id(Type::bool)?))?;
                self.resolve_expression(item.right.as_expression_mut().ice()?, Some(self.primitive_type_id(Type::bool)?))?;
                item.set_type_id(self, self.primitive_type_id(Type::bool)?);
            },
            Less | Greater | LessOrEq | GreaterOrEq | Equal | NotEqual => {
                self.resolve_expression(item.left.as_expression_mut().ice()?, None)?;
                let left_type_id = item.left.type_id(self);
                self.resolve_expression(item.right.as_expression_mut().ice()?, left_type_id)?;
                let right_type_id = item.right.type_id(self);
                item.set_type_id(self, self.primitive_type_id(Type::bool)?);
                if let Some(common_type_id) = left_type_id.or(right_type_id) {
                    self.set_type_id(item.left.as_expression_mut().ice()?, common_type_id)?;
                    self.set_type_id(item.right.as_expression_mut().ice()?, common_type_id)?;
                }
            },
            Add | Sub | Mul | Div | Rem | Range | RangeInclusive => {
                self.resolve_expression(item.left.as_expression_mut().ice()?, expected_result)?;
                let left_type_id = item.left.type_id(self);
                self.resolve_expression(item.right.as_expression_mut().ice()?, left_type_id)?;
                let type_id = item.type_id(self);
                let right_type_id = item.right.type_id(self);
                if let Some(common_type_id) = type_id.or(left_type_id).or(right_type_id) {
                    self.set_type_id(item, common_type_id)?;
                    self.set_type_id(item.left.as_expression_mut().ice()?, common_type_id)?;
                    self.set_type_id(item.right.as_expression_mut().ice()?, common_type_id)?;
                }
            },
            Index | IndexWrite => {
                self.resolve_expression(item.left.as_expression_mut().ice()?, None)?;
                if let Some(left_type_id) = item.left.type_id(self) {
                    if !self.type_by_id(left_type_id).as_array().is_some() {
                        return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} does not implement index access", &item.left)), self.module_path));
                    }
                }
                self.resolve_expression(item.right.as_expression_mut().ice()?, Some(self.primitive_type_id(STACK_ADDRESS_TYPE)?))?;
                // left[right] : item
                self.set_type_id(item.right.as_expression_mut().ice()?, self.primitive_type_id(STACK_ADDRESS_TYPE)?)?;
                // if we expect the result to be of a particular type, set it now
                if let Some(expected_result) = expected_result {
                    self.set_type_id(item, expected_result)?;
                }
                // if we know the result type, set the array element type to that
                if let Some(result_type_id) = item.type_id(self) {
                    if let Some(Type::Array(array)) = self.item_type(&item.left) {
                        if let Some(array_type_id) = array.type_id {
                            self.check_type_accepted_for(item, result_type_id, array_type_id)?;
                        } else {
                            // TODO: this is pretty ugly, referencing left type again. working around mutable borrow issues when directly borrowing mutably in above if let
                            let ty = item.left.type_id(self).map(|type_id| self.type_by_id_mut(type_id));
                            if let Some(Type::Array(array)) = ty {
                                array.type_id = Some(result_type_id);
                            }
                        }
                    }
                }
                // if we know the array element type, set the result type to that
                if let Some(&Type::Array(Array { type_id: Some(element_type_id), .. })) = self.item_type(&item.left) {
                    self.set_type_id(item, element_type_id)?;
                }
            },
            Access | AccessWrite => {
                self.resolve_expression(item.left.as_expression_mut().ice()?, None)?;
                //self.resolve_expression(item.right.as_member_mut().ice()?, None)?;
                // left.right : item
                if let Some(left_type_id) = item.left.type_id(self) {
                    // check if a built-in method is accessed
                    if item.right.type_id(self).is_none() { // TODO: Access only
                        let owning_type_id = item.left.type_id(self).ice()?;
                        let member_name = &item.right.as_member().ice()?.ident.name;
                        let left_ty = self.type_by_id(left_type_id);
                        let constant_id = if left_ty.as_array().is_some() {
                            // lookup array builtin or try to create it
                            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                                .or(self.try_create_array_builtin(item.left.as_expression().ice()?, member_name, owning_type_id)?)
                        } else if left_ty.is_float() || left_ty.is_integer() || left_ty.is_string() {
                            // lookup scalar builtin or try to create it
                            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                                .or(self.try_create_scalar_builtin(member_name, owning_type_id)?)
                        } else {
                            // try method on type or trait default implementation
                            let type_name = self.type_flat_name(owning_type_id).ice_msg("Unnamed type")?;
                            let path = self.make_path(&[ type_name, member_name ]);
                            self.scopes.constant_id(self.scope_id, &path, owning_type_id)
                                .or(self.scopes.trait_provided_constant_id(member_name, owning_type_id))
                        };
                        if let Some(constant_id) = constant_id {
                            let function_id = self.scopes.constant_function_id(constant_id).ice()?;
                            let function = self.scopes.function_ref(function_id);
                            let member = item.right.as_member_mut().ice()?;
                            member.constant_id = Some(constant_id);
                            self.set_type_id(member, function.signature_type_id)?;
                        }
                    }
                    // check if a struct field or method is accessed
                    if item.right.type_id(self).is_none() {
                        match self.type_by_id(left_type_id) {
                            Type::Struct(struct_) => {
                                // TODO: struct methods
                                let field = item.right.as_member_mut().ice_msg("Member access using a non-field")?;
                                let field_type_id = *struct_.fields.get(&field.ident.name).usr(Some(field), ResolveErrorKind::UndefinedMember(field.ident.name.clone()))?;
                                if let Some(field_type_id) = field_type_id {
                                    //self.set_type_id(item, field_type_id)?;
                                    self.set_type_id(field, field_type_id)?;
                                }
                            },
                            Type::Array(_) => {
                                // can't ice here since builtin resolve above might actually temporarily fail on arrays that don't have
                                // their inner type resolved yet
                            },
                            x @ _ => {
                                Self::ice(&format!("Member access on unsupported type {:?}", x))?;
                            },
                        }
                    }
                    if let Some(type_id) = item.right.type_id(self) {
                        self.set_type_id(item, type_id)?; // ignored if member is constant
                    }
                }
            },
            Cast => {
                self.resolve_type_name(item.right.as_type_name_mut().ice()?, expected_result)?;
                self.resolve_expression(item.left.as_expression_mut().ice()?, None)?;
                if let Some(type_id) = item.right.type_id(self) {
                    item.set_type_id(self, type_id);
                }
                if let (Some(from_type_id), Some(to_type_id)) = (item.left.type_id(self), item.right.type_id(self)) {
                    if from_type_id != to_type_id {
                        let from_type = self.type_by_id(from_type_id);
                        let to_type = self.type_by_id(to_type_id);
                        if
                            (to_type.is_string() && !(from_type.is_numeric())) ||
                            (to_type.is_integer() && !(from_type.is_numeric() || from_type.is_bool() || from_type.is_simple_enum())) ||
                            (to_type.is_float() && !from_type.is_numeric()) ||
                            (to_type.is_bool() && !from_type.is_integer()) ||
                            !(to_type.is_string() || to_type.is_integer() || to_type.is_float() || to_type.is_bool())
                        {
                            let from_name = self.type_name(from_type_id);
                            let to_name = self.type_name(to_type_id);
                            return Err(ResolveError::new(item, ResolveErrorKind::InvalidCast(from_name, to_name), self.module_path));
                        }
                    }
                }
            },
            Call => {
                // resolve function
                let call_func = item.left.as_expression_mut().ice()?;
                self.resolve_expression(call_func, None)?;
                let call_args = item.right.as_argument_list_mut().ice()?;
                self.transform_constant_call(call_func, call_args)?;
                let num_args = call_args.args.len();
                // set return type from signature
                if let Some(function_type_id) = call_func.type_id(self) {

                    let func = self.type_by_id(function_type_id).as_callable().ice()?.clone(); //FIXME borrow

                    if let Some(ret_type_id) = func.ret_type_id {
                        self.set_type_id(item, ret_type_id)?;
                    }

                    if let (Some(item_type_id), Some(expected_result)) = (item.type_id(self), expected_result) {
                        self.check_type_accepted_for(item, item_type_id, expected_result)?;
                    }

                    if func.arg_type_ids.len() != num_args {
                        let function_name = format!("{}", item.left.as_expression().ice()?);
                        return Err(ResolveError::new(item, ResolveErrorKind::NumberOfArguments(function_name, func.arg_type_ids.len() as ItemIndex, num_args as ItemIndex), self.module_path));
                    }

                    let arguments = item.right.as_argument_list_mut().ice()?;

                    for (index, &expected_type_id) in func.arg_type_ids.iter().enumerate() {
                        self.resolve_expression(&mut arguments.args[index], expected_type_id)?;
                        let actual_type_id = arguments.args[index].type_id(self);
                        // infer arguments
                        if actual_type_id.is_none() && expected_type_id.is_some() {
                            if let ast::Expression::Constant(x) = &arguments.args[index] {
                                if x.constant_id.is_none() {
                                    continue; // FIXME: omfg, get this out of here
                                }
                            }
                            self.set_type_id(&mut arguments.args[index], expected_type_id.ice()?)?;
                        } else if let (Some(actual_type_id), Some(expected_type_id)) = (actual_type_id, expected_type_id) {
                            self.check_type_accepted_for(&arguments.args[index], actual_type_id, expected_type_id)?;
                        }
                    }
                }
                // if we expect the result to be of a particular type, set it now // TODO move above inference block, fix borrow issue
                if let Some(expected_result) = expected_result {
                    self.set_type_id(item, expected_result)?;
                }
            },
            Assign | AddAssign | SubAssign | MulAssign | DivAssign | RemAssign => {
                return Self::ice("Unexpected operator in resolve_binary_op");
            },
        }
        self.resolved(item)
    }

    /// Resolves a binding created by let, for or a signature.
    fn resolve_let_binding(self: &mut Self, item: &mut ast::LetBinding) -> ResolveResult {

        // check if a type is specified
        if let Some(inline_type) = &mut item.ty {
            if let Some(inline_type_id) = self.resolve_inline_type(inline_type)? {
                self.set_type_id(item, inline_type_id)?;
            }
        }

        // resolve expression, if we have a type, apply it to the expression
        let lhs = item.type_id(self);

        if let Some(expr) = &mut item.expr {
            self.resolve_expression(expr, lhs)?;
            // if the expression has a type and the binding doesn't specify an explicit type, apply it back to the binding
            if let (None, None, Some(expr_type_id)) = (&item.ty, lhs, expr.type_id(self)) {
                self.set_type_id(item, expr_type_id)?;
            }
        };

        // check binding is resolved. Resolvable (via resolved_or_err) does not do this for us. (TODO/FIXME)
        if self.stage.must_resolve() {
            if let Binding { type_id: None, .. } = self.binding_by_id(item.binding_id) {
                return Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(item.ident.to_string()), self.module_path))
            }
        }

        self.types_resolved(item, None)
    }

    /// Resolves a unary operation.
    fn resolve_unary_op(self: &mut Self, item: &mut ast::UnaryOp, expected_type: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::UnaryOperator::*;
        self.resolve_expression(&mut item.expr, expected_type)?;
        match item.op {
            Not => {
                if let Some(expected_type_id) = expected_type {
                    self.check_type_accepted_for(item, self.primitive_type_id(Type::bool)?, expected_type_id)?;
                }
                self.set_type_id(item, self.primitive_type_id(Type::bool)?)?;
            },
            Plus | Minus => {
                if let Some(type_id) = item.expr.type_id(self) {
                    self.set_type_id(item, type_id)?;
                }
            },
        }
        self.types_resolved(&item.expr, None)?;
        self.types_resolved(item, expected_type)
    }

    /// Resolves a literal type if it is annotated, otherwise let the parent expression pick a concrete type.
    fn resolve_literal(self: &mut Self, item: &mut ast::Literal, expected_type: Option<TypeId>) -> ResolveResult {
        use self::ast::LiteralValue as LV;

        if let LV::Bool(_) = item.value { // todo: all of these need to check expected type if any
            if let Some(expected_type_id) = expected_type {
                self.check_type_accepted_for(item, self.primitive_type_id(Type::bool)?, expected_type_id)?;
            }
            self.set_type_id(item, self.primitive_type_id(Type::bool)?)?;
        } else if let LV::String(_) = item.value {
            if let Some(expected_type_id) = expected_type {
                self.check_type_accepted_for(item, self.primitive_type_id(Type::String)?, expected_type_id)?;
            }
            self.set_type_id(item, self.primitive_type_id(Type::String)?)?;
        } else if let LV::Array(_) = item.value {
            self.resolve_array_literal(item, expected_type)?;
        } else if let LV::Struct(_) = item.value {
            self.resolve_struct_literal(item)?;
        } else if let Some(type_name) = &mut item.type_name {
            // literal has explicit type, use it
            if let Some(explicit_type_id) = self.resolve_type_name(type_name, expected_type)? {
                self.set_type_id(item, explicit_type_id)?;
                if let Some(expected_type_id) = expected_type {
                    self.check_type_accepted_for(item, explicit_type_id, expected_type_id)?;
                }
            }
        } else if let (&LV::Numeric(numeric), Some(expected_type)) = (&item.value, expected_type) {
            let ty = self.type_by_id(expected_type);
            if !ty.is_compatible_numeric(numeric) {
                return Err(ResolveError::new(item, ResolveErrorKind::IncompatibleNumeric(self.type_name(expected_type), numeric), self.module_path));
            }
            self.set_type_id(item, expected_type)?;
        } else if self.stage.infer_literals() {
            // numerics, once normal resolution has failed
            if let LV::Numeric(value) = item.value {
                if let Some(type_id) = self.classify_numeric(value)? {
                    self.set_type_id(item,type_id)?;
                }
            }
        }
        self.types_resolved(item, expected_type)
    }

    /// Resolves an struct literal and creates the required field types.
    fn resolve_struct_literal(self: &mut Self, item: &mut ast::Literal) -> ResolveResult {

        // resolve type from name

        let type_name = item.type_name.as_mut().ice()?;
        let type_id = self.resolve_type_name(type_name, None)?;

        // resolve fields from field definition

        if let Some(type_id) = type_id {
            self.set_type_id(item, type_id)?;
            let struct_def = self.type_by_id(type_id).as_struct().usr(Some(item), ResolveErrorKind::Internal("Tried to resolve a struct but got different type".to_string()))?.clone();
            let struct_ = item.value.as_struct_mut().ice()?;
            for (name, field) in &mut struct_.fields {
                if !struct_def.has_field(name) {
                    return Err(ResolveError::new(field, ResolveErrorKind::UndefinedMember(name.clone()), self.module_path));
                }
                self.resolve_expression(field, struct_def.type_id(name))?;
                self.types_resolved(field, None)?;
            }
        }

        self.types_resolved(item, None)
    }

    /// Resolves an array literal and creates the required array types.
    fn resolve_array_literal(self: &mut Self, item: &mut ast::Literal, expected_type_id: Option<TypeId>) -> ResolveResult {

        let mut elements_type_id = None;

        // apply expected type if known. if we already have a known type, set_type_id will check that it matches the one we're trying to set
        if let Some(expected_type_id) = expected_type_id {
            if self.type_by_id(expected_type_id).as_array().is_some() {
                self.set_type_id(item, expected_type_id)?;
            } else {
                let expected_name = self.type_name(expected_type_id);
                let received_name = if let Some(type_id) = item.type_id { self.type_name(type_id) } else { "?".to_string() };
                return Err(ResolveError::new(item, ResolveErrorKind::TypeMismatch(received_name, expected_name), self.module_path));
            }
        }

        // if we have a type for the array, check if we also have an inner type. if so we want to apply it to all contained literals later.
        if let Some(&Type::Array(Array { type_id: Some(type_id), .. })) = self.item_type(item) {
            elements_type_id = Some(type_id);
        }

        // mutably borrow literal as array literal
        let type_id = item.type_id(self);
        let array_literal = item.value.as_array_mut().ice_msg("Expected array type, got something else")?;

        // try to get inner type from element literals
        if elements_type_id.is_none() {
            for element in &mut array_literal.elements {
                self.resolve_expression(element, None)?;
                if element.type_id(self).is_some() {
                    elements_type_id = element.type_id(self);
                    break;
                }
            }
        }

        // if we have a known inner type, resolve contained literals with it to help with their inference
        if elements_type_id.is_some() {
            for element in &mut array_literal.elements {
                self.resolve_expression(element, elements_type_id)?;
            }
        }

        // if we don't yet have one, create type based on the inner type, otherwise check types all match
        if type_id.is_none() {
            let new_type_id = self.scopes.insert_type(None, Type::Array(Array {
                type_id : elements_type_id,
            }));
            item.set_type_id(self, new_type_id);
        } else if let Some(elements_type_id) = elements_type_id {
            let array_type_id = item.type_id(self).ice()?;
            let array_ty = self.type_by_id_mut(array_type_id).as_array_mut().ice()?;
            if let Some(current_element_type_id) = array_ty.type_id {
                self.check_type_accepted_for(item, current_element_type_id, elements_type_id)?;
            } else {
                array_ty.type_id = Some(elements_type_id);
            }
        }

        self.types_resolved(item, expected_type_id)
    }
}

/// Support TypeContainer for Scopes so that methods that need to follow type_ids can be implemented once and be used in both
/// the Resolver where types are stored in Scopes and the Compiler where types are a stored in a Vec.
impl<'ctx> TypeContainer for Resolver<'ctx> {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type {
        self.scopes.type_ref(type_id)
    }
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type {
        self.scopes.type_mut(type_id)
    }
    fn type_flat_name(self: &Self, type_id: TypeId) -> Option<&String> {
        self.scopes.type_flat_name(type_id)
    }
}

/// A container holding binding id to Binding mappings
impl<'ctx> BindingContainer for Resolver<'ctx> {
    fn binding_by_id(self: &Self, binding_id: BindingId) -> &Binding {
        self.scopes.binding_ref(binding_id)
    }
    fn binding_by_id_mut(self: &mut Self, binding_id: BindingId) -> &mut Binding {
        self.scopes.binding_mut(binding_id)
    }
    fn constant_by_id(self: &Self, constant_id: ConstantId) -> &Constant {
        self.scopes.constant_ref(constant_id)
    }
    fn constant_by_id_mut(self: &mut Self, constant_id: ConstantId) -> &mut Constant {
        self.scopes.constant_mut(constant_id)
    }
}