//! AST type checker and resolver.

#[path="scopes/scopes.rs"]
mod scopes;
pub mod error;
pub mod resolved;

use crate::{prelude::*, VariantIndex};
use crate::{ItemIndex, STACK_ADDRESS_TYPE};
use crate::frontend::parser::types::ParsedProgram;
use crate::frontend::ast::{self, Visibility, LiteralValue, Positioned, Typeable, Resolvable, CallSyntax};
use crate::frontend::resolver::error::{SomeOrResolveError, ResolveResult, ResolveError, ResolveErrorKind, ice, ICE};
use crate::frontend::resolver::resolved::ResolvedProgram;
use crate::shared::{Progress, TypeContainer, BindingContainer, parts_to_path};
use crate::shared::meta::{Array, Struct, Enum, EnumVariant, Trait, ImplTrait, Type, FunctionKind, Binding, Constant};
use crate::shared::typed_ids::{BindingId, ScopeId, TypeId, FunctionId, ConstantId};
use crate::shared::numeric::Numeric;
use crate::bytecode::{VMFunc, builtins::builtin_types};

#[derive(Copy, Clone, Debug)]
struct Stage(u8);

impl Stage {
    /// Construct stage.
    fn new() -> Self {
        Stage(0)
    }
    /// Proceed to next stage.
    fn next(self: &mut Self) {
        self.0 += 1;
    }
    /// Resets back to first stage.
    fn reset(self: &mut Self) {
        self.0 = 0;
    }
    /// Assume paths to be absolute.
    fn absolute_paths(self: &Self) -> bool {
        self.0 == 1
    }
    /// Assume unresolved numeric literals to be their default types.
    fn infer_literals(self: &Self) -> bool {
        self.0 == 2
    }
    /// Allow unresolved types to be inferred from inconcrete types.
    fn infer_as_concrete(self: &Self) -> bool {
        self.0 == 3
    }
    /// Previous stages failed, next resolution failure must trigger a resolution error.
    fn must_resolve(self: &Self) -> bool {
        self.0 >= 4
    }
}

/// Temporary internal state during program type/binding resolution.
pub(crate) struct Resolver<'ctx> {
    /// Resolution stage.
    stage           : Stage,
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
/// itsy_api!(MyAPI, (), {
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
///             print(\"Hello from Itsy!\");
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
    let mut scopes = scopes::Scopes::new();
    let root_scope_id = ScopeId::ROOT;

    let mut primitives = UnorderedMap::new();
    primitives.insert(&Type::void, scopes.insert_type(root_scope_id, None, Type::void));
    primitives.insert(&Type::bool, scopes.insert_type(root_scope_id, Some("bool"), Type::bool));
    primitives.insert(&Type::u8, scopes.insert_type(root_scope_id, Some("u8"), Type::u8));
    primitives.insert(&Type::u16, scopes.insert_type(root_scope_id, Some("u16"), Type::u16));
    primitives.insert(&Type::u32, scopes.insert_type(root_scope_id, Some("u32"), Type::u32));
    primitives.insert(&Type::u64, scopes.insert_type(root_scope_id, Some("u64"), Type::u64));
    primitives.insert(&Type::i8, scopes.insert_type(root_scope_id, Some("i8"), Type::i8));
    primitives.insert(&Type::i16, scopes.insert_type(root_scope_id, Some("i16"), Type::i16));
    primitives.insert(&Type::i32, scopes.insert_type(root_scope_id, Some("i32"), Type::i32));
    primitives.insert(&Type::i64, scopes.insert_type(root_scope_id, Some("i64"), Type::i64));
    primitives.insert(&Type::f32, scopes.insert_type(root_scope_id, Some("f32"), Type::f32));
    primitives.insert(&Type::f64, scopes.insert_type(root_scope_id, Some("f64"), Type::f64));
    primitives.insert(&Type::String, scopes.insert_type(root_scope_id, Some("String"), Type::String));

    // insert rust functions into root scope
    for (&name, (index, ret_type_name, arg_type_names)) in T::resolve_info().iter() {
        let ret_type = if *ret_type_name == "" {
            Some(*primitives.get(&Type::void).unwrap_or_ice(ICE)?)
        } else {
            Some(scopes.local_type_id(root_scope_id, ret_type_name).unwrap_or_ice(&format!("Unknown type '{}' encountered in rust fn '{}' return position", ret_type_name, name))?)
        };
        let arg_type_id: ResolveResult<Vec<_>> = arg_type_names
            .iter()
            .map(|arg_type_name| {
                let arg_type_name = if &arg_type_name[0..2] == "& " { &arg_type_name[2..] } else { &arg_type_name[..] };
                scopes.local_type_id(root_scope_id, if arg_type_name == "str" { "String" } else { arg_type_name }) // todo: fix string hack
                    .some_or_ice(&format!("Unknown type '{}' encountered in rust fn '{}' argument position", arg_type_name, name))
            })
            .collect();
        scopes.insert_function(root_scope_id, name, ret_type, arg_type_id?, Some(FunctionKind::Rust(*index)));
    }

    // create scopes for each module
    for module in &mut program.0 {
        module.scope_id = Some(scopes.create_scope(root_scope_id));
    }

    // assemble set of module paths to check use declarations against
    let module_paths = program.modules().map(|m| m.path.clone()).collect::<Set<_>>();

    // repeatedly try to resolve items until no more progress is made
    let mut now_resolved = Progress::zero();
    let mut prev_resolved;
    let mut stage = Stage::new();

    loop {
        let mut resolver = Resolver {
            stage,
            scope_id        : root_scope_id,
            scopes          : &mut scopes,
            primitives      : &primitives,
            module_paths    : &module_paths,
            module_path     : "",
        };

        for module in &mut program.0 {
            resolver.module_path = &module.path;
            resolver.scope_id = module.scope_id.unwrap();
            for mut statement in module.ast.iter_mut() {
                resolver.resolve_statement(&mut statement)?;
            }
        }

        prev_resolved = now_resolved;
        now_resolved = scopes.resolved() + program.modules().flat_map(|m| m.statements()).fold(Progress::zero(), |acc, statement| acc + statement.num_resolved());

        if !now_resolved.done() {
            if now_resolved == prev_resolved {
                if !stage.must_resolve() {
                    stage.next();
                } else {
                    return ice("Unresolved types remaining but no errors were triggered in error run");
                }
            } else {
                stage.reset();
            }
        } else if now_resolved.done() {
            break;
        }
    }

    // find entry module (empty path) and main function within
    let entry_scope_id = program.modules().find(|&m| m.path == "").unwrap().scope_id.unwrap();
    let entry_fn = scopes.lookup_function_id(entry_scope_id, (entry_function, TypeId::VOID))
        .unwrap_or_err(None, ResolveErrorKind::UndefinedFunction(entry_function.to_string()), "")?;

    Ok(ResolvedProgram {
        ty              : PhantomData,
        modules         : program.0,
        entry_fn        : entry_fn,
        resolved        : scopes.into(),
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

    /// Try to create concrete array builtin function signature for the given array type
    fn try_create_array_builtin(self: &mut Self, name: &str, type_id: TypeId) -> ResolveResult<Option<FunctionId>> {
        let array_ty = self.type_by_id(type_id).as_array().unwrap_or_ice(ICE)?;
        if let &Array { type_id: element_type_id @ Some(_) } = array_ty {
            Ok(match builtin_types::Array::resolve(self, name, type_id, element_type_id) {
                None => None,
                Some((builtin_type, result_type_id, arg_type_ids)) => {
                    Some(self.scopes.insert_function(
                        ScopeId::ROOT,
                        name,
                        Some(result_type_id),
                        arg_type_ids.iter().map(|id| Some(*id)).collect::<Vec<Option<TypeId>>>(),
                        Some(FunctionKind::Builtin(type_id, builtin_type))
                    ))
                }
            })
        } else {
            Ok(None)
        }
    }

    /// Create integer/float/string builtin function signature.
    fn try_create_scalar_builtin(self: &mut Self, name: &str, type_id: TypeId) -> ResolveResult<Option<FunctionId>> {

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
                    ScopeId::ROOT,
                    name,
                    Some(result_type_id),
                    arg_type_ids.iter().map(|id| Some(*id)).collect::<Vec<Option<TypeId>>>(),
                    Some(FunctionKind::Builtin(type_id, builtin_type))
                ))
            }
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
        self.primitives.get(&ty).cloned().unwrap_or_ice(ICE)
    }

    /// Returns Err if item and expected type are resolved but do not match as well as if must_resolve is set and item is not resolved.
    fn resolved_or_err(self: &Self, item: &(impl Typeable+Positioned+Resolvable), expected_result: Option<TypeId>) -> ResolveResult {
        if self.stage.must_resolve() {
            if item.is_resolved() {
                match expected_result {
                    Some(expected_result) => self.check_type_accepted_for(item, item.type_id(self).unwrap(), expected_result),
                    None => Ok(())
                }
            } else {
                Err(ResolveError::new(item, item.unresolved_error(), self.module_path))
            }
        } else {
            match (item.type_id(self), expected_result) {
                (Some(item_type_id), Some(expected_result)) => self.check_type_accepted_for(item, item_type_id, expected_result),
                _ => Ok(())
            }
        }
    }

    /// Sets type if for given AST item if it is not set yet, otherwise checks that new type equals existing type.
    fn set_type_id(self: &mut Self, item: &mut (impl Typeable+Positioned), new_type_id: TypeId) -> ResolveResult {
        if let Some(item_type_id) = item.type_id(self) {
            self.check_type_equals(item, item_type_id, new_type_id)?;
        }
        *item.type_id_mut(self) = Some(new_type_id);
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
            Some(type_id) => Some(self.type_by_id(type_id))
        }
    }

    /// Returns an absolute or relative path string (depending on stage) built from current module and additional path segments.
    fn make_path<T: AsRef<str>>(self: &Self, parts: &[ T ]) -> String {
        if self.module_path == "" || !self.stage.absolute_paths() {
            parts_to_path(&parts)
        } else {
            self.module_path.to_string() + "::" + &parts_to_path(&parts)
        }
    }

    /// Returns absolute path string built from current module path and additional path segments.
    fn abs_path<T: AsRef<str>>(self: &Self, parts: &[ T ]) -> String {
        if self.module_path == "" {
            parts_to_path(&parts)
        } else {
            self.module_path.to_string() + "::" + &parts_to_path(&parts)
        }
    }
}

/// Methods to resolve individual AST structures.
impl<'ast, 'ctx> Resolver<'ctx> where 'ast: 'ctx {

    /// Resolves types and bindings used in a statement.
    fn resolve_statement(self: &mut Self, item: &mut ast::Statement) -> ResolveResult  {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.resolve_function(function, None),
            S::StructDef(structure)     => self.resolve_struct_def(structure),
            S::ImplBlock(impl_block)     => self.resolve_impl_block(impl_block),
            S::TraitDef(trait_def)     => self.resolve_trait_def(trait_def),
            S::LetBinding(binding)         => self.resolve_let_binding(binding),
            S::IfBlock(if_block)        => self.resolve_if_block(if_block, None), // accept any type for these, result is discarded
            S::ForLoop(for_loop)        => self.resolve_for_loop(for_loop),
            S::WhileLoop(while_loop)    => self.resolve_while_loop(while_loop),
            S::Block(block)             => self.resolve_block(block, None),
            S::Return(ret)              => self.resolve_return(ret),
            S::Expression(expression)   => self.resolve_expression(expression, None),
            S::Use(use_declaration)                => self.resolve_use_declaration(use_declaration),
            S::EnumDef(enum_def)        => self.resolve_enum_def(enum_def),
            S::Module(_) | S::Break(_) | S::Continue(_) => { Ok(()) /* nothing to do here */ },
        }
    }

    /// Resolves use declarations.
    fn resolve_use_declaration(self: &mut Self, item: &mut ast::Use) -> ResolveResult  {
        let mut unresolved = None;
        for (name, (path, resolved)) in &mut item.mapping.iter_mut().filter(|(_, (_, r))| !r) {
            if self.module_paths.contains(path) {
                *resolved = true;
                return Err(ResolveError::new(item, ResolveErrorKind::Unsupported("importing entire module not yet implemented".to_string()), self.module_path));
            } else if let Some(type_id) = self.scopes.lookup_type_id(self.scope_id, path) {
                self.scopes.alias_type(self.scope_id, name, type_id); // TODO should probably be prefixed with module name
                *resolved = true;
            } else if let Some(function_id) = self.scopes.lookup_function_id(self.scope_id, (path, TypeId::VOID)) {
                self.scopes.alias_function(self.scope_id, name, function_id); // TODO should probably be prefixed with module name
                *resolved = true;
            } else if unresolved == None {
                unresolved = Some(path.clone());
            }
        }
        if !self.stage.must_resolve() || unresolved.is_none() {
            Ok(())
        } else {
            Err(ResolveError::new(item, ResolveErrorKind::UndefinedItem(unresolved.unwrap()), self.module_path))
        }
    }

    /// Resolves types and bindings used in an expression.
    fn resolve_expression(self: &mut Self, item: &mut ast::Expression, expected_result: Option<TypeId>) -> ResolveResult {
        use ast::Expression as E;
        match item { // todo: these all need to check expected_result since the caller might depend on an error result on type mismatch
            E::Literal(literal)         => self.resolve_literal(literal, expected_result),
            E::Value(value) => match value {
                ast::Value::Variable(variable) => self.resolve_variable(variable, expected_result),
                ast::Value::Constant(constant) => self.resolve_constant(constant, expected_result),
                ast::Value::Unknown { position, ident } => {
                    if self.scopes.constant_id(self.scope_id, &ident.name).is_some() {
                        let mut constant = ast::Constant {
                            position: *position,
                            ident: ident.clone(),
                            constant_id: None,
                        };
                        self.resolve_constant(&mut constant, expected_result)?;
                        *item = E::Value(ast::Value::Constant(constant));
                    } else if self.scopes.lookup_binding_id(self.scope_id, &ident.name).is_some() {
                        let mut variable = ast::Variable {
                            position: *position,
                            ident: ident.clone(),
                            binding_id: None,
                        };
                        self.resolve_variable(&mut variable, expected_result)?;
                        *item = E::Value(ast::Value::Variable(variable));
                    }
                    Ok(())
                },
            },
            E::Call(call)               => self.resolve_call(call, expected_result),
            E::Member(_)                => { Ok(()) /* nothing to do here */ },
            E::Assignment(assignment)   => self.resolve_assignment(assignment),
            E::BinaryOp(binary_op)      => self.resolve_binary_op(binary_op, expected_result),
            E::UnaryOp(unary_op)        => self.resolve_unary_op(unary_op, expected_result),
            E::Cast(cast)               => self.resolve_cast(cast, expected_result),
            E::Block(block)             => self.resolve_block(block, expected_result),
            E::IfBlock(if_block)        => self.resolve_if_block(if_block, expected_result),
            E::MatchBlock(match_block)        => self.resolve_match_block(match_block, expected_result),
            E::Closure(_closure)        => unimplemented!("Closure resolution todo"),
        }
    }

    // Resolves an inline type definition.
    fn resolve_inline_type(self: &mut Self, item: &mut ast::InlineType) -> ResolveResult<Option<TypeId>> {
        match item {
            ast::InlineType::TypeName(type_name) => self.resolve_type_name(type_name, None), // todo: not sure about this one. inline-type is defining, so if it differs, the other side should be wrong
            ast::InlineType::Array(array) => self.resolve_array(array),
        }
    }

    /// Resolves a type (name) to a type_id.
    fn resolve_type_name(self: &mut Self, item: &mut ast::TypeName, expected_result: Option<TypeId>) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_none() {
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &self.make_path(&item.path.name)) {
                item.type_id = Some(new_type_id);
            }
        }
        if let (Some(item_type_id), Some(expected_result)) = (item.type_id, expected_result) {
            self.check_type_accepted_for(item, item_type_id, expected_result)?;
        }
        self.resolved_or_err(item, expected_result)?;
        Ok(item.type_id)
    }

    /// Resolves an array definition
    fn resolve_array(self: &mut Self, item: &mut ast::Array) -> ResolveResult<Option<TypeId>> {
        let inner_type_id = self.resolve_inline_type(&mut item.element_type)?;
        if item.type_id.is_none() && inner_type_id.is_some() {
            let ty = Type::Array(Array {
                type_id : inner_type_id,
            });
            let new_type_id = self.scopes.insert_anonymous_type(false, ty);
            item.type_id = Some(new_type_id);
        }
        self.resolved_or_err(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves a struct definition.
    fn resolve_struct_def(self: &mut Self, item: &mut ast::StructDef) -> ResolveResult {
        if item.is_resolved() {
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
            self.type_by_id_mut(type_id).as_struct_mut().unwrap().fields = fields;
        } else {
            let qualified = self.abs_path(&[ &item.ident.name ]);
            let type_id = self.scopes.insert_type(self.scope_id, Some(&qualified), Type::Struct(Struct { fields: fields, impl_traits: Map::new() }));
            item.type_id = Some(type_id);
            if item.vis == Visibility::Public {
                self.scopes.alias_type(ScopeId::ROOT, &qualified, type_id);
            }
        }
        self.resolved_or_err(item, None)
    }

    /// Resolves an enum definition.
    fn resolve_enum_def(self: &mut Self, item: &mut ast::EnumDef) -> ResolveResult {
        if item.is_resolved() {
            return Ok(());
        }

        // check whether at least one enum variant specifies a type, default to i32 if none do
        let simple_type_id = if let Some(named_type) = item.named_type() {
            named_type.type_id.or(self.scopes.lookup_type_id(self.scope_id, &self.make_path(&named_type.path.name))).unwrap_or_ice("Invalid variant type")?
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
            self.type_by_id_mut(type_id).as_enum_mut().unwrap().variants = variants;
        } else {
            let qualified = self.abs_path(&[ &item.ident.name ]);
            let primitive = if item.is_primitive() {
                Some((simple_type_id, self.type_by_id(simple_type_id).primitive_size()))
            } else {
                None
            };
            let type_id = self.scopes.insert_type(self.scope_id, Some(&qualified), Type::Enum(Enum { primitive, variants, impl_traits: Map::new() }));
            item.type_id = Some(type_id);
            if item.vis == Visibility::Public {
                self.scopes.alias_type(ScopeId::ROOT, &qualified, type_id);
            }
        }
        // create variant constructors
        //let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        for (index, variant) in item.variants.iter_mut().enumerate() {
            if variant.is_resolved() {
                match &mut variant.kind {
                    ast::VariantKind::Data(function_id @ None, fields) => {
                        let arg_type_ids: Vec<_> = fields.iter().map(|field| field.type_id(self)).collect::<Vec<_>>();
                        let path = self.abs_path(&[ &item.ident.name, &variant.ident.name ]);
                        let kind = FunctionKind::Variant(item.type_id.unwrap(), index as VariantIndex);
                        *function_id = Some(self.scopes.insert_function(self.scope_id, &path, item.type_id, arg_type_ids, Some(kind)));
                    },
                    _ => { },
                }
            }
        }
        //self.scope_id = parent_scope_id;
        self.resolved_or_err(item, None)
    }

    /// Resolves a struct definition.
    fn resolve_impl_block(self: &mut Self, item: &mut ast::ImplBlock) -> ResolveResult {
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
            let parent_scope_id = self.try_create_scope(&mut item.scope_id);
            if self.scopes.local_type_id(self.scope_id, "Self").is_none() {
                self.scopes.alias_type(self.scope_id, "Self", type_id);
            }
            for function in &mut item.functions {
                self.resolve_function(function, Some((parent_scope_id, type_id)))?;
                // if this is a trait impl and the trait is resolved
                if let Some(trait_type_id) = trait_type_id {
                    if let Some(trait_type_id) = trait_type_id {
                        let trt = self.type_by_id(trait_type_id).as_trait().unwrap();
                        let function_id = function.function_id;
                        let function_name = &function.sig.ident.name;
                        // check if function is defined in trait
                        if trt.provided.get(function_name).is_none() && trt.required.get(function_name).is_none() {
                            let trait_name = format!("{}", &item.trt.as_ref().unwrap().path);
                            return Err(ResolveError::new(function, ResolveErrorKind::NotATraitMethod(function_name.clone(), trait_name), self.module_path));
                        }
                        if let Some(struct_) = self.type_by_id_mut(type_id).as_struct_mut() {
                            let impl_trait = struct_.impl_traits.entry(trait_type_id).or_insert(ImplTrait::new());
                            impl_trait.functions.insert(function_name.clone(), function_id);
                        }
                    }
                }
            }
            self.scope_id = parent_scope_id;
        }
        Ok(())
    }

    fn resolve_trait_def(self: &mut Self, item: &mut ast::TraitDef) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        // ensure trait exists
        if item.type_id.is_none() {
            let mut trt = Trait { provided: Map::new(), required: Map::new() };
            for function in &mut item.functions {
                let name = function.sig.ident.name.clone();
                match function.block {
                    Some(_) => trt.provided.insert(name, None),
                    None => trt.required.insert(name, None),
                };
            }
            let qualified = self.abs_path(&[ &item.ident.name ]);
            let type_id = self.scopes.insert_type(self.scope_id, Some(&qualified), Type::Trait(trt));
            item.type_id = Some(type_id);
            // create aliases
            self.scopes.alias_type(self.scope_id, "Self", type_id);
            if item.vis == Visibility::Public {
                self.scopes.alias_type(ScopeId::ROOT, &qualified, type_id);
            }
        }
        // try to resolve functions
        for function in &mut item.functions {
            self.resolve_function(function, Some((parent_scope_id, item.type_id.unwrap())))?;
        }
        // update trait
        let trt = self.type_by_id_mut(item.type_id.unwrap()).as_trait_mut().unwrap();
        for function in &item.functions {
            if function.is_resolved() {
                let name = &function.sig.ident.name;
                match function.block {
                    Some(_) => *trt.provided.get_mut(name).unwrap() = function.function_id,
                    None => *trt.required.get_mut(name).unwrap() = function.function_id,
                };
            }
        }
        self.scope_id = parent_scope_id;
        self.resolved_or_err(item, None)
    }

    /// Resolves a function signature.
    fn resolve_signature(self: &mut Self, item: &mut ast::Signature) -> ResolveResult {
        // resolve arguments
        for arg in item.args.iter_mut() {
            self.resolve_let_binding(arg)?; // checks resolved_or_err
        }
        // resolve return type
        if let Some(ret) = &mut item.ret {
            self.resolve_inline_type(ret)?; // checks resolved_or_err
        }
        Ok(())
    }

    /// Resolves a function defintion.
    fn resolve_function(self: &mut Self, item: &mut ast::Function, struct_scope: Option<(ScopeId, TypeId)>) -> ResolveResult {

        let parent_scope_id = self.try_create_scope(&mut item.scope_id);

        self.resolve_signature(&mut item.sig)?;

        //let signature_scope_id = self.try_create_scope(&mut item.scope_id); // todo: put body into separate scope so signature can't access body
        if item.function_id.is_none() && item.sig.ret_resolved(self) && item.sig.args_resolved(self) {
            let result_type_id = item.sig.ret_type_id(self);
            let arg_type_ids: Vec<_> = item.sig.arg_type_ids(self);
            // function/method switch
            let (target_scope_id, function_kind, qualified) = if let Some(scope) = struct_scope {
                let type_name = self.scopes.type_name(scope.1).unwrap();
                let path = self.abs_path(&[ type_name, &item.sig.ident.name ]);
                if item.sig.args.len() == 0 || item.sig.args[0].ident.name != "self" {
                    (scope.0, FunctionKind::Function, path)
                } else {
                    (scope.0, FunctionKind::Method(scope.1), path)
                }
            } else {
                let path = self.abs_path(&[ &item.sig.ident.name ]);
                (parent_scope_id, FunctionKind::Function, path)
            };

            let function_id = self.scopes.insert_function(target_scope_id, &qualified, result_type_id, arg_type_ids, Some(function_kind));
            if item.sig.vis == Visibility::Public {
                self.scopes.alias_function(ScopeId::ROOT, &qualified, function_id);
            }
            item.function_id = Some(function_id);
            self.scopes.set_scopefunction_id(self.scope_id, function_id);
        }
        if let Some(function_id) = item.function_id {
            let ret_type = self.scopes.function_ref(function_id).ret_type_id;
            if let Some(block) = &mut item.block {
                self.resolve_block(block, ret_type)?;
            }
        }
        self.scope_id = parent_scope_id;
        if self.stage.must_resolve() && (!item.sig.args_resolved(self) || !item.sig.ret_resolved(self)) {
            Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(format!("signature for '{}'", item.sig.ident.name)), self.module_path))
        } else {
            Ok(())
        }
    }

    /// Resolves a return statement.
    fn resolve_return(self: &mut Self, item: &mut ast::Return) -> ResolveResult {
        let function_id = self.scopes.lookup_scopefunction_id(self.scope_id).unwrap_or_err(Some(item), ResolveErrorKind::InvalidOperation("Use of return outside of function".to_string()), self.module_path)?;
        let ret_type_id = self.scopes.function_ref(function_id).ret_type_id;
        self.resolved_or_err(&mut item.expr, None)?;
        self.resolve_expression(&mut item.expr, ret_type_id)?;
        // check return type matches function result type
        self.resolved_or_err(&mut item.expr, ret_type_id)
    }

    fn resolve_call_method(self: &mut Self, item: &mut ast::Call, _expected_result: Option<TypeId>) -> ResolveResult {
        let arg = item.args.get_mut(0).unwrap_or_ice(ICE)?;
        self.resolve_expression(arg, None)?;
        if let Some(type_id) = arg.type_id(self) {
            let ty = self.type_by_id(type_id);
            if ty.as_array().is_some() {
                item.function_id = self.scopes.lookup_function_id(self.scope_id, (&item.ident.name, type_id));
                if item.function_id.is_none() {
                    item.function_id = self.try_create_array_builtin(&item.ident.name, type_id)?;
                }
            } else if ty.is_float() || ty.is_integer() || ty.is_string() {
                item.function_id = self.scopes.lookup_function_id(self.scope_id, (&item.ident.name, type_id));
                if item.function_id.is_none() {
                    item.function_id = self.try_create_scalar_builtin(&item.ident.name, type_id)?;
                }
            } else {
                // try method on type
                let type_name = self.scopes.type_name(type_id).unwrap_or_ice("Unnamed type")?;
                let path = self.make_path(&[ type_name, &item.ident.name ]);
                item.function_id = self.scopes.lookup_function_id(self.scope_id, (&path, type_id));
                // try trait default implementations
                if item.function_id.is_none() {
                    let function_id = self.scopes.trait_function_id(self.scope_id, &item.ident.name, type_id);
                    if function_id.is_some() {
                        item.function_id = function_id;
                    }
                }
            }
        }
        Ok(())
    }

    /// Resolves an occurance of a function call.
    fn resolve_call(self: &mut Self, item: &mut ast::Call, expected_result: Option<TypeId>) -> ResolveResult {
        // locate function definition
        if item.function_id.is_none() {
            let path;
            match &item.call_syntax {
                CallSyntax::Method => {
                    let arg = item.args.get_mut(0).unwrap_or_ice(ICE)?;
                    let type_name = if let Some(type_id) = arg.type_id(self) {
                        self.scopes.type_name(type_id).map_or(format!("{}::{}", self.type_by_id(type_id), &item.ident.name), |t| t.to_string())
                    } else {
                        "<unknown>".to_string()
                    };
                    path = self.make_path(&[ type_name, item.ident.name.clone() ]);// TODO: this should be lazy on error
                    self.resolve_call_method(item, expected_result)?;
                },
                CallSyntax::Ident => {
                    path = self.make_path(&[ &item.ident.name ]);
                    item.function_id = self.scopes.lookup_function_id(self.scope_id, (&path, TypeId::VOID));
                },
                CallSyntax::Path(static_path) => {
                    path = self.make_path(&[ &parts_to_path(&static_path.name), &item.ident.name ]);
                    // todo fix this hack: path handling needs a rework
                    let if_let_chain_where_are_you_type_id = self.scopes.local_type_id(ScopeId::ROOT, &static_path.name[0].name);
                    if static_path.name.len() == 1 && if_let_chain_where_are_you_type_id.is_some() {
                        item.function_id = self.try_create_scalar_builtin(&item.ident.name, if_let_chain_where_are_you_type_id.unwrap())?;
                    } else {
                        item.function_id = self.scopes.lookup_function_id(self.scope_id, (&path, TypeId::VOID));
                    }
                },
            }
            if item.function_id.is_none() && self.stage.must_resolve() {
                return Err(ResolveError::new(item, ResolveErrorKind::UndefinedFunction(path), self.module_path));
            }
        }

        // found a function, resolve return type and arguments
        if let Some(function_id) = item.function_id {

            // return value
            let function_info = self.scopes.function_ref(function_id).clone();

            if let Some(ret_type_id) = function_info.ret_type_id {
                self.set_type_id(item, ret_type_id)?;
            }

            if let (Some(item_type_id), Some(expected_result)) = (item.type_id(self), expected_result) {
                self.check_type_accepted_for(item, item_type_id, expected_result)?;
            }

            // argument count
            if function_info.arg_type_ids.len() != item.args.len() {
                return Err(ResolveError::new(item, ResolveErrorKind::NumberOfArguments(item.ident.name.clone(), function_info.arg_type_ids.len() as ItemIndex, item.args.len() as ItemIndex), self.module_path));
            }

            // arguments
            for (index, &expected_type_id) in function_info.arg_type_ids.iter().enumerate() {
                self.resolve_expression(&mut item.args[index], expected_type_id)?;
                let actual_type_id = item.args[index].type_id(self);
                // infer arguments
                if actual_type_id.is_none() && expected_type_id.is_some() {
                    *item.args[index].type_id_mut(self) = expected_type_id;
                } else if let (Some(actual_type_id), Some(expected_type_id)) = (actual_type_id, expected_type_id) {
                    self.check_type_accepted_for(&item.args[index], actual_type_id, expected_type_id)?;
                }
            }
        }

        self.resolved_or_err(item, expected_result)
    }

    /// Resolves a simple enum variant. (Data variants are handled by resolve_call.)
    fn resolve_variant_literal(self: &mut Self, item: &mut ast::Literal, expected_type: Option<TypeId>) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_none() {
            let variant = match &item.value {
                LiteralValue::Variant(v) => v,
                _ => unreachable!("expected variant literal"),
            };
            if let Some(new_type_id) = self.scopes.lookup_type_id(self.scope_id, &self.make_path(&variant.path.name)) {
                item.type_id = Some(new_type_id);
            }
        }
        if let (Some(item_type_id), Some(expected_type)) = (item.type_id, expected_type) {
            self.check_type_accepted_for(item, item_type_id, expected_type)?;
        }
        self.resolved_or_err(item, expected_type)?;
        Ok(item.type_id)
    }

    /// Resolves a variable name to a variable.
    fn resolve_variable(self: &mut Self, item: &mut ast::Variable, expected_result: Option<TypeId>) -> ResolveResult {
        // resolve binding
        if item.binding_id.is_none() {
            item.binding_id = self.scopes.lookup_binding_id(self.scope_id, &item.ident.name);
            if item.binding_id.is_none() {
                return Err(ResolveError::new(item, ResolveErrorKind::UndefinedVariable(item.ident.name.to_string()), self.module_path));
            }
        }
        // set expected type, if any
        if item.type_id(self).is_none() {
            if let Some(expected_result) = expected_result {
                if self.stage.infer_as_concrete() || !self.type_by_id(expected_result).as_trait().is_some() {
                    self.set_type_id(item, expected_result)?;
                }
            }
        }
        self.resolved_or_err(item, expected_result)
    }

    /// Resolves a constant name to a constant.
    fn resolve_constant(self: &mut Self, item: &mut ast::Constant, expected_result: Option<TypeId>) -> ResolveResult {
        // TODO Resolve constants
        /*if item.binding_id.is_none() {
            item.binding_id = self.scopes.lookup_binding_id(self.scope_id, &item.ident.name);
            if item.binding_id.is_none() {
                return Err(ResolveError::new(item, ResolveErrorKind::UndefinedVariable(item.ident.name.to_string()), self.module_path));
            }
        }*/
        // set expected type, if any
        if item.type_id(self).is_none() {
            if let Some(expected_result) = expected_result {
                if self.stage.infer_as_concrete() || !self.type_by_id(expected_result).as_trait().is_some() {
                    self.set_type_id(item, expected_result)?;
                }
            }
        }
        self.resolved_or_err(item, expected_result)
    }

    /// Resolves an if block.
    fn resolve_if_block(self: &mut Self, item: &mut ast::IfBlock, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
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
        Ok(())
    }

    /// Resolves a match expression.
    fn resolve_match_block(self: &mut Self, item: &mut ast::MatchBlock, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        self.resolve_expression(&mut item.expr, None)?;
        for (_, block) in &mut item.branches {
            self.resolve_block(block, expected_result)?;
        }
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a for loop.
    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop) -> ResolveResult {
        use ast::{Expression, BinaryOperator as Op};
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        // create binding for the iterator variable
        self.resolve_let_binding(&mut item.iter)?;
        match &item.expr { // NOTE: these need to match Compiler::compile_for_loop
            Expression::BinaryOp(bo) if bo.op == Op::Range || bo.op == Op::RangeInclusive => {
                let type_id = item.iter.type_id(self);
                self.resolve_expression(&mut item.expr, type_id)?;
                if let Some(type_id) = item.expr.type_id(self) {
                    self.set_type_id(&mut item.iter, type_id)?;
                }
            },
            Expression::Block(_) | Expression::Call(_) | Expression::IfBlock(_) | Expression::Literal(_) | Expression::Value(_) => {
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
                    let type_name = self.type_name(item.expr.type_id(self).unwrap());
                    return Err(ResolveError::new(&item.expr, ResolveErrorKind::NotIterable(type_name), self.module_path));
                }
            },
            _ => return Err(ResolveError::new(&item.iter, ResolveErrorKind::InvalidOperation("Unsupported for in operand".to_string()), self.module_path)),
        };
        // handle block
        self.resolve_block(&mut item.block, Some(self.primitive_type_id(Type::void)?))?;
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a while loop.
    fn resolve_while_loop(self: &mut Self, item: &mut ast::WhileLoop) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        self.resolve_expression(&mut item.expr, None)?;
        self.resolve_block(&mut item.block, None)?;
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves a block.
    fn resolve_block(self: &mut Self, item: &mut ast::Block, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.try_create_scope(&mut item.scope_id);
        // resolve statments, result and returns
        for statement in item.statements.iter_mut() {
            self.resolve_statement(statement)?;
        }
        if let Some(ref mut result) = item.result {
            self.resolve_expression(result, expected_result)?;
        }
        self.scope_id = parent_scope_id;
        Ok(())
    }

    /// Resolves an assignment expression.
    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment) -> ResolveResult {
        let right_type_id = item.right.type_id(self);
        self.resolve_expression(&mut item.left, right_type_id)?;
        let left_type_id = item.left.type_id(self);
        self.resolve_expression(&mut item.right, left_type_id)?;
        item.type_id = Some(TypeId::VOID);
        Ok(())
    }

    /// Resolves an assignment expression.
    fn resolve_cast(self: &mut Self, item: &mut ast::Cast, expected_result: Option<TypeId>) -> ResolveResult {
        self.resolve_type_name(&mut item.ty/*, 0*/, expected_result)?; // FIXME hardcoded ref arg
        self.resolve_expression(&mut item.expr, None)?;
        if let Some(type_id) = item.ty.type_id {
            *item.type_id_mut(self) = Some(type_id);
        }
        if let (Some(from_type_id), Some(to_type_id)) = (item.expr.type_id(self), item.ty.type_id) {
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
                    let from_name = self.scopes.type_name(from_type_id).unwrap_or(&format!("<{}>", from_type)).clone();
                    let to_name = self.scopes.type_name(to_type_id).unwrap_or(&format!("<{}>", to_type)).clone();
                    return Err(ResolveError::new(item, ResolveErrorKind::InvalidCast(from_name, to_name), self.module_path));
                }
            }
        }

        Ok(())
    }

    /// Resolves a binary operation.
    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::BinaryOperator as O;

        match item.op {
            O::And | O::Or => {
                self.resolve_expression(&mut item.left, Some(self.primitive_type_id(Type::bool)?))?;
                self.resolve_expression(&mut item.right, Some(self.primitive_type_id(Type::bool)?))?;
                *item.type_id_mut(self) = Some(self.primitive_type_id(Type::bool)?);
            }
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual => {
                self.resolve_expression(&mut item.left, None)?;
                let left_type_id = item.left.type_id(self);
                self.resolve_expression(&mut item.right, left_type_id)?;
                let right_type_id = item.right.type_id(self);
                *item.type_id_mut(self) = Some(self.primitive_type_id(Type::bool)?);
                if let Some(common_type_id) = left_type_id.or(right_type_id) {
                    self.set_type_id(&mut item.left, common_type_id)?;
                    self.set_type_id(&mut item.right, common_type_id)?;
                }
            }
            O::Add | O::Sub | O::Mul | O::Div | O::Rem | O::Range | O::RangeInclusive => {
                self.resolve_expression(&mut item.left, expected_result)?;
                let left_type_id = item.left.type_id(self);
                self.resolve_expression(&mut item.right, left_type_id)?;
                let type_id = item.type_id(self);
                let right_type_id = item.right.type_id(self);
                if let Some(common_type_id) = type_id.or(left_type_id).or(right_type_id) {
                    self.set_type_id(item, common_type_id)?;
                    self.set_type_id(&mut item.left, common_type_id)?;
                    self.set_type_id(&mut item.right, common_type_id)?;
                }
            }
            O::Index | O::IndexWrite => {
                self.resolve_expression(&mut item.left, None)?;
                self.resolve_expression(&mut item.right, Some(self.primitive_type_id(STACK_ADDRESS_TYPE)?))?;
                // left[right] : item
                self.set_type_id(&mut item.right, self.primitive_type_id(STACK_ADDRESS_TYPE)?)?;
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
            }
            O::Access | O::AccessWrite => {
                self.resolve_expression(&mut item.left, None)?;
                self.resolve_expression(&mut item.right, None)?;
                // left.right : item
                if let Some(ty) = self.item_type(&item.left) {
                    let struct_ = ty.as_struct().unwrap_or_err(Some(item), ResolveErrorKind::InvalidOperation("Member access on a non-struct".to_string()), self.module_path)?;
                    let field = item.right.as_member_mut().unwrap_or_ice("Member access using a non-field")?;
                    let field_type_id = *struct_.fields.get(&field.ident.name).unwrap_or_err(Some(field), ResolveErrorKind::UndefinedMember(field.ident.name.clone()), self.module_path)?;
                    if let Some(field_type_id) = field_type_id {
                        self.set_type_id(item, field_type_id)?;
                        self.set_type_id(&mut item.right, field_type_id)?;
                    }
                }
            }
            O::Assign | O::AddAssign | O::SubAssign | O::MulAssign | O::DivAssign | O::RemAssign => {
                return ice("Unexpected operator in resolve_binary_op");
            }
        }

        Ok(())
    }

    /// Resolves a binding created by let, for or a signature.
    fn resolve_let_binding(self: &mut Self, item: &mut ast::LetBinding) -> ResolveResult {

        // create binding id if we don't have one yet
        if item.binding_id.is_none() {
            // this binding ast node wasn't processed yet. if the binding name already exists we're shadowing - which is NYI
            if self.scopes.local_binding_id(self.scope_id, &item.ident.name).is_some() {
                unimplemented!("cannot shadow {}", item.ident.name); // todo: support shadowing
            }
            let binding_id = self.scopes.insert_binding(self.scope_id, Some(&item.ident.name), item.mutable, None);
            item.binding_id = Some(binding_id);
        }

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
            if let Binding { type_id: None, .. } = self.binding_by_id(item.binding_id.unwrap()) {
                return Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(item.ident.to_string()), self.module_path))
            }
        }

        self.resolved_or_err(item, None)
    }

    /// Resolves a unary operation.
    fn resolve_unary_op(self: &mut Self, item: &mut ast::UnaryOp, expected_type: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::UnaryOperator as UO;
        self.resolve_expression(&mut item.expr, expected_type)?;
        match item.op {
            UO::Not => {
                if let Some(expected_type_id) = expected_type {
                    self.check_type_accepted_for(item, self.primitive_type_id(Type::bool)?, expected_type_id)?;
                }
                self.set_type_id(item, self.primitive_type_id(Type::bool)?)?;
            },
            UO::Plus | UO::Minus => {
                if let Some(type_id) = item.expr.type_id(self) {
                    self.set_type_id(item, type_id)?;
                }
            },
        }
        self.resolved_or_err(&item.expr, None)?;
        self.resolved_or_err(item, expected_type)
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
        } else if let LV::Variant(_) = item.value {
            self.resolve_variant_literal(item, expected_type)?; // handles simple variants only. data variants are parsed as calls.
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
                return Err(ResolveError::new(item, ResolveErrorKind::IncompatibleNumeric(format!("{}", ty), numeric), self.module_path));
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
        self.resolved_or_err(item, expected_type)
    }

    /// Resolves an struct literal and creates the required field types.
    fn resolve_struct_literal(self: &mut Self, item: &mut ast::Literal) -> ResolveResult {

        // resolve type from name

        let type_name = item.type_name.as_mut().unwrap_or_ice(ICE)?;
        let type_id = self.resolve_type_name(type_name, None)?;

        // resolve fields from field definition

        if let Some(type_id) = type_id {
            self.set_type_id(item, type_id)?;
            let struct_def = self.type_by_id(type_id).as_struct().unwrap_or_err(Some(item), ResolveErrorKind::Internal("Tried to resolve a struct but got different type".to_string()), self.module_path)?.clone();
            let struct_ = item.value.as_struct_mut().unwrap_or_ice(ICE)?;
            for (name, field) in &mut struct_.fields {
                if !struct_def.has_field(name) {
                    return Err(ResolveError::new(field, ResolveErrorKind::UndefinedMember(name.clone()), self.module_path));
                }
                self.resolve_expression(field, struct_def.type_id(name))?;
                self.resolved_or_err(field, None)?;
            }
        }

        self.resolved_or_err(item, None)
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
        let array_literal = item.value.as_array_mut().unwrap_or_ice("Expected array type, got something else")?;

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
            let new_type_id = self.scopes.insert_anonymous_type(false, Type::Array(Array {
                type_id : elements_type_id,
            }));
            *item.type_id_mut(self) = Some(new_type_id);
        } else if let Some(elements_type_id) = elements_type_id {
            let array_type_id = item.type_id(self).unwrap_or_ice(ICE)?;
            let array_ty = self.type_by_id_mut(array_type_id).as_array_mut().unwrap_or_ice(ICE)?;
            if let Some(current_element_type_id) = array_ty.type_id {
                self.check_type_accepted_for(item, current_element_type_id, elements_type_id)?;
            } else {
                array_ty.type_id = Some(elements_type_id);
            }
        }

        self.resolved_or_err(item, expected_type_id)
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
        self.scopes.type_name(type_id)
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