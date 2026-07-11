//! AST type checker and resolver.

#[path="scopes/scopes.rs"]
mod scopes;
mod stage;
mod exhaustiveness;
mod apinamespace;
mod intrinsic;
mod type_synthesis;
mod const_eval;
mod binary_op;
pub mod error;
pub mod resolved;

use crate::{prelude::*, VariantIndex};
use crate::{ItemIndex, STACK_ADDRESS_TYPE};
use crate::frontend::parser::types::ParsedProgram;
use crate::frontend::ast::{self, Visibility, Positioned, Typeable, Resolvable, ControlFlow};
use crate::frontend::resolver::error::{OptionToResolveError, ResolveResult, ResolveError, ResolveErrorKind};
use crate::frontend::resolver::resolved::ResolvedProgram;
use crate::shared::{Progress, MetaContainer, parts_to_path, path_to_parts};
use crate::shared::meta::{Array, MapType, Struct, Enum, EnumVariant, Trait, ImplTrait, Type, FunctionKind, Binding, Constant, ConstantValue, UserConstValue, Callable, Function};
use crate::shared::typed_ids::{BindingId, ScopeId, TypeId, ConstantId, FunctionId};
use crate::shared::numeric::Numeric;
use crate::bytecode::{VMFunc, builtins::builtin_types};
use crate::frontend::resolver::intrinsic::{IntrinsicCast, IntrinsicOp, IntrinsicUnaryOp, IntrinsicIndex, OrderingInfo, register_intrinsics};
use crate::frontend::resolver::type_synthesis::{ResultTypeInfo, OptionTypeInfo};

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
    /// Maps a cast target type id to the intrinsic conversion trait that backs it (e.g. `String` ->
    /// the `ToString` trait/`to_string`).
    intrinsic_casts : &'ctx UnorderedMap<TypeId, IntrinsicCast>,
    /// Maps a binary operator to the intrinsic operator trait that backs it.
    intrinsic_ops   : &'ctx UnorderedMap<ast::BinaryOperator, IntrinsicOp>,
    /// Maps a unary operator to the intrinsic operator trait that backs it (e.g. `-` -> `Neg`, `!` -> `Not`).
    intrinsic_unary_ops : &'ctx UnorderedMap<ast::UnaryOperator, IntrinsicUnaryOp>,
    /// The intrinsic `Index` trait that backs overloadable `[]` on custom types. The concrete index
    /// and value types are impl-defined and read at resolution time.
    intrinsic_index : Option<IntrinsicIndex>,
    /// Type id of the built-in `Error` trait. Fixed `Err` payload type of every synthesized `Result<T>`.
    error_trait_type_id: TypeId,
    /// Registry of synthesized `Result<T>` enums (keyed by enum type id).
    result_types    : &'ctx mut UnorderedMap<TypeId, ResultTypeInfo>,
    /// Registry of synthesized `Option<T>` enums (keyed by enum type id).
    option_types    : &'ctx mut UnorderedMap<TypeId, OptionTypeInfo>,
    /// Built-in `Ordering` enum type id and the constant ids for its three variants.
    ordering        : OrderingInfo,
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
///     fn print(&mut context, value: str) {
///         println!("print: {}", value);
///     }
///     // ... more functions ...
/// });
///
/// fn main() {
///     let mut state = parser::LinkState::new();
///     let module = parser::parse_module(&mut state, "
///         // An Itsy program that calls the Rust 'print' function.
///         fn main() {
///             MyAPI::print(\"Hello from Itsy!\");
///         }
///     ", "").unwrap();
///     let mut parsed = parser::ParsedProgram::new();
///     parsed.add_module(module);
///     parsed.set_link_state(state);
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
        replace(&mut program.binding_ids, Set::new())
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

    // Register intrinsic traits (casts, operators, Error, Index) and the built-in Ordering enum.
    let intrinsic = register_intrinsics(&mut scopes, &primitives)?;
    let intrinsic_trait_names = intrinsic.trait_names;
    let intrinsic_casts = intrinsic.casts;
    let intrinsic_ops = intrinsic.ops;
    let intrinsic_unary_ops = intrinsic.unary_ops;
    let intrinsic_index = intrinsic.index;
    let error_trait_type_id = intrinsic.error_trait;
    let ordering_info = intrinsic.ordering;

    // registry of synthesized `Result<T>` and `Option<T>` enums, populated on demand as type annotations and
    // variant constructors are resolved. Persistent across passes.
    let mut result_types = UnorderedMap::new();
    let mut option_types = UnorderedMap::new();

    // insert userdefined struct/enum types from derive-macro/itsy_api
    let ns = apinamespace::insert::<T>(&mut scopes, &primitives)?;

    // insert rust functions into root scope
    for (name, (index, ret_type, arg_types)) in T::resolve_info().into_iter() {
        let ret_type = match &ret_type {
            None => Some(*primitives.get(&Type::void).ice()?),
            Some(ret_type) => Some(apinamespace::resolve_type_id(&mut scopes, ret_type, &ns, name, "return")?),
        };
        let arg_type_id: ResolveResult<Vec<_>> = arg_types
            .iter()
            .map(|arg_type| Ok(Some(apinamespace::resolve_type_id(&mut scopes, arg_type, &ns, name, "argument")?)))
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
        // make intrinsic traits (e.g. `ToString`) visible in every module, like the primitives above
        for name in &intrinsic_trait_names {
            scopes.insert_alias(module.scope_id, name, &(module_path.clone() + name));
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
            intrinsic_casts : &intrinsic_casts,
            intrinsic_ops   : &intrinsic_ops,
            intrinsic_unary_ops : &intrinsic_unary_ops,
            intrinsic_index,
            error_trait_type_id,
            result_types    : &mut result_types,
            option_types    : &mut option_types,
            ordering        : ordering_info,
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
        now_resolved = program.modules().flat_map(|m| m.statements()).fold(Progress::zero(), |acc, statement| acc + statement.num_resolved(&scopes));

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

    // Reject recursive struct/enum types that would allow reference loops. Note: Currently these would
    // also result in infinite sized type constructors.
    for module in program.modules() {
        for statement in module.statements() {
            let (type_id, name) = match statement {
                ast::Statement::StructDef(def) => (def.type_id, &def.ident.name),
                ast::Statement::EnumDef(def) => (def.type_id, &def.ident.name),
                _ => continue,
            };
            if let Some(type_id) = type_id {
                if scopes.type_contains_self(type_id) {
                    return Err(ResolveError::new(statement, ResolveErrorKind::RecursiveType(name.clone()), &module.path));
                }
            }
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

/// General utility methods.
impl<'ctx> Resolver<'ctx> {

    /// Insert a resolved builtin function signature into the root scope.
    fn insert_builtin_fn(self: &mut Self, name: &str, type_id: TypeId, builtin_type: crate::bytecode::builtins::BuiltinType, result_type_id: TypeId, arg_type_ids: Vec<TypeId>) -> ConstantId {
        self.scopes.insert_function(
            name,
            Some(result_type_id),
            arg_type_ids.into_iter().map(Some).collect(),
            Some(FunctionKind::Builtin(type_id, builtin_type)),
        )
    }

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
                    Some(self.insert_builtin_fn(name, type_id, builtin_type, result_type_id, arg_type_ids))
                },
            })
        } else if self.stage.must_resolve() {
            Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(format!("{}", &item)), self.module_path))
        } else {
            Ok(None)
        }
    }

    /// Try to create a concrete map builtin function signature for the given map type. Uses the
    /// macro-generated [`builtin_types::Map::resolve`] function, registered as [`FunctionKind::Builtin`].
    fn try_create_map_builtin(self: &mut Self, item: &ast::Expression, name: &str, type_id: TypeId) -> ResolveResult<Option<ConstantId>> {

        if let Some(constant_id) = self.scopes.constant_id(ScopeId::ROOT, name, type_id) {
            return Ok(Some(constant_id));
        }

        let map_ty = self.type_by_id(type_id).as_map().ice()?;
        let (key_type_id, _value_type_id) = match (map_ty.key_type_id, map_ty.value_type_id) {
            (Some(key_type_id), Some(value_type_id)) => (key_type_id, value_type_id),
            _ => return if self.stage.must_resolve() {
                Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(format!("{}", &item)), self.module_path))
            } else {
                Ok(None)
            },
        };

        Ok(match builtin_types::Map::resolve(self, name, type_id, Some(key_type_id)) {
            None => None,
            Some((builtin_type, result_type_id, arg_type_ids)) => {
                Some(self.insert_builtin_fn(name, type_id, builtin_type, result_type_id, arg_type_ids))
            },
        })
    }

    /// Create a generator method signature (`next`/`value`/`key`). Uses the macro-generated
    /// [`builtin_types::Generator::resolve`] function, registered as [`FunctionKind::Builtin`].
    fn try_create_generator_builtin(self: &mut Self, name: &str, type_id: TypeId) -> ResolveResult<Option<ConstantId>> {

        if let Some(constant_id) = self.scopes.constant_id(ScopeId::ROOT, name, type_id) {
            return Ok(Some(constant_id));
        }

        // Pass type_id as inner_type_id so that GenValue/GenKey pseudo-types can resolve via
        // generator_signature, and the require_type check falls through to Type::void.
        Ok(match builtin_types::Generator::resolve(self, name, type_id, Some(type_id)) {
            None => None,
            Some((builtin_type, result_type_id, arg_type_ids)) => {
                Some(self.insert_builtin_fn(name, type_id, builtin_type, result_type_id, arg_type_ids))
            },
        })
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
                Some(self.insert_builtin_fn(name, type_id, builtin_type, result_type_id, arg_type_ids))
            },
        })
    }

    /// Creates an Array<T> type and returns its TypeId. Used by impl_builtins macro.
    pub(crate) fn create_array_type(self: &mut Self, element_type_id: TypeId) -> TypeId {
        use crate::shared::meta::Array;
        self.scopes.insert_type(None, Type::Array(Array { type_id: Some(element_type_id) }))
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

    /// If `expected_result` is a trait (object) type that every one of the given branch types satisfies
    /// and that is not already their common type, returns it as the type a branching expression (`if` /
    /// `match`) should collapse to. Returns `None` when there is nothing to collapse to, in which case the
    /// branches must share a common concrete type the usual way.
    ///
    /// Collapsing is what lets branches of differing concrete types be used as a single trait-typed value:
    /// each branch keeps its own concrete type and constructs its own value, while the whole expression is
    /// typed as the trait so method calls on the result dispatch virtually. It requires an explicit trait
    /// type in scope (an annotation, parameter or return type) because there is no principled common type to
    /// infer otherwise - a concrete type may implement several traits.
    fn branch_collapse_target(self: &Self, expected_result: Option<TypeId>, branch_type_ids: &[TypeId]) -> Option<TypeId> {
        let expected = expected_result?;
        if !self.type_by_id(expected).is_trait_object() {
            return None;
        }
        // homogeneous branches keep their natural (concrete) type even under a trait annotation; the value
        // coerces to the trait on use just like a plain value does, avoiding needless virtual dispatch
        if branch_type_ids.iter().all(|&type_id| type_id == branch_type_ids[0]) {
            return None;
        }
        if branch_type_ids.iter().all(|&type_id| self.type_accepted_for(type_id, expected)) {
            Some(expected)
        } else {
            None
        }
    }

    /// Whether a type id and all of its inner types are fully known (no still-inferring array element,
    /// map key/value or callable signature type). Used to gate type checks that must not fire over a
    /// type that is only partially resolved, since a resolution error aborts the whole resolve pass.
    fn type_id_complete(self: &Self, type_id: TypeId) -> bool {
        match self.type_by_id(type_id) {
            Type::Array(Array { type_id: inner, .. }) => inner.map_or(false, |inner| self.type_id_complete(inner)),
            Type::Map(MapType { key_type_id, value_type_id }) => match (key_type_id, value_type_id) {
                (Some(key), Some(value)) => self.type_id_complete(*key) && self.type_id_complete(*value),
                _ => false,
            },
            Type::Callable(Callable { arg_type_ids, ret_type_id }) => {
                ret_type_id.map_or(false, |ret| self.type_id_complete(ret))
                    && arg_type_ids.iter().all(|arg| arg.map_or(false, |arg| self.type_id_complete(arg)))
            },
            _ => true,
        }
    }

    /// Returns Ok if given types are the same, otherwise a TypeMismatch error.
    fn check_type_equals(self: &Self, item: &impl Positioned, given_type_id: TypeId, expected_type_id: TypeId) -> ResolveResult  {
        if !self.type_equals(given_type_id, expected_type_id) {
            let name_got = self.type_name(given_type_id);
            let name_expected = self.type_name(expected_type_id);
            let error_kind = ResolveErrorKind::TypeMismatch(name_got, name_expected);
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

    /// Resolves a constant name to a constant.
    fn try_resolve_constant_enum(self: &mut Self, item: &mut ast::Constant) -> Option<ConstantId> {

        let enum_info = if item.path.segments.len() > 1 {
            // "Enum::Test" -> drop Test, then alias-resolve Enum to e.g. MyModule::Enum
            let type_name = item.path.to_string(-1);
            let variant_name = item.path.segments[item.path.segments.len() - 1].name.clone();
            Some((self.scopes.alias(self.scope_id, &type_name).map(|a| a.to_string()).unwrap_or_else(|| self.make_path(&[ type_name ])), variant_name))
        } else {
            // "Test" -> alias-resolve to MyModule::Enum::Test, then drop Test.
            let alias = item.path.to_string(0);
            if let Some(qualified_item) = self.scopes.alias(self.scope_id, &alias) {
                let mut qualified = path_to_parts(qualified_item);
                qualified.pop();
                Some((parts_to_path(&qualified), alias.clone()))
            } else {
                None
            }
        };
        if let Some((type_name, variant_name)) = enum_info {
            if let Some(type_id) = self.scopes.type_id(self.scope_id, &type_name) {
                let qualified_name = type_name + "::" + &variant_name;
                // unit variants are constants owned by the enum type; data-variant constructors are
                // functions owned by VOID (see insert_function). Try both so the leading path segment
                // (e.g. a `use`-aliased API/module namespace) is resolved for either kind.
                self.scopes.constant_id(self.scope_id, &qualified_name, type_id)
                    .or_else(|| self.scopes.constant_id(self.scope_id, &qualified_name, TypeId::VOID))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Transforms a method call from a.b.c() format to c(a.b) if c is a function name (i.e. not a function reference)
    fn rewrite_method_call_to_constant_call(self: &mut Self, call_exp: &mut ast::Expression, call_args: &mut ast::ArgumentList) -> ResolveResult {
        let mut constant = None;
        if let Some(binary_op) = call_exp.as_binary_op_mut() {
            if binary_op.op == ast::BinaryOperator::Access {
                if let Some(member) = binary_op.right.as_member() {
                    if member.constant_id.is_some() {
                        // last member item is a function name, convert to const
                        constant = Some(
                            ast::Constant {
                                position: member.position,
                                path: ast::Path { position: member.ident.position, segments: vec![ member.ident.clone() ]},
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

    /// Returns an error kind if the pattern is refutable and thus cannot be used in a let binding.
    fn check_pattern_irrefutable(pattern: &ast::Pattern) -> Result<(), ResolveErrorKind> {
        use ast::Pattern;
        match pattern {
            Pattern::Wildcard(_) | Pattern::Binding(_) => Ok(()),
            Pattern::Struct(structure) => {
                for (_, field) in &structure.fields {
                    Self::check_pattern_irrefutable(field)?;
                }
                Ok(())
            },
            Pattern::Literal(_) => Err(ResolveErrorKind::InvalidOperation("Refutable literal pattern not allowed in let binding".to_string())),
            Pattern::Range(_) => Err(ResolveErrorKind::InvalidOperation("Refutable range pattern not allowed in let binding".to_string())),
            Pattern::VariantTuple(_) | Pattern::Path(_) => Err(ResolveErrorKind::InvalidOperation("Refutable enum pattern not allowed in let binding".to_string())),
            Pattern::Or(_) => Err(ResolveErrorKind::InvalidOperation("Refutable or-pattern not allowed in let binding".to_string())),
        }
    }

    /// Returns whether the (possibly nested) pattern introduces any variable binding. Used to reject bindings
    /// inside or-patterns, which would need consistent names/types across all alternatives (not yet supported).
    fn pattern_introduces_binding(pattern: &ast::Pattern) -> bool {
        use ast::Pattern;
        match pattern {
            Pattern::Binding(_) => true,
            Pattern::Wildcard(_) | Pattern::Literal(_) | Pattern::Path(_) | Pattern::Range(_) => false,
            Pattern::VariantTuple(variant) => variant.elements.iter().any(Self::pattern_introduces_binding),
            Pattern::Struct(structure) => structure.fields.iter().any(|(_, field)| Self::pattern_introduces_binding(field)),
            Pattern::Or(or) => or.alternatives.iter().any(Self::pattern_introduces_binding),
        }
    }

    /// For the `for k, v in g` desugaring the parser inserts `let v = $iter[k]` as the block's first
    /// statement (looking the value up by key, as it would for a map or array). A generator can't be
    /// indexed, so repoint that lookup at `$iter.value()`. Idempotent across resolver passes: only an
    /// index whose receiver is the loop's own generator variable is rewritten, and afterwards it is a
    /// method call (not an index), so a later pass leaves it alone.
    /// Builds the half-open range `0 .. upper`. The `0` is an unconstrained numeric literal so it unifies
    /// to `upper`'s type during range resolution. Used to lower array index iteration.
    fn make_range(position: ast::Position, upper: ast::Expression) -> ast::Expression {
        let zero = ast::Expression::Literal(ast::Literal {
            position,
            value: ast::LiteralValue::Numeric(Numeric::Unsigned(0)),
            type_name: None,
            type_id: None,
        });
        ast::Expression::BinaryOp(Box::new(ast::BinaryOp {
            position,
            op: ast::BinaryOperator::Range,
            left: ast::BinaryOperand::Expression(zero),
            right: ast::BinaryOperand::Expression(upper),
            type_id: None,
            op_resolved: false,
        }))
    }

    /// For a call like `arr.push(x)` whose receiver is an array of still-unknown element type, infer that
    /// element type from the value argument.
    /// Note: method names/parameter index for inference are hardcoded in value_index match block and may need to be updated for new array builtins.
    fn try_infer_array_element_from_builtin_args(self: &mut Self, call_exp: &mut ast::Expression, call_args: &mut ast::ArgumentList) -> ResolveResult {
        // identify `<array>.<method>` access whose array element type is still unresolved
        let (array_type_id, value_index) = {
            let binary_op = match call_exp.as_binary_op_mut() {
                Some(binary_op) if binary_op.op == ast::BinaryOperator::Access => binary_op,
                _ => return Ok(()),
            };
            // which (pre-transform, so the receiver is not yet prepended) argument carries the Element value
            let value_index = match binary_op.right.as_member().map(|member| member.ident.name.as_str()) {
                Some("push") => 0,
                Some("insert") => 1,
                _ => return Ok(()),
            };
            match binary_op.left.type_id(self).map(|type_id| self.type_by_id(type_id).as_array()) {
                Some(Some(&Array { type_id: None })) => (binary_op.left.type_id(self).ice()?, value_index),
                _ => return Ok(()),
            }
        };
        if let Some(arg) = call_args.args.get_mut(value_index) {
            self.resolve_expression(arg, None)?;
            if let Some(element_type_id) = arg.type_id(self) {
                self.type_by_id_mut(array_type_id).as_array_mut().ice()?.type_id = Some(element_type_id);
            }
        }
        Ok(())
    }

    /// For a call like `map.insert(k, v)` (or `get`/`remove`) whose receiver map has a still-unknown key or
    /// value type, infer those types from the relevant arguments. Required so methods can be called on an empty
    /// map literal `[ => ]` whose types are not yet determined.
    fn try_infer_map_types_from_builtin_args(self: &mut Self, call_exp: &mut ast::Expression, call_args: &mut ast::ArgumentList) -> ResolveResult {
        // identify `<map>.<method>` access and which (pre-transform, receiver not yet prepended) arguments
        // carry the key and value
        let (map_type_id, key_index, value_index) = {
            let binary_op = match call_exp.as_binary_op_mut() {
                Some(binary_op) if binary_op.op == ast::BinaryOperator::Access => binary_op,
                _ => return Ok(()),
            };
            let (key_index, value_index) = match binary_op.right.as_member().map(|member| member.ident.name.as_str()) {
                Some("insert") => (Some(0), Some(1)),
                Some("get") | Some("remove") => (Some(0), None),
                _ => return Ok(()),
            };
            match binary_op.left.type_id(self).map(|type_id| self.type_by_id(type_id).as_map()) {
                Some(Some(_)) => (binary_op.left.type_id(self).ice()?, key_index, value_index),
                _ => return Ok(()),
            }
        };
        if let Some(key_index) = key_index {
            if self.type_by_id(map_type_id).as_map().ice()?.key_type_id.is_none() {
                if let Some(arg) = call_args.args.get_mut(key_index) {
                    self.resolve_expression(arg, None)?;
                    if let Some(key_type_id) = arg.type_id(self) {
                        self.type_by_id_mut(map_type_id).as_map_mut().ice()?.key_type_id = Some(key_type_id);
                    }
                }
            }
        }
        if let Some(value_index) = value_index {
            if self.type_by_id(map_type_id).as_map().ice()?.value_type_id.is_none() {
                if let Some(arg) = call_args.args.get_mut(value_index) {
                    self.resolve_expression(arg, None)?;
                    if let Some(value_type_id) = arg.type_id(self) {
                        self.type_by_id_mut(map_type_id).as_map_mut().ice()?.value_type_id = Some(value_type_id);
                    }
                }
            }
        }
        Ok(())
    }
}

/// Methods to resolve individual AST structures.
impl<'ctx> Resolver<'ctx> {

    /// Resolves types and bindings used in a statement.
    fn resolve_statement(self: &mut Self, item: &mut ast::Statement) -> ResolveResult  {
        use self::ast::Statement::*;
        match item {
            Function(function) => self.resolve_function(function, None),
            StructDef(structure) => self.resolve_struct_type(structure),
            ImplBlock(impl_block) => self.resolve_impl_block(impl_block),
            TraitDef(trait_def) => self.resolve_trait_type(trait_def),
            LetBinding(binding) => self.resolve_let_binding(binding),
            LetPattern(let_pattern) => self.resolve_let_pattern(let_pattern),
            ConstDef(const_def) => self.resolve_const_def(const_def, None, TypeId::VOID),
            IfBlock(if_block) => self.resolve_if_block(if_block, None), // accept any type for these, result is discarded
            ForLoop(for_loop) => self.resolve_for_loop(for_loop),
            WhileLoop(while_loop) => self.resolve_while_loop(while_loop),
            Block(block) => self.resolve_block(block, None),
            Return(ret) => self.resolve_return(ret),
            Yield(yield_stmt) => self.resolve_yield(yield_stmt),
            Expression(expression) => self.resolve_expression(expression, None),
            UseDecl(use_declaration) => self.resolve_use_decl(use_declaration),
            EnumDef(enum_def) => self.resolve_enum_type(enum_def),
            Module(_) | Break(_) | Continue(_) | Suspend(_) => { Ok(()) /* nothing to do here */ },
        }
    }

    /// Resolves use declarations.
    fn resolve_use_decl(self: &mut Self, item: &mut ast::UseDecl) -> ResolveResult  {
        let mut unresolved = None;
        for (name, &mut (ref path, ref mut resolved)) in &mut item.mapping.iter_mut().filter(|(_, (_, r))| !r) {
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
        match item {
            Literal(literal) => self.resolve_literal(literal, expected_result),
            Variable(variable) => self.resolve_variable(variable, expected_result),
            Constant(constant) => self.resolve_constant(constant, expected_result),
            Assignment(_) => self.resolve_assignment(item, expected_result),
            BinaryOp(binary_op) => self.resolve_binary_op(binary_op, expected_result),
            UnaryOp(_) => self.resolve_unary_op(item, expected_result),
            Block(block) => self.resolve_block(block, expected_result),
            IfBlock(if_block) => self.resolve_if_block(if_block, expected_result),
            MatchBlock(match_block) => self.resolve_match_block(match_block, expected_result),
            AnonymousFunction(anonymous_function) => self.resolve_function(anonymous_function, None),
            Closure(closure) => self.resolve_closure(closure, expected_result),
            Cast(_) => self.resolve_cast(item, expected_result),
        }
    }

    // Resolves an inline type definition.
    fn resolve_inline_type(self: &mut Self, item: &mut ast::InlineType) -> ResolveResult<Option<TypeId>> {
        use ast::InlineType::*;
        match item {
            TypeName(type_name) => self.resolve_type_name(type_name, None),
            ArrayDef(array) => self.resolve_array_type(array),
            MapDef(map) => self.resolve_map_type(map),
            ResultDef(result) => self.resolve_result_type(result),
            OptionDef(option) => self.resolve_option_type(option),
            GeneratorDef(generator) => self.resolve_generator_type(generator),
            CallableDef(callable) => self.resolve_callable_type(callable),
            TraitBound(trait_bound) => self.resolve_trait_bound(trait_bound),
        }
    }

    /// Resolves a multiple-trait bound (e.g. `TraitA + TraitB`) into a `Type::TraitBound`.
    fn resolve_trait_bound(self: &mut Self, item: &mut ast::TraitBound) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        // resolve constituent trait names first
        for trait_name in &mut item.traits {
            self.resolve_type_name(trait_name, None)?;
        }
        // only build the bound once every constituent is resolved
        let mut trait_ids = Vec::with_capacity(item.traits.len());
        for trait_name in &item.traits {
            match trait_name.type_id {
                Some(type_id) => {
                    // each member must be a trait
                    if self.type_by_id(type_id).as_trait().is_none() {
                        return Err(ResolveError::new(trait_name, ResolveErrorKind::NotATrait(trait_name.path.to_string(0)), self.module_path));
                    }
                    // members must be distinct
                    if trait_ids.contains(&type_id) {
                        return Err(ResolveError::new(trait_name, ResolveErrorKind::DuplicateTraitBound(trait_name.path.to_string(0)), self.module_path));
                    }
                    trait_ids.push(type_id);
                },
                None => return Ok(None), // not all constituents resolved yet, retry next pass
            }
        }
        if item.type_id.is_none() {
            // reuse an existing identical bound so distinct occurrences share a type id
            let type_id = self.scopes.insert_anonymous_type(true, Type::TraitBound(trait_ids));
            item.type_id = Some(type_id);
        }
        self.types_resolved(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves a the definition of a callable type.
    fn resolve_callable_type(self: &mut Self, item: &mut ast::CallableDef) -> ResolveResult<Option<TypeId>> {
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
        for field in &mut item.params {
            self.resolve_inline_type(field)?;
        }
        // assemble type list first to avoid borrow issues
        let ret_type_id = item.ret.as_ref().map_or(Some(TypeId::VOID), |ret| ret.type_id(self));
        let arg_type_ids: Vec<_> = item.params.iter()
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
            item.type_id = self.scopes.type_id(self.scope_id, &self.make_path(&item.path.segments));
        }
        if let (Some(item_type_id), Some(expected_result)) = (item.type_id, expected_result) {
            self.check_type_accepted_for(item, item_type_id, expected_result)?;
        }
        self.types_resolved(item, expected_result)?;
        Ok(item.type_id)
    }

    /// Resolves an array definition.
    fn resolve_array_type(self: &mut Self, item: &mut ast::ArrayDef) -> ResolveResult<Option<TypeId>> {
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

    /// Resolves a map definition.
    fn resolve_map_type(self: &mut Self, item: &mut ast::MapDef) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        let key_type_id = self.resolve_inline_type(&mut item.key_type)?;
        let value_type_id = self.resolve_inline_type(&mut item.value_type)?;
        if let (None, Some(_), Some(_)) = (item.type_id, key_type_id, value_type_id) {
            let ty = Type::Map(MapType {
                key_type_id,
                value_type_id,
            });
            let new_type_id = self.scopes.insert_type(None, ty);
            item.type_id = Some(new_type_id);
        }
        self.types_resolved(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves a struct definition.
    fn resolve_struct_type(self: &mut Self, item: &mut ast::StructDef) -> ResolveResult {
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

    // Resolves/generates discriminants for all variants.
    fn resolve_enum_type_discriminants(self: &mut Self, variant_defs: &mut [ ast::VariantDef ]) -> ResolveResult<(TypeId, Vec<(String, EnumVariant)>)> {

        let mut variants = Vec::new();
        let mut next_discriminant = Numeric::Unsigned(0);
        let mut seen_discriminants = Vec::new();

        // check whether at least one enum variant specifies a type, default to i32 if none do
        let simple_type_name = variant_defs.iter().find_map(|v| match &v.kind {
            ast::VariantKind::Simple(_, Some(ast::Literal { type_name: Some(type_name), .. })) => Some(type_name),
            _ => None,
        });

        let discriminant_type_id = if let Some(named_type) = simple_type_name {
            named_type.type_id.or(self.scopes.type_id(self.scope_id, &self.make_path(&named_type.path.segments))).ice_msg("Invalid variant type")?
        } else {
            self.primitive_type_id(Type::i32)?
        };

        for variant in variant_defs {
            match &mut variant.kind {
                ast::VariantKind::Data(_, fields) => {
                    let mut field_type_ids = Vec::new();
                    for field in fields.iter_mut() {
                        self.resolve_inline_type(field)?;
                        field_type_ids.push(field.type_id(self));
                    }
                    variants.push((variant.ident.name.clone(), EnumVariant::Data(field_type_ids)));
                },
                ast::VariantKind::Simple(_, value_literal) => {
                    if let Some(value_literal) = value_literal {
                        self.resolve_literal(value_literal, Some(discriminant_type_id))?;
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

        Ok((discriminant_type_id, variants))
    }

    /// Resolves an enum definition.
    fn resolve_enum_type(self: &mut Self, item: &mut ast::EnumDef) -> ResolveResult {

        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(());
        }

        // resolve enum variant fields, assemble variant field lists.
        let (discriminant_type_id, variants) = self.resolve_enum_type_discriminants(&mut item.variants)?;

        // insert or update type
        if let Some(type_id) = item.type_id {
            self.type_by_id_mut(type_id).as_enum_mut().ice()?.variants = variants;
        } else {
            let qualified = self.make_path(&[ &item.ident.name ]);
            let primitive = if item.is_primitive() {
                Some((discriminant_type_id, self.type_by_id(discriminant_type_id).primitive_size()))
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
        if let Some(type_id) = item.type_id {
            for (index, variant) in item.variants.iter_mut().enumerate() {
                match &mut variant.kind {
                    ast::VariantKind::Data(constant_id @ None, fields) => {
                        let arg_type_ids: Vec<_> = fields.iter().map(|field| field.type_id(self)).collect::<Vec<_>>();
                        if arg_type_ids.iter().all(|t| t.is_some()) {
                            let path = self.make_path(&[ &item.ident.name, &variant.ident.name ]);
                            let kind = FunctionKind::Variant(item.type_id.ice()?, index as VariantIndex);
                            *constant_id = Some(self.scopes.insert_function(&path, Some(type_id), arg_type_ids, Some(kind)));
                        }
                    },
                    ast::VariantKind::Simple(constant_id, _) => {
                        let path = self.make_path(&[ &item.ident.name, &variant.ident.name ]);
                        let variants = &self.type_by_id_mut(type_id).as_enum_mut().ice()?.variants;
                        let discriminant = variants[index].1.as_simple().ice()?;
                        *constant_id = Some(self.scopes.insert_constant(&path, type_id, Some(type_id), ConstantValue::Discriminant(discriminant)));
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
            // Register the trait implementation on the type (even for empty impl blocks)
            // so that trait-default fallback can find provided consts/methods.
            if let Some(Some(trait_type_id)) = trait_type_id {
                if let Some(impl_traits) = self.type_by_id_mut(type_id).impl_traits_map_mut() {
                    impl_traits.entry(trait_type_id).or_insert(ImplTrait::new());
                }
            }
            // Resolve const declarations inside the impl block FIRST
            // so they're in scope when functions resolve (they may reference them).
            // Store them with qualified names: TypeName::CONST
            let type_name = self.type_name(type_id);
            for const_def in &mut item.consts {
                // For trait impls, validate that the const is declared by the trait
                if let Some(Some(trait_type_id)) = trait_type_id {
                    let trt = self.type_by_id(trait_type_id).as_trait().ice()?;
                    let const_name = &const_def.ident.name;
                    // Check if const is declared in trait (required or provided)
                    if trt.required_consts.get(const_name).is_none() && trt.provided_consts.get(const_name).is_none() {
                        let trait_name = item.trt.as_ref().ice()?.path.to_string(0);
                        return Err(ResolveError::new(const_def, ResolveErrorKind::TraitConstNotDeclared(const_name.clone(), trait_name), self.module_path));
                    }
                    // If the trait declares this const, check type matches
                    if let Some(&trait_const_id) = trt.required_consts.get(const_name).or(trt.provided_consts.get(const_name)) {
                        if let Some(trait_const_type_id) = trait_const_id.and_then(|id| self.scopes.constant_ref(id).type_id) {
                            // If the impl const has no explicit type, infer it from the trait const
                            if const_def.ty.is_none() {
                                const_def.ty = Some(ast::InlineType::TypeName(ast::TypeName {
                                    path:    ast::Path { position: const_def.position, segments: vec![] },
                                    type_id: Some(trait_const_type_id),
                                }));
                            }
                            // Resolve the impl const's type
                            if let Some(ty) = &mut const_def.ty {
                                self.resolve_inline_type(ty)?;
                            }
                            if let Some(impl_type_id) = const_def.ty.as_ref().and_then(|t| t.type_id(self)) {
                                if !self.type_equals(impl_type_id, trait_const_type_id) {
                                    let expected = self.type_name(trait_const_type_id);
                                    let got = self.type_name(impl_type_id);
                                    return Err(ResolveError::new(const_def, ResolveErrorKind::TraitConstTypeMismatch(const_name.clone(), expected, got), self.module_path));
                                }
                            }
                        }
                    }
                }
                self.resolve_const_def(const_def, Some(&type_name), type_id)?;
            }
            for function in &mut item.functions {
                self.resolve_function(function, Some(type_id))?;
                // if this is a trait impl and the trait is resolved
                if let Some(Some(trait_type_id)) = trait_type_id {
                    let trt = self.type_by_id(trait_type_id).as_trait().ice()?;
                    let function_name = &function.shared.sig.ident.name;
                    // check if function is defined in trait
                    if trt.provided.get(function_name).is_none() && trt.required.get(function_name).is_none() {
                        let trait_name = item.trt.as_ref().ice()?.path.to_string(0);
                        return Err(ResolveError::new(function, ResolveErrorKind::NotATraitMethod(function_name.clone(), trait_name), self.module_path));
                    }
                    if let Some(constant_id) = function.constant_id {
                        if let Some(impl_traits) = self.type_by_id_mut(type_id).impl_traits_map_mut() {
                            let impl_trait = impl_traits.entry(trait_type_id).or_insert(ImplTrait::new());
                            impl_trait.functions.insert(function_name.clone(), Some(constant_id));
                        }
                    }
                }
            }
            // Generate specialized trait default methods if impl overrides any provided const.
            // When a trait default method references `Self::CONST` and the impl overrides that const,
            // we need a specialized copy of the method that uses the impl's const instead.
            if let Some(Some(trait_type_id)) = trait_type_id {
                let trt = self.type_by_id(trait_type_id).as_trait().ice()?;

                // Build set of const names the impl defines
                let impl_const_names: Set<String> = item.consts.iter()
                    .map(|c| c.ident.name.clone()).collect();

                // Check if impl overrides any trait-provided const
                let has_overridden_const = impl_const_names.iter().any(|name| {
                    trt.provided_consts.get(name).is_some()
                });

                if has_overridden_const {
                    // Build replacement map: trait const_id -> impl const_id
                    let mut const_replacements: UnorderedMap<ConstantId, ConstantId> = UnorderedMap::new();
                    for const_name in &impl_const_names {
                        if let Some(&Some(trait_const_id)) = trt.provided_consts.get(const_name) {
                            let impl_const_id = item.consts.iter()
                                .find(|c| c.ident.name == *const_name)
                                .and_then(|c| c.constant_id);
                            if let Some(impl_const_id) = impl_const_id {
                                const_replacements.insert(trait_const_id, impl_const_id);
                            }
                        }
                    }

                    // For each trait default method not overridden by the impl
                    for (method_name, _) in &trt.provided {
                        // Skip if impl already provides this method
                        if item.functions.iter().any(|f| f.shared.sig.ident.name == *method_name) {
                            continue;
                        }

                        // Get the trait method's FunctionShared
                        let trait_func_shared = match trt.method_bodies.get(method_name) {
                            Some(fs) => fs,
                            None => continue,
                        };

                        // Check if this method uses any Self::CONST that we're overriding
                        let block = trait_func_shared.block.as_ref().ice()?;
                        let uses_overridden = Self::block_uses_consts(block, &const_replacements);
                        if !uses_overridden {
                            continue;
                        }

                        // Clone the FunctionShared and patch const refs in the body
                        let mut new_func_shared = trait_func_shared.clone();
                        let mut patched_block = new_func_shared.block.take().ice()?;
                        Self::patch_const_refs(&mut patched_block, &const_replacements);
                        new_func_shared.block = Some(patched_block);

                        // Reuse the original scope — all names are already resolved (constants
                        // have constant_id set, types have type_id set), so no new scope is needed.

                        // Create the new Function AST node
                        let new_func = ast::Function {
                            shared: new_func_shared,
                            constant_id: None,  // will be set during resolution on next pass
                        };

                        // Add to impl block's functions list
                        // The resolver will pick it up on the next pass
                        item.functions.push(new_func);
                    }
                }
            }
        }
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a const declaration.
    ///
    /// `type_name_prefix` — when `Some`, the const is stored with a qualified name
    /// (`TypeName::CONST`) in the root scope, making it accessible via `Self::CONST`
    /// or `TypeName::CONST` rather than a bare name. Used for impl block and trait consts.
    ///
    /// `owning_type_id` — the type that owns this constant. For module-level consts this is
    /// `TypeId::VOID`; for impl block consts this is the impl target type; for trait consts
    /// this is the trait type id.
    fn resolve_const_def(self: &mut Self, item: &mut ast::ConstDef, type_name_prefix: Option<&str>, owning_type_id: TypeId) -> ResolveResult {
        // Handle required trait consts (no default value)
        if item.expr.is_none() {
            // Resolve the type annotation (required for trait consts)
            let type_id = if let Some(ty) = &mut item.ty {
                self.resolve_inline_type(ty)?;
                ty.type_id(self)
            } else {
                None
            };

            // Insert placeholder constant (value will be filled by the impl)
            if item.constant_id.is_none() {
                let name = if let Some(prefix) = type_name_prefix {
                    self.make_path(&[prefix, &item.ident.name])
                } else {
                    self.make_path(&[&item.ident.name])
                };
                item.constant_id = Some(self.scopes.insert_constant(&name, owning_type_id, type_id, ConstantValue::UserConst(UserConstValue::Unresolved)));
            }
            return self.resolved(item);
        }

        // Capture position before mutable borrow
        let item_position = item.position();

        // Resolve the expression first (this also resolves any const references inside it)
        let expr = item.expr.as_mut().ice()?;
        self.resolve_expression(expr, item.ty.as_ref().and_then(|t| t.type_id(self)))?;

        // Validate: expression must be static (literals + const references + supported ops)
        if !self.is_const_expr(expr) {
            return Err(ResolveError::new(item,
                ResolveErrorKind::InvalidOperation("const expression must be a literal, a reference to another const, or a supported operation (+, -, *, /, %).".into()),
                self.module_path));
        }

        // Determine the type
        let type_id = if let Some(ty) = &mut item.ty {
            self.resolve_inline_type(ty)?;
            ty.type_id(self)
        } else {
            expr.type_id(self)
        };

        // Build the name: qualified for impl/trait consts, bare for module-level consts
        let name = if let Some(prefix) = type_name_prefix {
            self.make_path(&[prefix, &item.ident.name])
        } else {
            self.make_path(&[&item.ident.name])
        };

        // Build the user const value
        let user_const_value = if let Some(type_id) = type_id {
            let ty = self.type_by_id(type_id);
            if ty.is_string() {
                // String const: evaluate the expression (concatenation, interpolation, etc.)
                self.evaluate_const_expr(expr, item_position, &self.module_path, Some(type_id))?
            } else if ty.is_ref() {
                // Other reference type (array, map, struct): placeholder, updated during compilation
                UserConstValue::HeapRef(0)
            } else {
                // Value type: evaluate the const expression
                self.evaluate_const_expr(expr, item_position, &self.module_path, Some(type_id))?
            }
        } else {
            // Type not yet resolved — use a placeholder that will be set once the type is known
            UserConstValue::Unresolved
        };

        // Insert constant into scope (or update if already inserted)
        if item.constant_id.is_none() {
            item.constant_id = Some(self.scopes.insert_constant(
                &name, owning_type_id, type_id,
                ConstantValue::UserConst(user_const_value),
            ));
        } else {
            // Update the constant value and type_id in later resolution passes.
            let constant_id = item.constant_id.ice()?;
            let constant = self.scopes.constant_mut(constant_id);
            constant.type_id = type_id;
            constant.value = ConstantValue::UserConst(user_const_value);
        }

        self.resolved(item)
    }

    /// Resolves a trait definition block.
    fn resolve_trait_type(self: &mut Self, item: &mut ast::TraitDef) -> ResolveResult {
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        // ensure trait exists
        if item.type_id.is_none() {
            let mut trt = Trait::new();
            for function in &mut item.functions {
                let name = function.shared.sig.ident.name.clone();
                match function.shared.block {
                    Some(_) => trt.provided.insert(name, None),
                    None => trt.required.insert(name, None),
                };
            }
            // Register trait consts: required (no default) and provided (has default)
            for const_def in &item.consts {
                let name = const_def.ident.name.clone();
                if const_def.expr.is_some() {
                    trt.provided_consts.insert(name, None);
                } else {
                    trt.required_consts.insert(name, None);
                }
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
        // resolve trait consts (with default values) and required consts (placeholders)
        let trait_name = self.type_name(item.type_id.ice()?);
        for const_def in &mut item.consts {
            self.resolve_const_def(const_def, Some(&trait_name), item.type_id.ice()?)?;
        }
        // update trait with function constant_ids
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
        // update trait with const constant_ids
        for const_def in &item.consts {
            if let Some(constant_id) = const_def.constant_id {
                let name = &const_def.ident.name;
                let trt = self.type_by_id_mut(item.type_id.ice()?).as_trait_mut().ice()?;
                if const_def.expr.is_some() {
                    *trt.provided_consts.get_mut(name).ice()? = Some(constant_id);
                } else {
                    *trt.required_consts.get_mut(name).ice()? = Some(constant_id);
                }
            }
        }
        // Store method bodies of provided (default) methods for later specialization.
        // Done AFTER resolution so that constant references in the body are resolved.
        for function in &item.functions {
            if function.shared.block.is_some() {
                let name = function.shared.sig.ident.name.clone();
                let trt = self.type_by_id_mut(item.type_id.ice()?).as_trait_mut().ice()?;
                trt.method_bodies.insert(name, function.shared.clone());
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
        for arg in item.params.iter_mut() {
            self.resolve_let_binding(arg)?; // checks resolved_or_err
        }
        // resolve return type
        if let Some(ret) = &mut item.ret {
            self.resolve_inline_type(ret)?; // checks resolved_or_err
        }
        self.resolved(item)
    }

    /// Resolves a closure defintion.
    fn resolve_closure(self: &mut Self, item: &mut ast::Closure, expected_result: Option<TypeId>) -> ResolveResult {

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

        // if an expected callable type is known (e.g. from a call argument or type annotation), use it to infer not yet resolved parameter types
        let expected_callable = expected_result.and_then(|type_id| self.type_by_id(type_id).as_callable().cloned());
        if let Some(expected_callable) = &expected_callable {
            if expected_callable.arg_type_ids.len() == item.shared.sig.params.len() {
                for index in 0..item.shared.sig.params.len() {
                    if item.shared.sig.params[index].ty.is_none() && item.shared.sig.params[index].type_id(self).is_none() {
                        if let Some(expected_arg_type_id) = expected_callable.arg_type_ids[index] {
                            self.set_type_id(&mut item.shared.sig.params[index], expected_arg_type_id)?;
                        }
                    }
                }
            }
        }

        // resolve the signature (parameters and explicit return annotation, if any)
        self.resolve_signature(&mut item.shared.sig)?;

        // determine the return type: an explicit annotation or, lacking that, the type expected from context (call argument or annotation).
        // when neither is available it is left unresolved here and inferred from the closure's (parser-synthesized) return statement in resolve_return.
        let ret_type_id = if let Some(ret) = &item.shared.sig.ret {
            ret.type_id(self)
        } else {
            expected_callable.as_ref().and_then(|c| c.ret_type_id)
        };

        // resolve the closure's body, providing the return type so it reaches the synthesized return statement
        self.resolve_block(item.shared.block.as_mut().ice()?, ret_type_id)?;

        // collect parameter types (inferred parameters resolve through their bindings)
        let arg_type_ids: Vec<_> = item.shared.sig.params.iter().map(|field| field.type_id(self)).collect();

        // insert the closure on first encounter, otherwise update its (possibly newly inferred) types
        if let Some(function_id) = item.function_id {
            self.scopes.update_closure(function_id, ret_type_id, arg_type_ids);
        } else {
            let function_id = self.scopes.insert_closure(ret_type_id, arg_type_ids);
            item.function_id = Some(function_id);
            self.scopes.scope_set_function_id(self.scope_id, function_id);
        }
        // Collect which parameters are mutated in the body
        let function_id = item.function_id.ice()?;
        let param_binding_ids: Vec<BindingId> = item.shared.sig.params.iter().map(|p| p.binding_id).collect();
        let mutated = self.collect_mutated_param_indices(item.shared.block.as_ref().ice()?, &param_binding_ids);
        self.scopes.function_mut(function_id).mutated_params = mutated;

        self.scope_id = parent_scope_id;

        // the closure's resolved types live in its function/callable rather than the (possibly inferred, unannotated) signature, so check those
        let resolved = match item.function_id {
            Some(function_id) => self.scopes.function_ref(function_id).is_resolved(self),
            None => false,
        };
        if self.stage.must_resolve() && !resolved {
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

        if item.constant_id.is_none() && item.shared.sig.ret_resolved(self, Some(TypeId::VOID)) && item.shared.sig.args_resolved(self) {
            let result_type_id = item.shared.sig.ret_type_id(self, Some(TypeId::VOID));
            let arg_type_ids: Vec<_> = item.shared.sig.arg_type_ids(self);
            // function/method switch
            let (function_kind, qualified, alias) = if let Some(type_id) = struct_scope {
                let type_name = self.type_flat_name(type_id).ice()?;
                let path = self.make_path(&[ type_name, &item.shared.sig.ident.name ]);
                if item.shared.sig.params.len() == 0 || item.shared.sig.params[0].ident.name != "self" { // FIXME ugh
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
                // Collect which parameters are mutated in the body
                let param_binding_ids: Vec<BindingId> = item.shared.sig.params.iter().map(|p| p.binding_id).collect();
                let mutated = self.collect_mutated_param_indices(block, &param_binding_ids);
                self.scopes.function_mut(function_id).mutated_params = mutated;
            }
        }
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a return statement.
    fn resolve_return(self: &mut Self, item: &mut ast::Return) -> ResolveResult {
        let function_id = self.scopes.scope_function_id(self.scope_id).usr(Some(item), ResolveErrorKind::InvalidOperation("Use of return outside of function".to_string()))?;
        // a generator's `return` is for early exit only: `return;` is allowed, `return <expr>;` is not.
        // handled before the regular path so a valueless return is not checked against the `Generator<..>`
        // return type (which would surface a misleading void-vs-generator mismatch).
        if let Some(function_id) = function_id {
            if let Some(ret_type_id) = self.scopes.function_ref(function_id).ret_type_id(self) {
                if self.generator_signature(ret_type_id).is_some() {
                    if !item.expr.is_void() {
                        return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation("a generator's `return` is for early exit only and may not return a value".to_string()), self.module_path));
                    }
                    return self.types_resolved(&mut item.expr, Some(TypeId::VOID));
                }
            }
        }
        let ret_type_id = if let Some(function_id) = function_id {
            let declared_ret_type_id = self.scopes.function_ref(function_id).ret_type_id(self);
            // resolve before the unresolved-check below, so a returned expression that fails to resolve
            // gets its own dedicated diagnostic (e.g. `Ok`/`Err`/`?` outside a `Result` function) rather
            // than the generic "cannot resolve" fallback. The trailing `types_resolved` still enforces it.
            self.resolve_expression(&mut item.expr, declared_ret_type_id)?;
            // if the function's return type is already known (explicit, or previously inferred) verify the returned expression matches it,
            // otherwise infer the function's return type from the returned expression (used to infer closure return types).
            // acceptance (not strict equality) so a concrete implementor may be returned where a trait or trait bound
            // is declared, mirroring argument passing and let bindings (the heap object already carries its implementor index).
            if let Some(expr_type_id) = item.expr.type_id(self) {
                let callable_type_id = self.scopes.function_ref(function_id).callable_type_id;
                let callable = self.scopes.type_mut(callable_type_id).as_callable_mut().ice_msg("Function type is not callable")?;
                if let Some(callable_ret_type_id) = callable.ret_type_id {
                    self.check_type_accepted_for(item, expr_type_id, callable_ret_type_id)?;
                    Some(callable_ret_type_id)
                } else {
                    callable.ret_type_id = Some(expr_type_id);
                    Some(expr_type_id)
                }
            } else {
                declared_ret_type_id
            }
        } else {
            None
        };
        // check return type matches function result type
        self.types_resolved(&mut item.expr, ret_type_id)
    }

    /// Resolves a yield statement, validating it against the enclosing generator function's declared
    /// `Generator<V>` / `Generator<K, V>` return type. Validation is deferred until that return type
    /// is resolved (the resolver runs in repeated passes); operands are resolved every pass to make
    /// progress, with the generator's key/value types fed in as expected types once they are known.
    fn resolve_yield(self: &mut Self, item: &mut ast::Yield) -> ResolveResult {
        let function_id = self.scopes.scope_function_id(self.scope_id).usr(Some(item), ResolveErrorKind::InvalidOperation("Use of yield outside of function".to_string()))?;
        let ret_type_id = function_id.and_then(|function_id| self.scopes.function_ref(function_id).ret_type_id(self));
        let gen_sig = ret_type_id.and_then(|ret_type_id| self.generator_signature(ret_type_id));
        // resolve operands, supplying the generator's key/value types as expectations when known
        let (expected_key, expected_value) = match gen_sig {
            Some((key_type_id, value_type_id)) => (key_type_id, Some(value_type_id)),
            None => (None, None),
        };
        self.resolve_expression(&mut item.value, expected_value)?;
        if let Some(key) = &mut item.key {
            self.resolve_expression(key, expected_key)?;
        }
        // validate against the enclosing function once its return type is known
        if let Some(ret_type_id) = ret_type_id {
            match gen_sig {
                None => {
                    let ret_name = self.type_name(ret_type_id);
                    return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("`yield` is only allowed in a generator function (one returning `Generator<...>`); this function returns `{ret_name}`")), self.module_path));
                },
                Some((key_type_id, value_type_id)) => {
                    // a key may be yielded iff the generator declares one
                    match (item.key.is_some(), key_type_id) {
                        (true, None) => return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation("this generator yields values only; use `yield value`, not `yield key, value`".to_string()), self.module_path)),
                        (false, Some(_)) => return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation("this generator yields key/value pairs; use `yield key, value`".to_string()), self.module_path)),
                        _ => {},
                    }
                    // type-check operands against the declared value (and key) types
                    if let Some(value_expr_type_id) = item.value.type_id(self) {
                        self.check_type_accepted_for(&item.value, value_expr_type_id, value_type_id)?;
                    }
                    if let (Some(key), Some(key_type_id)) = (&item.key, key_type_id) {
                        if let Some(key_expr_type_id) = key.type_id(self) {
                            self.check_type_accepted_for(key, key_expr_type_id, key_type_id)?;
                        }
                    }
                },
            }
        }
        Ok(())
    }

    /// Resolves a variable name to a variable.
    fn resolve_variable(self: &mut Self, item: &mut ast::Variable, expected_result: Option<TypeId>) -> ResolveResult {
        // Adopt the type expected by a *use* of this variable only once the binding has had a chance to
        // resolve from its own declaration. Pinning a still-unresolved binding to a usage-expected type
        // during the early structural passes can poison it (e.g. an assignment target's element type
        // leaking onto the assigned value), which masks the genuine type mismatch and surfaces a bogus
        // error further downstream. So defer adoption to a later stage and let declaration-derived types
        // win: traits until infer_as_concrete (a concrete implementor may still turn up), concrete types
        // until infer_from_usage (one structural pass on, but still ahead of literal defaulting so a
        // genuinely usage-typed binding like `let mut y;` keeps constraining numeric literals).
        if item.type_id(self).is_none() {
            if let Some(expected_result) = expected_result {
                let ready = if self.type_by_id(expected_result).is_trait_object() {
                    self.stage.infer_as_concrete()
                } else {
                    self.stage.infer_from_usage()
                };
                if ready {
                    self.set_type_id(item, expected_result)?;
                }
            }
        } else if let (Some(item_type_id), Some(expected_result)) = (item.type_id(self), expected_result) {
            // the binding already carries a (possibly only partially inferred) type: complete its
            // inner types from the expected type instead of failing the check in types_resolved
            self.unify_type_ids(item_type_id, expected_result);
        }
        self.types_resolved(item, expected_result)
    }

    /// Resolves a constant name to a constant.
    fn resolve_constant(self: &mut Self, item: &mut ast::Constant, expected_result: Option<TypeId>) -> ResolveResult {
        // try resolving enum first
        if item.constant_id.is_none() {
            item.constant_id = self.try_resolve_constant_enum(item);
        }
        // resolve constant
        if item.constant_id.is_none() {
            // Resolve `Self` alias for the first path segment so that `Self::CONST` resolves to `TypeName::CONST`.
            let resolved_segments: Vec<String> = item.path.segments.iter().enumerate().map(|(i, seg)| {
                if i == 0 {
                    self.scopes.alias(self.scope_id, &seg.name)
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| seg.name.clone())
                } else {
                    seg.name.clone()
                }
            }).collect();
            let path = self.make_path(&resolved_segments);

            // try module-level consts, then try the type's own id (impl block consts)
            item.constant_id = self.scopes.constant_id(self.scope_id, &path, TypeId::VOID);
            if item.constant_id.is_none() && resolved_segments.len() >= 2 {
                // For `TypeName::CONST`, also try looking up with the type as the owner
                if let Some(type_id) = self.scopes.type_id(self.scope_id, &resolved_segments[0]) {
                    item.constant_id = self.scopes.constant_id(self.scope_id, &path, type_id);
                }
            }

            // look up the const in the a trait's provided consts
            if item.constant_id.is_none() && item.path.segments.len() >= 2 {
                if let Some(trait_const_id) = self.try_resolve_trait_const_default(&resolved_segments) {
                    item.constant_id = Some(trait_const_id);
                }
            }

            // check builtin functions
            if item.constant_id.is_none() && item.path.segments.len() == 2 {
                if let Some(type_id) = self.scopes.type_id(ScopeId::ROOT, &item.path.segments[0]) {
                    if let Some(constant_id) = self.try_create_scalar_builtin(&item.path.segments[1].name, type_id)? {
                        item.constant_id = Some(constant_id);
                    }
                }
            }

            // handle specialcase None constant
            if item.constant_id.is_none() && item.path.segments.len() == 1 && item.path.segments[0].name == "None" {
                // bare `None` is the unit variant of a synthesized `Option<T>`; bind it once the expected
                // Option type is known (`return None`, `let x: Option<_> = None`, match arm bodies, ...)
                if let Some(option_type_id) = expected_result.filter(|t| self.option_types.contains_key(t)) {
                    item.constant_id = Some(self.option_types.get(&option_type_id).ice()?.none_constant);
                }
            }

            // still unresolved, check stage, select appropriate error message
            if item.constant_id.is_none() {
                // `Ok`/`Err` are bound to a `Result<T>` variant constructor by try_bind_result_constructor
                // once a `Result` context is known, which can be as late as literal/type inference. Defer
                // their existence check to must_resolve so a valid `Ok`/`Err` isn't prematurely rejected;
                // genuine misuse (e.g. `?` in a non-`Result` function) gets a dedicated diagnostic there.
                // Same for `Some`/`None` - defer so a valid `Some`/`None` isn't prematurely rejected.
                let is_result_ctor = item.path.segments.len() == 1 && matches!(item.path.segments[0].name.as_str(), "Ok" | "Err");
                let is_option_ctor = item.path.segments.len() == 1 && matches!(item.path.segments[0].name.as_str(), "Some" | "None");
                let must_report = if is_result_ctor || is_option_ctor { self.stage.must_resolve() } else { self.stage.must_exist() };
                if must_report {
                    if is_option_ctor {
                        let expected = expected_result.map(|type_id| self.type_name(type_id));
                        return Err(ResolveError::new(item, ResolveErrorKind::OptionOutsideOptionContext(expected), self.module_path));
                    }
                    return Err(ResolveError::new(item, ResolveErrorKind::UndefinedIdentifier(item.path.to_string(0)), self.module_path));
                }
            }
        }
        // set expected type, if it isn't a trait or we're in final resolver stage
        if item.type_id(self).is_none() && item.constant_id.is_some() {
            if let Some(expected_result) = expected_result {
                if self.stage.infer_as_concrete() || !self.type_by_id(expected_result).is_trait_object() {
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
                // collapse to a declared common trait when the branches differ but both implement it,
                // otherwise the else branch must be acceptable for the if branch's type
                if let Some(trait_type_id) = self.branch_collapse_target(expected_result, &[ if_type_id, else_type_id ]) {
                    item.coerced_type_id = Some(trait_type_id);
                } else {
                    self.check_type_accepted_for(item, else_type_id, if_type_id)?;
                }
            }
        } else if let Some(if_type_id) = item.if_block.type_id(self) {
            // an if without an else produces no value on the path where the condition is false, so it must be
            // void. a non-void branch type means the if is being used as a value and needs an else to supply
            // the missing case.
            if if_type_id != TypeId::VOID {
                let type_name = self.type_name(if_type_id);
                return Err(ResolveError::new(item, ResolveErrorKind::MissingElseBranch(type_name), self.module_path));
            }
        }
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a match expression.
    fn resolve_match_block(self: &mut Self, item: &mut ast::MatchBlock, expected_result: Option<TypeId>) -> ResolveResult {
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        self.resolve_expression(&mut item.expr, None)?;
        let subject_type_id = item.expr.type_id(self);
        for (pattern, block) in &mut item.branches {
            // resolve the pattern against the matched value's type before the body so pattern bindings are typed
            self.resolve_pattern(pattern, subject_type_id)?;
            self.resolve_block(block, expected_result)?;
        }
        // Determine the result type of the match from its value-producing arms. Arms that diverge via
        // return/break/continue produce no value (e.g. the `Err(e) => return Err(e)` arm the `?` operator
        // lowers to) and are excluded from the type computation entirely.
        //
        // When the value arms differ but a declared trait type accepts all of them, collapse the match to
        // that trait (each arm keeps its concrete type and builds its own value; the match-expression is
        // typed as the trait so method calls on the result dispatch virtually). Otherwise every value arm
        // must be acceptable for the first one's type (which the match reports as its own); the `unify`
        // first lets partially-resolved compound types (e.g. empty map literal `[ => ]` with unknown value
        // type) pick up concrete inner types from sibling arms, and the acceptance check is gated on both
        // types being fully resolved so that still-inferring arms aren't rejected on an early pass - a
        // resolution error aborts the whole resolve.
        let value_arm_type_ids: Option<Vec<TypeId>> = item.branches.iter()
            .filter(|(_, block)| block.control_flow().is_none())
            .map(|(_, block)| block.type_id(self))
            .collect();
        let collapse_target = value_arm_type_ids.as_ref().and_then(|type_ids| self.branch_collapse_target(expected_result, type_ids));
        if let Some(trait_type_id) = collapse_target {
            item.coerced_type_id = Some(trait_type_id);
        } else {
            let mut reference_type_id = None;
            for (_, block) in item.branches.iter().filter(|(_, block)| block.control_flow().is_none()) {
                if let Some(block_type_id) = block.type_id(self) {
                    match reference_type_id {
                        None => reference_type_id = Some(block_type_id),
                        Some(reference_type_id) => {
                            self.unify_type_ids(reference_type_id, block_type_id);
                            if self.type_id_complete(reference_type_id) && self.type_id_complete(block_type_id) {
                                self.check_type_accepted_for(block, block_type_id, reference_type_id)?;
                            }
                        },
                    }
                }
            }
        }
        self.scope_id = parent_scope_id;
        // the match must cover every possible value of the subject (the compiler relies on this: a non-matching
        // subject would fall through past the arms without being discarded). the check runs every pass but stays
        // silent (Err) until all the types it needs are resolved, so it cannot report a false gap on an early pass.
        if let Some(subject_type_id) = subject_type_id {
            let matrix: Vec<Vec<Option<&ast::Pattern>>> = item.branches.iter().map(|(pattern, _)| vec![Some(pattern)]).collect();
            if let Ok(Some(witness)) = exhaustiveness::witness(self, &[Some(subject_type_id)], &matrix) {
                let witness = witness.into_iter().next().unwrap_or_else(|| "_".to_string());
                return Err(ResolveError::new(item, ResolveErrorKind::NonExhaustiveMatch(witness), self.module_path));
            }
        }
        self.resolved(item)
    }

    /// Resolves a match (or, later, let) pattern against the type of the value being matched, typing any
    /// bindings the pattern introduces. `subject_type_id` is None until the matched value's type is known.
    fn resolve_pattern(self: &mut Self, item: &mut ast::Pattern, subject_type_id: Option<TypeId>) -> ResolveResult {
        use ast::Pattern;
        match item {
            Pattern::Wildcard(_) => {},
            // a qualified path names a unit variant (or constant); nothing to bind, validity is checked below
            Pattern::Path(path) => {
                if let Some(subject_type_id) = subject_type_id {
                    let variant_name = &path.segments.last().ice()?.name;
                    if let Some(enum_) = self.type_by_id(subject_type_id).as_enum() {
                        if enum_.variant_index(variant_name).is_none() {
                            return Err(ResolveError::new(path, ResolveErrorKind::UndefinedMember(variant_name.clone()), self.module_path));
                        }
                    }
                }
            },
            // a single identifier binds the entire matched value
            Pattern::Binding(binding) => {
                if let Some(subject_type_id) = subject_type_id {
                    self.set_type_id(binding, subject_type_id)?;
                }
                if self.stage.must_resolve() && binding.type_id(self).is_none() {
                    return Err(ResolveError::new(binding, ResolveErrorKind::CannotResolve(binding.ident.to_string()), self.module_path));
                }
            },
            // a literal is matched by value
            Pattern::Literal(literal) => {
                self.resolve_literal(literal, subject_type_id)?;
            },
            // a range is matched against its two numeric endpoints
            Pattern::Range(range) => {
                if let Some(subject_type_id) = subject_type_id {
                    if !self.type_by_id(subject_type_id).is_numeric() {
                        return Err(ResolveError::new(range, ResolveErrorKind::InvalidOperation(format!("Range pattern requires a numeric type, got {}", self.type_name(subject_type_id))), self.module_path));
                    }
                }
                self.resolve_literal(&mut range.lo, subject_type_id)?;
                self.resolve_literal(&mut range.hi, subject_type_id)?;
            },
            // a data variant matches a variant tag and recurses into its fields
            Pattern::VariantTuple(variant) => {
                if let Some(subject_type_id) = subject_type_id {
                    let variant_name = variant.path.segments.last().ice()?.name.clone();
                    // pull the variant's field types out of the enum (cloned to release the immutable borrow before recursing)
                    let field_type_ids = match self.type_by_id(subject_type_id).as_enum() {
                        Some(enum_) => match enum_.variants.iter().find(|(name, _)| name == &variant_name) {
                            Some((_, EnumVariant::Data(field_type_ids))) => field_type_ids.clone(),
                            Some((_, EnumVariant::Simple(_))) => return Err(ResolveError::new(variant, ResolveErrorKind::InvalidOperation(format!("Enum variant '{}' carries no data", variant_name)), self.module_path)),
                            None => return Err(ResolveError::new(variant, ResolveErrorKind::UndefinedMember(variant_name), self.module_path)),
                        },
                        None => return Err(ResolveError::new(variant, ResolveErrorKind::InvalidOperation(format!("Cannot match variant pattern against non-enum type {}", self.type_name(subject_type_id))), self.module_path)),
                    };
                    if field_type_ids.len() != variant.elements.len() {
                        return Err(ResolveError::new(variant, ResolveErrorKind::NumberOfArguments(variant.path.to_string(0), field_type_ids.len() as ItemIndex, variant.elements.len() as ItemIndex), self.module_path));
                    }
                    for (element, field_type_id) in variant.elements.iter_mut().zip(field_type_ids.into_iter()) {
                        self.resolve_pattern(element, field_type_id)?;
                    }
                } else {
                    // subject type unknown this pass; still recurse so nested patterns get a chance to make progress
                    for element in variant.elements.iter_mut() {
                        self.resolve_pattern(element, None)?;
                    }
                }
            },
            // a struct matches field-wise against the sub-patterns; all fields must be present
            Pattern::Struct(structure) => {
                if let Some(subject_type_id) = subject_type_id {
                    // clone the struct metadata to release the immutable borrow before recursing
                    let struct_ = match self.type_by_id(subject_type_id).as_struct() {
                        Some(struct_) => struct_.clone(),
                        None => return Err(ResolveError::new(structure, ResolveErrorKind::InvalidOperation(format!("Cannot match struct pattern against non-struct type {}", self.type_name(subject_type_id))), self.module_path)),
                    };
                    // reject unknown fields
                    for (field_name, _) in structure.fields.iter() {
                        if !struct_.has_field(&field_name.name) {
                            return Err(ResolveError::new(structure, ResolveErrorKind::UndefinedMember(field_name.name.clone()), self.module_path));
                        }
                    }
                    // each field may be matched at most once; without a trailing `..` every field is also
                    // required, so a missing field (count 0) is an error unless rest-syntax was given
                    for (struct_field, _) in struct_.fields.iter() {
                        let count = structure.fields.iter().filter(|(name, _)| &name.name == struct_field).count();
                        if count > 1 || (count == 0 && !structure.rest) {
                            return Err(ResolveError::new(structure, ResolveErrorKind::InvalidOperation(format!("Struct pattern must match field '{}' exactly once", struct_field)), self.module_path));
                        }
                    }
                    for (field_name, field_pattern) in structure.fields.iter_mut() {
                        let field_type_id = struct_.type_id(&field_name.name);
                        self.resolve_pattern(field_pattern, field_type_id)?;
                    }
                } else {
                    // subject type unknown this pass; still recurse so nested patterns get a chance to make progress
                    for (_, field_pattern) in structure.fields.iter_mut() {
                        self.resolve_pattern(field_pattern, None)?;
                    }
                }
            },
            // an or-pattern matches if any alternative matches; each is resolved against the same subject type
            Pattern::Or(or) => {
                // bindings inside an or-pattern would need consistent names/types across all alternatives, which
                // is not yet supported; reject them so a matched alternative never leaves a binding half-defined
                for alternative in or.alternatives.iter() {
                    if Self::pattern_introduces_binding(alternative) {
                        return Err(ResolveError::new(alternative, ResolveErrorKind::InvalidOperation("Bindings are not allowed inside an or-pattern".to_string()), self.module_path));
                    }
                }
                for alternative in or.alternatives.iter_mut() {
                    self.resolve_pattern(alternative, subject_type_id)?;
                }
            },
        }
        Ok(())
    }

    /// Resolves a for loop.
    fn resolve_for_loop(self: &mut Self, item: &mut ast::ForLoop) -> ResolveResult {
        use ast::{Expression::*, BinaryOperator as Op};
        let parent_scope_id = self.scope_id;
        self.scope_id = item.scope_id;
        // create bindings for the iteration variables present in this loop form
        if let Some(binding) = item.iter.as_mut() { self.resolve_let_binding(binding)?; }
        if let Some(binding) = item.iter_key.as_mut() { self.resolve_let_binding(binding)?; }
        match &item.expr { // NOTE: these need to match Compiler::compile_for_loop
            BinaryOp(bo) if bo.op == Op::Range || bo.op == Op::RangeInclusive => {
                // range iteration: the value sequence is bound into `iter`.
                let type_id = item.iter.as_ref().and_then(|b| b.type_id(self));
                self.resolve_expression(&mut item.expr, type_id)?;
                if let (Some(type_id), Some(binding)) = (item.expr.type_id(self), item.iter.as_mut()) {
                    self.set_type_id(binding, type_id)?;
                }
            },
            _ => {
                self.resolve_expression(&mut item.expr, None)?;
                // generator iteration: the compiler drives next()/value()/key() (see
                // Compiler::compile_for_loop_generator). Here we only bind the iteration variables' types.
                // Checked before the map/array logic so a generator never misroutes to those paths.
                if let Some((key_type_id, value_type_id)) = item.expr.type_id(self).and_then(|t| self.generator_signature(t)) {
                    if let Some(binding) = item.iter.as_mut() {
                        self.set_type_id(binding, value_type_id)?;
                    }
                    if item.iter_key.is_some() {
                        // a value-only generator can't be key-iterated.
                        let key_type_id = key_type_id.usr(Some(&item.expr), ResolveErrorKind::NotKeyValueIterable(self.type_name(value_type_id)))?;
                        if let Some(binding) = item.iter_key.as_mut() {
                            self.set_type_id(binding, key_type_id)?;
                        }
                    }
                    self.resolve_block(&mut item.block, Some(self.primitive_type_id(Type::void)?))?;
                    self.scope_id = parent_scope_id;
                    return self.resolved(item);
                }
                let item_is_map = self.item_type(&item.expr).map_or(false, |ty| ty.as_map().is_some());
                let item_is_array = self.item_type(&item.expr).map_or(false, |ty| ty.as_array().is_some());
                if item.iter.is_none() && item.iter_key.is_some() {
                    // key-only iteration (`for k, _`): normalize to plain value iteration with the key binding
                    // moved into `iter`, over a one-shot snapshot so the body can freely mutate the original.
                    if item_is_array {
                        // `for k in 0 .. arr.len()`. On the next pass `item.expr` is a range and routes to the
                        // range arm above, walked by value into the (now relocated) `iter` binding.
                        let position = item.expr.position();
                        let array_expr = replace(&mut item.expr, ast::Expression::void(position));
                        let len_call = Self::make_method_call(array_expr, "len", Vec::new(), position);
                        item.expr = Self::make_range(position, len_call);
                        item.iter = item.iter_key.take();
                        let type_id = item.iter.as_ref().and_then(|b| b.type_id(self));
                        self.resolve_expression(&mut item.expr, type_id)?;
                        if let (Some(type_id), Some(binding)) = (item.expr.type_id(self), item.iter.as_mut()) {
                            self.set_type_id(binding, type_id)?;
                        }
                    } else if item_is_map {
                        // `for v in map.keys()`: a values-like snapshot of the keys, walked by value into `iter`.
                        let position = item.expr.position();
                        let map_expr = replace(&mut item.expr, ast::Expression::void(position));
                        item.expr = Self::make_method_call(map_expr, "keys", Vec::new(), position);
                        item.iter = item.iter_key.take();
                        self.resolve_expression(&mut item.expr, None)?;
                        if let Some(&Type::Array(Array { type_id: Some(elements_type_id) })) = self.item_type(&item.expr) {
                            if let Some(binding) = item.iter.as_mut() { self.set_type_id(binding, elements_type_id)?; }
                        }
                    } else if let Some(type_id) = item.expr.type_id(self) {
                        // key-only iteration over a non-map, non-array is rejected once its type is known.
                        let type_name = self.type_name(type_id);
                        return Err(ResolveError::new(&item.expr, ResolveErrorKind::NotKeyValueIterable(type_name), self.module_path));
                    }
                } else if item_is_map {
                    // map value iteration (`for v`) or key-value iteration (`for k, v`): the map is left in
                    // place and lowered to map_iter / map_iter_kv by the compiler. Type the bindings off the map.
                    let (key_type_id, value_type_id) = {
                        let map = self.item_type(&item.expr).and_then(|ty| ty.as_map());
                        (map.and_then(|m| m.key_type_id), map.and_then(|m| m.value_type_id))
                    };
                    if let (Some(value_type_id), Some(binding)) = (value_type_id, item.iter.as_mut()) {
                        self.set_type_id(binding, value_type_id)?;
                    }
                    if let (Some(key_type_id), Some(binding)) = (key_type_id, item.iter_key.as_mut()) {
                        self.set_type_id(binding, key_type_id)?;
                    }
                } else {
                    // array iteration: value (`for v`) or index+value (`for k, v`).
                    if let Some(&Type::Array(Array { type_id: Some(elements_type_id) })) = self.item_type(&item.expr) {
                        // infer iter type from array element type
                        if let Some(binding) = item.iter.as_mut() { self.set_type_id(binding, elements_type_id)?; }
                    } else if let (Some(iter_type_id), Some(array_type_id)) = (item.iter.as_ref().and_then(|b| b.type_id(self)), item.expr.type_id(self)) {
                        // infer array element type from iter
                        if let Some(array) = self.type_by_id_mut(array_type_id).as_array_mut() {
                            array.type_id = Some(iter_type_id);
                        }
                    } else if self.item_type(&item.expr).map_or(false, |expr| expr.as_array().is_none()) {
                        let type_name = self.type_name(item.expr.type_id(self).ice()?);
                        return Err(ResolveError::new(&item.expr, ResolveErrorKind::NotIterable(type_name), self.module_path));
                    }
                    // the running index of `for k, v` over an array is StackAddress-typed (like `arr.len()`).
                    if let Some(binding) = item.iter_key.as_mut() {
                        self.set_type_id(binding, self.primitive_type_id(STACK_ADDRESS_TYPE)?)?;
                    }
                }
            },
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
        //println!("{:?}", item);
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
    fn resolve_assignment(self: &mut Self, item: &mut ast::Expression, expected_result: Option<TypeId>) -> ResolveResult {
        // Check for custom `Index` trait on index-write targets (`a[i] = v` / `a[i] += v`).
        // We inspect the assignment before extracting it so we can replace the whole expression
        // with a method call for plain writes.
        if let Some(assignment) = item.as_assignment() {
            if let Some(bin) = assignment.left.as_binary_op() {
                if bin.op == IndexWrite {
                    let recv_type_id = bin.left.type_id(self);
                    if let (Some(recv_type_id), Some(intrinsic)) = (recv_type_id, self.intrinsic_index) {
                        if self.type_accepted_for(recv_type_id, intrinsic.trait_type_id) {
                            if assignment.op == Assign {
                                // Plain write `a[i] = v` -> rewrite to `a.set(i, v)`
                                return self.rewrite_index_write_to_method(item, intrinsic, expected_result);
                            } else if assignment.op.arithmetic_assign_base().is_some() {
                                // Compound assignment `a[i] += v` -> use IndexMethod dispatch
                                let assignment = item.as_assignment_mut().ice()?;
                                return self.resolve_index_compound_assign(assignment, recv_type_id, &intrinsic, expected_result);
                            }
                        }
                    }
                }
            }
        }

        let assignment = item.as_assignment_mut().ice()?;
        use ast::BinaryOperator::*;
        // shift-assigns take an independent integer shift amount on the right that must not be unified with
        // the target type (mirrors the standalone `<<`/`>>` handling in resolve_binary_op)
        let shift_assign = matches!(assignment.op, ShlAssign | ShrAssign);
        // Resolve the assignment target first *without* imposing the value's type on it, then resolve the
        // value against the target's type. This ordering means a genuine mismatch is reported as
        // "expected <target>, got <value>"; resolving the target against the value's type instead (the
        // reverse) would name the operands the wrong way round for an assignment.
        self.resolve_expression(&mut assignment.left, None)?;
        // Check if the assignment target references a user const or const-derived binding.
        // This check runs AFTER resolving the left side so that constant_id is set on const references.
        if let Some(const_name) = self.find_const_in_receiver_chain(&assignment.left) {
            return Err(ResolveError::new(item, ResolveErrorKind::AssignToConst(const_name), self.module_path));
        }
        let left_type_id = assignment.left.type_id(self);

        // classify compound assignment once the target type is known. Numeric/string targets for arithmetic,
        // and integer targets for bitwise/shift, keep their built-in codegen; a custom target (variable, field
        // or index) is dispatched in-place to the operator-trait method so the target is evaluated only once.
        if let (Some(base_op), Some(left_type_id)) = (assignment.op.arithmetic_assign_base(), left_type_id) {
            let left_type = self.type_by_id(left_type_id);
            let is_bitwise_shift = matches!(base_op, BitAnd | BitOr | BitXor | Shl | Shr);
            let is_builtin = if is_bitwise_shift {
                left_type.is_integer()
            } else {
                left_type.is_numeric() || left_type.is_string()
            };
            if is_builtin {
                assignment.op_dispatch = Some(ast::CompoundDispatch::Builtin);
            } else {
                let intrinsic = self.intrinsic_ops.get(&base_op).copied().ice()?;
                let rhs_expected = if matches!(base_op, Shl | Shr) {
                    Some(intrinsic.rhs_type_id) // i64
                } else {
                    Some(left_type_id) // Self
                };
                self.resolve_expression(&mut assignment.right, rhs_expected)?;
                // Propagate from_const to the assignment target if RHS references a user const
                if self.expr_references_user_const(&assignment.right) {
                    if let Some(target_binding_id) = const_eval::extract_root_binding_id(&assignment.left) {
                        self.scopes.binding_mut(target_binding_id).from_const = true;
                    }
                }
                if let Some(right_type_id) = assignment.right.type_id(self) {
                    if !matches!(base_op, Shl | Shr) {
                        self.check_type_accepted_for(assignment, right_type_id, left_type_id)?; // rhs must be Self
                    }
                }
                if let Some(constant_id) = self.intrinsic_op_method_constant(left_type_id, &intrinsic) {
                    assignment.op_dispatch = Some(ast::CompoundDispatch::Method(constant_id));
                } else if self.stage.must_resolve() {
                    let type_name = self.type_name(left_type_id);
                    let trait_name = self.type_name(intrinsic.trait_type_id);
                    return Err(ResolveError::new(assignment, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path));
                }
                assignment.type_id = Some(TypeId::VOID);
                return self.types_resolved(assignment, expected_result);
            }
        }
        self.resolve_expression(&mut assignment.right, if shift_assign { None } else { left_type_id })?;
        // Propagate from_const to the assignment target if RHS references a user const
        if self.expr_references_user_const(&assignment.right) {
            if let Some(target_binding_id) = const_eval::extract_root_binding_id(&assignment.left) {
                self.scopes.binding_mut(target_binding_id).from_const = true;
            }
        }
        let right_type_id = assignment.right.type_id(self);
        // if the target's type can't be determined on its own, infer it from the value instead (e.g. an
        // untyped array's element type, or an uninitialized binding learned from what is assigned to it)
        if left_type_id.is_none() && !shift_assign {
            self.resolve_expression(&mut assignment.left, right_type_id)?;
        }
        // bitwise/shift compound assigns require integer operands
        if matches!(assignment.op, BitAndAssign | BitOrAssign | BitXorAssign | ShlAssign | ShrAssign) {
            if let Some(left_type_id) = assignment.left.type_id(self) {
                if !self.type_by_id(left_type_id).is_integer() {
                    return Err(ResolveError::new(assignment, ResolveErrorKind::InvalidOperation(format!("{} requires integer operands", assignment.op)), self.module_path));
                }
            }
            if let Some(right_type_id) = assignment.right.type_id(self) {
                if !self.type_by_id(right_type_id).is_integer() {
                    return Err(ResolveError::new(assignment, ResolveErrorKind::InvalidOperation(format!("{} requires an integer right operand", assignment.op)), self.module_path));
                }
            }
        }
        // an assignment is an expression that produces no value, so it must match the expected type (if any) against void
        assignment.type_id = Some(TypeId::VOID);
        self.types_resolved(assignment, expected_result)
    }

    /// Resolves a type cast. Built-in primitive conversions are validated against the conversion matrix;
    /// a cast to a type backed by an intrinsic conversion trait (e.g. `String` via `ToString`) that is not
    /// a built-in cast is lowered to a call of the trait's method on the operand (e.g. `expr.to_string()`).
    fn resolve_cast(self: &mut Self, item: &mut ast::Expression, expected_result: Option<TypeId>) -> ResolveResult {
        let cast = item.as_cast_mut().ice()?;
        self.resolve_type_name(&mut cast.ty, expected_result)?;
        let cast = item.as_cast_mut().ice()?;
        self.resolve_expression(&mut cast.expr, None)?;
        let cast = item.as_cast_mut().ice()?;
        let (from_type_id, to_type_id) = match (cast.expr.type_id(self), cast.ty.type_id(self)) {
            (Some(from_type_id), Some(to_type_id)) => (from_type_id, to_type_id),
            _ => return Ok(()), // can't decide until both operand and target are known
        };
        let cast = item.as_cast_mut().ice()?;
        cast.set_type_id(self, to_type_id);
        if from_type_id == to_type_id {
            item.as_cast_mut().ice()?.kind = Some(ast::CastKind::Primitive);
            return Ok(());
        }
        let from_type = self.type_by_id(from_type_id);
        let to_type = self.type_by_id(to_type_id);
        let is_builtin_cast =
            (to_type.is_string() && from_type.is_numeric()) ||
            (to_type.is_integer() && (from_type.is_numeric() || from_type.is_bool() || from_type.is_simple_enum())) ||
            (to_type.is_float() && from_type.is_numeric()) ||
            (to_type.is_bool() && from_type.is_integer());
        if is_builtin_cast {
            item.as_cast_mut().ice()?.kind = Some(ast::CastKind::Primitive);
            return Ok(());
        }
        // not a built-in primitive cast: the target must be backed by an intrinsic conversion trait. The
        // cast stays unclassified (kind == None, hence unresolved) until either the operand is found to
        // implement the trait (lowered to a method call) or resolution stalls (dedicated error).
        match self.intrinsic_casts.get(&to_type_id).copied() {
            Some(intrinsic) => {
                if self.type_accepted_for(from_type_id, intrinsic.trait_type_id) {
                    if intrinsic.method_return_type_id == to_type_id {
                        // method returns exactly the target (e.g. ToString): replace the whole cast
                        self.rewrite_cast_to_method(item, intrinsic.method, expected_result)
                    } else {
                        // method returns a wider numeric type (ToSigned/ToUnsigned/ToFloat): keep the Cast node as the
                        // trailing primitive cast, lower its operand to the trait method call
                        self.rewrite_cast_via_method(item, intrinsic.method, to_type_id)
                    }
                } else if self.stage.must_resolve() {
                    // resolution has stalled: no impl block will make the trait appear, so the operand
                    // genuinely does not implement it
                    let from_name = self.type_name(from_type_id);
                    let trait_name = self.type_name(intrinsic.trait_type_id);
                    Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(from_name, trait_name), self.module_path))
                } else {
                    // an impl block may still resolve in a later pass; defer without classifying the cast
                    Ok(())
                }
            },
            None => {
                let from_name = self.type_name(from_type_id);
                let to_name = self.type_name(to_type_id);
                Err(ResolveError::new(item, ResolveErrorKind::InvalidCast(from_name, to_name), self.module_path))
            },
        }
    }

    /// Resolves a binary operation.
    fn resolve_binary_op(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::BinaryOperator::*;
        match item.op {
            And | Or => self.resolve_logical_op(item, expected_result)?,
            Less | Greater | LessOrEq | GreaterOrEq | Equal | NotEqual => self.resolve_comparison_op(item, expected_result)?,
            Add | Sub | Mul | Div | Rem => self.resolve_arithmetic_op(item, expected_result)?,
            Range | RangeInclusive => self.resolve_range_op(item, expected_result)?,
            BitAnd | BitOr | BitXor => self.resolve_bitwise_op(item, expected_result)?,
            Shl | Shr => self.resolve_shift_op(item, expected_result)?,
            Index | IndexWrite => self.resolve_index_op(item, expected_result)?,
            Access | AccessWrite => self.resolve_access_op(item, expected_result)?,
            Call => self.resolve_call_op(item, expected_result)?,
            Assign | AddAssign | SubAssign | MulAssign | DivAssign | RemAssign
            | BitAndAssign | BitOrAssign | BitXorAssign | ShlAssign | ShrAssign => {
                return Self::ice("Unexpected operator in resolve_binary_op");
            },
        }
        // validate the produced type against what the caller expects. several branches above (comparisons,
        // logical and/or, member access) produce a type independent of expected_result, so without this check
        // a mismatch (e.g. `let x: i32 = a < b;`) would slip past the resolver and corrupt the VM stack.
        self.types_resolved(item, expected_result)
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
        // Track whether this binding was initialized from a user const expression.
        // This lets us reject mutation of const-derived data through variable bindings.
        // Done after the block above so that mutable borrows of item.expr have ended.
        if let Some(expr) = &item.expr {
            if self.expr_references_user_const(expr) {
                self.scopes.binding_mut(item.binding_id).from_const = true;
            }
        }

        // check binding is resolved. Resolvable (via resolved_or_err) does not do this for us. (TODO/FIXME)
        if self.stage.must_resolve() {
            if let Binding { type_id: None, .. } = self.binding_by_id(item.binding_id) {
                return Err(ResolveError::new(item, ResolveErrorKind::CannotResolve(item.ident.to_string()), self.module_path))
            }
        }

        self.types_resolved(item, None)
    }

    /// Resolves a destructuring let binding, e.g. `let Struct { a, b } = value;`. The pattern must be irrefutable.
    fn resolve_let_pattern(self: &mut Self, item: &mut ast::LetPattern) -> ResolveResult {
        // refutability is structural (independent of type resolution), so reject refutable patterns up front
        Self::check_pattern_irrefutable(&item.pattern).map_err(|kind| ResolveError::new(&item.pattern, kind, self.module_path))?;
        // resolve the optional type annotation and apply it as a hint to the subject expression
        let hint = match &mut item.ty {
            Some(inline_type) => self.resolve_inline_type(inline_type)?,
            None => None,
        };
        self.resolve_expression(&mut item.expr, hint)?;
        let subject_type_id = item.expr.type_id(self);
        self.resolve_pattern(&mut item.pattern, subject_type_id)?;
        self.resolved(item)
    }

    /// Resolves a unary operation. A prefix operator applied to a custom (non-primitive) type that
    /// implements the corresponding intrinsic unary operator trait (`Neg` for `-`, `Not` for `!`) is
    /// lowered to a call of that trait's method (e.g. `-a` -> `a.neg()`, `!a` -> `a.not()`); primitive
    /// operands keep the built-in behavior.
    fn resolve_unary_op(self: &mut Self, item: &mut ast::Expression, expected_type: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::UnaryOperator::*;
        let unary = item.as_unary_op_mut().ice()?;
        let op = unary.op;
        self.resolve_expression(&mut unary.expr, expected_type)?;
        let operand_type_id = unary.expr.type_id(self);
        match op {
            Not => {
                // `!` is logical-not for bool and bitwise-not for integers; the result type matches the
                // operand. Only commit once the operand type is known, otherwise a premature `bool` here
                // would conflict with an integer operand inferred in a later pass.
                match operand_type_id {
                    Some(operand_type_id) if self.type_by_id(operand_type_id).is_integer() => {
                        self.set_type_id(item.as_unary_op_mut().ice()?, operand_type_id)?;
                    },
                    Some(operand_type_id) if !self.type_by_id(operand_type_id).is_primitive() => {
                        // custom type: dispatch through the `Not` trait, lowering `!a` to `a.not()`
                        if self.dispatch_unary_op(item, Not, operand_type_id, expected_type)? {
                            return Ok(());
                        }
                    },
                    Some(_) => {
                        if let Some(expected_type_id) = expected_type {
                            self.check_type_accepted_for(item, self.primitive_type_id(Type::bool)?, expected_type_id)?;
                        }
                        self.set_type_id(item.as_unary_op_mut().ice()?, self.primitive_type_id(Type::bool)?)?;
                    },
                    None => {},
                }
            },
            Minus => {
                if let Some(operand_type_id) = operand_type_id {
                    if self.type_by_id(operand_type_id).is_primitive() {
                        // built-in numeric negation (and the prior no-op behavior for any other primitive)
                        self.set_type_id(item.as_unary_op_mut().ice()?, operand_type_id)?;
                    } else if self.dispatch_unary_op(item, Minus, operand_type_id, expected_type)? {
                        // custom type: dispatched through the `Neg` trait, lowering `-a` to `a.neg()`
                        return Ok(());
                    }
                }
            },
            Plus => {
                if let Some(operand_type_id) = operand_type_id {
                    self.set_type_id(item.as_unary_op_mut().ice()?, operand_type_id)?;
                }
            },
        }
        let unary = item.as_unary_op_mut().ice()?;
        self.types_resolved(&unary.expr, None)?;
        self.types_resolved(item, expected_type)
    }

    /// Resolves a literal type if it is annotated, otherwise let the parent expression pick a concrete type.
    fn resolve_literal(self: &mut Self, item: &mut ast::Literal, expected_type: Option<TypeId>) -> ResolveResult {
        use self::ast::LiteralValue as LV;

        if let LV::Bool(_) = item.value {
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
        } else if let LV::Map(_) = item.value {
            self.resolve_map_literal(item, expected_type)?;
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

        // apply expected type if known
        if let Some(expected_type_id) = expected_type_id {
            self.validate_expected_container_type(item, expected_type_id, |ty| ty.as_array().is_some())?;
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

        // synthesize the type if not yet created, otherwise propagate the inferred element type
        if type_id.is_none() {
            let new_type_id = self.scopes.insert_type(None, Type::Array(Array { type_id: elements_type_id }));
            item.set_type_id(self, new_type_id);
        } else if let Some(elements_type_id) = elements_type_id {
            self.update_array_element_type(item, elements_type_id)?;
        }

        self.types_resolved(item, expected_type_id)
    }

    /// Resolves a map literal and creates the required map type.
    fn resolve_map_literal(self: &mut Self, item: &mut ast::Literal, expected_type_id: Option<TypeId>) -> ResolveResult {

        let mut key_type_id = None;
        let mut value_type_id = None;

        // apply expected type if known
        if let Some(expected_type_id) = expected_type_id {
            self.validate_expected_container_type(item, expected_type_id, |ty| ty.as_map().is_some())?;
        }

        // if we have a type for the map, pick up its known key/value types to apply to the entries
        if let Some(&Type::Map(MapType { key_type_id: k, value_type_id: v })) = self.item_type(item) {
            key_type_id = k;
            value_type_id = v;
        }

        let type_id = item.type_id(self);
        let map_literal = item.value.as_map_mut().ice_msg("Expected map type, got something else")?;

        // try to infer key/value types from the entries
        if key_type_id.is_none() || value_type_id.is_none() {
            for (key, value) in &mut map_literal.entries {
                self.resolve_expression(key, key_type_id)?;
                self.resolve_expression(value, value_type_id)?;
                if key_type_id.is_none() {
                    key_type_id = key.type_id(self);
                }
                if value_type_id.is_none() {
                    value_type_id = value.type_id(self);
                }
            }
        }

        // resolve entries against the (possibly newly) known key/value types to aid their inference
        if key_type_id.is_some() || value_type_id.is_some() {
            for (key, value) in &mut map_literal.entries {
                self.resolve_expression(key, key_type_id)?;
                self.resolve_expression(value, value_type_id)?;
            }
        }

        // synthesize the type if not yet created, otherwise propagate the inferred key/value types
        if type_id.is_none() {
            let new_type_id = self.scopes.insert_type(None, Type::Map(MapType { key_type_id, value_type_id }));
            item.set_type_id(self, new_type_id);
        } else {
            self.update_map_types(item, key_type_id, value_type_id)?;
        }

        self.types_resolved(item, expected_type_id)
    }
}

/// Support MetaContainer for Scopes so that methods that need to follow type_ids can be implemented once and be used in both
/// the Resolver where types are stored in Scopes and the Compiler where types are a stored in a Vec.
impl<'ctx> MetaContainer for Resolver<'ctx> {
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type {
        self.scopes.type_ref(type_id)
    }
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type {
        self.scopes.type_mut(type_id)
    }
    fn type_flat_name(self: &Self, type_id: TypeId) -> Option<&String> {
        self.scopes.type_flat_name(type_id)
    }
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
    fn function_by_id(self: &Self, function_id: FunctionId) -> &Function {
        self.scopes.function_by_id(function_id)
    }
    fn function_by_id_mut(self: &mut Self, function_id: FunctionId) -> &mut Function {
        self.scopes.function_by_id_mut(function_id)
    }
}