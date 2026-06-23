//! AST type checker and resolver.

#[path="scopes/scopes.rs"]
mod scopes;
mod stage;
mod exhaustiveness;
mod apinamespace;
mod intrinsic;
pub mod error;
pub mod resolved;

use crate::{prelude::*, VariantIndex};
use crate::{ItemIndex, STACK_ADDRESS_TYPE};
use crate::frontend::parser::types::ParsedProgram;
use crate::frontend::ast::{self, Visibility, Positioned, Typeable, Resolvable};
use crate::frontend::resolver::error::{OptionToResolveError, ResolveResult, ResolveError, ResolveErrorKind};
use crate::frontend::resolver::resolved::ResolvedProgram;
use crate::shared::{Progress, MetaContainer, parts_to_path, path_to_parts};
use crate::shared::meta::{Array, MapType, Struct, Enum, EnumVariant, Trait, ImplTrait, Type, FunctionKind, Binding, Constant, ConstantValue, Callable, Function};
use crate::shared::typed_ids::{BindingId, ScopeId, TypeId, ConstantId, FunctionId};
use crate::shared::numeric::Numeric;
use crate::bytecode::{VMFunc, builtins::builtin_types};
use crate::frontend::resolver::intrinsic::{INTRINSIC_CAST_TRAITS, IntrinsicCast, INTRINSIC_OP_TRAITS, IntrinsicOp, IntrinsicResult, ResultTypeInfo, OptionTypeInfo};

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
    /// the `ToString` trait/`to_string`). A cast to such a target that is not a built-in primitive cast
    /// is lowered to a call of the trait method on the cast operand, provided the operand implements it.
    intrinsic_casts : &'ctx UnorderedMap<TypeId, IntrinsicCast>,
    /// Maps a binary operator to the intrinsic operator trait that backs it (e.g. `+` -> the `Add`
    /// trait/`add`). An operator applied to a type with no built-in meaning for it is lowered to a call of
    /// the trait method on the operands, provided they implement it. `Eq` is keyed under `Equal` and backs
    /// both `==` and `!=` (`a != b` lowers to the negated `eq` result).
    intrinsic_ops   : &'ctx UnorderedMap<ast::BinaryOperator, IntrinsicOp>,
    /// Type id of the built-in `Error` trait. Fixed `Err` payload type of every synthesized `Result<T>`.
    error_trait_type_id: TypeId,
    /// Registry of synthesized `Result<T>` enums (keyed by enum type id), persistent across resolution
    /// passes so `Ok`/`Err` calls can be recognized and bound to the right variant constructors.
    result_types    : &'ctx mut UnorderedMap<TypeId, ResultTypeInfo>,
    /// Registry of synthesized `Option<T>` enums (keyed by enum type id), persistent across resolution
    /// passes so `Some(x)` calls and bare `None` can be recognized and bound to the right variants.
    option_types    : &'ctx mut UnorderedMap<TypeId, OptionTypeInfo>,
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

    // register intrinsic conversion traits (e.g. `ToString`) and the cast targets they back. These are
    // ordinary traits that scripts implement via `impl ToString for MyType { ... }`; a cast to the trait's
    // target type (e.g. `myValue as String`) that isn't a built-in primitive cast lowers to a call of the
    // trait's method (e.g. `myValue.to_string()`). Add a row here to make a future trait drive a cast.
    let mut intrinsic_trait_names = Vec::new();
    let mut intrinsic_casts = UnorderedMap::new();
    for intrinsic in INTRINSIC_CAST_TRAITS {
        let target_type_id = *primitives.get(&intrinsic.target).ice()?;
        let trait_type_id = scopes.insert_type(Some(intrinsic.trait_name), Type::Trait(Trait { provided: Map::new(), required: Map::new() }));
        // required method `fn <method>(self: Self) -> <target>`, registered exactly as the resolver would
        // for a source-defined trait method so impl-matching and vtable construction pick it up.
        let constant_id = scopes.insert_function(
            &format!("{}::{}", intrinsic.trait_name, intrinsic.method),
            Some(target_type_id),
            vec![ Some(trait_type_id) ],
            Some(FunctionKind::Method(trait_type_id)),
        );
        scopes.type_mut(trait_type_id).as_trait_mut().ice()?.required.insert(intrinsic.method.to_string(), Some(constant_id));
        intrinsic_trait_names.push(intrinsic.trait_name.to_string());
        intrinsic_casts.insert(target_type_id, IntrinsicCast { trait_type_id, method: intrinsic.method });
    }

    // register intrinsic operator traits (e.g. `Add`, `Eq`). These are ordinary traits scripts implement via
    // `impl Add for MyType { ... }`; an operator on a type with no built-in meaning for it lowers to a call
    // of the trait method (e.g. `a + b` to `a.add(b)`, `a == b` to `a.eq(b)`). Add a row to
    // INTRINSIC_OP_TRAITS to back a further operator.
    let mut intrinsic_ops = UnorderedMap::new();
    for intrinsic in INTRINSIC_OP_TRAITS {
        let trait_type_id = scopes.insert_type(Some(intrinsic.trait_name), Type::Trait(Trait { provided: Map::new(), required: Map::new() }));
        // required method `fn <method>(self: Self, rhs: <rhs>) -> <result>`, where rhs defaults to Self
        // (arithmetic, bitwise, equality) but can be a fixed primitive for shift operators.
        let rhs_type_id = match &intrinsic.rhs {
            IntrinsicResult::SelfType => trait_type_id,
            IntrinsicResult::Type(ty) => *primitives.get(ty).ice()?,
        };
        let result_type_id = match &intrinsic.result {
            IntrinsicResult::SelfType => trait_type_id,
            IntrinsicResult::Type(ty) => *primitives.get(ty).ice()?,
        };
        let constant_id = scopes.insert_function(
            &format!("{}::{}", intrinsic.trait_name, intrinsic.method),
            Some(result_type_id),
            vec![ Some(trait_type_id), Some(rhs_type_id) ],
            Some(FunctionKind::Method(trait_type_id)),
        );
        scopes.type_mut(trait_type_id).as_trait_mut().ice()?.required.insert(intrinsic.method.to_string(), Some(constant_id));
        intrinsic_trait_names.push(intrinsic.trait_name.to_string());
        intrinsic_ops.insert(intrinsic.op, IntrinsicOp { trait_type_id, rhs_type_id, method: intrinsic.method });
    }

    // register the built-in `Error` trait so scripts can `impl Error for MyType { ... }`. Required method:
    // `fn description(self: Self) -> String`, registered exactly as a source-defined trait method would be.
    let string_type_id = *primitives.get(&Type::String).ice()?;
    let error_trait_type_id = scopes.insert_type(Some("Error"), Type::Trait(Trait { provided: Map::new(), required: Map::new() }));
    let error_description_id = scopes.insert_function(
        "Error::description",
        Some(string_type_id),
        vec![ Some(error_trait_type_id) ],
        Some(FunctionKind::Method(error_trait_type_id)),
    );
    scopes.type_mut(error_trait_type_id).as_trait_mut().ice()?.required.insert("description".to_string(), Some(error_description_id));
    intrinsic_trait_names.push("Error".to_string());

    // registry of synthesized `Result<T>` enums, populated on demand as `Result<T>` annotations and
    // `Ok`/`Err` constructors are resolved. Persistent across passes (the Resolver is rebuilt each pass).
    let mut result_types = UnorderedMap::new();
    // registry of synthesized `Option<T>` enums, populated on demand as `Option<T>` annotations and
    // `Some`/`None` constructs are resolved. Persistent across passes (the Resolver is rebuilt each pass).
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
            error_trait_type_id,
            result_types    : &mut result_types,
            option_types    : &mut option_types,
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
                if type_contains_self(&scopes, type_id) {
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

/// Returns whether the type identified by `start` contains itself, directly or transitively.
fn type_contains_self(scopes: &scopes::Scopes, start: TypeId) -> bool {
    fn reaches(scopes: &scopes::Scopes, current: TypeId, start: TypeId, visited: &mut Set<TypeId>) -> bool {
        for next in scopes.type_ref(current).contained_type_ids() {
            if next == start {
                return true;
            }
            if visited.insert(next) && reaches(scopes, next, start, visited) {
                return true;
            }
        }
        false
    }
    reaches(scopes, start, start, &mut Set::new())
}

/// General utility methods.
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
                Some(self.scopes.insert_function(
                    name,
                    Some(result_type_id),
                    arg_type_ids.iter().map(|id| Some(*id)).collect::<Vec<Option<TypeId>>>(),
                    Some(FunctionKind::Builtin(type_id, builtin_type))
                ))
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
                Some(self.scopes.insert_function(
                    name,
                    Some(result_type_id),
                    arg_type_ids.iter().map(|id| Some(*id)).collect::<Vec<Option<TypeId>>>(),
                    Some(FunctionKind::Builtin(type_id, builtin_type))
                ))
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
                Some(self.scopes.insert_function(
                    name,
                    Some(result_type_id),
                    arg_type_ids.iter().map(|id| Some(*id)).collect::<Vec<Option<TypeId>>>(),
                    Some(FunctionKind::Builtin(type_id, builtin_type))
                ))
            },
        })
    }

    /// Creates an Array<T> type and returns its TypeId.
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

    /// Recursively unifies two structurally matching types by propagating resolved inner type-ids
    /// onto their still-unresolved counterpart (in either direction). This lets a fully resolved
    /// compound type help resolve a partially inferred one, e.g. comparing `[ i32 ]` with `[ ? ]`
    /// fills in the latter's element type instead of failing the equality check.
    fn unify_type_ids(self: &mut Self, first_type_id: TypeId, second_type_id: TypeId) -> ResolveResult {
        if first_type_id == second_type_id {
            return Ok(());
        }
        // unify arrays: propagate known element type onto the unresolved counterpart
        let inner = match (self.type_by_id(first_type_id).as_array(), self.type_by_id(second_type_id).as_array()) {
            (Some(first), Some(second)) => Some((first.type_id, second.type_id)),
            _ => None,
        };
        if let Some((first_inner, second_inner)) = inner {
            match (first_inner, second_inner) {
                (Some(first_inner), Some(second_inner)) => self.unify_type_ids(first_inner, second_inner)?,
                (Some(first_inner), None) => self.type_by_id_mut(second_type_id).as_array_mut().ice()?.type_id = Some(first_inner),
                (None, Some(second_inner)) => self.type_by_id_mut(first_type_id).as_array_mut().ice()?.type_id = Some(second_inner),
                (None, None) => {},
            }
        }
        // unify maps: propagate known key/value types onto the unresolved counterpart
        let map_inner = match (self.type_by_id(first_type_id).as_map(), self.type_by_id(second_type_id).as_map()) {
            (Some(first), Some(second)) => Some((first.key_type_id, second.key_type_id, first.value_type_id, second.value_type_id)),
            _ => None,
        };
        if let Some((first_key, second_key, first_value, second_value)) = map_inner {
            // unify keys
            match (first_key, second_key) {
                (Some(fk), Some(sk)) => self.unify_type_ids(fk, sk)?,
                (Some(fk), None) => {
                    let map_ty = self.type_by_id_mut(second_type_id).as_map_mut().ice()?;
                    map_ty.key_type_id = Some(fk);
                },
                (None, Some(sk)) => {
                    let map_ty = self.type_by_id_mut(first_type_id).as_map_mut().ice()?;
                    map_ty.key_type_id = Some(sk);
                },
                (None, None) => {},
            }
            // unify values
            match (first_value, second_value) {
                (Some(fv), Some(sv)) => self.unify_type_ids(fv, sv)?,
                (Some(fv), None) => {
                    let map_ty = self.type_by_id_mut(second_type_id).as_map_mut().ice()?;
                    map_ty.value_type_id = Some(fv);
                },
                (None, Some(sv)) => {
                    let map_ty = self.type_by_id_mut(first_type_id).as_map_mut().ice()?;
                    map_ty.value_type_id = Some(sv);
                },
                (None, None) => {},
            }
        }
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

    /// Returns the type id of the `Result<T>` data enum for the given success type, synthesizing it on
    /// first use. `Result<T>` is represented as an anonymous data enum with variants `Ok(T)` and
    /// `Err(Error)`; structurally identical results (same `T`) dedupe to a single type id. On first
    /// synthesis the two variant constructors are created and recorded in `result_types`.
    fn synthesize_result_type(self: &mut Self, ok_type_id: TypeId) -> TypeId {
        let error_trait_type_id = self.error_trait_type_id;
        let variants = vec![
            ("Ok".to_string(), EnumVariant::Data(vec![ Some(ok_type_id) ])),
            ("Err".to_string(), EnumVariant::Data(vec![ Some(error_trait_type_id) ])),
        ];
        // primitive: None -> data-carrying (heap) enum, like a source enum with data variants
        let ty = Type::Enum(Enum { primitive: None, variants, impl_traits: Map::new() });
        let result_type_id = self.scopes.insert_anonymous_type(true, ty);
        if !self.result_types.contains_key(&result_type_id) {
            // names embed the enum type id to stay unique across distinct `Result<T>`; they are never
            // looked up by name (callers bind to the constructor's constant id directly).
            let raw = result_type_id.into_usize();
            let ok_constructor = self.scopes.insert_function(
                &format!("Result#{raw}::Ok"), Some(result_type_id), vec![ Some(ok_type_id) ],
                Some(FunctionKind::Variant(result_type_id, 0)),
            );
            let err_constructor = self.scopes.insert_function(
                &format!("Result#{raw}::Err"), Some(result_type_id), vec![ Some(error_trait_type_id) ],
                Some(FunctionKind::Variant(result_type_id, 1)),
            );
            self.result_types.insert(result_type_id, ResultTypeInfo { ok_type_id, ok_constructor, err_constructor });
        }
        result_type_id
    }

    /// Returns the type id of the `Option<T>` enum for the given inner type, synthesizing it on first
    /// use. `Option<T>` is represented as an anonymous mixed enum with variants `Some(T)` (data) and
    /// `None` (simple/unit); structurally identical options (same `T`) dedupe to a single type id.
    /// On first synthesis the `Some` constructor function and the `None` unit-variant constant are
    /// created and recorded in `option_types`.
    pub(crate) fn synthesize_option_type(self: &mut Self, some_type_id: TypeId) -> TypeId {
        let variants = vec![
            ("Some".to_string(), EnumVariant::Data(vec![ Some(some_type_id) ])),
            ("None".to_string(), EnumVariant::Simple(None)),
        ];
        // primitive: None -> data-carrying (heap) enum, since the Some variant carries data
        let ty = Type::Enum(Enum { primitive: None, variants, impl_traits: Map::new() });
        let option_type_id = self.scopes.insert_anonymous_type(true, ty);
        if !self.option_types.contains_key(&option_type_id) {
            // mangled, never looked up by name: callers bind to the constant/constructor id directly
            let raw = option_type_id.into_usize();
            let some_constructor = self.scopes.insert_function(
                &format!("Option#{raw}::Some"), Some(option_type_id), vec![ Some(some_type_id) ],
                Some(FunctionKind::Variant(option_type_id, 0)),
            );
            // None is a unit variant: a constant, like a user enum's `VariantKind::Simple`. The numeric is
            // a placeholder (reference-enum codegen tags by variant index, ignoring it).
            let none_constant = self.scopes.insert_constant(
                &format!("Option#{raw}::None"), option_type_id, Some(option_type_id),
                ConstantValue::Discriminant(Numeric::Unsigned(1)),
            );
            self.option_types.insert(option_type_id, OptionTypeInfo {
                some_type_id,
                some_constructor,
                none_constant,
            });
        }
        option_type_id
    }

    /// Returns the type id of the `Generator<V>` / `Generator<K, V>` carrier for the given key/value
    /// types, synthesizing it on first use. The carrier is an anonymous struct with sentinel fields
    /// `$value` (and `$key` for the keyed form); the `$` keeps the field names unwritable by user
    /// code, so the structural `generator_signature` check can identify it. Structurally identical
    /// generators (same key/value) dedupe to a single type id, like `Result<T>`. The frozen frame the
    /// carrier will hold at runtime is *not* modelled as typed fields here — that stays opaque bytes.
    fn synthesize_generator_type(self: &mut Self, key_type_id: Option<TypeId>, value_type_id: TypeId) -> TypeId {
        let mut fields = Map::new();
        fields.insert("$value".to_string(), Some(value_type_id));
        if let Some(key_type_id) = key_type_id {
            fields.insert("$key".to_string(), Some(key_type_id));
        }
        let ty = Type::Struct(Struct { fields, impl_traits: Map::new() });
        self.scopes.insert_anonymous_type(true, ty)
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

    /// Looks up the constant for an intrinsic operator trait's method on the given target type, used to
    /// dispatch a compound assignment in-place. Concrete implementors resolve to their own impl method
    /// (static dispatch); a trait-object target resolves to the trait's required method (virtual dispatch).
    /// Returns `None` while the implementation is not yet known.
    fn intrinsic_op_method_constant(self: &Self, type_id: TypeId, intrinsic: &IntrinsicOp) -> Option<ConstantId> {
        if let Some(impl_traits) = self.type_by_id(type_id).impl_traits_map() {
            if let Some(impl_trait) = impl_traits.get(&intrinsic.trait_type_id) {
                if let Some(Some(constant_id)) = impl_trait.functions.get(intrinsic.method) {
                    return Some(*constant_id);
                }
            }
        }
        if self.type_by_id(type_id).is_trait_object() {
            if let Some(trt) = self.type_by_id(intrinsic.trait_type_id).as_trait() {
                if let Some(Some(constant_id)) = trt.required.get(intrinsic.method) {
                    return Some(*constant_id);
                }
            }
        }
        None
    }

    /// Builds an unresolved method-call expression `receiver.method(args...)` for the resolver to lower
    /// intrinsic-trait operations (casts, operators) onto the regular method-call machinery.
    fn make_method_call(receiver: ast::Expression, method: &str, args: Vec<ast::Expression>, position: ast::Position) -> ast::Expression {
        let access = ast::Expression::BinaryOp(Box::new(ast::BinaryOp {
            position,
            op: ast::BinaryOperator::Access,
            left: ast::BinaryOperand::Expression(receiver),
            right: ast::BinaryOperand::Member(ast::Member {
                position,
                ident: ast::Ident { position, name: method.to_string() },
                type_id: None,
                constant_id: None,
            }),
            type_id: None,
            op_resolved: false,
        }));
        ast::Expression::BinaryOp(Box::new(ast::BinaryOp {
            position,
            op: ast::BinaryOperator::Call,
            left: ast::BinaryOperand::Expression(access),
            right: ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args }),
            type_id: None,
            op_resolved: false,
        }))
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
}

/// Methods to resolve individual AST structures.
impl<'ast, 'ctx> Resolver<'ctx> where 'ast: 'ctx {

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
            Assignment(assignment) => self.resolve_assignment(assignment, expected_result),
            BinaryOp(binary_op) => self.resolve_binary_op(binary_op, expected_result),
            UnaryOp(unary_op) => self.resolve_unary_op(unary_op, expected_result),
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

    /// Resolves a result definition `Result<T>` into its synthesized two-variant data enum
    /// `Ok(T)` / `Err(Error)`.
    fn resolve_result_type(self: &mut Self, item: &mut ast::ResultDef) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        if let (None, Some(ok_type_id)) = (item.type_id, self.resolve_inline_type(&mut item.ok_type)?) {
            item.type_id = Some(self.synthesize_result_type(ok_type_id));
        }
        self.types_resolved(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves `Option<T>` into its synthesized `Some(T)` / `None` enum.
    fn resolve_option_type(self: &mut Self, item: &mut ast::OptionDef) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        if let (None, Some(some_type_id)) = (item.type_id, self.resolve_inline_type(&mut item.some_type)?) {
            item.type_id = Some(self.synthesize_option_type(some_type_id));
        }
        self.types_resolved(item, None)?;
        Ok(item.type_id)
    }

    /// Resolves a generator definition `Generator<V>` / `Generator<K, V>` into its synthesized struct
    /// carrier.
    fn resolve_generator_type(self: &mut Self, item: &mut ast::GeneratorDef) -> ResolveResult<Option<TypeId>> {
        if item.type_id.is_some() && item.is_resolved(self) {
            return Ok(item.type_id);
        }
        // resolve the value type and the optional key type first
        let value_type_id = self.resolve_inline_type(&mut item.value_type)?;
        let key_type_id = match &mut item.key_type {
            Some(key_type) => self.resolve_inline_type(key_type)?,
            None => None,
        };
        // synthesize once the value (and the key, if this generator has one) are resolved
        if item.type_id.is_none() {
            if let Some(value_type_id) = value_type_id {
                if item.key_type.is_none() || key_type_id.is_some() {
                    item.type_id = Some(self.synthesize_generator_type(key_type_id, value_type_id));
                }
            }
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
        }
        self.scope_id = parent_scope_id;
        self.resolved(item)
    }

    /// Resolves a trait definition block.
    fn resolve_trait_type(self: &mut Self, item: &mut ast::TraitDef) -> ResolveResult {
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
            self.unify_type_ids(item_type_id, expected_result)?;
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
            let path = self.make_path(&item.path.segments);
            item.constant_id = self.scopes.constant_id(self.scope_id, &path, TypeId::VOID);
            // builtin function
            if item.path.segments.len() == 2 {
                if let Some(type_id) = self.scopes.type_id(ScopeId::ROOT, &item.path.segments[0]) {
                    if let Some(constant_id) = self.try_create_scalar_builtin(&item.path.segments[1].name, type_id)? {
                        item.constant_id = Some(constant_id);
                    }
                }
            }
            if item.constant_id.is_none() {
                // bare `None` is the unit variant of a synthesized `Option<T>`; bind it once the expected
                // Option type is known (`return None`, `let x: Option<_> = None`, match arm bodies, ...)
                if item.constant_id.is_none() && item.path.segments.len() == 1 && item.path.segments[0].name == "None" {
                    if let Some(option_type_id) = expected_result.filter(|t| self.option_types.contains_key(t)) {
                        item.constant_id = Some(self.option_types.get(&option_type_id).ice()?.none_constant);
                    }
                }
            }
            if item.constant_id.is_none() {
                // `Ok`/`Err` are bound to a `Result<T>` variant constructor by try_bind_result_constructor
                // once a `Result` context is known, which can be as late as literal/type inference. Defer
                // their existence check to must_resolve so a valid `Ok`/`Err` isn't prematurely rejected;
                // genuine misuse (e.g. `?` in a non-`Result` function) gets a dedicated diagnostic there.
                // Same for `Some`/`None` — defer so a valid `Some`/`None` isn't prematurely rejected.
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
                self.check_type_accepted_for(item, else_type_id, if_type_id)?;
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
        // unify arm result types so that partially-resolved compound types (e.g. empty map literal
        // `[ => ]` with unknown value type) pick up concrete types from sibling arms
        if item.branches.len() > 1 {
            if let Some(first_type_id) = item.branches[0].1.type_id(self) {
                for (_, block) in item.branches.iter().skip(1) {
                    if let Some(block_type_id) = block.type_id(self) {
                        self.unify_type_ids(first_type_id, block_type_id)?;
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
    fn resolve_assignment(self: &mut Self, item: &mut ast::Assignment, expected_result: Option<TypeId>) -> ResolveResult {
        use ast::BinaryOperator::*;
        // shift-assigns take an independent integer shift amount on the right that must not be unified with
        // the target type (mirrors the standalone `<<`/`>>` handling in resolve_binary_op)
        let shift_assign = matches!(item.op, ShlAssign | ShrAssign);
        // Resolve the assignment target first *without* imposing the value's type on it, then resolve the
        // value against the target's type. This ordering means a genuine mismatch is reported as
        // "expected <target>, got <value>"; resolving the target against the value's type instead (the
        // reverse) would name the operands the wrong way round for an assignment.
        self.resolve_expression(&mut item.left, None)?;
        let left_type_id = item.left.type_id(self);
        // classify compound assignment once the target type is known. Numeric/string targets for arithmetic,
        // and integer targets for bitwise/shift, keep their built-in codegen; a custom target (variable, field
        // or index) is dispatched in-place to the operator-trait method so the target is evaluated only once.
        if let (Some(base_op), Some(left_type_id)) = (item.op.arithmetic_assign_base(), left_type_id) {
            let left_type = self.type_by_id(left_type_id);
            let is_bitwise_shift = matches!(base_op, BitAnd | BitOr | BitXor | Shl | Shr);
            let is_builtin = if is_bitwise_shift {
                left_type.is_integer()
            } else {
                left_type.is_numeric() || left_type.is_string()
            };
            if is_builtin {
                item.op_dispatch = Some(ast::CompoundDispatch::Builtin);
            } else {
                let intrinsic = self.intrinsic_ops.get(&base_op).copied().ice()?;
                let rhs_expected = if matches!(base_op, Shl | Shr) {
                    Some(intrinsic.rhs_type_id) // i64
                } else {
                    Some(left_type_id) // Self
                };
                self.resolve_expression(&mut item.right, rhs_expected)?;
                if let Some(right_type_id) = item.right.type_id(self) {
                    if !matches!(base_op, Shl | Shr) {
                        self.check_type_accepted_for(item, right_type_id, left_type_id)?; // rhs must be Self
                    }
                }
                if let Some(constant_id) = self.intrinsic_op_method_constant(left_type_id, &intrinsic) {
                    item.op_dispatch = Some(ast::CompoundDispatch::Method(constant_id));
                } else if self.stage.must_resolve() {
                    let type_name = self.type_name(left_type_id);
                    let trait_name = self.type_name(intrinsic.trait_type_id);
                    return Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path));
                }
                item.type_id = Some(TypeId::VOID);
                return self.types_resolved(item, expected_result);
            }
        }
        self.resolve_expression(&mut item.right, if shift_assign { None } else { left_type_id })?;
        let right_type_id = item.right.type_id(self);
        // if the target's type can't be determined on its own, infer it from the value instead (e.g. an
        // untyped array's element type, or an uninitialized binding learned from what is assigned to it)
        if left_type_id.is_none() && !shift_assign {
            self.resolve_expression(&mut item.left, right_type_id)?;
        }
        // bitwise/shift compound assigns require integer operands
        if matches!(item.op, BitAndAssign | BitOrAssign | BitXorAssign | ShlAssign | ShrAssign) {
            if let Some(left_type_id) = item.left.type_id(self) {
                if !self.type_by_id(left_type_id).is_integer() {
                    return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires integer operands", item.op)), self.module_path));
                }
            }
            if let Some(right_type_id) = item.right.type_id(self) {
                if !self.type_by_id(right_type_id).is_integer() {
                    return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires an integer right operand", item.op)), self.module_path));
                }
            }
        }
        // an assignment is an expression that produces no value, so it must match the expected type (if any) against void
        item.type_id = Some(TypeId::VOID);
        self.types_resolved(item, expected_result)
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
                    // operand implements the backing trait: lower to a call of the trait method
                    self.rewrite_cast_to_method(item, intrinsic.method, expected_result)
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
                // propagate inner types between operands so a partially inferred compound type
                // (e.g. `[ ? ]`) is completed from the other side before the equality check below
                if let (Some(left_type_id), Some(right_type_id)) = (left_type_id, right_type_id) {
                    self.unify_type_ids(left_type_id, right_type_id)?;
                }
                if let Some(common_type_id) = left_type_id.or(right_type_id) {
                    self.set_type_id(item.left.as_expression_mut().ice()?, common_type_id)?;
                    self.set_type_id(item.right.as_expression_mut().ice()?, common_type_id)?;
                    // operator overloading: `==`/`!=` on a type implementing the intrinsic `Eq` trait are
                    // lowered to a call of its `eq` method (overriding the built-in deep comparison). The `Eq`
                    // trait backs both operators and is keyed under `Equal` in `intrinsic_ops`; `!=` negates
                    // the `eq` result. Types that do not implement `Eq` keep the built-in comparison, so a
                    // missing impl is not an error here. Ordering comparisons (`<`, `>`, ...) never dispatch.
                    if matches!(item.op, Equal | NotEqual) {
                        let intrinsic = self.intrinsic_ops.get(&Equal).copied().ice()?;
                        if self.type_accepted_for(common_type_id, intrinsic.trait_type_id) {
                            let negate = item.op == NotEqual;
                            return self.rewrite_eq_op_to_method(item, intrinsic.method, negate, expected_result);
                        }
                    }
                }
            },
            Add | Sub | Mul | Div | Rem => {
                self.resolve_expression(item.left.as_expression_mut().ice()?, expected_result)?;
                let left_type_id = item.left.type_id(self);
                self.resolve_expression(item.right.as_expression_mut().ice()?, left_type_id)?;
                let type_id = item.type_id(self);
                let right_type_id = item.right.type_id(self);
                if let Some(common_type_id) = type_id.or(left_type_id).or(right_type_id) {
                    self.set_type_id(item, common_type_id)?;
                    self.set_type_id(item.left.as_expression_mut().ice()?, common_type_id)?;
                    self.set_type_id(item.right.as_expression_mut().ice()?, common_type_id)?;
                    let common_type = self.type_by_id(common_type_id);
                    // built-in arithmetic applies to numeric types (all operators) and to `String` (the `+`
                    // concatenation operator); any other type dispatches through an operator trait
                    let is_builtin = common_type.is_numeric() || (item.op == Add && common_type.is_string());
                    if is_builtin {
                        item.op_resolved = true;
                    } else {
                        // operator overloading: an arithmetic operator on a custom type is lowered to a call
                        // of the corresponding operator trait's method (e.g. `a + b` -> `a.add(b)`). The node
                        // stays unresolved (op_resolved == false) until the operand is found to implement the
                        // trait (then lowered) or resolution stalls (dedicated error).
                        let intrinsic = self.intrinsic_ops.get(&item.op).copied().ice()?;
                        if self.type_accepted_for(common_type_id, intrinsic.trait_type_id) {
                            return self.rewrite_binary_op_to_method(item, intrinsic.method, expected_result);
                        } else if self.stage.must_resolve() {
                            let type_name = self.type_name(common_type_id);
                            let trait_name = self.type_name(intrinsic.trait_type_id);
                            return Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path));
                        }
                    }
                }
            },
            Range | RangeInclusive => {
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
            BitAnd | BitOr | BitXor => {
                // both operands share a single integer type, like arithmetic, but floats are rejected;
                // custom types dispatch through the corresponding operator trait
                self.resolve_expression(item.left.as_expression_mut().ice()?, expected_result)?;
                let left_type_id = item.left.type_id(self);
                self.resolve_expression(item.right.as_expression_mut().ice()?, left_type_id)?;
                let type_id = item.type_id(self);
                let right_type_id = item.right.type_id(self);
                if let Some(common_type_id) = type_id.or(left_type_id).or(right_type_id) {
                    self.set_type_id(item, common_type_id)?;
                    self.set_type_id(item.left.as_expression_mut().ice()?, common_type_id)?;
                    self.set_type_id(item.right.as_expression_mut().ice()?, common_type_id)?;
                    let common_type = self.type_by_id(common_type_id);
                    let is_builtin = common_type.is_integer();
                    if is_builtin {
                        item.op_resolved = true;
                    } else if common_type.is_primitive() {
                        // primitive type that is not an integer (e.g. float, bool, string) — reject early
                        return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires integer operands", item.op)), self.module_path));
                    } else {
                        let intrinsic = self.intrinsic_ops.get(&item.op).copied().ice()?;
                        if self.type_accepted_for(common_type_id, intrinsic.trait_type_id) {
                            return self.rewrite_binary_op_to_method(item, intrinsic.method, expected_result);
                        } else if self.stage.must_resolve() {
                            let type_name = self.type_name(common_type_id);
                            let trait_name = self.type_name(intrinsic.trait_type_id);
                            return Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path));
                        }
                    }
                }
            },
            Shl | Shr => {
                // result type follows the left operand; the right operand (shift amount) is an
                // independent integer type (cast to u32 at compile time), so it is not unified with the left;
                // custom types dispatch through the corresponding operator trait
                self.resolve_expression(item.left.as_expression_mut().ice()?, expected_result)?;
                let left_type_id = item.left.type_id(self);
                let type_id = item.type_id(self);
                let mut is_builtin = false;
                if let Some(common_type_id) = type_id.or(left_type_id) {
                    self.set_type_id(item, common_type_id)?;
                    self.set_type_id(item.left.as_expression_mut().ice()?, common_type_id)?;
                    let common_type = self.type_by_id(common_type_id);
                    is_builtin = common_type.is_integer();
                    if is_builtin {
                        item.op_resolved = true;
                    } else if common_type.is_primitive() {
                        // primitive type that is not an integer (e.g. float, bool, string) — reject early
                        return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires an integer left operand", item.op)), self.module_path));
                    } else {
                        let intrinsic = self.intrinsic_ops.get(&item.op).copied().ice()?;
                        if self.type_accepted_for(common_type_id, intrinsic.trait_type_id) {
                            return self.rewrite_shift_op_to_method(item, intrinsic.method, expected_result);
                        } else if self.stage.must_resolve() {
                            let type_name = self.type_name(common_type_id);
                            let trait_name = self.type_name(intrinsic.trait_type_id);
                            return Err(ResolveError::new(item, ResolveErrorKind::MissingTraitImplementation(type_name, trait_name), self.module_path));
                        }
                    }
                }
                self.resolve_expression(item.right.as_expression_mut().ice()?, None)?;
                // integer checks only apply to the builtin path; custom types go through trait dispatch
                if is_builtin {
                    if let Some(left_type_id) = item.type_id(self) {
                        if !self.type_by_id(left_type_id).is_integer() {
                            return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires an integer left operand", item.op)), self.module_path));
                        }
                    }
                    if let Some(right_type_id) = item.right.type_id(self) {
                        if !self.type_by_id(right_type_id).is_integer() {
                            return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} requires an integer shift amount", item.op)), self.module_path));
                        }
                    }
                }
            },
            Index | IndexWrite => {
                self.resolve_expression(item.left.as_expression_mut().ice()?, None)?;
                let left_type_id = item.left.type_id(self);
                let is_map = left_type_id.map_or(false, |id| self.type_by_id(id).as_map().is_some());
                let is_array = left_type_id.map_or(false, |id| self.type_by_id(id).as_array().is_some());
                if left_type_id.is_some() && !is_array && !is_map {
                    return Err(ResolveError::new(item, ResolveErrorKind::InvalidOperation(format!("{} does not implement index access", &item.left)), self.module_path));
                }
                if is_map {
                    // map index: the index operand is the key type, the result is the value type
                    let key_type_id = if let Some(&Type::Map(MapType { key_type_id, .. })) = self.item_type(&item.left) { key_type_id } else { None };
                    self.resolve_expression(item.right.as_expression_mut().ice()?, key_type_id)?;
                    // infer the map key type from the index operand if still unknown
                    if key_type_id.is_none() {
                        if let (Some(index_type_id), Some(left_type_id)) = (item.right.type_id(self), item.left.type_id(self)) {
                            if let Some(map_ty) = self.type_by_id_mut(left_type_id).as_map_mut() {
                                map_ty.key_type_id = Some(index_type_id);
                            }
                        }
                    }
                    // if we expect a particular result type, set it now
                    if let Some(expected_result) = expected_result {
                        self.set_type_id(item, expected_result)?;
                    }
                    // if we know the result type, propagate it to the map value type
                    if let Some(result_type_id) = item.type_id(self) {
                        let value_type_id = if let Some(&Type::Map(MapType { value_type_id, .. })) = self.item_type(&item.left) { value_type_id } else { None };
                        if let Some(value_type_id) = value_type_id {
                            self.check_type_accepted_for(item, result_type_id, value_type_id)?;
                        } else if let Some(left_type_id) = item.left.type_id(self) {
                            if let Some(map_ty) = self.type_by_id_mut(left_type_id).as_map_mut() {
                                map_ty.value_type_id = Some(result_type_id);
                            }
                        }
                    }
                    // if we know the map value type, set the result type to Option<value_type>
                    // Only for Index (read), not IndexWrite (assignment result type is the assigned value)
                    if item.op == ast::BinaryOperator::Index {
                        if let Some(&Type::Map(MapType { value_type_id: Some(value_type_id), .. })) = self.item_type(&item.left) {
                            let opt_type_id = self.synthesize_option_type(value_type_id);
                            self.set_type_id(item, opt_type_id)?;
                        }
                    }
                } else {
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
                        let is_array = left_ty.as_array().is_some();
                        let is_map = left_ty.as_map().is_some();
                        let is_scalar = left_ty.is_float() || left_ty.is_integer() || left_ty.is_string();
                        // a generator carrier is structurally a struct; recognize it before the struct path
                        let is_generator = self.generator_signature(left_type_id).is_some();
                        // constituent trait ids if the receiver is a multiple-trait bound (`A + B`)
                        let trait_bound_ids = left_ty.as_trait_bound().cloned();
                        let constant_id = if is_generator {
                            // lookup generator builtin (next/value/key) or try to create it
                            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                                .or(self.try_create_generator_builtin(member_name, owning_type_id)?)
                        } else if is_array {
                            // lookup array builtin or try to create it
                            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                                .or(self.try_create_array_builtin(item.left.as_expression().ice()?, member_name, owning_type_id)?)
                        } else if is_map {
                            // lookup map builtin or try to create it
                            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                                .or(self.try_create_map_builtin(item.left.as_expression().ice()?, member_name, owning_type_id)?)
                        } else if is_scalar {
                            // lookup scalar builtin or try to create it
                            self.scopes.constant_id(self.scope_id, member_name, owning_type_id)
                                .or(self.try_create_scalar_builtin(member_name, owning_type_id)?)
                        } else if let Some(trait_bound_ids) = trait_bound_ids {
                            // trait bound receiver: search each constituent trait for the method (required or provided)
                            let mut found = None;
                            for trait_id in trait_bound_ids {
                                let type_name = self.type_flat_name(trait_id).ice_msg("Unnamed trait")?.clone();
                                let path = self.make_path(&[ &type_name, member_name ]);
                                if let Some(constant_id) = self.scopes.constant_id(self.scope_id, &path, trait_id) {
                                    found = Some(constant_id);
                                    break;
                                }
                            }
                            found
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
                            self.set_type_id(member, function.callable_type_id)?;
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
                            Type::Map(_) => {
                                // as for arrays, builtin resolution above may temporarily fail while the
                                // map's key/value types are not yet resolved
                            },
                            // any other receiver (trait, trait bound, enum, scalar): the method/builtin lookup
                            // above failed, so the member does not exist. report it rather than ICE-ing. we name
                            // the receiver type, not a trait, because a receiver may satisfy several traits and we
                            // cannot know which the user intended.
                            _ => {
                                let member = item.right.as_member().ice_msg("Member access using a non-member")?;
                                let member_name = member.ident.name.clone();
                                let type_name = self.type_name(left_type_id);
                                return Err(ResolveError::new(member, ResolveErrorKind::UndefinedMethod(member_name, type_name), self.module_path));
                            },
                        }
                    }
                    if let Some(type_id) = item.right.type_id(self) {
                        self.set_type_id(item, type_id)?; // ignored if member is constant
                    }
                }
            },
            Call => {
                // bind a bare `Ok`/`Err` callee to its `Result<T>` variant constructor before generic
                // resolution (which then handles argument checking and the result type like any other call)
                self.try_bind_result_constructor(item, expected_result)?;
                self.try_bind_option_constructor(item, expected_result)?;
                // resolve function
                let call_func = item.left.as_expression_mut().ice()?;
                self.resolve_expression(call_func, None)?;
                let call_args = item.right.as_argument_list_mut().ice()?;
                // an array builtin like `arr.push(x)` can't resolve while the array's element type is
                // unknown, but for `push`/`insert` that type is exactly what the value argument supplies.
                self.try_infer_array_element_from_args(call_func, call_args)?;
                self.try_infer_map_types_from_args(call_func, call_args)?;
                self.rewrite_method_call_to_constant_call(call_func, call_args)?;
                let num_args = call_args.args.len();
                // set return type from signature
                if let Some(function_type_id) = call_func.type_id(self) {

                    // check type is actually callable
                    let func = match self.type_by_id(function_type_id).as_callable() {
                        Some(callable) => callable.clone(), //FIXME borrow
                        None => {
                            let type_name = self.type_name(function_type_id);
                            return Err(ResolveError::new(item, ResolveErrorKind::NotCallable(type_name), self.module_path));
                        }
                    };

                    // A trait method declared to return `Self` records its return type as the declaring
                    // trait. When invoked on a trait-object receiver whose static type is broader (e.g. a
                    // multiple-trait bound `A + B`), propagate the receiver's type to the result so methods
                    // from the other constituent traits remain callable on the returned value.
                    let ret_type_id = func.ret_type_id.map(|ret_type_id| {
                        let receiver_type_id = item.left.as_expression()
                            .and_then(|e| e.as_constant())
                            .and_then(|c| c.constant_id)
                            .and_then(|cid| self.scopes.constant_function_id(cid))
                            .filter(|&fid| matches!(self.scopes.function_ref(fid).kind, Some(FunctionKind::Method(declaring_id)) if declaring_id == ret_type_id))
                            .and(item.right.as_argument_list().and_then(|a| a.args.first()))
                            .and_then(|receiver| receiver.type_id(self));
                        match receiver_type_id {
                            Some(receiver_type_id) if receiver_type_id != ret_type_id && self.type_accepted_for(receiver_type_id, ret_type_id) => receiver_type_id,
                            _ => ret_type_id,
                        }
                    });
                    if let Some(ret_type_id) = ret_type_id {
                        self.set_type_id(item, ret_type_id)?;
                    }

                    if let (Some(item_type_id), Some(expected_result)) = (item.type_id(self), expected_result) {
                        self.check_type_accepted_for(item, item_type_id, expected_result)?;
                    }
                    // !TODO! try to fill in closure types here.

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
                    // a concrete result passed where a trait is expected stays concrete (coerced to a
                    // trait object at compile time); only its acceptance is verified, not its identity
                    if self.type_by_id(expected_result).is_trait_object() {
                        if let Some(item_type_id) = item.type_id(self) {
                            self.check_type_accepted_for(item, item_type_id, expected_result)?;
                        }
                    } else {
                        self.set_type_id(item, expected_result)?;
                    }
                }
            },
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

    /// Resolves a unary operation.
    fn resolve_unary_op(self: &mut Self, item: &mut ast::UnaryOp, expected_type: Option<TypeId>) -> ResolveResult {
        use crate::frontend::ast::UnaryOperator::*;
        self.resolve_expression(&mut item.expr, expected_type)?;
        match item.op {
            Not => {
                // `!` is logical-not for bool and bitwise-not for integers; the result type matches the
                // operand. Only commit once the operand type is known, otherwise a premature `bool` here
                // would conflict with an integer operand inferred in a later pass.
                match item.expr.type_id(self) {
                    Some(operand_type_id) if self.type_by_id(operand_type_id).is_integer() => {
                        self.set_type_id(item, operand_type_id)?;
                    },
                    Some(_) => {
                        if let Some(expected_type_id) = expected_type {
                            self.check_type_accepted_for(item, self.primitive_type_id(Type::bool)?, expected_type_id)?;
                        }
                        self.set_type_id(item, self.primitive_type_id(Type::bool)?)?;
                    },
                    None => {},
                }
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

    /// Resolves a map literal and creates the required map type.
    fn resolve_map_literal(self: &mut Self, item: &mut ast::Literal, expected_type_id: Option<TypeId>) -> ResolveResult {

        let mut key_type_id = None;
        let mut value_type_id = None;

        // apply expected type if known
        if let Some(expected_type_id) = expected_type_id {
            if self.type_by_id(expected_type_id).as_map().is_some() {
                self.set_type_id(item, expected_type_id)?;
            } else {
                let expected_name = self.type_name(expected_type_id);
                let received_name = if let Some(type_id) = item.type_id { self.type_name(type_id) } else { "?".to_string() };
                return Err(ResolveError::new(item, ResolveErrorKind::TypeMismatch(received_name, expected_name), self.module_path));
            }
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

        // create the map type if we don't have one yet, otherwise propagate inferred key/value types
        if type_id.is_none() {
            let new_type_id = self.scopes.insert_type(None, Type::Map(MapType {
                key_type_id,
                value_type_id,
            }));
            item.set_type_id(self, new_type_id);
        } else {
            let map_type_id = item.type_id(self).ice()?;
            if let Some(key_type_id) = key_type_id {
                let map_ty = self.type_by_id_mut(map_type_id).as_map_mut().ice()?;
                if map_ty.key_type_id.is_none() {
                    map_ty.key_type_id = Some(key_type_id);
                }
            }
            if let Some(value_type_id) = value_type_id {
                let map_ty = self.type_by_id_mut(map_type_id).as_map_mut().ice()?;
                if map_ty.value_type_id.is_none() {
                    map_ty.value_type_id = Some(value_type_id);
                }
            }
        }

        self.types_resolved(item, expected_type_id)
    }
}

/// Utility methods for resolve_binary_op.
impl<'ast, 'ctx> Resolver<'ctx> where 'ast: 'ctx {

    /// For a call like `arr.push(x)` whose receiver is an array of still-unknown element type, infer that
    /// element type from the value argument.
    /// Note: method names/parameter index for inference are hardcoded in value_index match block and may need to be updated for new array builtins.
    fn try_infer_array_element_from_args(self: &mut Self, call_exp: &mut ast::Expression, call_args: &mut ast::ArgumentList) -> ResolveResult {
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
    /// value type, infer those types from the relevant arguments (the map equivalent of
    /// [`Self::try_create_array_builtin`]'s element inference). Required so methods can be called on an empty
    /// map literal `[ => ]` whose types are not yet determined.
    fn try_infer_map_types_from_args(self: &mut Self, call_exp: &mut ast::Expression, call_args: &mut ast::ArgumentList) -> ResolveResult {
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

    /// Rewrites a cast expression `expr as T` into a method call `expr.<method>()` and resolves it. Used to
    /// dispatch casts backed by an intrinsic conversion trait (e.g. `ToString`) through the regular method
    /// call machinery, which handles both static dispatch and dynamic dispatch on trait objects.
    fn rewrite_cast_to_method(self: &mut Self, item: &mut ast::Expression, method: &str, expected_result: Option<TypeId>) -> ResolveResult {
        let cast = item.as_cast_mut().ice()?;
        let position = cast.position;
        let receiver = std::mem::replace(&mut cast.expr, ast::Expression::void(position));
        *item = Self::make_method_call(receiver, method, Vec::new(), position);
        self.resolve_expression(item, expected_result)
    }

    /// Rewrites an arithmetic binary operation `a OP b` into a method call `a.<method>(b)` and resolves it.
    /// Used to dispatch operators backed by an intrinsic operator trait (e.g. `Add`) through the regular
    /// method-call machinery, reusing its static and dynamic dispatch.
    fn rewrite_binary_op_to_method(self: &mut Self, item: &mut ast::BinaryOp, method: &str, expected_result: Option<TypeId>) -> ResolveResult {
        let position = item.position;
        let receiver = match std::mem::replace(&mut item.left, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator left operand is not an expression"),
        };
        let rhs = match std::mem::replace(&mut item.right, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator right operand is not an expression"),
        };
        match Self::make_method_call(receiver, method, vec![ rhs ], position) {
            ast::Expression::BinaryOp(call) => *item = *call,
            _ => return Self::ice("make_method_call did not produce a binary operation"),
        }
        self.resolve_binary_op(item, expected_result)
    }

    /// Rewrites an equality comparison `a == b` / `a != b` on a type implementing the intrinsic `Eq` trait
    /// into a call of its `eq` method and resolves it. `==` lowers to `a.eq(b)`; `!=` lowers to the negated
    /// form `a.eq(b) == false`, which the built-in boolean comparison then evaluates. Mirrors
    /// `rewrite_binary_op_to_method` (used for the arithmetic operator traits) but accounts for `eq`
    /// returning `bool` rather than `Self`.
    fn rewrite_eq_op_to_method(self: &mut Self, item: &mut ast::BinaryOp, method: &str, negate: bool, expected_result: Option<TypeId>) -> ResolveResult {
        let position = item.position;
        let receiver = match std::mem::replace(&mut item.left, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator left operand is not an expression"),
        };
        let rhs = match std::mem::replace(&mut item.right, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator right operand is not an expression"),
        };
        let call = Self::make_method_call(receiver, method, vec![ rhs ], position);
        if negate {
            // `a != b` becomes `a.eq(b) == false`: comparing the boolean `eq` result against `false`
            // negates it through the built-in boolean comparison (the `false` operand is a primitive, so
            // this re-resolves as a regular comparison and is not itself dispatched through `Eq`).
            let false_literal = ast::Expression::Literal(ast::Literal {
                position,
                value: ast::LiteralValue::Bool(false),
                type_name: None,
                type_id: None,
            });
            item.op = ast::BinaryOperator::Equal;
            item.left = ast::BinaryOperand::Expression(call);
            item.right = ast::BinaryOperand::Expression(false_literal);
            item.type_id = None;
            item.op_resolved = false;
        } else {
            match call {
                ast::Expression::BinaryOp(call) => *item = *call,
                _ => return Self::ice("make_method_call did not produce a binary operation"),
            }
        }
        self.resolve_binary_op(item, expected_result)
    }

    /// Rewrites a shift operation `a << b` / `a >> b` on a type implementing the intrinsic `Shl`/`Shr` trait
    /// into a call of its `shl`/`shr` method. Mirrors `rewrite_binary_op_to_method` (used for the arithmetic
    /// operator traits) but the shift amount (right operand) is already resolved as an independent integer type.
    fn rewrite_shift_op_to_method(self: &mut Self, item: &mut ast::BinaryOp, method: &str, expected_result: Option<TypeId>) -> ResolveResult {
        let position = item.position;
        let receiver = match std::mem::replace(&mut item.left, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator left operand is not an expression"),
        };
        let rhs = match std::mem::replace(&mut item.right, ast::BinaryOperand::ArgumentList(ast::ArgumentList { position, args: Vec::new() })) {
            ast::BinaryOperand::Expression(expr) => expr,
            _ => return Self::ice("binary operator right operand is not an expression"),
        };
        match Self::make_method_call(receiver, method, vec![ rhs ], position) {
            ast::Expression::BinaryOp(call) => *item = *call,
            _ => return Self::ice("make_method_call did not produce a binary operation"),
        }
        self.resolve_binary_op(item, expected_result)
    }

    /// If the call target is a bare, still-unbound `Ok`/`Err`, bind it to the matching `Result<T>` variant
    /// constructor so the generic call resolution below handles argument checking and the result type.
    /// `T` comes from the expected result type when available, otherwise (for `Ok`) is inferred from the
    /// argument. `Err` without an expected `Result` type cannot infer `T` and is left unbound (reported as
    /// an undefined identifier in the final stage).
    fn try_bind_result_constructor(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        let variant = match item.left.as_expression().and_then(|e| e.as_constant()) {
            Some(c) if c.constant_id.is_none() && c.path.segments.len() == 1 => match c.path.segments[0].name.as_str() {
                "Ok" => 0 as VariantIndex,
                "Err" => 1 as VariantIndex,
                _ => return Ok(()),
            },
            _ => return Ok(()),
        };
        // an expected result type pins T directly; this is the common case (`return Ok(x)`, match arms, ...)
        let expected_result_type = expected_result.filter(|t| self.result_types.contains_key(t));
        let result_type_id = match (variant, expected_result_type) {
            (_, Some(result_type_id)) => Some(result_type_id),
            // Ok(x): infer T from the argument's type
            (0, None) => {
                if let Some(arg) = item.right.as_argument_list_mut().ice()?.args.first_mut() {
                    self.resolve_expression(arg, None)?;
                    arg.type_id(self).map(|ok_type_id| self.synthesize_result_type(ok_type_id))
                } else {
                    None
                }
            },
            // Err(e): cannot determine T from the error alone; wait for / require context
            (_, None) => None,
        };
        if let Some(result_type_id) = result_type_id {
            let info = *self.result_types.get(&result_type_id).ice()?;
            let constructor = if variant == 0 { info.ok_constructor } else { info.err_constructor };
            item.left.as_expression_mut().ice()?.as_constant_mut().ice()?.constant_id = Some(constructor);
        } else if self.stage.must_resolve() {
            // the constructor could not be bound to any `Result<T>`: there is no result type to infer from
            // (e.g. `?` or a bare `Err`/`Ok` in a function that does not return `Result`). Report this
            // directly instead of letting the unbound `Ok`/`Err` surface as a misleading "undefined identifier".
            let from_try = item.right.as_argument_list().and_then(|a| a.args.first())
                .and_then(|arg| arg.as_variable())
                .map_or(false, |var| var.ident.name == "try$err" || var.ident.name == "try$ok");
            let expected = expected_result.map(|type_id| self.type_name(type_id));
            return Err(ResolveError::new(item, ResolveErrorKind::ResultOutsideResultContext(from_try, expected), self.module_path));
        }
        Ok(())
    }

    /// If the call target is a bare, still-unbound `Some`, bind it to the matching `Option<T>` variant
    /// constructor so the generic call resolution below handles argument checking and the result type.
    /// `T` comes from the expected result type when available, otherwise is inferred from the argument.
    /// `None` is a bare constant (not a call) and is handled in `resolve_constant`.
    fn try_bind_option_constructor(self: &mut Self, item: &mut ast::BinaryOp, expected_result: Option<TypeId>) -> ResolveResult {
        // only `Some(x)` is a call; `None` is a bare unit-variant constant, bound in resolve_constant
        let is_some = matches!(
            item.left.as_expression().and_then(|e| e.as_constant()),
            Some(c) if c.constant_id.is_none() && c.path.segments.len() == 1 && c.path.segments[0].name == "Some"
        );
        if !is_some {
            return Ok(());
        }
        // an expected Option type pins T directly; otherwise infer T from the argument
        let option_type_id = match expected_result.filter(|t| self.option_types.contains_key(t)) {
            Some(option_type_id) => Some(option_type_id),
            None => {
                if let Some(arg) = item.right.as_argument_list_mut().ice()?.args.first_mut() {
                    self.resolve_expression(arg, None)?;
                    arg.type_id(self).map(|some_type_id| self.synthesize_option_type(some_type_id))
                } else {
                    None
                }
            }
        };
        if let Some(option_type_id) = option_type_id {
            let info = *self.option_types.get(&option_type_id).ice()?;
            item.left.as_expression_mut().ice()?.as_constant_mut().ice()?.constant_id = Some(info.some_constructor);
        }
        Ok(())
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