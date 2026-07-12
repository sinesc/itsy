use std::collections::HashMap;
use crate::{prelude::*, ItemIndex, VariantIndex};
use crate::frontend::resolver::scopes::Scopes;
use crate::frontend::resolver::error::{OptionToResolveError, ResolveResult, ResolveError, ResolveErrorKind};
use crate::frontend::parser::types::ParsedModule;
use crate::frontend::ast::Statement;
use crate::shared::MetaContainer;
use crate::shared::meta::{Array, Struct, Enum, EnumVariant, Type, FunctionKind, ConstantValue};
use crate::shared::typed_ids::{ScopeId, TypeId};
use crate::shared::numeric::Numeric;
use crate::bytecode::VMFunc;
use crate::bytecode::marshal::{ApiType, ApiTypeKind};

/// Maps bare API type names (e.g. `MyStruct`) to their namespaced form under the API typename
/// (e.g. `MyAPI::MyStruct`), mirroring how API functions are namespaced. Built-in type names
/// (primitives, `String`) are not registered as API types and are left unqualified.
pub(super) struct ApiNamespace {
    prefix: &'static str,
    api_type_names: Set<&'static str>,
}

impl ApiNamespace {
    /// Returns the namespaced name for an API type, or the name unchanged if it is not a registered
    /// API type (i.e. a built-in such as a primitive or `String`).
    fn qualify(self: &Self, name: &str) -> String {
        if self.api_type_names.contains(name) {
            format!("{}::{}", self.prefix, name)
        } else {
            name.to_string()
        }
    }
}

/// Recursively resolves an API [ApiType] descriptor to a [TypeId] in the root scope. Named types are
/// looked up (with `str` normalized to `String` and API types qualified under the API namespace via
/// `ns`); arrays are created as anonymous `[ element ]` types, which the resolver matches structurally
/// so they unify with array literals in scripts.
pub(super) fn resolve_type_id(scopes: &mut Scopes, api_type: &ApiType, ns: &ApiNamespace, fn_name: &str, position: &str) -> ResolveResult<TypeId> {
    match api_type {
        ApiType::Name(type_name) => {
            let type_name = if *type_name == "str" { "String" } else { *type_name };
            let type_name = ns.qualify(type_name);
            scopes.type_id(ScopeId::ROOT, &type_name)
                .ice_msg(&format!("Unknown type '{}' encountered in rust fn '{}' {} position", type_name, fn_name, position))
        },
        ApiType::Array(element) => {
            let element_type_id = resolve_type_id(scopes, element, ns, fn_name, position)?;
            Ok(scopes.insert_type(None, Type::Array(Array { type_id: Some(element_type_id) })))
        },
    }
}

/// Insert userdefined struct/enum types from itsy_api! to global scope.
pub(super) fn insert<T: VMFunc<T>>(scopes: &mut Scopes, primitives: &HashMap<&Type, TypeId>) -> ResolveResult<ApiNamespace> {

    // get list of type defs provided by itsy_api! via VMFunc
    let api_type_defs = T::resolve_types();

    // API types are namespaced under the API typename (e.g. `MyAPI::MyStruct`), mirroring how API functions are namespaced.
    let ns = ApiNamespace {
        prefix: T::api_name(),
        api_type_names: api_type_defs.iter().map(|def| def.name).collect(),
    };

    // pass 1: insert the named, empty types
    for def in &api_type_defs {
        let type_name = ns.qualify(def.name);
        if scopes.type_id(ScopeId::ROOT, &type_name).is_none() {
            let placeholder = match &def.kind {
                ApiTypeKind::Struct { .. } => Type::Struct(Struct { fields: Map::new(), impl_traits: Map::new() }),
                ApiTypeKind::PrimitiveEnum { .. } | ApiTypeKind::DataEnum { .. } => Type::Enum(Enum { variants: Vec::new(), impl_traits: Map::new(), primitive: None }),
            };
            scopes.insert_type(Some(&type_name), placeholder);
        }
    }

    // pass 2: resolve fields/variants against the now-known set of names
    let api_discriminant_type_id = *primitives.get(&Type::i32).ice()?;
    let api_discriminant_size = Type::i32.primitive_size();

    for def in &api_type_defs {
        let type_name = ns.qualify(def.name);
        let type_id = scopes.type_id(ScopeId::ROOT, &type_name).ice_msg(&format!("Failed to register API type '{}'", type_name))?;
        match &def.kind {
            ApiTypeKind::Struct { fields } => {
                let mut field_map = Map::new();
                for (field_name, field_type_name) in fields {
                    let field_type_id = scopes.type_id(ScopeId::ROOT, &ns.qualify(field_type_name))
                        .ice_msg(&format!("Unknown type '{}' encountered in field '{}' of API type '{}'", field_type_name, field_name, def.name))?;
                    field_map.insert(field_name.to_string(), Some(field_type_id));
                }
                *scopes.type_mut(type_id) = Type::Struct(Struct { fields: field_map, impl_traits: Map::new() });
            },
            ApiTypeKind::PrimitiveEnum { variants } => {
                let mut enum_variants = Vec::new();
                for (name, discriminant) in variants {
                    let discriminant = Numeric::Signed(*discriminant);
                    enum_variants.push((name.to_string(), EnumVariant::Simple(Some(discriminant))));
                    // C-like enum variant: a constant holding the bare discriminant
                    scopes.insert_constant(&format!("{}::{}", type_name, name), type_id, Some(type_id), ConstantValue::Discriminant(discriminant));
                }
                *scopes.type_mut(type_id) = Type::Enum(Enum { variants: enum_variants, impl_traits: Map::new(), primitive: Some((api_discriminant_type_id, api_discriminant_size)) });
            },
            ApiTypeKind::DataEnum { variants } => {
                let mut enum_variants = Vec::new();
                for (index, (name, field_type_names)) in variants.iter().enumerate() {
                    let path = format!("{}::{}", type_name, name);
                    if field_type_names.is_empty() {
                        // unit variant of a data enum: a constant; the discriminant is unused (the heap
                        // tag is the variant index) but must be present for as_simple() lookups.
                        let discriminant = Numeric::Signed(index as i64);
                        enum_variants.push((name.to_string(), EnumVariant::Simple(Some(discriminant))));
                        scopes.insert_constant(&path, type_id, Some(type_id), ConstantValue::Discriminant(discriminant));
                    } else {
                        let mut field_type_ids = Vec::new();
                        for field_type_name in field_type_names {
                            let field_type_id = scopes.type_id(ScopeId::ROOT, &ns.qualify(field_type_name))
                                .ice_msg(&format!("Unknown type '{}' encountered in variant '{}' of API type '{}'", field_type_name, name, def.name))?;
                            field_type_ids.push(Some(field_type_id));
                        }
                        // data variant: a constructor function tagged with its variant index
                        scopes.insert_function(&path, Some(type_id), field_type_ids.clone(), Some(FunctionKind::Variant(type_id, index as VariantIndex)));
                        enum_variants.push((name.to_string(), EnumVariant::Data(field_type_ids)));
                    }
                }
                *scopes.type_mut(type_id) = Type::Enum(Enum { variants: enum_variants, impl_traits: Map::new(), primitive: None });
            },
        }
    }
    Ok(ns)
}

/// Verifies that every function the host declared as callable (an `itsy_api!` `callables { ... }` block)
/// is defined in the script's entry module with a signature matching the host's declaration. This makes
/// a host/script mismatch a compile-time [ResolveError] instead of a heap corruption at call time. Run
/// after resolution so the script functions' types are known; reuses `ns`/[resolve_type_id] to map the
/// host-declared types to the same registered type ids the script uses.
pub(super) fn validate_callables<T: VMFunc<T>>(scopes: &mut Scopes, ns: &ApiNamespace, entry_scope_id: ScopeId, entry_module: &ParsedModule) -> ResolveResult<()> {
    for (name, ret_api, arg_apis) in T::resolve_callables() {
        // Resolve the host-declared types first (resolve_type_id may insert anonymous array types, hence
        // the &mut borrow). Done before looking at the script function to keep borrows non-overlapping.
        let expected_args: Vec<TypeId> = arg_apis.iter()
            .map(|api| resolve_type_id(scopes, api, ns, name, "argument"))
            .collect::<ResolveResult<_>>()?;
        let expected_ret: Option<TypeId> = match &ret_api {
            Some(api) => Some(resolve_type_id(scopes, api, ns, name, "return")?),
            None => None,
        };
        // Locate the script's function definition (for a precise error position).
        let function_def = entry_module.statements().find_map(|s| match s {
            Statement::Function(f) if f.shared.sig.ident.name == name => Some(f),
            _ => None,
        });
        let function_def = match function_def {
            Some(f) => f,
            None => return Err(ResolveError::global(ResolveErrorKind::UndefinedFunction(name.to_string()), &entry_module.path)),
        };
        // Fetch its resolved signature (cloned, to release the scopes borrow before comparisons).
        let (script_args, script_ret) = match scopes.constant_id(entry_scope_id, name, TypeId::VOID).and_then(|cid| scopes.constant_function_id(cid)) {
            Some(fid) => {
                let func = scopes.function_ref(fid);
                (func.arg_type_ids(scopes).clone(), func.ret_type_id(scopes))
            },
            None => return Err(ResolveError::new(function_def, ResolveErrorKind::UndefinedFunction(name.to_string()), &entry_module.path)),
        };
        // Arity.
        if script_args.len() != expected_args.len() {
            return Err(ResolveError::new(function_def, ResolveErrorKind::NumberOfArguments(name.to_string(), expected_args.len() as ItemIndex, script_args.len() as ItemIndex), &entry_module.path));
        }
        // Argument types.
        for (expected, actual) in expected_args.iter().zip(script_args.iter()) {
            let actual = actual.ice()?;
            if !types_compatible(scopes, *expected, actual) {
                return Err(ResolveError::new(function_def, ResolveErrorKind::TypeMismatch(type_name(scopes, actual), type_name(scopes, *expected)), &entry_module.path));
            }
        }
        // Return type. A missing host return means the script function must be `void`.
        match (expected_ret, script_ret) {
            (Some(expected), Some(actual)) if !types_compatible(scopes, expected, actual) => {
                return Err(ResolveError::new(function_def, ResolveErrorKind::TypeMismatch(type_name(scopes, actual), type_name(scopes, expected)), &entry_module.path));
            },
            (None, Some(actual)) if !matches!(scopes.type_ref(actual), Type::void) => {
                return Err(ResolveError::new(function_def, ResolveErrorKind::TypeMismatch(type_name(scopes, actual), "void".to_string()), &entry_module.path));
            },
            (Some(expected), None) => {
                return Err(ResolveError::new(function_def, ResolveErrorKind::TypeMismatch("void".to_string(), type_name(scopes, expected)), &entry_module.path));
            },
            _ => {},
        }
    }
    Ok(())
}

/// Structural type compatibility for callable-signature validation. Named types (structs, enums) and
/// primitives are interned, so equal [TypeId]s already match; only structural types (arrays) need a
/// recursive element comparison, since the host's anonymous `[ T ]` gets a fresh id.
fn types_compatible(scopes: &Scopes, a: TypeId, b: TypeId) -> bool {
    if a == b {
        return true;
    }
    match (scopes.type_ref(a), scopes.type_ref(b)) {
        (Type::Array(a), Type::Array(b)) => match (a.type_id, b.type_id) {
            (Some(a), Some(b)) => types_compatible(scopes, a, b),
            _ => false,
        },
        _ => false,
    }
}

/// Renders a readable type name for error messages, recursing into arrays.
fn type_name(scopes: &Scopes, type_id: TypeId) -> String {
    if let Some(name) = scopes.type_flat_name(type_id) {
        name.clone()
    } else if let Type::Array(array) = scopes.type_ref(type_id) {
        format!("[ {} ]", array.type_id.map_or_else(|| "_".to_string(), |e| type_name(scopes, e)))
    } else {
        format!("{}", scopes.type_ref(type_id))
    }
}
