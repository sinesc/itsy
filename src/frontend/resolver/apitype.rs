use std::collections::HashMap;
use crate::{prelude::*, VariantIndex};
use crate::frontend::resolver::scopes::Scopes;
use crate::frontend::resolver::error::{OptionToResolveError, ResolveResult};
use crate::shared::meta::{Array, Struct, Enum, EnumVariant, Type, FunctionKind, ConstantValue};
use crate::shared::typed_ids::{ScopeId, TypeId};
use crate::shared::numeric::Numeric;
use crate::bytecode::VMFunc;
use crate::marshal::{ApiType, ApiTypeKind};

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
pub(super) fn insert_all<T: VMFunc<T>>(scopes: &mut Scopes, primitives: &HashMap<&Type, TypeId>) -> ResolveResult<ApiNamespace> {

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
