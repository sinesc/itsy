//! Shared code for frontend and bytecode

pub mod numeric;
pub mod typed_ids;
pub mod meta;
pub mod error;

use crate::shared::{typed_ids::{TypeId, BindingId, ConstantId, FunctionId}, meta::{Type, Binding, Constant, Array, MapType, Callable, Function, Enum, EnumVariant, Struct}};
use crate::prelude::*;

/// A container holding AST metadata
pub trait MetaContainer {
    /// Returns a reference to the type.
    fn type_by_id(self: &Self, type_id: TypeId) -> &Type;
    /// Returns a mutable reference to the type.
    fn type_by_id_mut(self: &mut Self, type_id: TypeId) -> &mut Type;
    /// Returns the flat type name or None if that cannot represent a type (e.g. an array)
    fn type_flat_name(self: &Self, type_id: TypeId) -> Option<&String>;
    /// Returns whether given_type_id is acceptable to a binding of the accepted_type_id
    fn type_accepted_for(self: &Self, given_type_id: TypeId, accepted_type_id: TypeId) -> bool {
        if given_type_id == accepted_type_id {
            true
        } else {
            match (self.type_by_id(given_type_id), self.type_by_id(accepted_type_id)) {
                (&Type::Array(Array { type_id: Some(given_type_id), .. }), &Type::Array(Array { type_id: Some(accepted_type_id), .. })) => {
                    self.type_accepted_for(given_type_id, accepted_type_id)
                },
                (&Type::Map(MapType { key_type_id: Some(given_key), value_type_id: Some(given_value) }), &Type::Map(MapType { key_type_id: Some(accepted_key), value_type_id: Some(accepted_value) })) => {
                    self.type_accepted_for(given_key, accepted_key) && self.type_accepted_for(given_value, accepted_value)
                },
                (Type::Callable(Callable { arg_type_ids: given_arg_type_ids, ret_type_id: Some(given_ret_type_id) }), Type::Callable(Callable { arg_type_ids: accepted_arg_type_ids, ret_type_id: Some(accepted_ret_type_id) })) => {
                    self.type_accepted_for(*given_ret_type_id, *accepted_ret_type_id) &&
                        given_arg_type_ids.len() == accepted_arg_type_ids.len() &&
                        given_arg_type_ids.iter().zip(accepted_arg_type_ids.iter()).all(|item| {
                            if let (Some(given_arg_type_id), Some(accepted_arg_type_id)) = item {
                                self.type_accepted_for(*given_arg_type_id, *accepted_arg_type_id)
                            } else {
                                false
                            }
                        })
                },
                (given, Type::Trait(_)) if given.impl_traits_map().is_some() => {
                    given.impl_traits_map().unwrap().contains_key(&accepted_type_id)
                },
                // a multiple-trait bound is accepted for a single trait if the bound includes that trait
                (Type::TraitBound(given_trait_ids), Type::Trait(_)) => {
                    given_trait_ids.contains(&accepted_type_id)
                },
                // a type is accepted for a multiple-trait bound if it satisfies every constituent trait
                (given, Type::TraitBound(accepted_trait_ids)) => {
                    accepted_trait_ids.iter().all(|trait_id| match given {
                        Type::TraitBound(given_trait_ids) => given_trait_ids.contains(trait_id),
                        _ => given.impl_traits_map().map_or(false, |impl_traits| impl_traits.contains_key(trait_id)),
                    })
                },
                _ => false,
            }
        }
    }
    /// Returns whether the given types are the same. (Anonymous types may have differing type ids but still be the same type)
    fn type_equals(self: &Self, first_type_id: TypeId, second_type_id: TypeId) -> bool {
        if first_type_id == second_type_id {
            true
        } else {
            match (self.type_by_id(first_type_id), self.type_by_id(second_type_id)) {
                (&Type::Array(Array { type_id: Some(first_type_id), .. }), &Type::Array(Array { type_id: Some(second_type_id), .. })) => {
                    self.type_equals(first_type_id, second_type_id)
                },
                (&Type::Map(MapType { key_type_id: Some(first_key), value_type_id: Some(first_value) }), &Type::Map(MapType { key_type_id: Some(second_key), value_type_id: Some(second_value) })) => {
                    self.type_equals(first_key, second_key) && self.type_equals(first_value, second_value)
                },
                (Type::Callable(Callable { arg_type_ids: given_arg_type_ids, ret_type_id: Some(given_ret_type_id) }), Type::Callable(Callable { arg_type_ids: accepted_arg_type_ids, ret_type_id: Some(accepted_ret_type_id) })) => {
                    self.type_equals(*given_ret_type_id, *accepted_ret_type_id) &&
                        given_arg_type_ids.len() == accepted_arg_type_ids.len() &&
                        given_arg_type_ids.iter().zip(accepted_arg_type_ids.iter()).all(|item| {
                            if let (Some(given_arg_type_id), Some(accepted_arg_type_id)) = item {
                                self.type_equals(*given_arg_type_id, *accepted_arg_type_id)
                            } else {
                                false
                            }
                        })
                },
                _ => false,
            }
        }
    }
    /// If the given type is a compiler-synthesized `Result<T>` data enum, returns the success type `T`.
    /// `Result<T>` is an anonymous `Ok(T)`/`Err(Error)` enum (see `synthesize_result_type`), so it has no
    /// flat name; this structural check lets `type_name` render it legibly in diagnostics.
    fn result_ok_type_id(self: &Self, type_id: TypeId) -> Option<TypeId> {
        if let Type::Enum(Enum { primitive: None, variants, .. }) = self.type_by_id(type_id) {
            if let [(ok_name, EnumVariant::Data(ok_fields)), (err_name, EnumVariant::Data(err_fields))] = &variants[..] {
                if ok_name == "Ok" && err_name == "Err" && ok_fields.len() == 1 && err_fields.len() == 1 {
                    if let Some(err_type_id) = err_fields[0] {
                        // the Err payload is fixed to the built-in `Error` trait
                        if matches!(self.type_by_id(err_type_id), Type::Trait(_)) && self.type_flat_name(err_type_id).map_or(false, |n| n == "Error") {
                            return ok_fields[0];
                        }
                    }
                }
            }
        }
        None
    }
    /// If the given type is a compiler-synthesized `Option<T>` enum, returns the `Some` payload type `T`.
    fn option_some_type_id(self: &Self, type_id: TypeId) -> Option<TypeId> {
        if let Type::Enum(Enum { primitive: None, variants, .. }) = self.type_by_id(type_id) {
            if let [(some_name, EnumVariant::Data(some_fields)), (none_name, EnumVariant::Simple(_))] = &variants[..] {
                if some_name == "Some" && none_name == "None" && some_fields.len() == 1 {
                    return some_fields[0];
                }
            }
        }
        None
    }
    /// If the given type is a compiler-synthesized `Generator<V>` / `Generator<K, V>` carrier, returns
    /// its `(key, value)` signature (key is `None` for the single-argument form). The carrier is an
    /// anonymous struct with sentinel fields `$value` (and `$key`); the `$` makes the names
    /// unwritable by user code, so this structural check unambiguously identifies generators and lets
    /// `type_name` render them legibly (see `synthesize_generator_type`).
    fn generator_signature(self: &Self, type_id: TypeId) -> Option<(Option<TypeId>, TypeId)> {
        if let Type::Struct(Struct { fields, .. }) = self.type_by_id(type_id) {
            let value_type_id = (*fields.get("$value")?)?;
            match fields.len() {
                1 => Some((None, value_type_id)),
                2 => {
                    let key_type_id = (*fields.get("$key")?)?;
                    Some((Some(key_type_id), value_type_id))
                },
                _ => None,
            }
        } else {
            None
        }
    }
    // Returns recursively resolved type name.
    fn type_name(self: &Self, type_id: TypeId) -> String {
        let ty = self.type_by_id(type_id);
        if let Some(type_name) = self.type_flat_name(type_id) {
            type_name.to_string()
        } else if let Some(ok_type_id) = self.result_ok_type_id(type_id) {
            format!("Result<{}>", self.type_name(ok_type_id))
        } else if let Some(some_type_id) = self.option_some_type_id(type_id) {
            format!("Option<{}>", self.type_name(some_type_id))
        } else if let Some((key_type_id, value_type_id)) = self.generator_signature(type_id) {
            match key_type_id {
                Some(key_type_id) => format!("Generator<{}, {}>", self.type_name(key_type_id), self.type_name(value_type_id)),
                None => format!("Generator<{}>", self.type_name(value_type_id)),
            }
        } else {
            match ty {
                Type::void => "void".to_string(), // void is explicitly not a named type so we have to manually give it a name here
                &Type::Array(Array { type_id: Some(type_id) }) => {
                    format!("[ {} ]", self.type_name(type_id))
                }
                Type::Array(Array { type_id: None }) => {
                    "[ ? ]".to_string()
                }
                &Type::Map(MapType { key_type_id, value_type_id }) => {
                    let key = key_type_id.map_or("?".to_string(), |type_id| self.type_name(type_id));
                    let value = value_type_id.map_or("?".to_string(), |type_id| self.type_name(type_id));
                    format!("[ {} => {} ]", key, value)
                }
                Type::Callable(callable) => {
                    let result_type_name = if let Some(type_id) = callable.ret_type_id { self.type_name(type_id) } else { "?".to_string() };
                    let arg_type_names = callable.arg_type_ids
                        .iter()
                        .map(|type_id| if let Some(type_id) = type_id { self.type_name(*type_id) } else { "?".to_string() })
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("fn({arg_type_names}) -> {result_type_name}")
                }
                Type::TraitBound(trait_ids) => {
                    trait_ids.iter().map(|&type_id| self.type_name(type_id)).collect::<Vec<_>>().join(" + ")
                }
                _ => "?".to_string()
            }
        }
    }
    /// Returns a reference to the Binding.
    fn binding_by_id(self: &Self, binding_id: BindingId) -> &Binding;
    /// Returns a mutable reference to the Binding.
    fn binding_by_id_mut(self: &mut Self, binding_id: BindingId) -> &mut Binding;
    /// Returns a reference to the Constant.
    fn constant_by_id(self: &Self, constant_id: ConstantId) -> &Constant;
    /// Returns a mutable reference to the Constant.
    fn constant_by_id_mut(self: &mut Self, constant_id: ConstantId) -> &mut Constant;
    /// Returns a reference to the Function.
    fn function_by_id(self: &Self, function_id: FunctionId) -> &Function;
    /// Returns a mutable reference to the Function.
    fn function_by_id_mut(self: &mut Self, function_id: FunctionId) -> &mut Function;
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) struct Progress {
    pub current: usize,
    pub total: usize,
}

impl Progress {
    pub fn new(current: usize, total: usize) -> Self {
        Self { current, total }
    }
    pub fn zero() -> Self {
        Self { current: 0, total: 0 }
    }
    pub fn done(self: &Self) -> bool {
        self.current == self.total
    }
}

impl fmt::Debug for Progress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.current, self.total)
    }
}

impl Add for Progress {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            current: self.current + other.current,
            total: self.total + other.total,
        }
    }
}

/// Splits a path string into its constituent parts.
pub fn path_to_parts<T: AsRef<str>>(path: T) -> Vec<String> {
    let path = path.as_ref();
    match path {
        "" => Vec::new(),
        _ => path.split("::").map(|s| s.to_string()).collect()
    }
}

/// Joins parts of a path into a string.
pub fn parts_to_path<T: AsRef<str>>(parts: &[T]) -> String {
    let parts = parts.iter().map(|p| p.as_ref()).collect::<Vec<_>>(); // todo: avoid collecting first
    parts.join("::")
}

/// Implement variant getters for an enaum.
///
/// # Examples
///
/// impl_as_getter!(EnumType {
///     pub as_variant_name: Variant -> *ResultType         // get by value
///     pub as_variant_name_ref: Variant -> &ResultType     // get by reference
///     pub as_variant_name_mut: Variant -> mut ResultType  // get by mut reference
///     pub is_variant_name: Variant ? bool                 // check if is variant
/// })
macro_rules! impl_as_getter {
    (@ref_val * $ident:ident) => { Some(*$ident) };
    (@ref_val & $ident:ident) => { Some($ident) };
    (@ref_val mut $ident:ident) => { Some($ident) };
    (@ref_val $ident:ident) => { true };
    (@ref_result * $ty:ty) => { Option<$ty> };
    (@ref_result & $ty:ty) => { Option<&$ty> };
    (@ref_result mut $ty:ty) => { Option<&mut $ty> };
    (@ref_result) => { bool };
    (@ref_self * $ty:ty) => { &$ty };
    (@ref_self & $ty:ty) => { &$ty };
    (@ref_self mut $ty:ty) => { &mut $ty };
    (@ref_self $ty:ty) => { &$ty };
    (@ref_default $ref_type:tt) => { None };
    (@ref_default) => { false };
    (@ref_fn $( #[ $attr:meta ] )* ( mut ) $vis:vis $label:ident : $variant:ident $( -> $ref_type:tt $ty:tt )? $( ? bool )? ) => {
        $( #[ $attr ] )*
        $vis fn $label (self: impl_as_getter!(@ref_self $($ref_type)? Self)) -> impl_as_getter!(@ref_result $($ref_type)? $($ty)?) {
            #[allow(unreachable_patterns,unused_variables)]
            match self {
                Self::$variant(v) => impl_as_getter!(@ref_val $($ref_type)? v),
                _ => impl_as_getter!(@ref_default $($ref_type)?),
            }
        }
    };
    (@ref_fn $( #[ $attr:meta ] )* ( $( $any:tt )? ) $vis:vis $label:ident : $variant:ident $( -> $ref_type:tt $ty:tt )? $( ? bool )? ) => {
        $( #[ $attr ] )*
        $vis const fn $label (self: impl_as_getter!(@ref_self $($ref_type)? Self)) -> impl_as_getter!(@ref_result $($ref_type)? $($ty)?) {
            #[allow(unreachable_patterns,unused_variables)]
            match self {
                Self::$variant(v) => impl_as_getter!(@ref_val $($ref_type)? v),
                _ => impl_as_getter!(@ref_default $($ref_type)?),
            }
        }
    };
    ($enum:ident {
        $( $( #[ $attr:meta ] )* $vis:vis $label:ident : $variant:ident $( -> $ref_type:tt $ty:tt )? $( ? $tb:tt )? ),+ $(,)?
    }) => {
        #[allow(unused_attributes)]
        impl $enum {
            $(
                impl_as_getter!(@ref_fn $( #[ $attr ] )* ( $( $ref_type )? ) $vis $label : $variant $( -> $ref_type $ty )? $( ? $tb )? );
            )+
        }
    };
}
pub(crate) use impl_as_getter;