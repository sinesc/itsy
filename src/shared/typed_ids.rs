use core::num::NonZeroUsize;
use crate::prelude::*;

/// Macro to implement typesafe ids.
macro_rules! impl_typed_id {
    ($name:ident, $string:expr) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[doc = $string]
        pub struct $name(NonZeroUsize);
        impl $name {
            /// Converts the typed id into a usize. Useful to avoid `Into::<usize>::into(self)` when inference fails.
            pub fn into_usize(self: Self) -> usize {
                self.into()
            }
        }
        impl From<$name> for usize {
            fn from(input: $name) -> usize {
                Into::<usize>::into(input.0) - 1
            }
        }
        impl From<usize> for $name {
            fn from(input: usize) -> $name {
                $name(NonZeroUsize::new(input + 1).expect("Expected non-zero input id"))
            }
        }
        impl Debug for $name {
            fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}({})", stringify!($name), Into::<usize>::into(self.0))
            }
        }
        impl Default for $name {
            fn default() -> Self { Self(NonZeroUsize::new(1).unwrap()) }
        }
    };
}

impl_typed_id!(TypeId, "Unique numeric id of a type");

impl TypeId {
    pub fn void() -> TypeId {
        0.into()
    }
}

impl_typed_id!(ScopeId, "Unique numeric id of a scope.");

impl_typed_id!(BindingId, "Unique numeric id of a variable binding.");

impl_typed_id!(FunctionId, "Unique numeric id of a function.");
