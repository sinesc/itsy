use std::num::NonZeroUsize;
use std::fmt::Debug;
use std::default::Default;
use std::convert::Into;

/// Macro to implement typesafe ids.
macro_rules! impl_typed_id {
    ($name:ident) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash)]
        pub struct $name(NonZeroUsize);
        impl $name {
            /// Converts the typed id into a usize. Useful to avoid Into::<usize>::into(self) when inference fails.
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
            fn fmt(self: &Self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, "{}({})", stringify!($name), Into::<usize>::into(self.0) - 1)
            }
        }
        impl Default for $name {
            fn default() -> Self { Self(NonZeroUsize::new(1).unwrap()) }
        }
    };
}

/// Unique numeric id of a type.
impl_typed_id!(TypeId);

impl TypeId {
    pub fn void() -> TypeId {
        0.into()
    }
}

/// Unique numeric id of a scope.
impl_typed_id!(ScopeId);

/// Unique numeric id of a variable binding.
impl_typed_id!(BindingId);

/// Unique numeric id of a function.
impl_typed_id!(FunctionId);
