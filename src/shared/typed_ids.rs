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
            /// Creates a new typed id from given usize.
            pub const fn new(input: usize) -> Self {
                Self(match NonZeroUsize::new(input + 1) {
                    Some(v) => v,
                    None => panic!("Input usize overflowed"),
                })
            }
        }
        impl From<$name> for usize {
            fn from(input: $name) -> usize {
                Into::<usize>::into(input.0) - 1
            }
        }
        impl From<usize> for $name {
            fn from(input: usize) -> $name {
                Self(NonZeroUsize::new(input + 1).expect("Input usize overflowed"))
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
    pub const VOID: TypeId = TypeId::new(0);
}

impl_typed_id!(ScopeId, "Unique numeric id of a scope.");

impl ScopeId {
    pub const ROOT: ScopeId = ScopeId::new(0);
}

impl_typed_id!(BindingId, "Unique numeric id of a variable binding.");
impl_typed_id!(FunctionId, "Unique numeric id of a function.");
impl_typed_id!(ConstantId, "Unique numeric id of a constant value.");
