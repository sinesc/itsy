use core::num::NonZeroUsize;
use crate::prelude::*;

/// Macro to implement typesafe ids.
macro_rules! impl_typed_id {
    ($name:ident, $string:expr) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[doc = $string]
        pub struct $name(NonZeroUsize);
        impl $name {
            /// Creates a new typed id from given usize.
            pub const fn new(input: usize) -> Self {
                Self(match NonZeroUsize::new(input + 1) {
                    Some(v) => v,
                    None => panic!("Input usize overflowed."),
                })
            }
            /// Converts the typed id into a usize. Useful to avoid `Into::<usize>::into(self)` when inference fails.
            pub const fn into_usize(self: Self) -> usize {
                self.0.get() - 1
            }
            /// Returns the next id following this one.
            pub fn next(self: Self) -> Self {
                Self::from(self.into_usize() + 1)
            }
        }
        impl From<$name> for usize {
            fn from(input: $name) -> usize {
                input.into_usize()
            }
        }
        impl From<usize> for $name {
            fn from(input: usize) -> $name {
                Self::new(input)
            }
        }
        impl Debug for $name {
            fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}({})", stringify!($name), self.into_usize())
            }
        }
        impl Default for $name {
            fn default() -> Self { Self::new(0) }
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
