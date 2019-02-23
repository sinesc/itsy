/// Macro to implement typesafe ids.
macro_rules! impl_typed_id {
    ($name:ident) => {
        #[derive(Copy, Clone, Default, PartialEq, Eq, PartialOrd, Hash)]
        pub struct $name(usize);
        impl From<$name> for usize {
            fn from(input: $name) -> usize {
                input.0
            }
        }
        impl From<usize> for $name {
            fn from(input: usize) -> $name {
                $name(input)
            }
        }
        impl ::std::fmt::Debug for $name {
            fn fmt(self: &Self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, "{}({})", stringify!($name), self.0)
            }
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
