//! Macro to implement typesafe ids.

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