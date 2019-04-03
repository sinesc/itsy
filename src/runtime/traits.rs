
use std::fmt::Debug;
use std::collections::HashMap;

/// An internal trait used to make resolver, compiler and VM generic over a user-defined set of Rust functions.
/// Use the `extern_rust!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMFunc<T>: Clone + Debug + 'static where T: VMFunc<T> {
    #[doc(hidden)]
    fn from_u16(index: u16) -> Self;
    #[doc(hidden)]
    fn into_u16(self: Self) -> u16;
    #[doc(hidden)]
    fn call_info() -> HashMap<&'static str, (u16, &'static str, Vec<&'static str>)>;
}

/// An internal trait used to make VM generic over a user-defined data context.
/// Use the `extern_rust!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMData<T, U> where T: VMFunc<T> {
    #[doc(hidden)]
    fn exec(self: Self, vm: &mut crate::runtime::VM<T, U>, context: &mut U);
}