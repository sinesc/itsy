
//! Strongly typed scripting language with a rusty syntax and nice Rust integration.
//!
//! Look at the [`vm` Examples](fn.vm.html#examples) to get started.

pub mod frontend;
#[macro_use]
pub mod bytecode;
pub(crate)mod util;

use std::fmt::Debug;
use std::collections::HashMap;

/// Generates a type implementing [`VMFunc`](trait.VMFunc.html) and [`VMData`](trait.VMData.html).
///
/// # Examples
///
/// The following code makes the two Rust functions `print(i32)` and `hello_world()` available to Itsy code compiled with `vm::<MyFns>(...)`.
///
/// ```
/// # #[macro_use] extern crate itsy; fn main() {
/// extern_rust!(MyFns, (), {
///     /// prints given i32 value
///     fn print(&mut context, value: i32) {
///         println!("print:{}", value);
///     }
///     /// prints hello world!
///     fn hello_world(&mut context) {
///         println!("hello world!");
///     }
/// });
/// # }
/// ```
#[macro_export]
macro_rules! extern_rust {
    (@enum $enum_name:ident $(, $name:tt [ $( $attr:meta ),* ] )* ) => {
        #[allow(non_camel_case_types)]
        #[repr(u16)]
        #[derive(Copy, Clone, Debug)]
        pub enum $enum_name {
            $(
                $( #[ $attr ] )*
                $name,
            )*
            #[doc(hidden)]
            _dummy
        }
    };
    // handle return values
    (@handle-ret $vm:ident, u8, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, u16, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, u32, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, u64, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, i8, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, i16, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, i32, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, i64, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, f32, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, f64, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, bool, $value:ident) => { $vm.stack.push($value); };
    (@handle-ret $vm:ident, $_:tt, $value:ident) => { // object by value
        let heap_index: u32 = $vm.heap.store($value);
        $vm.stack.push(heap_index);
    };
    // handle parameters
    (@handle-param $vm:ident, u8) => { $vm.stack.pop() };
    (@handle-param $vm:ident, u16) => { $vm.stack.pop() };
    (@handle-param $vm:ident, u32) => { $vm.stack.pop() };
    (@handle-param $vm:ident, u64) => { $vm.stack.pop() };
    (@handle-param $vm:ident, i8) => { $vm.stack.pop() };
    (@handle-param $vm:ident, i16) => { $vm.stack.pop() };
    (@handle-param $vm:ident, i32) => { $vm.stack.pop() };
    (@handle-param $vm:ident, i64) => { $vm.stack.pop() };
    (@handle-param $vm:ident, f32) => { $vm.stack.pop() };
    (@handle-param $vm:ident, f64) => { $vm.stack.pop() };
    (@handle-param $vm:ident, bool) => { $vm.stack.pop() };
    (@handle-param $vm:ident, & str) => { { // rust &str specialcase
        let heap_index: u32 = $vm.stack.pop();
        let string_ref: &String = $vm.heap.load(heap_index);
        &string_ref[..]
    } };
    (@handle-param $vm:ident, & $_:tt) => { { // object by reference
        let heap_index: u32 = $vm.stack.pop();
        $vm.heap.load(heap_index)
    } };
    (@handle-param $vm:ident, $_:tt) => { { // object by value
        let heap_index: u32 = $vm.stack.pop();
        $vm.heap.clone(heap_index)
    } };
    (@trait $enum_name:ident, $context_type:ty $(, $name:tt, $context:ident [ $( $arg_name:ident : $($arg_type:tt)+ ),* ] [ $($ret_type:ident)? ] $code:block )* ) => {
        impl $crate::VMFunc<$enum_name> for $enum_name {
            fn to_u16(self: Self) -> u16 {
                unsafe { ::std::mem::transmute(self) }
            }
            fn from_u16(index: u16) -> Self {
                unsafe { ::std::mem::transmute(index) }
            }
            #[allow(unused_mut)]
            fn call_info() -> ::std::collections::HashMap<&'static str, (u16, &'static str, Vec<&'static str>)> {
                let mut map = ::std::collections::HashMap::new();
                $(
                    map.insert(stringify!($name), ($enum_name::$name.to_u16(), stringify!($($ret_type)?), vec![ $(stringify!( $($arg_type)+ )),* ]));
                )*
                map
            }
        }
        impl $crate::VMData<$enum_name, $context_type> for $enum_name {
            #[inline(always)]
            #[allow(unused_variables, unused_assignments, unused_imports)]
            fn exec(self: Self, vm: &mut $crate::bytecode::VM<$enum_name, $context_type>, context: &mut $context_type) {
                use $crate::bytecode::{StackOp, HeapOp};
                match self {
                    $(
                        $enum_name::$name => {
                            let $context = context;
                            // set rust function arguments, insert function body
                            let ret = {
                                $(
                                    let $arg_name: $($arg_type)+ = extern_rust!(@handle-param vm, $($arg_type)*);
                                )*
                                $code
                            };
                            // set return value, if any
                            $(
                                let ret_typed: $ret_type = ret;
                                extern_rust!(@handle-ret vm, $ret_type, ret_typed);
                            )?
                        },
                    )*
                    $enum_name::_dummy => panic!("Attempted to execute dummy function")
                }
            }
        }
    };
    (
        $enum_name:ident, $context_type:ty, { $(
            $( #[ $attr:meta ] )*
            fn $name:tt ( & mut $context:ident $(, $arg_name:ident : $($arg_type:tt)+ )* ) $( -> $ret_type:ident )? $code:block // ret_type cannot be ty as that can't be matched by handle-ret-val (macro shortcoming), or tt as that is ambiguous with $code. We'll just accept simple return types for now.
        )* }
    ) => {
        /// Rust function mapping. Generated from function signatures defined via the `extern_rust!` macro.
        extern_rust!(@enum $enum_name $(, $name [ $( $attr ),* ] )* );
        extern_rust!(@trait $enum_name, $context_type $(, $name, $context [ $( $arg_name : $($arg_type)+ ),* ] [ $( $ret_type )? ] $code )* );
    };
}

/// A default implementation of VMFunc that maps no functions.
#[allow(non_camel_case_types)]
#[repr(u16)]
#[derive(Copy, Clone, Debug)]
pub enum Standalone {
    #[doc(hidden)]
    _dummy
}
extern_rust!(@trait Standalone, ());

/// An internal trait used to make resolver, compiler and VM generic over a user-defined set of Rust functions.
/// Use the `extern_rust!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMFunc<T>: Clone + Debug + 'static where T: VMFunc<T> {
    #[doc(hidden)]
    fn from_u16(index: u16) -> Self;
    #[doc(hidden)]
    fn to_u16(self: Self) -> u16;
    #[doc(hidden)]
    fn call_info() -> HashMap<&'static str, (u16, &'static str, Vec<&'static str>)>;
}

/// An internal trait used to make VM generic over a user-defined data context.
/// Use the `extern_rust!` macro to generate a type implementing `VMData` and `VMFunc`.
pub trait VMData<T, U> where T: VMFunc<T> {
    #[doc(hidden)]
    fn exec(self: Self, vm: &mut bytecode::VM<T, U>, context: &mut U);
}

/// One stop shop to `parse`, `resolve` and `compile` given Itsy source code and create a VM for it.
/// Program execution starts from the "main" function.
///
/// Call `run` on the returned `VM` struct to execute the program.
///
/// # Examples
///
/// The following example defines a VM with a custom context and some Rust function bindings and runs it a few times.
///
/// ```
/// use itsy::*;
///
/// #[derive(Debug)]
/// struct MyGameState {
///     lives: i32,
///     // ...
/// }
///
/// extern_rust!(MyFns, MyGameState, {
///     // Retrieve some game state.
///     fn get_lives(&mut context) -> i32 {
///         context.lives
///     }
///     // Set some game state.
///     fn set_lives(&mut context, value: i32) {
///         context.lives = value;
///     }
/// });
///
/// fn main() {
///     // Create a VM for the given context and set of rust functions
///     // and compile the given source code.
///     let mut vm = vm::<MyFns, MyGameState>("
///         fn main() {
///             let lives = get_lives();
///             set_lives(lives - 1);
///         }
///     ");
///
///     // Create some application state and run the VM a few times on it.
///     let mut state = MyGameState { lives: 3 };
///     println!("game state before: {:?}", state);
///     vm.run(&mut state);
///     println!("game state first run: {:?}", state);
///     vm.run(&mut state);
///     println!("game state second run: {:?}", state);
/// }
/// ```
///
/// Output:
/// ```text
/// game state before: MyGameState { lives: 3 }
/// game state first run: MyGameState { lives: 2 }
/// game state second run: MyGameState { lives: 1 }
/// ```
pub fn vm<T, U>(program: &str) -> bytecode::VM<T, U> where T: VMFunc<T>+VMData<T, U> {
    use crate::{frontend::{parse, resolve}, bytecode::{compile, VM}};
    let parsed = parse(program).unwrap();
    let resolved = resolve::<T>(parsed, "main");
    let program = compile(resolved);
    VM::new(program)
}
