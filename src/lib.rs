
//! Itsy, a tiny language for embedded use.

pub mod frontend;
#[macro_use]
pub mod bytecode;
pub(crate)mod util;

use std::fmt::Debug;
use std::collections::HashMap;

/// Generates a type implementing [`ExternRust`](trait.ExternRust.html).
///
/// ### Example:
///
/// The following code makes the two Rust functions `print(i32)` and `hello_world()` available to Itsy code compiled with `vm::<MyFns>(...)`.
///
/// ```
/// # #[macro_use] extern crate itsy; fn main() {
/// extern_rust!(MyFns, {
///     /// prints given i32 value
///     fn print(vm: &mut VM, value: i32) {
///         println!("print:{}", value);
///     }
///     /// prints hello world!
///     fn hello_world(vm: &mut VM) {
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
    (@trait $enum_name:ident $(, $name:tt, $vm:ident [ $( $arg_name:ident : $($arg_type:tt)+ ),* ] [ $($ret_type:ident)? ] $code:block )* ) => {
        impl $crate::ExternRust<$enum_name> for $enum_name {
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
            #[inline(always)]
            #[allow(unused_variables, unused_assignments, unused_imports)]
            fn exec(self: Self, vm: &mut $crate::bytecode::VM<$enum_name>) {
                use $crate::bytecode::{StackOp, HeapOp};
                match self {
                    $(
                        $enum_name::$name => {
                            let $vm = vm;
                            // set rust function arguments, insert function body
                            let ret = {
                                $(
                                    let $arg_name: $($arg_type)+ = extern_rust!(@handle-param $vm, $($arg_type)*);
                                )*
                                $code
                            };
                            // set return value, if any
                            $(
                                let ret_typed: $ret_type = ret;
                                extern_rust!(@handle-ret $vm, $ret_type, ret_typed);
                            )?
                        },
                    )*
                    $enum_name::_dummy => panic!("Attempted to execute dummy function")
                }
            }
        }
    };
    (
        $enum_name:ident, { $(
            $( #[ $attr:meta ] )*
            fn $name:tt ( $vm:ident : & mut VM $(, $arg_name:ident : $($arg_type:tt)+ )* ) $( -> $ret_type:ident )? $code:block // ret_type cannot be ty as that can't be matched by handle-ret-val (macro shortcoming), or tt as that is ambiguous with $code. We'll just accept simple return types for now.
        )* }
    ) => {
        /// Rust function mapping. Generated from function signatures defined via the `extern_rust!` macro.
        extern_rust!(@enum $enum_name $(, $name [ $( $attr ),* ] )* );
        extern_rust!(@trait $enum_name $(, $name, $vm [ $( $arg_name : $($arg_type)+ ),* ] [ $( $ret_type )? ] $code )* );
    };
}

/// A default implementation of ExternRust that maps no functions.
#[allow(non_camel_case_types)]
#[repr(u16)]
#[derive(Copy, Clone, Debug)]
pub enum Standalone {
    #[doc(hidden)]
    _dummy
}
extern_rust!(@trait Standalone);

/// A trait used to make resolver, compiler and VM generic over a set of Rust functions.
/// Use `extern_rust!` macro to generate a type implementing `ExternRust`.
pub trait ExternRust<T>: Clone + Debug + 'static where T: ExternRust<T> {
    #[doc(hidden)]
    fn from_u16(index: u16) -> Self;
    #[doc(hidden)]
    fn to_u16(self: Self) -> u16;
    #[doc(hidden)]
    fn call_info() -> HashMap<&'static str, (u16, &'static str, Vec<&'static str>)>;
    #[doc(hidden)]
    fn exec(self: Self, vm: &mut bytecode::VM<T>);
}

/// One stop shop to `parse`, `resolve` and `compile` given Itsy source code and create a VM for it.
/// Program execution starts from the "main" function.
///
/// Call `run` on the returned `VM` struct to execute the program.
pub fn vm<T>(program: &str) -> bytecode::VM<T> where T: ExternRust<T> {
    use crate::{frontend::{parse, resolve}, bytecode::{compile, VM}};
    let parsed = parse(program).unwrap();
    let resolved = resolve::<T>(parsed, "main");
    let program = compile(resolved);
    VM::new(program)
}
