#![doc(html_favicon_url = "https://sinesc.github.io/images/itsy/favicon.png")]
#![doc(html_logo_url = "https://sinesc.github.io/images/itsy/logo.png")]

//! Strongly typed scripting language with a rusty syntax and nice Rust integration.
//!
//! Look at the [build] or [run] examples to get started.

#[path="frontend/frontend.rs"]
mod frontend;

#[macro_use]
#[path="bytecode/bytecode.rs"]
mod bytecode;

#[path="shared/shared.rs"]
mod shared;
mod interface;

pub use interface::*;

use bytecode::{compiler::compile, runtime::vm::VM, runtime::vm::VMState, VMFunc, VMData, Program};
use frontend::{parser::parse, resolver::resolve};

/// Used to make Rust functions and data available to Itsy code by generating a type for compilation and runtime to be generic over.
///
/// Generates a type implementing [`VMFunc`](trait.VMFunc.html) and [`VMData`](trait.VMData.html).
/// The VM is generic over `VMFunc` and `VMData`. Parser and Resolver are generic over `VMFunc`.
///
/// `vm_func!( [ <Visibility> ] <TypeName>, <ContextType>, { <Implementations> });`
///
/// Currently supported parameter types are rust primitives as well as String and &str.
///
/// # Examples
///
/// The following example defines a few Rust function and calls them from itsy code.
///
/// ```
/// use itsy::{vm_func, build, runtime::VM};
///
/// struct MyContext {
///     // ...
/// }
///
/// vm_func!(MyFns, MyContext, {
///     // Rust function that does something
///     fn intense_computation(&mut context, a: i16, b: i16) -> i16 {
///         a * b
///     }
///     fn print(&mut context, val: i16) {
///         println!("{}", val);
///     }
///     //... more functions ...
/// });
///
/// fn main() {
///     let mut context = MyContext { /* ... */ };
///
///     let program = build::<MyFns>("
///         fn main() {
///             print(intense_computation(4, 5));
///         }
///     ").unwrap();
///
///     let mut vm = VM::new(&program);
///     vm.run(&mut context);
/// }
/// ```
///
/// Output:
/// ```text
/// 20
/// ```
#[cfg(doc)]
#[macro_export]
macro_rules! vm_func {
    ($vis:vis $type_name:ident, $context_type:ty, {
        $(
            fn $name:tt(&mut $context:ident $(, $arg_name:ident: $($arg_type:tt)+)*) $(-> $ret_type:ident)? $code:block
        )*
    } ) => { }
}

#[cfg(not(doc))]
#[macro_export]
macro_rules! vm_func {
    (@enum $vis:vis, $type_name:ident $(, $name:tt [ $( $attr:meta ),* ] )* ) => {
        #[allow(non_camel_case_types)]
        #[repr(u16)]
        #[derive(Copy, Clone, Debug)]
        $vis enum $type_name {
            $(
                $( #[ $attr ] )*
                $name,
            )*
            #[doc(hidden)]
            _dummy
        }
    };
    // handle return values
    (@handle_ret $vm:ident, u8, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, u16, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, u32, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, u64, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, i8, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, i16, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, i32, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, i64, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, f32, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, f64, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, bool, $value:ident) => { $vm.stack.push($value as u8); };
    (@handle_ret $vm:ident, String, $value:ident) => { {
        let raw_bytes = &$value.as_bytes();
        let index = $vm.heap.alloc(raw_bytes.to_vec());
        $vm.stack.push($crate::runtime::HeapRef::new(index, 0));
    } };
    (@handle_ret $vm:ident, $_:tt, $value:ident) => {
        compile_error!("Unsupported return type");
    };
    // handle parameters
    (@handle_param $vm:ident, u8) => { $vm.stack.pop() };
    (@handle_param $vm:ident, u16) => { $vm.stack.pop() };
    (@handle_param $vm:ident, u32) => { $vm.stack.pop() };
    (@handle_param $vm:ident, u64) => { $vm.stack.pop() };
    (@handle_param $vm:ident, i8) => { $vm.stack.pop() };
    (@handle_param $vm:ident, i16) => { $vm.stack.pop() };
    (@handle_param $vm:ident, i32) => { $vm.stack.pop() };
    (@handle_param $vm:ident, i64) => { $vm.stack.pop() };
    (@handle_param $vm:ident, f32) => { $vm.stack.pop() };
    (@handle_param $vm:ident, f64) => { $vm.stack.pop() };
    (@handle_param $vm:ident, bool) => { { let tmp: u8 = $vm.stack.pop(); tmp != 0 } };
    (@handle_param $vm:ident, String) => { {
        let item: $crate::runtime::heap::HeapRef = $vm.stack.pop();
        $vm.heap.string(item).to_string()
    } };
    (@handle_param $vm:ident, str) => { {
        let item: $crate::runtime::heap::HeapRef = $vm.stack.pop();
        $vm.heap.string(item)
    } };
    (@handle_param $vm:ident, & $_:tt) => { {
        compile_error!("Unsupported parameter type");
    } };
    (@handle_param $vm:ident, $_:tt) => { {
        compile_error!("Unsupported parameter type");
    } };
    (@handle_str str) => { &str }; // FIXME: hack to support &str, see fixmes in main arm. remove these two once fixed
    (@handle_str $other:ident) => { $other };
    (@trait $type_name:ident, $context_type:ty $(, $name:tt, $context:ident [ $( $arg_name:ident : $arg_type:ident , )* ] [ $($ret_type:ident)? ] $code:block )* ) => {
        impl $crate::runtime::VMFunc<$type_name> for $type_name {
            fn into_u16(self: Self) -> u16 {
                self as u16
            }
            fn from_u16(index: u16) -> Self {
                //un safe { ::std::mem::trans mute(index) }
                match index {
                    $(
                        x if x == Self::$name as u16 => Self::$name,
                    )+
                    _ => panic!("Invalid VMFunc index {}", index),
                }
            }
            #[allow(unused_mut)]
            fn call_info() -> ::std::collections::HashMap<&'static str, (u16, &'static str, Vec<&'static str>)> {
                let mut map = ::std::collections::HashMap::new();
                $(
                    map.insert(stringify!($name), ($type_name::$name.into_u16(), stringify!($($ret_type)?), vec![ $(stringify!( $arg_type )),* ]));
                )*
                map
            }
        }
        impl $crate::runtime::VMData<$type_name, $context_type> for $type_name {
            #[inline(always)]
            #[allow(unused_variables, unused_assignments, unused_imports)]
            fn exec(self: Self, vm: &mut $crate::runtime::VM<$type_name, $context_type>, context: &mut $context_type) {
                use $crate::runtime::stack::StackOp;
                match self {
                    $(
                        $type_name::$name => {
                            let $context = context;
                            // set rust function arguments, insert function body
                            let ret = {
                                $(
                                    let $arg_name: vm_func!(@handle_str $arg_type) = vm_func!(@handle_param vm, $arg_type);
                                )*
                                $code
                            };
                            // set return value, if any
                            $(
                                let ret_typed: $ret_type = ret;
                                vm_func!(@handle_ret vm, $ret_type, ret_typed);
                            )?
                        },
                    )*
                    $type_name::_dummy => panic!("Attempted to execute dummy function")
                }
            }
        }
    };
    (
        $vis:vis $type_name:ident, $context_type:ty, { $(
            $( #[ $attr:meta ] )*
            // FIXME: ret_type is simple ident only (ambiguity issues)
            // FIXME: arg_type accepts & for everything and doesn't validate whether that type supports it. matching $($arg_type:tt)+ caused ambiguity when multiple args were present
            // this could probably be solved by capturing all args via ( $($args:tt)+ ) and then using an incremental tt muncher to capture the individual
            // components using push down accumulation to transform them into a non-ambiguous format
            fn $name:tt ( & mut $context:ident $( , $arg_name:ident : $( & )? $arg_type:ident )* ) $( -> $ret_type:ident )? $code:block
        )* }
    ) => {
        /// Rust function mapping. Generated from function signatures defined via the `vm_func!` macro.
        vm_func!(@enum $vis, $type_name $(, $name [ $( $attr ),* ] )* );
        vm_func!(@trait $type_name, $context_type $(, $name, $context [ $( $arg_name : $arg_type , )* ] [ $( $ret_type )? ] $code )* );
    };
}

/// Parses, resolves and compiles given Itsy source code. The name of the
/// entry function must be `main`. For more control, see [parser::parse],
/// [resolver::resolve] and [compiler::compile].
/// Use [run] or [VM] to execute the given program.
///
/// The following example builds and runs a program:
/// ```
/// use itsy::{vm_func, build, run};
///
/// vm_func!(MyFns, (), {
///     /// a rust function that prints given string
///     fn print(&mut context, value: &str) {
///         println!("print: {}", value);
///     }
/// });
///
/// fn main() {
///     let program = build::<MyFns>("
///         /// an itsy function that calls a rust function
///         fn main() {
///             print(\"Hello from Itsy!\");
///         }
///     ").unwrap();
///
///     run(&program, &mut ()).unwrap();
/// }
/// ```
pub fn build<F>(source: &str) -> Result<Program<F>, Error> where F: VMFunc<F> {
    let parsed = parse(source)?;
    let resolved = resolve::<F>(parsed, "main")?;
    Ok(compile(resolved)?)
}

/// Runs the given compiled program. The name of the entry function must be `main`.
/// See [VM] for more control about running a program.
///
/// The following example builds and runs a program:
/// ```
/// use itsy::{vm_func, build, run};
///
/// vm_func!(MyFns, (), {
///     /// a rust function that prints given string
///     fn print(&mut context, value: &str) {
///         println!("print: {}", value);
///     }
/// });
///
/// fn main() {
///     let program = build::<MyFns>("
///         /// an itsy function that calls a rust function
///         fn main() {
///             print(\"Hello from Itsy!\");
///         }
///     ").unwrap();
///
///     run(&program, &mut ()).unwrap();
/// }
/// ```
pub fn run<F, D>(program: &Program<F>, context: &mut D) -> Result<VM<F, D>, Error> where F: VMFunc<F> + VMData<F, D> {
    let mut vm = VM::new(program);
    match vm.run(context) {
        VMState::Ready => Err(Error::RuntimeError), // TODO: or maybe panic? if this happens its an itsy bug
        VMState::Terminated => Ok(vm),
        VMState::Yielded => Ok(vm),
        VMState::RuntimeError => Err(Error::RuntimeError),
    }
}