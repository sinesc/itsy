
//! Strongly typed scripting language with a rusty syntax and nice Rust integration.
//!
//! Look at the [`vm()` Examples](fn.vm.html#examples) to get started.

#[path="frontend/frontend.rs"]
mod frontend;

#[macro_use]
#[path="bytecode/bytecode.rs"]
mod bytecode;

#[path="shared/shared.rs"]
mod shared;
mod interface;

pub use interface::*;

use bytecode::{compiler::compile, runtime::vm::VM, VMFunc, VMData};
use frontend::{parser::parse, resolver::resolve};

/// Used to make Rust functions and data available to Itsy code by generating a type for compilation and runtime to be generic over.
///
/// Generates a type implementing [`VMFunc`](trait.VMFunc.html) and [`VMData`](trait.VMData.html).
/// The VM is generic over `VMFunc` and `VMData`. Parser and Resolver are generic over `VMFunc`.
///
/// `vm_func!( [ <Visibility> ] <TypeName>, <ContextType>, { <Implementations> });`
///
/// # Examples
///
/// The following example defines a VM with a custom context and some Rust function bindings and runs it a few times.
///
/// ```
/// use itsy::{vm_func, vm};
///
/// struct MyGameState {
///     lives: i32,
///     // ...
/// }
///
/// vm_func!(MyFns, MyGameState, {
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
///     ").unwrap();
///
///     // Create some application state and run the VM a few times on it.
///     let mut state = MyGameState { lives: 3 };
///     println!("lives started with: {:?}", state.lives);
///     vm.run(&mut state);
///     vm.reset();
///     println!("lives after first run: {:?}", state.lives);
///     vm.run(&mut state);
///     vm.reset();
///     println!("lives after second run: {:?}", state.lives);
/// }
/// ```
///
/// Output:
/// ```text
/// lives started with: 3
/// lives after first run: 2
/// lives after second run: 1
/// ```
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
    (@handle_param $vm:ident, & str) => { {
        let item: $crate::runtime::heap::HeapRef = $vm.stack.pop();
        $vm.heap.string(item)
    } };
    (@handle_param $vm:ident, & $_:tt) => { {
        compile_error!("Unsupported parameter type");
    } };
    (@handle_param $vm:ident, $_:tt) => { {
        compile_error!("Unsupported parameter type");
    } };
    (@trait $type_name:ident, $context_type:ty $(, $name:tt, $context:ident [ $( $arg_name:ident : $($arg_type:tt)+ ),* ] [ $($ret_type:ident)? ] $code:block )* ) => {
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
                    map.insert(stringify!($name), ($type_name::$name.into_u16(), stringify!($($ret_type)?), vec![ $(stringify!( $($arg_type)+ )),* ]));
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
                                    let $arg_name: $($arg_type)+ = vm_func!(@handle_param vm, $($arg_type)+);
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
            fn $name:tt ( & mut $context:ident $(, $arg_name:ident : $($arg_type:tt)+ )* ) $( -> $ret_type:ident )? $code:block // ret_type cannot be ty as that can't be matched by handle_ret-val (macro shortcoming), or tt as that is ambiguous with $code. We'll just accept simple return types for now.
        )* }
    ) => {
        /// Rust function mapping. Generated from function signatures defined via the `vm_func!` macro.
        vm_func!(@enum $vis, $type_name $(, $name [ $( $attr ),* ] )* );
        vm_func!(@trait $type_name, $context_type $(, $name, $context [ $( $arg_name : $($arg_type)+ ),* ] [ $( $ret_type )? ] $code )* );
    };
}

/// One stop shop to `parse`, `resolve` and `compile` given Itsy source code and create a `VM` for it.
/// Program execution starts from the `main` function.
///
/// Call `run` on the returned `VM` struct to execute the program.
///
/// # Examples
///
/// The following code calls the Rust function `print` from Itsy code. It uses an empty tuple as context.
/// For an example on how to share more useful data with Itsy code, have a look at the [`vm_func!`](macro.vm_func.html) documentation.
///
/// ```
/// use itsy::{vm_func, vm};
///
/// vm_func!(MyFns, (), {
///     /// prints given string
///     fn print(&mut context, value: &str) {
///         println!("print:{}", value);
///     }
/// });
///
/// fn main() {
///     let mut vm = vm::<MyFns, ()>("
///         fn main() {
///             print(\"Hello from Itsy!\");
///         }
///     ").unwrap();
///
///     vm.run(&mut ());
/// }
/// ```
pub fn vm<T, U>(program: &str) -> Result<VM<T, U>, Error> where T: VMFunc<T> + VMData<T, U> {
    let parsed = parse(program)?;
    let resolved = resolve::<T>(parsed, "main")?;
    let program = compile(resolved)?;
    Ok(VM::new(program))
}
