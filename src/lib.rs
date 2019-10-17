
//! Strongly typed scripting language with a rusty syntax and nice Rust integration.
//!
//! Look at the [`vm()` Examples](fn.vm.html#examples) to get started.

use std::fmt::{self, Display};
pub mod frontend;
pub mod runtime;
#[macro_use]
pub mod bytecode;
mod util;

pub use {frontend::{parse, resolve}, bytecode::compile};

/// Used to make Rust functions and data available to Itsy code by generating a type for compilation and runtime to be generic over.
///
/// Generates a type implementing [`VMFunc`](trait.VMFunc.html) and [`VMData`](trait.VMData.html).
/// The VM is generic over `VMFunc` and `VMData`. Parser and Resolver are generic over `VMFunc`.
///
/// `extern_rust!( [ <Visibility> ] <TypeName>, <ContextType>, { <Implementations> });`
///
/// # Examples
///
/// The following example defines a VM with a custom context and some Rust function bindings and runs it a few times.
///
/// ```
/// use itsy::*;
///
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
///     ").unwrap();
///
///     // Create some application state and run the VM a few times on it.
///     let mut state = MyGameState { lives: 3 };
///     println!("lives started with: {:?}", state.lives);
///     vm.run(&mut state);
///     println!("lives after first run: {:?}", state.lives);
///     vm.run(&mut state);
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
macro_rules! extern_rust {
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
    (@handle-param $vm:ident, String) => { { // rust String specialcase
        let item: $crate::runtime::HeapRef = $vm.stack.pop();
        $vm.heap.str(item).to_string()
    } };
    (@handle-param $vm:ident, & str) => { { // rust &str specialcase
        let item: $crate::runtime::HeapRef = $vm.stack.pop();
        $vm.heap.str(item)
    } };
    (@handle-param $vm:ident, & $_:tt) => { { // object by reference // fixme: this won't work, structs aren't aligned
        let heap_offset: u32 = $vm.stack.pop();
        let heap_index: u32 = $vm.stack.pop();
        $vm.heap.load(heap_index)
    } };
    (@handle-param $vm:ident, $_:tt) => { { // object by value // fixme: this won't work, structs aren't aligned
        let heap_offset: u32 = $vm.stack.pop();
        let heap_index: u32 = $vm.stack.pop();
        $vm.heap.clone(heap_index)
    } };
    (@trait $type_name:ident, $context_type:ty $(, $name:tt, $context:ident [ $( $arg_name:ident : $($arg_type:tt)+ ),* ] [ $($ret_type:ident)? ] $code:block )* ) => {
        impl $crate::runtime::VMFunc<$type_name> for $type_name {
            fn into_u16(self: Self) -> u16 {
                self as u16
            }
            fn from_u16(index: u16) -> Self {
                unsafe { ::std::mem::transmute(index) }
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
                use $crate::runtime::StackOp;
                match self {
                    $(
                        $type_name::$name => {
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
                    $type_name::_dummy => panic!("Attempted to execute dummy function")
                }
            }
        }
    };
    (
        $vis:vis $type_name:ident, $context_type:ty, { $(
            $( #[ $attr:meta ] )*
            fn $name:tt ( & mut $context:ident $(, $arg_name:ident : $($arg_type:tt)+ )* ) $( -> $ret_type:ident )? $code:block // ret_type cannot be ty as that can't be matched by handle-ret-val (macro shortcoming), or tt as that is ambiguous with $code. We'll just accept simple return types for now.
        )* }
    ) => {
        /// Rust function mapping. Generated from function signatures defined via the `extern_rust!` macro.
        extern_rust!(@enum $vis, $type_name $(, $name [ $( $attr ),* ] )* );
        extern_rust!(@trait $type_name, $context_type $(, $name, $context [ $( $arg_name : $($arg_type)+ ),* ] [ $( $ret_type )? ] $code )* );
    };
}

/// An error generated during program compilation or execution.
#[derive(Clone, Debug)]
pub enum ItsyError {
    ParseError(frontend::ParseError),
    ResolveError(frontend::ResolveError),
    CompileError(bytecode::CompileError),
}

impl ItsyError {
    pub fn loc(self: &Self, input: &str) -> (u32, u32) {
        match self {
            Self::ParseError(e) => e.loc(input),
            Self::ResolveError(e) => e.loc(input),
            Self::CompileError(e) => e.loc(input),
        }
    }
}

impl Display for ItsyError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParseError(e) => write!(f, "{}", e),
            Self::ResolveError(e) => write!(f, "{}", e),
            Self::CompileError(e) => write!(f, "{}", e),
        }
    }
}

impl From<frontend::ParseError> for ItsyError {
    fn from(error: frontend::ParseError) -> ItsyError {
        ItsyError::ParseError(error)
    }
}

impl From<frontend::ResolveError> for ItsyError {
    fn from(error: frontend::ResolveError) -> ItsyError {
        ItsyError::ResolveError(error)
    }
}

impl From<bytecode::CompileError> for ItsyError {
    fn from(error: bytecode::CompileError) -> ItsyError {
        ItsyError::CompileError(error)
    }
}

/// One stop shop to `parse`, `resolve` and `compile` given Itsy source code and create a `VM` for it.
/// Program execution starts from the `main` function.
///
/// Call `run` on the returned `VM` struct to execute the program.
///
/// # Examples
///
/// The following code calls the Rust function `print` from Itsy code. It uses an empty tuple as context.
/// For an example on how to share more useful data with Itsy code, have a look at the [`extern_rust!`](macro.extern_rust.html) documentation.
///
/// ```
/// use itsy::*;
///
/// extern_rust!(MyFns, (), {
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
pub fn vm<T, U>(program: &str) -> Result<runtime::VM<T, U>, ItsyError> where T: crate::runtime::VMFunc<T> + crate::runtime::VMData<T, U> {
    use crate::runtime::VM;
    let parsed = parse(program)?;
    let resolved = resolve::<T>(parsed, "main")?;
    let program = compile(resolved)?; // todo: compiler needs error handling. then forward error
    Ok(VM::new(program))
}
