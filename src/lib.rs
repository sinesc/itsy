#![doc(html_favicon_url = "https://sinesc.github.io/images/itsy/favicon.png")]
#![doc(html_logo_url = "https://sinesc.github.io/images/itsy/logo.png")]

//! Strongly typed scripting language with a rusty syntax and nice Rust integration.
//!
//! Look at the [build_str] or [itsy_api!] example to get started.
//!
//! Documentation of the types available to Itsy code is available [here](crate::internals::documentation).

#[path="frontend/frontend.rs"]
#[cfg(feature="compiler")]
mod frontend;
#[path="shared/shared.rs"]
#[cfg(feature="compiler")]
mod shared;
#[path="bytecode/bytecode.rs"]
mod bytecode;
mod interface;
mod prelude;
mod config;
mod marshal;

pub use interface::*;

/// Derives the marshalling glue required to use a struct as an Itsy API argument or return type.
///
/// See [itsy_api!] for how API functions are defined. Requires the `derive` feature.
#[cfg(feature="derive")]
pub use itsy_derive::VMValue;

use config::*;
mod config_derived {
    /// Type used to index RustFns.
    pub type RustFnIndex = crate::ItemIndex;
    /// Type used to index builtins.
    pub type BuiltinIndex = crate::ItemIndex;
    /// Type used to index enum variants.
    pub type VariantIndex = crate::ItemIndex;
    /// Itsy type used to store stack addresses and vector indices.
    #[cfg(feature="compiler")]
    use crate::{shared::meta::Type, prelude::size_of, StackAddress};
    #[cfg(feature="compiler")]
    pub const STACK_ADDRESS_TYPE: Type = Type::unsigned(size_of::<StackAddress>());
    #[cfg(feature="compiler")]
    pub const STACK_OFFSET_TYPE: Type = Type::signed(size_of::<StackAddress>());
}
use config_derived::*;

/// Creates an opaque type defining an API of Itsy-callable Rust functions.
///
/// The resolver, compiler and VM are generic over this type to ensure that the generated
/// Itsy API-bindings match the given Rust API.
///
/// `itsy_api!( [ <Visibility> ] <TypeName>, <ContextType>, { <Implementations> });`
///
/// Out of the box supported parameter types are Rust primitives as well as `String` and `str` (a `str`
/// parameter is received by the function body as a `&str`). The itsy-derive crate provides
/// `#[derive(VMValue)]` to enable use of custom structs and enums.
///
/// Arrays of any supported type are written using Itsy's `[ T ]` syntax and map to a Rust `Vec<T>`;
/// nesting is supported, so e.g. `[ [ String ] ]` maps to `Vec<Vec<String>>`.
///
/// # Examples
///
/// The following example defines a few Rust functions and calls them from itsy code.
///
/// ```
/// use itsy::{itsy_api, build_str, runtime::VM};
///
/// // A custom context accessible to the functions defined below.
/// struct MyContext {
///     // ...
/// }
///
/// // Define an API of Rust functions that are callable from the Itsy script.
/// itsy_api!(MyAPI<MyContext> {
///     fn intense_computation(&mut context, a: i16, b: i16) -> i16 {
///         a * b
///     }
///     fn print(&mut context, val: i16) {
///         println!("{}", val);
///     }
/// });
///
/// fn main() {
///     let mut context = MyContext { /* ... */ };
///
///     // Compile a short Itsy program and bind it to our MyAPI.
///     let program = build_str::<MyAPI>(stringify!(
///         fn main() {
///             MyAPI::print(MyAPI::intense_computation(4, 5));
///         }
///     )).unwrap();
///
///     // Create a new VM and run the program.
///     let mut vm = VM::new(program);
///     vm.run(&mut context).unwrap();
/// }
/// ```
///
/// Output:
/// ```text
/// 20
/// ```
#[cfg(doc)]
#[macro_export]
macro_rules! itsy_api {
    ($vis:vis $type_name:ident, $context_type:ty, {
        $(
            fn $name:tt(&mut $context_name:ident $(, $arg_name:ident: $($arg_type:tt)+)*) $(-> $ret_type:ident)? $code:block
        )*
    } ) => { }
}

#[cfg(not(doc))]
#[macro_export]
macro_rules! itsy_api {
    // VM: Pushes the return value and converts some special cases.
    (@handle_ret_value $vm:ident, u8, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, u16, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, u32, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, u64, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, i8, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, i16, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, i32, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, i64, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, f32, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, f64, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, bool, $value:ident) => { $vm.stack.push($value as u8); };
    (@handle_ret_value $vm:ident, String, $value:ident) => { {
        let index = $vm.heap.alloc_copy($value.as_bytes(), $crate::internals::binary::sizes::ItemIndex::MAX);
        $vm.stack.push($crate::internals::binary::heap::HeapRef::new(index, 0));
    } };
    (@handle_ret_value $vm:ident, [ $($inner:tt)+ ], $value:ident) => { {
        let stack_value = <itsy_api!(@vec_type [ $($inner)+ ]) as $crate::internals::binary::VMValue>::to_stack($value, &mut $vm.heap);
        $vm.stack.push(stack_value);
    } };
    (@handle_ret_value $vm:ident, $other:ident, $value:ident) => { {
        let stack_value = <$other as $crate::internals::binary::VMValue>::to_stack($value, &mut $vm.heap);
        $vm.stack.push(stack_value);
    } };
    // Translate an `itsy_api!` pseudo-type into the concrete Rust type seen by API function bodies.
    // Array types `[ T ]` become `Vec<T>` (recursively); everything else maps to itself.
    (@vec_type [ $($inner:tt)+ ]) => { ::std::vec::Vec< itsy_api!(@vec_type $($inner)+) > };
    (@vec_type $other:ident) => { $other };
    // Compiler: build an ApiType descriptor for the resolver from an `itsy_api!` pseudo-type. Named types
    // resolve their Itsy name via VMType; array types `[ T ]` become ApiType::Array (recursively).
    (@type_descriptor [ $($inner:tt)+ ]) => { $crate::internals::binary::ApiType::Array(::std::boxed::Box::new(itsy_api!(@type_descriptor $($inner)+))) };
    (@type_descriptor $other:ident) => { $crate::internals::binary::ApiType::Name(<$other as $crate::internals::binary::VMType>::ITSY_NAME) };
    // Compiler: optional return-type descriptor; absent return type (void) yields None.
    (@ret_descriptor) => { ::std::option::Option::None };
    (@ret_descriptor $ret_type:tt) => { ::std::option::Option::Some(itsy_api!(@type_descriptor $ret_type)) };
    // VM: Pops an argument and converts some special cases.
    (@handle_param_value $vm:ident, u8) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, u16) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, u32) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, u64) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, i8) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, i16) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, i32) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, i64) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, f32) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, f64) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, bool) => { { let tmp: u8 = $vm.stack.pop(); tmp != 0 } };
    (@handle_param_value $vm:ident, String) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, str) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, [ $($inner:tt)+ ]) => { $vm.stack.pop() };
    (@handle_param_value $vm:ident, $other:ident) => { $vm.stack.pop() };
    // VM: Translate parameter list pseudo-types to internally used types. Primitives are loaded as
    // themselves; strings and reference types (structs/enums) are loaded as a HeapRef.
    (@handle_param_type u8) => { u8 };
    (@handle_param_type u16) => { u16 };
    (@handle_param_type u32) => { u32 };
    (@handle_param_type u64) => { u64 };
    (@handle_param_type i8) => { i8 };
    (@handle_param_type i16) => { i16 };
    (@handle_param_type i32) => { i32 };
    (@handle_param_type i64) => { i64 };
    (@handle_param_type f32) => { f32 };
    (@handle_param_type f64) => { f64 };
    (@handle_param_type bool) => { bool };
    (@handle_param_type String) => { $crate::internals::binary::heap::HeapRef };
    (@handle_param_type str) => { $crate::internals::binary::heap::HeapRef };
    (@handle_param_type [ $($inner:tt)+ ]) => { $crate::internals::binary::heap::HeapRef };
    (@handle_param_type $other:ident) => { <$other as $crate::internals::binary::VMValue>::Stack };
    // VM: Convert some pseudo-type arguments to values of concrete rust types. This happens after the value was initially loaded as the type prescribed by @handle_param_type.
    (@handle_ref_param_value $vm:ident, u8, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, u16, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, u32, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, u64, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, i8, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, i16, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, i32, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, i64, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, f32, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, f64, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, bool, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, String, $arg_name:ident) => { $vm.heap.string($arg_name).unwrap().to_string() };
    (@handle_ref_param_value $vm:ident, str, $arg_name:ident) => { $vm.heap.string($arg_name).unwrap() };
    (@handle_ref_param_value $vm:ident, [ $($inner:tt)+ ], $arg_name:ident) => { <itsy_api!(@vec_type [ $($inner)+ ]) as $crate::internals::binary::VMValue>::from_stack(&$vm.heap, $arg_name) };
    (@handle_ref_param_value $vm:ident, $other:ident, $arg_name:ident) => { <$other as $crate::internals::binary::VMValue>::from_stack(&$vm.heap, $arg_name) };
    // VM: Translate some pseudo-type names to concrete rust types.
    (@handle_ref_param_type str) => { &str }; // FIXME: hack to support &str, see fixmes in main arm. remove these two once fixed
    (@handle_ref_param_type [ $($inner:tt)+ ]) => { itsy_api!(@vec_type [ $($inner)+ ]) };
    (@handle_ref_param_type $other:ident) => { $other };
    // VM: Generate reference counting code for some pseudo type arguments. Primitives need no cleanup;
    // strings free their temporary heap object; reference types recursively free theirs.
    (@handle_ref_param_free $vm:ident, u8, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, u16, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, u32, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, u64, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, i8, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, i16, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, i32, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, i64, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, f32, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, f64, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, bool, $arg_name:ident) => { };
    (@handle_ref_param_free $vm:ident, String, $arg_name:ident) => { $vm.heap.ref_item($arg_name.index(), $crate::internals::binary::heap::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, str, $arg_name:ident) => { $vm.heap.ref_item($arg_name.index(), $crate::internals::binary::heap::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, [ $($inner:tt)+ ], $arg_name:ident) => { <itsy_api!(@vec_type [ $($inner)+ ]) as $crate::internals::binary::VMValue>::free_stack(&mut $vm.heap, $arg_name) };
    (@handle_ref_param_free $vm:ident, $other:ident, $arg_name:ident) => { <$other as $crate::internals::binary::VMValue>::free_stack(&mut $vm.heap, $arg_name) };
    // VM: Pops arguments in reverse order off the stack (so that it is the correct order for the function call).
    (@load_args_reverse $vm:ident [] $($arg_name:ident $arg_type:tt)*) => {
        $(
            let $arg_name: itsy_api!(@handle_param_type $arg_type) = itsy_api!(@handle_param_value $vm, $arg_type);
        )*
    };
    (@load_args_reverse $vm:ident [ $first_arg_name:ident $first_arg_type:tt $($rest:tt)* ] $($reversed:tt)*) => {
        itsy_api!(@load_args_reverse $vm [ $($rest)* ] $first_arg_name $first_arg_type $($reversed)*)
    };
    // Main definition block
    (
        $(#[$globalmeta:meta])*
        $vis:vis $type_name:ident < $context_type:ty > { $(
            $( #[ $attr:meta ] )*
            // FIXME: ret_type is simple ident only (ambiguity issues)
            // FIXME: arg_type accepts & for everything and doesn't validate whether that type supports it. matching $($arg_type:tt)+ caused ambiguity when multiple args were present
            // this could probably be solved by capturing all args via ( $($args:tt)+ ) and then using an incremental tt muncher to capture the individual
            // components using push down accumulation to transform them into a non-ambiguous format
            fn $name:tt ( & mut $context_name:ident $( , & mut $vm_name:ident )? $( , $arg_name:ident : $arg_type:tt )* ) $( -> $ret_type:tt )? $code:block
        )* }
    ) => {

        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug)]
        $(#[$globalmeta])*
        // Rust function mapping. Generated from function signatures defined via the `itsy_api!` macro.
        $vis enum $type_name {
            $(
                $( #[ $attr ] )*
                $name,
            )*
            #[doc(hidden)]
            _dummy
        }

        impl $crate::internals::binary::VMFunc<$type_name> for $type_name {
            #[cfg(feature="compiler")]
            fn to_index(self: Self) -> $crate::internals::binary::sizes::RustFnIndex {
                self as $crate::internals::binary::sizes::RustFnIndex
            }
            fn from_index(index: $crate::internals::binary::sizes::RustFnIndex) -> Self {
                //un safe { ::std::mem::trans mute(index) }
                match index {
                    $(
                        x if x == Self::$name as $crate::internals::binary::sizes::RustFnIndex => Self::$name,
                    )+
                    _ => panic!("Invalid RustFnIndex {}.", index),
                }
            }
            #[allow(unused_mut)]
            #[cfg(feature="compiler")]
            fn resolve_info() -> ::std::collections::HashMap<&'static str, ($crate::internals::binary::sizes::RustFnIndex, ::std::option::Option<$crate::internals::binary::ApiType>, Vec<$crate::internals::binary::ApiType>)> {
                let mut map = ::std::collections::HashMap::new();
                $(
                    map.insert(concat!(stringify!($type_name), "::", stringify!($name)), ($type_name::$name.to_index(), itsy_api!(@ret_descriptor $($ret_type)?), vec![ $( itsy_api!(@type_descriptor $arg_type) ),* ]));
                )*
                map
            }
            #[allow(unused_mut)]
            #[cfg(feature="compiler")]
            fn resolve_types() -> Vec<$crate::internals::binary::ApiTypeDef> {
                let mut defs = Vec::new();
                $(
                    $( <itsy_api!(@vec_type $arg_type) as $crate::internals::binary::VMType>::collect_type_defs(&mut defs); )*
                    $( <itsy_api!(@vec_type $ret_type) as $crate::internals::binary::VMType>::collect_type_defs(&mut defs); )?
                )*
                defs
            }
        }

        #[cfg(feature="runtime")]
        impl $crate::internals::binary::VMData<$type_name, $context_type> for $type_name {
            #[allow(unused_variables, unused_assignments, unused_imports)]
            fn exec(self: Self, vm: &mut $crate::runtime::VM<$type_name, $context_type>, context: &mut $context_type) {
                use $crate::internals::binary::stack::StackOp;
                match self {
                    $(
                        $type_name::$name => {
                            let $context_name = context;
                            // Load arguments. For references this loads the HeapRef. We'll need it later to handle refcounts.
                            itsy_api!(@load_args_reverse vm [ $( $arg_name $arg_type )* ]);
                            // Run code.
                            let ret = {
                                // Shadow HeapRef arguments with actual argument value.
                                $(
                                    let $arg_name: itsy_api!(@handle_ref_param_type $arg_type) = itsy_api!(@handle_ref_param_value vm, $arg_type, $arg_name);
                                )*
                                $( let $vm_name = &mut *vm; )?
                                $code
                            };
                            // Handle refcounting.
                            $(
                                itsy_api!(@handle_ref_param_free vm, $arg_type, $arg_name);
                            )*
                            // Set return value, if any.
                            $(
                                let ret_typed: itsy_api!(@vec_type $ret_type) = ret;
                                itsy_api!(@handle_ret_value vm, $ret_type, ret_typed);
                            )?
                        },
                    )*
                    func @ _ => panic!("Invalid RustFn {:?}.", func)
                }
            }
        }
    };
}

/// Parses, resolves and compiles given Itsy source code.
///
/// The name of the entry function must be `main`. This utility-function does not support
/// external Itsy modules. For more control, see either [build] or
/// [parser::parse], [resolver::resolve] and [compiler::compile].
/// Use [run] or [VM::run](crate::runtime::VM::run) to execute the given program.
///
/// # Examples
///
/// The following example builds and runs a program:
/// ```
/// use itsy::{itsy_api, build_str, run};
///
/// // Define an API of Rust functions that are callable from the Itsy script.
/// itsy_api!(MyAPI<()> {
///     fn print(&mut context, value: str) {
///         println!("print: {}", value);
///     }
///     // ... more functions ...
/// });
///
/// fn main() {
///     // Build the itsy program and link it to the MyAPI API we created above.
///     let program = build_str::<MyAPI>(stringify!(
///         // An Itsy program that calls the Rust 'print' function.
///         fn main() {
///             MyAPI::print("Hello from Itsy!");
///         }
///     )).unwrap();
///
///     run(program, &mut ()).unwrap();
/// }
/// ```
#[cfg(feature="compiler")]
pub fn build_str<F>(source: &str) -> Result<Program<F>, Error> where F: bytecode::VMFunc<F> {
    use crate::frontend::parser::{parse_module, types::{LinkState, ParsedProgram}, error::{ParseError, ParseErrorKind}};
    let mut state = LinkState::new();
    let parsed = parse_module(&mut state, source, "")?;
    if let Some(module) = parsed.modules().next() {
        return Err(Error::ParseError(ParseError::new(
            ParseErrorKind::DisabledFeature("build_str() does not support module loading. Please use either build() or parser::parse() and provide a module loader"),
            module.position,
            "",
        )));
    }
    let mut program = ParsedProgram::new();
    program.add_module(parsed);
    program.set_link_state(state);
    let resolved = resolver::resolve::<F>(program, "main")?;
    Ok(compiler::compile(resolved)?)
}

/// Parses, resolves and compiles given Itsy source file.
///
/// The name of the entry function must be `main`. Modules are loaded from disk relative to the
/// given source file. For more control about how the files are loaded and processed,
/// see [parser::parse], [resolver::resolve] and [compiler::compile].
/// Use [run] or [VM](crate::runtime::VM::run) to execute the given program.
#[cfg(feature="compiler")]
pub fn build<F, P>(source_file: P) -> Result<Program<F>, BuildError> where F: bytecode::VMFunc<F>, P: AsRef<std::path::Path> {
    let source_file = source_file.as_ref();
    let mut files = std::collections::HashMap::new();
    match build_inner(source_file, &mut files) {
        Ok(program) => Ok(program),
        Err(error) => {
            let (filename, source) = files.remove(error.module_path()).unwrap_or_default();
            Err(BuildError { error, filename, source })
        }
    }
}

#[cfg(feature="compiler")]
fn build_inner<F>(source_file: &std::path::Path, files: &mut std::collections::HashMap<String, (std::path::PathBuf, String)>) -> Result<Program<F>, Error> where F: bytecode::VMFunc<F> {
    let parsed = parser::parse(|module_path, state| {
        let mut filename = parser::module_filename(source_file, module_path, false);
        let file = std::fs::read_to_string(&filename).or_else(|_| {
            filename = parser::module_filename(source_file, module_path, true);
            std::fs::read_to_string(&filename)
        })?;
        let module = parser::parse_module(state, &file, module_path);
        files.insert(module_path.to_string(), (filename, file));
        module
    })?;
    let resolved = resolver::resolve::<F>(parsed, "main")?;
    Ok(compiler::compile(resolved)?)
}

/// Runs the given compiled program.
///
/// The name of the entry function must be `main`. See [VM](crate::runtime::VM) for more control
/// about running a program or [build_str] for an example that uses the `run` function.
#[cfg(feature="runtime")]
pub fn run<F, D>(program: Program<F>, context: &mut D) -> Result<bytecode::runtime::vm::VM<F, D>, bytecode::runtime::error::RuntimeError> where F: bytecode::VMFunc<F> + bytecode::VMData<F, D> {
    use bytecode::runtime::vm::VM;
    let mut vm = VM::new(program);
    vm.run(context).map(|_| vm)
}