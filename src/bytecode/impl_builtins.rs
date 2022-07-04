/// Macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_builtins {
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
        let index = $vm.heap.alloc_copy($value.as_bytes(), $crate::ItemIndex::MAX);
        $vm.stack.push($crate::bytecode::HeapRef::new(index, 0));
    } };
    (@handle_ret $vm:ident, HeapRef, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, StackAddress, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, StackOffset, $value:ident) => { $vm.stack.push($value); };
    (@handle_ret $vm:ident, $_:tt, $value:ident) => { compile_error!("Unsupported return type"); };
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
    (@handle_param $vm:ident, String) => { $vm.stack.pop() };
    (@handle_param $vm:ident, str) => { $vm.stack.pop() };
    (@handle_param $vm:ident, HeapRef) => { $vm.stack.pop() };
    (@handle_param $vm:ident, StackAddress) => { $vm.stack.pop() };
    (@handle_param $vm:ident, StackOffset) => { $vm.stack.pop() };
    (@handle_param $vm:ident, $_:tt) => { compile_error!("Unsupported parameter type") };
    // translate parameter types
    (@handle_param_type String) => { $crate::bytecode::HeapRef };
    (@handle_param_type str) => { $crate::bytecode::HeapRef };
    (@handle_param_type $other:ident) => { $other };
    // translate ref-param values
    (@handle_ref_param $vm:ident, String, $arg_name:ident) => { $vm.heap.string($arg_name).to_string() };
    (@handle_ref_param $vm:ident, str, $arg_name:ident) => { $vm.heap.string($arg_name) };
    (@handle_ref_param $vm:ident, $other:ident, $arg_name:ident) => { $arg_name };
    // translate ref-param types
    (@handle_ref_param_type str) => { &str }; // FIXME: hack to support &str, see fixmes in main arm. remove these two once fixed
    (@handle_ref_param_type $other:ident) => { $other };
    // refcount handling for ref-params
    (@handle_ref_param_free $vm:ident, String, $arg_name:ident) => { $vm.heap.ref_item($arg_name.index(), $crate::bytecode::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, str, $arg_name:ident) => { $vm.heap.ref_item($arg_name.index(), $crate::bytecode::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, HeapRef, $arg_name:ident) => { $vm.heap.ref_item($arg_name.index(), $crate::bytecode::HeapRefOp::Free) }; // FIXME need constructor here
    (@handle_ref_param_free $vm:ident, $other:ident, $arg_name:ident) => { };
    // reverse argument load order
    (@load_args_reverse $vm:ident [] $($arg_name:ident $arg_type:ident)*) => {
        $(
            let $arg_name: impl_builtins!(@handle_param_type $arg_type) = impl_builtins!(@handle_param $vm, $arg_type);
        )*
    };
    (@load_args_reverse $vm:ident [ $first_arg_name:ident $first_arg_type:ident $($rest:tt)* ] $($reversed:tt)*) => {
        impl_builtins!(@load_args_reverse $vm [ $($rest)* ] $first_arg_name $first_arg_type $($reversed)*)
    };
    // type id mapping for resolver function
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Self) => { $type_id };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Any) => { $inner_type_id.expect("Generic type not defined but used.") };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, $primitive:ident) => { $resolver.primitive_type_id(Type::$primitive).unwrap() };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident) => { TypeId::void() };
    // write implementation variant
    (@write $compiler:ident, $ty:ident, $variant_8:ident, $variant_16:ident, $variant_32:ident, $variant_64:ident, $variant_x:ident) => { {
        if $ty.is_ref() {
            let constructor = $compiler.get_constructor($ty);
            $compiler.writer.builtincallx(Builtin::$variant_x, constructor);
        } else {
            match $ty.primitive_size() {
                8 => $compiler.writer.builtincall(Builtin::$variant_64),
                4 => $compiler.writer.builtincall(Builtin::$variant_32),
                2 => $compiler.writer.builtincall(Builtin::$variant_16),
                1 => $compiler.writer.builtincall(Builtin::$variant_8),
                _ => unreachable!("Invalid type size for builtin call"),
            };
        }
    } };
    (@write $compiler:ident, $ty:ident, $variant_32:ident, $variant_64:ident) => { {
        match $ty.primitive_size() {
            8 => $compiler.writer.builtincall(Builtin::$variant_64),
            4 => $compiler.writer.builtincall(Builtin::$variant_32),
            _ => unreachable!("Invalid type size for builtin call"),
        };
    } };
    (@write $compiler:ident, $ty:ident, $variant:ident) => { {
        $compiler.writer.builtincall(Builtin::$variant)
    } };
    // main definition block
    (
        $( $builtin_type:ident { $(
            $( #[ $attr:meta ] )*
            $builtin_function:ident ( $( $doc_arg:ident : $doc_type:ident ),* ) $( -> $doc_result_type:ident )? { $(
                fn
                $( /* either multiple function variants */
                    < $( $variant_name:ident $( < $( $generic_name:ident : $generic_type:ident ),+ > )? ( $( $variant_arg:ident : $variant_type:ident $( as $variant_type_as:ident )? ),* ) $( -> $variant_ret_type:ident )? ),+ $(,)? >
                    ( & mut $variant_vm:ident )
                )?
                $(
                    $name:ident ( $( & mut $vm:ident $( + $constructor:ident )? , )?  $( $arg_name:ident : $( & )? $arg_type:ident ),* ) $( -> $ret_type:ident )?
                )?
                $code:block
            )+ }
        )+ } )+
    ) => {

        /// Builtin functions callable via the `builtincall` opcode. Generated from method signatures defined via the impl_builtins! macro.
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug)]
        pub enum Builtin {
            $( // type
                $( // function
                    $( #[ $attr ] )*
                    $( // implementation
                        $( // builtin variants
                            $(
                                $variant_name,
                            )+
                        )?
                        $( // single builtin
                            $name,
                        )?
                    )+
                )+
            )+
        }

        #[cfg(feature="compiler")]
        #[derive(Copy, Clone, Debug, PartialEq)]
        pub enum BuiltinType {
            $( // type
                $builtin_type(builtin_types::$builtin_type),
            )+
        }

        #[cfg(feature="compiler")]
        pub mod builtin_types {
            use super::{Builtin, BuiltinType};
            use crate::frontend::resolver::Resolver;
            use crate::bytecode::{compiler::Compiler, VMFunc};
            use crate::shared::{meta::Type, typed_ids::TypeId, TypeContainer};

            $( // type
                #[allow(non_camel_case_types)]
                #[derive(Copy, Clone, Debug, PartialEq)]
                pub enum $builtin_type {
                    $( // function
                        $builtin_function,
                    )+
                }

                impl $builtin_type {
                    #[allow(unused_variables)]
                    pub(crate) fn resolve(resolver: &Resolver, name: &str, type_id: TypeId, inner_type_id: Option<TypeId>) -> Option<(BuiltinType, TypeId, Vec<TypeId>)> {
                        match name {
                            $( // function
                                stringify!($builtin_function) => {
                                    Some((
                                        super::BuiltinType::$builtin_type($builtin_type::$builtin_function),
                                        impl_builtins!(@type_map resolver, type_id, inner_type_id $(, $doc_result_type )?),
                                        vec![ $( impl_builtins!(@type_map resolver, type_id, inner_type_id, $doc_type) ),* ]
                                    ))
                                },
                            )+
                            _ => None,
                        }
                    }
                    #[allow(unused_variables)]
                    pub(crate) fn write<T>(self: &Self, compiler: &Compiler<T>, type_id: TypeId) where T: VMFunc<T> {
                        let ty = compiler.type_by_id(type_id);
                        match self {
                            $( // function
                                Self::$builtin_function => {
                                    impl_builtins!(@write compiler, ty
                                        $( // implementation
                                            $( // builtin variants
                                                $(
                                                    , $variant_name
                                                )+
                                            )?
                                            $( // single builtin
                                                , $name
                                            )?
                                        )+
                                    );
                                }
                            )+
                        }
                    }
                }
            )+
        }

        #[cfg(doc)]
        pub mod documentation {
            //! Generated documentation of builtin Itsy types.
            //!
            //! This module is only built with cargo doc and not generally available to Rust code. It is used as a place to document the builtin
            //! types available from within Itsy code. Due to limitations in the generation the following simplifications were made:
            //!
            //! - `Array` documents the builtin `[ ]` type. `Any` here refers to the type the array was specialized with.
            //! - `Float` documents the builtin `f32` and `f64` types. There is no actual `Float` type in Itsy.
            //! - `String` documents the builtin `String`-type.
            //!
            //! Most of the builtins are thin wrappers over Rust `std` methods. The `Float` documentation is mostly copied from the standard library documentation.
            $( pub struct $builtin_type { } )+
            struct Any { }
            $( // type
                impl $builtin_type {
                    $( // function
                        $( #[ $attr ] )*
                        pub fn $builtin_function( $( $doc_arg : $doc_type ),* ) $( -> $doc_result_type )? { }
                    )+
                }
            )+
        }

        impl Builtin {
            #[cfg(feature="compiler")]
            pub(crate) fn to_index(self: Self) -> $crate::BuiltinIndex {
                self as $crate::BuiltinIndex
            }
            #[cfg(feature="runtime")]
            pub(crate) fn from_index(index: $crate::BuiltinIndex) -> Self {
                match index {
                    $( // type
                        $( // function
                            $( // implementation
                                $( // builtin variants
                                    $(
                                        x if x == Self::$variant_name as $crate::BuiltinIndex => Self::$variant_name,
                                    )+
                                )?
                                $( // single builtin
                                    x if x == Self::$name as $crate::BuiltinIndex => Self::$name,
                                )?
                            )+
                        )+
                    )+
                    _ => panic!("Invalid Builtin index {}", index),
                }
            }
            #[allow(unused_variables, unused_assignments, unused_imports)]
            #[cfg(feature="runtime")]
            pub(crate) fn exec<T, U>(self: Self, vm: &mut crate::bytecode::runtime::vm::VM<T, U>, constructor: StackAddress) {
                use $crate::bytecode::runtime::{heap::HeapOp, stack::StackOp};
                match self {
                    $( // type
                        $( // function
                            $( // implementation
                                $( // builtin variants
                                    $(
                                        Builtin::$variant_name => {
                                            $(
                                                #[allow(dead_code)]
                                                $( type $generic_name = $generic_type; )+
                                            )?
                                            // Load arguments. For references this loads the HeapRef. We'll need it later to handle refcounts.
                                            impl_builtins!(@load_args_reverse vm [ $( $variant_arg $variant_type )* ]);
                                            // Run code.
                                            let ret = {
                                                // Shadow HeapRef arguments with actual argument value.
                                                $(
                                                    let $variant_arg: impl_builtins!(@handle_ref_param_type $variant_type) = impl_builtins!(@handle_ref_param vm, $variant_type, $variant_arg);
                                                )*
                                                let $variant_vm = &mut *vm;
                                                $code
                                            };
                                            // Handle refcounting.
                                            $(
                                                impl_builtins!(@handle_ref_param_free vm, $variant_type, $variant_arg);
                                            )*
                                            // Set return value, if any.
                                            $(
                                                let ret_typed: $variant_ret_type = ret;
                                                impl_builtins!(@handle_ret vm, $variant_ret_type, ret_typed);
                                            )?
                                        },
                                    )+
                                )?
                                $( // single variant
                                    Builtin::$name => {
                                        #[allow(dead_code)]
                                        /// Single variant builtins don't provide T. This definition of T is intended to shadow the VM's generic T in order to trigger an error on accidental use. This is not the T you are looking for.
                                        trait T { }
                                        #[allow(dead_code)]
                                        /// Single variant builtins don't provide U. This definition of U is intended to shadow the VM's generic U in order to trigger an error on accidental use. This is not the U you are looking for.
                                        trait U { }
                                        // Load arguments. For references this loads the HeapRef. We'll need it later to handle refcounts.
                                        impl_builtins!(@load_args_reverse vm [ $( $arg_name $arg_type )* ]);
                                        // Run code.
                                        let ret = {
                                            // Shadow HeapRef arguments with actual argument value.
                                            $(
                                                let $arg_name: impl_builtins!(@handle_ref_param_type $arg_type) = impl_builtins!(@handle_ref_param vm, $arg_type, $arg_name);
                                            )*
                                            $(
                                                let $vm = &mut *vm;
                                                $( let $constructor = constructor; )?
                                            )?
                                            $code
                                        };
                                        // Handle refcounting.
                                        $(
                                            impl_builtins!(@handle_ref_param_free vm, $arg_type, $arg_name);
                                        )*
                                        // Set return value, if any.
                                        $(
                                            let ret_typed: $ret_type = ret;
                                            impl_builtins!(@handle_ret vm, $ret_type, ret_typed);
                                        )?
                                    },
                                )?
                            )+
                        )+
                    )+
                }
            }
        }
    };
}
