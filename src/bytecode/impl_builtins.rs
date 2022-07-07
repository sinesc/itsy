/// Macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_builtins {
    // push return values
    (@handle_ret_value $vm:ident, bool, $value:ident) => { $vm.stack.push($value as u8); };
    (@handle_ret_value $vm:ident, String, $value:ident) => { {
        let index = $vm.heap.alloc_copy($value.as_bytes(), $crate::ItemIndex::MAX);
        $vm.stack.push(HeapRef::new(index, 0));
    } };
    (@handle_ret_value $vm:ident, $_:tt, $value:ident) => { $vm.stack.push($value);  };
    // pop parameter values
    (@handle_param_value $vm:ident, bool) => { { let tmp: u8 = $vm.stack.pop(); tmp != 0 } };
    (@handle_param_value $vm:ident, $_:tt) => { $vm.stack.pop() };
    // translate parameter types
    (@handle_param_type String) => { HeapRef };
    (@handle_param_type str) => { HeapRef };
    (@handle_param_type Array) => { HeapRef };
    (@handle_param_type $other:ident) => { $other };
    // convert ref-param values
    (@handle_ref_param_value $vm:ident, String, $arg_name:ident) => { $vm.heap.string($arg_name).to_string() };
    (@handle_ref_param_value $vm:ident, str, $arg_name:ident) => { $vm.heap.string($arg_name) };
    (@handle_ref_param_value $vm:ident, $other:ident, $arg_name:ident) => { $arg_name };
    // translate ref-param pseudo types to actual types
    (@handle_ref_param_type str) => { &str };
    (@handle_ref_param_type Array) => { HeapRef };
    (@handle_ref_param_type $other:ident) => { $other };
    // implement refcount handling for ref-params
    (@handle_ref_param_free $vm:ident, String, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { $vm.heap.ref_item($arg_name.index(), $crate::bytecode::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, str, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { $vm.heap.ref_item($arg_name.index(), $crate::bytecode::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, Array, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { $vm.refcount_value(HeapRef::new($arg_name.index(), 0), $constructor, $crate::bytecode::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, HeapRef, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { $vm.refcount_value(HeapRef::new($arg_name.index(), 0), $element_constructor, $crate::bytecode::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, $other:ident, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { };
    // reverse argument load order
    (@load_args_reverse $vm:ident [] $($arg_name:ident $arg_type:ident)*) => {
        $(
            let $arg_name: impl_builtins!(@handle_param_type $arg_type) = impl_builtins!(@handle_param_value $vm, $arg_type);
        )*
    };
    (@load_args_reverse $vm:ident [ $first_arg_name:ident $first_arg_type:ident $($rest:tt)* ] $($reversed:tt)*) => {
        impl_builtins!(@load_args_reverse $vm [ $($rest)* ] $first_arg_name $first_arg_type $($reversed)*)
    };
    // type id mapping for resolver function
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Self) => { $type_id };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Element) => { $inner_type_id.expect("Generic type not defined but used.") };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, $primitive:ident) => { $resolver.primitive_type_id(Type::$primitive).unwrap() };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident) => { TypeId::void() };
    // write implementation variant
    (@write $compiler:ident, $ty:ident, $element_ty:ident, $variant_8:ident, $variant_16:ident, $variant_32:ident, $variant_64:ident, $variant_x:ident) => { {
        let $element_ty = $element_ty.unwrap();
        if $element_ty.is_ref() {
            let constructor = $compiler.get_constructor($ty);
            let element_constructor = $compiler.get_constructor($element_ty);
            $compiler.writer.builtincallx(Builtin::$variant_x, constructor, element_constructor);
        } else {
            let constructor = $compiler.get_constructor($ty);
            match $element_ty.primitive_size() {
                8 => $compiler.writer.builtincallx(Builtin::$variant_64, constructor, 0),
                4 => $compiler.writer.builtincallx(Builtin::$variant_32, constructor, 0),
                2 => $compiler.writer.builtincallx(Builtin::$variant_16, constructor, 0),
                1 => $compiler.writer.builtincallx(Builtin::$variant_8, constructor, 0),
                _ => unreachable!("Invalid type size for builtin call"),
            };
        }
    } };
    (@write $compiler:ident, $ty:ident, $element_ty:ident, $variant_32:ident, $variant_64:ident) => { {
        match $ty.primitive_size() {
            8 => $compiler.writer.builtincall(Builtin::$variant_64),
            4 => $compiler.writer.builtincall(Builtin::$variant_32),
            _ => unreachable!("Invalid type size for builtin call"),
        };
    } };
    (@write $compiler:ident, $ty:ident, $element_ty:ident, $variant:ident) => { {
        $compiler.writer.builtincall(Builtin::$variant)
    } };
    // main definition block
    (
        $(
            $( #[ $builtin_type_attr:meta ] )*
            $builtin_type:ident { $(
                $( #[ $attr:meta ] )*
                $builtin_function:ident ( $( $doc_arg:ident : $doc_type:ident ),* ) $( -> $doc_result_type:ident )? { $(
                    fn
                    $( /* either multiple function variants */
                        < $( $variant_name:ident $( < $( $generic_name:ident : $generic_type:ident ),+ > )? ( $( $variant_arg:ident : $variant_type:ident $( as $variant_type_as:ident )? ),* ) $( -> $variant_ret_type:ident )? ),+ $(,)? >
                        ( & mut $variant_vm:ident )
                    )?
                    $(
                        $name:ident ( $( & mut $vm:ident $( + $element_constructor:ident )? , )?  $( $arg_name:ident : $( & )? $arg_type:ident ),* ) $( -> $ret_type:ident )?
                    )?
                    $code:block
                )+ }
            )+ }
        )+
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
                    pub(crate) fn write<T>(self: &Self, compiler: &Compiler<T>, type_id: TypeId, inner_type_id: Option<TypeId>) where T: VMFunc<T> {
                        let ty = compiler.type_by_id(type_id);
                        let element_ty = inner_type_id.map(|type_id| compiler.type_by_id(type_id));
                        match self {
                            $( // function
                                Self::$builtin_function => {
                                    impl_builtins!(@write compiler, ty, element_ty
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
            //! This module is only built with `cfg(doc)` enabled and used as a place to document the builtin types available in Itsy.
            //!
            //! Most of the builtins are thin wrappers over Rust `std` methods. Much of the documentation is copied from the standard library documentation.
            $(
                $( #[ $builtin_type_attr ] )*
                pub struct $builtin_type { }
            )+
            struct Element { }
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
            pub(crate) fn exec<T, U>(self: Self, vm: &mut crate::bytecode::runtime::vm::VM<T, U>, constructor: StackAddress, element_constructor: StackAddress) {
                use $crate::bytecode::runtime::{heap::HeapOp, stack::StackOp};
                use builtin_functions::*;
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
                                                    let $variant_arg: impl_builtins!(@handle_ref_param_type $variant_type) = impl_builtins!(@handle_ref_param_value vm, $variant_type, $variant_arg);
                                                )*
                                                builtin_functions::$variant_name(vm, /*constructor, element_constructor,*/ $( $variant_arg ),* )
                                            };
                                            // Handle refcounting.
                                            $(
                                                impl_builtins!(@handle_ref_param_free vm, $variant_type, $variant_arg, constructor, element_constructor);
                                            )*
                                            // Set return value, if any.
                                            $(
                                                let ret_typed: $variant_ret_type = ret;
                                                impl_builtins!(@handle_ret_value vm, $variant_ret_type, ret_typed);
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
                                                let $arg_name: impl_builtins!(@handle_ref_param_type $arg_type) = impl_builtins!(@handle_ref_param_value vm, $arg_type, $arg_name);
                                            )*
                                            // note: borrowing issues with this. probably solvable but we really only need the variants to be able to call each other. single builtins would get no benefit from being moved to functions.
                                            //builtin_functions::$name(vm, constructor, element_constructor, $( $arg_name ),* )
                                            $(
                                                let $vm = &mut *vm;
                                                $( let $element_constructor = element_constructor; )?
                                            )?
                                            $code
                                        };
                                        // Handle refcounting.
                                        $(
                                            impl_builtins!(@handle_ref_param_free vm, $arg_type, $arg_name, constructor, element_constructor);
                                        )*
                                        // Set return value, if any.
                                        $(
                                            let ret_typed: $ret_type = ret;
                                            impl_builtins!(@handle_ret_value vm, $ret_type, ret_typed);
                                        )?
                                    },
                                )?
                            )+
                        )+
                    )+
                }
            }
        }

        #[cfg(feature="runtime")]
        mod builtin_functions {
            use super::*;
            $( // type
                $( // function
                    $( // implementation

                        $( // builtin variants
                            $(
                                #[allow(unused_variables, unused_assignments, unused_imports)]
                                #[cfg(feature="runtime")]
                                pub(super) fn $variant_name<T, U>(vm: &mut crate::bytecode::runtime::vm::VM<T, U>, /*constructor: StackAddress, element_constructor: StackAddress,*/ $( $variant_arg : impl_builtins!(@handle_ref_param_type $variant_type) ),* ) $( -> $variant_ret_type )? {
                                    use $crate::bytecode::runtime::{heap::HeapOp, stack::StackOp};
                                    $(
                                        #[allow(dead_code)]
                                        $( type $generic_name = $generic_type; )+
                                    )?
                                    let $variant_vm = &mut *vm;
                                    $code
                                }
                            )+
                        )?
                        /* (see exec())
                        $( // single variant
                            #[allow(unused_variables, unused_assignments, unused_imports)]
                            #[cfg(feature="runtime")]
                            pub(super) fn $name<T, U>(vm: &mut crate::bytecode::runtime::vm::VM<T, U>, constructor: StackAddress, element_constructor: StackAddress, $( $arg_name : impl_builtins!(@handle_ref_param_type $arg_type) ),* ) $( -> $ret_type )? {
                                use $crate::bytecode::runtime::{heap::HeapOp, stack::StackOp};
                                #[allow(dead_code)]
                                /// Single variant builtins don't provide T. This definition of T is intended to shadow the VM's generic T in order to trigger an error on accidental use. This is not the T you are looking for.
                                trait T { }
                                #[allow(dead_code)]
                                /// Single variant builtins don't provide U. This definition of U is intended to shadow the VM's generic U in order to trigger an error on accidental use. This is not the U you are looking for.
                                trait U { }
                                $(
                                    let $vm = &mut *vm;
                                    $( let $element_constructor = element_constructor; )?
                                )?
                                $code
                            }
                        )?
                        */

                    )+
                )+
            )+
        }
    };
}
