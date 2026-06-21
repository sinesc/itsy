/// Macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_builtins {
    // VM: Pushes the return value and converts some special cases.
    // The fourth parameter ($constructor) is the owning-type constructor (needed by Value/Map for refcounting).
    (@handle_ret_value $vm:ident, bool, $value:ident, $_ctor:ident) => { $vm.stack.push($value as u8); };
    (@handle_ret_value $vm:ident, String, $value:ident, $_ctor:ident) => { {
        let index = $vm.heap.alloc_copy($value.as_bytes(), $crate::ItemIndex::MAX);
        $vm.stack.push(HeapRef::new(index, 0));
    } };
    (@handle_ret_value $vm:ident, Value, $value:ident, $constructor:ident) => { {
        use $crate::bytecode::runtime::{heap::HeapOp, stack::StackOp};
        let (_key_ctor, value_ctor) = $crate::bytecode::Constructor::map_sub_constructors(&$vm.stack, $constructor);
        if $crate::bytecode::Constructor::is_primitive(&$vm.stack, value_ctor) {
            let n = $crate::bytecode::Constructor::primitive_size(&$vm.stack, value_ctor);
            $vm.map_unbox_push_primitive($value, n);
            $vm.heap.ref_item($value.index(), $crate::bytecode::HeapRefOp::Free);
        } else {
            $vm.heap.ref_item($value.index(), $crate::bytecode::HeapRefOp::Inc);
            $vm.stack.push($value);
            $vm.heap.ref_item($value.index(), $crate::bytecode::HeapRefOp::DecNoFree);
        }
    } };
    (@handle_ret_value $vm:ident, Map, $value:ident, $_ctor:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, KeyArray, $value:ident, $_ctor:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, ValueArray, $value:ident, $_ctor:ident) => { $vm.stack.push($value); };
    (@handle_ret_value $vm:ident, $_:tt, $value:ident, $_ctor:ident) => { $vm.stack.push($value);  };
    // VM: Pops an argument and converts some special cases.
    // Note: Map/Key/Value/KeyArray/ValueArray are handled by @load_args_map and will not reach this rule.
    (@handle_param_value $vm:ident, bool) => { { let tmp: u8 = $vm.stack.pop(); tmp != 0 } };
    (@handle_param_value $vm:ident, $_:tt) => { $vm.stack.pop() };
    // VM: Translate parameter list/result pseudo-types to internally used types.
    (@handle_param_type String) => { HeapRef };
    (@handle_param_type str) => { HeapRef };
    (@handle_param_type Array) => { HeapRef };
    (@handle_param_type Element) => { HeapRef };
    (@handle_param_type Map) => { HeapRef };
    (@handle_param_type Key) => { HeapRef };
    (@handle_param_type Value) => { HeapRef };
    (@handle_param_type KeyArray) => { HeapRef };
    (@handle_param_type ValueArray) => { HeapRef };
    (@handle_param_type GenValue) => { HeapRef };
    (@handle_param_type GenKey) => { HeapRef };
    (@handle_param_type $other:ident) => { $other };
    // VM: Convert some pseudo-type arguments to values of concrete rust types. This happens after the value was initially loaded as the type prescribed by @handle_param_type.
    (@handle_ref_param_value $vm:ident, String, $arg_name:ident) => { $vm.heap.string($arg_name).unwrap().to_string() };
    (@handle_ref_param_value $vm:ident, str, $arg_name:ident) => { $vm.heap.string($arg_name).unwrap() };
    (@handle_ref_param_value $vm:ident, Map, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, Key, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, Value, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, KeyArray, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, ValueArray, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, GenValue, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, GenKey, $arg_name:ident) => { $arg_name };
    (@handle_ref_param_value $vm:ident, $other:ident, $arg_name:ident) => { $arg_name };
    // VM: Translate some pseudo-type names to concrete rust types.
    (@map_ref_type str) => { &str };
    (@map_ref_type Array) => { HeapRef };
    (@map_ref_type Element) => { HeapRef };
    (@map_ref_type Map) => { HeapRef };
    (@map_ref_type Key) => { HeapRef };
    (@map_ref_type Value) => { HeapRef };
    (@map_ref_type KeyArray) => { HeapRef };
    (@map_ref_type ValueArray) => { HeapRef };
    (@map_ref_type GenValue) => { HeapRef };
    (@map_ref_type GenKey) => { HeapRef };
    (@map_ref_type $other:ident) => { $other };
    // VM: Generate reference counting code for some pseudo type arguments.
    (@handle_ref_param_free $vm:ident, String, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { $vm.heap.ref_item($arg_name.index(), $crate::bytecode::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, str, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { $vm.heap.ref_item($arg_name.index(), $crate::bytecode::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, Array, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { $vm.refcount_value(HeapRef::new($arg_name.index(), 0), $constructor, $crate::bytecode::HeapRefOp::Free) };
    (@handle_ref_param_free $vm:ident, Element, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { $vm.refcount_value(HeapRef::new($arg_name.index(), 0), $element_constructor, $crate::bytecode::HeapRefOp::Free) };
    // Map pseudo-types: refcounting is handled inside the code block, not by the macro.
    (@handle_ref_param_free $vm:ident, Map, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { };
    (@handle_ref_param_free $vm:ident, Key, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { };
    (@handle_ref_param_free $vm:ident, Value, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { };
    (@handle_ref_param_free $vm:ident, KeyArray, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { };
    (@handle_ref_param_free $vm:ident, ValueArray, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { };
    // Generator pseudo-types: refcounting is not needed (opcodes handle it directly).
    (@handle_ref_param_free $vm:ident, GenValue, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { };
    (@handle_ref_param_free $vm:ident, GenKey, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { };
    (@handle_ref_param_free $vm:ident, $other:ident, $arg_name:ident, $constructor:ident, $element_constructor:ident) => { };
    // VM: Pops arguments in reverse order off the stack (so that it is the correct order for the function call).
    (@load_args_reverse $vm:ident [] $($arg_name:ident $arg_type:ident)*) => {
        $(
            let $arg_name: impl_builtins!(@handle_param_type $arg_type) = impl_builtins!(@handle_param_value $vm, $arg_type);
        )*
    };
    (@load_args_reverse $vm:ident [ $first_arg_name:ident $first_arg_type:ident $($rest:tt)* ] $($reversed:tt)*) => {
        impl_builtins!(@load_args_reverse $vm [ $($rest)* ] $first_arg_name $first_arg_type $($reversed)*);
    };
    // VM: Load arguments for single-variant builtins, with awareness of Map sub-constructors.
    // Like @load_args_reverse but handles Map/Key/Value correctly using the constructor.
    // Uses element_constructor for Map/Key/Value (the map constructor is passed as element_constructor by the compiler).
    // Processes args in reverse order (right-to-left) so stack pops happen in the correct LIFO order.
    (@load_args_map $vm:ident, $constructor:ident, $element_constructor:ident []) => { };
    (@load_args_map $vm:ident, $constructor:ident, $element_constructor:ident [$first:ident $first_type:tt $($rest:tt)*]) => {
        impl_builtins!(@load_args_map $vm, $constructor, $element_constructor [$($rest)*]);
        impl_builtins!(@load_args_map_one $vm, $constructor, $element_constructor, $first, $first_type);
    };
    (@load_args_map_one $vm:ident, $_constructor:ident, $element_constructor:ident, $arg_name:ident, Key) => {
        let (key_ctor, _) = $crate::bytecode::Constructor::map_sub_constructors(&$vm.stack, $element_constructor);
        let $arg_name: HeapRef = $vm.map_box_pop(key_ctor);
    };
    (@load_args_map_one $vm:ident, $_constructor:ident, $element_constructor:ident, $arg_name:ident, Value) => {
        let (_, value_ctor) = $crate::bytecode::Constructor::map_sub_constructors(&$vm.stack, $element_constructor);
        let $arg_name: HeapRef = $vm.map_box_pop(value_ctor);
    };
    (@load_args_map_one $vm:ident, $_constructor:ident, $_element_constructor:ident, $arg_name:ident, Map) => {
        let $arg_name: HeapRef = $vm.stack.pop();
    };
    (@load_args_map_one $vm:ident, $_constructor:ident, $_element_constructor:ident, $arg_name:ident, $arg_type:tt) => {
        let $arg_name: impl_builtins!(@handle_param_type $arg_type) = impl_builtins!(@handle_param_value $vm, $arg_type);
    };
    // Resolver: Type id mapping for resolve() function
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Self) => { $type_id };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Map) => { $type_id };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Array) => { $type_id };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Element) => { $inner_type_id.expect("Generic inner type is not resolved.") };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Key) => { {
        let map_ty = $resolver.type_by_id($type_id).as_map().unwrap();
        map_ty.key_type_id.unwrap()
    } };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, Value) => { {
        let map_ty = $resolver.type_by_id($type_id).as_map().unwrap();
        map_ty.value_type_id.unwrap()
    } };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, KeyArray) => { {
        let map_ty = $resolver.type_by_id($type_id).as_map().unwrap();
        $resolver.create_array_type(map_ty.key_type_id.unwrap())
    } };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, ValueArray) => { {
        let map_ty = $resolver.type_by_id($type_id).as_map().unwrap();
        $resolver.create_array_type(map_ty.value_type_id.unwrap())
    } };
    // Generator pseudo-types: resolve via generator_signature.
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, GenValue) => { {
        let (_key_type_id, value_type_id) = $resolver.generator_signature($type_id).unwrap();
        value_type_id
    } };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, GenKey) => { {
        let (key_type_id, _value_type_id) = $resolver.generator_signature($type_id).unwrap();
        key_type_id.unwrap()
    } };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, str) => { $resolver.primitive_type_id(Type::String).unwrap() };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, StackAddress) => { $resolver.primitive_type_id(crate::STACK_ADDRESS_TYPE).unwrap() };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, StackOffset) => { $resolver.primitive_type_id(crate::STACK_OFFSET_TYPE).unwrap() };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, HeapRef) => { $inner_type_id.expect("Generic inner type is not resolved.") };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident, $primitive:ident) => { $resolver.primitive_type_id(Type::$primitive).unwrap() };
    (@type_map $resolver:ident, $type_id:ident, $inner_type_id:ident) => { TypeId::VOID };
    // Compiler: Write implementation variants
    // TODO these cases are super ugly. maybe a muncher could make this better?
    (@write $compiler:ident, $type_id:ident, $ty:ident, $element_ty:ident, $variant_8:ident, $variant_16:ident, $variant_32:ident, $variant_64:ident, $variant_x:ident) => { {
        let $element_ty = $element_ty.unwrap();
        if $element_ty.is_ref() {
            let constructor = $compiler.constructor($ty).unwrap();
            let element_constructor = $compiler.constructor($element_ty).unwrap();
            $compiler.writer.call_builtinx(Builtin::$variant_x, constructor, element_constructor);
        } else {
            let constructor = $compiler.constructor($ty).unwrap();
            match $element_ty.primitive_size() {
                8 => $compiler.writer.call_builtinx(Builtin::$variant_64, constructor, 0),
                4 => $compiler.writer.call_builtinx(Builtin::$variant_32, constructor, 0),
                2 => $compiler.writer.call_builtinx(Builtin::$variant_16, constructor, 0),
                1 => $compiler.writer.call_builtinx(Builtin::$variant_8, constructor, 0),
                _ => panic!("Invalid type size for builtin call."),
            };
        }
    } };
    (@write $compiler:ident, $type_id:ident, $ty:ident, $element_ty:ident, $variant_32:ident, $variant_64:ident) => { {
        match $ty.primitive_size() {
            8 => $compiler.writer.call_builtin(Builtin::$variant_64),
            4 => $compiler.writer.call_builtin(Builtin::$variant_32),
            _ => panic!("Invalid type size for builtin call."),
        };
    } };
    (@write $compiler:ident, $type_id:ident, $ty:ident, $element_ty:ident, $variant_8:ident, $variant_16:ident, $variant_32:ident, $variant_64:ident) => { {
        match $ty.primitive_size() {
            8 => $compiler.writer.call_builtin(Builtin::$variant_64),
            4 => $compiler.writer.call_builtin(Builtin::$variant_32),
            2 => $compiler.writer.call_builtin(Builtin::$variant_16),
            1 => $compiler.writer.call_builtin(Builtin::$variant_8),
            _ => panic!("Invalid type size for builtin call."),
        };
    } };
    (@write $compiler:ident, $type_id:ident, $ty:ident, $element_ty:ident, $variant_i8:ident, $variant_i16:ident, $variant_i32:ident, $variant_i64:ident, $variant_u8:ident, $variant_u16:ident, $variant_u32:ident, $variant_u64:ident) => { {
        if $ty.is_signed() {
            match $ty.primitive_size() {
                8 => $compiler.writer.call_builtin(Builtin::$variant_i64),
                4 => $compiler.writer.call_builtin(Builtin::$variant_i32),
                2 => $compiler.writer.call_builtin(Builtin::$variant_i16),
                1 => $compiler.writer.call_builtin(Builtin::$variant_i8),
                _ => panic!("Invalid type size for builtin call."),
            };
        } else {
            match $ty.primitive_size() {
                8 => $compiler.writer.call_builtin(Builtin::$variant_u64),
                4 => $compiler.writer.call_builtin(Builtin::$variant_u32),
                2 => $compiler.writer.call_builtin(Builtin::$variant_u16),
                1 => $compiler.writer.call_builtin(Builtin::$variant_u8),
                _ => panic!("Invalid type size for builtin call."),
            };
        }
    } };
    (@write $compiler:ident, $type_id:ident, $ty:ident, $element_ty:ident, $variant:ident) => { {
        if matches!($ty, Type::Map(_)) {
            // Map types need call_builtinx with the map constructor; all others use call_builtin.
            // For maps, we pass the map constructor as element_constructor (second arg) so that
            // the + constructor marker in the function signature binds it correctly.
            let constructor = $compiler.constructor($ty).unwrap();
            let _ = $compiler.writer.call_builtinx(Builtin::$variant, 0, constructor);
        } else if $compiler.generator_signature($type_id).is_some() {
            // Generator types use dedicated gen_* opcodes (not call_builtin) since gen_next cannot
            // be expressed as function (builtin). For these the macro emits the code to write the actual opcodes.
            match stringify!($variant) {
                "gen_next" => { $compiler.writer.gen_next(); },
                "gen_value" => {
                    let (_key_type_id, value_type_id) = $compiler.generator_signature($type_id).unwrap();
                    $compiler.writer.gen_value($compiler.type_by_id(value_type_id).primitive_size() as $crate::FrameAddress);
                },
                "gen_key" => {
                    let (key_type_id, _value_type_id) = $compiler.generator_signature($type_id).unwrap();
                    let key_type_id = key_type_id.unwrap();
                    $compiler.writer.gen_key($compiler.type_by_id(key_type_id).primitive_size() as $crate::FrameAddress);
                },
                _ => panic!("Unknown generator builtin variant: {}", stringify!($variant)),
            }
        } else {
            let _ = $compiler.writer.call_builtin(Builtin::$variant);
        }
    } };
    // Main definition block
    (
        $(
            $( #[ $builtin_type_attr:meta ] )*
            $builtin_type:ident { $(
                $( #[ $attr:meta ] )*
                $builtin_function:ident ( $( $doc_arg:ident : $doc_type:ident ),* ) $( -> $doc_result_type:ident )? { $(
                    fn
                    $( // Either multiple function variants...
                        < $(
                            $vname:ident $( < $vgeneric_name:ident : $vgeneric_type:ident > )? ( $( $varg_name:ident : $varg_type:ident $( as $varg_type_as:ident )? ),* )
                            $( -> $vret_type:ident )?
                        ),+ $(,)? >
                        ( & mut $vvm:ident )
                    )?
                    $( // or single function.
                        $name:ident
                        ( $( & mut $vm:ident $( + $element_constructor:ident )? , )?  $( $arg_name:ident : $( & )? $arg_type:ident ),* )
                        $( -> $ret_type:ident )?
                    )?
                    $code:block
                )+ }
            )+ }
        )+
    ) => {

        /// Builtin functions callable via the `call_builtin` opcode. Generated from method signatures defined via the impl_builtins! macro.
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug)]
        pub enum Builtin {
            $( // type
                $( // function
                    $( #[ $attr ] )*
                    $( // implementation
                        $( // builtin variants
                            $(
                                $vname,
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
            use crate::shared::{meta::Type, typed_ids::TypeId, MetaContainer};
            use crate::bytecode::macros::impl_builtins;

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
                    pub(crate) fn resolve(resolver: &mut Resolver, name: &str, type_id: TypeId, inner_type_id: Option<TypeId>) -> Option<(BuiltinType, TypeId, Vec<TypeId>)> {
                        // The require_type here is only used to typecheck primitive types of the same type-group against each other.
                        // Since the resolver already invokes the correct type-group version of this function (e.g. Integer::resolve vs String::resolve)
                        // we don't have to worry about guarding against types not present in this group.
                        // This allows us to always accept void and String types for each match arm. We use void for arrays (they are generic,
                        // there is nothing to check) as well as String for strings (so that we don't have to annotate each string builting with <T: String>).
                        let require_type = if inner_type_id.is_some() { &Type::void } else { resolver.type_by_id(type_id) };

                        match (name, require_type) {
                            $( // function
                                $( // implementation
                                    $( // Either multiple function variants...
                                        $( // variant
                                            (
                                                stringify!($builtin_function), $( Type::$vgeneric_type )?
                                            )
                                            => {
                                                Some((
                                                    super::BuiltinType::$builtin_type($builtin_type::$builtin_function),
                                                    impl_builtins!(@type_map resolver, type_id, inner_type_id $(, $vret_type )?),
                                                    vec![ $( impl_builtins!(@type_map resolver, type_id, inner_type_id, $varg_type) ),* ]
                                                ))
                                            },
                                        )+
                                    )?
                                    $( // or single function.
                                        (
                                            stringify!($builtin_function), Type::String | Type::void
                                        )
                                        => {
                                            Some((
                                                super::BuiltinType::$builtin_type($builtin_type::$builtin_function),
                                                impl_builtins!(@type_map resolver, type_id, inner_type_id $(, $ret_type )?),
                                                vec![ $( impl_builtins!(@type_map resolver, type_id, inner_type_id, $arg_type ) ),* ]
                                            ))
                                        }
                                    )?
                                )+
                            )+
                            _ => None,
                        }
                    }
                    #[allow(unused_variables)]
                    pub(crate) fn write<T>(self: &Self, compiler: &Compiler<T>, type_id: TypeId, inner_type_id: Option<TypeId>) -> crate::StackAddress where T: VMFunc<T> {
                        let ty = compiler.type_by_id(type_id);
                        let element_ty = inner_type_id.map(|type_id| compiler.type_by_id(type_id));
                        let position = compiler.writer.position();
                        match self {
                            $( // function
                                Self::$builtin_function => {
                                    impl_builtins!(@write compiler, type_id, ty, element_ty
                                        $( // implementation
                                            $( // builtin variants
                                                $(
                                                    , $vname
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
                        position
                    }
                }
            )+
        }

        // Auto-generated per-type documentation items. These are contributed to the hand-owned
        // `documentation` module (defined in builtins.rs) via a glob re-export, so that hand-written
        // entries which are not generated by this macro can live alongside them under one namespace.
        #[cfg(doc)]
        pub(crate) mod builtin_type_documentation {
            // Placeholder types referenced by generated method signatures. Kept private (not
            // re-exported) so they don't render as standalone types, matching their pseudo-type role.
            struct Element { }
            struct UnsignedSelf { }
            struct Key { }
            struct Value { }
            struct KeyArray { }
            struct ValueArray { }
            struct GenKey { }
            struct GenValue { }
            $(
                $( #[ $builtin_type_attr ] )*
                pub struct $builtin_type { }
            )+
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
                // TODO: Turns out this is quite slow. Instead, generate a module containting
                // integer constants for all the builtins, just like impl_opcodes does it for opcodes.
                match index {
                    $( // type
                        $( // function
                            $( // implementation
                                $( // builtin variants
                                    $(
                                        x if x == Self::$vname as $crate::BuiltinIndex => Self::$vname,
                                    )+
                                )?
                                $( // single builtin
                                    x if x == Self::$name as $crate::BuiltinIndex => Self::$name,
                                )?
                            )+
                        )+
                    )+
                    _ => panic!("Invalid Builtin index {}.", index),
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
                                        Builtin::$vname => {
                                            $(
                                                #[allow(dead_code)]
                                                type $vgeneric_name = $vgeneric_type;
                                            )?
                                            // Load arguments. For references this loads the HeapRef. We'll need it later to handle refcounts.
                                            impl_builtins!(@load_args_reverse vm [ $( $varg_name $varg_type )* ]);
                                            // Run code.
                                            let ret = {
                                                // Shadow HeapRef arguments with actual argument value.
                                                $(
                                                    let $varg_name: impl_builtins!(@map_ref_type $varg_type) = impl_builtins!(@handle_ref_param_value vm, $varg_type, $varg_name);
                                                )*
                                                builtin_functions::$vname(vm, /*constructor, element_constructor,*/ $( $varg_name ),* )
                                            };
                                            // Handle refcounting.
                                            $(
                                                impl_builtins!(@handle_ref_param_free vm, $varg_type, $varg_name, constructor, element_constructor);
                                            )*
                                            // Set return value, if any.
                                            $(
                                                let ret_typed: impl_builtins!(@map_ref_type $vret_type) = ret;
                                                impl_builtins!(@handle_ret_value vm, $vret_type, ret_typed, constructor);
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
                                        // Load arguments. For Map builtins this handles Key/Value/Map correctly via map_box_pop/stack.pop().
                                        impl_builtins!(@load_args_map vm, constructor, element_constructor [ $( $arg_name $arg_type )* ]);
                                        // Run code.
                                        let ret = {
                                            // Shadow HeapRef arguments with actual argument value.
                                            $(
                                                let $arg_name: impl_builtins!(@map_ref_type $arg_type) = impl_builtins!(@handle_ref_param_value vm, $arg_type, $arg_name);
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
                                            let ret_typed: impl_builtins!(@map_ref_type $ret_type) = ret;
                                            impl_builtins!(@handle_ret_value vm, $ret_type, ret_typed, element_constructor);
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
                                #[allow(unused_variables, unused_imports, non_snake_case)]
                                #[cfg(feature="runtime")]
                                pub(super) fn $vname<T, U>(vm: &mut crate::bytecode::runtime::vm::VM<T, U>, /*constructor: StackAddress, element_constructor: StackAddress,*/ $( $varg_name : impl_builtins!(@map_ref_type $varg_type) ),* ) $( -> impl_builtins!(@map_ref_type $vret_type) )? {
                                    use $crate::bytecode::runtime::{heap::HeapOp, stack::StackOp};
                                    $(
                                        #[allow(dead_code)]
                                        type $vgeneric_name = $vgeneric_type;
                                    )?
                                    let $vvm = &mut *vm;
                                    $code
                                }
                            )+
                        )?
                        /* (see exec())
                        $( // single variant
                            #[allow(unused_variables, unused_assignments, unused_imports)]
                            #[cfg(feature="runtime")]
                            pub(super) fn $name<T, U>(vm: &mut crate::bytecode::runtime::vm::VM<T, U>, constructor: StackAddress, element_constructor: StackAddress, $( $arg_name : impl_builtins!(@map_ref_type $arg_type) ),* ) $( -> $ret_type )? {
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

pub(crate) use impl_builtins;
