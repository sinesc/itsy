
/// The `impl_vm` macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_vm {

    // convert slices to arrays for from_le_bytes
    (to_array 1 $slice:ident) => ( array1($slice) );
    (to_array 2 $slice:ident) => ( array2($slice) );
    (to_array 4 $slice:ident) => ( array4($slice) );
    (to_array 8 $slice:ident) => ( array8($slice) );

    // perform read from slice
    (do_read $ty:tt $size:tt $from:ident) => ( {
        let slice = &$from.program.instructions[$from.pc as usize..];
        let dest: $ty = $ty::from_le_bytes( impl_vm!(to_array $size slice) );
        $from.pc += $size;
        dest
    });

    // wrappers for readers and writers
    (read RustFn, $from:ident) => ( impl_vm!(do_read u16 2 $from) );
    (read u8, $from:ident) => ( impl_vm!(do_read u8 1 $from) );
    (read u16, $from:ident) => ( impl_vm!(do_read u16 2 $from) );
    (read u32, $from:ident) => ( impl_vm!(do_read u32 4 $from) );
    (read u64, $from:ident) => ( impl_vm!(do_read u64 8 $from) );
    (read i8,  $from:ident) => ( impl_vm!(do_read i8 1 $from) );
    (read i16, $from:ident) => ( impl_vm!(do_read i16 2 $from) );
    (read i32, $from:ident) => ( impl_vm!(do_read i32 4 $from) );
    (read i64, $from:ident) => ( impl_vm!(do_read i64 8 $from) );
    (read f32, $from:ident) => ( impl_vm!(do_read f32 4 $from) );
    (read f64, $from:ident) => ( impl_vm!(do_read f64 8 $from) );

    (write RustFn, $value:expr, $to:ident) => ( $to.write(&$value.to_u16().to_le_bytes()[..]) );
    (write $ty:tt, $value:expr, $to:ident) => ( $to.write(&$value.to_le_bytes()[..]) );

    (map_writer_type RustFn) => ( T );
    (map_writer_type $ty:tt) => ( $ty );
    (map_reader_type RustFn) => ( u16 );
    (map_reader_type $ty:tt) => ( $ty );

    // main definition block
    (
        $(
            $( #[ $attr:meta ] )*
            fn $name:tt $( = $id:tt)* ( $self:ident : & mut Self $(, $op_name:ident : $op_type:tt )* ) $code:block
        )+
    ) => {

        /// Bytecode instructions. Generated from bytecode method signatures defined via the `impl_vm!` macro.
        #[allow(non_camel_case_types)]
        #[repr(u8)]
        pub(crate) enum ByteCode {
            rustcall,
            $(
                $( #[ $attr ] )*
                $name $(= $id)*
            ),+
        }

        impl ByteCode {
            /// Converts bytecode to u8.
            #[cfg_attr(not(debug_assertions), inline(always))]
            pub(crate) fn into_u8(self: Self) -> u8 {
                unsafe { ::std::mem::transmute(self) }
            }
            /// Converts u8 to bytecode.
            #[cfg_attr(not(debug_assertions), inline(always))]
            pub(crate) fn from_u8(bytecode: u8) -> Self {
                unsafe { ::std::mem::transmute(bytecode) }
            }
        }

        /// Bytecode writers. Generated from bytecode method signatures defined via the `impl_vm!` macro.
        impl<T> crate::bytecode::Writer<T> where T: crate::runtime::VMFunc<T> {
            /// Calls the given Rust function.
            pub fn rustcall(self: &Self, func: impl_vm!(map_writer_type RustFn)) -> u32 {
                let insert_pos = self.position();
                impl_vm!(write u8, ByteCode::rustcall.into_u8(), self);
                impl_vm!(write RustFn, func, self);
                insert_pos as u32
            }
            $(
                $( #[ $attr ] )*
                #[allow(unused_imports)]
                pub fn $name(self: &Self, $($op_name: impl_vm!(map_writer_type $op_type)),* ) -> u32 {
                    let insert_pos = self.position();
                    impl_vm!(write u8, ByteCode::$name.into_u8(), self);
                    $( impl_vm!(write $op_type, $op_name, self); )*
                    insert_pos as u32
                }
            )+
        }

        /// Bytecode instructions. Implemented on VM by the `impl_vm!` macro.
        impl<T, U> crate::runtime::VM<T, U> where T: crate::runtime::VMFunc<T> + crate::runtime::VMData<T, U> {

            // Generate methods for executing each bytecode on VM struct.
            $(
                $( #[ $attr ] )*
                #[cfg_attr(not(debug_assertions), inline(always))]
                fn $name ( $self: &mut Self, $($op_name: impl_vm!(map_reader_type $op_type)),* ) {
                    $code
                }
            )+

            /// Formats the given VMs bytecode data as human readable output.
            #[allow(unused_imports)]
            #[allow(unused_mut)]
            pub(crate) fn format_instruction(self: &mut Self) -> Option<String> {
                let position = self.pc;
                if position < self.program.instructions.len() as u32 {
                    let instruction = impl_vm!(read u8, self);
                    match ByteCode::from_u8(instruction) {
                        // todo: rustcall specialcase is required since normal opcodes don't receive the context
                        // considering refactoring this so that all opcodes receive the context if it turns out that
                        // additional opcodes require it
                        ByteCode::rustcall => {
                            let mut result = format!("{:?} {} ", position, stringify!(rustcall));
                            result.push_str(&format!("{:?} ", impl_vm!(read RustFn, self) ));
                            Some(result)
                        }
                        $(
                            ByteCode::$name => {
                                let mut result = format!("{:?} {} ", position, stringify!($name));
                                $(
                                    result.push_str(&format!("{:?} ", impl_vm!(read $op_type, self) ));
                                )*
                                Some(result)
                            }
                        ),+,
                    }
                } else {
                    None
                }
            }

            /// Execute the next bytecode from the VMs code buffer.
            #[allow(unused_imports)]
            #[cfg_attr(not(debug_assertions), inline(always))]
            pub(crate) fn exec(self: &mut Self, context: &mut U) {
                let instruction = impl_vm!(read u8, self);
                match ByteCode::from_u8(instruction) {
                    ByteCode::rustcall => {
                        T::from_u16(impl_vm!(read RustFn, self)).exec(self, context);
                    },
                    $(
                        ByteCode::$name => {
                            let ( (), $( $op_name ),* ) = ( (), $( impl_vm!(read $op_type, self) ),* );
                            self.$name( $( $op_name ),* );
                        }
                    ),+
                }
            }
        }
    }
}

