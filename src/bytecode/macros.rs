//! The `impl_vm` macro to generate bytecode writers and readers from instruction signatures.

macro_rules! impl_vm {

    // slightly faster reader
    (cast $ty:tt $size:tt $from:ident) => ( {
        let mut dest: $ty = 0;
        unsafe { ::std::ptr::copy_nonoverlapping(&$from.program[$from.pc as usize], &mut dest as *mut $ty as *mut u8, $size) };
        $from.pc += $size;
        dest
    });

    // wrappers for readers and writers
    (read u8 $from:ident) => ( impl_vm!(cast u8 1 $from) );
    (read u16 $from:ident) => ( impl_vm!(cast u16 2 $from) );
    (read u32 $from:ident) => ( impl_vm!(cast u32 4 $from) );
    //(read u64 $from:ident) => ( $from.read_u64::<LittleEndian>().unwrap() );
    (read i8  $from:ident) => ( $from.read_i8().unwrap() );
    //(read i16 $from:ident) => ( $from.read_i16::<LittleEndian>().unwrap() );
    (read i32 $from:ident) => ( impl_vm!(cast i32 4 $from) );
    //(read i64 $from:ident) => ( $from.read_i64::<LittleEndian>().unwrap() );
    //(read f32 $from:ident) => ( $from.read_f32::<LittleEndian>().unwrap() );
    //(read f64 $from:ident) => ( $from.read_f64::<LittleEndian>().unwrap() );

    (write u8  $value:ident $to:ident) => ( $to.write_u8($value).unwrap() );
    (write u16 $value:ident $to:ident) => ( $to.write_u16::<LittleEndian>($value).unwrap() );
    (write u32 $value:ident $to:ident) => ( $to.write_u32::<LittleEndian>($value).unwrap() );
    (write u64 $value:ident $to:ident) => ( $to.write_u64::<LittleEndian>($value).unwrap() );
    (write i8  $value:ident $to:ident) => ( $to.write_i8($value).unwrap() );
    (write i16 $value:ident $to:ident) => ( $to.write_i16::<LittleEndian>($value).unwrap() );
    (write i32 $value:ident $to:ident) => ( $to.write_i32::<LittleEndian>($value).unwrap() );
    (write i64 $value:ident $to:ident) => ( $to.write_i64::<LittleEndian>($value).unwrap() );
    (write f32 $value:ident $to:ident) => ( $to.write_f32::<LittleEndian>($value).unwrap() );
    (write f64 $value:ident $to:ident) => ( $to.write_f64::<LittleEndian>($value).unwrap() );

    // main definition block
    (
        $(
            $( #[ $attr:meta ] )*
            fn $name:tt $( = $id:tt)* ( $vm:ident : & mut Self $(, $op_name:ident : $op_type:tt)* ) $code:block
        )+
    ) => {

        /// Bytecode instructions. Generated from bytecode method signatures defined via the `impl_vm!` macro.
        #[allow(non_camel_case_types)]
        #[repr(u8)]
        enum ByteCode {
            $(
                $( #[ $attr ] )*
                $name $(= $id)*
            ),+
        }

        impl ByteCode {
            /// Converts bytecode to u8.
            #[inline(always)]
            fn into_u8(self: Self) -> u8 {
                unsafe { ::std::mem::transmute(self) }
            }
            /// Converts u8 to bytecode.
            #[inline(always)]
            fn from_u8(bytecode: u8) -> ByteCode {
                unsafe { ::std::mem::transmute(bytecode) }
            }
        }

        /// Bytecode writers. Generated from bytecode method signatures defined via the `impl_vm!` macro.
        impl ::bytecode::Writer {
            $(
                $( #[ $attr ] )*
                #[allow(unused_imports)]
                pub fn $name($vm: &mut Self, $($op_name: $op_type),* ) -> u32 {
                    use byteorder::{LittleEndian, WriteBytesExt};
                    let writer = &mut $vm.program;
                    let insert_pos = writer.len();
                    writer.write_u8(ByteCode::$name.into_u8()).unwrap();
                    $( impl_vm!(write $op_type $op_name writer); )*
                    insert_pos as u32
                }
            )+
        }

        /// Bytecode instructions. Implemented on VM by the `impl_vm!` macro.
        impl ::bytecode::VM {

            // Generate methods for executing each bytecode on VM struct.
            $(
                $( #[ $attr ] )*
                #[cfg_attr(not(debug_assertions), inline(always))]
                pub(crate) fn $name ( $vm: &mut Self, $($op_name: $op_type),* ) {
                    $code
                }
            )+

            /// Formats the given VMs bytecode data as human readable output.
            #[allow(unused_imports)]
            #[allow(unreachable_patterns)]
            pub(crate) fn format_instruction(self: &mut Self) -> Option<String> {
                use byteorder::{LittleEndian, ReadBytesExt};
                let position = self.pc;
                if let Ok(instruction) = self.read_u8() {
                    match ByteCode::from_u8(instruction) {
                        $(
                            ByteCode::$name => {
                                let mut result = format!("{:?} {} ", position, stringify!($name));
                                $(
                                    result.push_str(&format!("{:?} ", impl_vm!(read $op_type self) ));
                                )*
                                Some(result)
                            }
                        ),+,
                        _ => panic!("Encountered undefined instruction {:?}.", instruction)
                    }
                } else {
                    None
                }
            }

            /// Execute the next bytecode from the VMs code buffer.
            #[allow(unused_imports)]
            //#[allow(unreachable_patterns)]
            #[cfg_attr(not(debug_assertions), inline(always))]
            pub(crate) fn exec(self: &mut Self) {
                use byteorder::{LittleEndian, ReadBytesExt};
                let instruction = impl_vm!(read u8 self);
                match ByteCode::from_u8(instruction) {
                    $(
                        ByteCode::$name => {
                            let ( (), $( $op_name ),* ) = ( (), $( impl_vm!(read $op_type self) ),* );
                            self.$name( $( $op_name ),* );
                        }
                    ),+,
                    //_ => panic!("Encountered undefined instruction {:?}.", instruction)
                }
            }
        }
    }
}

