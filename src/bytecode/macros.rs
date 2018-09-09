
macro_rules! opcodes {

    // slightly faster reader
    (cast $ty:tt $size:tt $from:ident) => ( {
        let mut dest: $ty = 0; //= unsafe { ::std::mem::uninitialized() };
        unsafe { ::std::ptr::copy_nonoverlapping(&$from.code[$from.pc as usize], &mut dest as *mut $ty as *mut u8, $size) };
        $from.pc += $size;
        dest
    });

    // wrappers for readers and writers
    (read u8 $from:ident) => ( opcodes!(cast u8 1 $from) );
    (read u16 $from:ident) => ( opcodes!(cast u16 2 $from) );
    (read u32 $from:ident) => ( opcodes!(cast u32 4 $from) );
    //(read u64 $from:ident) => ( $from.read_u64::<LittleEndian>().unwrap() );
    //(read i8  $from:ident) => ( $from.read_i8().unwrap() );
    //(read i16 $from:ident) => ( $from.read_i16::<LittleEndian>().unwrap() );
    (read i32 $from:ident) => ( opcodes!(cast i32 4 $from) );
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

        // internally used to get incrementing bytecode ids for each bytecode
        #[allow(non_camel_case_types)]
        #[repr(u8)]
        enum ByteCode {
            $(
                $name $(= $id)*
            ),+
        }
        #[inline(always)]
        fn bytecode_to_u8(instruction: ByteCode) -> u8 {
            unsafe { ::std::mem::transmute(instruction) }
        }
        #[inline(always)]
        fn u8_to_bytecode(instruction: u8) -> ByteCode {
            unsafe { ::std::mem::transmute(instruction) }
        }

        /// Bytecode-write methods.
        impl ::bytecode::Writer {
            $(
                $( #[ $attr ] )*
                #[allow(unused_imports)]
                pub fn $name(self: &mut Self, $($op_name: $op_type),* ) -> u32 {
                    use byteorder::{LittleEndian, WriteBytesExt};
                    let writer = &mut self.code;
                    let insert_pos = writer.len();
                    writer.write_u8(bytecode_to_u8(ByteCode::$name)).unwrap();
                    $( opcodes!(write $op_type $op_name writer); )*
                    insert_pos as u32
                }
            )+
        }

        /// Wraps the method for executing the bytecode.
        $(
            $( #[ $attr ] )*
            #[allow(unused_imports)]
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn $name ( $vm: &mut ::bytecode::VM ) {
                use byteorder::{LittleEndian, ReadBytesExt};
                // leading () to avoid cases where we have 0 arguments which breaks the let (...) = read().
                let ( _, $($op_name),* ) = ( (), $( opcodes!(read $op_type $vm) ),* );
                $code
            }
        )+

        impl ::bytecode::VM {

            /// Formats the given VMs bytecode data as human readable output.
            #[allow(unused_imports)]
            #[allow(unreachable_patterns)]
            fn format_instruction(self: &mut Self) -> Option<String> {
                use byteorder::{LittleEndian, ReadBytesExt};
                let position = self.pc;
                if let Ok(instruction) = self.read_u8() {
                    match u8_to_bytecode(instruction) {
                        $(
                            ByteCode::$name => {
                                let mut result = format!("{:?} {} ", position, stringify!($name));
                                $(
                                    result.push_str(&format!("{:?} ", opcodes!(read $op_type self) ));
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
            #[allow(unreachable_patterns)]
            #[cfg_attr(not(debug_assertions), inline(always))]
            pub fn exec(self: &mut Self) {
                use byteorder::{LittleEndian, ReadBytesExt};
                let instruction = opcodes!(read u8 self);
                match u8_to_bytecode(instruction) {
                    $(
                        ByteCode::$name => $name(self)
                    ),+,
                    _ => panic!("Encountered undefined instruction {:?}.", instruction)
                }
            }
        }
    }
}

