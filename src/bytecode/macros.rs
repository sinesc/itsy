
macro_rules! opcodes {

    // wrappers for readers and writers
    (read u8  $from:ident) => ( $from.read_u8().unwrap() );
    (read u16 $from:ident) => ( $from.read_u16::<LittleEndian>().unwrap() );
    (read u32 $from:ident) => ( $from.read_u32::<LittleEndian>().unwrap() );
    (read u64 $from:ident) => ( $from.read_u64::<LittleEndian>().unwrap() );
    (read i8  $from:ident) => ( $from.read_i8().unwrap() );
    (read i16 $from:ident) => ( $from.read_i16::<LittleEndian>().unwrap() );
    (read i32 $from:ident) => ( $from.read_i32::<LittleEndian>().unwrap() );
    (read i64 $from:ident) => ( $from.read_i64::<LittleEndian>().unwrap() );
    (read f32 $from:ident) => ( $from.read_f32::<LittleEndian>().unwrap() );
    (read f64 $from:ident) => ( $from.read_f64::<LittleEndian>().unwrap() );
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
        enum bytecode {
            $(
                $name $(= $id)*
            ),+
        }

        // implement opcode argument reader and opcode writer
        $(
            #[allow(unused_imports, unused_variables)]
            mod $name {
                #[cfg_attr(not(debug_assertions), inline(always))]
                pub(in super) fn read_args(reader: &mut ::bytecode::VM) -> ( (), (), $($op_type),* ) {
                    use byteorder::{LittleEndian, ReadBytesExt};
                    (
                        // leading () to avoid cases where we have 0 or 1 arguments which breaks the let (...) = read().
                        (), (), $( opcodes!(read $op_type reader) ),*
                    )
                }
            }
        )+

        /// Bytecode-write methods.
        impl ::bytecode::Writer {
            $(
                $( #[ $attr ] )*
                #[allow(unused_imports)]
                pub fn $name(self: &mut Self, $($op_name: $op_type),* ) -> u32 {
                    use byteorder::{LittleEndian, WriteBytesExt};
                    let writer = &mut self.code;
                    let insert_pos = writer.len();
                    writer.write_u8(unsafe { ::std::mem::transmute(bytecode::$name) }).unwrap();
                    $( opcodes!(write $op_type $op_name writer); )*
                    insert_pos as u32
                }
            )+
        }

        /// Wraps the method for executing the bytecode.
        $(
            $( #[ $attr ] )*
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn $name ( $vm: &mut ::bytecode::VM ) {
                let ( _, _, $($op_name),* ) = $name::read_args($vm);
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
                    match unsafe { ::std::mem::transmute(instruction) } {
                        $(
                            bytecode::$name => {
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
            #[allow(unreachable_patterns)]
            pub fn exec(self: &mut Self) {
                use byteorder::ReadBytesExt;
                let instruction = self.read_u8().unwrap();
                match unsafe { ::std::mem::transmute(instruction) } {
                    $(
                        bytecode::$name => $name(self)
                    ),+,
                    _ => panic!("Encountered undefined instruction {:?}.", instruction)
                }
            }
        }
    }
}

