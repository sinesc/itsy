
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
        $( $( #[ $attr:meta ] )*
        fn $name:tt = $id:tt ( $vm:ident : & mut Self $(, $op_name:ident : $op_type:tt)* ) $code:block )+
    ) => {

        // implement opcode argument reader and opcode writer
        $(
            #[allow(non_upper_case_globals, non_snake_case, unused_imports)]
            mod $name {
                #[inline(always)]
                pub(in super) fn read_args(reader: &mut ::std::io::Cursor<Vec<u8>>) -> ( (), (), $($op_type),* ) {
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
                pub fn $name(self: &mut Self, $($op_name: $op_type),* ) {
                    use byteorder::{LittleEndian, WriteBytesExt};
                    let writer = &mut self.code;
                    writer.write_u8($id).unwrap();
                    $( opcodes!(write $op_type $op_name writer) );*
                }
            )+
        }

        /// Wraps the method for executing the bytecode.
        $(
            $( #[ $attr ] )*
            #[inline(always)]
            fn $name ( $vm: &mut ::bytecode::VM ) {
                let ( _, _, $($op_name),* ) = $name::read_args(&mut $vm.code);
                $code
            }
        )+

        impl ::bytecode::VM {

            /// Formats the given VMs bytecode data as human readable output.
            fn fmt_instruction(code: &mut ::std::io::Cursor<&Vec<u8>>, f: &mut ::std::fmt::Formatter) -> Option<::std::fmt::Result> { // todo: no
                use byteorder::{LittleEndian, ReadBytesExt};
                if let Ok(instruction) = code.read_u8() {
                    match instruction {
                        $(
                            $id => {
                                // todo: figure out sane error conversion
                                let mut error_wrapper = || {
                                    write!(f, "{:?} ", code.position())?;
                                    write!(f, stringify!($name))?;
                                    write!(f, " ")?;
                                    $( write!(f, "{:?} ", opcodes!(read $op_type code) )?; )*
                                    write!(f, "\n")
                                };
                                Some(error_wrapper())
                            }
                        ),+,
                        e => panic!("Encountered undefined instruction {:?}.", e)
                    }
                } else {
                    None
                }
            }

            /// Execute the next bytecode from the VMs code buffer.
            pub fn exec(self: &mut Self) {
                use byteorder::ReadBytesExt;
                let instruction = self.code.read_u8().unwrap();
                match instruction {
                    $(
                        $id => $name(self)
                    ),+,
                    e => panic!("Encountered undefined instruction {:?}.", e)
                }
            }
        }
    }
}

