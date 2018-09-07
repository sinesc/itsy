
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

    ( $( $( #[ $attr:meta ] )* fn $name:tt = $id:tt ( $vm:ident : & mut VM $(, $op_name:ident : $op_type:tt)* ) $code:block )+ ) => {

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
        pub mod write {
            $(
                $( #[ $attr ] )*
                #[allow(unused_imports)]
                pub fn $name(writer: &mut Vec<u8>, $($op_name: $op_type),* ) {
                    use byteorder::{LittleEndian, WriteBytesExt};
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

        /// Execute the next bytecode from the VMs code buffer.
        pub fn exec(vm: &mut ::bytecode::VM) {
            use byteorder::ReadBytesExt;
            let instruction = vm.code.read_u8().unwrap();
            match instruction {
                $(
                    $id => $name(vm)
                ),+,
                e => panic!("Encountered undefined instruction {:?}.", e)
            }
        }
    }
}

