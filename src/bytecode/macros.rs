
macro_rules! opcode {
    ($name:tt, $($op_name:ident : $op_type:tt),*) => (
        #[allow(non_upper_case_globals, non_snake_case, unused_imports)]
        pub mod $name {
            use std::io::Cursor;
            use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
            pub fn read(reader: &mut Cursor<Vec<u8>>) -> ( $($op_type),* ) {
                (
                    $( opcode!(read $op_type reader) ),*
                )
            }
            pub fn write(writer: &mut Vec<u8>, $($op_name: $op_type),* ) {
                $( opcode!(write $op_type $op_name writer) );*
            }
        }
    );
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
}