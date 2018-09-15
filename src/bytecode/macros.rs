/*
macro_rules! count {
    ($head:ident $(, $tail:ident)*) => {  1 + count!($($tail),*) };
    () => { 0 };
}
*/

/// Generates an enum implementing `RustFnMap`, which maps external Rust functions for use from Itsy code.
#[macro_export]
macro_rules! fn_map {
    (
        $enum_name:ident, { $(
            $( #[ $attr:meta ] )*
            fn $name:tt ( $self:ident : & mut VM $(, $op_name:ident : $op_type:tt)* ) $code:block
        )+ }
    ) => {
        /// Rust function mapping. Generated from function signatures defined via the `register_vm!` macro.
        #[allow(non_camel_case_types)]
        #[repr(u16)]
        #[derive(Copy, Clone, Debug)]
        enum $enum_name {
            $(
                $( #[ $attr ] )*
                $name
            ),+
        }

        impl $crate::bytecode::RustFnId for $enum_name {
            fn from_rustfn(self: Self) -> u16 {
                unsafe { ::std::mem::transmute(self) }
            }
        }

        impl $enum_name {
            fn map() -> Option<$crate::frontend::RustFnMap> {
                use $crate::frontend::RustFn;
                let mut map = ::std::collections::HashMap::new();
                $(
                    map.insert(stringify!($name), RustFn::new($enum_name::$name, Vec::new()));
                )+
                Some(map)
            }
        }
    }
}

/// The `impl_vm` macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_vm {

    // slightly faster reader
    (fastread $ty:tt $size:tt $from:ident) => ( {
        let mut dest: $ty = 0;
        unsafe { ::std::ptr::copy_nonoverlapping(&$from.program[$from.pc as usize], &mut dest as *mut $ty as *mut u8, $size) };
        $from.pc += $size;
        dest
    });

    // wrappers for readers and writers
    (read u8, $from:ident) => ( impl_vm!(fastread u8 1 $from) ); // todo: no fixed endianness on these, writer needs to do the same or stuff will break
    (read u16, $from:ident) => ( impl_vm!(fastread u16 2 $from) );
    (read u32, $from:ident) => ( impl_vm!(fastread u32 4 $from) );
    (read u64, $from:ident) => ( $from.read_u64::<LittleEndian>().unwrap() );
    (read i8,  $from:ident) => ( $from.read_i8().unwrap() );
    (read i16, $from:ident) => ( $from.read_i16::<LittleEndian>().unwrap() );
    (read i32, $from:ident) => ( impl_vm!(fastread i32 4 $from) );
    (read i64, $from:ident) => ( $from.read_i64::<LittleEndian>().unwrap() );
    (read f32, $from:ident) => ( $from.read_f32::<LittleEndian>().unwrap() );
    (read f64, $from:ident) => ( $from.read_f64::<LittleEndian>().unwrap() );
    (read RustFn, $from:ident) => ( $from.read_u16::<LittleEndian>().unwrap() );

    (write u8,  $value:expr, $to:ident) => ( $to.write_u8($value).unwrap() );
    (write u16, $value:expr, $to:ident) => ( $to.write_u16::<LittleEndian>($value).unwrap() );
    (write u32, $value:expr, $to:ident) => ( $to.write_u32::<LittleEndian>($value).unwrap() );
    (write u64, $value:expr, $to:ident) => ( $to.write_u64::<LittleEndian>($value).unwrap() );
    (write i8,  $value:expr, $to:ident) => ( $to.write_i8($value).unwrap() );
    (write i16, $value:expr, $to:ident) => ( $to.write_i16::<LittleEndian>($value).unwrap() );
    (write i32, $value:expr, $to:ident) => ( $to.write_i32::<LittleEndian>($value).unwrap() );
    (write i64, $value:expr, $to:ident) => ( $to.write_i64::<LittleEndian>($value).unwrap() );
    (write f32, $value:expr, $to:ident) => ( $to.write_f32::<LittleEndian>($value).unwrap() );
    (write f64, $value:expr, $to:ident) => ( $to.write_f64::<LittleEndian>($value).unwrap() );
    (write RustFn, $value:expr, $to:ident) => ( $to.write_u16::<LittleEndian>($value.from_rustfn()).unwrap() );

    (map_writer_type RustFn) => ( impl ::bytecode::RustFnId );
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
            fn from_u8(bytecode: u8) -> Self {
                unsafe { ::std::mem::transmute(bytecode) }
            }
        }

        /// Bytecode writers. Generated from bytecode method signatures defined via the `impl_vm!` macro.
        impl ::bytecode::Writer {
            $(
                $( #[ $attr ] )*
                #[allow(unused_imports)]
                pub fn $name(self: &mut Self, $($op_name: impl_vm!(map_writer_type $op_type)),* ) -> u32 {
                    use byteorder::{LittleEndian, WriteBytesExt};
                    let insert_pos = self.position;
                    impl_vm!(write u8, ByteCode::$name.into_u8(), self);
                    $( impl_vm!(write $op_type, $op_name, self); )*
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
                pub(crate) fn $name ( $self: &mut Self, $($op_name: impl_vm!(map_reader_type $op_type)),* ) {
                    $code
                }
            )+

            /// Formats the given VMs bytecode data as human readable output.
            #[allow(unused_imports)]
            pub(crate) fn format_instruction(self: &mut Self) -> Option<String> {
                use byteorder::{LittleEndian, ReadBytesExt};
                let position = self.pc;
                if let Ok(instruction) = self.read_u8() {
                    match ByteCode::from_u8(instruction) {
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
            pub(crate) fn exec(self: &mut Self) {
                use byteorder::{LittleEndian, ReadBytesExt};
                let instruction = impl_vm!(read u8, self);
                match ByteCode::from_u8(instruction) {
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

