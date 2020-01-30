
/// The `impl_vm` macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_vm {

    // convert slices to arrays for from_le_bytes
    (to_array 1 $slice:ident) => ( array1($slice) );
    (to_array 2 $slice:ident) => ( array2($slice) );
    (to_array 4 $slice:ident) => ( array4($slice) );
    (to_array 8 $slice:ident) => ( array8($slice) );

    // perform read from slice
    (do_read $ty:tt, $size:tt, $from:ident, $counter:expr) => ( {
        let slice = &$from.instructions[$counter as usize..];
        let dest: $ty = $ty::from_le_bytes( impl_vm!(to_array $size slice) );
        $counter += $size;
        dest
    });

    // wrappers for readers and writers
    (read RustFn, $from:ident, $counter:expr) => ( impl_vm!(do_read u16, 2, $from, $counter) );
    (read u8, $from:ident, $counter:expr) => ( impl_vm!(do_read u8, 1, $from, $counter) );
    (read u16, $from:ident, $counter:expr) => ( impl_vm!(do_read u16, 2, $from, $counter) );
    (read u32, $from:ident, $counter:expr) => ( impl_vm!(do_read u32, 4, $from, $counter) );
    (read u64, $from:ident, $counter:expr) => ( impl_vm!(do_read u64, 8, $from, $counter) );
    (read i8,  $from:ident, $counter:expr) => ( impl_vm!(do_read i8, 1, $from, $counter) );
    (read i16, $from:ident, $counter:expr) => ( impl_vm!(do_read i16, 2, $from, $counter) );
    (read i32, $from:ident, $counter:expr) => ( impl_vm!(do_read i32, 4, $from, $counter) );
    (read i64, $from:ident, $counter:expr) => ( impl_vm!(do_read i64, 8, $from, $counter) );
    (read f32, $from:ident, $counter:expr) => ( impl_vm!(do_read f32, 4, $from, $counter) );
    (read f64, $from:ident, $counter:expr) => ( impl_vm!(do_read f64, 8, $from, $counter) );
    (read String, $from:ident, $counter:expr) => ( {
        let len = impl_vm!(do_read u32, 4, $from, $counter);
        let slice = &$from.instructions[$counter as usize..($counter + len) as usize];
        $counter += len;
        ::std::str::from_utf8(slice).unwrap().to_string()
    });

    (write RustFn, $value:expr, $to:ident) => ( $to.write(&$value.into_u16().to_le_bytes()[..]) );
    (write String, $value:expr, $to:ident) => (
        $to.write(&($value.len() as u32).to_le_bytes()[..]);
        $to.write(&$value.as_bytes()[..]);
    );
    (write $ty:tt, $value:expr, $to:ident) => ( $to.write(&$value.to_le_bytes()[..]) );

    (map_writer_type RustFn) => ( T );
    (map_writer_type String) => ( &str );
    (map_writer_type $ty:tt) => ( $ty );
    (map_reader_type RustFn) => ( u16 );
    (map_reader_type $ty:tt) => ( $ty );

    // main definition block
    (
        $(
            $( #[ $attr:meta ] )*
            fn $name:tt $( < $( $variant_name:ident : $variant_type:tt as $variant_type_as:tt ),+ > )? ( & mut $self:ident $(, & mut $context:ident)? $(, $arg_name:ident : $arg_type:tt )* ) $( $ret:ident )? $code:block
        )+
    ) => {

        /// Bytecode instructions. Generated from bytecode method signatures defined via the `impl_vm!` macro.
        /// # Naming
        ///
        /// `<op><suffix1>_<suffix2>`
        ///
        /// `suffix1` refers to stack inputs/outputs\
        /// `suffix2` refers to bytecode (compiletime) arguments
        ///
        /// `f`/`s`/`u`/`i`/`r`   float/signed/unsigned/any integer/raw, default: `r` for stack input, `u` for arguments\
        /// `8`/`16`/`32`/`64`  size, default: `32` for stack input, `8` for arguments
        ///
        /// # Examples
        /// `addi`        add two 32 bit integer\
        /// `addf64`      add two 64 bit floats\
        /// `const_fetch_16`   load 32 bit from constpool at given 16 bit address
        #[allow(non_camel_case_types)]
        #[repr(u8)]
        #[derive(PartialEq)]
        pub enum OpCode {
            $(
                // add opcode variants first (better readability in docs because we can't document these)
                $(
                    $(
                        $variant_name,
                    )+
                )?
                // add actual opcode
                $( #[ $attr ] )*
                $name,
            )+
        }

        impl OpCode {
            /// Converts bytecode to u8.
            #[cfg_attr(not(debug_assertions), inline(always))]
            pub(crate) fn into_u8(self: Self) -> u8 {
                self as u8
            }
            /// Converts u8 to bytecode.
            #[cfg_attr(not(debug_assertions), inline(always))]
            pub(crate) fn from_u8(bytecode: u8) -> Self {
                unsafe { ::std::mem::transmute(bytecode) }
            }
        }

        /// Bytecode writers. Generated from bytecode method signatures defined via the `impl_vm!` macro.
        impl<T> crate::bytecode::Writer<T> where T: crate::runtime::VMFunc<T> {
            $(
                // add opcode variants first (better readability in docs because we can't document these)
                $(
                    $(
                        #[allow(unused_imports)]
                        pub fn $variant_name(self: &Self, value: impl_vm!(map_writer_type $variant_type)) -> u32 {
                            let insert_pos = self.position();
                            impl_vm!(write u8, OpCode::$variant_name.into_u8(), self);
                            impl_vm!(write $variant_type, value, self);
                            insert_pos as u32
                        }
                    )+
                )?
                // handle opcode
                $( #[ $attr ] )*
                #[allow(unused_imports)]
                pub fn $name(self: &Self, $($arg_name: impl_vm!(map_writer_type $arg_type)),* ) -> u32 {
                    let insert_pos = self.position();
                    impl_vm!(write u8, OpCode::$name.into_u8(), self);
                    $( impl_vm!(write $arg_type, $arg_name, self); )*
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
                fn $name ( $self: &mut Self, $($context: &mut U,)? $($arg_name: impl_vm!(map_reader_type $arg_type)),* ) {
                    $code
                }
            )+

            /// Returns disassembled opcode at given position along with the next opcode position.
            pub(crate) fn read_instruction(self: &Self, position: u32) -> Option<(OpCode, u32)> {
                let mut pc = position;
                if pc < self.instructions.len() as u32 {
                    let instruction = impl_vm!(read u8, self, pc);
                    Some((OpCode::from_u8(instruction), pc))
                } else {
                    None
                }
            }

            /// Returns disassembled opcode as string at given position along with the next opcode position.
            //#[allow(unused_imports)]
            #[allow(unused_mut)]
            pub(crate) fn describe_instruction(self: &Self, position: u32) -> Option<(String, u32)> {
                if let Some((opcode, mut pc)) = self.read_instruction(position) {
                    #[allow(unreachable_patterns)]
                    match opcode {
                        // implement special formatting for some opcodes
                        OpCode::rustcall => {
                            let mut result = format!("{:?} {} ", position, stringify!(rustcall));
                            result.push_str(&format!("{:?} ", T::from_u16( impl_vm!(read RustFn, self, pc) )));
                            Some((result, pc))
                        }
                        OpCode::comment => {
                            let message = impl_vm!(read String, self, pc);
                            let result = if &message[0..1] == "\n" { format!("\n; {}", &message[1..]) } else { format!("; {}", message) };
                            Some((result, pc))
                        }
                        $(
                            // handle opcode
                            OpCode::$name => {
                                let mut result = format!("{:?} {} ", position, stringify!($name));
                                $(
                                    result.push_str(&format!("{:?} ", impl_vm!(read $arg_type, self, pc) ));
                                )*
                                Some((result, pc))
                            }
                            // handle opcode variants
                            $(
                                $(
                                    OpCode::$variant_name => {
                                        let mut result = format!("{:?} {} ", position, stringify!($variant_name));
                                        result.push_str(&format!("{:?} ", impl_vm!(read $variant_type, self, pc) ));
                                        Some((result, pc))
                                    }
                                )+
                            )?
                        ),+,
                    }
                } else {
                    None
                }
            }

            /// Execute the next bytecode from the VMs code buffer.
            #[allow(unused_imports)]
            pub(crate) fn exec(self: &mut Self, context: &mut U) {
                loop {
                    let instruction = impl_vm!(read u8, self, self.pc);
                    #[allow(unreachable_patterns)]
                    match OpCode::from_u8(instruction) {
                        $(
                            OpCode::$name => {
                                let ( (), $( $arg_name ),* ) = ( (), $( impl_vm!(read $arg_type, self, self.pc) ),* );
                                $(let $context: &mut U = context;)?
                                self.$name( $($context,)? $( $arg_name ),* );
                                $( $ret; )?
                            }
                            // handle opcode variants
                            $(
                                $(
                                    OpCode::$variant_name => {
                                        let tmp: $variant_type = impl_vm!(read $variant_type, self, self.pc);
                                        self.$name( tmp as $variant_type_as );
                                    }
                                )+
                            )?
                        ),+
                    }
                }
            }
        }
    }
}

