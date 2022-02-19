/// Macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_opcodes {
    // perform read from slice
    (do_read $ty:tt, $from:ident, $counter:expr) => ( {
        let slice = &$from.instructions[$counter as usize..$counter as usize + ::std::mem::size_of::<$ty>()];
        let dest: $ty = $ty::from_le_bytes( slice.try_into().unwrap() );
        $counter += ::std::mem::size_of::<$ty>() as StackAddress;
        dest
    });
    // read parameters
    (read u8, $from:ident, $counter:expr) => ( impl_opcodes!(do_read u8, $from, $counter) );
    (read u16, $from:ident, $counter:expr) => ( impl_opcodes!(do_read u16, $from, $counter) );
    (read u32, $from:ident, $counter:expr) => ( impl_opcodes!(do_read u32, $from, $counter) );
    (read u64, $from:ident, $counter:expr) => ( impl_opcodes!(do_read u64, $from, $counter) );
    (read i8,  $from:ident, $counter:expr) => ( impl_opcodes!(do_read i8, $from, $counter) );
    (read i16, $from:ident, $counter:expr) => ( impl_opcodes!(do_read i16, $from, $counter) );
    (read i32, $from:ident, $counter:expr) => ( impl_opcodes!(do_read i32, $from, $counter) );
    (read i64, $from:ident, $counter:expr) => ( impl_opcodes!(do_read i64, $from, $counter) );
    (read f32, $from:ident, $counter:expr) => ( impl_opcodes!(do_read f32, $from, $counter) );
    (read f64, $from:ident, $counter:expr) => ( impl_opcodes!(do_read f64, $from, $counter) );
    (read String, $from:ident, $counter:expr) => ( {
        let len = impl_opcodes!(do_read StackAddress, $from, $counter);
        let slice = &$from.instructions[$counter as usize..($counter + len) as usize];
        $counter += len;
        ::std::str::from_utf8(slice).unwrap().to_string()
    });
    (read StackAddress, $from:ident, $counter:expr) => ( impl_opcodes!(do_read StackAddress, $from, $counter) );
    (read StackOffset, $from:ident, $counter:expr) => ( impl_opcodes!(do_read StackOffset, $from, $counter) );
    (read ItemIndex, $from:ident, $counter:expr) => ( impl_opcodes!(do_read ItemIndex, $from, $counter) );
    (read RustFn, $from:ident, $counter:expr) => ( T::from_index(impl_opcodes!(do_read RustFnIndex, $from, $counter)) );
    (read Builtin, $from:ident, $counter:expr) => ( Builtin::from_index(impl_opcodes!(do_read BuiltinIndex, $from, $counter)) );
    (read HeapRefOp, $from:ident, $counter:expr) => ( HeapRefOp::from_u8(impl_opcodes!(do_read u8, $from, $counter)) );
    // map parameter types to reader types
    (map_reader_type RustFn) => ( T );
    (map_reader_type $ty:tt) => ( $ty );
    // write parameters
    (write RustFn, $value:expr, $to:ident) => ( $to.write(&$value.into_index().to_le_bytes()[..]) );
    (write Builtin, $value:expr, $to:ident) => ( $to.write(&$value.into_index().to_le_bytes()[..]) );
    (write HeapRefOp, $value:expr, $to:ident) => ( $to.write(&[ $value as u8 ]) );
    (write String, $value:expr, $to:ident) => (
        $to.write(&($value.len() as StackAddress).to_le_bytes()[..]);
        $to.write(&$value.as_bytes()[..]);
    );
    (write $ty:tt, $value:expr, $to:ident) => ( $to.write(&$value.to_le_bytes()[..]) );
    // map parameter types to writer types
    (map_writer_type RustFn) => ( T );
    (map_writer_type String) => ( &str );
    (map_writer_type $ty:tt) => ( $ty );
    // main definition block
    (
        $(
            $( #[ $attr:meta ] )*
            fn
            $( /* either multiple function variants */
                < $( $variant_name:ident $( < $( $generic_name:tt : $generic_type:tt ),+ > )? ( $( $variant_arg:ident : $variant_type:tt $( as $variant_type_as:tt )? ),* ) ),+ $(,)? >
                ( & mut $variant_self:ident)
            )?
            $( /* or single function */
                $name:ident
                ( & mut $self:ident $(, & mut $context:ident)? $(, $arg_name:ident : $arg_type:tt )* )
                $( $ret:ident )?
            )?
            $code:block
        )+
    ) => {

        /// Bytecode instructions. Generated from bytecode method signatures defined via the `impl_opcodes!` macro.
        /// # Naming
        ///
        /// `<op><suffix1>[_<suffix2>[_nc]]`
        ///
        /// `suffix1` refers to stack inputs/outputs\
        /// `suffix2` refers to bytecode (compiletime) arguments
        ///
        /// `f`/`s`/`u`/`i`/`x`      float/signed/unsigned/any integer/heap reference (instruction performs refcounting)
        /// `8`/`16`/`32`/`64`/`sa`  size in bits, sa=StackAddress sized
        /// `nc`                     operation is non-consuming, meaning stack arguments will be read but not removed
        ///
        /// # Examples
        /// `addi32`    add two 32 bit integer\
        /// `addf64`    add two 64 bit floats\
        /// `const64_8` load 64 bit from constpool at given 8 bit address
        #[allow(non_camel_case_types)]
        #[repr(u8)]
        #[derive(PartialEq)]
        pub enum OpCode {
            $(
                $( #[ $attr ] )*
                // opcode variants
                $(
                    $(
                        $variant_name,
                    )+
                )?
                // single opcode
                $( $name, )?
            )+
        }

        impl OpCode {
            /// Converts u8 to bytecode.
            #[cfg_attr(not(debug_assertions), inline(always))]
            pub(crate) fn from_u8(opcode: u8) -> Self {
                // this is faster but introduces non-safe code
                /*if (opcode <= Self::comment as u8) {
                    un safe { ::std::mem::transmute(opcode) }
                } else {
                    panic!("Invalid opcode {}", opcode);
                }*/
                // note: also tried generating contants first and then match those (to avoid the x if x match guard). turned out to be slightly slower.
                match opcode {
                    $(
                        // opcode variants
                        $(

                            $(
                                x if x == Self::$variant_name as u8 => Self::$variant_name,
                            )+
                        )?
                        // single opcode
                        $(
                            x if x == Self::$name as u8 => Self::$name,
                        )?
                    )+
                    _ => panic!("Invalid opcode {}", opcode),
                }
            }
        }

        /// Bytecode writers. Generated from bytecode method signatures defined via the `impl_opcodes!` macro.
        #[cfg(feature="compiler")]
        impl<T> crate::bytecode::Writer<T> where T: crate::bytecode::VMFunc<T> {
            $(
                $( #[ $attr ] )*
                // opcode variants
                $(
                    $(
                        #[allow(unused_imports)]
                        pub fn $variant_name(self: &Self, $( $variant_arg: impl_opcodes!(map_writer_type $variant_type) ),* ) -> StackAddress {
                            let insert_pos = self.position();
                            impl_opcodes!(write u8, OpCode::$variant_name as u8, self);
                            $(
                                impl_opcodes!(write $variant_type, $variant_arg, self);
                            )*
                            insert_pos as StackAddress
                        }
                    )+
                )?
                // single opcode
                $(
                    #[allow(unused_imports)]
                    pub fn $name(self: &Self, $($arg_name: impl_opcodes!(map_writer_type $arg_type)),* ) -> StackAddress {
                        let insert_pos = self.position();
                        impl_opcodes!(write u8, OpCode::$name as u8, self);
                        $( impl_opcodes!(write $arg_type, $arg_name, self); )*
                        insert_pos as StackAddress
                    }
                )?
            )+
        }

        /// Bytecode execution/output/debug.
        impl<T, U> crate::bytecode::runtime::vm::VM<T, U> where T: crate::bytecode::VMFunc<T> + crate::bytecode::VMData<T, U> {

            /// Returns disassembled opcode at given position along with the next opcode position.
            #[cfg(feature="debugging")]
            pub(crate) fn read_instruction(self: &Self, position: StackAddress) -> Option<(OpCode, StackAddress)> {
                let mut pc = position;
                if pc < self.instructions.len() as StackAddress {
                    let instruction = impl_opcodes!(read u8, self, pc);
                    Some((OpCode::from_u8(instruction), pc))
                } else {
                    None
                }
            }

            /// Returns disassembled opcode as string at given position along with the next opcode position.
            //#[allow(unused_imports)]
            #[allow(unused_mut)]
            #[cfg(feature="debugging")]
            pub(crate) fn describe_instruction(self: &Self, position: StackAddress) -> Option<(String, StackAddress)> {
                if let Some((opcode, mut pc)) = self.read_instruction(position) {
                    #[allow(unreachable_patterns)]
                    match opcode {
                        // implement special formatting for some opcodes
                        OpCode::rustcall => {
                            let mut result = format!("{:?} {} ", position, stringify!(rustcall));
                            result.push_str(&format!("{:?} ", impl_opcodes!(read RustFn, self, pc)));
                            Some((result, pc))
                        }
                        OpCode::comment => {
                            let message = impl_opcodes!(read String, self, pc);
                            let result = if &message[0..1] == "\n" { format!("\n[{}]", &message[1..]) } else { format!("[{}]", message) };
                            Some((result, pc))
                        }
                        $(
                            // handle opcode
                            $(
                                OpCode::$name => {
                                    let mut result = format!("{:?} {} ", position, stringify!($name));
                                    $(
                                        result.push_str(&format!("{:?} ", impl_opcodes!(read $arg_type, self, pc) ));
                                    )*
                                    Some((result, pc))
                                }
                            )?
                            // handle opcode variants
                            $(
                                $(
                                    OpCode::$variant_name => {
                                        let mut result = format!("{:?} {} ", position, stringify!($variant_name));
                                        $(
                                            result.push_str(&format!("{:?} ", impl_opcodes!(read $variant_type, self, pc) ));
                                        )*
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

            /// Executes bytecode from the VMs code buffer until an instruction triggers a yield/terminate/error.
            #[allow(unused_imports)]
            pub(crate) fn exec(self: &mut Self, context: &mut U) {
                loop {
                    let instruction = impl_opcodes!(read u8, self, self.pc);
                    #[allow(unreachable_patterns)]
                    match OpCode::from_u8(instruction) {
                        $(
                            // handle single opcode
                            $(
                                OpCode::$name => {
                                    let ( (), $( $arg_name ),* ) = ( (), $( impl_opcodes!(read $arg_type, self, self.pc) ),* );
                                    $(let $context: &mut U = context;)?
                                    self.$name( $($context,)? $( $arg_name ),* );
                                    $( $ret; )?
                                }
                            )?
                            // handle opcode variants
                            $(
                                $(
                                    OpCode::$variant_name => {
                                        $(
                                            let $variant_arg: $variant_type = impl_opcodes!(read $variant_type, self, self.pc);
                                        )*
                                        self.$variant_name( $( $variant_arg ),* );
                                    }
                                )+
                            )?
                        ),+
                    }
                }
            }

            /// Execute the next bytecode from the VMs code buffer.
            #[allow(unused_imports)]
            #[cfg_attr(not(debug_assertions), inline(always))]
            #[cfg(feature="debugging")]
            pub(crate) fn exec_step(self: &mut Self, context: &mut U) {
                let instruction = impl_opcodes!(read u8, self, self.pc);
                #[allow(unreachable_patterns)]
                match OpCode::from_u8(instruction) {
                    $(
                        // handle single opcode
                        $(
                            OpCode::$name => {
                                let ( (), $( $arg_name ),* ) = ( (), $( impl_opcodes!(read $arg_type, self, self.pc) ),* );
                                $(let $context: &mut U = context;)?
                                self.$name( $($context,)? $( $arg_name ),* );
                                $( $ret; )?
                            }
                        )?
                        // handle opcode variants
                        $(
                            $(
                                OpCode::$variant_name => {
                                    $(
                                        let $variant_arg: $variant_type = impl_opcodes!(read $variant_type, self, self.pc);
                                    )*
                                    self.$variant_name( $( $variant_arg ),* );
                                }
                            )+
                        )?
                    ),+
                }
            }
        }

        /// Bytecode instructions. Implemented on VM by the `impl_opcodes!` macro.
        impl<T, U> crate::bytecode::runtime::vm::VM<T, U> where T: crate::bytecode::VMFunc<T> + crate::bytecode::VMData<T, U> {

            // Generate methods for executing each bytecode on VM struct.
            $(
                $( #[ $attr ] )*
                $(
                    #[cfg_attr(not(debug_assertions), inline(always))]
                    fn $name ( $self: &mut Self, $($context: &mut U,)? $($arg_name: impl_opcodes!(map_reader_type $arg_type)),* ) {
                        #[allow(dead_code)]
                        /// Single variant opcodes don't provide T. This definition of T is intended to shadow the VM's generic T in order to trigger an error on accidental use. This is not the T you are looking for.
                        trait T { }
                        #[allow(dead_code)]
                        /// Single variant opcodes don't provide U. This definition of U is intended to shadow the VM's generic U in order to trigger an error on accidental use. This is not the U you are looking for.
                        trait U { }
                        $code
                    }
                )?
                $(
                    $(
                        #[cfg_attr(not(debug_assertions), inline(always))]
                        fn $variant_name ( $variant_self: &mut Self, $( $variant_arg: impl_opcodes!(map_reader_type $variant_type) ),* ) {
                            $(
                                $( type $generic_name = $generic_type; )+
                            )?
                            $(
                                $(
                                    let $variant_arg = $variant_arg as $variant_type_as;
                                )?
                            )*
                            $code
                        }
                    )+
                )?
            )+
        }
    }
}

