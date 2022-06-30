/// Macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_opcodes {
    // perform read from slice
    (do_read u8, $from:expr, $counter:expr) => ( {
        let dest = $from.instructions[$counter];
        $counter += 1;
        dest
    });
    (do_read $ty:tt, $from:ident, $counter:expr) => ( {
        let slice = &$from.instructions[$counter as usize..$counter as usize + ::std::mem::size_of::<$ty>()];
        let dest: $ty = $ty::from_le_bytes( slice.try_into().unwrap() );
        $counter += ::std::mem::size_of::<$ty>() as StackAddress;
        dest
    });
    // read parameters
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
    (read $size:ident, $from:ident, $counter:expr) => ( impl_opcodes!(do_read $size, $from, $counter) );
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
            $( #[ $group_attr:meta ] )*
            fn
            $( /* either multiple function variants */
                < $( $( #[ $variant_attr:meta ] )* $variant_name:ident $( < $( $generic_name:tt : $generic_type:tt ),+ > )? ( $( $variant_arg:ident : $variant_type:tt $( as $variant_type_as:tt )? ),* ) ),+ $(,)? >
                ( & mut $variant_self:ident)
            )?
            $( /* or single function */
                $( #[ $attr:meta ] )* $name:ident
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
        /// `f`/`s`/`u`/`i`/`x`      float/signed/unsigned/any integer/heap reference (instruction performs refcounting)\
        /// `8`/`16`/`32`/`64`/`sa`  size in bits, sa=StackAddress sized\
        /// `nc`                     operation is non-consuming, meaning stack arguments will be read but not removed
        ///
        /// # Examples
        /// `addi32`    add two 32 bit integer\
        /// `addf64`    add two 64 bit floats\
        /// `const64_8` load 64 bit from constpool at given 8 bit address
        #[allow(non_camel_case_types)]
        #[repr(u8)]
        #[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
        pub enum OpCode {
            $(
                $( #[ $group_attr ] )*
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

        #[allow(non_upper_case_globals)]
        mod opcodes {
            $(
                // handle single opcode
                $(
                    pub(super) const $name: u8 = super::OpCode::$name as u8;
                )?
                // handle opcode variants
                $(
                    $(
                        pub(super) const $variant_name: u8 = super::OpCode::$variant_name as u8;
                    )+
                )?
            )+
        }

        /// Bytecode writers. Generated from bytecode method signatures defined via the `impl_opcodes!` macro.
        #[cfg(feature="compiler")]
        impl<T> crate::bytecode::Writer<T> where T: crate::bytecode::VMFunc<T> {
            $(
                $( #[ $group_attr ] )*
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

            #[cfg(feature="debugging")]
            #[allow(unused_assignments)]
            pub(crate) fn read_instruction(self: &Self, mut position: StackAddress) -> Option<OpCode> {
                if position >= self.instructions.len() as StackAddress {
                    None
                } else {
                    let instruction = impl_opcodes!(read u8, self, position);
                    match instruction {
                        $(
                            $(
                                opcodes::$name => Some(OpCode::$name),
                            )?
                            $(
                                $(
                                    opcodes::$variant_name => Some(OpCode::$variant_name),
                                )+
                            )?
                        )+
                        _ => None,
                    }
                }
            }

            /// Returns disassembled opcode as string at given position along with the next opcode position.
            //#[allow(unused_imports)]
            #[allow(unused_mut)]
            #[cfg(feature="debugging")]
            pub(crate) fn describe_instruction(self: &Self, mut position: StackAddress) -> Option<(String, StackAddress)> {
                if position >= self.instructions.len() as StackAddress {
                    None
                } else {
                    let instruction = impl_opcodes!(read u8, self, position);
                    #[allow(unreachable_patterns)]
                    match instruction {
                        // implement special formatting for some opcodes
                        opcodes::rustcall => {
                            let mut result = format!("{:?} {} ", position - 1, stringify!(rustcall));
                            result.push_str(&format!("{:?} ", impl_opcodes!(read RustFn, self, position)));
                            Some((result, position))
                        }
                        opcodes::comment => {
                            let message = impl_opcodes!(read String, self, position);
                            let result = if &message[0..1] == "\n" { format!("\n[{}]", &message[1..]) } else { format!("[{}]", message) };
                            Some((result, position))
                        }
                        $(
                            // handle opcode
                            $(
                                opcodes::$name => {
                                    let mut result = format!("{:?} {} ", position - 1, stringify!($name));
                                    $(
                                        result.push_str(&format!("{:?} ", impl_opcodes!(read $arg_type, self, position) ));
                                    )*
                                    Some((result, position))
                                }
                            )?
                            // handle opcode variants
                            $(
                                $(
                                    opcodes::$variant_name => {
                                        let mut result = format!("{:?} {} ", position - 1, stringify!($variant_name));
                                        $(
                                            result.push_str(&format!("{:?} ", impl_opcodes!(read $variant_type, self, position) ));
                                        )*
                                        Some((result, position))
                                    }
                                )+
                            )?
                        ),+,
                        _ => unreachable!("Invalid opcode"),
                    }
                }
            }

            /// Executes bytecode from the VMs code buffer until an instruction triggers a yield/terminate/error.
            pub(crate) fn exec(self: &mut Self, context: &mut U) {
                loop {
                    let instruction = impl_opcodes!(read u8, self, self.pc);
                    #[allow(unreachable_patterns)]
                    match instruction {
                        $(
                            // handle single opcode
                            $(
                                opcodes::$name => {
                                    let ( (), $( $arg_name ),* ) = ( (), $( impl_opcodes!(read $arg_type, self, self.pc) ),* );
                                    $(let $context: &mut U = context;)?
                                    self.$name( $($context,)? $( $arg_name ),* );
                                    $( $ret; )?
                                }
                            )?
                            // handle opcode variants
                            $(
                                $(
                                    opcodes::$variant_name => {
                                        $(
                                            let $variant_arg: $variant_type = impl_opcodes!(read $variant_type, self, self.pc);
                                        )*
                                        self.$variant_name( $( $variant_arg ),* );
                                    }
                                )+
                            )?
                        ),+
                        _ => unreachable!("Invalid opcode"),
                    }
                }
            }

            /// Execute the next bytecode from the VMs code buffer.
            #[cfg(feature="debugging")]
            pub(crate) fn exec_step(self: &mut Self, context: &mut U) {
                let instruction = impl_opcodes!(read u8, self, self.pc);
                #[allow(unreachable_patterns)]
                match instruction {
                    $(
                        // handle single opcode
                        $(
                            opcodes::$name => {
                                let ( (), $( $arg_name ),* ) = ( (), $( impl_opcodes!(read $arg_type, self, self.pc) ),* );
                                $(let $context: &mut U = context;)?
                                self.$name( $($context,)? $( $arg_name ),* );
                                $( $ret; )?
                            }
                        )?
                        // handle opcode variants
                        $(
                            $(
                                opcodes::$variant_name => {
                                    $(
                                        let $variant_arg: $variant_type = impl_opcodes!(read $variant_type, self, self.pc);
                                    )*
                                    self.$variant_name( $( $variant_arg ),* );
                                }
                            )+
                        )?
                    ),+
                    _ => unreachable!("Invalid opcode"),
                }
            }
        }

        /// Bytecode instructions. Implemented on VM by the `impl_opcodes!` macro.
        impl<T, U> crate::bytecode::runtime::vm::VM<T, U> where T: crate::bytecode::VMFunc<T> + crate::bytecode::VMData<T, U> {

            // Generate methods for executing each bytecode on VM struct.
            $(
                $( #[ $group_attr ] )*
                $(
                    $( #[$attr] )*
                    #[cfg_attr(not(debug_assertions), inline)]
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
                        $( #[$variant_attr] )*
                        #[cfg_attr(not(debug_assertions), inline)]
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

