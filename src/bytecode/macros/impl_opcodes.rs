/// Macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_opcodes {

    // Read from slice
    (@inc_pc String, $pc:expr) => (
        $pc += std::mem::size_of::<StackAddress>()
    );
    (@inc_pc RustFn, $pc:expr) => (
        $pc += std::mem::size_of::<RustFnIndex>()
    );
    (@inc_pc $ty:tt, $pc:expr) => (
        $pc += std::mem::size_of::<$ty>()
    );
    (@read_bytes2 $ty:tt, $from:ident) => ( {
        let (int_bytes, rest) = $from.split_at(std::mem::size_of::<$ty>());
        *$from = rest;
        $ty::from_le_bytes(int_bytes.try_into().unwrap())
    });
    // Read opcode arguments
    (@read_arg2 String, $from:ident, $counter:ident) => ( {
        let len = impl_opcodes!(@read_bytes2 StackAddress, $from);
        let (str_bytes, rest) = $from.split_at(len);
        *$from = rest;
        $counter += len;
        ::std::str::from_utf8(str_bytes).unwrap().to_string()
    });
    (@read_arg2 RustFn, $from:ident $(, $counter:ident )?) => ( T::from_index(impl_opcodes!(@read_bytes2 RustFnIndex, $from)) );
    (@read_arg2 Builtin, $from:ident $(, $counter:ident )?) => ( Builtin::from_index(impl_opcodes!(@read_bytes2 BuiltinIndex, $from)) );
    (@read_arg2 HeapRefOp, $from:ident $(, $counter:ident )?) => ( HeapRefOp::from_u8(impl_opcodes!(@read_bytes2 u8, $from)) );
    (@read_arg2 $ty:ident, $from:ident $(, $counter:ident )?) => ( impl_opcodes!(@read_bytes2 $ty, $from) );
    // Map parameter type names to reader types
    (@map_reader_type RustFn) => ( T );
    (@map_reader_type $ty:tt) => ( $ty );
    // Write opcode arguments
    (@write_arg RustFn, $value:expr, $to:ident) => ( $to.write(&$value.to_index().to_le_bytes()[..]) );
    (@write_arg Builtin, $value:expr, $to:ident) => ( $to.write(&$value.to_index().to_le_bytes()[..]) );
    (@write_arg HeapRefOp, $value:expr, $to:ident) => ( $to.write(&[ $value as u8 ]) );
    (@write_arg String, $value:expr, $to:ident) => (
        $to.write(&($value.len() as StackAddress).to_le_bytes()[..]);
        $to.write(&$value.as_bytes()[..]);
    );
    (@write_arg $ty:tt, $value:expr, $to:ident) => ( $to.write(&$value.to_le_bytes()[..]) );
    // Map parameter type names to writer types
    (@map_writer_type RustFn) => ( T );
    (@map_writer_type String) => ( &str );
    (@map_writer_type $ty:tt) => ( $ty );
    // Handle opcode flag setup (before running the opcode)
    (@setup_flag $self:ident, $prev_pc:ident, check) => {
        #[cfg(feature="debugging")]
        let $prev_pc = $self.pc;
    };
    (@setup_flag $self:ident, $prev_pc:ident, $any:ident) => { };
    // Handle opcode flag (perform action after opcode)
    (@flag $self:ident, $prev_pc:ident, return) => { return };
    (@flag $self:ident, $prev_pc:ident, check) => { match $self.state {
        VMState::Error(_) => {
            #[cfg(feature="debugging")]
            {
                $self.error_pc = $prev_pc - size_of::<OpCodeIndex>(); // opcode was already read
            }
            return;
        },
        _ => { },
    } };
    // Main definition block
    (
        $( [
            $( #[ $enum_attr:meta ] )*
        ] )?
        $(
            $( #[ $attr:meta ] )*
            fn
            $( // Either multiple function variants...
                < $(
                    $( #[ $vattr:meta ] )*
                    $vname:ident $( < $( $vgeneric_name:ident : $vgeneric_type:ident ),+ > )? ( $( $varg_name:ident : $vtype_name:ident $( as $vtype_name_as:ident )? ),* )
                    $( [ $vflag:ident ] )?
                ),+ $(,)? >
                ( & mut $vself:ident )
            )?
            $( // or single function.
                $name:ident
                ( & mut $self:ident $(, & mut $context:ident)? $(, $arg_name:ident : $arg_type:ident )* )
                $( [ $flag:ident ] )?
            )?
            $code:block
        )+
    ) => {

        type OpCodeIndex = u16;

        #[allow(non_camel_case_types)]
        #[repr(u16)]
        #[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
        $( $( #[ $enum_attr ] )* )?
        pub enum OpCode {
            $(
                $( #[ $attr ] )*
                // single opcode
                $(
                    $name,
                )?
                // opcode variants
                $(
                    $(
                        $( #[$vattr] )*
                        $vname,
                    )+
                )?
            )+
        }

        /// Integer representations of opcodes. These are useful to avoid having to convert the opcode integer
        /// value from the binary program back to an enum value (which would either require a match block since
        /// not all valid integers are also valid opcodes or an unsafe cast with a range check).
        /// Also note: Instead of casting from int to opcode one could instead cast the to be matched opcode to int
        /// during the main instruction match using match guard, e.g. `x if x == OpCode::$name as OpCodeIndex => ...`.
        /// Unfortunately this doesn't seem to get optimized and is currently significantly (~2x) slower than matching this
        /// integer representation directly.
        #[allow(non_upper_case_globals)]
        #[cfg(feature="runtime")]
        mod opcodes {
            use super::OpCodeIndex;
            $(
                $( #[ $attr ] )*
                // single opcode
                $(
                    pub(super) const $name: OpCodeIndex = super::OpCode::$name as OpCodeIndex;
                )?
                // opcode variants
                $(
                    $(
                        $( #[$vattr] )*
                        pub(super) const $vname: OpCodeIndex = super::OpCode::$vname as OpCodeIndex;
                    )+
                )?
            )+
        }

        /// Bytecode writers. Generated from bytecode method signatures defined via the `impl_opcodes!` macro.
        #[cfg(feature="compiler")]
        impl<T> crate::bytecode::Writer<T> where T: crate::bytecode::VMFunc<T> {
            $(
                $( #[ $attr ] )*
                // single opcode
                $(
                    pub fn $name(self: &Self, $($arg_name: impl_opcodes!(@map_writer_type $arg_type)),* ) -> StackAddress {
                        let insert_pos = self.position();
                        impl_opcodes!(@write_arg OpCodeIndex, OpCode::$name as OpCodeIndex, self);
                        $( impl_opcodes!(@write_arg $arg_type, $arg_name, self); )*
                        self.write_pad(insert_pos);
                        insert_pos as StackAddress
                    }
                )?
                // opcode variants
                $(
                    $(
                        #[allow(unused_imports)]
                        $( #[ $vattr ] )*
                        pub fn $vname(self: &Self, $( $varg_name: impl_opcodes!(@map_writer_type $vtype_name) ),* ) -> StackAddress {
                            let insert_pos = self.position();
                            impl_opcodes!(@write_arg OpCodeIndex, OpCode::$vname as OpCodeIndex, self);
                            $( impl_opcodes!(@write_arg $vtype_name, $varg_name, self); )*
                            self.write_pad(insert_pos);
                            insert_pos as StackAddress
                        }
                    )+
                )?
            )+
        }

        /// Bytecode execution/output.
        #[cfg(feature="runtime")]
        impl<T, U> crate::bytecode::runtime::vm::VM<T, U> where T: crate::bytecode::VMFunc<T> + crate::bytecode::VMData<T, U> {
            /// Executes bytecode from the VMs code buffer until an instruction triggers a yield/terminate/error.
            pub(crate) fn exec(self: &mut Self, context: &mut U) {
                use crate::{RustFnIndex, BuiltinIndex, INSTRUCTION_ALIGNMENT};
                const ADD_MASK: usize = INSTRUCTION_ALIGNMENT - 1;
                loop {
                    let instructions = &mut &self.instructions[self.pc..];
                    let instruction = impl_opcodes!(@read_arg2 OpCodeIndex, instructions);
                    #[allow(unused_doc_comments)]
                    match instruction {
                        $(
                            $( #[ $attr ] )*
                            // single opcode
                            $(
                                opcodes::$name => {
                                    $( impl_opcodes!(@setup_flag self, prev_pc, $flag); )?

                                    let mut size = 0;
                                    let ( (), $( $arg_name ),* ) = ( (), $( impl_opcodes!(@read_arg2 $arg_type, instructions, size) ),* );

                                    impl_opcodes!(@inc_pc OpCodeIndex, size);
                                    $( impl_opcodes!(@inc_pc $arg_type, size); )*
                                    size = (size + ADD_MASK) & !ADD_MASK;
                                    self.pc += size;

                                    $(let $context: &mut U = context;)?
                                    self.$name( $($context,)? $( $arg_name ),* );

                                    $( impl_opcodes!(@flag self, prev_pc, $flag); )?
                                }
                            )?
                            // opcode variants
                            $(
                                $(
                                    $( #[$vattr] )*
                                    opcodes::$vname => {
                                        $( impl_opcodes!(@setup_flag self, prev_pc, $vflag); )?

                                        let mut size = 0;
                                        $( let $varg_name: $vtype_name = impl_opcodes!(@read_arg2 $vtype_name, instructions, size); )*

                                        impl_opcodes!(@inc_pc OpCodeIndex, size);
                                        $( impl_opcodes!(@inc_pc $vtype_name, size); )*
                                        size = (size + ADD_MASK) & !ADD_MASK;
                                        self.pc += size;

                                        self.$vname( $( $varg_name ),* );
                                        $( impl_opcodes!(@flag self, prev_pc, $vflag); )?
                                    }
                                )+
                            )?
                        ),+
                        op @ _ => panic!("Invalid opcode {op:?}."),
                    }
                }
            }
        }

        /// Bytecode debugging.
        #[cfg(all(feature="runtime", feature="debugging"))]
        impl<T, U> crate::bytecode::runtime::vm::VM<T, U> where T: crate::bytecode::VMFunc<T> + crate::bytecode::VMData<T, U> {
            /// Execute the next bytecode from the VMs code buffer.
            pub(crate) fn exec_step(self: &mut Self, context: &mut U) {
                use crate::{RustFnIndex, BuiltinIndex, INSTRUCTION_ALIGNMENT};
                const ADD_MASK: usize = INSTRUCTION_ALIGNMENT - 1;
                let instructions = &mut &self.instructions[self.pc..];
                let instruction = impl_opcodes!(@read_arg2 OpCodeIndex, instructions);
                #[allow(unused_doc_comments)]
                match instruction {
                    $(
                        $( #[ $attr ] )*
                        // single opcode
                        $(
                            opcodes::$name => {
                                $( impl_opcodes!(@setup_flag self, prev_pc, $flag); )?

                                let mut size = 0;
                                let ( (), $( $arg_name ),* ) = ( (), $( impl_opcodes!(@read_arg2 $arg_type, instructions, size) ),* );

                                impl_opcodes!(@inc_pc OpCodeIndex, size);
                                $( impl_opcodes!(@inc_pc $arg_type, size); )*
                                size = (size + ADD_MASK) & !ADD_MASK;
                                self.pc += size;

                                $(let $context: &mut U = context;)?
                                self.$name( $($context,)? $( $arg_name ),* );
                                $( impl_opcodes!(@flag self, prev_pc, $flag); )?
                            }
                        )?
                        // opcode variants
                        $(
                            $(
                                $( #[$vattr] )*
                                opcodes::$vname => {
                                    $( impl_opcodes!(@setup_flag self, prev_pc, $vflag); )?

                                    let mut size = 0;
                                    $( let $varg_name: $vtype_name = impl_opcodes!(@read_arg2 $vtype_name, instructions, size); )*

                                    impl_opcodes!(@inc_pc OpCodeIndex, size);
                                    $( impl_opcodes!(@inc_pc $vtype_name, size); )*
                                    size = (size + ADD_MASK) & !ADD_MASK;
                                    self.pc += size;

                                    self.$vname( $( $varg_name ),* );
                                    $( impl_opcodes!(@flag self, prev_pc, $vflag); )?
                                }
                            )+
                        )?
                    ),+
                    op @ _ => panic!("Invalid opcode {op:?}."),
                }
            }

            #[allow(unused_assignments)]
            #[allow(unused_doc_comments)]
            pub(crate) fn read_instruction(self: &Self, position: StackAddress) -> Option<OpCode> {
                if position >= self.instructions.len() as StackAddress {
                    None
                } else {
                    let instructions = &mut &self.instructions[position..];
                    let instruction = impl_opcodes!(@read_arg2 OpCodeIndex, instructions);
                    match instruction {
                        $(
                            $( #[ $attr ] )*
                            // single opcode
                            $(
                                opcodes::$name => Some(OpCode::$name),
                            )?
                            // opcode variants
                            $(
                                $(
                                    $( #[$vattr] )*
                                    opcodes::$vname => Some(OpCode::$vname),
                                )+
                            )?
                        )+
                        _ => None,
                    }
                }
            }

            /// Returns disassembled opcode as string at given position along with the next opcode position.
            #[allow(unused_mut)]
            #[cfg(feature="symbols")]
            pub(crate) fn describe_instruction(self: &Self, position: StackAddress) -> Option<(String, StackAddress)> {
                use crate::{RustFnIndex, BuiltinIndex, INSTRUCTION_ALIGNMENT};
                const ADD_MASK: usize = INSTRUCTION_ALIGNMENT - 1;
                if position >= self.instructions.len() as StackAddress {
                    None
                } else {
                    let instructions = &mut &self.instructions[position..];
                    let instruction = impl_opcodes!(@read_arg2 OpCodeIndex, instructions);
                    #[allow(unreachable_patterns)]
                    #[allow(unused_doc_comments)]
                    match instruction {
                        // implement special formatting for some opcodes
                        opcodes::call_rust => {
                            let mut size = 0;
                            let mut result = format!("{} {} ", position, stringify!(call_rust));
                            result.push_str(&format!("{:?} ", impl_opcodes!(@read_arg2 RustFn, instructions, size)));
                            impl_opcodes!(@inc_pc OpCodeIndex, size);
                            impl_opcodes!(@inc_pc RustFn, size);
                            size = (size + ADD_MASK) & !ADD_MASK;
                            Some((result, position + size))
                        }
                        #[cfg(feature="comments")]
                        opcodes::comment => {
                            let mut size = 0;
                            let start = position;
                            let message = impl_opcodes!(@read_arg2 String, instructions, size);
                            let result = if &message[0..1] == "\n" { format!("\n;{} {}", start, &message[1..]) } else { format!(";{} {}", start, message) };
                            impl_opcodes!(@inc_pc OpCodeIndex, size);
                            impl_opcodes!(@inc_pc String, size);
                            size = (size + ADD_MASK) & !ADD_MASK;
                            Some((result, position + size))
                        }
                        $(
                            $( #[ $attr ] )*
                            // single opcode
                            $(
                                opcodes::$name => {
                                    let mut size = 0;
                                    let mut result = format!("{:?} {} ", position, stringify!($name));
                                    $( result.push_str(&format!("{:?} ", impl_opcodes!(@read_arg2 $arg_type, instructions, size) )); )*

                                    impl_opcodes!(@inc_pc OpCodeIndex, size);
                                    $( impl_opcodes!(@inc_pc $arg_type, size); )*
                                    size = (size + ADD_MASK) & !ADD_MASK;

                                    Some((result, position + size))
                                }
                            )?
                            // opcode variants
                            $(
                                $(
                                    $( #[$vattr] )*
                                    opcodes::$vname => {
                                        let mut size = 0;
                                        let mut result = format!("{:?} {} ", position, stringify!($vname));
                                        $( result.push_str(&format!("{:?} ", impl_opcodes!(@read_arg2 $vtype_name, instructions, size) )); )*

                                        impl_opcodes!(@inc_pc OpCodeIndex, size);
                                        $( impl_opcodes!(@inc_pc $vtype_name, size); )*
                                        size = (size + ADD_MASK) & !ADD_MASK;

                                        Some((result, position + size))
                                    }
                                )+
                            )?
                        ),+,
                        op @ _ => panic!("Invalid opcode {op:?}."),
                    }
                }
            }
        }

        /// Bytecode instructions. Implemented on VM by the `impl_opcodes!` macro.
        #[cfg(feature="runtime")]
        impl<T, U> crate::bytecode::runtime::vm::VM<T, U> where T: crate::bytecode::VMFunc<T> + crate::bytecode::VMData<T, U> {

            // Generate methods for executing each bytecode on VM struct.
            $(
                $( #[ $attr ] )*
                // single opcode
                $(
                    #[cfg_attr(not(debug_assertions), inline(always))]
                    fn $name ( $self: &mut Self, $($context: &mut U,)? $($arg_name: impl_opcodes!(@map_reader_type $arg_type)),* ) {
                        #[allow(dead_code)]
                        /// Single variant opcodes don't provide T. This definition of T is intended to shadow the VM's generic T in order to trigger an error on accidental use. This is not the T you are looking for.
                        trait T { }
                        #[allow(dead_code)]
                        /// Single variant opcodes don't provide U. This definition of U is intended to shadow the VM's generic U in order to trigger an error on accidental use. This is not the U you are looking for.
                        trait U { }
                        $code
                    }
                )?
                // opcode variants
                $(
                    $(
                        $( #[$vattr] )*
                        #[cfg_attr(not(debug_assertions), inline(always))]
                        fn $vname ( $vself: &mut Self, $( $varg_name: impl_opcodes!(@map_reader_type $vtype_name) ),* ) {
                            $(
                                $( type $vgeneric_name = $vgeneric_type; )+
                            )?
                            $(
                                $(
                                    let $varg_name = $varg_name as $vtype_name_as;
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

pub(crate) use impl_opcodes;