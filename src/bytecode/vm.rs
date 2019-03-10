//! A virtual machine for running Itsy bytecode.

use std::io::{self, Read};
use crate::bytecode::*;

/// Current state of the vm, checked after each instruction.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VMState {
    /// The VM will continue to execute white it is in this state.
    Continue,
    /// Yield after current instruction. The program can be resumed after a yield.
    Yield,
    /// Terminate after current instruction. The program state will be reset.
    Terminate,
    /// A runtime error was encountered.
    RuntimeError,
}

/// A virtual machine for running Itsy bytecode.
#[derive(Debug)]
pub struct VM<T, U> where T: crate::VMFunc<T> {
    context_type        : std::marker::PhantomData<U>,
    pub(crate) program  : Program<T>,
    pub(crate) pc       : u32,
    pub(crate) state    : VMState,
    pub stack           : Stack,
    pub heap            : Heap,
}

/// Public VM methods.
impl<T, U> VM<T, U> where T: crate::VMFunc<T>+crate::VMData<T, U> {
    /// Create a new VM instance with the given Program.
    pub fn new(program: Program<T>) -> Self {
        VM {
            context_type: std::marker::PhantomData,
            program     : program,
            pc          : 0,
            state       : VMState::Continue,
            stack       : Stack::new(),
            heap        : Heap::new(),
        }
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.reset();
        self.heap.reset();
        self.pc = 0;
        self.state = VMState::Continue;
    }

    /// Disassembles the bytecode and returns it as a string.
    pub fn dump_program(self: &mut Self) -> String { // todo: should not have to require mut
        let pc = self.pc;
        self.pc = 0;
        let mut result = "".to_string();
        while let Some(instruction) = self.format_instruction() {
            result.push_str(&instruction);
            result.push_str("\n");
        }
        self.pc = pc;
        return result;
    }

    /// Disassembles the current bytecode instruction and returns it as a string.
    pub fn dump_instruction(self: &mut Self) -> Option<String> {// todo: should not have to require mut
        let pc = self.pc;
        let result = self.format_instruction();
        self.pc = pc;
        result
    }

    /// Returns the current stack as a string.
    pub fn dump_stack(self: &Self) -> String {
        format!("{:?}", self.stack)
    }

    /// Returns the current stack-frame as a string.
    pub fn dump_frame(self: &Self) -> String {
        format!("{:?}", &self.stack.frame())
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self, context: &mut U) -> &mut Self {
        while self.state == VMState::Continue {
            self.exec(context);
        }
        if self.state == VMState::Terminate {
            self.reset();
        }
        self
    }

    /// Returns the current VM state.
    pub fn state(self: &Self) -> VMState {
        self.state
    }
}

impl<T, U> Read for VM<T, U> where T: crate::VMFunc<T> {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let n = Read::read(&mut &self.program.instructions[self.pc as usize..], buf)?;
        self.pc += n as u32;
        Ok(n)
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        let n = buf.len();
        Read::read_exact(&mut &self.program.instructions[self.pc as usize..], buf)?;
        self.pc += n as u32;
        Ok(())
    }
}

/// The `impl_vm` macro to generate bytecode writers and readers from instruction signatures.
macro_rules! impl_vm {

    // slightly faster reader // todo: look into 1.32's new .from/to_le_bytes api
    (fastread $ty:tt $size:tt $from:ident) => ( {
        let mut dest: $ty;
        unsafe {
            dest = ::std::mem::uninitialized();
            ::std::ptr::copy_nonoverlapping(&$from.program.instructions[$from.pc as usize], &mut dest as *mut $ty as *mut u8, $size)
        };
        $from.pc += $size;
        dest
    });

    // wrappers for readers and writers
    (read u8, $from:ident) => ( impl_vm!(fastread u8 1 $from) ); // todo: no fixed endianness on these, writer needs to do the same or stuff will break
    (read u16, $from:ident) => ( impl_vm!(fastread u16 2 $from) );
    (read u32, $from:ident) => ( impl_vm!(fastread u32 4 $from) );
    (read u64, $from:ident) => ( impl_vm!(fastread u64 8 $from) );
    (read i8,  $from:ident) => ( impl_vm!(fastread i8 1 $from) );
    (read i16, $from:ident) => ( impl_vm!(fastread i16 2 $from) );
    (read i32, $from:ident) => ( impl_vm!(fastread i32 4 $from) );
    (read i64, $from:ident) => ( impl_vm!(fastread i64 8 $from) );
    (read f32, $from:ident) => ( impl_vm!(fastread f32 4 $from) );
    (read f64, $from:ident) => ( impl_vm!(fastread f64 8 $from) );
    (read RustFn, $from:ident) => ( impl_vm!(fastread u16 2 $from) );

    (write u8,  $value:expr, $to:ident) => ( $to.write_u8($value).unwrap() );   // todo: see above
    (write u16, $value:expr, $to:ident) => ( $to.write_u16::<LittleEndian>($value).unwrap() );
    (write u32, $value:expr, $to:ident) => ( $to.write_u32::<LittleEndian>($value).unwrap() );
    (write u64, $value:expr, $to:ident) => ( $to.write_u64::<LittleEndian>($value).unwrap() );
    (write i8,  $value:expr, $to:ident) => ( $to.write_i8($value).unwrap() );
    (write i16, $value:expr, $to:ident) => ( $to.write_i16::<LittleEndian>($value).unwrap() );
    (write i32, $value:expr, $to:ident) => ( $to.write_i32::<LittleEndian>($value).unwrap() );
    (write i64, $value:expr, $to:ident) => ( $to.write_i64::<LittleEndian>($value).unwrap() );
    (write f32, $value:expr, $to:ident) => ( $to.write_f32::<LittleEndian>($value).unwrap() );
    (write f64, $value:expr, $to:ident) => ( $to.write_f64::<LittleEndian>($value).unwrap() );
    (write RustFn, $value:expr, $to:ident) => ( $to.write_u16::<LittleEndian>($value.to_u16()).unwrap() );

    (map_writer_type RustFn) => ( T );
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
        pub(crate) enum ByteCode {
            rustcall,
            $(
                $( #[ $attr ] )*
                $name $(= $id)*
            ),+
        }

        impl ByteCode {
            /// Converts bytecode to u8.
            #[inline(always)]
            pub(crate) fn into_u8(self: Self) -> u8 {
                unsafe { ::std::mem::transmute(self) }
            }
            /// Converts u8 to bytecode.
            #[inline(always)]
            pub(crate) fn from_u8(bytecode: u8) -> Self {
                unsafe { ::std::mem::transmute(bytecode) }
            }
        }

        /// Bytecode writers. Generated from bytecode method signatures defined via the `impl_vm!` macro.
        impl<T> crate::bytecode::Writer<T> where T: crate::VMFunc<T> {
            /// Calls the given Rust function.
            pub fn rustcall(self: &mut Self, func: impl_vm!(map_writer_type RustFn)) -> u32 {
                use byteorder::{LittleEndian, WriteBytesExt};
                let insert_pos = self.position;
                impl_vm!(write u8, ByteCode::rustcall.into_u8(), self);
                impl_vm!(write RustFn, func, self);
                insert_pos as u32
            }
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
        impl<T, U> crate::bytecode::VM<T, U> where T: crate::VMFunc<T>+crate::VMData<T, U> {

            // Generate methods for executing each bytecode on VM struct.
            $(
                $( #[ $attr ] )*
                #[cfg_attr(not(debug_assertions), inline(always))]
                pub fn $name ( $self: &mut Self, $($op_name: impl_vm!(map_reader_type $op_type)),* ) {
                    $code
                }
            )+

            /// Formats the given VMs bytecode data as human readable output.
            #[allow(unused_imports)]
            #[allow(unused_mut)]
            pub(crate) fn format_instruction(self: &mut Self) -> Option<String> {
                use byteorder::{LittleEndian, ReadBytesExt};
                let position = self.pc;
                if let Ok(instruction) = self.read_u8() {
                    match ByteCode::from_u8(instruction) {
                        // todo: rustcall specialcase is required since normal opcodes don't receive the context
                        // considering refactoring this so that all opcodes receive the context if it turns out that
                        // additional opcodes require it
                        ByteCode::rustcall => {
                            let mut result = format!("{:?} {} ", position, stringify!(rustcall));
                            result.push_str(&format!("{:?} ", impl_vm!(read RustFn, self) ));
                            Some(result)
                        }
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
            pub(crate) fn exec(self: &mut Self, context: &mut U) {
                use byteorder::{LittleEndian, ReadBytesExt};
                let instruction = impl_vm!(read u8, self);
                match ByteCode::from_u8(instruction) {
                    ByteCode::rustcall => {
                        T::from_u16(impl_vm!(read RustFn, self)).exec(self, context);
                    },
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

