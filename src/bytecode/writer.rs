//! Bytecode buffer and writer.

use std::mem::transmute;
use crate::bytecode::Program;
use crate::runtime::VMFunc;

/// Bytecode buffer and writer.
#[derive(Debug)]
pub struct Writer<T> where T: VMFunc<T> {
    pub(crate) program: Program<T>,
    pub(crate) position: u32,
}

impl<T> Writer<T> where T: VMFunc<T> {
    /// Creates a new writer instance.
    pub fn new() -> Self {
        Writer {
            program: Program::<T>::new(),
            position: 0,
        }
    }
    /// Returns the current length of the program.
    pub fn len(self: &Self) -> u32 {
        self.program.instructions.len() as u32
    }
    /// Returns the current write position.
    pub fn position(self: &Self) -> u32 {
        self.position
    }
    /// Sets the current write position.
    pub fn set_position(self: &mut Self, new_position: u32) {
        self.position = new_position;
    }
    /// Converts the writer into the underlying buffer.
    pub fn into_program(self: Self) -> Program<T> {
        self.program
    }
    /// Overwrites the program at given position and returns to the previous position afterwards.
    pub fn overwrite<F>(self: &mut Self, position: u32, mut write_fn: F) -> u32
         where F: FnMut(&mut Self) -> u32
    {
        let original_position = self.position;
        self.position = position;
        let result = write_fn(self);
        self.position = original_position;
        result
    }
    pub(crate) fn write(self: &mut Self, buf: &[u8]) {
        let buf_len = buf.len() as u32;
        if self.position() == self.len() {
            // append
            self.program.instructions.extend_from_slice(buf);
        } else {
            // overwrite
            let position = self.position();
            let end = ::std::cmp::min(position + buf_len, self.len());
            self.program.instructions.splice(position as usize .. end as usize, buf.iter().cloned());
        }
        self.position += buf_len;
    }
}

/// Implements const pool write traits
#[allow(unused_macros)]
macro_rules! impl_store_const {
    (@write u8, $self:ident, $value:ident) => {
        $self.program.consts.push(unsafe { transmute($value) });
    };
    (@write $size:ident, $self:ident, $value:ident) => {
        let unsigned: $size = unsafe { transmute($value) };
        $self.program.consts.extend_from_slice(&unsigned.to_le_bytes());
    };
    ($size:ident, $type:tt) => {
        impl<P> WriteConst<$type> for Writer<P> where P: VMFunc<P> {
            fn store_const(self: &mut Self, value: $type) -> u32 {
                let position = self.program.consts.len();
                impl_store_const!(@write $size, self, value);
                position as u32
            }
        }
    };
}

/// Trait for generic const writer operations.
pub(crate) trait WriteConst<T> {
    /// Write a constant to the constant pool
    fn store_const(self: &mut Self, value: T) -> u32;
}

impl_store_const!(u8, u8);
impl_store_const!(u8, i8);
impl_store_const!(u16, u16);
impl_store_const!(u16, i16);
impl_store_const!(u32, u32);
impl_store_const!(u32, i32);
impl_store_const!(u32, f32);
impl_store_const!(u64, u64);
impl_store_const!(u64, i64);
impl_store_const!(u64, f64);

impl<P> WriteConst<&str> for Writer<P> where P: VMFunc<P> {
    fn store_const(self: &mut Self, value: &str) -> u32 {
        let position = self.program.consts.len();
        let len = value.len() as u32;
        self.program.consts.extend_from_slice(&len.to_le_bytes());
        self.program.consts.extend_from_slice(&value.as_bytes());
        position as u32
    }
}