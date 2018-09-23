//! Bytecode buffer and writer.

use std::io::{self, Write, Seek, SeekFrom};
use crate::bytecode::{Value, Program};
use std::mem::transmute;
use crate::ExternRust;

/// Bytecode buffer and writer.
#[derive(Debug)]
pub struct Writer<T> where T: ExternRust<T> {
    pub(crate) program: Program<T>,
    pub(crate) position: u32,
}

impl<T> Writer<T> where T: ExternRust<T> {
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
}

/// Implements const pool write traits
#[allow(unused_macros)]
macro_rules! impl_write_const {
    // push operations
    (@push large, $type:tt, $writer: ident, $var:ident) => { {
        let (low, high): (u32, u32) = unsafe { transmute($var) };
        impl_write_const!(@push normal, u32, $writer, low);
        impl_write_const!(@push normal, u32, $writer, high);
    } };
    (@push $size:tt, $type:tt, $writer: ident, $var:ident) => { $writer.program.consts.push( impl_convert!($size, Value, $var) ); };

    // implement const write operations for Writer
    ($size:tt, $type:tt) => {
        impl<P> WriteConst<$type> for Writer<P> where P: ExternRust<P> {
            fn write_const(self: &mut Self, value: $type) -> u32 {
                let position = self.program.consts.len();
                impl_write_const!(@push $size, $type, self, value);
                position as u32
            }
        }
    };
}

/// Trait for generic const writer operations.
pub(crate) trait WriteConst<T> {
    /// Write a constant to the constant pool
    fn write_const(self: &mut Self, value: T) -> u32;
}

impl_write_const!(small, u8);
impl_write_const!(small, i8);
impl_write_const!(small, u16);
impl_write_const!(small, i16);
impl_write_const!(normal, u32);
impl_write_const!(normal, i32);
impl_write_const!(normal, f32);
impl_write_const!(large, u64);
impl_write_const!(large, i64);
impl_write_const!(large, f64);

impl<T> Write for Writer<T> where T: ExternRust<T> {
    fn write(self: &mut Self, buf: &[u8]) -> io::Result<usize> {
        let buf_len = buf.len() as u32;
        if self.position == self.len() {
            // append
            self.program.instructions.extend_from_slice(buf);
        } else {
            // overwrite
            let end = ::std::cmp::min(self.position + buf_len, self.len());
            self.program.instructions.splice(self.position as usize .. end as usize, buf.iter().cloned());
        }
        self.position += buf_len;
        Ok(buf.len())
    }
    fn write_all(self: &mut Self, buf: &[u8]) -> io::Result<()> {
        self.write(buf).unwrap();
        Ok(())
    }
    fn flush(self: &mut Self) -> io::Result<()> {
        Ok(())
    }
}

impl<T> Seek for Writer<T> where T: ExternRust<T> {
    fn seek(self: &mut Self, pos: SeekFrom) -> io::Result<u64> {
        self.position = match pos {
            SeekFrom::Start(pos) => pos as u32,
            SeekFrom::End(pos) => (self.program.instructions.len() as i64 + pos) as u32,
            SeekFrom::Current(pos) => (self.position as i64 + pos) as u32,
        };
        Ok(self.position as u64)
    }
}