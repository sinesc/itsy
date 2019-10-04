//! Bytecode buffer and writer.

use std::{mem::transmute, cell::{Cell, RefCell, RefMut}};
use crate::bytecode::Program;
use crate::runtime::VMFunc;
use crate::util::HeapRef;

/// Bytecode buffer and writer.
///
/// Provides methods to write bytecode instructions to the program as well as constants to the const pool (via the implemented [`StoreConst`](trait.StoreConst.html) trait).
#[derive(Debug)]
pub struct Writer<T> where T: VMFunc<T> {
    pub(crate) program: RefCell<Program<T>>,
    position: Cell<u32>,
}

impl<T> Writer<T> where T: VMFunc<T> {
    /// Creates a new writer instance.
    pub fn new() -> Self {
        Writer {
            program: RefCell::new(Program::<T>::new()),
            position: Cell::new(0),
        }
    }
    pub(crate) fn program(self: &Self) -> RefMut<Program<T>> {
        self.program.borrow_mut()
    }
    /// Returns the current length of the program.
    pub fn len(self: &Self) -> u32 {
        self.program().instructions.len() as u32
    }
    /// Returns the current length of the const pool.
    pub fn const_len(self: &Self) -> u32 {
        self.program().consts.len() as u32
    }
    /// Returns the current bytecode write position.
    pub fn position(self: &Self) -> u32 {
        self.position.get()
    }
    /// Sets the current bytecode write position.
    pub fn set_position(self: &Self, new_position: u32) {
        self.position.set(new_position);
    }
    /// Converts the writer into the program it wrote.
    pub fn into_program(self: Self) -> Program<T> {
        self.program.into_inner()
    }
    /// Overwrites the program at given position and returns to the previous position afterwards.
    pub fn overwrite<F>(self: &Self, position: u32, mut write_fn: F) -> u32
         where F: FnMut(&Self) -> u32
    {
        let original_position = self.position();
        self.position.set(position);
        let result = write_fn(self);
        self.position.set(original_position);
        result
    }
    /// Write to the current position.
    pub(crate) fn write(self: &Self, buf: &[u8]) {
        let buf_len = buf.len() as u32;
        if self.position() == self.len() {
            // append
            self.program().instructions.extend_from_slice(buf);
        } else {
            // overwrite
            let position = self.position();
            let end = ::std::cmp::min(position + buf_len, self.len());
            self.program().instructions.splice(position as usize .. end as usize, buf.iter().cloned());
        }
        self.position.set(self.position.get() + buf_len);
    }
}

/// Implements const pool write traits
#[allow(unused_macros)]
macro_rules! impl_store_const {
    (@write u8, $self:ident, $value:ident) => {
        $self.program().consts.push(unsafe { transmute($value) });
    };
    (@write $size:ident, $self:ident, $value:ident) => {
        let unsigned: $size = unsafe { transmute($value) };
        $self.program().consts.extend_from_slice(&unsigned.to_le_bytes());
    };
    ($size:ident, $type:tt) => {
        impl<P> StoreConst<$type> for Writer<P> where P: VMFunc<P> {
            fn store_const(self: &Self, value: $type) -> u32 {
                let position = self.program().consts.len();
                impl_store_const!(@write $size, self, value);
                position as u32
            }
        }
    };
}

/// Trait for writing typed data to a program const pool.
pub trait StoreConst<T> {
    /// Write a constant to the constant pool
    fn store_const(self: &Self, value: T) -> u32;
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

impl<P> StoreConst<HeapRef> for Writer<P> where P: VMFunc<P> {
    fn store_const(self: &Self, value: HeapRef) -> u32 {
        let position = self.program().consts.len();
        self.program().consts.extend_from_slice(&value.index.to_le_bytes());
        self.program().consts.extend_from_slice(&value.len.to_le_bytes());
        self.program().consts.extend_from_slice(&value.offset.to_le_bytes());
        position as u32
    }
}

impl<P> StoreConst<&str> for Writer<P> where P: VMFunc<P> {
    fn store_const(self: &Self, value: &str) -> u32 {
        let position = self.program().consts.len();
        self.program().consts.extend_from_slice(&value.as_bytes());
        position as u32
    }
}