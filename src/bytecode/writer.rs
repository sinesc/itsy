//! Bytecode buffer and writer.

use std::cell::{Cell, RefCell, RefMut};
use crate::bytecode::{Program, ConstEndianness, ConstDescriptor};
use crate::runtime::VMFunc;
use crate::util::StackAddress;

/// Bytecode buffer and writer.
///
/// Provides methods to write bytecode instructions to the program as well as constants to the const pool (via the implemented [`StoreConst`](trait.StoreConst.html) trait).
#[derive(Debug)]
pub struct Writer<T> where T: VMFunc<T> {
    pub(crate) program: RefCell<Program<T>>,
    position: Cell<StackAddress>,
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
    pub fn len(self: &Self) -> StackAddress {
        self.program().instructions.len() as StackAddress
    }
    /// Returns the current length of the const pool.
    pub fn const_len(self: &Self) -> StackAddress {
        self.program().consts.len() as StackAddress
    }
    /// Returns the current bytecode write position.
    pub fn position(self: &Self) -> StackAddress {
        self.position.get()
    }
    /// Sets the current bytecode write position.
    pub fn set_position(self: &Self, new_position: StackAddress) {
        self.position.set(new_position);
    }
    /// Converts the writer into the program it wrote.
    pub fn into_program(self: Self) -> Program<T> {
        self.program.into_inner()
    }
    /// Overwrites the program at given position and returns to the previous position afterwards.
    pub fn overwrite(self: &Self, position: StackAddress, mut write_fn: impl FnMut(&Self) -> StackAddress) -> StackAddress {
        let original_position = self.position();
        self.position.set(position);
        let result = write_fn(self);
        self.position.set(original_position);
        result
    }
    /// Write to the current position.
    pub(crate) fn write(self: &Self, buf: &[u8]) {
        let buf_len = buf.len() as StackAddress;
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
    (@write $as_type:ident, $self:ident, $value:ident, $endianess:path) => {
        let position = $self.program().consts.len() as StackAddress;
        let size = std::mem::size_of::<$as_type>() as StackAddress;
        let endianness = $endianess;
        $self.program().const_descriptors.push(ConstDescriptor { position, size, endianness });
        $self.program().consts.extend_from_slice(&$value.to_le_bytes());
    };
    ($type:tt, $endianess:path) => {
        impl<P> StoreConst<$type> for Writer<P> where P: VMFunc<P> {
            fn store_const(self: &Self, value: $type) -> StackAddress {
                let position = self.program().consts.len() as StackAddress;
                impl_store_const!(@write $type, self, value, $endianess);
                position
            }
        }
    };
}

/// Trait for writing typed data to a program const pool.
pub trait StoreConst<T> {
    /// Write a constant to the constant pool
    fn store_const(self: &Self, value: T) -> StackAddress;
}

impl_store_const!(u8, ConstEndianness::Integer);
impl_store_const!(i8, ConstEndianness::Integer);
impl_store_const!(u16, ConstEndianness::Integer);
impl_store_const!(i16, ConstEndianness::Integer);
impl_store_const!(u32, ConstEndianness::Integer);
impl_store_const!(i32, ConstEndianness::Integer);
impl_store_const!(f32, ConstEndianness::Float);
impl_store_const!(u64, ConstEndianness::Integer);
impl_store_const!(i64, ConstEndianness::Integer);
impl_store_const!(f64, ConstEndianness::Float);
impl_store_const!(usize, ConstEndianness::Integer);
impl_store_const!(isize, ConstEndianness::Integer);

impl<P> StoreConst<&str> for Writer<P> where P: VMFunc<P> {
    fn store_const(self: &Self, value: &str) -> StackAddress {
        // string length
        let raw_bytes = &value.as_bytes();
        let size = raw_bytes.len() as StackAddress;
        self.store_const(size);
        // string data
        let position = self.program().consts.len() as StackAddress;
        self.program().const_descriptors.push(ConstDescriptor { position, size, endianness: ConstEndianness::None });
        self.program().consts.extend_from_slice(raw_bytes);
        position
    }
}