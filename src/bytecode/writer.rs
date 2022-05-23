//! Bytecode buffer and writer.

use crate::prelude::*;
use crate::StackAddress;
use crate::bytecode::{VMFunc, Program, ConstEndianness, ConstDescriptor, Constructor};
use std::cell::{Cell, RefCell, RefMut};

/// Bytecode buffer and writer.
///
/// Provides methods to write bytecode instructions to the program as well as constants to the const pool (via the implemented [`StoreConst`](trait.StoreConst.html) trait).
#[derive(Debug)]
pub struct Writer<T> {
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
            let end = min(position + buf_len, self.len());
            self.program().instructions.splice(position as usize .. end as usize, buf.iter().cloned());
        }
        self.position.set(self.position.get() + buf_len);
    }
    /// Returns the current length of the const pool.
    pub fn const_len(self: &Self) -> StackAddress {
        self.program().consts.len() as StackAddress
    }
    /// Reserves space for a data-block on the const pool and returns its base address.
    pub fn reserve_const_data(self: &Self, size: StackAddress) -> StackAddress {
        let position = self.program().consts.len() as StackAddress;
        self.program().const_descriptors.push(ConstDescriptor { position, size, endianness: ConstEndianness::None });
        let pool_size = self.program().consts.len();
        self.program().consts.resize(pool_size + size as usize, 0);
        position
    }
}

/// Implements const pool write traits
#[allow(unused_macros)]
macro_rules! impl_store_const {
    ($type:tt, $endianess:path) => {
        impl<P> StoreConst<$type> for Writer<P> where P: VMFunc<P> {
            fn store_const(self: &Self, value: $type) -> StackAddress {
                let position = self.program().consts.len() as StackAddress;
                let size = size_of::<$type>() as StackAddress;
                let mut program = self.program();
                program.const_descriptors.push(ConstDescriptor { position, size, endianness: $endianess });
                program.consts.extend_from_slice(&value.to_le_bytes());
                position
            }
            fn update_const(self: &Self, position: StackAddress, value: $type) {
                let mut program = self.program();
                let position = position as usize;
                for (index, &byte) in value.to_le_bytes().iter().enumerate() {
                    program.consts[position + index] = byte;
                }
            }
        }
    };
}

/// Trait for writing typed data to a program const pool.
pub trait StoreConst<T> {
    /// Write a constant to the constant pool
    fn store_const(self: &Self, value: T) -> StackAddress;
    /// Update a previously written (placeholder) constant.
    fn update_const(self: &Self, position: StackAddress, value: T);
}

impl_store_const!(u8, ConstEndianness::None);
impl_store_const!(i8, ConstEndianness::None);
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
        let mut program = self.program();
        program.const_descriptors.push(ConstDescriptor { position, size, endianness: ConstEndianness::None });
        program.consts.extend_from_slice(raw_bytes);
        position
    }
    fn update_const(self: &Self, _position: StackAddress, _value: &str) {
        // this probably won't be needed and changing string length would be hairy
        unimplemented!("Cannot update constant string");
    }
}

impl<P> StoreConst<Constructor> for Writer<P> where P: VMFunc<P> {
    fn store_const(self: &Self, value: Constructor) -> StackAddress {
        let position = self.program().consts.len() as StackAddress;
        let size = size_of::<Constructor>() as StackAddress;
        let mut program = self.program();
        program.const_descriptors.push(ConstDescriptor { position, size, endianness: ConstEndianness::None });
        program.consts.push(value.to_u8());
        position
    }
    fn update_const(self: &Self, position: StackAddress, value: Constructor) {
        let mut program = self.program();
        program.consts[position as usize] = value.to_u8();
    }
}