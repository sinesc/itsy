//! Bytecode generation and execution.

// todo: remove
#![allow(dead_code)]

mod vm;
mod writer;
#[macro_use]
mod macros;
mod opcodes;
mod compiler;

use std::mem::transmute;
pub use self::vm::{VM, VMState};
pub use self::writer::Writer;
pub use self::compiler::{compile, Compiler};

/// A stack value.
pub(crate) type Value = i32;

/// An Itsy bytecode program.
pub type Program = Vec<u8>;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Enum {
    //repr: u8,
    keys: HashMap<usize, u64>
}

#[derive(Debug)]
pub struct Struct {
    fields: HashMap<usize, Type>
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Type {
    u8, u16, u32, u64,
    i8, i16, i32, i64,
    f32, f64,
    bool,
    String,
    Enum(Enum),
    Struct(Struct),
}

impl Type {
    pub fn is_unsigned(self: &Self) -> bool {
        match self {
            Type::u8 | Type::u16 | Type::u32 | Type::u64 => true,
            _ => false,
        }
    }
    pub fn is_signed(self: &Self) -> bool {
        match self {
            Type::i8 | Type::i16 | Type::i32 | Type::i64 => true,
            _ => false,
        }
    }
    pub fn is_float(self: &Self) -> bool {
        match self {
            Type::f32 | Type::f64 => true,
            _ => false,
        }
    }
    pub fn size(self: &Self) -> u8 {
        match self {
            Type::u8 | Type::i8 | Type::bool => 1,
            Type::u16 | Type::i16 => 2,
            Type::u32 | Type::i32 | Type::f32 => 4,
            Type::u64 | Type::i64 | Type::f64 => 8,
            _ => unimplemented!(), // todo: what do I want here? size of string ref and total enum/struct size?
        }
    }
}

/// Converts an i8 to a Value.
#[inline(always)]
pub(crate) fn i8val(value: i8) -> Value {
    value as Value
}

/// Converts a Value to i32.
#[inline(always)]
pub(crate) fn valu8(value: Value) -> u8 {
    value as u8
}

/// Converts a u8 to a Value.
#[inline(always)]
pub(crate) fn u8val(value: u8) -> Value {
    value as Value
}

/// Converts an i8 to a Value.
#[inline(always)]
pub(crate) fn i16val(value: i16) -> Value {
    value as Value
}

/// Converts a u8 to a Value.
#[inline(always)]
pub(crate) fn u16val(value: u16) -> Value {
    value as Value
}

/// Converts a Value to i32.
#[inline(always)]
pub(crate) fn vali32(value: Value) -> i32 {
    value
}

/// Converts an i32 to a Value.
#[inline(always)]
pub(crate) fn i32val(value: i32) -> Value {
    value
}

/// Converts a u32 to a Value.
#[inline(always)]
pub(crate) fn u32val(value: u32) -> Value {
    unsafe { transmute(value) }
}

/// Converts a Value to u32.
#[inline(always)]
pub(crate) fn valu32(value: Value) -> u32 {
    unsafe { transmute(value) }
}

/// Converts an f32 a Value.
#[inline(always)]
pub(crate) fn f32val(value: f32) -> Value {
    unsafe { transmute(value) }
}

/// Converts an i64 to two Values.
#[inline(always)]
pub(crate) fn i64val(value: i64) -> (Value, Value) {
    unsafe { transmute(value) }
}

/// Converts a u64 to two Values.
#[inline(always)]
pub(crate) fn u64val(value: u64) -> (Value, Value) {
    unsafe { transmute(value) }
}

/// Converts an f64 to two Values.
#[inline(always)]
pub(crate) fn f64val(value: f64) -> (Value, Value) {
    unsafe { transmute(value) }
}