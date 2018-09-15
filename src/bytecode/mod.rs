//! Bytecode generation and execution.

// todo: remove
#![allow(dead_code)]

mod vm;
mod writer;
#[macro_use]
mod macros;
mod opcodes;
mod compiler;

use std::marker::PhantomData;
use std::mem::transmute;
use std::fmt::Debug;
use std::collections::HashMap;
pub use self::vm::{VM, VMState};
pub use self::writer::Writer;
pub use self::compiler::{compile, Compiler};

/// A stack value.
pub(crate) type Value = i32;

/// An Itsy bytecode program.
#[derive(Debug)]
pub struct Program<T> where T: RustFnId {
    rust_fn: PhantomData<T>,
    pub(crate) instructions: Vec<u8>,
    pub(crate) consts      : Vec<Value>,
}

impl<T> Program<T> where T: RustFnId {
    pub fn new() -> Self {
        Program {
            rust_fn     : PhantomData,
            instructions: Vec::new(),
            consts      : Vec::new(),
        }
    }
}

/// Default generic marker for resolver, compiler and VM.
#[allow(non_camel_case_types)]
#[repr(u16)]
#[derive(Copy, Clone, Debug)]
pub enum Standalone {
    #[doc(hidden)]
    _dummy
}
fn_map!(@trait Standalone);

/// An internal trait used to make resolver, compiler and VM generic over a set of external rust functions.
pub trait RustFnId: Clone + Debug + 'static {
    #[doc(hidden)]
    fn from_u16(index: u16) -> Self;
    #[doc(hidden)]
    fn to_u16(self: Self) -> u16;
    #[doc(hidden)]
    fn map_name() -> HashMap<&'static str, u16>;
    #[doc(hidden)]
    fn exec(self: Self /*, vm: &mut VM*/);
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