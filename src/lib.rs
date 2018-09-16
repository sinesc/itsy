//! Itsy, a tiny language for embedded use.

#[macro_use]
extern crate nom;
extern crate byteorder;

pub mod frontend;
#[macro_use]
pub mod bytecode;

use std::fmt::Debug;
use std::collections::HashMap;

/// A default implementation of ExternRust that maps no functions.
#[allow(non_camel_case_types)]
#[repr(u16)]
#[derive(Copy, Clone, Debug)]
pub enum Standalone {
    #[doc(hidden)]
    _dummy
}
extern_rust!(@trait Standalone);

/// A trait used to make resolver, compiler and VM generic over a set of Rust functions.
/// Use `extern_rust!` macro to generate a type implementing `ExternRust`.
pub trait ExternRust<T>: Clone + Debug + 'static where T: ExternRust<T> {
    #[doc(hidden)]
    fn from_u16(index: u16) -> Self;
    #[doc(hidden)]
    fn to_u16(self: Self) -> u16;
    #[doc(hidden)]
    fn map_name() -> HashMap<&'static str, u16>;
    #[doc(hidden)]
    fn exec(self: Self, vm: &mut bytecode::VM<T>);
}

/// One stop shop to `parse`, `resolve` and `compile` given Itsy source code.
///
/// Call `run` on the returned `VM` struct to execute the program.
pub fn exec<T>(program: &str) -> bytecode::VM<T> where T: ExternRust<T> {
    use frontend::{parse, resolve};
    use bytecode::{compile, VM};

    let parsed = parse(program).unwrap();
    let resolved = resolve::<T>(parsed);
    let mut writer = compile(resolved);

    let start = writer.len();
    writer.call(0, 0); // call first method todo: lookup given function
    //writer.rustcall(MyFns::print);
    writer.exit();

    VM::new(writer.into_program(), start)
}
