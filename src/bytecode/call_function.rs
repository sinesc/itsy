//! Support for calling top-level Itsy functions from Rust via
//! [`VM::call_function`](crate::runtime::VM::call_function).
//!
//! A compiled [`Program`](crate::bytecode::Program) records a table of host-callable functions
//! ([`FunctionMeta`]) describing each function's entry point and the marshallable type
//! ([`ValueKind`]) of its arguments and return value. This module holds that type information
//! together with the compiler-side helper that builds the table ([`build_function_table`]) and the
//! runtime-side helpers that marshal host values across the boundary ([`arg_matches`] and the
//! [`match_value_kind`] macro). The call entry point itself lives on the VM; the
//! [`CallError`](crate::runtime::CallError)/[`CallResult`](crate::runtime::CallResult) types live in
//! the runtime error module.

use crate::prelude::*;
use crate::{StackAddress, FrameAddress};
use crate::bytecode::read;

#[cfg(feature="compiler")]
use crate::shared::{MetaContainer, meta::Type, typed_ids::TypeId};
#[cfg(feature="compiler")]
use crate::frontend::{ast, parser::types::ParsedModule};
#[cfg(feature="compiler")]
use crate::bytecode::{VMFunc, compiler::{Compiler, error::{CompileResult, OptionToCompileError}}};

#[cfg(feature="runtime")]
use std::any::Any;

/// The marshallable type of a top-level function's argument or return value, recorded in a
/// [`Program`](crate::bytecode::Program)'s callable function table. Lets the host convert `dyn Any`
/// values to and from the VM stack without the resolver's type information (which is dropped after
/// compilation).
#[derive(Copy, Clone, Debug, PartialEq)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum ValueKind {
    void, u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, bool, String,
    /// A reference type other than String (array/struct/enum/etc.) — not yet marshallable by
    /// [`VM::call_function`](crate::runtime::VM::call_function).
    Unsupported,
}

impl ValueKind {
    /// Reconstructs a `ValueKind` from its serialized discriminant.
    pub(crate) fn from_u8(value: u8) -> Option<Self> {
        use ValueKind::*;
        Some(match value {
            0 => void, 1 => u8, 2 => u16, 3 => u32, 4 => u64,
            5 => i8, 6 => i16, 7 => i32, 8 => i64, 9 => f32, 10 => f64,
            11 => bool, 12 => String, 13 => Unsupported,
            _ => return None,
        })
    }
    /// Human-readable Itsy type name, used in host-facing error messages.
    #[cfg(feature="runtime")]
    pub fn type_name(self: &Self) -> &'static str {
        use ValueKind::*;
        match self {
            void => "void", u8 => "u8", u16 => "u16", u32 => "u32", u64 => "u64",
            i8 => "i8", i16 => "i16", i32 => "i32", i64 => "i64", f32 => "f32", f64 => "f64",
            bool => "bool", String => "String", Unsupported => "<unsupported>",
        }
    }
}

/// Metadata describing a top-level Itsy function, allowing the host to invoke it by name via
/// [`VM::call_function`](crate::runtime::VM::call_function).
#[derive(Clone, Debug)]
pub struct FunctionMeta {
    /// Bytecode address of the function's entry point.
    pub(crate) addr: StackAddress,
    /// Combined primitive size of all arguments (their on-stack footprint).
    pub(crate) arg_size: FrameAddress,
    /// Marshallable kind of each argument, in declaration order.
    pub(crate) args: Vec<ValueKind>,
    /// Marshallable kind of the return value (`Void` for no return).
    pub(crate) ret: ValueKind,
}

/// Appends the host-call return address and callable function table to `result`. Companion to
/// [`deserialize_function_table`]; called from [`Program::to_bytes`](crate::bytecode::Program::to_bytes).
pub(crate) fn serialize_function_table(host_return_addr: StackAddress, functions: &Map<String, FunctionMeta>, result: &mut Vec<u8>) {
    result.extend_from_slice(&host_return_addr.to_ne_bytes()[..]);
    result.extend_from_slice(&functions.len().to_ne_bytes()[..]);
    for (name, meta) in functions {
        result.extend_from_slice(&name.len().to_ne_bytes()[..]);
        result.extend_from_slice(name.as_bytes());
        result.extend_from_slice(&meta.addr.to_ne_bytes()[..]);
        result.extend_from_slice(&meta.arg_size.to_ne_bytes()[..]);
        result.extend_from_slice(&meta.args.len().to_ne_bytes()[..]);
        for arg in &meta.args {
            result.push(*arg as u8);
        }
        result.push(meta.ret as u8);
    }
}

/// Reads the host-call return address and callable function table from the front of `program`,
/// advancing it past the consumed bytes. Companion to [`serialize_function_table`]; returns `None` on
/// malformed input. Called from [`Program::from_bytes`](crate::bytecode::Program::from_bytes).
pub(crate) fn deserialize_function_table(program: &mut &[ u8 ]) -> Option<(StackAddress, Map<String, FunctionMeta>)> {
    const USIZE: usize = size_of::<usize>();
    const SA: usize = size_of::<StackAddress>();
    const FA: usize = size_of::<FrameAddress>();
    let host_return_addr = StackAddress::from_ne_bytes(read(program, SA)?.try_into().ok()?);
    let functions_count: usize = usize::from_ne_bytes(read(program, USIZE)?.try_into().ok()?);
    let mut functions: Map<String, FunctionMeta> = Map::new();
    for _ in 0..functions_count {
        let name_len: usize = usize::from_ne_bytes(read(program, USIZE)?.try_into().ok()?);
        let name = String::from_utf8(read(program, name_len)?.to_vec()).ok()?;
        let addr = StackAddress::from_ne_bytes(read(program, SA)?.try_into().ok()?);
        let arg_size = FrameAddress::from_ne_bytes(read(program, FA)?.try_into().ok()?);
        let args_count: usize = usize::from_ne_bytes(read(program, USIZE)?.try_into().ok()?);
        let mut args: Vec<ValueKind> = Vec::with_capacity(args_count);
        for _ in 0..args_count {
            args.push(ValueKind::from_u8(read(program, 1)?[0])?);
        }
        let ret = ValueKind::from_u8(read(program, 1)?[0])?;
        functions.insert(name, FunctionMeta { addr, arg_size, args, ret });
    }
    Some((host_return_addr, functions))
}

/// Builds the host-callable function table from all top-level (free) functions in `modules` so a
/// host can invoke them by name via [`VM::call_function`](crate::runtime::VM::call_function).
/// Methods/trait functions/generators are not top-level and are intentionally excluded.
#[cfg(feature="compiler")]
pub(crate) fn build_function_table<T>(compiler: &Compiler<T>, modules: &[ParsedModule]) -> CompileResult<Map<String, FunctionMeta>> where T: VMFunc<T> {
    let mut functions = Map::new();
    for module in modules {
        for statement in module.statements() {
            if let ast::Statement::Function(function) = statement {
                let function_id = compiler.constant_by_id(function.constant_id.ice()?).value.as_function_id().ice()?;
                let addr = compiler.function_address(function_id).ice_msg("Missing function address")?;
                let func = compiler.function_by_id(function_id);
                let arg_size = func.arg_size(compiler);
                let arg_type_ids = func.arg_type_ids(compiler).clone();
                let ret_type_id = func.ret_type_id(compiler);
                let args = arg_type_ids.into_iter()
                    .map(|tid| value_kind_of(compiler, tid.ice()?))
                    .collect::<CompileResult<Vec<_>>>()?;
                let ret = match ret_type_id {
                    Some(tid) => value_kind_of(compiler, tid)?,
                    None => ValueKind::void,
                };
                functions.insert(function.shared.sig.ident.name.clone(), FunctionMeta { addr, arg_size, args, ret });
            }
        }
    }
    Ok(functions)
}

/// Maps a resolved type to the marshallable [`ValueKind`] recorded in the program's function table.
#[cfg(feature="compiler")]
fn value_kind_of<T>(compiler: &Compiler<T>, type_id: TypeId) -> CompileResult<ValueKind> where T: VMFunc<T> {
    Ok(match compiler.type_by_id(type_id) {
        Type::void => ValueKind::void,
        Type::u8 => ValueKind::u8,
        Type::u16 => ValueKind::u16,
        Type::u32 => ValueKind::u32,
        Type::u64 => ValueKind::u64,
        Type::i8 => ValueKind::i8,
        Type::i16 => ValueKind::i16,
        Type::i32 => ValueKind::i32,
        Type::i64 => ValueKind::i64,
        Type::f32 => ValueKind::f32,
        Type::f64 => ValueKind::f64,
        Type::bool => ValueKind::bool,
        Type::String => ValueKind::String,
        _ => ValueKind::Unsupported,
    })
}

/// Builds a `match` over a primitive/String/void [`ValueKind`] used when marshalling host values.
#[cfg(feature="runtime")]
macro_rules! match_value_kind {
    (
        $kind:expr,
        numeric(| $t:ident | $numeric:expr),
        $( $pat:pat => $arm:expr ),+ $(,)?
    ) => {{
        match $kind {
            ValueKind::u8  => { type $t = u8;  $numeric },
            ValueKind::u16 => { type $t = u16; $numeric },
            ValueKind::u32 => { type $t = u32; $numeric },
            ValueKind::u64 => { type $t = u64; $numeric },
            ValueKind::i8  => { type $t = i8;  $numeric },
            ValueKind::i16 => { type $t = i16; $numeric },
            ValueKind::i32 => { type $t = i32; $numeric },
            ValueKind::i64 => { type $t = i64; $numeric },
            ValueKind::f32 => { type $t = f32; $numeric },
            ValueKind::f64 => { type $t = f64; $numeric },
            $( $pat => $arm ),+
        }
    }}
}
#[cfg(feature="runtime")]
pub(crate) use match_value_kind;

/// Returns whether `arg` can be marshalled to the given primitive/String [`ValueKind`].
#[cfg(feature="runtime")]
pub(crate) fn arg_matches(arg: &dyn Any, kind: ValueKind) -> bool {
    match_value_kind!(kind,
        numeric(|N| arg.is::<N>()),
        ValueKind::bool => arg.is::<bool>(),
        ValueKind::String => arg.is::<String>(),
        ValueKind::void | ValueKind::Unsupported => false,
    )
}
