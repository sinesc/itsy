// todo: remove
#![allow(dead_code, unused_imports)]

use util::{Integer, Signed, Unsigned};
use std::{u8, u16, u32, u64, i8, i16, i32, i64};

enum PrimitiveRepr {
    Unsigned(Integer, Integer),
    Signed(Integer, Integer),
    Float(u8),
    Bool,
    String,
}

struct Primitive {
    name    : &'static str,
    repr    : PrimitiveRepr,
    //size    : u8,
}

const PRIMITIVES: [ Primitive; 12 ] = [
    Primitive {
        name: "bool",
        repr: PrimitiveRepr::Bool,
    },
    Primitive {
        name: "String",
        repr: PrimitiveRepr::String,
    },

    Primitive {
        name: "u8",
        repr: PrimitiveRepr::Unsigned(Integer::Unsigned(u8::MIN as Unsigned), Integer::Unsigned(u8::MAX as Unsigned))
    },
    Primitive {
        name: "u16",
        repr: PrimitiveRepr::Unsigned(Integer::Unsigned(u16::MIN as Unsigned), Integer::Unsigned(u16::MAX as Unsigned))
    },
    Primitive {
        name: "u32",
        repr: PrimitiveRepr::Unsigned(Integer::Unsigned(u32::MIN as Unsigned), Integer::Unsigned(u32::MAX as Unsigned))
    },
    Primitive {
        name: "u64",
        repr: PrimitiveRepr::Unsigned(Integer::Unsigned(u64::MIN as Unsigned), Integer::Unsigned(u64::MAX as Unsigned))
    },

    Primitive {
        name: "i8",
        repr: PrimitiveRepr::Signed(Integer::Signed(i8::MIN as Signed), Integer::Signed(i8::MAX as Signed))
    },
    Primitive {
        name: "i16",
        repr: PrimitiveRepr::Signed(Integer::Signed(i16::MIN as Signed), Integer::Signed(i16::MAX as Signed))
    },
    Primitive {
        name: "i32",
        repr: PrimitiveRepr::Signed(Integer::Signed(i32::MIN as Signed), Integer::Signed(i32::MAX as Signed))
    },
    Primitive {
        name: "i64",
        repr: PrimitiveRepr::Signed(Integer::Signed(i64::MIN as Signed), Integer::Signed(i64::MAX as Signed))
    },

    Primitive {
        name: "f32",
        repr: PrimitiveRepr::Float(32)
    },
    Primitive {
        name: "f64",
        repr: PrimitiveRepr::Float(64)
    }
];

