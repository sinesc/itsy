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
    void,
    u8, u16, u32, u64,
    i8, i16, i32, i64,
    f32, f64,
    bool,
    String,
    Enum(Enum),
    Struct(Struct),
}

/*
#[allow(non_camel_case_types)]
pub enum Signed {
    u16(u16),
    u32(u32),
    u64(u64),
}

#[allow(non_camel_case_types)]
pub enum Unsigned {
    i8(i8),
    i16(i16),
    i32(i32),
    i64(i64),
}

#[allow(non_camel_case_types)]
pub enum Float {
    f32(f32),
    f64(f64),
}
*/

pub enum ByteOp<'a> {
    CreateFrame(&'a [u8])
}