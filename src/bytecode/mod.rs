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
