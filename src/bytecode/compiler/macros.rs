
// Writes a comment to the bytecode if the symbols feature is enabled.
macro_rules! comment {
    ($self:ident, $format:literal $(, $value:expr)*) => {
        #[cfg(all(feature="symbols", feature="comments"))]
        $self.writer.comment(&format!($format $(, $value)*));
    }
}
pub(crate) use comment;

// Selects an opcode variant based on type-size.
macro_rules! select_primitive_size {
    ($self:ident, $result_type:ident, $instruction:ident $(, $more:expr)*) => {{
        use paste::paste;
        paste! {
            match $result_type.primitive_size() {
                1 => $self.writer.[<$instruction 8>]($($more),*),
                2 => $self.writer.[<$instruction 16>]($($more),*),
                4 => $self.writer.[<$instruction 32>]($($more),*),
                8 => $self.writer.[<$instruction 64>]($($more),*),
                size @ _ => Self::ice(&format!("{}: Unsupported size {} of type {:?}", stringify!($instruction), size, $result_type))?,
            }
        }
    }}
}
pub(crate) use select_primitive_size;

// Selects an integer opcode variant based on type.
macro_rules! select_integer_type {
    ($self:ident, $result_type:ident, $instruction:ident $(, $more:expr)*) => {{
        use paste::paste;
        paste! {
            match $result_type {
                Type::i8 => $self.writer.[<$instruction s8>]($($more),*),
                Type::i16 => $self.writer.[<$instruction s16>]($($more),*),
                Type::i32 => $self.writer.[<$instruction s32>]($($more),*),
                Type::i64 => $self.writer.[<$instruction s64>]($($more),*),
                Type::u8 => $self.writer.[<$instruction u8>]($($more),*),
                Type::u16 => $self.writer.[<$instruction u16>]($($more),*),
                Type::u32 => $self.writer.[<$instruction u32>]($($more),*),
                Type::u64 => $self.writer.[<$instruction u64>]($($more),*),
                _ => Self::ice(&format!("{}: Unsupported type {:?}", stringify!($instruction), $result_type))?,
            }
        }
    }}
}
pub(crate) use select_integer_type;

// Selects a numeric opcode variant based on type.
macro_rules! select_numeric_type {
    ($self:ident, $result_type:ident, $instruction:ident $(, $more:expr)*) => {{
        use paste::paste;
        paste! {
            match $result_type {
                Type::i8 => $self.writer.[<$instruction s8>]($($more),*),
                Type::i16 => $self.writer.[<$instruction s16>]($($more),*),
                Type::i32 => $self.writer.[<$instruction s32>]($($more),*),
                Type::i64 => $self.writer.[<$instruction s64>]($($more),*),
                Type::u8 => $self.writer.[<$instruction u8>]($($more),*),
                Type::u16 => $self.writer.[<$instruction u16>]($($more),*),
                Type::u32 => $self.writer.[<$instruction u32>]($($more),*),
                Type::u64 => $self.writer.[<$instruction u64>]($($more),*),
                Type::f32 => $self.writer.[<$instruction f32>]($($more),*),
                Type::f64 => $self.writer.[<$instruction f64>]($($more),*),
                _ => Self::ice(&format!("{}: Unsupported type {:?}", stringify!($instruction), $result_type))?,
            }
        }
    }}
}
pub(crate) use select_numeric_type;

// Selects a numeric opcode variant based on type.
macro_rules! select_numeric_cast_type {
    ($self:ident, $result_type:ident, $instruction:ident $([ $extra:expr ])? $(, $more:expr)*) => {{
        use paste::paste;
        paste! {
            match $result_type {
                Type::i8 => $self.writer.[<$instruction s8>]($($extra,)? $($more.as_signed().ice()? as i8),*),
                Type::i16 => $self.writer.[<$instruction s16>]($($extra,)? $($more.as_signed().ice()? as i16),*),
                Type::i32 => $self.writer.[<$instruction s32>]($($extra,)? $($more.as_signed().ice()? as i32),*),
                Type::i64 => $self.writer.[<$instruction s64>]($($extra,)? $($more.as_signed().ice()? as i64),*),
                Type::u8 => $self.writer.[<$instruction u8>]($($extra,)? $($more.as_unsigned().ice()? as u8),*),
                Type::u16 => $self.writer.[<$instruction u16>]($($extra,)? $($more.as_unsigned().ice()? as u16),*),
                Type::u32 => $self.writer.[<$instruction u32>]($($extra,)? $($more.as_unsigned().ice()? as u32),*),
                Type::u64 => $self.writer.[<$instruction u64>]($($extra,)? $($more.as_unsigned().ice()? as u64),*),
                Type::f32 => $self.writer.[<$instruction f32>]($($extra,)? $($more.as_float().ice()? as f32),*),
                Type::f64 => $self.writer.[<$instruction f64>]($($extra,)? $($more.as_float().ice()? as f64),*),
                _ => Self::ice(&format!("{}: Unsupported type {:?}", stringify!($instruction), $result_type))?,
            }
        }
    }}
}
pub(crate) use select_numeric_cast_type;
