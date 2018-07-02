use super::ast::{Signed, Unsigned};

pub const MIN_I8: Signed = ::std::i8::MIN as Signed;
pub const MIN_I16: Signed = ::std::i16::MIN as Signed;
pub const MIN_I32: Signed = ::std::i32::MIN as Signed;
pub const MIN_I64: Signed = ::std::i64::MIN as Signed;
pub const MAX_I8: Signed = ::std::i8::MAX as Signed;
pub const MAX_I16: Signed = ::std::i16::MAX as Signed;
pub const MAX_I32: Signed = ::std::i32::MAX as Signed;
pub const MAX_I64: Signed = ::std::i64::MAX as Signed;
pub const MIN_U8: Unsigned = ::std::u8::MIN as Unsigned;
pub const MIN_U16: Unsigned = ::std::u16::MIN as Unsigned;
pub const MIN_U32: Unsigned = ::std::u32::MIN as Unsigned;
pub const MIN_U64: Unsigned = ::std::u64::MIN as Unsigned;
pub const MAX_U8: Unsigned = ::std::u8::MAX as Unsigned;
pub const MAX_U16: Unsigned = ::std::u16::MAX as Unsigned;
pub const MAX_U32: Unsigned = ::std::u32::MAX as Unsigned;
pub const MAX_U64: Unsigned = ::std::u64::MAX as Unsigned;