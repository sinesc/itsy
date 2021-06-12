
// Writes a comment to the bytecode when in debug mode. // TODO: add compiler option instead
macro_rules! comment {
    ($self:ident, $format:literal $(, $value:expr)*) => {
        #[cfg(debug_assertions)]
        $self.writer.comment(&format!($format $(, $value)*));
    }
}

// Writes a 8, 16 or 32 bit variant of an instruction that takes one signed argument.
macro_rules! opcode_signed {
    ($self:ident, $variant8:ident, $varian16:ident, $variant32:ident, $value:expr) => {{
        use std::{i8, i16};
        if $value >= i8::MIN as i32 && $value <= i8::MAX as i32 {
            $self.writer.$variant8($value as i8)
        } else if $value >= i16::MIN as i32 && $value <= i16::MAX as i32 {
            $self.writer.$varian16($value as i16)
        } else {
            $self.writer.$variant32($value)
        }
    }}
}

// Writes a 8, 16 or 32 bit variant of an instruction that takes one unsigned argument.
macro_rules! opcode_unsigned {
    ($self:ident, $variant8:ident, $varian16:ident, $variant32:ident, $value:expr) => {{
        use std::{u8, u16};
        if $value <= u8::MAX as u32 {
            $self.writer.$variant8($value as u8)
        } else if $value <= u16::MAX as u32 {
            $self.writer.$varian16($value as u16)
        } else {
            $self.writer.$variant32($value)
        }
    }}
}