
// Writes a comment to the bytecode when in debug mode. // TODO: add compiler option instead
macro_rules! comment {
    ($self:ident, $format:literal $(, $value:expr)*) => {
        #[cfg(debug_assertions)]
        $self.writer.comment(&format!($format $(, $value)*));
    }
}

// Writes an 8bit, 16bit or StackAddress sized variant of an instruction that takes one signed argument.
macro_rules! opcode_signed {
    ($self:ident, $variant_8:ident, $variant_16:ident, $variant_sa:ident, $value:expr) => {{
        use core::{i8, i16};
        if $value >= i8::MIN as StackOffset && $value <= i8::MAX as StackOffset {
            $self.writer.$variant_8($value as i8)
        } else if $value >= i16::MIN as StackOffset && $value <= i16::MAX as StackOffset {
            $self.writer.$variant_16($value as i16)
        } else {
            $self.writer.$variant_sa($value)
        }
    }}
}

// Writes an 8bit, 16bit or StackAddress sized variant of an instruction that takes one unsigned argument.
macro_rules! opcode_unsigned {
    ($self:ident, $variant_8:ident, $variant_16:ident, $variant_sa:ident, $value:expr) => {{
        use core::{u8, u16};
        if $value <= u8::MAX as StackAddress {
            $self.writer.$variant_8($value as u8)
        } else if $value <= u16::MAX as StackAddress {
            $self.writer.$variant_16($value as u16)
        } else {
            $self.writer.$variant_sa($value)
        }
    }}
}