
// Writes a comment to the bytecode when in debug mode. // TODO: add compiler option instead
macro_rules! comment {
    ($self:ident, $format:literal $(, $value:expr)*) => {
        #[cfg(debug_assertions)]
        $self.writer.comment(&format!($format $(, $value)*));
    }
}

// Writes an 8bit, 16bit or StackAddress sized variant of an instruction that takes one signed argument.
macro_rules! select_signed_opcode {
    ($self:ident, $variant_8:ident, $variant_16:ident, $variant_sa:ident, $value:expr $(, $more:expr)*) => {{
        use core::{i8, i16};
        if $value >= i8::MIN as StackOffset && $value <= i8::MAX as StackOffset {
            $self.writer.$variant_8($value as i8 $(, $more)*)
        } else if $value >= i16::MIN as StackOffset && $value <= i16::MAX as StackOffset {
            $self.writer.$variant_16($value as i16 $(, $more)*)
        } else {
            $self.writer.$variant_sa($value $(, $more)*)
        }
    }}
}

// Writes an 8bit, 16bit or StackAddress sized variant of an instruction that takes one unsigned argument.
macro_rules! select_unsigned_opcode {
    ($self:ident, $variant_8:ident, $variant_16:ident, $variant_sa:ident, $value:expr $(, $more:expr)*) => {{
        use core::{u8, u16};
        if $value <= u8::MAX as StackAddress {
            $self.writer.$variant_8($value as u8 $(, $more)*)
        } else if $value <= u16::MAX as StackAddress {
            $self.writer.$variant_16($value as u16 $(, $more)*)
        } else {
            $self.writer.$variant_sa($value $(, $more)*)
        }
    }}
}

macro_rules! select_builtin {
    ($self:ident, $ty:ident, $variant_8:ident, $variant_16:ident, $variant_32:ident, $variant_64:ident, $variant_x:ident) => {
        if $ty.is_ref() {
            $self.writer.builtincall(Builtin::$variant_x);
        } else {
            match $ty.primitive_size() {
                8 => $self.writer.builtincall(Builtin::$variant_64),
                4 => $self.writer.builtincall(Builtin::$variant_32),
                2 => $self.writer.builtincall(Builtin::$variant_16),
                1 => $self.writer.builtincall(Builtin::$variant_8),
                _ => unreachable!("Invalid type size for builtin call"),
            };
        }
    }
}