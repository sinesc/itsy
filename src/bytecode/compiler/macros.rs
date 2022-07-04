
// Writes a comment to the bytecode if the symbols feature is enabled.
macro_rules! comment {
    ($self:ident, $format:literal $(, $value:expr)*) => {
        #[cfg(feature="symbols")]
        $self.writer.comment(&format!($format $(, $value)*));
    }
}

// Writes an 8bit, 16bit or StackAddress sized variant of an instruction that takes one signed argument.
/*macro_rules! select_signed_opcode {
    (@if_sa none, $self:ident, $value:expr $(, $more:expr)*) => { unreachable!("Unsupported sa-sized variant") };
    (@if_sa $variant_sa:ident, $self:ident, $value:expr $(, $more:expr)*) => {
        $self.writer.$variant_sa($value as StackOffset $(, $more)*)
    };
    ($self:ident, $variant_8:ident, $variant_16:ident, $variant_sa:ident, $value:expr $(, $more:expr)*) => {{
        use core::{i8, i16};
        if $value as StackOffset >= i8::MIN as StackOffset && $value as StackOffset <= i8::MAX as StackOffset {
            $self.writer.$variant_8($value as i8 $(, $more)*)
        } else if $value as StackOffset >= i16::MIN as StackOffset && $value as StackOffset <= i16::MAX as StackOffset {
            $self.writer.$variant_16($value as i16 $(, $more)*)
        } else {
            select_signed_opcode!(@if_sa $variant_sa, $self, $value $(, $more)*)
        }
    }}
}*/

// Writes an 8bit, 16bit or StackAddress sized variant of an instruction that takes one unsigned argument.
macro_rules! select_unsigned_opcode {
    (@if_sa none, $self:ident, $value:expr $(, $more:expr)*) => { unreachable!("Unsupported sa-sized variant") };
    (@if_sa $variant_sa:ident, $self:ident, $value:expr $(, $more:expr)*) => {
        $self.writer.$variant_sa($value as StackAddress $(, $more)*)
    };
    ($self:ident, $variant_8:ident, $variant_16:ident, $variant_sa:ident, $value:expr $(, $more:expr)*) => {{
        use core::{u8, u16};
        if $value as StackAddress <= u8::MAX as StackAddress {
            $self.writer.$variant_8($value as u8 $(, $more)*)
        } else if $value as StackAddress <= u16::MAX as StackAddress {
            $self.writer.$variant_16($value as u16 $(, $more)*)
        } else {
            select_unsigned_opcode!(@if_sa $variant_sa, $self, $value $(, $more)*)
        }
    }}
}
