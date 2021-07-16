
use crate::util::StackAddress;

#[derive(Clone, Copy)]
pub struct CallInfo {
    pub arg_size: StackAddress,
    pub addr: StackAddress,
}

impl CallInfo {
    pub const PLACEHOLDER: Self = Self { addr: 123, arg_size: 0 };
}