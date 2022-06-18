use crate::prelude::*;
use crate::StackAddress;
use crate::shared::typed_ids::BindingId;

/// Maps bindings and arguments to indices relative to the stack frame.
pub struct StackFrame {
    /// Maps a binding ID to a variable or argument position on the stack.
    pub map     : Map<BindingId, StackAddress>,
    /// Index for the NEXT argument to be inserted.
    pub arg_pos : StackAddress,
    /// Index for the NEXT variable to be inserted.
    pub var_pos : StackAddress,
    /// Size of the local block return value.
    pub ret_size: u8,
}

impl StackFrame {
    const UNKNOWN_BINDING: &'static str = "Unknown local binding";
    pub fn new() -> Self {
        StackFrame {
            map     : Map::new(),
            arg_pos : 0,
            var_pos : 0,
            ret_size: 0,
        }
    }
    /// Add new local variable.
    pub fn insert(self: &mut Self, binding_id: BindingId, index: StackAddress) {
        self.map.insert(binding_id, index);
    }
    /// Look up local variable descriptor for the given BindingId.
    pub fn lookup(self: &Self, binding_id: BindingId) -> StackAddress {
        *self.map.get(&binding_id).expect(Self::UNKNOWN_BINDING)
    }
}

/// A stack of Locals mappings.
pub struct StackFrames(Vec<StackFrame>);

impl StackFrames {
    const NO_STACK: &'static str = "Attempted to access empty LocalsStack";
    /// Create new local stack frame descriptor stack.
    pub fn new() -> Self {
        StackFrames(Vec::new())
    }
    /// Push stack frame descriptor.
    pub fn push(self: &mut Self, frame: StackFrame) {
        self.0.push(frame);
    }
    /// Pop stack frame descriptor and return it.
    pub fn pop(self: &mut Self) -> StackFrame {
        self.0.pop().expect(Self::NO_STACK)
    }
    /// Returns the argument size in bytes for the top stack frame descriptor.
    pub fn arg_size(self: &Self) -> StackAddress {
        self.0.last().expect(Self::NO_STACK).arg_pos
    }
    /// Returns the return value size in bytes for the top stack frame descriptor.
    pub fn ret_size(self: &Self) -> u8 {
        self.0.last().expect(Self::NO_STACK).ret_size
    }
    /// Look up local variable descriptor for the given BindingId.
    pub fn lookup(self: &mut Self, binding_id: BindingId) -> StackAddress {
        self.0.last().expect(Self::NO_STACK).lookup(binding_id)
    }
}