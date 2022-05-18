use crate::prelude::*;
use crate::StackAddress;
use crate::shared::typed_ids::BindingId;

#[derive(Copy, Clone, PartialEq)]
pub enum LocalOrigin {
    Binding,
    Argument,
}

/// Describes a single local variable of a stack frame
#[derive(Copy, Clone)]
pub struct Local {
    /// The load-index for this variable.
    pub index: StackAddress,
    /// Whether is is an argument passed into a function or a binding.
    pub origin: LocalOrigin,
}

/// Maps bindings and arguments to indices relative to the stack frame.
pub struct StackFrame {
    /// Maps a binding ID to a variable or argument position on the stack.
    pub map     : Map<BindingId, Local>,
    /// Index of the NEXT argument to be inserted.
    pub arg_pos : StackAddress,
    /// Index for the NEXT variable to be inserted.
    pub var_pos : StackAddress,
    /// Size of the local block return value.
    pub ret_size: u8,
    /// Addresses of forward jumps within the function that need to be replaced with the return location.
    pub unfixed_exit_jmps: Vec<StackAddress>,
}

impl StackFrame {
    const UNKNOWN_BINDING: &'static str = "Unknown local binding";
    pub fn new() -> Self {
        StackFrame {
            map     : Map::new(),
            arg_pos : 0,
            var_pos : 0,
            ret_size: 0,
            unfixed_exit_jmps: Vec::new(),
        }
    }
    /// Add new local variable.
    pub fn insert(self: &mut Self, binding_id: BindingId, index: StackAddress, origin: LocalOrigin) {
        self.map.insert(binding_id, Local { index, origin });
    }
    /// Look up local variable descriptor for the given BindingId.
    pub fn lookup(self: &Self, binding_id: BindingId) -> Local {
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
    // /// Returns the argument size in bytes for the top stack frame descriptor.
    //pub fn arg_size(self: &Self) -> StackAddress {
    //    self.0.borrow().last().expect(Self::NO_STACK).arg_pos
    //}
    // /// Returns the return value size in bytes for the top stack frame descriptor.
    //pub fn ret_size(self: &Self) -> u8 {
    //    self.0.borrow().last().expect(Self::NO_STACK).ret_size
    //}
    /// Look up local variable descriptor for the given BindingId.
    pub fn lookup(self: &mut Self, binding_id: BindingId) -> Local {
        self.0.last().expect(Self::NO_STACK).lookup(binding_id)
    }
    /// Adds a forward jump to the function exit to the list of jumps that need to be fixed (exit address not known at time of adding yet)
    pub fn add_forward_jmp(self: &mut Self, address: StackAddress) {
        self.0.last_mut().expect(Self::NO_STACK).unfixed_exit_jmps.push(address);
    }
}