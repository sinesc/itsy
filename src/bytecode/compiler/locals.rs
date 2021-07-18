use std::{collections::HashMap, cell::RefCell};
use crate::shared::{typed_ids::BindingId, types::StackAddress};

/// Describes a single local variable of a stack frame
#[derive(Copy,Clone)]
pub struct Local {
    /// The load-index for this variable.
    pub index: StackAddress,
    /// Whether this variable is currently in scope (stack frame does not equal scope!)
    pub active: bool,
}

impl Local {
    pub fn new(index: StackAddress, active: bool) -> Self {
        Local { index, active }
    }
}

/// Maps bindings and arguments to indices relative to the stack frame.
pub struct Locals { // TODO rename Frame?
    /// Maps a binding ID to a variable or argument position on the stack.
    pub map     : HashMap<BindingId, Local>,
    /// Index of the NEXT argument to be inserted.
    pub arg_pos : StackAddress,
    /// Index for the NEXT variable to be inserted.
    pub var_pos : StackAddress,
    /// Size of the local block return value.
    pub ret_size: u8,
    /// Addresses of forward jumps within the function that need to be replaced with the return location
    pub unfixed_exit_jmps: Vec<StackAddress>,
}

impl Locals {
    pub fn new() -> Self {
        Locals {
            map     : HashMap::new(),
            arg_pos : 0,
            var_pos : 0,
            ret_size: 0,
            unfixed_exit_jmps: Vec::new(),
        }
    }
}

/// A stack of Locals mappings.
pub struct LocalsStack(RefCell<Vec<Locals>>); // TODO rename Frames?

impl LocalsStack {
    const NO_STACK: &'static str = "Attempted to access empty LocalsStack";
    const UNKNOWN_BINDING: &'static str = "Unknown local binding";
    /// Create new local stack frame descriptor stack.
    pub fn new() -> Self {
        LocalsStack(RefCell::new(Vec::new()))
    }
    /// Push stack frame descriptor.
    pub fn push(self: &Self, frame: Locals) {
        self.0.borrow_mut().push(frame);
    }
    /// Pop stack frame descriptor and return it.
    pub fn pop(self: &Self) -> Locals {
        self.0.borrow_mut().pop().expect(Self::NO_STACK)
    }
    /// Borrow the top stack frame descriptor within given function.
    pub fn borrow_mut(self: &Self, func: impl FnOnce(&mut Locals)) {
        let mut inner = self.0.borrow_mut();
        let mut locals = inner.last_mut().expect(Self::NO_STACK);
        func(&mut locals);
    }
    // /// Returns the argument size in bytes for the top stack frame descriptor.
    //pub fn arg_size(self: &Self) -> StackAddress {
    //    self.0.borrow().last().expect(Self::NO_STACK).arg_pos
    //}
    /// Returns the return value size in bytes for the top stack frame descriptor.
    pub fn ret_size(self: &Self) -> u8 {
        self.0.borrow().last().expect(Self::NO_STACK).ret_size
    }
    /// Look up local variable descriptor for the given BindingId.
    pub fn lookup(self: &Self, binding_id: BindingId) -> Local {
        *self.0.borrow().last().expect(Self::NO_STACK).map.get(&binding_id).expect(Self::UNKNOWN_BINDING)
    }
    /// Marks the the given local variable as initialized and returns its index.
    pub fn make_active(self: &Self, binding_id: BindingId) -> StackAddress {
        let mut inner = self.0.borrow_mut();
        let locals = inner.last_mut().expect(Self::NO_STACK);
        let local = locals.map.get_mut(&binding_id).expect(Self::UNKNOWN_BINDING);
        local.active = true;
        local.index
    }
    /// Adds a forward jump to the function exit to the list of jumps that need to be fixed (exit address not known at time of adding yet)
    pub fn add_forward_jmp(self: &Self, address: StackAddress) {
        self.0.borrow_mut().last_mut().expect(Self::NO_STACK).unfixed_exit_jmps.push(address);
    }
}