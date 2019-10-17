use std::{collections::HashMap, cell::RefCell};
use crate::util::BindingId;
use crate::bytecode::ARG1;

/// Describes a single local variable of a stack frame
#[derive(Copy,Clone)]
pub struct Local {
    /// The load-index for this variable.
    pub index   : i32,
    /// Whether this variable is currently in scope (stack frame does not equal scope!)
    pub in_scope: bool,
}

impl Local {
    pub fn new(index: i32) -> Self {
        Local { index, in_scope: false }
    }
}

/// Maps bindings and arguments to indices relative to the stack frame.
pub struct Locals {
    pub map     : HashMap<BindingId, Local>,
    pub next_arg: i32,
    pub next_var: i32,
    pub arg_size: u32,
    pub ret_size: u32,
}

impl Locals {
    pub fn new() -> Self {
        Locals {
            map     : HashMap::new(),
            next_arg: ARG1,
            next_var: 0,
            arg_size: 0,
            ret_size: 0,
        }
    }
}

/// A stack Locals mappings for nested structures.
pub struct LocalsStack(RefCell<Vec<Locals>>);

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
    pub fn borrow(self: &Self, func: impl FnOnce(&Locals)) {
        let inner = self.0.borrow_mut();
        let locals = inner.last().expect(Self::NO_STACK);
        func(&locals);
    }
    /// Returns the argument size in stack elements for the top stack frame descriptor.
    pub fn arg_size(self: &Self) -> u32 {
        self.0.borrow_mut().last().expect(Self::NO_STACK).arg_size
    }
    /// Returns the return value size in stack elements for the top stack frame descriptor.
    pub fn ret_size(self: &Self) -> u32 {
        self.0.borrow_mut().last().expect(Self::NO_STACK).ret_size
    }
    /// Look up local variable descriptor for the given BindingId.
    pub fn lookup(self: &Self, binding_id: BindingId) -> Local {
        *self.0.borrow().last().expect(Self::NO_STACK).map.get(&binding_id).expect(Self::UNKNOWN_BINDING)
    }
    /// Sets whether the given local variable is currently in scope.
    pub fn set_active(self: &Self, binding_id: BindingId, active: bool) {
        let mut inner = self.0.borrow_mut();
        let locals = inner.last_mut().expect(Self::NO_STACK);
        locals.map.get_mut(&binding_id).expect(Self::UNKNOWN_BINDING).in_scope = active;
    }
}