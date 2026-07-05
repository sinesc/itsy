#![allow(unused_variables)]
#![allow(dead_code)]

use crate::prelude::*;
use crate::StackAddress;
use crate::shared::{typed_ids::FunctionId, typed_ids::TypeId};

/// Tracks call and function addresses so that calls made before the function address was known can be fixed.
pub struct Functions {
    // Maps functions to their call address.
    functions: UnorderedMap<FunctionId, StackAddress>,
    /// Bytecode locations of function call instructions that need their target address fixed (because the target wasn't written yet).
    call_placeholder: UnorderedMap<FunctionId, Vec<(StackAddress, bool)>>,
}

impl Functions {
    const PLACEHOLDER: StackAddress = 123;

    /// Creates a new instance.
    pub fn new() -> Self {
        Self {
            functions: UnorderedMap::new(),
            call_placeholder: UnorderedMap::new(),
        }
    }
    /// Registers an absolute function address and returns a list of placeholder positions that need to be replaced with calls to this function.
    pub fn register_function(self: &mut Self, function_id: FunctionId, addr: StackAddress) -> Option<Vec<(StackAddress, bool)>> {
        self.functions.insert(function_id, addr);
        self.call_placeholder.remove(&function_id)
    }
    /// Either returns actual function address or registers the call for later replacement and returns placeholder data.
    pub fn register_call(self: &mut Self, function_id: FunctionId, writer_position: StackAddress, load_only: bool) -> StackAddress {
        if let Some(&target) = self.functions.get(&function_id) {
            target
        } else {
            self.call_placeholder.entry(function_id).or_insert(Vec::new()).push((writer_position, load_only));
            Self::PLACEHOLDER
        }
    }
    /// Returns the address for the given function, if available.
    pub fn get(self: &Self, function_id: FunctionId) -> Option<StackAddress> {
        self.functions.get(&function_id).cloned()
    }
}

/// Loop break/continue jump instruction position.
pub(crate) enum LoopControl {
    Continue(StackAddress),
    Break(StackAddress),
}

/// Tracks loop break/continue jump instruction placeholders for later replacement (once addresses are known)
pub(crate) struct LoopControlStack {
    stack: Vec<Vec<LoopControl>>,
}

impl LoopControlStack {
    /// Creates a new loop control stack.
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
        }
    }
    /// Push another loop structure onto the loopcontrol stack
    pub fn push(self: &mut Self) {
        self.stack.push(Vec::new());
    }
    /// Pop the top loop structure off the loopcontrol stack
    pub fn pop(self: &mut Self) -> Vec<LoopControl> {
        self.stack.pop().unwrap()
    }
    /// Register a loop control instruction placceholder.
    pub fn add_jump(self: &mut Self, item: LoopControl) {
        self.stack.last_mut().unwrap().push(item);
    }
}

/// Cleanup required when exiting a for-loop early (via `return`).
///
/// Array and map loops clone the collection at entry and must Dec the clone
/// and the source on exit. Generator loops hold one reference that must be Dec'd.
/// Range loops leave the upper bound on the stack during the body; it must be discarded.
#[derive(Clone, Copy)]
pub(crate) enum ForLoopCleanup {
    /// Array or map loop: two Dec operations (clone + source).
    Collection(TypeId),
    /// Generator loop: one Dec operation (generator reference).
    Generator(TypeId),
    /// Range loop: discard the upper bound (primitive) left on the stack.
    Range(StackAddress), // size in bytes to discard
}

/// Stack of active for-loop cleanup requirements.
///
/// Used so that `return` inside a for-loop can emit the necessary refcount
/// decrements before exiting the function.
pub(crate) struct ForLoopStack {
    stack: Vec<ForLoopCleanup>,
}

impl ForLoopStack {
    /// Creates a new empty for-loop stack.
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
        }
    }
    /// Push cleanup info for a new for-loop.
    pub fn push(self: &mut Self, cleanup: ForLoopCleanup) {
        self.stack.push(cleanup);
    }
    /// Pop cleanup info when a for-loop finishes normally.
    pub fn pop(self: &mut Self) {
        self.stack.pop();
    }
    /// Iterate all active for-loop cleanups (innermost first).
    pub fn iter(&self) -> std::slice::Iter<'_, ForLoopCleanup> {
        self.stack.iter()
    }
    /// Returns `true` if there are no active for-loops.
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
}
