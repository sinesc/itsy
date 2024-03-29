use crate::prelude::*;
use crate::{StackAddress, FrameAddress};
use crate::bytecode::HeapRef;

/// A stack holding temporary bytecode operation results and inputs.
#[derive(Debug)]
pub struct Stack {
    /// Raw stack data.
    data            : Vec<u8>,
    /// Current frame pointer.
    pub(crate) fp   : StackAddress,
    /// Base frame pointer, pointing to the first byte after constant data.
    base_fp         : StackAddress,
}

impl Stack {
    /// Creates a new VM value stack.
    pub fn new() -> Self {
        Stack {
            data    : Vec::with_capacity(4096),
            fp      : 0,
            base_fp : 0,
        }
    }
    /// Returns the current stack pointer.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn sp(self: &Self) -> StackAddress {
        self.data.len() as StackAddress
    }
    /// Returns the current stack frame pointer.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn fp(self: &Self) -> StackAddress {
        self.fp
    }
    /// Sets the base frame pointer to the current stack length.
    pub fn begin(self: &mut Self) {
        self.base_fp = self.data.len() as StackAddress;
        self.fp = self.base_fp;
    }
    /// Resets the stack.
    pub fn reset(self: &mut Self) {
        self.fp = self.base_fp;
        self.data.truncate(self.base_fp as usize);
    }
    /// Truncates the stack to given size.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn truncate(self: &mut Self, size: StackAddress) {
        debug_assert!(size >= self.base_fp);
        self.data.truncate(size as usize);
    }
    /// Returns the current frame as slice.
    pub fn frame(self: &Self) -> &[u8] {
        &self.data[if self.sp() >= self.fp + 16 { self.fp + 16 } else { self.fp } as usize..]
    }
    /// Returns the entire stack as slice.
    pub fn data(self: &Self) -> &[u8] {
        &self.data[..]
    }
    /// Copies data within the stack.
    pub fn copy(self: &mut Self, from: StackAddress, to: StackAddress, num_bytes: StackAddress) {
        //debug_assert!((to + num_bytes <= from) || (to >= from + num_bytes));
        let from = from as usize;
        let to = to as usize;
        let num_bytes = num_bytes as usize;
        self.data.copy_within(from..from+num_bytes, to);
    }
    /// Extends stack with data from stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn extend(self: &mut Self, from: StackAddress, num_bytes: StackAddress) {
        debug_assert!(from + num_bytes <= self.data.len() as StackAddress);
        let from = from as usize;
        let num_bytes = num_bytes as usize;
        self.data.extend_from_within(from..from+num_bytes)
    }
    /// Extends stack with given data.
    pub fn extend_from(self: &mut Self, slice: &[u8]) {
        self.data.extend_from_slice(slice);
    }
    /// Extends stack with zeroed data.
    pub fn extend_zero(self: &mut Self, num_bytes: StackAddress) {
        self.data.resize(self.data.len() + num_bytes as usize, 0);
    }
}

/// Generic stack operations.
pub trait StackOp<T> {
    /// Push given value onto the stack.
    fn push(self: &mut Self, value: T);
    /// Pop the top value off the stack.
    fn pop(self: &mut Self) -> T;
    /// Store given value in the stack.
    fn store(self: &mut Self, pos: StackAddress, value: T);
    /// Load a value from the stack.
    fn load(self: &Self, pos: StackAddress) -> T;
    /// Load the top stack value.
    fn top(self: &Self) -> T;
}

/// Stack offset-computations to support stack relative operations.
pub trait StackOffsetOp {
    /// Offset given value by the current frame pointer
    fn offset_fp(self: &Self, offset: StackAddress) -> StackAddress;
    /// Offset given value by the current stack pointer
    fn offset_sp(self: &Self, offset: StackAddress) -> StackAddress;
}

/// Generic stack operations relative to the stack or frame pointer.
pub trait StackRelativeOp<T>: StackOffsetOp + StackOp<T> {
    /// Store given value in the stack relative to the frame pointer.
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn store_fp(self: &mut Self, offset: FrameAddress, value: T) {
        let pos = self.offset_fp(offset as StackAddress);
        self.store(pos, value);
    }
    /// Load a value from the stack relative to the frame pointer.
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn load_fp(self: &Self, offset: FrameAddress) -> T {
        let pos = self.offset_fp(offset as StackAddress);
        self.load(pos)
    }
    /// Store given value in the stack downwards relative to the stack pointer.
    fn store_sp(self: &mut Self, offset: StackAddress, value: T) {
        let pos = self.offset_sp(offset);
        self.store(pos, value);
    }
    /// Load a value from the stack downwards relative to the stack pointer.
    fn load_sp(self: &Self, offset: StackAddress) -> T {
        let pos = self.offset_sp(offset);
        self.load(pos)
    }
}

impl StackOffsetOp for Stack {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn offset_fp(self: &Self, offset: StackAddress) -> StackAddress {
        self.fp as StackAddress + offset
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn offset_sp(self: &Self, offset: StackAddress) -> StackAddress {
        self.data.len() as StackAddress - offset
    }
}

impl<T> StackRelativeOp<T> for Stack where Stack: StackOp<T> + StackOffsetOp { }

/// Implements Stack operations.
#[allow(unused_macros)]
macro_rules! impl_stack {

    // push operationsoperations

    (@push single, $type:tt, $stack: ident, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data.push(bytes[0]);
    };
    (@push multi, $type:tt, $stack: ident, $var:tt) => {
        $stack.data.extend_from_slice(&$var.to_ne_bytes());
    };

    // pop operations

    (@pop single, $type:tt, $stack: ident) => { {
        let bytes = [ $stack.data.pop().unwrap() ];
        $type::from_ne_bytes(bytes)
    } };
    (@pop multi, $type:tt, $stack: ident) => { {
        let stack_len = $stack.data.len();
        let start_pos = stack_len - size_of::<$type>();
        let bytes = &$stack.data[start_pos .. stack_len];
        let result = $type::from_ne_bytes(bytes.try_into().unwrap());
        $stack.data.truncate(start_pos);
        result
    } };

    // store operations

    (@store single, $type:tt, $stack: ident, $pos:expr, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data[$pos] = bytes[0];
    };
    (@store multi, $type:tt, $stack: ident, $pos:expr, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data[$pos .. $pos + size_of::<$type>()].copy_from_slice(&bytes);
    };

    // load operations

    (@load single, $type:tt, $stack: ident, $pos:expr) => { {
        $type::from_ne_bytes([ $stack.data[$pos] ])
    } };
    (@load multi, $type:tt, $stack: ident, $pos:expr) => { {
        let bytes = &$stack.data[$pos .. $pos + size_of::<$type>()];
        $type::from_ne_bytes(bytes.try_into().unwrap())
    } };

    // implement stack operations for Stack
    ($size:tt, $type:tt) => {
        impl StackOp<$type> for Stack {
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn push(self: &mut Self, value: $type) {
                impl_stack!(@push $size, $type, self, value);
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn pop(self: &mut Self) -> $type {
                impl_stack!(@pop $size, $type, self)
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn store(self: &mut Self, pos: StackAddress, value: $type) {
                impl_stack!(@store $size, $type, self, pos as usize, value);
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn load(self: &Self, pos: StackAddress) -> $type {
                impl_stack!(@load $size, $type, self, pos as usize)
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn top(self: &Self) -> $type {
                self.load(self.sp() - size_of::<$type>() as StackAddress)
            }
        }
    };
}

impl_stack!(single, u8);
impl_stack!(single, i8);
impl_stack!(multi, u16);
impl_stack!(multi, i16);
impl_stack!(multi, u32);
impl_stack!(multi, i32);
impl_stack!(multi, f32);
impl_stack!(multi, u64);
impl_stack!(multi, i64);
impl_stack!(multi, f64);
impl_stack!(multi, usize);
impl_stack!(multi, isize);
impl_stack!(multi, HeapRef);
