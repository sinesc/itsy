use std::ops::{Index, IndexMut};
use std::mem::size_of;
use std::convert::TryInto;

use crate::util::{HeapRef, HeapSlice};

/// A stack holding temporary bytecode operation results and inputs.
#[derive(Debug)]
pub struct Stack {
    /// Raw stack data.
    data            : Vec<u8>,
    /// Frames
    frames          : Vec<(u32, u32)>,
    /// Current frame pointer.
    pub(crate) fp   : u32,
    /// Base frame pointer, pointing to the first byte after constant data.
    base_fp         : u32,
}

impl Stack {
    /// Creates a new VM value stack.
    pub fn new() -> Self {
        Stack {
            data    : Vec::new(),
            frames  : Vec::new(),
            fp      : 0,
            base_fp : 0,
        }
    }
    pub fn push_frame(self: &mut Self, pc: u32) {
        self.frames.push((self.fp, pc));
    }
    pub fn pop_frame(self: &mut Self) -> u32 {
        let (fp, pc) = self.frames.pop().expect("Pop on empty frame-stack");
        self.fp = fp;
        pc
    }
    /// Returns the current stack pointer.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn sp(self: &Self) -> u32 {
        self.data.len() as u32
    }
    /// Returns the current stack frame pointer.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn fp(self: &Self) -> u32 {
        self.fp
    }
    /// Sets the base frame pointer to the current stack length.
    pub fn begin(self: &mut Self) {
        self.base_fp = self.data.len() as u32;
        self.fp = self.base_fp;
    }
    /// Resets the stack.
    pub fn reset(self: &mut Self) {
        self.fp = self.base_fp;
        self.data.truncate(self.base_fp as usize);
    }
    /// Truncates the stack to given size.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn truncate(self: &mut Self, size: u32) {
        debug_assert!(size >= self.base_fp);
        self.data.truncate(size as usize);
    }
    /// Returns the current frame as slice.
    pub fn frame(self: &Self) -> &[u8] {
        &self.data[self.fp as usize..]
    }
    /// Returns the current stack as slice.
    pub fn data(self: &Self) -> &[u8] {
        &self.data[self.base_fp as usize..]
    }
    /// Returns the stacks const-pool as slice.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn consts(self: &Self) -> &[u8] {
        &self.data[..self.base_fp as usize]
    }
    /// Copies data within the stack.
    pub fn copy(self: &mut Self, from: u32, to: u32, num_bytes: u32) {
        //debug_assert!((to + num_bytes <= from) || (to >= from + num_bytes));
        let from = from as usize;
        let to = to as usize;
        let num_bytes = num_bytes as usize;
        self.data.copy_within(from..from+num_bytes, to);
    }
    /// Extends stack with data from stack.
    pub fn extend(self: &mut Self, from: u32, num_bytes: u32) { // TODO: check that this isn't as slow as it looks, used quite a lot in construction
        debug_assert!(from + num_bytes <= self.data.len() as u32);
        let from = from as usize;
        let num_bytes = num_bytes as usize;
        let mut data: Vec<_> = self.data[from..from+num_bytes].into();
        self.data.append(&mut data);
    }
    /// Extends stack with given data.
    pub fn extend_from(self: &mut Self, slice: &[u8]) {
        self.data.extend_from_slice(slice);
    }
    pub fn extend_zero(self: &mut Self, num_bytes: u32) {
        self.data.resize(self.data.len() + num_bytes as usize, 0);
    }
}

impl Index<u32> for Stack {
    type Output = u8;

    #[inline]
    fn index(&self, index: u32) -> &Self::Output {
        Index::index(&*self.data, index as usize)
    }
}

impl IndexMut<u32> for Stack {
    #[inline]
    fn index_mut(&mut self, index: u32) -> &mut Self::Output {
        IndexMut::index_mut(&mut *self.data, index as usize)
    }
}

/// Trait for generic stack operations.
pub trait StackOp<T> {
    /// Push given value onto the stack.
    fn push(self: &mut Self, value: T);
    /// Pop the top value off the stack.
    fn pop(self: &mut Self) -> T;
    /// Store given value in the stack.
    fn store(self: &mut Self, pos: u32, value: T);
    /// Load a value from the stack.
    fn load(self: &Self, pos: u32) -> T;
    /// Load the top stack value.
    fn top(self: &Self) -> T;
}

/// Trait for stack frame pointer.
pub trait StackOffset<T> {
    /// Offset given value by the current frame pointer
    fn offset_fp(self: &Self, offset: i32) -> u32;
    /// Offset given value by the current stack pointer
    fn offset_sp(self: &Self, offset: i32) -> u32;
}

/// Trait for generic stack operations relative to the frame pointer.
pub trait StackOffsetOp<T>: StackOffset<T> + StackOp<T> {
    /// Store given value in the stack relative to the frame pointer.
    fn store_fp(self: &mut Self, offset: i32, value: T) {
        let pos = self.offset_fp(offset);
        self.store(pos, value);
    }
    /// Load a value from the stack relative to the frame pointer.
    fn load_fp(self: &Self, offset: i32) -> T {
        let pos = self.offset_fp(offset);
        self.load(pos)
    }
    /// Store given value in the stack relative to the stack pointer.
    fn store_sp(self: &mut Self, offset: i32, value: T) {
        let pos = self.offset_sp(offset);
        self.store(pos, value);
    }
    /// Load a value from the stack relative to the stack pointer.
    fn load_sp(self: &Self, offset: i32) -> T {
        let pos = self.offset_sp(offset);
        self.load(pos)
    }
}

impl<T> StackOffset<T> for Stack {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn offset_fp(self: &Self, offset: i32) -> u32 {
        (self.fp as i64 + offset as i64) as u32
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn offset_sp(self: &Self, offset: i32) -> u32 {
        (self.data.len() as i64 + offset as i64) as u32
    }
}

impl<T> StackOffsetOp<T> for Stack where Stack: StackOp<T> + StackOffset<T> { }

/// Implements Stack operations.
#[allow(unused_macros)]
macro_rules! impl_stack {

    // push operationsoperations
/*
    (@push 32bit, $type:tt, $stack: ident, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data.push(bytes[0]);
        $stack.data.push(bytes[1]);
        $stack.data.push(bytes[2]);
        $stack.data.push(bytes[3]);
    };
    (@push 16bit, $type:tt, $stack: ident, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data.push(bytes[0]);
        $stack.data.push(bytes[1]);
    };
*/
    (@push 8bit, $type:tt, $stack: ident, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data.push(bytes[0]);
    };
    (@push $size:tt, $type:tt, $stack: ident, $var:tt) => {
        $stack.data.extend_from_slice(&$var.to_ne_bytes());
    };

    // pop operations
/*
    (@pop 32bit, $type:tt, $stack: ident) => { {
        let d = $stack.data.pop().unwrap();
        let c = $stack.data.pop().unwrap();
        let b = $stack.data.pop().unwrap();
        let a = $stack.data.pop().unwrap();
        $type::from_ne_bytes([ a, b, c, d ])
    } };
    (@pop 16bit, $type:tt, $stack: ident) => { {
        let b = $stack.data.pop().unwrap();
        let a = $stack.data.pop().unwrap();
        $type::from_ne_bytes([ a, b ])
    } };
*/
    (@pop 8bit, $type:tt, $stack: ident) => { {
        let bytes = [ $stack.data.pop().unwrap() ];
        $type::from_ne_bytes(bytes)
    } };
    (@pop $size:tt, $type:tt, $stack: ident) => { {
        let stack_len = $stack.data.len();
        let start_pos = stack_len - size_of::<$type>();
        let bytes = &$stack.data[start_pos .. stack_len];
        let result = $type::from_ne_bytes(bytes.try_into().unwrap());
        $stack.data.truncate(start_pos);
        result
    } };

    // store operations
/*
    (@store 32bit, $type:tt, $stack: ident, $pos:expr, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data[$pos] = bytes[0];
        $stack.data[$pos + 1] = bytes[1];
        $stack.data[$pos + 2] = bytes[2];
        $stack.data[$pos + 3] = bytes[3];
    };
    (@store 16bit, $type:tt, $stack: ident, $pos:expr, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data[$pos] = bytes[0];
        $stack.data[$pos + 1] = bytes[1];
    };
*/
    (@store 8bit, $type:tt, $stack: ident, $pos:expr, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data[$pos] = bytes[0];
    };
    (@store $size:tt, $type:tt, $stack: ident, $pos:expr, $var:tt) => {
        let bytes = $var.to_ne_bytes();
        $stack.data[$pos .. $pos + size_of::<$type>()].copy_from_slice(&bytes);
    };

    // load operations
/*
    (@load 32bit, $type:tt, $stack: ident, $pos:expr) => { {
        $type::from_ne_bytes([ $stack.data[$pos], $stack.data[$pos + 1], $stack.data[$pos + 2], $stack.data[$pos + 3] ])
    } };
    (@load 16bit, $type:tt, $stack: ident, $pos:expr) => { {
        $type::from_ne_bytes([ $stack.data[$pos], $stack.data[$pos + 1] ])
    } };
*/
    (@load 8bit, $type:tt, $stack: ident, $pos:expr) => { {
        $type::from_ne_bytes([ $stack.data[$pos] ])
    } };
    (@load $size:tt, $type:tt, $stack: ident, $pos:expr) => { {
        let bytes = &$stack.data[$pos .. $pos + size_of::<$type>()];
        $type::from_ne_bytes(bytes.try_into().unwrap())
    } };

    // implement stack operations for Stack
    ($target:ident, $size:tt, $type:tt) => {
        impl StackOp<$type> for $target {
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn push(self: &mut Self, value: $type) {
                impl_stack!(@push $size, $type, self, value);
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn pop(self: &mut Self) -> $type {
                impl_stack!(@pop $size, $type, self)
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn store(self: &mut Self, pos: u32, value: $type) {
                impl_stack!(@store $size, $type, self, pos as usize, value);
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn load(self: &Self, pos: u32) -> $type {
                impl_stack!(@load $size, $type, self, pos as usize)
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn top(self: &Self) -> $type {
                self.load(self.sp() - size_of::<$type>() as u32)
            }
        }
    };
}

impl_stack!(Stack, 8bit, u8);
impl_stack!(Stack, 8bit, i8);
impl_stack!(Stack, 16bit, u16);
impl_stack!(Stack, 16bit, i16);
impl_stack!(Stack, 32bit, u32);
impl_stack!(Stack, 32bit, i32);
impl_stack!(Stack, 32bit, f32);
impl_stack!(Stack, 64bit, u64);
impl_stack!(Stack, 64bit, i64);
impl_stack!(Stack, 64bit, f64);
impl_stack!(Stack, 128bit, u128);
impl_stack!(Stack, 128bit, i128);

impl StackOp<HeapRef> for Stack {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn push(self: &mut Self, value: HeapRef) {
        self.push(value.index);
        self.push(value.offset);
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn pop(self: &mut Self) -> HeapRef {
        HeapRef {
            offset: self.pop(),
            index: self.pop(),
        }
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn store(self: &mut Self, pos: u32, value: HeapRef) {
        self.store(value.index, pos);
        self.store(value.offset, pos + 4);
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn load(self: &Self, pos: u32) -> HeapRef {
        HeapRef {
            index: self.load(pos),
            offset: self.load(pos + 4),
        }
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn top(self: &Self) -> HeapRef {
        self.load(self.sp() - size_of::<HeapRef>() as u32)
    }
}

impl StackOp<HeapSlice> for Stack {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn push(self: &mut Self, value: HeapSlice) {
        self.push(value.len);
        self.push(value.index);
        self.push(value.offset);
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn pop(self: &mut Self) -> HeapSlice {
        HeapSlice {
            offset: self.pop(),
            index: self.pop(),
            len: self.pop(),
        }
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn store(self: &mut Self, pos: u32, value: HeapSlice) {
        self.store(value.len, pos);
        self.store(value.index, pos + 4);
        self.store(value.offset, pos + 8);
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn load(self: &Self, pos: u32) -> HeapSlice {
        HeapSlice {
            len: self.load(pos),
            index: self.load(pos + 4),
            offset: self.load(pos + 8),
        }
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn top(self: &Self) -> HeapSlice {
        self.load(self.sp() - size_of::<HeapSlice>() as u32)
    }
}