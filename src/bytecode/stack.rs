use std::ops::{Index, IndexMut};
use std::mem::transmute;
use crate::bytecode::Value;

/// A stack holding temporary bytecode operation results and inputs.
#[derive(Debug)]
pub struct Stack {
    data            : Vec<Value>,
    pub(crate) fp   : u32,
}

impl Stack {
    /// Creates a new VM value stack.
    pub fn new() -> Self {
        Stack {
            data: Vec::with_capacity(256),
            fp  : 0,
        }
    }
    /// Returns the current stack pointer.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn sp(self: &Self) -> u32 {
        self.data.len() as u32
    }
    /// Resets the stack.
    pub fn reset(self: &mut Self) {
        self.fp = 0;
        self.data.truncate(0);
    }
    /// Truncates the stack to given size.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub fn truncate(self: &mut Self, size: u32) {
        self.data.truncate(size as usize);
    }
    /// Returns the current frame as slice
    pub fn frame(self: &Self) -> &[i32] {
        &self.data[self.fp as usize..]
    }
}

impl Index<u32> for Stack {
    type Output = i32;

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
pub trait StackFp<T> {
    /// Offset given value by the current frame pointer
    fn offset_fp(self: &Self, offset: i32) -> u32;
}

/// Trait for generic stack operations relative to the frame pointer.
pub trait StackOpFp<T>: StackFp<T> + StackOp<T> {
    /// Store given value in the stack relative to the frame pointer.
    fn store_fp(self: &mut Self, offset: i32, value: T) {
        let pos = self.offset_fp(offset);
        self.store(pos, value);
    }
    /// Load a value from the stack relative to the frame pointer.
    fn load_fp(self: &mut Self, offset: i32) -> T {
        let pos = self.offset_fp(offset);
        self.load(pos)
    }
}

impl<T> StackFp<T> for Stack {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn offset_fp(self: &Self, offset: i32) -> u32 {
        (self.fp as i64 + offset as i64) as u32
    }
}

impl<T> StackOpFp<T> for Stack where Stack: StackOp<T> + StackFp<T> { }

/// Implements Stack operations.
#[allow(unused_macros)]
macro_rules! impl_stack {

    // push operations
    (@push large, $type:tt, $stack: ident, $var:ident) => { {
        let (low, high): (u32, u32) = unsafe { transmute($var) };
        impl_stack!(@push normal, u32, $stack, low);
        impl_stack!(@push normal, u32, $stack, high);
    } };
    (@push $size:tt, $type:tt, $stack: ident, $var:ident) => { $stack.data.push( impl_convert!($size, Value, $var) ); };

    // store operations
    (@store large, $type:tt, $stack: ident, $pos:expr, $var:ident) => { {
        let (low, high): (u32, u32) = unsafe { transmute($var) };
        impl_stack!(@store normal, u32, $stack, $pos, low);
        impl_stack!(@store normal, u32, $stack, $pos + 1, high);
    } };
    (@store $size:tt, $type:tt, $stack: ident, $pos:expr, $var:ident) => {
        *$stack.data.get_mut($pos as usize).expect("Stack bounds exceeded") = impl_convert!($size, Value, $var);
    };

    // pop operations
    (@pop large, $type:tt, $stack: ident) => { {
        let high: u32 = impl_stack!(@pop normal, u32, $stack);
        let low: u32 = impl_stack!(@pop normal, u32, $stack);
        unsafe { transmute((low, high)) }
    } };
    (@pop $size:tt, $type:tt, $stack: ident) => { impl_convert!($size, $type, $stack.data.pop().expect("Stack underflow")) };

    // load operations
    (@load large, $type:tt, $stack: ident, $pos:expr) => { {
        let low: u32 = impl_stack!(@load normal, u32, $stack, $pos);
        let high: u32 = impl_stack!(@load normal, u32, $stack, $pos + 1);
        unsafe { transmute((low, high)) }
    } };
    (@load $size:tt, $type:tt, $stack: ident, $pos:expr) => {
        impl_convert!($size, $type, *$stack.data.get($pos as usize).expect("Stack bounds exceeded"))
    };

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
                impl_stack!(@store $size, $type, self, pos, value);
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn load(self: &Self, pos: u32) -> $type {
                impl_stack!(@load $size, $type, self, pos)
            }
            #[cfg_attr(not(debug_assertions), inline(always))]
            fn top(self: &Self) -> $type {
                self.load(self.sp() - 1)
            }
        }
    };
}

impl_stack!(Stack, small, bool);
impl_stack!(Stack, small, u8);
impl_stack!(Stack, small, i8);
impl_stack!(Stack, small, u16);
impl_stack!(Stack, small, i16);
impl_stack!(Stack, normal, u32);
impl_stack!(Stack, normal, i32);
impl_stack!(Stack, normal, f32);
impl_stack!(Stack, large, u64);
impl_stack!(Stack, large, i64);
impl_stack!(Stack, large, f64);
