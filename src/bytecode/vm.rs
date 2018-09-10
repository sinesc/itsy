// todo: remove
#![allow(dead_code)]

use std::io;
use std::io::Read;
use bytecode::utils::*;
use bytecode::Value;

/// Current state of the vm, checked after each instruction.
#[derive(Debug, PartialEq)]
pub enum VMState {
    Continue,
    Yield,
    Terminate,
    RuntimeError,
}

/// A virtual machine for running itsy bytecode.
#[derive(Debug)]
pub struct VM {
    pub(crate) code     : Vec<u8>,
    pub(crate) consts   : Vec<Value>,
    stack               : Vec<Value>,
    pub(crate) fp       : u32,
    pub(crate) pc       : u32,
    pub(crate) mem      : Vec<Value>,
    pub(crate) state    : VMState,
    start               : u32,
}

/// Public VM methods.
impl VM {
    /// Create a new VM instance.
    pub fn new(data: Vec<u8>, start: u32) -> Self {
        VM {
            code        : data,
            consts      : Vec::with_capacity(256),
            stack       : Vec::with_capacity(256),
            fp          : 0,
            pc          : start,
            mem         : Vec::with_capacity(256),
            state       : VMState::Continue,
            start       : start,
        }
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.truncate(0);
        self.mem.truncate(0);
        self.fp = 0;
        self.pc = self.start;
        self.state = VMState::Continue;
    }

    /// Pushes a constant value into the VMs constant pool and returns the index.
    pub fn add_const(self: &mut Self, value: i32) -> usize {
        let pos = self.consts.len();
        self.consts.push(value);
        pos
    }

    // Disassembles the bytecode and returns it as a string.
    pub fn dump_code(self: &mut Self) -> String { // todo: should not have to require mut
        let pc = self.pc;
        self.pc = 0;
        let mut result = "".to_string();
        while let Some(instruction) = self.format_instruction() {
            result.push_str(&instruction);
            result.push_str("\n");
        }
        self.pc = pc;
        return result;
    }

    // Disassembles the current bytecode instruction and returns it as a string.
    pub fn dump_instruction(self: &mut Self) -> Option<String> {
        let pc = self.pc;
        let result = self.format_instruction();
        self.pc = pc;
        result
    }

    // Returns the current stack as a string.
    pub fn dump_stack(self: &Self) -> String {
        format!("{:?}", self.stack)
    }

    // Returns the current stack as a string.
    pub fn dump_frame(self: &Self) -> String {
        format!("{:?}", &self.stack[self.fp as usize..])
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self) {
        while self.state == VMState::Continue {
            self.exec();
        }
    }
}

/// Bytecode support methods.
impl VM {
    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Current stack pointer.
    pub(crate) fn sp(self: &mut Self) -> u32 {
        self.stack.len() as u32
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Truncate stack at given length.
    pub(crate) fn truncate(self: &mut Self, new_len: u32) {
        self.stack.truncate(new_len as usize);
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Peek stack top value.
    pub(crate) fn top(self: &mut Self) -> i32 {
        vali32(*self.stack.last().expect("Stack is empty."))
    }
    /// Peek stack top value.
    pub(crate) fn topu(self: &mut Self) -> u32 {
        valu32(*self.stack.last().expect("Stack is empty."))
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Peek stack value relative to frame pointer.
    pub(crate) fn peek(self: &mut Self, position: i32) -> i32 {
        self.stack[ ( (self.fp as isize) + (position as isize) ) as usize ]
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Peek stack value relative to frame pointer.
    pub(crate) fn peeku(self: &mut Self, position: i32) -> u32 {
        valu32(self.peek(position))
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Store given value at given position relative to frame pointer.
    pub(crate) fn storei(self: &mut Self, position: i32, value: i32) {
        self.stack[ ( (self.fp as isize) + (position as isize) ) as usize ] = i32val(value);
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Store given value at given position relative to frame pointer.
    pub(crate) fn storeu(self: &mut Self, position: i32, value: u32) {
        self.stack[ ( (self.fp as isize) + (position as isize) ) as usize ] = u32val(value);
    }

    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Pops a value from the stack.
    pub(crate) fn pop(self: &mut Self) -> i32 {
        vali32(self.stack.pop().expect("Stack underflow."))
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Pops a value from the stack.
    pub(crate) fn popu(self: &mut Self) -> u32 {
        valu32(self.stack.pop().expect("Stack underflow."))
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    /// Pops a value from the stack.
    pub(crate) fn popu8(self: &mut Self) -> u8 {
        valu8(self.stack.pop().expect("Stack underflow."))
    }

    /// Pushes given i8 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn pushs8(self: &mut Self, value: i8) {
        self.stack.push(i8val(value));
    }
    /// Pushes given u8 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn pushu8(self: &mut Self, value: u8) {
        self.stack.push(u8val(value));
    }
    /// Pushes given i16 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn pushs16(self: &mut Self, value: i16) {
        self.stack.push(i16val(value));
    }
    /// Pushes given u16 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn pushu16(self: &mut Self, value: u16) {
        self.stack.push(u16val(value));
    }
    /// Pushes given i32 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn push(self: &mut Self, value: i32) {
        self.stack.push(i32val(value));
    }
    /// Pushes given u32 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn pushu(self: &mut Self, value: u32) {
        self.stack.push(u32val(value));
    }
    /// Pushes given f32 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn pushf(self: &mut Self, value: f32) {
        self.stack.push(f32val(value));
    }
    /// Pushes given i64 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn pushs64(self: &mut Self, value: i64) {
        let (value1, value2) = i64val(value);
        self.stack.push(value1);
        self.stack.push(value2);
    }
    /// Pushes given u64 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn pushu64(self: &mut Self, value: u64) {
        let (value1, value2) = u64val(value);
        self.stack.push(value1);
        self.stack.push(value2);
    }
    /// Pushes given f64 onto the stack.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn pushf64(self: &mut Self, value: f64) {
        let (value1, value2) = f64val(value);
        self.stack.push(value1);
        self.stack.push(value2);
    }
}

impl Read for VM {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let n = Read::read(&mut &self.code[self.pc as usize..], buf)?;
        self.pc += n as u32;
        Ok(n)
    }
    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        let n = buf.len();
        Read::read_exact(&mut &self.code[self.pc as usize..], buf)?;
        self.pc += n as u32;
        Ok(())
    }
}