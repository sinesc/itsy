//! A virtual machine for running Itsy bytecode.

// todo: remove
#![allow(dead_code)]

use std::io::{self, Read};
use crate::bytecode::*;

/// Current state of the vm, checked after each instruction.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VMState {
    /// The VM will continue to execute white it is in this state.
    Continue,
    /// Yield after current instruction. The program can be resumed after a yield.
    Yield,
    /// Terminate after current instruction. The program state will be reset.
    Terminate,
    /// A runtime error was encountered.
    RuntimeError,
}

/// A virtual machine for running Itsy bytecode.
#[derive(Debug)]
pub struct VM<T> where T: crate::ExternRust<T> {
    pub(crate) program  : Program<T>,
    pub stack           : Stack,
    pub heap            : Heap,
    pub(crate) pc       : u32,
    pub(crate) state    : VMState,
}

/// Public VM methods.
impl<T> VM<T> where T: crate::ExternRust<T> {
    /// Create a new VM instance with the given Program.
    pub fn new(program: Program<T>) -> Self {
        VM {
            program     : program,
            stack       : Stack::new(),
            heap        : Heap::new(),
            pc          : 0,
            state       : VMState::Continue,
        }
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.reset();
        self.heap.reset();
        self.pc = 0;
        self.state = VMState::Continue;
    }

    /// Disassembles the bytecode and returns it as a string.
    pub fn dump_program(self: &mut Self) -> String { // todo: should not have to require mut
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

    /// Disassembles the current bytecode instruction and returns it as a string.
    pub fn dump_instruction(self: &mut Self) -> Option<String> {// todo: should not have to require mut
        let pc = self.pc;
        let result = self.format_instruction();
        self.pc = pc;
        result
    }

    /// Returns the current stack as a string.
    pub fn dump_stack(self: &Self) -> String {
        format!("{:?}", self.stack)
    }

    /// Returns the current stack-frame as a string.
    pub fn dump_frame(self: &Self) -> String {
        format!("{:?}", &self.stack.frame())
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self) {
        while self.state == VMState::Continue {
            self.exec();
        }
        if self.state == VMState::Terminate {
            self.reset();
        }
    }

    /// Returns the current VM state.
    pub fn state(self: &Self) -> VMState {
        self.state
    }
}

impl<T> Read for VM<T> where T: crate::ExternRust<T> {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let n = Read::read(&mut &self.program.instructions[self.pc as usize..], buf)?;
        self.pc += n as u32;
        Ok(n)
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        let n = buf.len();
        Read::read_exact(&mut &self.program.instructions[self.pc as usize..], buf)?;
        self.pc += n as u32;
        Ok(())
    }
}