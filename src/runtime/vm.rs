//! A virtual machine for running Itsy bytecode.

use crate::bytecode::Program;
use crate::runtime::*;

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
pub struct VM<T, U> where T: crate::runtime::VMFunc<T> {
    context_type        : std::marker::PhantomData<U>,
    func_type           : std::marker::PhantomData<T>,
    pub(crate) instructions  : Vec<u8>,
    pub(crate) pc       : u32,
    pub(crate) state    : VMState,
    pub stack           : Stack,
    pub heap            : Heap,
    pub(crate) tmp      : Stack,
}

/// Public VM methods.
impl<T, U> VM<T, U> where T: crate::runtime::VMFunc<T>+crate::runtime::VMData<T, U> {
    /// Create a new VM instance with the given Program.
    pub fn new(program: Program<T>) -> Self {
        let Program { instructions, consts, .. } = program;
        let mut heap = Heap::new();
        heap.alloc(consts); // FIXME: need type table for the pool so that it can be converted to correct endianess during heap upload
        VM {
            context_type: std::marker::PhantomData,
            func_type   : std::marker::PhantomData,
            instructions: instructions,
            pc          : 0,
            state       : VMState::Continue,
            stack       : Stack::new(),
            heap        : heap,
            tmp         : Stack::new(),
        }
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.reset();
        self.heap.reset();
        self.tmp.reset();
        self.pc = 0;
        self.state = VMState::Continue;
    }

    /// Disassembles the bytecode and returns it as a string.
    pub fn format_program(self: &Self) -> String {
        let mut position = 0;
        let mut result = "".to_string();
        while let Some((instruction, next_position)) = self.describe_instruction(position) {
            result.push_str(&instruction);
            result.push_str("\n");
            position = next_position;
        }
        return result;
    }

    /// Disassembles the current bytecode instruction and returns it as a string.
    pub fn format_instruction(self: &Self) -> Option<String> {
        self.describe_instruction(self.pc).map(|result| result.0)
    }

    /// Returns the current stack as a string.
    pub fn format_stack(self: &Self) -> String {
        format!("{:?}", self.stack)
    }

    /// Returns the current stack-frame as a string.
    pub fn format_frame(self: &Self) -> String {
        format!("{:?}", &self.stack.frame())
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self, context: &mut U) -> &mut Self {
        while self.state == VMState::Continue {
            self.exec(context);
        }
        if self.state == VMState::Terminate {
            self.reset();
        }
        self
    }

    /// Returns the current VM state.
    pub fn state(self: &Self) -> VMState {
        self.state
    }
}