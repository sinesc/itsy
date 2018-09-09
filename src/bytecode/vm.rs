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
    pub(crate) consts   : Vec<i32>,
    pub(crate) stack    : Vec<i32>, // todo: i32 placeholder
    pub(crate) fp       : usize,
    pub(crate) pc       : usize,
    pub(crate) reg_cond : bool,
    pub(crate) mem      : Vec<i32>,
    pub(crate) state    : VMState,
    start               : usize,
}

impl VM {
    /// Create a new VM instance.
    pub fn new(data: Vec<u8>, start: u32) -> Self {
        VM {
            code        : data,
            consts      : Vec::with_capacity(256),
            stack       : Vec::with_capacity(256),
            fp          : 0,
            pc          : start as usize,
            reg_cond    : false,
            mem         : Vec::with_capacity(256),
            state       : VMState::Continue,
            start       : start as usize,
        }
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.truncate(0);
        self.mem.truncate(0);
        self.fp = 0;
        self.pc = self.start;
        self.reg_cond = false;
        self.state = VMState::Continue;
    }

    /// Pushes a constant value into the VMs constant pool and returns the index.
    pub fn push_const(self: &mut Self, value: i32) -> usize {
        let pos = self.consts.len();
        self.consts.push(value);
        pos
    }

    // Disassembles the bytecode and returns it as a string.
    pub fn dump_code(self: &mut Self) -> String { // todo: should not have to require mut
        let pc = self.pc;
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
        format!("{:?}", &self.stack[self.fp..])
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self) {
        while self.state == VMState::Continue {
            self.exec()
        }
    }
}

use std::io;
use std::io::Read;

impl Read for VM {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let n = Read::read(&mut &self.code[self.pc..], buf)?;
        self.pc += n;
        Ok(n)
    }
    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        let n = buf.len();
        Read::read_exact(&mut &self.code[self.pc..], buf)?;
        self.pc += n;
        Ok(())
    }
}