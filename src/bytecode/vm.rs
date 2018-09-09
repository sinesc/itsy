use std::io::Cursor;

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
    pub(crate) code     : Cursor<Vec<u8>>,
    pub(crate) consts   : Vec<i32>,
    pub(crate) stack    : Vec<i32>, // todo: i32 placeholder
    pub(crate) fp       : usize,
    pub(crate) mem      : Vec<i32>,
    pub(crate) state    : VMState,
    start               : u32,
}

impl VM {
    /// Create a new VM instance.
    pub fn new(data: Vec<u8>, start: u32) -> VM {
        let mut cursor = Cursor::new(data);
        cursor.set_position(start as u64);
        VM {
            code    : cursor,
            consts  : Vec::with_capacity(256),
            stack   : Vec::with_capacity(256),
            fp      : 0,
            mem     : Vec::with_capacity(256),
            state   : VMState::Continue,
            start   : start,
        }
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.code.set_position(self.start as u64);
        self.stack.truncate(0);
        self.mem.truncate(0);
        self.fp = 0;
        self.state = VMState::Continue;
    }

    /// Pushes a constant value into the VMs constant pool and returns the index.
    pub fn push_const(self: &mut Self, value: i32) -> usize {
        let pos = self.consts.len();
        self.consts.push(value);
        pos
    }

    // Disassembles the bytecode and returns it as a string.
    pub fn dump_code(self: &Self) -> String {
        let mut result = "".to_string();
        let mut cursor = Cursor::new(self.code.get_ref());
        while let Some(instruction) = Self::format_instruction(&mut cursor) {
            result.push_str(&instruction);
            result.push_str("\n");
        }
        return result;
    }

    // Disassembles the current bytecode instruction and returns it as a string.
    pub fn dump_instruction(self: &Self) -> Option<String> {
        // todo: this is a pretty lame to way to keep cursor position
        let mut code = ::std::io::Cursor::new(self.code.get_ref());
        code.set_position(self.code.position());
        Self::format_instruction(&mut code)
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
