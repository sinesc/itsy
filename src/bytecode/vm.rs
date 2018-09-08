use std::io::Cursor;
use std::fmt;

/// A virtual machine for running itsy bytecode.
#[derive(Debug)]
pub struct VM {
    pub(crate) code     : Cursor<Vec<u8>>,
    pub(crate) stack    : Vec<i32>, // todo: i32 placeholder
    pub(crate) consts   : Vec<i32>,
    pub(crate) exit     : bool,
}

impl VM {
    /// Create a new VM instance.
    pub fn new(data: Vec<u8>) -> VM {
        VM {
            code    : Cursor::new(data) ,
            stack   : Vec::with_capacity(256),
            consts  : Vec::with_capacity(256),
            exit    : false,
        }
    }

    /// Pushes a constant value into the VMs constant pool and returns the index.
    pub fn push_const(self: &mut Self, value: i32) -> usize {
        let pos = self.consts.len();
        self.consts.push(value);
        pos
    }

    /// Return bytecode in somewhat human readable form.
    pub fn disassemble(self: &Self) -> String {
        format!("{:?}", VMDump(self))
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self) {
        while !self.exit {
            self.exec()
        }
    }
}

/// Helper struct to be able to use formatters without being able to construct them.
struct VMDump<'a>(&'a VM);

impl<'a> fmt::Debug for VMDump<'a> {
    fn fmt(self: &Self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut code = ::std::io::Cursor::new(self.0.code.get_ref());
        while let Some(result) = VM::fmt_instruction(&mut code, f) {
            if result.is_err() { return result; }
        }
        Ok(())
    }
}
