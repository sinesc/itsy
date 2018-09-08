use std::io::Cursor;
use std::fmt;

use bytecode;

/// A virtual machine for running itsy bytecode.
#[derive(Debug)]
pub struct VM {
    pub code: Cursor<Vec<u8>>,
}

impl VM {
    /// Create a new instance.
    pub fn new(data: Vec<u8>) -> VM {
        VM { code: Cursor::new(data) }
    }
    /// Return bytecode in somewhat human readable form.
    pub fn disassemble(self: &Self) -> String {
        format!("{:?}", VMDump(self))
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
