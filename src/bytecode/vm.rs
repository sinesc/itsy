
use std::io::Cursor;

/// A virtual machine for running itsy bytecode.
#[derive(Debug)]
pub struct VM {
    pub code: Cursor<Vec<u8>>,
}

impl VM {
    pub fn new(data: Vec<u8>) -> VM {
        VM { code: Cursor::new(data) }
    }
}