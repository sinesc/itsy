//! Bytecode buffer and writer.

use std::io::{self, Write, Seek, SeekFrom};
use bytecode::Program;

/// Bytecode buffer and writer.
#[derive(Debug)]
pub struct Writer {
    pub(crate) program: Program,
    pub(crate) position: usize,
}

impl Writer {
    /// Creates a new writer instance.
    pub fn new() -> Self {
        Writer {
            program: Vec::new(),
            position: 0,
        }
    }
    /// Converts the writer into the underlying buffer.
    pub fn into_program(self: Self) -> Program {
        self.program
    }
}

impl Write for Writer {
    fn write(self: &mut Self, buf: &[u8]) -> io::Result<usize> {
        if self.position == self.program.len() {
            // append
            self.program.extend_from_slice(buf);
        } else {
            // overwrite
            let end = ::std::cmp::min(self.position + buf.len(), self.program.len());
            self.program.splice(self.position..end, buf.iter().cloned());
        }
        self.position += buf.len();
        Ok(buf.len())
    }
    fn write_all(self: &mut Self, buf: &[u8]) -> io::Result<()> {
        self.write(buf).unwrap();
        Ok(())
    }
    fn flush(self: &mut Self) -> io::Result<()> {
        Ok(())
    }
}

impl Seek for Writer {
    fn seek(self: &mut Self, pos: SeekFrom) -> io::Result<u64> {
        self.position = match pos {
            SeekFrom::Start(pos) => pos as usize,
            SeekFrom::End(pos) => (self.program.len() as i64 + pos) as usize,
            SeekFrom::Current(pos) => (self.position as i64 + pos) as usize,
        };
        Ok(self.position as u64)
    }
}