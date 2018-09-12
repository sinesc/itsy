//! Bytecode buffer and writer.

use std::io::{self, Write, Seek, SeekFrom};
use bytecode::Program;

/// Bytecode buffer and writer.
#[derive(Debug)]
pub struct Writer {
    pub(crate) program: Program,
    pub(crate) position: u32,
}

impl Writer {
    /// Creates a new writer instance.
    pub fn new() -> Self {
        Writer {
            program: Vec::new(),
            position: 0,
        }
    }
    /// Returns the current length of the program.
    pub fn len(self: &Self) -> u32 {
        self.program.len() as u32
    }
    /// Returns the current write position.
    pub fn position(self: &Self) -> u32 {
        self.position
    }
    /// Sets the current write position.
    pub fn set_position(self: &mut Self, new_position: u32) {
        self.position = new_position;
    }
    /// Converts the writer into the underlying buffer.
    pub fn into_program(self: Self) -> Program {
        self.program
    }
    /// Overwrites the program at given position and returns to the previous position afterwards.
    pub fn overwrite<T>(self: &mut Self, position: u32, mut write_fn: T) -> u32
         where T: FnMut(&mut Self) -> u32
    {
        let original_position = self.position;
        self.position = position;
        let result = write_fn(self);
        self.position = original_position;
        result
    }
}

impl Write for Writer {
    fn write(self: &mut Self, buf: &[u8]) -> io::Result<usize> {
        let buf_len = buf.len() as u32;
        if self.position == self.len() {
            // append
            self.program.extend_from_slice(buf);
        } else {
            // overwrite
            let end = ::std::cmp::min(self.position + buf_len, self.len());
            self.program.splice(self.position as usize .. end as usize, buf.iter().cloned());
        }
        self.position += buf_len;
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
            SeekFrom::Start(pos) => pos as u32,
            SeekFrom::End(pos) => (self.program.len() as i64 + pos) as u32,
            SeekFrom::Current(pos) => (self.position as i64 + pos) as u32,
        };
        Ok(self.position as u64)
    }
}