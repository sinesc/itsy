
/// Bytecode buffer and writer.
#[derive(Debug)]
pub struct Writer {
    pub(crate) code: Vec<u8>,
}

impl Writer {
    /// Creates a new writer instance.
    pub fn new() -> Self {
        Writer {
            code: Vec::new(),
        }
    }
    /// Converts the writer into the underlying buffer.
    pub fn into(self: Self) -> Vec<u8> {
        self.code
    }
}