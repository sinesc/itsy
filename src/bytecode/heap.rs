use crate::util::{array2, array4, array8};

/// A heap holding non-primitive objects, e.g. strings.
#[derive(Debug)]
pub struct Heap {
    data: Vec<Vec<u8>>,
    free: Vec<u32>,
}

impl Heap {
    /// Creates a new VM heap.
    pub fn new() -> Self {
        Self {
            data: Vec::with_capacity(128),
            free: Vec::with_capacity(16),
        }
    }
    /// Allocate a chunk of memory and return its index.
    pub fn alloc(self: &mut Self) -> u32 {
        self.free.pop().unwrap_or_else(|| {
            let pos = self.data.len();
            self.data.push(Vec::new());
            pos as u32
        })
    }
    /// Free a chunk of memory.
    pub fn free(self: &mut Self, position: u32) {
        self.free.push(position);
    }
    /// Removes and returns a chunk of memory, freeing its slot on the heap.
    pub fn remove(self: &mut Self, position: u32) -> Vec<u8> {
        let contents = ::std::mem::replace(&mut self.data[position as usize], Vec::new());
        self.free.push(position);
        contents
    }
    /// Deallocates freed chunks of memory.
    pub fn purge(self: &mut Self) {
        for v in self.free.drain(..) {
            ::std::mem::replace(&mut self.data[v as usize], Vec::new());
        }
        /*self.free.drain(..).map(|v| { // bck no clever enough
            ::std::mem::replace(&mut self.data[v], Vec::new());
        });*/
    }
    /// Resets the heap, freeing all memory.
    pub fn reset(self: &mut Self) {
        self.data = Vec::with_capacity(128);
        self.free = Vec::with_capacity(16);
    }

    pub fn read8(self: &Self, position: u32, offset: u32) -> u8 {
        self.data[position as usize][offset as usize]
    }

    pub fn read16(self: &Self, position: u32, offset: u32) -> u16 {
        let offset = offset as usize;
        u16::from_ne_bytes(array2(&self.data[position as usize][offset..offset + 2]))
    }

    pub fn read32(self: &Self, position: u32, offset: u32) -> u32 {
        let offset = offset as usize;
        u32::from_ne_bytes(array4(&self.data[position as usize][offset..offset + 4]))
    }

    pub fn read64(self: &Self, position: u32, offset: u32) -> u64 {
        let offset = offset as usize;
        u64::from_ne_bytes(array8(&self.data[position as usize][offset..offset + 8]))
    }

    pub fn write8(self: &mut Self, position: u32, offset: u32, value: u8) {
        self.data[position as usize][offset as usize] = value;
    }

    pub fn write16(self: &mut Self, position: u32, offset: u32, value: u16) {
        let offset = offset as usize;
        self.data[position as usize][offset..offset+2].copy_from_slice(&value.to_ne_bytes());
    }

    pub fn write32(self: &mut Self, position: u32, offset: u32, value: u32) {
        let offset = offset as usize;
        self.data[position as usize][offset..offset+4].copy_from_slice(&value.to_ne_bytes());
    }

    pub fn write64(self: &mut Self, position: u32, offset: u32, value: u64) {
        let offset = offset as usize;
        self.data[position as usize][offset..offset+8].copy_from_slice(&value.to_ne_bytes());
    }
}

/// Trait for generic stack operations.
pub trait HeapOp<T: Clone> {
    /// Store given value on the heap.
    fn store(self: &mut Self, value: T) -> u32;
    /// Load a value from the heap.
    fn load(self: &Self, pos: u32) -> &T;
    /// Clone a value from the heap.
    fn clone(self: &Self, pos: u32) -> T {
        (*self.load(pos)).clone()
    }
}

impl HeapOp<String> for Heap {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn store(self: &mut Self, value: String) -> u32 {
        let pos = self.alloc();
        self.data[pos as usize] = value.into_bytes();
        pos
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn load(self: &Self, pos: u32) -> &String {
        unsafe { ::std::mem::transmute(&self.data[pos as usize]) }
    }
}

impl HeapOp<Vec<u8>> for Heap {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn store(self: &mut Self, value: Vec<u8>) -> u32 {
        let pos = self.alloc();
        self.data[pos as usize] = value;
        pos as u32
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn load(self: &Self, pos: u32) -> &Vec<u8> {
        &self.data[pos as usize]
    }
}
