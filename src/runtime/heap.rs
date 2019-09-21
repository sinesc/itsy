use crate::util::{array2, array4, array8, index_twice};

/// Allowed operator for compare.
pub enum HeapCmp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte
}

/// A reference counted heap object.
#[derive(Debug)]
struct HeapObject {
    refs: u32,
    data: Vec<u8>,
}

impl HeapObject {
    fn new() -> Self {
        Self {
            refs: 0,
            data: Vec::new(),
        }
    }
}

/// A heap holding non-primitive objects, e.g. strings.
#[derive(Debug)]
pub struct Heap {
    objects: Vec<HeapObject>,
    free: Vec<u32>,
}

impl Heap {
    /// Creates a new VM heap.
    pub fn new() -> Self {
        Self {
            objects: Vec::with_capacity(128),
            free: Vec::with_capacity(16),
        }
    }
    /// Allocate a heap object with a reference count of 0 and return its index.
    pub fn alloc(self: &mut Self) -> u32 {
        self.free.pop().unwrap_or_else(|| {
            let pos = self.objects.len();
            self.objects.push(HeapObject::new());
            pos as u32
        })
    }
    /// Increase reference count for given heap object.
    pub fn inc_ref(self: &mut Self, index: u32) {
        self.objects[index as usize].refs += 1;
    }
    /// Decrease reference count for given heap object, free it if count is at 0.
    pub fn dec_ref_or_free(self: &mut Self, index: u32) {
        self.objects[index as usize].refs -= 1;
        if self.objects[index as usize].refs == 0 {
            self.free(index);
        }
    }
    /// Decrease reference count for given heap object.
    pub fn dec_ref(self: &mut Self, index: u32) {
        self.objects[index as usize].refs -= 1;
    }
    /// Free a chunk of memory.
    pub fn free(self: &mut Self, index: u32) {
        #[cfg(debug_assertions)]
        self.assert_exists(index);
        self.free.push(index);
    }
    /// Removes and returns a chunk of memory, freeing its slot on the heap.
    pub fn remove(self: &mut Self, index: u32) -> Vec<u8> {
        #[cfg(debug_assertions)]
        self.assert_exists(index);
        let object = ::std::mem::replace(&mut self.objects[index as usize], HeapObject::new());
        self.free.push(index);
        object.data
    }
    /// Deallocates freed chunks of memory.
    pub fn purge(self: &mut Self) {
        // todo: truncate if len=0 or items are consecutive
        for v in self.free.iter() {
            ::std::mem::replace(&mut self.objects[*v as usize], HeapObject::new());
        }
    }
    /// Number of active heap elements.
    pub fn len(self: &Self) -> u32 {
        (self.objects.len() - self.free.len()) as u32
    }
    /// Resets the heap, freeing all memory.
    pub fn reset(self: &mut Self) {
        self.objects = Vec::with_capacity(128);
        self.free = Vec::with_capacity(16);
    }
    /// Asserts that the given heap object exists.
    fn assert_exists(self: &Self, index: u32) {
        if let Some(pos) = self.free.iter().find(|&&pos| pos == index) {
            panic!("HEAP: double free of object {}", pos);
        }
    }

    pub fn read8(self: &Self, index: u32, offset: u32) -> u8 {
        self.objects[index as usize].data[offset as usize]
    }

    pub fn read16(self: &Self, index: u32, offset: u32) -> u16 {
        let offset = offset as usize;
        u16::from_ne_bytes(array2(&self.objects[index as usize].data[offset..offset + 2]))
    }

    pub fn read32(self: &Self, index: u32, offset: u32) -> u32 {
        let offset = offset as usize;
        u32::from_ne_bytes(array4(&self.objects[index as usize].data[offset..offset + 4]))
    }

    pub fn read64(self: &Self, index: u32, offset: u32) -> u64 {
        let offset = offset as usize;
        u64::from_ne_bytes(array8(&self.objects[index as usize].data[offset..offset + 8]))
    }

    pub fn write8(self: &mut Self, index: u32, offset: u32, value: u8) {
        self.objects[index as usize].data[offset as usize] = value;
    }

    pub fn write16(self: &mut Self, index: u32, offset: u32, value: u16) {
        let offset = offset as usize;
        self.objects[index as usize].data[offset..offset+2].copy_from_slice(&value.to_ne_bytes());
    }

    pub fn write32(self: &mut Self, index: u32, offset: u32, value: u32) {
        let offset = offset as usize;
        self.objects[index as usize].data[offset..offset+4].copy_from_slice(&value.to_ne_bytes());
    }

    pub fn write64(self: &mut Self, index: u32, offset: u32, value: u64) {
        let offset = offset as usize;
        self.objects[index as usize].data[offset..offset+8].copy_from_slice(&value.to_ne_bytes());
    }

    pub fn size_of(self: &Self, index: u32) -> u32 {
        self.objects[index as usize].data.len() as u32
    }

    // Copies bytes from one heap object to another (from/to their respective current offset), extending it if necessary.
    pub fn copy(self: &mut Self, index_dest: u32, offset_dest: u32, index_src: u32, offset_src: u32, num: u32) {

        let index_dest = index_dest as usize;
        let index_src = index_src as usize;
        let (dest, src) = index_twice(&mut self.objects, index_dest, index_src);

        let offset_dest = offset_dest as usize;
        let offset_src = offset_src as usize;
        let num_bytes = usize::min(num as usize, src.data.len() - offset_src);
        let copy_bytes = usize::min(num_bytes, dest.data.len() - offset_dest);
        let push_bytes = num_bytes - copy_bytes;

        if copy_bytes > 0 {
            let slice_dest = &mut dest.data[offset_dest .. offset_dest + copy_bytes];
            let slice_src = &mut src.data[offset_src .. offset_src + copy_bytes];
            slice_dest.copy_from_slice(slice_src);
        }

        if push_bytes > 0 {
            let (dest, src) = index_twice(&mut self.objects, index_dest, index_src);
            let slice_src = &mut src.data[offset_src + copy_bytes .. offset_src + copy_bytes + push_bytes];
            dest.data.extend_from_slice(slice_src);
        }
    }

    // Compares bytes in one heap object with another (starting at their respective current offset).
    pub fn compare(self: &mut Self, index_a: u32, offset_a: u32, index_b: u32, offset_b: u32, num: u32, op: HeapCmp) -> bool {

        let index_a = index_a as usize;
        let index_b = index_b as usize;
        let (a, b) = index_twice(&mut self.objects, index_a, index_b);

        let offset_a = offset_a as usize;
        let offset_b = offset_b as usize;
        let num = num as usize;

        let slice_a = &a.data[offset_a .. offset_a + num];
        let slice_b = &b.data[offset_b .. offset_b + num];

        match op {
            HeapCmp::Eq => slice_a == slice_b,
            HeapCmp::Neq => slice_a != slice_b,
            //HeapCmp::Lt => slice_a < slice_b,
            //HeapCmp::Lte => slice_a <= slice_b,
            //HeapCmp::Gt => slice_a > slice_b,
            //HeapCmp::Gte => slice_a >= slice_b,
            _ => panic!("invalid compare operation"),
        }
    }

    // Compares string in one heap object with another (starting at their respective current offset).
    pub fn compare_string(self: &mut Self, index_a: u32, offset_a: u32, index_b: u32, offset_b: u32, num: u32, op: HeapCmp) -> bool {

        let index_a = index_a as usize;
        let index_b = index_b as usize;
        let (a, b) = index_twice(&mut self.objects, index_a, index_b);
        let a = as_string(&a.data);
        let b = as_string(&b.data);

        let offset_a = offset_a as usize;
        let offset_b = offset_b as usize;
        let num = num as usize;

        let (num_b, num_a) = if num == 0 {
            (b.len() - offset_b, a.len() - offset_a)
        } else {
            (usize::min(num, b.len() - offset_b), usize::min(num, a.len() - offset_a))
        };

        let slice_a = &a[offset_a .. offset_a + num_a];
        let slice_b = &b[offset_b .. offset_b + num_b];

        match op {
            HeapCmp::Eq => slice_a == slice_b,
            HeapCmp::Neq => slice_a != slice_b,
            HeapCmp::Lt => slice_a < slice_b,
            HeapCmp::Lte => slice_a <= slice_b,
            HeapCmp::Gt => slice_a > slice_b,
            HeapCmp::Gte => slice_a >= slice_b,
        }
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

fn as_string(data: &Vec<u8>) -> &String {
    unsafe { ::std::mem::transmute(data) }
}

impl HeapOp<String> for Heap {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn store(self: &mut Self, value: String) -> u32 {
        let pos = self.alloc();
        self.objects[pos as usize].data = value.into_bytes();
        pos
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn load(self: &Self, pos: u32) -> &String {
        as_string(&self.objects[pos as usize].data)
    }
}

impl HeapOp<Vec<u8>> for Heap {
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn store(self: &mut Self, value: Vec<u8>) -> u32 {
        let pos = self.alloc();
        self.objects[pos as usize].data = value;
        pos as u32
    }
    #[cfg_attr(not(debug_assertions), inline(always))]
    fn load(self: &Self, pos: u32) -> &Vec<u8> {
        &self.objects[pos as usize].data
    }
}
