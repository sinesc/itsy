use crate::util::{HeapRef, Constructor, array2, array4, array8, index_twice};

pub(crate) const CONSTPOOL_INDEX: u32 = 0;

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
    pub fn new() -> Self { // todo: accept const pool here?
        Self {
            objects: Vec::with_capacity(128),
            free: Vec::with_capacity(16),
        }
    }
    /// Allocate a heap object with a reference count of 0 and return its index.
    pub fn alloc(self: &mut Self, data: Vec<u8>) -> u32 {
        let index = if let Some(index) = self.free.pop() {
            self.objects[index as usize] = HeapObject { refs: 0, data: data };
            index
        } else {
            let index = self.objects.len();
            self.objects.push(HeapObject { refs: 0, data: data });
            index as u32
        };
        index
    }
    /// Increase reference count for given heap object.
    pub fn inc_ref(self: &mut Self, index: u32) {
        self.objects[index as usize].refs += 1;
    }
    /// Decrease reference count for given heap object, free it if count is at 0.
    pub fn dec_ref_or_free(self: &mut Self, index: u32) {
        self.objects[index as usize].refs -= 1;
        if self.objects[index as usize].refs == 0 && index != CONSTPOOL_INDEX {
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
        self.objects.truncate(1);
        self.free = Vec::with_capacity(16);
    }

    fn read_op(self: &Self, index: u32, offset: &mut u32) -> Constructor {
        let result = Constructor::from_u8(self.objects[index as usize].data[*offset as usize]);
        *offset += 1;
        result
    }
    fn read_arg(self: &Self, index: u32, offset: &mut u32) -> u32 {
        let result = u32::from_ne_bytes(array4(&self.objects[index as usize].data[*offset as usize..*offset as usize + 4]));
        *offset += 4;
        result
    }
    fn append_ref(self: &mut Self, item: HeapRef, value: HeapRef) {
        let data = &mut self.objects[item.index as usize].data;
        data.extend_from_slice(&value.index.to_ne_bytes());
        data.extend_from_slice(&value.len.to_ne_bytes());
        data.extend_from_slice(&value.offset.to_ne_bytes());
    }
    /// Creates a new instance from construction instructions (constructor) and prototype.
    /// TODO: probably should go into VM
    pub fn construct(self: &mut Self, constructor: &mut HeapRef, prototype: &mut HeapRef, dest: &mut HeapRef) {
        // see util::types::Constructor for details
        match self.read_op(constructor.index, &mut constructor.offset) {
            Constructor::Copy => {
                let len = self.read_arg(constructor.index, &mut constructor.offset);
                let (dest, src) = index_twice(&mut self.objects, dest.index as usize, prototype.index as usize);
                let src_slice = &src.data[prototype.offset as usize .. prototype.offset as usize + len as usize];
                prototype.offset += len;
                dest.data.extend_from_slice(src_slice);
            }
            Constructor::CopyRef => {
                let src_ref = self.read96(*prototype);
                prototype.offset += 12;
                // clone src data
                let src = &self.objects[src_ref.index as usize];
                let src_slice = &src.data[src_ref.offset as usize .. src_ref.offset as usize + src_ref.len as usize];
                let src_vec = src_slice.to_vec();
                // store in new heap object
                let new_dest = HeapRef { index: self.alloc(src_vec), len: src_ref.len, offset: 0 };
                self.objects[new_dest.index as usize].refs = 1;
                // append reference to newly created object
                self.append_ref(*dest, new_dest);
            }
            Constructor::Array => {
                let len = self.read_arg(constructor.index, &mut constructor.offset);
                for _ in 0..len {
                    self.construct(constructor, prototype, dest);
                }
            }
            Constructor::ArrayRef => {
                unimplemented!();
            }
            Constructor::Struct => {
                let len = self.read_arg(constructor.index, &mut constructor.offset);
                for _ in 0..len {
                    self.construct(constructor, prototype, dest);
                }
            }
            _ => unreachable!()
        };
        dest.len = self.size_of(dest.index);
    }

    /// Asserts that the given heap object exists.
    fn assert_exists(self: &Self, index: u32) {
        if let Some(pos) = self.free.iter().find(|&&pos| pos == index) {
            panic!("HEAP: double free of object {}", pos);
        }
    }

    pub fn read8(self: &Self, item: HeapRef) -> u8 {
        let HeapRef { index, offset, .. } = item;
        self.objects[index as usize].data[offset as usize]
    }

    pub fn read16(self: &Self, item: HeapRef) -> u16 {
        let HeapRef { index, offset, .. } = item;
        let offset = offset as usize;
        u16::from_ne_bytes(array2(&self.objects[index as usize].data[offset..offset + 2]))
    }

    pub fn read32(self: &Self, item: HeapRef) -> u32 {
        let HeapRef { index, offset, .. } = item;
        let offset = offset as usize;
        u32::from_ne_bytes(array4(&self.objects[index as usize].data[offset..offset + 4]))
    }

    pub fn read64(self: &Self, item: HeapRef) -> u64 {
        let HeapRef { index, offset, .. } = item;
        let offset = offset as usize;
        u64::from_ne_bytes(array8(&self.objects[index as usize].data[offset..offset + 8]))
    }

    pub fn read96(self: &Self, item: HeapRef) -> HeapRef {
        HeapRef {
            index   : self.read32(item),
            len     : self.read32(item.offset(4)),
            offset  : self.read32(item.offset(8)),
        }
    }

    pub fn write8(self: &mut Self, item: HeapRef, value: u8) {
        let HeapRef { index, offset, .. } = item;
        self.objects[index as usize].data[offset as usize] = value;
    }

    pub fn write16(self: &mut Self, item: HeapRef, value: u16) {
        let HeapRef { index, offset, .. } = item;
        let offset = offset as usize;
        self.objects[index as usize].data[offset..offset+2].copy_from_slice(&value.to_ne_bytes());
    }

    pub fn write32(self: &mut Self, item: HeapRef, value: u32) {
        let HeapRef { index, offset, .. } = item;
        let offset = offset as usize;
        self.objects[index as usize].data[offset..offset+4].copy_from_slice(&value.to_ne_bytes());
    }

    pub fn write64(self: &mut Self, item: HeapRef, value: u64) {
        let HeapRef { index, offset, .. } = item;
        let offset = offset as usize;
        self.objects[index as usize].data[offset..offset+8].copy_from_slice(&value.to_ne_bytes());
    }

    pub fn write96(self: &mut Self, item: HeapRef, value: HeapRef) {
        self.write32(item, value.index);
        self.write32(item.offset(4), value.len);
        self.write32(item.offset(8), value.offset);
    }

    /// Returns the size in bytes of the given heap object.
    pub fn size_of(self: &Self, index: u32) -> u32 {
        self.objects[index as usize].data.len() as u32
    }

    /// Returns a byte slice for the given heap reference.
    pub fn slice(self: &Self, item: HeapRef) -> &[u8] {
        let HeapRef { index, offset, len } = item;
        let offset = offset as usize;
        let len = len as usize;
        &self.objects[index as usize].data[offset..offset + len]
    }

    /// Returns a string slice for the given heap reference.
    pub fn str(self: &Self, item: HeapRef) -> &str {
        use std::str;
        let slice = self.slice(item);
        //unsafe { str::from_utf8_unchecked(&slice) } // Todo: probably want to use this later, for now the extra check is nice
        str::from_utf8(&slice).unwrap()
    }

    // Copies bytes from one heap object to another (from/to their respective current offset), extending it if necessary.
    pub fn copy(self: &mut Self, dest_item: HeapRef, src_item: HeapRef) {

        if dest_item.index != src_item.index {

            let (dest, src) = index_twice(&mut self.objects, dest_item.index as usize, src_item.index as usize);

            let offset_src = src_item.offset as usize;
            let offset_dest = dest_item.offset as usize;
            let num_bytes = src_item.len as usize;
            let copy_bytes = usize::min(num_bytes, dest.data.len() - offset_dest);
            let push_bytes = num_bytes - copy_bytes;

            if copy_bytes > 0 {
                let slice_dest = &mut dest.data[offset_dest .. offset_dest + copy_bytes];
                let slice_src = &mut src.data[offset_src .. offset_src + copy_bytes];
                slice_dest.copy_from_slice(slice_src);
            }

            if push_bytes > 0 {
                let slice_src = &mut src.data[offset_src + copy_bytes .. offset_src + copy_bytes + push_bytes];
                dest.data.extend_from_slice(slice_src);
            }

        } else {

            let slice = &mut self.objects[src_item.index as usize].data;

            let offset_src = src_item.offset as usize;
            let offset_dest = dest_item.offset as usize;
            let num_bytes = src_item.len as usize;
            let copy_bytes = usize::min(src_item.len as usize, slice.len() - offset_src);
            let push_bytes = num_bytes - copy_bytes;

            if copy_bytes > 0 {
                slice.copy_within(offset_src..offset_src + copy_bytes, offset_dest);
            }

            if push_bytes > 0 {
                let mut tmp = slice[offset_src + copy_bytes..offset_src + copy_bytes + push_bytes].to_vec();
                slice.append(&mut tmp);
            }
        }
    }

    // Compares bytes of one heap object with another.
    pub fn compare(self: &mut Self, a: HeapRef, b: HeapRef, op: HeapCmp) -> bool {
        let slice_a = self.slice(a);
        let slice_b = self.slice(b);
        match op {
            HeapCmp::Eq => slice_a == slice_b,
            HeapCmp::Neq => slice_a != slice_b,
            HeapCmp::Lt => slice_a < slice_b,
            HeapCmp::Lte => slice_a <= slice_b,
            HeapCmp::Gt => slice_a > slice_b,
            HeapCmp::Gte => slice_a >= slice_b,
        }
    }

    // Compares string of one heap object with another.
    pub fn compare_string(self: &mut Self, a: HeapRef, b: HeapRef, op: HeapCmp) -> bool {
        let slice_a = self.str(a);
        let slice_b = self.str(b);
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
