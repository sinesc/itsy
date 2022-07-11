use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex};
use crate::bytecode::{HeapRef, HeapRefOp};

// Asserts that a heap item exists when debug_assertions are enabled
macro_rules! debug_assert_index {
    ($self:ident, $index:expr) => {
        #[cfg(debug_assertions)]
        if $self.free_small.iter().find(|&&pos| pos == $index).is_some() ||
            $self.free_medium.iter().find(|&&pos| pos == $index).is_some() ||
            $self.free_large.iter().find(|&&pos| pos == $index).is_some() {
            panic!("HEAP: operation on previously freed object {}", $index);
        }
        #[cfg(debug_assertions)]
        if $index as usize >= $self.objects.len() {
            panic!("HEAP: invalid heap object index {}", $index);
        }
    }
}

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
pub struct HeapObject {
    /// Data stored in the heap object.
    pub data            : Vec<u8>,
    /// For trait implementing types, the type index to use for dynamic dispatch.
    implementor_index   : ItemIndex,
    /// Number of references to the heap object.
    refs                : StackAddress,
    /// Reference epoch. Epochs are used to prevent refcount updates from getting stuck in reference cycles.
    epoch               : usize,
}

impl HeapObject {
    fn new(data: Vec<u8>, implementor_index: ItemIndex, epoch: usize) -> Self {
        Self {
            data,
            implementor_index,
            refs: 0,
            epoch
        }
    }
}

/// A heap holding non-primitive objects, e.g. strings.
#[derive(Debug)]
pub struct Heap {
    objects     : Vec<HeapObject>,
    free_small  : Vec<StackAddress>,
    free_medium : Vec<StackAddress>,
    free_large  : Vec<StackAddress>,
    epoch       : usize,
}

impl Heap {
    const MAX_SIZE_SMALL: StackAddress = 64;
    const MAX_SIZE_LARGE: StackAddress = 64 * 1024;

    /// Creates a new VM heap.
    pub fn new() -> Self {
        Self {
            objects     : Vec::with_capacity(256),
            free_small  : Vec::with_capacity(256),
            free_medium : Vec::with_capacity(16),
            free_large  : Vec::with_capacity(4),
            epoch       : 0,
        }
    }
    /// Deallocates freed chunks of memory.
    pub fn purge(self: &mut Self) {
        for &v in self.free_small.iter() {
            let _drop = replace(&mut self.objects[v as usize].data, Vec::new());
        }
        for &v in self.free_medium.iter() {
            let _drop = replace(&mut self.objects[v as usize].data, Vec::new());
        }
        for &v in self.free_large.iter() {
            let _drop = replace(&mut self.objects[v as usize].data, Vec::new());
        }
    }
    /// Number of active heap elements.
    pub fn len(self: &Self) -> StackAddress {
        (self.objects.len() - self.free_small.len() - self.free_medium.len() - self.free_large.len()) as StackAddress
    }
    /// Number of active heap elements.
    pub fn free(self: &Self) -> (StackAddress, StackAddress, StackAddress, StackAddress) {
        (
            self.free_small.len() as StackAddress,
            self.free_medium.len() as StackAddress,
            self.free_large.len() as StackAddress,
            (self.free_small.iter().map(|&i| self.objects[i as usize].data.capacity()).sum::<usize>()
                + self.free_medium.iter().map(|&i| self.objects[i as usize].data.capacity()).sum::<usize>()
                + self.free_large.iter().map(|&i| self.objects[i as usize].data.capacity()).sum::<usize>()
            ) as StackAddress
        )
    }
    /// Resets the heap, freeing all memory.
    pub fn reset(self: &mut Self) {
        self.objects.truncate(0);
        self.free_small = Vec::with_capacity(256);
        self.free_medium = Vec::with_capacity(16);
        self.free_large = Vec::with_capacity(4);
    }
    /// Returns a map of unfreed heap objects and their reference counts.
    pub fn data(self: &Self) -> Map<StackAddress, (StackAddress, &Vec<u8>)> {
        self.objects.iter()
            .enumerate()
            .filter(|&(i, _)| !self.free_small.contains(&(i as StackAddress)) && !self.free_medium.contains(&(i as StackAddress)) && !self.free_large.contains(&(i as StackAddress)))
            .map(|(i, h)| (i as StackAddress, (h.refs, &h.data)))
            .collect()
    }
    /// Starts a new reference counting epoch. Epochs are used to avoid revisiting already visited (circular) references.
    pub fn new_epoch(self: &mut Self) -> usize {
        self.epoch = self.epoch.wrapping_add(1);
        self.epoch
    }
    /// Allocate a heap object with a reference count of 0 and return its index.
    pub fn alloc(self: &mut Self, size_hint: StackAddress, implementor_index: ItemIndex) -> StackAddress {
        if let Some(index) = self.find_empty(size_hint) {
            let object = &mut self.objects[index as usize];
            object.data.truncate(0);
            object.data.shrink_to(size_hint as usize);
            object.implementor_index = implementor_index;
            object.refs = 0;
            object.epoch = self.epoch;
            index
        } else {
            let index = self.objects.len();
            self.objects.push(HeapObject::new(Vec::with_capacity(size_hint as usize), implementor_index, self.epoch));
            index as StackAddress
        }
    }
    /// Allocate a heap object with a reference count of 0 and return its index.
    pub fn alloc_place(self: &mut Self, data: Vec<u8>, implementor_index: ItemIndex) -> StackAddress {
        if let Some(index) = self.find_empty(data.capacity() as StackAddress) {
            let object = &mut self.objects[index as usize];
            object.data = data;
            object.implementor_index = implementor_index;
            object.refs = 0;
            object.epoch = self.epoch;
            index
        } else {
            let index = self.objects.len();
            self.objects.push(HeapObject::new(data, implementor_index, self.epoch));
            index as StackAddress
        }
    }
    /// Allocate a heap object with a reference count of 0 and return its index.
    pub fn alloc_copy(self: &mut Self, data: &[ u8 ], implementor_index: ItemIndex) -> StackAddress {
        if let Some(index) = self.find_empty(data.len() as StackAddress) {
            let object = &mut self.objects[index as usize];
            object.data.truncate(0);
            object.data.shrink_to(data.len());
            object.data.extend_from_slice(data);
            object.implementor_index = implementor_index;
            object.refs = 0;
            object.epoch = self.epoch;
            index
        } else {
            let index = self.objects.len();
            self.objects.push(HeapObject::new(data.to_vec(), implementor_index, self.epoch));
            index as StackAddress
        }
    }
    /// Returns the epoch of the last reference counting operation on the item. Epochs are used to avoid revisiting already visited (circular) references.
    pub fn item_epoch(self: &Self, index: StackAddress) -> usize {
        debug_assert_index!(self, index);
        self.objects[index as usize].epoch
    }
    /// Returns the number of references on the given item.
    pub fn item_refs(self: &Self, index: StackAddress) -> StackAddress {
        debug_assert_index!(self, index);
        self.objects[index as usize].refs
    }
    /// Increase/decrease reference count for given heap object, optionally freeing it at count 0.
    pub fn ref_item(self: &mut Self, index: StackAddress, op: HeapRefOp) {
        debug_assert_index!(self, index);
        let refs = &mut self.objects[index as usize].refs;
        match op {
            HeapRefOp::Inc => {
                (*refs) += 1;
            },
            HeapRefOp::Dec => {
                debug_assert!(*refs >= 1, "attempted to decrement reference count of 0");
                if *refs == 1 {
                    self.free_item(index);
                } else {
                    (*refs) -= 1;
                }
            },
            HeapRefOp::Free => {
                if *refs == 0 {
                    self.free_item(index);
                }
            },
            HeapRefOp::DecNoFree => {
                debug_assert!(*refs >= 1, "attempted to decrement reference count of 0");
                (*refs) -= 1;
            },
        }
    }
    /// Free a chunk of memory.
    pub fn free_item(self: &mut Self, index: StackAddress) {
        debug_assert_index!(self, index);
        if self.objects[index as usize].data.capacity() <= Self::MAX_SIZE_SMALL {
            self.free_small.push(index);
        } else if self.objects[index as usize].data.capacity() <= Self::MAX_SIZE_LARGE {
            self.free_medium.push(index);
        } else {
            self.free_large.push(index);
        }
    }
    fn find_empty(self: &mut Self, size: StackAddress) -> Option<StackAddress> {
        if size <= Self::MAX_SIZE_SMALL {
            self.free_small.pop()
        } else if size <= Self::MAX_SIZE_LARGE {
            self.free_medium.pop()
        } else {
            self.free_large.pop()
        }
    }
    /// Returns the dynamic dispatch implementor index of the given heap object.
    pub fn item_implementor_index(self: &Self, index: StackAddress) -> ItemIndex {
        debug_assert_index!(self, index);
        self.objects[index as usize].implementor_index
    }
    /// Returns a reference to a HeapObject.
    pub fn item(self: &Self, index: StackAddress) -> &HeapObject {
        debug_assert_index!(self, index);
        &self.objects[index as usize]
    }
    /// Returns a mutable reference to a HeapObject.
    pub fn item_mut(self: &mut Self, index: StackAddress) -> &mut HeapObject {
        debug_assert_index!(self, index);
        &mut self.objects[index as usize]
    }
    /// Returns mutable references to two distinct HeapObjects.
    pub fn items_mut(self: &mut Self, index_a: StackAddress, index_b: StackAddress) -> (&mut HeapObject, &mut HeapObject) {
        debug_assert_index!(self, index_a);
        debug_assert_index!(self, index_b);
        debug_assert!(index_a != index_b);
        index_twice(&mut self.objects, index_a as usize, index_b as usize)
    }
    // Copies bytes from one heap object to another (from/to their respective current offset), extending it if necessary.
    pub fn copy(self: &mut Self, dest_item: HeapRef, src_item: HeapRef, len: StackAddress) {
        debug_assert_index!(self, dest_item.index());
        debug_assert_index!(self, src_item.index());
        if dest_item.index() != src_item.index() {
            let (dest, src) = index_twice(&mut self.objects, dest_item.index() as usize, src_item.index() as usize);
            let offset_src = src_item.offset() as usize;
            let offset_dest = dest_item.offset() as usize;
            let num_bytes = len as usize;
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
            let slice = &mut self.objects[src_item.index() as usize].data;
            let offset_src = src_item.offset() as usize;
            let offset_dest = dest_item.offset() as usize;
            let num_bytes = len as usize;
            let copy_bytes = usize::min(len as usize, slice.len() - offset_src);
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
    /// Returns a string slice for the given heap reference.
    pub fn string(self: &Self, item: HeapRef) -> &str {
        debug_assert_index!(self, item.index());
        let (index, offset) = item.into();
        let slice = &self.objects[index as usize].data[offset as usize..];
        //un safe { std::str::from_utf8_unchecked(&slice) }
        std::str::from_utf8(slice).unwrap()
    }
    // Compares string of one heap object with another.
    pub fn compare_string(self: &mut Self, a: HeapRef, b: HeapRef, op: HeapCmp) -> bool {
        debug_assert_index!(self, a.index());
        debug_assert_index!(self, b.index());
        let slice_a = self.string(a);
        let slice_b = self.string(b);
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

/// Generic heap operations.
pub trait HeapOp<T> {
    /// Load a value from an heap object.
    fn load(self: &Self, index: StackAddress, offset: StackAddress) -> T;
    // Store a value to a heap object.
    fn store(self: &mut Self, index: StackAddress, offset: StackAddress, value: T);
    // load using a HeapRef
    fn read(self: &Self, item: HeapRef) -> T {
        let (index, offset) = item.into();
        self.load(index, offset)
    }
    // store using a HeapRef
    fn write(self: &mut Self, item: HeapRef, value: T) {
        let (index, offset) = item.into();
        self.store(index, offset, value);
    }
    /// Reads from the given HeapRef and increments its offset by the number of read bytes.
    fn read_seq(self: &Self, item: &mut HeapRef) -> T {
        let result = self.read(*item);
        item.add_offset(size_of::<T>() as StackOffset);
        result
    }
    /*
    /// Writes to the given HeapRef and increments its offset by the number of written bytes.
    fn write_seq(self: &mut Self, item: &mut HeapRef, value: T) {
        self.write(*item, value);
        item.add_offset(size_of::<T>() as StackOffset);
    }
    */
}

macro_rules! impl_heap {
    (single, $type:ident) => (
        impl HeapOp<$type> for Heap {
            fn load(self: &Self, index: StackAddress, offset: StackAddress) -> $type {
                self.objects[index as usize].data[offset as usize] as $type
            }
            fn store(self: &mut Self, index: StackAddress, offset: StackAddress, value: $type) {
                self.objects[index as usize].data[offset as usize] = value as u8;
            }
        }
    );
    (multi, $type:ident) => (
        impl HeapOp<$type> for Heap {
            fn load(self: &Self, index: StackAddress, offset: StackAddress) -> $type {
                let offset = offset as usize;
                $type::from_ne_bytes(self.objects[index as usize].data[offset..offset + size_of::<$type>()].try_into().unwrap())
            }
            fn store(self: &mut Self, index: StackAddress, offset: StackAddress, value: $type) {
                let offset = offset as usize;
                self.objects[index as usize].data[offset..offset + size_of::<$type>()].copy_from_slice(&value.to_ne_bytes());
            }
        }
    );
}

impl_heap!(single, u8);
impl_heap!(multi, u16);
impl_heap!(multi, u32);
impl_heap!(multi, u64);
impl_heap!(single, i8);
impl_heap!(multi, i16);
impl_heap!(multi, i32);
impl_heap!(multi, i64);
impl_heap!(multi, usize);
impl_heap!(multi, HeapRef);

/// References two elements of a slice mutably
pub fn index_twice<T>(slice: &mut [T], a: usize, b: usize) -> (&mut T, &mut T) {
    if a == b {
        panic!("tried to index element {} twice", a);
    } else if a >= slice.len() || b >= slice.len() {
        panic!("index ({}, {}) out of bounds", a, b);
    } else {
        if a > b {
            let (left, right) = slice.split_at_mut(a);
            (&mut right[0], &mut left[b])
        } else {
            let (left, right) = slice.split_at_mut(b);
            (&mut left[a], &mut right[0])
        }
    }
}