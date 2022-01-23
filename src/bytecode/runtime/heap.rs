use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex};
use crate::bytecode::{HeapRef /*, HeapSlice*/};
use crate::shared::index_twice;

// Asserts that a heap item exists when debug_assertions are enabled
macro_rules! debug_assert_index {
    ($self:ident, $index:expr) => {
        #[cfg(debug_assertions)]
        if $self.free.iter().find(|&&pos| pos == $index).is_some() {
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

/// Allowed heap reference counting operations.
#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum HeapRefOp {
    /// Increase reference counter.
    Inc,
    /// Decrease reference counter, free on zero.
    Dec,
    /// Free if reference counter is 0, otherwise do nothing.
    FreeTmp,
    /// Decrease reference counter but do not free on zero.
    Zero,
}

impl HeapRefOp {
    pub(crate) fn from_u8(index: u8) -> Self {
        match index {
            x if x == Self::Inc as u8 => Self::Inc,
            x if x == Self::Dec as u8 => Self::Dec,
            x if x == Self::FreeTmp as u8 => Self::FreeTmp,
            x if x == Self::Zero as u8 => Self::Zero,
            _ => panic!("Invalid HeapRefOp index {}", index),
        }
    }
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
    objects: Vec<HeapObject>,
    free: Vec<StackAddress>,
    epoch: usize,
}

impl Heap {
    /// Creates a new VM heap.
    pub fn new() -> Self {
        Self {
            objects: Vec::with_capacity(128),
            free: Vec::with_capacity(16),
            epoch: 0,
        }
    }
    /// Deallocates freed chunks of memory.
    pub fn purge(self: &mut Self) {
        // todo: truncate if len=0 or items are consecutive
        for &v in self.free.iter() {
            let _drop = replace(&mut self.objects[v as usize].data, Vec::new());
        }
    }
    /// Number of active heap elements.
    pub fn len(self: &Self) -> StackAddress {
        (self.objects.len() - self.free.len()) as StackAddress
    }
    /// Resets the heap, freeing all memory.
    pub fn reset(self: &mut Self) {
        self.objects.truncate(0);
        self.free = Vec::with_capacity(16);
    }
    /// Returns a map of unfreed heap objects and their reference counts.
    pub fn data(self: &Self) -> Map<StackAddress, (StackAddress, &Vec<u8>)> {
        self.objects.iter()
            .enumerate()
            .filter(|&(i, _)| !self.free.contains(&(i as StackAddress)))
            .map(|(i, h)| (i as StackAddress, (h.refs, &h.data)))
            .collect()
    }
    /// Starts a new reference counting epoch. Epochs are used to avoid revisiting already visited (circular) references.
    pub fn new_epoch(self: &mut Self) -> usize {
        self.epoch = self.epoch.wrapping_add(1);
        self.epoch
    }
    /// Allocate a heap object with a reference count of 0 and return its index.
    pub fn alloc(self: &mut Self, data: Vec<u8>, implementor_index: ItemIndex) -> StackAddress {
        if let Some(index) = self.free.pop() {
            self.objects[index as usize] = HeapObject::new(data, implementor_index, self.epoch);
            index
        } else {
            let index = self.objects.len();
            self.objects.push(HeapObject::new(data, implementor_index, self.epoch));
            index as StackAddress
        }
    }
    /// Returns the epoch of the last reference counting operation on the item. Epochs are used to avoid revisiting already visited (circular) references.
    pub fn item_epoch(self: &Self, index: StackAddress) -> usize {
        debug_assert_index!(self, index);
        self.objects[index as usize].epoch
    }
    /// Increase/decrease reference count for given heap object, optionally freeing it at count 0.
    pub fn ref_item(self: &mut Self, index: StackAddress, op: HeapRefOp) {
        debug_assert_index!(self, index);
        let refs = &mut self.objects[index as usize].refs;
        match op {
            HeapRefOp::Inc => {
                (*refs) += 1;
            }
            HeapRefOp::Dec => {
                debug_assert!(*refs >= 1, "attempted to decrement reference count of 0");
                if *refs == 1 {
                    self.free_item(index);
                } else {
                    (*refs) -= 1;
                }
            }
            HeapRefOp::FreeTmp => {
                if *refs == 0 {
                    self.free_item(index);
                }
            }
            HeapRefOp::Zero => {
                debug_assert!(*refs >= 1, "attempted to decrement reference count of 0");
                (*refs) -= 1;
            }
        }
    }
    /// Free a chunk of memory.
    pub fn free_item(self: &mut Self, index: StackAddress) {
        debug_assert_index!(self, index);
        self.free.push(index);
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

    /// Returns a byte slice for the given heap reference.
    pub fn slice(self: &Self, item: HeapRef, len: StackAddress) -> &[u8] {
        let (index, offset) = item.into();
        debug_assert_index!(self, index);
        let offset = offset as usize;
        let len = len as usize;
        &self.objects[index as usize].data[offset..offset + len]
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
    /*
    /// Compares bytes of one heap object with another.
    pub fn compare(self: &mut Self, a: HeapRef, b: HeapRef, len: StackAddress, op: HeapCmp) -> bool {
        debug_assert_index!(self, a.index());
        debug_assert_index!(self, b.index());
        let slice_a = self.slice(a, len);
        let slice_b = self.slice(b, len);
        match op {
            HeapCmp::Eq => slice_a == slice_b,
            HeapCmp::Neq => slice_a != slice_b,
            HeapCmp::Lt => slice_a < slice_b,
            HeapCmp::Lte => slice_a <= slice_b,
            HeapCmp::Gt => slice_a > slice_b,
            HeapCmp::Gte => slice_a >= slice_b,
        }
    }
    /// Returns a string slice for the given heap reference.
    pub fn str(self: &Self, item: HeapRef, len: StackAddress) -> &str {
        debug_assert_index!(self, item.index());
        let slice = self.slice(item, len);
        //un safe { std::str::from_utf8_unchecked(slice) }
        std::str::from_utf8(slice).unwrap()
    }
    // Compares string slice of one heap object with another.
    pub fn compare_str(self: &mut Self, a: HeapSlice, b: HeapSlice, op: HeapCmp) -> bool {
        debug_assert_index!(self, a.heap_ref.index());
        debug_assert_index!(self, b.heap_ref.index());
        let slice_a = self.str(a.to_ref(), a.len);
        let slice_b = self.str(b.to_ref(), b.len);
        match op {
            HeapCmp::Eq => slice_a == slice_b,
            HeapCmp::Neq => slice_a != slice_b,
            HeapCmp::Lt => slice_a < slice_b,
            HeapCmp::Lte => slice_a <= slice_b,
            HeapCmp::Gt => slice_a > slice_b,
            HeapCmp::Gte => slice_a >= slice_b,
        }
    }
    */
    /// Returns a string slice for the given heap reference.
    pub fn string(self: &Self, item: HeapRef) -> &str {
        debug_assert_index!(self, item.index());
        let (index, offset) = item.into();
        let slice = &self.objects[index as usize].data[offset as usize..];
        //un safe { str::from_utf8_unchecked(&slice) }
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
    fn read(self: &Self, item: HeapRef) -> T;
    fn write(self: &mut Self, item: HeapRef, value: T);
    /// Reads from the given HeapRef and increments its offset by the number of read bytes.
    fn read_seq(self: &Self, item: &mut HeapRef) -> T {
        let result = self.read(*item);
        item.add_offset(size_of::<T>() as StackOffset);
        result
    }
    /// Writes to the given HeapRef and increments its offset by the number of written bytes.
    fn write_seq(self: &mut Self, item: &mut HeapRef, value: T) {
        self.write(*item, value);
        item.add_offset(size_of::<T>() as StackOffset);
    }
}

macro_rules! impl_heap {
    (single, $type:ident) => (
        impl HeapOp<$type> for Heap {
            fn read(self: &Self, item: HeapRef) -> $type {
                let (index, offset) = item.into();
                self.objects[index as usize].data[offset as usize]
            }
            fn write(self: &mut Self, item: HeapRef, value: $type) {
                let (index, offset) = item.into();
                self.objects[index as usize].data[offset as usize] = value;
            }
        }
    );
    (multi, $type:ident) => (
        impl HeapOp<$type> for Heap {
            fn read(self: &Self, item: HeapRef) -> $type {
                let (index, offset) = item.into();
                let offset = offset as usize;
                $type::from_ne_bytes(self.objects[index as usize].data[offset..offset + size_of::<$type>()].try_into().unwrap())
            }
            fn write(self: &mut Self, item: HeapRef, value: $type) {
                let (index, offset) = item.into();
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
impl_heap!(multi, i8);
impl_heap!(multi, i16);
impl_heap!(multi, i32);
impl_heap!(multi, i64);
impl_heap!(multi, usize);
impl_heap!(multi, HeapRef);
