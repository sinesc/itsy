//! Key/value map support for the VM.
//!
//! Maps use a boxed-uniform layout in the heap object's data buffer, with an inline open-addressed hash
//! index for O(1) average lookup layered over an insertion-ordered entries region:
//!   [ len: StackAddress | n_entries: StackAddress | n_buckets: StackAddress ]
//!   [ buckets: n_buckets × StackAddress ]
//!   [ entries: n_entries × (key: HeapRef, value: HeapRef) ]
//! Keys and values are boxed: each is stored inline as a HeapRef (primitives are boxed onto the heap,
//! reference values are already HeapRefs). The entries region is append-only and preserves insertion
//! order (used by iteration / keys / values); a removed entry is tombstoned by setting its key
//! reference's index to MAP_EMPTY. `len` counts live entries, `n_entries` counts appended slots
//! (including tombstones).
//!
//! Each bucket holds `entry_index + 1` for an occupied slot, or 0 for an empty slot (linear probing).
//! Buckets only ever reference live entries: they are rebuilt from the live entries on removal and on
//! growth, so no bucket tombstones are needed. Hashing (see `hash_value`) mirrors the equality used by
//! `map_box_eq` so that equal keys always collide.

use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex, VariantIndex};
use crate::bytecode::{Constructor, HeapRef, HeapRefOp};
use crate::bytecode::runtime::heap::HeapOp;
use crate::bytecode::runtime::stack::StackOp;
use super::vm::VM;

/// Header field count (len, n_entries, n_buckets), each StackAddress sized.
pub(crate) const MAP_HEADER: StackAddress = 3 * size_of::<StackAddress>() as StackAddress;
/// Slot sentinel marking a tombstoned entry key. Uses the largest representable HeapRef index so it
/// round-trips through `HeapRef::new`/`index` (a raw `StackAddress::MAX` would be truncated).
pub(crate) const MAP_EMPTY: StackAddress = HeapRef::MAX_INDEX;
/// FNV-1a offset basis, used for key hashing.
const FNV_OFFSET: u64 = 0xcbf29ce484222325;
/// FNV-1a prime, used for key hashing.
const FNV_PRIME: u64 = 0x100000001b3;

/// FNV-1a hash of a byte slice seeded with `h`.
fn fnv_bytes(mut h: u64, bytes: &[u8]) -> u64 {
    for &b in bytes {
        h ^= b as u64;
        h = h.wrapping_mul(FNV_PRIME);
    }
    h
}


impl<T, U> VM<T, U> {
    // -- Layout access ------------------------------------------------------------------------------------

    /// Returns the byte offset of the entries region for the map at `idx` (past the header and buckets).
    pub(crate) fn map_entries_offset(self: &Self, idx: StackAddress) -> StackAddress {
        let sa = size_of::<StackAddress>() as StackAddress;
        MAP_HEADER + self.map_n_buckets(idx) * sa
    }

    /// Returns the number of hash buckets of the map at `idx`.
    pub(crate) fn map_n_buckets(self: &Self, idx: StackAddress) -> StackAddress {
        let sa = size_of::<StackAddress>() as StackAddress;
        self.heap.load(idx, 2 * sa)
    }

    /// Reads bucket `b` of the map at `idx` (0 = empty, else `entry_index + 1`).
    pub(crate) fn map_bucket_get(self: &Self, idx: StackAddress, b: StackAddress) -> StackAddress {
        let sa = size_of::<StackAddress>() as StackAddress;
        self.heap.load(idx, MAP_HEADER + b * sa)
    }

    /// Writes bucket `b` of the map at `idx`.
    pub(crate) fn map_bucket_set(self: &mut Self, idx: StackAddress, b: StackAddress, value: StackAddress) {
        let sa = size_of::<StackAddress>() as StackAddress;
        self.heap.store(idx, MAP_HEADER + b * sa, value);
    }

    /// Returns the live (non-tombstoned) (key, value) reference pairs of the map at `idx` in insertion order.
    pub(crate) fn map_live_entries(self: &Self, idx: StackAddress) -> Vec<(HeapRef, HeapRef)> {
        let sa = size_of::<StackAddress>() as StackAddress;
        let hr = size_of::<HeapRef>() as StackAddress;
        let n_entries: StackAddress = self.heap.load(idx, sa);
        let base = self.map_entries_offset(idx);
        let mut result = Vec::new();
        for e in 0..n_entries {
            let entry_off = base + e * 2 * hr;
            let key: HeapRef = self.heap.read(HeapRef::new(idx, entry_off));
            if key.index() == MAP_EMPTY {
                continue; // tombstone
            }
            let value: HeapRef = self.heap.read(HeapRef::new(idx, entry_off + hr));
            result.push((key, value));
        }
        result
    }

    // -- Boxing / unboxing -------------------------------------------------------------------------------

    /// Adjusts the refcount of a boxed map key or value. Boxed primitives are leaf heap objects whose
    /// refcount is adjusted directly; boxed reference values are descended into via their constructor.
    pub(crate) fn refcount_box(self: &mut Self, boxed: HeapRef, box_ctor_offset: StackAddress, op: HeapRefOp, epoch: usize) {
        let mut o = box_ctor_offset;
        let box_op = Constructor::read_op(&self.stack, &mut o);
        if box_op == Constructor::Primitive {
            self.heap.ref_item(boxed.index(), op);
        } else if epoch != self.heap.item_epoch(boxed.index()) {
            self.refcount_value_epoch(boxed, box_ctor_offset, op, epoch);
        }
    }

    /// As [`refcount_box`], starting a fresh refcounting epoch. Used by the standalone map operations.
    pub(crate) fn refcount_box_top(self: &mut Self, boxed: HeapRef, box_ctor_offset: StackAddress, op: HeapRefOp) {
        let epoch = self.heap.new_epoch();
        self.refcount_box(boxed, box_ctor_offset, op, epoch);
    }

    /// Pops a boxed key/value off the stack using the given sub-constructor. A primitive is popped as raw
    /// bytes and boxed onto the heap; a reference value is already a HeapRef and is popped directly.
    pub(crate) fn map_box_pop(self: &mut Self, ctor_offset: StackAddress) -> HeapRef {
        let mut o = ctor_offset;
        let op = Constructor::read_op(&self.stack, &mut o);
        if op == Constructor::Primitive {
            let n = Constructor::read_index(&self.stack, &mut o) as usize;
            let sp = self.stack.sp() as usize;
            let start = sp - n;
            let bytes = self.stack.data()[start..sp].to_vec();
            self.stack.truncate(start as StackAddress);
            let index = self.heap.alloc_copy(&bytes, ItemIndex::MAX);
            HeapRef::new(index, 0)
        } else {
            self.stack.pop()
        }
    }

    /// Pushes a boxed primitive value onto the stack by copying its bytes. Reference values are pushed
    /// directly by the caller (see [`op_map_get`]).
    pub(crate) fn map_unbox_push_primitive(self: &mut Self, value: HeapRef, primitive_size: usize) {
        let bytes = self.heap.item(value.index()).data[0..primitive_size].to_vec();
        self.stack.extend_from(&bytes);
    }

    // -- Equality / hashing -----------------------------------------------------------------------------

    /// Compares two boxed keys/values for equality using the given sub-constructor.
    fn map_box_eq(self: &Self, a: HeapRef, b: HeapRef, ctor_offset: StackAddress) -> bool {
        let mut o = ctor_offset;
        let op = Constructor::read_op(&self.stack, &mut o);
        if op == Constructor::Primitive {
            let n = Constructor::read_index(&self.stack, &mut o) as usize;
            self.heap.item(a.index()).data[0..n] == self.heap.item(b.index()).data[0..n]
        } else {
            self.compare_value(a, b, ctor_offset)
        }
    }

    /// Hashes a boxed key consistently with [`map_box_eq`]: primitive boxes hash their raw bytes, reference
    /// keys are hashed structurally (mirroring [`compare_value`]).
    fn map_box_hash(self: &Self, boxed: HeapRef, ctor_offset: StackAddress) -> u64 {
        let mut o = ctor_offset;
        let op = Constructor::read_op(&self.stack, &mut o);
        if op == Constructor::Primitive {
            let n = Constructor::read_index(&self.stack, &mut o) as usize;
            fnv_bytes(FNV_OFFSET, &self.heap.item(boxed.index()).data[0..n])
        } else {
            self.hash_value(boxed, ctor_offset)
        }
    }

    /// Computes a structural hash of a reference value, guided by its serialized constructor. Equal values
    /// (per [`compare_value`]) always produce the same hash.
    fn hash_value(self: &Self, item: HeapRef, constructor_offset: StackAddress) -> u64 {
        match self.resolve_constructor_offset(item, constructor_offset) {
            None => item.index() as u64,
            Some(mut constructor_offset) => {
                let constructor = Constructor::read_op(&self.stack, &mut constructor_offset);
                self.hash_recurse(constructor, HeapRef::new(item.index(), 0), &mut constructor_offset)
            }
        }
    }

    /// Recursively hashes a reference value, mirroring the traversal of [`compare_recurse`].
    fn hash_recurse(self: &Self, constructor: Constructor, mut item: HeapRef, constructor_offset: &mut StackAddress) -> u64 {
        let parsed = Constructor::parse_with(&self.stack, *constructor_offset, constructor);
        *constructor_offset = parsed.offset;
        let result = match constructor {
            Constructor::Array => {
                let element_constructor = Constructor::read_op(&self.stack, constructor_offset);
                if element_constructor != Constructor::Primitive {
                    let num_elements = self.heap.item(item.index()).data.len() / HeapRef::primitive_size() as usize;
                    let original_constructor_offset = *constructor_offset;
                    let mut h = FNV_OFFSET;
                    for _ in 0..num_elements {
                        *constructor_offset = original_constructor_offset;
                        let element: HeapRef = self.heap.read_seq(&mut item);
                        h = (h ^ self.hash_recurse(element_constructor, element, constructor_offset)).wrapping_mul(FNV_PRIME);
                    }
                    if num_elements == 0 {
                        // ensure the offset advances past the (otherwise unvisited) element constructor
                        let mut skip = original_constructor_offset;
                        let op = Constructor::read_op(&self.stack, &mut skip);
                        *constructor_offset = Constructor::parse_with(&self.stack, skip, op).next;
                    }
                    h
                } else {
                    Constructor::read_index(&self.stack, constructor_offset); // skip primitive num_bytes
                    fnv_bytes(FNV_OFFSET, &self.heap.item(item.index()).data)
                }
            },
            Constructor::Struct => {
                let (_implementor_index, num_fields, mut field_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                self.hash_fields(num_fields, &mut field_offset, &mut item)
            },
            Constructor::Enum => {
                let variant: VariantIndex = self.heap.read_seq(&mut item);
                let (_implementor_index, _num_variants, variant_table_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                let (num_fields, mut field_offset) = Constructor::parse_variant_table(&self.stack, variant_table_offset, variant);
                let h = fnv_bytes(FNV_OFFSET, &variant.to_ne_bytes());
                h ^ self.hash_fields(num_fields, &mut field_offset, &mut item)
            },
            Constructor::String => {
                fnv_bytes(FNV_OFFSET, self.heap.string(item).unwrap_or("").as_bytes())
            },
            // closures, generators and maps compare by reference identity, so hash by it too
            Constructor::Closure | Constructor::Map | Constructor::Generator => item.index() as u64,
            Constructor::Virtual => {
                match self.resolve_constructor_offset(item, 0) {
                    None => item.index() as u64,
                    Some(mut concrete_offset) => {
                        let constructor = Constructor::read_op(&self.stack, &mut concrete_offset);
                        self.hash_recurse(constructor, HeapRef::new(item.index(), 0), &mut concrete_offset)
                    }
                }
            },
            Constructor::Primitive => panic!("Unexpected primitive constructor."),
        };
        *constructor_offset = parsed.next;
        result
    }

    /// Hashes a sequence of struct/enum-variant fields, mirroring [`compare_fields`].
    fn hash_fields(self: &Self, num_fields: ItemIndex, field_offset: &mut StackAddress, item: &mut HeapRef) -> u64 {
        let mut h = FNV_OFFSET;
        for _ in 0..num_fields {
            let field_constructor = Constructor::read_op(&self.stack, field_offset);
            if field_constructor != Constructor::Primitive {
                let field: HeapRef = self.heap.read_seq(item);
                h = (h ^ self.hash_recurse(field_constructor, field, field_offset)).wrapping_mul(FNV_PRIME);
            } else {
                let num_bytes = Constructor::read_index(&self.stack, field_offset) as StackOffset;
                let start = item.offset() as usize;
                let bytes = self.heap.item(item.index()).data[start..start + num_bytes as usize].to_vec();
                h = (h ^ fnv_bytes(FNV_OFFSET, &bytes)).wrapping_mul(FNV_PRIME);
                item.add_offset(num_bytes);
            }
        }
        h
    }

    // -- Bucket management ------------------------------------------------------------------------------

    /// Returns the entry index of `key` in the map at `idx`, probing the hash buckets. Boxed keys are
    /// compared by value via [`map_box_eq`].
    pub(crate) fn map_find(self: &Self, idx: StackAddress, key: HeapRef, key_ctor: StackAddress) -> Option<StackAddress> {
        let hr = size_of::<HeapRef>() as StackAddress;
        let n_buckets = self.map_n_buckets(idx);
        if n_buckets == 0 {
            return None;
        }
        let start = (self.map_box_hash(key, key_ctor) % n_buckets as u64) as StackAddress;
        let base = self.map_entries_offset(idx);
        let mut b = start;
        loop {
            let slot = self.map_bucket_get(idx, b);
            if slot == 0 {
                return None; // empty bucket terminates the probe
            }
            let entry_index = slot - 1;
            let stored: HeapRef = self.heap.read(HeapRef::new(idx, base + entry_index * 2 * hr));
            if stored.index() != MAP_EMPTY && self.map_box_eq(key, stored, key_ctor) {
                return Some(entry_index);
            }
            b = (b + 1) % n_buckets;
            if b == start {
                return None; // table full (should not happen below load factor 1)
            }
        }
    }

    /// Places `entry_index` into the first empty bucket on `key`'s probe chain. The caller guarantees a free
    /// bucket exists (load factor < 1).
    fn map_bucket_place(self: &mut Self, idx: StackAddress, key: HeapRef, key_ctor: StackAddress, entry_index: StackAddress, n_buckets: StackAddress) {
        let mut b = (self.map_box_hash(key, key_ctor) % n_buckets as u64) as StackAddress;
        loop {
            if self.map_bucket_get(idx, b) == 0 {
                self.map_bucket_set(idx, b, entry_index + 1);
                return;
            }
            b = (b + 1) % n_buckets;
        }
    }

    /// Rewrites the entries region in place, dropping tombstoned slots so that `n_entries == len` and the
    /// live entries occupy a contiguous prefix (their insertion order preserved). Entry (key, value)
    /// reference pairs are moved verbatim, so reference counts are unaffected. Entry indices change, so the
    /// caller must rebuild the bucket table afterwards.
    pub(crate) fn map_compact(self: &mut Self, idx: StackAddress) {
        let sa = size_of::<StackAddress>() as StackAddress;
        let hr = size_of::<HeapRef>() as StackAddress;
        let entry_size = 2 * hr;
        let n_entries: StackAddress = self.heap.load(idx, sa);
        let base = self.map_entries_offset(idx);
        let mut write: StackAddress = 0;
        for read in 0..n_entries {
            let read_off = base + read * entry_size;
            let key: HeapRef = self.heap.read(HeapRef::new(idx, read_off));
            if key.index() == MAP_EMPTY {
                continue; // skip tombstone
            }
            if write != read {
                let value: HeapRef = self.heap.read(HeapRef::new(idx, read_off + hr));
                let write_off = base + write * entry_size;
                self.heap.write(HeapRef::new(idx, write_off), key);
                self.heap.write(HeapRef::new(idx, write_off + hr), value);
            }
            write += 1;
        }
        // drop the now-dead tail and record the compacted entry count (== live count)
        self.heap.item_mut(idx).data.truncate((base + write * entry_size) as usize);
        self.heap.store(idx, sa, write);
    }

    /// Rebuilds the bucket table from the live entries, dropping any references to tombstoned entries.
    pub(crate) fn map_refill_buckets(self: &mut Self, idx: StackAddress, key_ctor: StackAddress) {
        let sa = size_of::<StackAddress>() as StackAddress;
        let hr = size_of::<HeapRef>() as StackAddress;
        let n_buckets = self.map_n_buckets(idx);
        for b in 0..n_buckets {
            self.map_bucket_set(idx, b, 0);
        }
        let n_entries: StackAddress = self.heap.load(idx, sa);
        let base = self.map_entries_offset(idx);
        for e in 0..n_entries {
            let key: HeapRef = self.heap.read(HeapRef::new(idx, base + e * 2 * hr));
            if key.index() == MAP_EMPTY {
                continue;
            }
            self.map_bucket_place(idx, key, key_ctor, e, n_buckets);
        }
    }

    /// Grows the map's bucket table to `new_n_buckets` by rebuilding the heap object's buffer (header + the
    /// larger zeroed bucket region + the unchanged entries bytes) and re-hashing the live entries. Entry
    /// bytes (boxed HeapRefs) are copied verbatim, so reference counts are unaffected.
    fn map_grow(self: &mut Self, idx: StackAddress, new_n_buckets: StackAddress, key_ctor: StackAddress) {
        let sa = size_of::<StackAddress>() as usize;
        let len: StackAddress = self.heap.load(idx, 0);
        let n_entries: StackAddress = self.heap.load(idx, size_of::<StackAddress>() as StackAddress);
        let old_entries_off = self.map_entries_offset(idx) as usize;
        let entries_bytes = self.heap.item(idx).data[old_entries_off..].to_vec();
        let mut data = Vec::with_capacity(MAP_HEADER as usize + new_n_buckets as usize * sa + entries_bytes.len());
        data.extend_from_slice(&len.to_ne_bytes());
        data.extend_from_slice(&n_entries.to_ne_bytes());
        data.extend_from_slice(&new_n_buckets.to_ne_bytes());
        data.resize(MAP_HEADER as usize + new_n_buckets as usize * sa, 0); // zeroed buckets
        data.extend_from_slice(&entries_bytes);
        self.heap.item_mut(idx).data = data;
        self.map_refill_buckets(idx, key_ctor);
    }

    /// Inserts or updates `(key, value)` in the map at `idx`, maintaining the bucket index. On a new key the
    /// entry is appended and the table grown if the load factor is exceeded; on an existing key the value is
    /// replaced (last write wins) and the duplicate lookup key box released. Boxes are consumed.
    ///
    /// `retain` selects the reference-counting discipline. For mutation (`m[k] = v` / `m.insert`) the map is
    /// an established container that takes ownership, so newly stored boxes are incremented and a replaced
    /// value is decremented. For literal construction the boxes are still un-adopted temporaries (the
    /// binding's store recursion increments them exactly once afterwards), so nothing is incremented here and
    /// a replaced value is simply freed.
    pub(crate) fn map_put(self: &mut Self, idx: StackAddress, key: HeapRef, value: HeapRef, key_ctor: StackAddress, value_ctor: StackAddress, retain: bool) {
        let sa = size_of::<StackAddress>() as StackAddress;
        let hr = size_of::<HeapRef>() as StackAddress;
        if let Some(e) = self.map_find(idx, key, key_ctor) {
            // existing key: replace value (last wins), drop the duplicate lookup key box
            let value_off = self.map_entries_offset(idx) + e * 2 * hr + hr;
            let old_value: HeapRef = self.heap.read(HeapRef::new(idx, value_off));
            self.refcount_box_top(old_value, value_ctor, if retain { HeapRefOp::Dec } else { HeapRefOp::Free });
            self.heap.write(HeapRef::new(idx, value_off), value);
            if retain {
                self.refcount_box_top(value, value_ctor, HeapRefOp::Inc);
            }
            self.refcount_box_top(key, key_ctor, HeapRefOp::Free);
        } else {
            // new key: grow first if needed so the entry hashes into the final table, then append
            let len: StackAddress = self.heap.load(idx, 0);
            let n_buckets = self.map_n_buckets(idx);
            if 4 * (len + 1) > 3 * n_buckets {
                self.map_grow(idx, n_buckets * 2, key_ctor);
            }
            let n_entries: StackAddress = self.heap.load(idx, sa);
            let zero = HeapRef::new(0, 0);
            self.heap.item_mut(idx).data.extend_from_slice(&zero.to_ne_bytes());
            self.heap.item_mut(idx).data.extend_from_slice(&zero.to_ne_bytes());
            let entry_off = self.map_entries_offset(idx) + n_entries * 2 * hr;
            self.heap.write(HeapRef::new(idx, entry_off), key);
            self.heap.write(HeapRef::new(idx, entry_off + hr), value);
            if retain {
                self.refcount_box_top(key, key_ctor, HeapRefOp::Inc);
                self.refcount_box_top(value, value_ctor, HeapRefOp::Inc);
            }
            self.heap.store(idx, 0, len + 1);
            self.heap.store(idx, sa, n_entries + 1);
            let n_buckets = self.map_n_buckets(idx);
            self.map_bucket_place(idx, key, key_ctor, n_entries, n_buckets);
        }
    }
}
