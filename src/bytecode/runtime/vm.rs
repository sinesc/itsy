//! A virtual machine for running Itsy bytecode.

use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex, VariantIndex};
use crate::bytecode::{HeapRef, HeapRefOp, Constructor, Program, ConstDescriptor, ConstEndianness, VMFunc, VMData, runtime::{stack::{Stack, StackOp}, heap::{Heap, HeapOp, HeapCmp}, error::*}};
#[cfg(feature="debugging")]
use crate::bytecode::opcodes::OpCode;

/// Current state of the vm, checked after each instruction.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VMState {
    /// The program is ready to run.
    Ready,
    /// The program has yielded. It can be resumed.
    Yielded,
    /// The program has terminated and must be reset before it can be run again.
    Terminated,
    /// The program encountered a runtime error and must be reset before it can be run again.
    Error(RuntimeErrorKind),
}

/// A virtual machine for running Itsy bytecode.
///
/// See [itsy_api](crate::itsy_api) for an example that runs an Itsy program using [VM::run].
#[derive(Debug)]
pub struct VM<T, U> {
    context_type            : PhantomData<U>,
    func_type               : PhantomData<T>,
    pub(crate) instructions : Vec<u8>,
    pub(crate) pc           : StackAddress,
    pub(crate) error_pc     : StackAddress,
    pub(crate) state        : VMState,
    pub stack               : Stack,
    pub heap                : Heap,
}

// todo check where bounds here, some seam pointless

/// Public VM methods.
impl<T, U> VM<T, U> {
    /// Create a new VM instance with the given Program.
    pub fn new(program: Program<T>) -> Self where T: VMFunc<T> + VMData<T, U> {
        let Program { instructions, consts, const_descriptors, .. } = program;
        let stack = Self::init_consts(&consts, &const_descriptors);
        // occupy heap element 0 so we can identify intialized heap objects their address != 0
        let mut heap = Heap::new();
        heap.alloc(0, 0);
        VM {
            context_type: PhantomData,
            func_type   : PhantomData,
            instructions: instructions,
            pc          : 0,
            error_pc    : 0,
            state       : VMState::Ready,
            stack       : stack,
            heap        : heap,
        }
    }

    /// Executes the current program until it yields or terminates.
    pub fn run(self: &mut Self, context: &mut U) -> RuntimeResult<VMState> where T: VMFunc<T> + VMData<T, U> {
        if self.state != VMState::Ready && self.state != VMState::Yielded {
            return Err(RuntimeError::new(0, RuntimeErrorKind::NotReady, None));
        }
        self.exec(context);
        match self.state {
            VMState::Terminated if self.heap.len() > 1 => Err(RuntimeError::new(0, RuntimeErrorKind::HeapCorruption, None)),
            VMState::Error(kind) => Err(RuntimeError::new(self.error_pc, kind, None)),
            VMState::Ready => {
                let kind = RuntimeErrorKind::UnexpectedReady;
                self.state = VMState::Error(kind);
                Err(RuntimeError::new(self.pc, kind, None))
            },
            VMState::Terminated | VMState::Yielded => Ok(self.state),
        }
    }

    /// Clears runtime error, allowing the VM to resume via run(). This is a no-op if the VM is
    /// in Ready or Yielded state and an error in Terminated state.
    pub fn clear_error(self: &mut Self) -> RuntimeResult where T: VMFunc<T> + VMData<T, U> {
        match self.state {
            VMState::Error(_) => {
                self.error_pc = 0;
                self.state = VMState::Ready;
                Ok(())
            },
            VMState::Ready | VMState::Yielded => Ok(()),
            _ => Err(RuntimeError::new(0, RuntimeErrorKind::CannotClear, None)),
        }
    }

    /// Returns the current VM state
    pub fn state(self: &Self) -> VMState {
        self.state
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.reset();
        self.heap.reset();
        self.heap.alloc(0, 0);
        self.pc = 0;
        self.error_pc = 0;
        self.state = VMState::Ready;
    }

    /// Pushes program const pool onto the stack, converting them from Little Endian to native endianness.
    fn init_consts(consts: &Vec<u8>, const_descriptors: &Vec<ConstDescriptor>) -> Stack {
        use ConstEndianness as CE;
        let mut stack = Stack::new();
        // descriptors describe consecutive blocks of the const pool, so the start of each block is
        // the running sum of all preceding block sizes
        let mut start = 0usize;
        for descriptor in const_descriptors {
            let end = start + descriptor.size as usize;
            match (descriptor.endianness, descriptor.size) {
                (_, 1)              => stack.push(consts[start]),
                (CE::Integer, 2)    => stack.push(u16::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Integer, 4)    => stack.push(u32::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Integer, 8)    => stack.push(u64::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Float, 4)      => stack.push(f32::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Float, 8)      => stack.push(f64::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::None, _)       => stack.extend_from(&consts[start..end]),
                _ => panic!("Unexpected ConstDescriptor {:?}.", &descriptor),
            }
            start = end;
        }
        stack.begin();
        stack
    }

    /// Reads a constructor opcode.
    fn construct_read_op(self: &Self, constructor_offset: &mut StackAddress) -> Constructor {
        let op = Constructor::from_u8(self.stack.load(*constructor_offset));
        *constructor_offset += size_of_val(&op) as StackAddress;
        op
    }

    /// Reads an ItemIndex sized constructor argument.
    fn construct_read_index(self: &Self, constructor_offset: &mut StackAddress) -> ItemIndex {
        let arg: ItemIndex = self.stack.load(*constructor_offset);
        *constructor_offset += size_of_val(&arg) as StackAddress;
        arg
    }

    /// Resolves a value's constructor offset, following the indirection used for trait implementors (offset 0,
    /// looked up via the object's implementor index) and dynamic constructors (offset 1, stored at the end of the
    /// object). Returns `None` if the object has no constructor, i.e. contains no nested heap references.
    fn resolve_constructor_offset(self: &Self, item: HeapRef, mut constructor_offset: StackAddress) -> Option<StackAddress> {
        if constructor_offset == 0 {
            // trait implementor specific constructor
            let implementor_index = self.heap.item_implementor_index(item.index()) as usize;
            constructor_offset = self.stack.load((implementor_index * size_of::<StackAddress>()) as StackAddress);
        } else if constructor_offset == 1 {
            // dynamic constructor, index stored at the end of the heap-object
            let data = &self.heap.item(item.index()).data;
            let pos = data.len() - size_of::<StackAddress>();
            constructor_offset = StackAddress::from_ne_bytes(data[pos..].try_into().unwrap());
            if constructor_offset == 0 {
                return None;
            }
        }
        Some(constructor_offset)
    }

    /// Updates the refcounts for given heap reference and any nested heap references. Looks up virtual constructor if offset is 0.
    pub(crate) fn refcount_value(self: &mut Self, item: HeapRef, constructor_offset: StackAddress, op: HeapRefOp) {
        let epoch = self.heap.new_epoch();
        self.refcount_value_epoch(item, constructor_offset, op, epoch);
    }

    /// As [`refcount_value`], but reuses an existing refcounting epoch instead of starting a new one.
    /// Used to descend into nested trait-object (virtual) references while traversing a containing value.
    fn refcount_value_epoch(self: &mut Self, item: HeapRef, constructor_offset: StackAddress, op: HeapRefOp, epoch: usize) {
        match self.resolve_constructor_offset(item, constructor_offset) {
            None => self.heap.ref_item(item.index(), op),
            Some(mut constructor_offset) => {
                let constructor = self.construct_read_op(&mut constructor_offset);
                self.refcount_recurse(constructor, HeapRef::new(item.index(), 0), &mut constructor_offset, op, epoch);
            }
        }
    }

    /// Support method usd by refcount_value() to allow for reading the type before recursing into the type-constructor.
    fn refcount_recurse(self: &mut Self, constructor: Constructor, mut item: HeapRef, constructor_offset: &mut StackAddress, op: HeapRefOp, epoch: usize) {
        let item_index = item.index();
        let refs = self.heap.item_refs(item_index);
        let recurse = (refs == 1 && (op == HeapRefOp::Dec || op == HeapRefOp::DecNoFree)) || (refs == 0 && (op == HeapRefOp::Inc || op == HeapRefOp::Free));
        let parsed = Constructor::parse_with(&self.stack, *constructor_offset, constructor);
        if !recurse {
            // No recursion is required if only the refcount changes, but we need to skip the constructor
            self.heap.ref_item(item_index, op);
            *constructor_offset = parsed.next;
        } else {
            // Value will either be be..
            //         dropped: recursively decrease reference count for referenced values
            // or materialized: recursively increase reference count for referenced values
            *constructor_offset = parsed.offset;
            match constructor {
                Constructor::Array => {
                    let element_constructor = self.construct_read_op(constructor_offset);
                    if element_constructor != Constructor::Primitive {
                        let original_constructor_offset = *constructor_offset;
                        // compute number of elements from heap size
                        let num_elements = self.heap.item(item_index).data.len() / HeapRef::primitive_size() as usize;
                        for _ in 0..num_elements {
                            // reset offset each iteration to keep referencing the same type for each element but make sure we have advanced once at the end of the loop
                            *constructor_offset = original_constructor_offset;
                            let element: HeapRef = self.heap.read_seq(&mut item);
                            let element_index = element.index();
                            if epoch != self.heap.item_epoch(element_index) {
                                self.refcount_recurse(element_constructor, element, constructor_offset, op, epoch);
                            }
                        }
                    } else {
                        // skip primitive num_bytes
                        self.construct_read_index(constructor_offset);
                    }
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Struct => {
                    let (_implementor_index, num_fields, mut field_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                    self.refcount_fields(num_fields, &mut field_offset, &mut item, op, epoch);
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Enum => {
                    let variant_index: VariantIndex = self.heap.read_seq(&mut item);
                    let (_implementor_index, num_variants, variant_table_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                    assert!(variant_index < num_variants, "Enum object specifies invalid enum variant");
                    // seek to variant offset, read it from constructor and seek to the offset
                    let (num_fields, mut field_offset) = Constructor::parse_variant_table(&self.stack, variant_table_offset, variant_index);
                    self.refcount_fields(num_fields, &mut field_offset, &mut item, op, epoch);
                    self.heap.ref_item(item_index, op);
                },
                Constructor::String => {
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Closure => {
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Virtual => {
                    // trait-object reference: resolve the concrete type's constructor via the object's
                    // implementor index and descend into it. The recursive call performs the single
                    // ref_item for this item; the virtual marker itself carries no inline layout.
                    self.refcount_value_epoch(HeapRef::new(item_index, 0), 0, op, epoch);
                },
                Constructor::Map => {
                    // Keys and values are boxed (stored inline as HeapRefs). Refcount each live entry's
                    // boxed key and value via their respective sub-constructors. parsed.offset points at
                    // the key constructor; the value constructor follows it.
                    let key_ctor_offset = *constructor_offset;
                    let value_ctor_offset = self.constructor_skip(key_ctor_offset);
                    for (key, value) in self.map_live_entries(item_index) {
                        self.refcount_box(key, key_ctor_offset, op, epoch);
                        self.refcount_box(value, value_ctor_offset, op, epoch);
                    }
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Primitive => {
                    panic!("Unexpected primitive constructor.");
                },
            };
            *constructor_offset = parsed.next;
        }
    }

    /// Updates the refcounts for a sequence of struct fields or enum-variant fields described by the constructor at field_offset.
    fn refcount_fields(self: &mut Self, num_fields: ItemIndex, field_offset: &mut StackAddress, item: &mut HeapRef, op: HeapRefOp, epoch: usize) {
        for _ in 0..num_fields {
            let field_constructor = self.construct_read_op(field_offset);
            if field_constructor != Constructor::Primitive {
                let field: HeapRef = self.heap.read_seq(item);
                let field_index = field.index();
                if epoch != self.heap.item_epoch(field_index) {
                    self.refcount_recurse(field_constructor, field, field_offset, op, epoch);
                    // fixme: skip when epoch matches, constructor_offset will be wrong otherwise
                }
            } else {
                let num_bytes = self.construct_read_index(field_offset) as StackOffset;
                item.add_offset(num_bytes);
            }
        }
    }

    /// Compares the targets of two heap references of identical reference type for deep equality, guided by the
    /// type's serialized constructor. Used to implement equality for non-primitive enums.
    pub(crate) fn compare_value(self: &Self, a: HeapRef, b: HeapRef, constructor_offset: StackAddress) -> bool {
        // both operands share the static type, so the constructor is resolved via a
        match self.resolve_constructor_offset(a, constructor_offset) {
            // no nested constructor: equal iff the same object is referenced
            None => a.index() == b.index(),
            Some(mut constructor_offset) => {
                let constructor = self.construct_read_op(&mut constructor_offset);
                self.compare_recurse(constructor, HeapRef::new(a.index(), 0), HeapRef::new(b.index(), 0), &mut constructor_offset)
            }
        }
    }

    /// Recursively compares two heap reference targets for equality, guided by the serialized type constructor.
    fn compare_recurse(self: &Self, constructor: Constructor, mut a: HeapRef, mut b: HeapRef, constructor_offset: &mut StackAddress) -> bool {
        let parsed = Constructor::parse_with(&self.stack, *constructor_offset, constructor);
        *constructor_offset = parsed.offset;
        let result = match constructor {
            Constructor::Array => {
                let element_constructor = self.construct_read_op(constructor_offset);
                if element_constructor != Constructor::Primitive {
                    let a_len = self.heap.item(a.index()).data.len();
                    let b_len = self.heap.item(b.index()).data.len();
                    if a_len != b_len {
                        false
                    } else {
                        let original_constructor_offset = *constructor_offset;
                        let num_elements = a_len / HeapRef::primitive_size() as usize;
                        let mut equal = true;
                        for _ in 0..num_elements {
                            // reset offset each iteration to re-read the same element type, advancing once at the end
                            *constructor_offset = original_constructor_offset;
                            let a_element: HeapRef = self.heap.read_seq(&mut a);
                            let b_element: HeapRef = self.heap.read_seq(&mut b);
                            if !self.compare_recurse(element_constructor, a_element, b_element, constructor_offset) {
                                equal = false;
                                break;
                            }
                        }
                        equal
                    }
                } else {
                    // primitive elements: comparing the raw object data covers both length and contents
                    self.construct_read_index(constructor_offset); // skip primitive num_bytes
                    self.heap.item(a.index()).data == self.heap.item(b.index()).data
                }
            },
            Constructor::Struct => {
                let (_implementor_index, num_fields, mut field_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                self.compare_fields(num_fields, &mut field_offset, &mut a, &mut b)
            },
            Constructor::Enum => {
                let a_variant: VariantIndex = self.heap.read_seq(&mut a);
                let b_variant: VariantIndex = self.heap.read_seq(&mut b);
                if a_variant != b_variant {
                    false
                } else {
                    let (_implementor_index, _num_variants, variant_table_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                    let (num_fields, mut field_offset) = Constructor::parse_variant_table(&self.stack, variant_table_offset, a_variant);
                    self.compare_fields(num_fields, &mut field_offset, &mut a, &mut b)
                }
            },
            Constructor::String => {
                self.heap.compare_string(a, b, HeapCmp::Eq)
            },
            Constructor::Closure => {
                // closures are opaque: equal only if the same heap object is referenced
                a.index() == b.index()
            },
            Constructor::Map => {
                // v1: maps compare by reference identity
                a.index() == b.index()
            },
            Constructor::Virtual => {
                // trait-object references: resolve each operand's concrete constructor via its implementor
                // index. Differing concrete types are never equal; otherwise compare guided by that constructor.
                match (self.resolve_constructor_offset(a, 0), self.resolve_constructor_offset(b, 0)) {
                    (None, None) => a.index() == b.index(),
                    (Some(a_offset), Some(b_offset)) if a_offset == b_offset => {
                        let mut a_offset = a_offset;
                        let constructor = self.construct_read_op(&mut a_offset);
                        self.compare_recurse(constructor, HeapRef::new(a.index(), 0), HeapRef::new(b.index(), 0), &mut a_offset)
                    },
                    _ => false,
                }
            },
            Constructor::Primitive => {
                panic!("Unexpected primitive constructor.");
            },
        };
        *constructor_offset = parsed.next;
        result
    }

    /// Compares a sequence of struct fields or enum-variant fields, described by the constructor at field_offset.
    fn compare_fields(self: &Self, num_fields: ItemIndex, field_offset: &mut StackAddress, a: &mut HeapRef, b: &mut HeapRef) -> bool {
        for _ in 0..num_fields {
            let field_constructor = self.construct_read_op(field_offset);
            if field_constructor != Constructor::Primitive {
                let a_field: HeapRef = self.heap.read_seq(a);
                let b_field: HeapRef = self.heap.read_seq(b);
                if !self.compare_recurse(field_constructor, a_field, b_field, field_offset) {
                    return false;
                }
            } else {
                let num_bytes = self.construct_read_index(field_offset) as StackOffset;
                if !self.heap.compare_bytes(*a, *b, num_bytes as usize) {
                    return false;
                }
                a.add_offset(num_bytes);
                b.add_offset(num_bytes);
            }
        }
        true
    }

    // === Map support ===================================================================================
    //
    // Maps use a boxed-uniform layout in the heap object's data buffer, with an inline open-addressed hash
    // index for O(1) average lookup layered over an insertion-ordered entries region:
    //   [ len: StackAddress | n_entries: StackAddress | n_buckets: StackAddress ]
    //   [ buckets: n_buckets × StackAddress ]
    //   [ entries: n_entries × (key: HeapRef, value: HeapRef) ]
    // Keys and values are boxed: each is stored inline as a HeapRef (primitives are boxed onto the heap,
    // reference values are already HeapRefs). The entries region is append-only and preserves insertion
    // order (used by iteration / keys / values); a removed entry is tombstoned by setting its key
    // reference's index to MAP_EMPTY. `len` counts live entries, `n_entries` counts appended slots
    // (including tombstones).
    //
    // Each bucket holds `entry_index + 1` for an occupied slot, or 0 for an empty slot (linear probing).
    // Buckets only ever reference live entries: they are rebuilt from the live entries on removal and on
    // growth, so no bucket tombstones are needed. Hashing (see `hash_value`) mirrors the equality used by
    // `map_box_eq` so that equal keys always collide.

    /// Header field count (len, n_entries, n_buckets), each StackAddress sized.
    pub(crate) const MAP_HEADER: StackAddress = 3 * size_of::<StackAddress>() as StackAddress;
    /// Number of buckets a freshly allocated map starts with (power of two).
    const MAP_INITIAL_BUCKETS: StackAddress = 8;
    /// Slot sentinel marking a tombstoned entry key. Uses the largest representable HeapRef index so it
    /// round-trips through `HeapRef::new`/`index` (a raw `StackAddress::MAX` would be truncated).
    pub(crate) const MAP_EMPTY: StackAddress = HeapRef::MAX_INDEX;
    /// FNV-1a offset basis, used for key hashing.
    const FNV_OFFSET: u64 = 0xcbf29ce484222325;
    /// FNV-1a prime, used for key hashing.
    const FNV_PRIME: u64 = 0x100000001b3;

    /// Returns the byte offset of the entries region for the map at `idx` (past the header and buckets).
    fn map_entries_offset(self: &Self, idx: StackAddress) -> StackAddress {
        let sa = size_of::<StackAddress>() as StackAddress;
        Self::MAP_HEADER + self.map_n_buckets(idx) * sa
    }

    /// Returns the number of hash buckets of the map at `idx`.
    fn map_n_buckets(self: &Self, idx: StackAddress) -> StackAddress {
        let sa = size_of::<StackAddress>() as StackAddress;
        self.heap.load(idx, 2 * sa)
    }

    /// Reads bucket `b` of the map at `idx` (0 = empty, else `entry_index + 1`).
    fn map_bucket_get(self: &Self, idx: StackAddress, b: StackAddress) -> StackAddress {
        let sa = size_of::<StackAddress>() as StackAddress;
        self.heap.load(idx, Self::MAP_HEADER + b * sa)
    }

    /// Writes bucket `b` of the map at `idx`.
    fn map_bucket_set(self: &mut Self, idx: StackAddress, b: StackAddress, value: StackAddress) {
        let sa = size_of::<StackAddress>() as StackAddress;
        self.heap.store(idx, Self::MAP_HEADER + b * sa, value);
    }

    /// Returns the live (non-tombstoned) (key, value) reference pairs of the map at `idx` in insertion order.
    fn map_live_entries(self: &Self, idx: StackAddress) -> Vec<(HeapRef, HeapRef)> {
        let sa = size_of::<StackAddress>() as StackAddress;
        let hr = size_of::<HeapRef>() as StackAddress;
        let n_entries: StackAddress = self.heap.load(idx, sa);
        let base = self.map_entries_offset(idx);
        let mut result = Vec::new();
        for e in 0..n_entries {
            let entry_off = base + e * 2 * hr;
            let key: HeapRef = self.heap.read(HeapRef::new(idx, entry_off));
            if key.index() == Self::MAP_EMPTY {
                continue; // tombstone
            }
            let value: HeapRef = self.heap.read(HeapRef::new(idx, entry_off + hr));
            result.push((key, value));
        }
        result
    }

    /// Given the offset of a serialized constructor, returns the offset immediately following it.
    fn constructor_skip(self: &Self, ctor_offset: StackAddress) -> StackAddress {
        let mut o = ctor_offset;
        let op = self.construct_read_op(&mut o);
        Constructor::parse_with(&self.stack, o, op).next
    }

    /// Adjusts the refcount of a boxed map key or value. Boxed primitives are leaf heap objects whose
    /// refcount is adjusted directly; boxed reference values are descended into via their constructor.
    fn refcount_box(self: &mut Self, boxed: HeapRef, box_ctor_offset: StackAddress, op: HeapRefOp, epoch: usize) {
        let mut o = box_ctor_offset;
        let box_op = self.construct_read_op(&mut o);
        if box_op == Constructor::Primitive {
            self.heap.ref_item(boxed.index(), op);
        } else if epoch != self.heap.item_epoch(boxed.index()) {
            self.refcount_value_epoch(boxed, box_ctor_offset, op, epoch);
        }
    }

    /// As [`refcount_box`], starting a fresh refcounting epoch. Used by the standalone map operations.
    fn refcount_box_top(self: &mut Self, boxed: HeapRef, box_ctor_offset: StackAddress, op: HeapRefOp) {
        let epoch = self.heap.new_epoch();
        self.refcount_box(boxed, box_ctor_offset, op, epoch);
    }

    /// Splits a map's constructor offset into its key and value sub-constructor offsets.
    fn map_sub_constructors(self: &Self, map_ctor_offset: StackAddress) -> (StackAddress, StackAddress) {
        let mut o = map_ctor_offset;
        let op = self.construct_read_op(&mut o);
        debug_assert_eq!(op, Constructor::Map);
        let parsed = Constructor::parse_with(&self.stack, o, op);
        let key_ctor = parsed.offset;
        let value_ctor = self.constructor_skip(key_ctor);
        (key_ctor, value_ctor)
    }

    /// Allocates a new empty map (header + zeroed initial buckets, no entries) and returns a reference to it.
    pub(crate) fn map_alloc(self: &mut Self) -> HeapRef {
        let sa = size_of::<StackAddress>() as usize;
        let n_buckets = Self::MAP_INITIAL_BUCKETS;
        let mut data = vec![0u8; Self::MAP_HEADER as usize + n_buckets as usize * sa];
        // len = 0, n_entries = 0 (already zeroed); store n_buckets
        data[2 * sa .. 3 * sa].copy_from_slice(&n_buckets.to_ne_bytes());
        let index = self.heap.alloc_copy(&data, ItemIndex::MAX);
        HeapRef::new(index, 0)
    }

    /// Pops a boxed key/value off the stack using the given sub-constructor. A primitive is popped as raw
    /// bytes and boxed onto the heap; a reference value is already a HeapRef and is popped directly.
    fn map_box_pop(self: &mut Self, ctor_offset: StackAddress) -> HeapRef {
        let mut o = ctor_offset;
        let op = self.construct_read_op(&mut o);
        if op == Constructor::Primitive {
            let n = self.construct_read_index(&mut o) as usize;
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

    /// Returns whether the constructor at the given offset is a (boxed) primitive.
    fn ctor_is_primitive(self: &Self, ctor_offset: StackAddress) -> bool {
        let mut o = ctor_offset;
        self.construct_read_op(&mut o) == Constructor::Primitive
    }

    /// Pushes a boxed primitive value onto the stack by copying its bytes. Reference values are pushed
    /// directly by the caller (see [`op_map_get`]).
    fn map_unbox_push_primitive(self: &mut Self, value: HeapRef, ctor_offset: StackAddress) {
        let mut o = ctor_offset;
        let _op = self.construct_read_op(&mut o);
        let n = self.construct_read_index(&mut o) as usize;
        let bytes = self.heap.item(value.index()).data[0..n].to_vec();
        self.stack.extend_from(&bytes);
    }

    /// Compares two boxed keys/values for equality using the given sub-constructor.
    fn map_box_eq(self: &Self, a: HeapRef, b: HeapRef, ctor_offset: StackAddress) -> bool {
        let mut o = ctor_offset;
        let op = self.construct_read_op(&mut o);
        if op == Constructor::Primitive {
            let n = self.construct_read_index(&mut o) as usize;
            self.heap.item(a.index()).data[0..n] == self.heap.item(b.index()).data[0..n]
        } else {
            self.compare_value(a, b, ctor_offset)
        }
    }

    /// FNV-1a hash of a byte slice seeded with `h`.
    fn fnv_bytes(mut h: u64, bytes: &[u8]) -> u64 {
        for &b in bytes {
            h ^= b as u64;
            h = h.wrapping_mul(Self::FNV_PRIME);
        }
        h
    }

    /// Hashes a boxed key consistently with [`map_box_eq`]: primitive boxes hash their raw bytes, reference
    /// keys are hashed structurally (mirroring [`compare_value`]).
    fn map_box_hash(self: &Self, boxed: HeapRef, ctor_offset: StackAddress) -> u64 {
        let mut o = ctor_offset;
        let op = self.construct_read_op(&mut o);
        if op == Constructor::Primitive {
            let n = self.construct_read_index(&mut o) as usize;
            Self::fnv_bytes(Self::FNV_OFFSET, &self.heap.item(boxed.index()).data[0..n])
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
                let constructor = self.construct_read_op(&mut constructor_offset);
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
                let element_constructor = self.construct_read_op(constructor_offset);
                if element_constructor != Constructor::Primitive {
                    let num_elements = self.heap.item(item.index()).data.len() / HeapRef::primitive_size() as usize;
                    let original_constructor_offset = *constructor_offset;
                    let mut h = Self::FNV_OFFSET;
                    for _ in 0..num_elements {
                        *constructor_offset = original_constructor_offset;
                        let element: HeapRef = self.heap.read_seq(&mut item);
                        h = (h ^ self.hash_recurse(element_constructor, element, constructor_offset)).wrapping_mul(Self::FNV_PRIME);
                    }
                    if num_elements == 0 {
                        // ensure the offset advances past the (otherwise unvisited) element constructor
                        let mut skip = original_constructor_offset;
                        let op = self.construct_read_op(&mut skip);
                        *constructor_offset = Constructor::parse_with(&self.stack, skip, op).next;
                    }
                    h
                } else {
                    self.construct_read_index(constructor_offset); // skip primitive num_bytes
                    Self::fnv_bytes(Self::FNV_OFFSET, &self.heap.item(item.index()).data)
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
                let h = Self::fnv_bytes(Self::FNV_OFFSET, &variant.to_ne_bytes());
                h ^ self.hash_fields(num_fields, &mut field_offset, &mut item)
            },
            Constructor::String => {
                Self::fnv_bytes(Self::FNV_OFFSET, self.heap.string(item).unwrap_or("").as_bytes())
            },
            // closures and maps compare by reference identity, so hash by it too
            Constructor::Closure | Constructor::Map => item.index() as u64,
            Constructor::Virtual => {
                match self.resolve_constructor_offset(item, 0) {
                    None => item.index() as u64,
                    Some(mut concrete_offset) => {
                        let constructor = self.construct_read_op(&mut concrete_offset);
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
        let mut h = Self::FNV_OFFSET;
        for _ in 0..num_fields {
            let field_constructor = self.construct_read_op(field_offset);
            if field_constructor != Constructor::Primitive {
                let field: HeapRef = self.heap.read_seq(item);
                h = (h ^ self.hash_recurse(field_constructor, field, field_offset)).wrapping_mul(Self::FNV_PRIME);
            } else {
                let num_bytes = self.construct_read_index(field_offset) as StackOffset;
                let start = item.offset() as usize;
                let bytes = self.heap.item(item.index()).data[start..start + num_bytes as usize].to_vec();
                h = (h ^ Self::fnv_bytes(Self::FNV_OFFSET, &bytes)).wrapping_mul(Self::FNV_PRIME);
                item.add_offset(num_bytes);
            }
        }
        h
    }

    /// Returns the entry index of `key` in the map at `idx`, probing the hash buckets. Boxed keys are
    /// compared by value via [`map_box_eq`].
    fn map_find(self: &Self, idx: StackAddress, key: HeapRef, key_ctor: StackAddress) -> Option<StackAddress> {
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
            if stored.index() != Self::MAP_EMPTY && self.map_box_eq(key, stored, key_ctor) {
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
    fn map_compact(self: &mut Self, idx: StackAddress) {
        let sa = size_of::<StackAddress>() as StackAddress;
        let hr = size_of::<HeapRef>() as StackAddress;
        let entry_size = 2 * hr;
        let n_entries: StackAddress = self.heap.load(idx, sa);
        let base = self.map_entries_offset(idx);
        let mut write: StackAddress = 0;
        for read in 0..n_entries {
            let read_off = base + read * entry_size;
            let key: HeapRef = self.heap.read(HeapRef::new(idx, read_off));
            if key.index() == Self::MAP_EMPTY {
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
    fn map_refill_buckets(self: &mut Self, idx: StackAddress, key_ctor: StackAddress) {
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
            if key.index() == Self::MAP_EMPTY {
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
        let mut data = Vec::with_capacity(Self::MAP_HEADER as usize + new_n_buckets as usize * sa + entries_bytes.len());
        data.extend_from_slice(&len.to_ne_bytes());
        data.extend_from_slice(&n_entries.to_ne_bytes());
        data.extend_from_slice(&new_n_buckets.to_ne_bytes());
        data.resize(Self::MAP_HEADER as usize + new_n_buckets as usize * sa, 0); // zeroed buckets
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
    fn map_put(self: &mut Self, idx: StackAddress, key: HeapRef, value: HeapRef, key_ctor: StackAddress, value_ctor: StackAddress, retain: bool) {
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

    /// Inserts or updates an entry for `m[k] = v` / `m.insert(k, v)`. Boxes are reference-counted up because
    /// the map is an already-established container that takes ownership of them. Stack on entry (top first):
    /// value, key, map-reference.
    ///
    /// Note: the map receiver is intentionally not reference-counted here. Like an array index-write target,
    /// the container reference is consumed by the store; loading it did not increment its count.
    pub(crate) fn op_map_insert(self: &mut Self, map_ctor_offset: StackAddress) {
        let (key_ctor, value_ctor) = self.map_sub_constructors(map_ctor_offset);
        let value = self.map_box_pop(value_ctor);
        let key = self.map_box_pop(key_ctor);
        let map: HeapRef = self.stack.pop();
        self.map_put(map.index(), key, value, key_ctor, value_ctor, true);
    }

    /// Appends an entry while populating a map literal. Like array-literal construction the boxes are not
    /// reference-counted here (the binding's store recursion adopts them exactly once). Duplicate keys
    /// resolve last-wins, matching Rust's map-from-iterator behaviour. Stack on entry (top first):
    /// value, key, map-reference.
    pub(crate) fn op_map_append(self: &mut Self, map_ctor_offset: StackAddress) {
        let (key_ctor, value_ctor) = self.map_sub_constructors(map_ctor_offset);
        let value = self.map_box_pop(value_ctor);
        let key = self.map_box_pop(key_ctor);
        let map: HeapRef = self.stack.pop();
        self.map_put(map.index(), key, value, key_ctor, value_ctor, false);
    }

    /// Looks up a key and pushes its value, trapping if the key is absent. Stack on entry (top first):
    /// key, map-reference.
    pub(crate) fn op_map_get(self: &mut Self, map_ctor_offset: StackAddress) {
        let (key_ctor, value_ctor) = self.map_sub_constructors(map_ctor_offset);
        let key = self.map_box_pop(key_ctor);
        let map: HeapRef = self.stack.pop();
        let idx = map.index();
        let hr = size_of::<HeapRef>() as StackAddress;
        let found = self.map_find(idx, key, key_ctor);
        self.refcount_box_top(key, key_ctor, HeapRefOp::Free);
        match found {
            Some(e) => {
                let value_off = self.map_entries_offset(idx) + e * 2 * hr + hr;
                let value: HeapRef = self.heap.read(HeapRef::new(idx, value_off));
                if self.ctor_is_primitive(value_ctor) {
                    // unbox the primitive bytes, then release the (possibly temporary) container
                    self.map_unbox_push_primitive(value, value_ctor);
                    self.refcount_value(map, map_ctor_offset, HeapRefOp::Free);
                } else {
                    // reference value: protect it from being dropped while the container is freed (the
                    // container still owns it), leaving its refcount net-unchanged for the caller.
                    self.heap.ref_item(value.index(), HeapRefOp::Inc);
                    self.refcount_value(map, map_ctor_offset, HeapRefOp::Free);
                    self.heap.ref_item(value.index(), HeapRefOp::DecNoFree);
                    self.stack.push(value);
                }
            },
            None => {
                self.state = VMState::Error(RuntimeErrorKind::KeyNotFound);
                self.refcount_value(map, map_ctor_offset, HeapRefOp::Free);
            },
        }
    }

    /// Pushes the number of live entries in the map. Stack on entry (top): map-reference.
    pub(crate) fn op_map_len(self: &mut Self, map_ctor_offset: StackAddress) {
        let map: HeapRef = self.stack.pop();
        let len: StackAddress = self.heap.load(map.index(), 0);
        self.stack.push(len);
        self.refcount_value(map, map_ctor_offset, HeapRefOp::Free);
    }

    /// Removes an entry by key. Does nothing if the key is absent. Stack on entry (top first):
    /// key, map-reference.
    pub(crate) fn op_map_remove(self: &mut Self, map_ctor_offset: StackAddress) {
        let (key_ctor, value_ctor) = self.map_sub_constructors(map_ctor_offset);
        let key = self.map_box_pop(key_ctor);
        let map: HeapRef = self.stack.pop();
        let idx = map.index();
        let hr = size_of::<HeapRef>() as StackAddress;
        if let Some(e) = self.map_find(idx, key, key_ctor) {
            let entry_off = self.map_entries_offset(idx) + e * 2 * hr;
            let stored_key: HeapRef = self.heap.read(HeapRef::new(idx, entry_off));
            let stored_value: HeapRef = self.heap.read(HeapRef::new(idx, entry_off + hr));
            // drop the stored key and value boxes
            self.refcount_box_top(stored_key, key_ctor, HeapRefOp::Dec);
            self.refcount_box_top(stored_value, value_ctor, HeapRefOp::Dec);
            // tombstone the entry (key reference index set to MAP_EMPTY) and decrement live count
            self.heap.write(HeapRef::new(idx, entry_off), HeapRef::new(Self::MAP_EMPTY, 0));
            let sa = size_of::<StackAddress>() as StackAddress;
            let len: StackAddress = self.heap.load(idx, 0);
            let len = len - 1;
            self.heap.store(idx, 0, len);
            // compact away dead slots once tombstones reach half the entries region, bounding its growth
            // under insert/remove churn. Compaction renumbers entries, so the bucket rebuild must follow it.
            let n_entries: StackAddress = self.heap.load(idx, sa);
            if n_entries >= len.saturating_mul(2) {
                self.map_compact(idx);
            }
            // rebuild the buckets so they no longer reference the removed (or relocated) entries
            // (no bucket tombstones)
            self.map_refill_buckets(idx, key_ctor);
        }
        // release the (possibly temporary) lookup key box and map receiver
        self.refcount_box_top(key, key_ctor, HeapRefOp::Free);
        self.refcount_value(map, map_ctor_offset, HeapRefOp::Free);
    }

    /// Removes all entries, dropping their key and value boxes. Stack on entry (top): map-reference.
    pub(crate) fn op_map_clear(self: &mut Self, map_ctor_offset: StackAddress) {
        let (key_ctor, value_ctor) = self.map_sub_constructors(map_ctor_offset);
        let map: HeapRef = self.stack.pop();
        let idx = map.index();
        for (key, value) in self.map_live_entries(idx) {
            self.refcount_box_top(key, key_ctor, HeapRefOp::Dec);
            self.refcount_box_top(value, value_ctor, HeapRefOp::Dec);
        }
        // reset to an empty map: drop the entries region (keeping the bucket region), zero the live/entry
        // counters and clear all buckets
        let sa = size_of::<StackAddress>() as StackAddress;
        let entries_off = self.map_entries_offset(idx);
        self.heap.item_mut(idx).data.truncate(entries_off as usize);
        self.heap.store(idx, 0, 0 as StackAddress);
        self.heap.store(idx, sa, 0 as StackAddress);
        let n_buckets = self.map_n_buckets(idx);
        for b in 0..n_buckets {
            self.map_bucket_set(idx, b, 0);
        }
        self.refcount_value(map, map_ctor_offset, HeapRefOp::Free);
    }

    /// Returns the byte size of a (boxed) primitive described by the constructor at the given offset.
    fn ctor_primitive_size(self: &Self, ctor_offset: StackAddress) -> usize {
        let mut o = ctor_offset;
        let op = self.construct_read_op(&mut o);
        debug_assert_eq!(op, Constructor::Primitive);
        self.construct_read_index(&mut o) as usize
    }

    /// Builds a new array holding the live keys (`want_values == false`) or values (`want_values == true`)
    /// of the map in insertion order and returns a reference to it. Primitive elements are unboxed into the
    /// array's inline storage; reference elements are stored as references and retained (the array becomes a
    /// co-owner). Stack on entry (top): map-reference.
    fn map_collect(self: &mut Self, map_ctor_offset: StackAddress, want_values: bool) -> HeapRef {
        let (key_ctor, value_ctor) = self.map_sub_constructors(map_ctor_offset);
        let elem_ctor = if want_values { value_ctor } else { key_ctor };
        let map: HeapRef = self.stack.pop();
        let entries = self.map_live_entries(map.index());
        let array_index = self.heap.alloc_copy(&[], 0);
        if self.ctor_is_primitive(elem_ctor) {
            let n = self.ctor_primitive_size(elem_ctor);
            for (key, value) in &entries {
                let boxed = if want_values { *value } else { *key };
                let bytes = self.heap.item(boxed.index()).data[0..n].to_vec();
                self.heap.item_mut(array_index).data.extend_from_slice(&bytes);
            }
        } else {
            // store the entry's reference values directly. Like array-literal construction (`upload`),
            // the contained references are not counted here; the surrounding store/discard reference-counts
            // the result array's contents recursively.
            for (key, value) in &entries {
                let boxed = if want_values { *value } else { *key };
                self.heap.item_mut(array_index).data.extend_from_slice(&boxed.to_ne_bytes());
            }
        }
        self.refcount_value(map, map_ctor_offset, HeapRefOp::Free);
        HeapRef::new(array_index, 0)
    }

    /// Pushes a new array of the map's live keys in insertion order. Stack on entry (top): map-reference.
    pub(crate) fn op_map_keys(self: &mut Self, map_ctor_offset: StackAddress) {
        let array = self.map_collect(map_ctor_offset, false);
        self.stack.push(array);
    }

    /// Pushes a new array of the map's live values in insertion order. Stack on entry (top): map-reference.
    pub(crate) fn op_map_values(self: &mut Self, map_ctor_offset: StackAddress) {
        let array = self.map_collect(map_ctor_offset, true);
        self.stack.push(array);
    }
}

#[cfg(feature="debugging")]
impl<T, U> VM<T, U> {
    /// Executes single bytecode instruction.
    pub fn step(self: &mut Self, context: &mut U) -> RuntimeResult<VMState> where T: VMFunc<T> + VMData<T, U> {
        if self.state != VMState::Ready && self.state != VMState::Yielded {
            return Err(RuntimeError::new(0, RuntimeErrorKind::NotReady, None));
        }
        self.exec_step(context);
        match self.state {
            VMState::Terminated if self.heap.len() > 1 => Err(RuntimeError::new(0, RuntimeErrorKind::HeapCorruption, None)),
            VMState::Error(kind) => {
                #[cfg(feature="symbols")]
                let opcode = self.describe_instruction(self.error_pc).map(|result| result.0);
                #[cfg(not(feature="symbols"))]
                let opcode = None;
                Err(RuntimeError::new(self.pc, kind, opcode))
            },
            VMState::Terminated | VMState::Yielded | VMState::Ready => Ok(self.state),
        }
    }

    /// Disassembles the bytecode and returns it as a string.
    #[cfg(feature="symbols")]
    pub fn format_program(self: &Self) -> String where T: VMFunc<T> + VMData<T, U> {
        let mut position = 0;
        let mut result = "".to_string();
        while let Some((instruction, next_position)) = self.describe_instruction(position) {
            result.push_str(&instruction);
            result.push_str("\n");
            position = next_position;
        }
        result
    }

    /// Disassembles the current bytecode instruction and returns it as a string.
    #[cfg(feature="symbols")]
    pub fn format_instruction(self: &Self) -> Option<String> where T: VMFunc<T> + VMData<T, U> {
        self.describe_instruction(self.pc).map(|result| result.0)
    }

    /// Returns the opcode for the current instruction.
    pub fn get_opcode(self: &Self) -> Option<OpCode> where T: VMFunc<T> + VMData<T, U> {
        self.read_opcode(self.pc)
    }

    /// Returns the current stack as a string.
    pub fn format_stack(self: &Self) -> String {
        format!("{:?}", self.stack)
    }

    /// Returns the current stack-frame as a string.
    pub fn format_frame(self: &Self) -> String {
        format!("{:?}", &self.stack.frame())
    }
}