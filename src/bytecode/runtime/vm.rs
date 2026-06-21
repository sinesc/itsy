//! A virtual machine for running Itsy bytecode.

use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex, VariantIndex, FrameAddress};
use crate::bytecode::{HeapRef, HeapRefOp, Constructor, GEN_PRIMITIVE_CTOR, Program, ConstDescriptor, ConstEndianness, VMFunc, VMData, runtime::{stack::{Stack, StackOp}, heap::{Heap, HeapOp, HeapCmp}, error::*}};
#[cfg(feature="debugging")]
use crate::bytecode::opcodes::OpCode;

/// Layout of a generator heap object's data: a fixed header followed by the frozen stack frame.
/// The frame is stored as opaque bytes (the refcounter treats the whole object as a `Closure`-like
/// opaque value), per the milestone-2 design-in constraint. `next()`/`value()`/`key()` read the header.
pub(crate) const GEN_STATE_OFFSET: usize = 0;                                        // u8 generator state
pub(crate) const GEN_PC_OFFSET: usize = GEN_STATE_OFFSET + 1;                        // StackAddress resume program counter
pub(crate) const GEN_YIELD_OFFSET: usize = GEN_PC_OFFSET + size_of::<StackAddress>();// StackAddress const-pool offset of the current suspension point's live-ref-map (drop cleanup)
pub(crate) const GEN_VALUE_OFFSET: usize = GEN_YIELD_OFFSET + size_of::<StackAddress>(); // last yielded value
pub(crate) const GEN_VALUE_SIZE: usize = 8;                                          // value slot holds any primitive (<= 8 bytes)
pub(crate) const GEN_KEY_OFFSET: usize = GEN_VALUE_OFFSET + GEN_VALUE_SIZE;          // last yielded key (`Generator<K, V>` only; unused otherwise)
pub(crate) const GEN_KEY_SIZE: usize = 8;                                            // key slot holds any primitive (<= 8 bytes)
pub(crate) const GEN_FRAME_OFFSET: usize = GEN_KEY_OFFSET + GEN_KEY_SIZE;            // frozen frame bytes follow

/// Generator state stored at `GEN_STATE_OFFSET`.
pub(crate) const GEN_NOT_STARTED: u8 = 0;
pub(crate) const GEN_SUSPENDED: u8 = 1;
pub(crate) const GEN_DONE: u8 = 2;
pub(crate) const GEN_RUNNING: u8 = 3;

/// Caller-resume bookkeeping pushed by `gen_next` and consumed by `gen_yield`/`gen_return` to transfer
/// control back to the code that resumed the generator. The exec loop never unwinds; control simply
/// transfers between the caller frame and the (relocated) generator frame on the shared stack.
#[derive(Copy, Clone, Debug)]
pub(crate) struct GenControl {
    /// Frame pointer to restore for the caller.
    pub caller_fp: StackAddress,
    /// Stack pointer to truncate back to (also where the generator frame was copied and where
    /// `next()`'s bool result is written).
    pub caller_sp: StackAddress,
    /// Program counter at which the caller continues after `next()`.
    pub caller_pc: StackAddress,
    /// Heap index of the generator being driven.
    pub gen_index: StackAddress,
}

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
    /// Stack of in-flight generator resumptions (see [`GenControl`]).
    pub(crate) gen_control  : Vec<GenControl>,
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
            gen_control : Vec::new(),
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
        self.gen_control.clear();
    }
}

/// Internal VM methods.
impl<T, U> VM<T, U> {
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

    /// Constructs a generator object from the arguments currently on the stack and pushes a reference to
    /// it. The entry address was pushed (forward-reference safe) immediately above the arguments and is
    /// popped here. The generator does not run yet; its initial frame is `ARGS | prev_fp | prev_pc`
    /// (the locals are reserved by the function prologue on the first `next()`).
    pub(crate) fn gen_make_impl(self: &mut Self, arg_size: FrameAddress, entry_map: StackAddress) {
        let entry: StackAddress = self.stack.pop();
        let arg_size = arg_size as usize;
        let data_start = self.stack.sp() as usize - arg_size;
        let mut data = Vec::with_capacity(GEN_FRAME_OFFSET + arg_size + 2 * size_of::<StackAddress>());
        data.push(GEN_NOT_STARTED);
        data.extend_from_slice(&entry.to_ne_bytes());                   // resume pc = function entry
        data.extend_from_slice(&entry_map.to_ne_bytes());              // live-ref-map for the captured args (NotStarted drop cleanup)
        data.resize(GEN_FRAME_OFFSET, 0);                              // zero the value slot
        data.extend_from_slice(&self.stack.data()[data_start..data_start + arg_size]); // captured args
        data.resize(data.len() + 2 * size_of::<StackAddress>(), 0);     // prev_fp / prev_pc slots
        let index = self.heap.alloc_place(data, ItemIndex::MAX);
        self.stack.truncate(data_start as StackAddress);
        self.stack.push(HeapRef::new(index, 0));
    }

    /// Resumes the generator referenced on the stack top: copies its frozen frame onto the live stack,
    /// records caller-resume bookkeeping and transfers control into the generator body. The bool result
    /// of `next()` is written later by `gen_yield`/`gen_return` (or here, immediately, if already done).
    pub(crate) fn gen_next_impl(self: &mut Self) {
        let gen_ref: HeapRef = self.stack.pop();
        let gen_index = gen_ref.index();
        if self.heap.item(gen_index).data[GEN_STATE_OFFSET] == GEN_DONE {
            self.stack.push(0u8); // next() == false
            return;
        }
        let resume_pc = StackAddress::from_ne_bytes(self.heap.item(gen_index).data[GEN_PC_OFFSET..GEN_PC_OFFSET + size_of::<StackAddress>()].try_into().unwrap());
        let frame = self.heap.item(gen_index).data[GEN_FRAME_OFFSET..].to_vec();
        let caller_sp = self.stack.sp();
        self.gen_control.push(GenControl { caller_fp: self.stack.fp, caller_sp, caller_pc: self.pc, gen_index });
        self.stack.extend_from(&frame);
        self.stack.fp = caller_sp;
        self.pc = resume_pc;
        self.heap.item_mut(gen_index).data[GEN_STATE_OFFSET] = GEN_RUNNING;
    }

    /// Suspends the running generator: stashes the yielded value, freezes the current frame back into the
    /// generator object, restores the caller and pushes `true` as the result of the driving `next()`.
    pub(crate) fn gen_yield_impl(self: &mut Self, live_ref_map: StackAddress, value_ctor: StackAddress, value_size: FrameAddress) {
        let value_size = value_size as usize;
        // pop the yielded value off the stack top
        let sp = self.stack.sp() as usize;
        let value_bytes = self.stack.data()[sp - value_size..sp].to_vec();
        self.stack.truncate((sp - value_size) as StackAddress);
        // freeze [fp..sp]; with the value popped, sp is exactly the end of the frame
        let fp = self.stack.fp as usize;
        let frame_end = self.stack.sp() as usize;
        let frame = self.stack.data()[fp..frame_end].to_vec();
        let resume_pc = self.pc;
        let control = *self.gen_control.last().expect("yield outside generator resumption");
        // a reference-typed value is retained by the value slot (replace semantics: release the previous slot value)
        self.gen_replace_slot(control.gen_index, GEN_VALUE_OFFSET, &value_bytes, value_ctor);
        {
            let data = &mut self.heap.item_mut(control.gen_index).data;
            data[GEN_STATE_OFFSET] = GEN_SUSPENDED;
            data[GEN_PC_OFFSET..GEN_PC_OFFSET + size_of::<StackAddress>()].copy_from_slice(&resume_pc.to_ne_bytes());
            data[GEN_YIELD_OFFSET..GEN_YIELD_OFFSET + size_of::<StackAddress>()].copy_from_slice(&live_ref_map.to_ne_bytes());
            data[GEN_VALUE_OFFSET..GEN_VALUE_OFFSET + value_size].copy_from_slice(&value_bytes);
            data.truncate(GEN_FRAME_OFFSET);
            data.extend_from_slice(&frame);
        }
        self.gen_control.pop();
        self.stack.truncate(control.caller_sp);
        self.stack.fp = control.caller_fp;
        self.pc = control.caller_pc;
        self.stack.push(1u8); // next() == true
    }

    /// Keyed variant of [`gen_yield_impl`]: the stack top holds `key` then `value` (value on top, key
    /// below). Both are stashed into the generator object before the frame is frozen and control returns
    /// to the driving `next()`.
    pub(crate) fn gen_yield_kv_impl(self: &mut Self, live_ref_map: StackAddress, key_ctor: StackAddress, value_ctor: StackAddress, key_size: FrameAddress, value_size: FrameAddress) {
        let key_size = key_size as usize;
        let value_size = value_size as usize;
        // pop the yielded value (top) and key (below it) off the stack
        let sp = self.stack.sp() as usize;
        let value_bytes = self.stack.data()[sp - value_size..sp].to_vec();
        let key_bytes = self.stack.data()[sp - value_size - key_size..sp - value_size].to_vec();
        self.stack.truncate((sp - value_size - key_size) as StackAddress);
        // freeze [fp..sp]; with key and value popped, sp is exactly the end of the frame
        let fp = self.stack.fp as usize;
        let frame_end = self.stack.sp() as usize;
        let frame = self.stack.data()[fp..frame_end].to_vec();
        let resume_pc = self.pc;
        let control = *self.gen_control.last().expect("yield outside generator resumption");
        // reference-typed key/value are retained by their slots (replace semantics)
        self.gen_replace_slot(control.gen_index, GEN_VALUE_OFFSET, &value_bytes, value_ctor);
        self.gen_replace_slot(control.gen_index, GEN_KEY_OFFSET, &key_bytes, key_ctor);
        {
            let data = &mut self.heap.item_mut(control.gen_index).data;
            data[GEN_STATE_OFFSET] = GEN_SUSPENDED;
            data[GEN_PC_OFFSET..GEN_PC_OFFSET + size_of::<StackAddress>()].copy_from_slice(&resume_pc.to_ne_bytes());
            data[GEN_YIELD_OFFSET..GEN_YIELD_OFFSET + size_of::<StackAddress>()].copy_from_slice(&live_ref_map.to_ne_bytes());
            data[GEN_VALUE_OFFSET..GEN_VALUE_OFFSET + value_size].copy_from_slice(&value_bytes);
            data[GEN_KEY_OFFSET..GEN_KEY_OFFSET + key_size].copy_from_slice(&key_bytes);
            data.truncate(GEN_FRAME_OFFSET);
            data.extend_from_slice(&frame);
        }
        self.gen_control.pop();
        self.stack.truncate(control.caller_sp);
        self.stack.fp = control.caller_fp;
        self.pc = control.caller_pc;
        self.stack.push(1u8); // next() == true
    }

    /// Completes the running generator (its body returned or fell off the end): marks it done, releases
    /// the frozen frame, restores the caller and pushes `false` as the result of the driving `next()`.
    pub(crate) fn gen_return_impl(self: &mut Self, value_ctor: StackAddress, key_ctor: StackAddress) {
        let control = *self.gen_control.last().expect("generator return outside generator resumption");
        self.gen_control.pop();
        // release the last-yielded reference held in the value/key slots (value()/key() are illegal once done)
        self.gen_release_slot(control.gen_index, GEN_VALUE_OFFSET, value_ctor);
        self.gen_release_slot(control.gen_index, GEN_KEY_OFFSET, key_ctor);
        {
            let data = &mut self.heap.item_mut(control.gen_index).data;
            data[GEN_STATE_OFFSET] = GEN_DONE;
            data.truncate(GEN_FRAME_OFFSET); // a done generator never resumes
        }
        self.stack.truncate(control.caller_sp);
        self.stack.fp = control.caller_fp;
        self.pc = control.caller_pc;
        self.stack.push(0u8); // next() == false
    }

    /// Reads the generator's last-yielded value (its size known at compile time) and pushes it. The
    /// generator reference on the stack top is consumed (borrowed; its refcount is owned by the caller).
    pub(crate) fn gen_value_impl(self: &mut Self, value_size: FrameAddress) {
        let value_size = value_size as usize;
        let gen_ref: HeapRef = self.stack.pop();
        let value_bytes = self.heap.item(gen_ref.index()).data[GEN_VALUE_OFFSET..GEN_VALUE_OFFSET + value_size].to_vec();
        self.stack.extend_from(&value_bytes);
    }

    /// Reads the generator's last-yielded key (`Generator<K, V>` only; its size known at compile time)
    /// and pushes it. The generator reference on the stack top is consumed (borrowed; its refcount is
    /// owned by the caller).
    pub(crate) fn gen_key_impl(self: &mut Self, key_size: FrameAddress) {
        let key_size = key_size as usize;
        let gen_ref: HeapRef = self.stack.pop();
        let key_bytes = self.heap.item(gen_ref.index()).data[GEN_KEY_OFFSET..GEN_KEY_OFFSET + key_size].to_vec();
        self.stack.extend_from(&key_bytes);
    }

    /// Reads a heap reference stored at `offset` within a generator carrier's heap object.
    fn gen_slot_ref(self: &Self, gen_index: StackAddress, offset: usize) -> HeapRef {
        const REF_SIZE: usize = size_of::<crate::HeapAddress>();
        HeapRef::from_ne_bytes(self.heap.item(gen_index).data[offset..offset + REF_SIZE].try_into().unwrap())
    }

    /// Stores a (about to be written) reference-typed value into a generator header slot with replace
    /// semantics: retains the new reference and releases the one it overwrites. A no-op for primitive
    /// slots (`GEN_PRIMITIVE_CTOR`). The actual byte write into the slot is performed by the caller.
    fn gen_replace_slot(self: &mut Self, gen_index: StackAddress, offset: usize, value_bytes: &[ u8 ], constructor_offset: StackAddress) {
        if constructor_offset == GEN_PRIMITIVE_CTOR {
            return;
        }
        let next = HeapRef::from_ne_bytes(value_bytes.try_into().expect("reference-typed generator slot value is not heap-reference sized"));
        let prev = self.gen_slot_ref(gen_index, offset);
        if next != prev {
            self.refcount_value(next, constructor_offset, HeapRefOp::Inc);
            // prev may be null on the first yield (the slot starts zeroed)
            if prev.index() != 0 {
                self.refcount_value(prev, constructor_offset, HeapRefOp::Dec);
            }
        }
    }

    /// Releases (with `Dec`) the reference held in a generator header slot, if it is reference-typed and
    /// non-null. Used on generator completion; a no-op for primitive slots (`GEN_PRIMITIVE_CTOR`).
    fn gen_release_slot(self: &mut Self, gen_index: StackAddress, offset: usize, constructor_offset: StackAddress) {
        if constructor_offset == GEN_PRIMITIVE_CTOR {
            return;
        }
        let reference = self.gen_slot_ref(gen_index, offset);
        if reference.index() != 0 {
            self.refcount_value(reference, constructor_offset, HeapRefOp::Dec);
        }
    }

    /// Releases the live heap references held by a (to-be-freed) generator carrier: the ref-typed slots
    /// of its frozen frame plus the yielded value/key held in the header. The carrier's header records
    /// the const-pool offset of the live-ref-map for its current suspension point (the entry point while
    /// `NotStarted`, the active `yield` while `Suspended`); a `Done`/`Running` generator holds no
    /// releasable refs (a done generator already released its slots in `gen_return`). The map is a
    /// `count` followed by `count` pairs of `(frame_offset, constructor_offset)`; each slot whose stored
    /// reference is non-null is refcounted. `value_ctor`/`key_ctor` (from the carrier's
    /// `Constructor::Generator`) release the header value/key slots unless `GEN_PRIMITIVE_CTOR`.
    fn refcount_generator_frame(self: &mut Self, gen_index: StackAddress, value_ctor: StackAddress, key_ctor: StackAddress, op: HeapRefOp, epoch: usize) {
        const REF_SIZE: usize = size_of::<crate::HeapAddress>();
        // collect (reference, constructor_offset) pairs first to release the immutable borrows before refcounting
        let mut live: Vec<(HeapRef, StackAddress)> = Vec::new();
        {
            let data = &self.heap.item(gen_index).data;
            let state = data[GEN_STATE_OFFSET];
            if state == GEN_DONE || state == GEN_RUNNING {
                return;
            }
            let map_offset = StackAddress::from_ne_bytes(data[GEN_YIELD_OFFSET..GEN_YIELD_OFFSET + size_of::<StackAddress>()].try_into().unwrap());
            let count: ItemIndex = self.stack.load(map_offset as StackAddress);
            let mut entry_offset = map_offset + size_of::<ItemIndex>();
            for _ in 0..count {
                let frame_offset: StackAddress = self.stack.load(entry_offset as StackAddress);
                entry_offset += size_of::<StackAddress>();
                let constructor_offset: StackAddress = self.stack.load(entry_offset as StackAddress);
                entry_offset += size_of::<StackAddress>();
                let slot = GEN_FRAME_OFFSET + frame_offset;
                let reference = HeapRef::from_ne_bytes(data[slot..slot + REF_SIZE].try_into().unwrap());
                // a null reference (heap index 0) marks an uninitialized/maybe-uninitialized slot: skip it
                if reference.index() != 0 {
                    live.push((reference, constructor_offset));
                }
            }
            // the yielded value/key held in the header are released too (Suspended only; NotStarted slots are null)
            for (slot, ctor) in [ (GEN_VALUE_OFFSET, value_ctor), (GEN_KEY_OFFSET, key_ctor) ] {
                if ctor != GEN_PRIMITIVE_CTOR {
                    let reference = HeapRef::from_ne_bytes(data[slot..slot + REF_SIZE].try_into().unwrap());
                    if reference.index() != 0 {
                        live.push((reference, ctor));
                    }
                }
            }
        }
        for (reference, constructor_offset) in live {
            self.refcount_value_epoch(reference, constructor_offset, op, epoch);
        }
    }

    /// Resolves a value's constructor offset, following the indirection used for trait implementors (offset 0,
    /// looked up via the object's implementor index) and dynamic constructors (offset 1, stored at the end of the
    /// object). Returns `None` if the object has no constructor, i.e. contains no nested heap references.
    pub(crate) fn resolve_constructor_offset(self: &Self, item: HeapRef, mut constructor_offset: StackAddress) -> Option<StackAddress> {
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
    pub(crate) fn refcount_value_epoch(self: &mut Self, item: HeapRef, constructor_offset: StackAddress, op: HeapRefOp, epoch: usize) {
        match self.resolve_constructor_offset(item, constructor_offset) {
            None => self.heap.ref_item(item.index(), op),
            Some(mut constructor_offset) => {
                let constructor = Constructor::read_op(&self.stack, &mut constructor_offset);
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
                    let element_constructor = Constructor::read_op(&self.stack, constructor_offset);
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
                        Constructor::read_index(&self.stack, constructor_offset);
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
                Constructor::Generator => {
                    // A generator carrier's frozen frame (ref-typed args/locals) and its yielded value/key
                    // slots hold heap references owned exclusively by the carrier, so they are released
                    // exactly once, when the carrier itself is freed — never touched as its refcount
                    // otherwise rises and falls (so Inc/DecNoFree skip them). The Generator constructor
                    // carries the value/key constructor offsets; the frame refs come from the live-ref-map
                    // recorded in the carrier's header.
                    if op == HeapRefOp::Dec || op == HeapRefOp::Free {
                        let value_ctor: StackAddress = self.stack.load(parsed.offset);
                        let key_ctor: StackAddress = self.stack.load(parsed.offset + size_of::<StackAddress>() as StackAddress);
                        self.refcount_generator_frame(item_index, value_ctor, key_ctor, op, epoch);
                    }
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
                    let value_ctor_offset = Constructor::skip(&self.stack, key_ctor_offset);
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
            let field_constructor = Constructor::read_op(&self.stack, field_offset);
            if field_constructor != Constructor::Primitive {
                let field: HeapRef = self.heap.read_seq(item);
                let field_index = field.index();
                if epoch != self.heap.item_epoch(field_index) {
                    self.refcount_recurse(field_constructor, field, field_offset, op, epoch);
                    // fixme: skip when epoch matches, constructor_offset will be wrong otherwise
                }
            } else {
                let num_bytes = Constructor::read_index(&self.stack, field_offset) as StackOffset;
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
                let constructor = Constructor::read_op(&self.stack, &mut constructor_offset);
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
                let element_constructor = Constructor::read_op(&self.stack, constructor_offset);
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
                    Constructor::read_index(&self.stack, constructor_offset); // skip primitive num_bytes
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
            Constructor::Closure | Constructor::Generator => {
                // closures and generators are opaque: equal only if the same heap object is referenced
                a.index() == b.index()
            },
            Constructor::Map => {
                a.index() == b.index() // FIXME: this compares reference only
            },
            Constructor::Virtual => {
                // trait-object references: resolve each operand's concrete constructor via its implementor
                // index. Differing concrete types are never equal; otherwise compare guided by that constructor.
                match (self.resolve_constructor_offset(a, 0), self.resolve_constructor_offset(b, 0)) {
                    (None, None) => a.index() == b.index(),
                    (Some(a_offset), Some(b_offset)) if a_offset == b_offset => {
                        let mut a_offset = a_offset;
                        let constructor = Constructor::read_op(&self.stack, &mut a_offset);
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
            let field_constructor = Constructor::read_op(&self.stack, field_offset);
            if field_constructor != Constructor::Primitive {
                let a_field: HeapRef = self.heap.read_seq(a);
                let b_field: HeapRef = self.heap.read_seq(b);
                if !self.compare_recurse(field_constructor, a_field, b_field, field_offset) {
                    return false;
                }
            } else {
                let num_bytes = Constructor::read_index(&self.stack, field_offset) as StackOffset;
                if !self.heap.compare_bytes(*a, *b, num_bytes as usize) {
                    return false;
                }
                a.add_offset(num_bytes);
                b.add_offset(num_bytes);
            }
        }
        true
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