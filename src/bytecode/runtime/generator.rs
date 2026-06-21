//! Generator support for the VM.
//!
//! A generator is an opaque heap carrier whose data buffer is a fixed header followed by the frozen
//! stack frame. The frame is stored as opaque bytes (the refcounter treats the whole object as a
//! `Closure`-like opaque value). `next()`/`value()`/`key()` read the header. The exec loop never unwinds:
//! control simply transfers between the caller frame and the (relocated) generator frame on the shared
//! stack, tracked by a stack of [`GenControl`] records.

use crate::prelude::*;
use crate::{StackAddress, ItemIndex, FrameAddress, HeapAddress};
use crate::bytecode::{HeapRef, HeapRefOp, GEN_PRIMITIVE_CTOR};
use crate::bytecode::runtime::stack::StackOp;
use super::vm::VM;

/// Layout of a generator heap object's data: a fixed header followed by the frozen stack frame.
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

impl<T, U> VM<T, U> {
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
        const REF_SIZE: usize = size_of::<HeapAddress>();
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
    pub(crate) fn refcount_generator_frame(self: &mut Self, gen_index: StackAddress, value_ctor: StackAddress, key_ctor: StackAddress, op: HeapRefOp, epoch: usize) {
        const REF_SIZE: usize = size_of::<HeapAddress>();
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
}
