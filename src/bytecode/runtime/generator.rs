//! Generator support for the VM.
//!
//! A generator is an opaque heap carrier whose data buffer is a fixed header followed by the frozen
//! stack frame. The frame is stored as opaque bytes (the refcounter treats the whole object as a
//! `Closure`-like opaque value). `next()`/`value()`/`key()` read the header. The exec loop never unwinds:
//! control simply transfers between the caller frame and the (relocated) generator frame on the shared
//! stack, tracked by a stack of [`GenControl`] records.

use crate::prelude::*;
use crate::{StackAddress, ItemIndex, HeapAddress};
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

    /// Reads a heap reference stored at `offset` within a generator carrier's heap object.
    fn gen_slot_ref(self: &Self, gen_index: StackAddress, offset: usize) -> HeapRef {
        const REF_SIZE: usize = size_of::<HeapAddress>();
        HeapRef::from_ne_bytes(self.heap.item(gen_index).data[offset..offset + REF_SIZE].try_into().unwrap())
    }

    /// Stores a reference-typed value into a generator header slot with replace semantics (retain new, release old). No-op for primitive slots.
    pub(crate) fn gen_replace_slot(self: &mut Self, gen_index: StackAddress, offset: usize, value_bytes: &[ u8 ], constructor_offset: StackAddress) {
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

    /// Releases (with `Dec`) the reference held in a generator header slot, if reference-typed and non-null. No-op for primitive slots.
    pub(crate) fn gen_release_slot(self: &mut Self, gen_index: StackAddress, offset: usize, constructor_offset: StackAddress) {
        if constructor_offset == GEN_PRIMITIVE_CTOR {
            return;
        }
        let reference = self.gen_slot_ref(gen_index, offset);
        if reference.index() != 0 {
            self.refcount_value(reference, constructor_offset, HeapRefOp::Dec);
        }
    }

    /// Writes the generator header (state, resume PC, live-ref map, yielded value/key) and freezes the current frame for a yield.
    pub(crate) fn gen_suspend(self: &mut Self, gen_index: StackAddress, resume_pc: StackAddress, live_ref_map: StackAddress, value_bytes: &[ u8 ], value_size: usize, key: Option<(&[ u8 ], usize)>, frame: Vec<u8>) {
        if let Some((key_bytes, _key_size)) = key {
            self.gen_replace_slot(gen_index, GEN_KEY_OFFSET, key_bytes, GEN_PRIMITIVE_CTOR);
        }
        let data = &mut self.heap.item_mut(gen_index).data;
        data[GEN_STATE_OFFSET] = GEN_SUSPENDED;
        data[GEN_PC_OFFSET..GEN_PC_OFFSET + size_of::<StackAddress>()].copy_from_slice(&resume_pc.to_ne_bytes());
        data[GEN_YIELD_OFFSET..GEN_YIELD_OFFSET + size_of::<StackAddress>()].copy_from_slice(&live_ref_map.to_ne_bytes());
        data[GEN_VALUE_OFFSET..GEN_VALUE_OFFSET + value_size].copy_from_slice(value_bytes);
        if let Some((key_bytes, key_size)) = key {
            data[GEN_KEY_OFFSET..GEN_KEY_OFFSET + key_size].copy_from_slice(key_bytes);
        }
        data.truncate(GEN_FRAME_OFFSET);
        data.extend_from_slice(&frame);
    }

    /// Restores the caller's stack and program counter after a yield or return, and pushes the result value.
    pub(crate) fn gen_restore_caller(self: &mut Self, control: GenControl, result: u8) {
        self.gen_control.pop();
        self.stack.truncate(control.caller_sp);
        self.stack.fp = control.caller_fp;
        self.pc = control.caller_pc;
        self.stack.push(result);
    }

    /// Releases the live heap references held by a generator carrier: the ref-typed slots of its frozen frame plus the yielded value/key in the header.
    /// Skips `Done`/`Running` generators (already released). The header records the const-pool offset of the live-ref-map for the current suspension point;
    /// the map is a `count` followed by `(frame_offset, constructor_offset)` pairs, each non-null reference is refcounted.
    pub(crate) fn refcount_generator_frame(self: &mut Self, gen_index: StackAddress, value_ctor: StackAddress, key_ctor: StackAddress, op: HeapRefOp) {
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
            self.refcount_value(reference, constructor_offset, op);
        }
    }
}
