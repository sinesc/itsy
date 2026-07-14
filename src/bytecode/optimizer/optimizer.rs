//! Peephole optimizer for Itsy bytecode.
//!
//! Runs after compilation and before execution. Feature-gated behind `optimizer`.

mod rewrite;
mod passes {
    pub mod const_fold;
    pub mod cnt_cancel;
    pub mod redundant_ls;
    //pub mod jump_norm;
    //pub mod store_forward;
}

use crate::bytecode::opcodes::{OpCodeData, read_opcode_data, write_opcode_data};
use crate::bytecode::{Program, VMFunc};
use crate::StackAddress;
use std::collections::HashSet;
use std::mem::size_of;

/// Run peephole optimizations on a compiled program.
///
/// Returns the optimized program. The optimizer is re-entrant: calling it
/// multiple times is safe (subsequent calls are no-ops on already-optimized code).
///
/// Passes run repeatedly in a fixpoint loop: each round re-enumerates the
/// compacted bytecode, recomputes jump targets, and runs all passes. The
/// loop stops when a full round produces no changes.
pub fn optimize<T: VMFunc<T>>(program: Program<T>) -> Program<T> {
    let mut instructions = program.instructions.clone();

    // Fixpoint loop: run passes until no more changes.
    // `cumulative_map` tracks original-pc -> current-pc across iterations so that
    // addresses stored in program metadata (vtable, FunctionMeta) can be remapped
    // correctly even after multiple optimization rounds.
    let mut cumulative_map: Vec<(StackAddress, StackAddress)> = Vec::new();

    let (optimized, optimized_size) = loop {
        let jump_targets = collect_jump_targets(&instructions);
        let output = run_passes(&instructions, &jump_targets);
        let has_removals = output.iter().any(|(_, _, d)| d.is_none());
        let (compact, iter_map, os) = build_optimized(&instructions, &output);

        // Compose this iteration's addr_map into the cumulative one.
        // iter_map: current_pc -> new_pc;  cumulative_map: orig_pc -> current_pc
        // Result:   orig_pc -> new_pc
        cumulative_map = compose_addr_maps(&cumulative_map, &iter_map);

        if !has_removals {
            break (compact, os);
        }

        instructions = compact;
    };

    let mut program = program;
    program.instructions = optimized;

    // Patch vtable entries: remap stale function addresses to new locations
    patch_vtable(&mut program, &cumulative_map, optimized_size);

    // Patch callable addresses: host_return_addr and each FunctionMeta::addr
    #[cfg(feature = "call_function")]
    {
        let patch_addr = |orig: StackAddress| -> StackAddress {
            match cumulative_map.binary_search_by_key(&orig, |(orig, _)| *orig) {
                Ok(idx) => cumulative_map[idx].1,
                Err(idx) => {
                    if idx < cumulative_map.len() {
                        cumulative_map[idx].1
                    } else {
                        optimized_size
                    }
                }
            }
        };
        program.host_return_addr = patch_addr(program.host_return_addr);
        for meta in program.functions.values_mut() {
            meta.addr = patch_addr(meta.addr);
        }
    }

    program
}

/// Compose two address maps: `prev_map` (orig -> intermediate) and `iter_map`
/// (intermediate -> new). Returns a sorted map (orig -> new).
fn compose_addr_maps(prev_map: &[(StackAddress, StackAddress)], iter_map: &[(StackAddress, StackAddress)]) -> Vec<(StackAddress, StackAddress)> {
    if prev_map.is_empty() {
        return iter_map.to_vec();
    }
    let mut composed = Vec::with_capacity(prev_map.len());
    for (orig, intermediate) in prev_map {
        let new = match iter_map.binary_search_by_key(&intermediate, |(k, _)| k) {
            Ok(idx) => iter_map[idx].1,
            Err(idx) => {
                if idx < iter_map.len() {
                    iter_map[idx].1
                } else {
                    *intermediate
                }
            }
        };
        composed.push((*orig, new));
    }
    composed
}

/// Collect all addresses that are jump/call targets from the instruction stream.
fn collect_jump_targets(instructions: &[u8]) -> HashSet<StackAddress> {
    let mut targets = HashSet::new();
    let mut pc = 0;
    while let Some((data, next_pc)) = read_opcode_data(instructions, pc) {
        match data {
            OpCodeData::jmp(addr)
            | OpCodeData::j0(addr)
            | OpCodeData::jn0(addr)
            | OpCodeData::j0_nc(addr)
            | OpCodeData::jn0_nc(addr)
            | OpCodeData::j0sa_nc(addr)
            | OpCodeData::jn0sa_nc(addr) => {
                targets.insert(addr);
            }
            OpCodeData::call(addr, _) => {
                targets.insert(addr);
            }
            // target opcode: value is a bytecode address stored as data
            OpCodeData::target(addr) => {
                targets.insert(addr as StackAddress);
            }
            // loop* opcodes: second argument is the loop start address
            OpCodeData::loops8(_, start)
            | OpCodeData::loops16(_, start)
            | OpCodeData::loops32(_, start)
            | OpCodeData::loops64(_, start)
            | OpCodeData::loopu8(_, start)
            | OpCodeData::loopu16(_, start)
            | OpCodeData::loopu32(_, start)
            | OpCodeData::loopu64(_, start) => {
                targets.insert(start);
            }
            // array_iter* opcodes: the exit argument is a jump target
            OpCodeData::array_iter8(_, exit)
            | OpCodeData::array_iter16(_, exit)
            | OpCodeData::array_iter32(_, exit)
            | OpCodeData::array_iter64(_, exit)
            | OpCodeData::array_iter_iv8(_, _, exit)
            | OpCodeData::array_iter_iv16(_, _, exit)
            | OpCodeData::array_iter_iv32(_, _, exit)
            | OpCodeData::array_iter_iv64(_, _, exit) => {
                targets.insert(exit);
            }
            _ => {}
        }
        pc = next_pc;
    }
    targets
}

/// Run all optimization passes in order.
/// Returns a list of `(original_pc, original_size, Option<OpCodeData>)` for every
/// instruction in the original stream. `None` means remove, `Some` means keep.
fn run_passes(instructions: &[u8], jump_targets: &HashSet<StackAddress>) -> Vec<(StackAddress, StackAddress, Option<OpCodeData>)> {
    let mut output = enumerate_instructions(instructions);
    passes::const_fold::fold_constants(&mut output, jump_targets);
    passes::redundant_ls::eliminate_redundant_ls(&mut output, jump_targets);
    //passes::store_forward::forward_stores(&mut output, jump_targets); // TODO: fix or remove, pass is ineffective (no changes)
    //passes::jump_norm::normalize_jumps(&mut output, jump_targets); // TODO: fix or remove, pass is ineffective (no changes)
    passes::cnt_cancel::cancel_refcounts(&mut output, jump_targets);
    output
}

/// Enumerate all instructions in the stream.
fn enumerate_instructions(instructions: &[u8]) -> Vec<(StackAddress, StackAddress, Option<OpCodeData>)> {
    let mut list = Vec::new();
    let mut pc = 0;
    while let Some((data, next_pc)) = read_opcode_data(instructions, pc) {
        let size = next_pc - pc;
        list.push((pc, size, Some(data)));
        pc = next_pc;
    }
    list
}

/// Build the compacted instruction stream from the pass output.
/// Returns `(optimized_bytes, addr_map, optimized_size)` where `addr_map` is a sorted
/// list of `(original_pc, new_pc)` pairs for all surviving instructions.
fn build_optimized(instructions: &[u8], output: &[(StackAddress, StackAddress, Option<OpCodeData>)]) -> (Vec<u8>, Vec<(StackAddress, StackAddress)>, StackAddress) {
    // First pass: compute original-pc -> new-pc mapping
    let mut new_pc: StackAddress = 0;
    let mut addr_map: Vec<(StackAddress, StackAddress)> = Vec::new();

    for (orig_pc, _orig_size, new_data) in output.iter() {
        if new_data.is_some() {
            addr_map.push((*orig_pc, new_pc));
            if let Some(data) = new_data {
                let size = estimate_size(data);
                new_pc += size;
            }
        }
    }
    let optimized_size = new_pc;

    let patch_target = |orig_target: StackAddress| -> StackAddress {
        match addr_map.binary_search_by_key(&orig_target, |(orig, _)| *orig) {
            Ok(idx) => addr_map[idx].1,
            Err(idx) => {
                if idx < addr_map.len() {
                    addr_map[idx].1
                } else {
                    optimized_size
                }
            }
        }
    };

    // Second pass: write instructions, patching jump targets
    let mut buf = Vec::with_capacity(instructions.len());
    for (_orig_pc, _orig_size, new_data) in output.iter() {
        if let Some(data) = new_data {
            let patched = patch_opcode_data(data, &patch_target);
            let start = buf.len();
            let estimated = estimate_size(&patched);
            buf.resize(start + estimated as usize, 0);
            let size = write_opcode_data(&mut buf[start..], 0, patched);
            buf.truncate(start + size);
        }
    }

    // Verify round-trip: read back each instruction and compare
    let mut pc = 0;
    let mut idx = 0;
    for (_orig_pc, _orig_size, new_data) in output.iter() {
        if let Some(original_data) = new_data {
            let patched = patch_opcode_data(original_data, &patch_target);
            if let Some((read_back, next_pc)) = read_opcode_data(&buf, pc) {
                debug_assert_eq!(read_back, patched, "round-trip mismatch at pc={} idx={}", pc, idx);
                pc = next_pc;
            }
            idx += 1;
        }
    }

    (buf, addr_map, optimized_size)
}

/// Estimate the serialized size of an OpCodeData variant.
fn estimate_size(data: &OpCodeData) -> StackAddress {
    // Compute a safe buffer size based on the opcode content.
    // Comments can carry long strings, so we need to account for that.
    let extra = match data {
        #[cfg(all(feature="symbols", feature="comments"))]
        OpCodeData::comment(s) => s.len() + size_of::<StackAddress>(),
        _ => 0,
    };
    let capacity = std::cmp::max(64, extra + 64);
    let mut buf = vec![0u8; capacity];
    write_opcode_data(&mut buf, 0, data.clone()) as StackAddress
}

/// Patch jump targets inside an OpCodeData variant.
fn patch_opcode_data(data: &OpCodeData, patch: &dyn Fn(StackAddress) -> StackAddress) -> OpCodeData {
    match data {
        OpCodeData::jmp(addr) => OpCodeData::jmp(patch(*addr)),
        OpCodeData::j0(addr) => OpCodeData::j0(patch(*addr)),
        OpCodeData::jn0(addr) => OpCodeData::jn0(patch(*addr)),
        OpCodeData::j0_nc(addr) => OpCodeData::j0_nc(patch(*addr)),
        OpCodeData::jn0_nc(addr) => OpCodeData::jn0_nc(patch(*addr)),
        OpCodeData::j0sa_nc(addr) => OpCodeData::j0sa_nc(patch(*addr)),
        OpCodeData::jn0sa_nc(addr) => OpCodeData::jn0sa_nc(patch(*addr)),
        OpCodeData::call(addr, arg_size) => OpCodeData::call(patch(*addr), *arg_size),
        OpCodeData::loops8(iter, start) => OpCodeData::loops8(*iter, patch(*start)),
        OpCodeData::loops16(iter, start) => OpCodeData::loops16(*iter, patch(*start)),
        OpCodeData::loops32(iter, start) => OpCodeData::loops32(*iter, patch(*start)),
        OpCodeData::loops64(iter, start) => OpCodeData::loops64(*iter, patch(*start)),
        OpCodeData::loopu8(iter, start) => OpCodeData::loopu8(*iter, patch(*start)),
        OpCodeData::loopu16(iter, start) => OpCodeData::loopu16(*iter, patch(*start)),
        OpCodeData::loopu32(iter, start) => OpCodeData::loopu32(*iter, patch(*start)),
        OpCodeData::loopu64(iter, start) => OpCodeData::loopu64(*iter, patch(*start)),
        // array_iter* opcodes: the exit argument is a jump target
        OpCodeData::array_iter8(elem, exit) => OpCodeData::array_iter8(*elem, patch(*exit)),
        OpCodeData::array_iter16(elem, exit) => OpCodeData::array_iter16(*elem, patch(*exit)),
        OpCodeData::array_iter32(elem, exit) => OpCodeData::array_iter32(*elem, patch(*exit)),
        OpCodeData::array_iter64(elem, exit) => OpCodeData::array_iter64(*elem, patch(*exit)),
        // array_iter_iv* opcodes: the exit argument is a jump target
        OpCodeData::array_iter_iv8(idx, elem, exit) => OpCodeData::array_iter_iv8(*idx, *elem, patch(*exit)),
        OpCodeData::array_iter_iv16(idx, elem, exit) => OpCodeData::array_iter_iv16(*idx, *elem, patch(*exit)),
        OpCodeData::array_iter_iv32(idx, elem, exit) => OpCodeData::array_iter_iv32(*idx, *elem, patch(*exit)),
        OpCodeData::array_iter_iv64(idx, elem, exit) => OpCodeData::array_iter_iv64(*idx, *elem, patch(*exit)),
        // target opcode: bytecode address stored as data, must be remapped
        OpCodeData::target(addr) => OpCodeData::target(patch(*addr as StackAddress) as u32),
        other => other.clone(),
    }
}

/// Patch vtable entries in the program const pool.
/// Remaps stale function bytecode addresses to their new locations after instruction compaction.
fn patch_vtable<T: VMFunc<T>>(program: &mut Program<T>, addr_map: &[(StackAddress, StackAddress)], optimized_size: StackAddress) {
    let Some(vtable) = &program.vtable_region else { return };
    if vtable.size == 0 {
        return;
    }

    let patch_addr = |orig: StackAddress| -> StackAddress {
        match addr_map.binary_search_by_key(&orig, |(orig, _)| *orig) {
            Ok(idx) => addr_map[idx].1,
            Err(idx) => {
                if idx < addr_map.len() {
                    addr_map[idx].1
                } else {
                    optimized_size
                }
            }
        }
    };

    let entries = vtable.size / size_of::<StackAddress>() as StackAddress;
    for i in 0..entries {
        let offset = vtable.base + i * size_of::<StackAddress>() as StackAddress;
        let orig_addr = read_stack_address(&program.consts, offset);
        let new_addr = patch_addr(orig_addr);
        if new_addr != orig_addr {
            write_stack_address(&mut program.consts, offset, new_addr);
        }
    }
}

/// Read a StackAddress from the const pool at the given byte offset (native endianness).
fn read_stack_address(consts: &[u8], offset: StackAddress) -> StackAddress {
    StackAddress::from_ne_bytes(
        consts[offset as usize..offset as usize + size_of::<StackAddress>()]
            .try_into().unwrap(),
    )
}

/// Write a StackAddress into the const pool at the given byte offset (native endianness).
fn write_stack_address(consts: &mut [u8], offset: StackAddress, value: StackAddress) {
    let o = offset as usize;
    consts[o..o + size_of::<StackAddress>()].copy_from_slice(&value.to_ne_bytes());
}
