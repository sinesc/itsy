//! Pass 5: Redundant load/store elimination.
//!
//! Detects two patterns:
//!
//! **Basic:** `load*(loc)` immediately followed by `store*(loc)` — the value
//! is loaded onto the stack and immediately stored back to the same location.
//! Net effect: nothing changed. Both instructions are removed.
//!
//! **Clone variant:** `load*(loc)` → `clone` → `store*(loc)` — the value is
//! loaded, cloned (duplicated on stack), then one copy is stored back to the
//! same location. Net effect: one copy on stack, location unchanged.
//! Replacing with just `load*(loc)` gives the identical result.
//! The clone and store are both removed.
//!
//! **Constraint:** Only applies to primitive types. Reference types have
//! refcounting side effects that make this unsafe without deeper analysis.

use crate::bytecode::opcodes::OpCodeData;
use crate::{FrameAddress, StackAddress};
use std::collections::HashSet;

/// Remove redundant load-then-store sequences.
///
/// **Basic pattern:** `load(loc) → store(loc)` — remove both.
/// The load pushes the value onto the stack; the store pops it back into
/// the same location. Net effect: the frame location is unchanged and the
/// stack is balanced.
///
/// **Clone variant:** `load(loc) → clone → store(loc)` — remove clone + store.
/// The load pushes the value, the clone duplicates it, and the store pops one
/// copy back to the same location. Net effect: one copy on stack, location
/// unchanged. Keeping just `load(loc)` gives the identical result.
///
/// Returns `true` if any instructions were removed.
pub fn eliminate_redundant_ls(output: &mut Vec<(StackAddress, StackAddress, Option<OpCodeData>)>, jump_targets: &HashSet<StackAddress>) -> bool {
    let mut changed = false;

    // ------------------------------------------------------------------
    // Basic pattern: load(loc) → store(loc)
    // ------------------------------------------------------------------
    if output.len() >= 2 {
        for i in 0..output.len().saturating_sub(1) {
            let Some(data_load) = &output[i].2 else { continue };
            let Some(data_store) = &output[i + 1].2 else { continue };

            // Neither instruction may be a jump target.
            if jump_targets.contains(&output[i].0)
                || jump_targets.contains(&output[i + 1].0)
            {
                continue;
            }

            if match_load_store(data_load, data_store).is_some() {
                output[i].2 = None; // load removed
                output[i + 1].2 = None; // store removed
                changed = true;
            }
        }
    }

    // ------------------------------------------------------------------
    // Clone variant: load(loc) → clone → store(loc)
    // ------------------------------------------------------------------
    if output.len() >= 3 {
        for i in 0..output.len().saturating_sub(2) {
            let Some(data_load) = &output[i].2 else { continue };
            let Some(data_mid) = &output[i + 1].2 else { continue };
            let Some(data_store) = &output[i + 2].2 else { continue };

            // None of the three instructions may be a jump target.
            if jump_targets.contains(&output[i].0)
                || jump_targets.contains(&output[i + 1].0)
                || jump_targets.contains(&output[i + 2].0)
            {
                continue;
            }

            if match_load_store(data_load, data_store).is_some() {
                if is_clone(data_mid) {
                    // Remove the clone and store; keep the load.
                    // load(loc): stack [v]
                    // clone:     stack [v, v]
                    // store(loc): stack [v], loc = v (unchanged)
                    // → just load(loc): stack [v], loc unchanged (same)
                    output[i + 1].2 = None; // clone removed
                    output[i + 2].2 = None; // store removed
                    changed = true;
                }
            }
        }
    }
    changed
}

/// Try to match a `load(loc)` / `store(loc)` pair at the same location.
///
/// Returns `Some(loc)` if the two instructions form a valid pair, `None`
/// otherwise.  The load and store must use the same type size.
fn match_load_store(load: &OpCodeData, store: &OpCodeData) -> Option<FrameAddress> {
    if let (OpCodeData::load8(l), OpCodeData::store8(s)) = (load, store) {
        if l == s { return Some(*l); }
    }
    if let (OpCodeData::load16(l), OpCodeData::store16(s)) = (load, store) {
        if l == s { return Some(*l); }
    }
    if let (OpCodeData::load32(l), OpCodeData::store32(s)) = (load, store) {
        if l == s { return Some(*l); }
    }
    if let (OpCodeData::load64(l), OpCodeData::store64(s)) = (load, store) {
        if l == s { return Some(*l); }
    }
    None
}

/// Check if an opcode is a `clone` instruction.
fn is_clone(data: &OpCodeData) -> bool {
    matches!(data, OpCodeData::clone(_))
}
