//! Refcount cancellation.
//!
//! Detects adjacent pairs of non-consuming refcount operations on the same
//! heap slot where one is `Inc` and the other is `DecNoFree`.  The net effect
//! is zero — the refcount is bumped then brought back — so both instructions
//! are removed.
//!
//! Handles all orderings (`Inc`→`DecNoFree` and `DecNoFree`→`Inc`) and all
//! non-consuming variants (`cnt_16_nc`, `cnt_sa_nc`).

use crate::bytecode::HeapRefOp;
use crate::bytecode::opcodes::OpCodeData;
use crate::optimizer::rewrite::{next_real, next_real_after};
use crate::StackAddress;
use std::collections::HashSet;

/// HeapRefOp discriminant values (matches #[repr(u8)] ordering).
/// OpCodeData stores HeapRefOp as a raw u8.
const OP_INC: u8 = HeapRefOp::Inc as u8;
const OP_DEC_NO_FREE: u8 = HeapRefOp::DecNoFree as u8;

/// Remove adjacent refcount Inc/DecNoFree pairs on the same slot.
///
/// Scans for two logically-adjacent non-consuming refcount instructions
/// targeting the same heap slot where one is `Inc` and the other is
/// `DecNoFree`. Both are removed because their net effect is zero.
///
/// Both `cnt_16_nc` and `cnt_sa_nc` variants are handled.  The pair must
/// use the same variant (both 16-bit or both stack-address) so that the
/// serialized form is compatible.
pub fn cancel_refcounts(output: &mut Vec<(StackAddress, StackAddress, Option<OpCodeData>)>, jump_targets: &HashSet<StackAddress>) {
    let mut i = 0usize;
    loop {
        let Some(idx_a) = next_real(output, i) else { break };
        let Some(data_a) = &output[idx_a].2 else { i = idx_a + 1; continue };

        // Must be a non-consuming refcount instruction.
        if !is_cnt_nc(data_a) {
            i = idx_a + 1;
            continue;
        }

        let Some(idx_b) = next_real_after(output, idx_a) else { break };
        let Some(data_b) = &output[idx_b].2 else { i = idx_a + 1; continue };

        // Both instructions must not be jump targets.
        if jump_targets.contains(&output[idx_a].0)
            || jump_targets.contains(&output[idx_b].0)
        {
            i = idx_a + 1;
            continue;
        }

        if is_cancel_pair(data_a, data_b) {
            output[idx_a].2 = None;
            output[idx_b].2 = None;
        }

        i = idx_a + 1;
    }
}

/// Check if an opcode is a non-consuming refcount instruction.
fn is_cnt_nc(data: &OpCodeData) -> bool {
    matches!(
        data,
        OpCodeData::cnt_16_nc(..) | OpCodeData::cnt_sa_nc(..)
    )
}

/// Check if two non-consuming refcount instructions form a cancellable pair.
///
/// A pair is cancellable when:
/// - Both use the same variant (both `cnt_16_nc` or both `cnt_sa_nc`).
/// - Both target the same heap slot (constructor address).
/// - One is `Inc` and the other is `DecNoFree`.
///
/// Note: OpCodeData stores HeapRefOp as a raw u8.
fn is_cancel_pair(a: &OpCodeData, b: &OpCodeData) -> bool {
    // Must be opposite operations.
    let (op_a, op_b) = match (a, b) {
        (OpCodeData::cnt_16_nc(slot_a, op_a), OpCodeData::cnt_16_nc(slot_b, op_b)) => {
            if slot_a != slot_b {
                return false;
            }
            (*op_a, *op_b)
        }
        (OpCodeData::cnt_sa_nc(slot_a, op_a), OpCodeData::cnt_sa_nc(slot_b, op_b)) => {
            if slot_a != slot_b {
                return false;
            }
            (*op_a, *op_b)
        }
        _ => return false,
    };

    op_a == OP_INC && op_b == OP_DEC_NO_FREE
}
