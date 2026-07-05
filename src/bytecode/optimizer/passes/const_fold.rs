//! Constant folding (immediate arithmetic).
//!
//! Folds adjacent `immediate` + `immediate` + `arithmetic-op` sequences into a
//! single `immediate` with the precomputed result.  Also folds `immediate` +
//! `unary-op` sequences.
//!
//! Only folds when **both** operands are immediate pushes.  Does not propagate
//! folded constants into subsequent non-immediate operations (that would require
//! a larger analysis).

use crate::bytecode::opcodes::OpCodeData;
use crate::optimizer::rewrite::{next_real, next_real_after};
use crate::StackAddress;
use std::collections::HashSet;

/// Fold constant arithmetic where both operands are immediates.
/// Returns `true` if any instructions were folded.
pub fn fold_constants(output: &mut Vec<(StackAddress, StackAddress, Option<OpCodeData>)>, jump_targets: &HashSet<StackAddress>) -> bool {
    if output.len() < 3 {
        return false;
    }

    let mut changed = false;

    // Scan for binary-op patterns: immediate(A) + immediate(B) + op
    // Use logical adjacency: skip over comment instructions.
    let mut i = 0usize;
    loop {
        let Some(idx_a) = next_real(output, i) else { break };
        let Some(data_a) = &output[idx_a].2 else { i = idx_a + 1; continue };
        let Some(idx_b) = next_real_after(output, idx_a) else { break };
        let Some(data_b) = &output[idx_b].2 else { i = idx_a + 1; continue };
        let Some(idx_op) = next_real_after(output, idx_b) else { break };
        let Some(data_op) = &output[idx_op].2 else { i = idx_a + 1; continue };

        // Safety: instructions in the middle of a fold must not be jump targets.
        if jump_targets.contains(&output[idx_b].0)
            || jump_targets.contains(&output[idx_op].0)
        {
            i = idx_a + 1;
            continue;
        }

        if let Some(folded) = try_fold_binary(data_a, data_b, data_op) {
            output[idx_a].2 = Some(folded);
            output[idx_b].2 = None;
            output[idx_op].2 = None;
            changed = true;
        }
        i = idx_a + 1;
    }

    // Scan for unary-op patterns: immediate(A) + unary_op
    // Use logical adjacency: skip over comment instructions.
    let mut i = 0usize;
    loop {
        let Some(idx_a) = next_real(output, i) else { break };
        let Some(data_a) = &output[idx_a].2 else { i = idx_a + 1; continue };
        let Some(idx_op) = next_real_after(output, idx_a) else { break };
        let Some(data_op) = &output[idx_op].2 else { i = idx_a + 1; continue };

        if jump_targets.contains(&output[idx_op].0) {
            i = idx_a + 1;
            continue;
        }

        if let Some(folded) = try_fold_unary(data_a, data_op) {
            output[idx_a].2 = Some(folded);
            output[idx_op].2 = None;
            changed = true;
        }
        i = idx_a + 1;
    }
    changed
}

// ---------------------------------------------------------------------------
// Binary folding
// ---------------------------------------------------------------------------

fn try_fold_binary(a: &OpCodeData, b: &OpCodeData, op: &OpCodeData) -> Option<OpCodeData> {
    match op {
        // --- Integer addition (wrapping, same as VM overflow semantics) ---
        OpCodeData::addu8(..) => fold_imm_binary::<u8, _>(a, b, |x, y| x.wrapping_add(y)),
        OpCodeData::addu16(..) => fold_imm_binary::<u16, _>(a, b, |x, y| x.wrapping_add(y)),
        OpCodeData::addu32(..) => fold_imm_binary::<u32, _>(a, b, |x, y| x.wrapping_add(y)),
        OpCodeData::addu64(..) => fold_imm_binary::<u64, _>(a, b, |x, y| x.wrapping_add(y)),
        OpCodeData::adds8(..) => fold_imm_binary::<i8, _>(a, b, |x, y| x.wrapping_add(y)),
        OpCodeData::adds16(..) => fold_imm_binary::<i16, _>(a, b, |x, y| x.wrapping_add(y)),
        OpCodeData::adds32(..) => fold_imm_binary::<i32, _>(a, b, |x, y| x.wrapping_add(y)),
        OpCodeData::adds64(..) => fold_imm_binary::<i64, _>(a, b, |x, y| x.wrapping_add(y)),
        OpCodeData::addf32(..) => fold_imm_binary::<f32, _>(a, b, |x, y| x + y),
        OpCodeData::addf64(..) => fold_imm_binary::<f64, _>(a, b, |x, y| x + y),

        // --- Integer subtraction ---
        OpCodeData::subu8(..) => fold_imm_binary::<u8, _>(a, b, |x, y| x.wrapping_sub(y)),
        OpCodeData::subu16(..) => fold_imm_binary::<u16, _>(a, b, |x, y| x.wrapping_sub(y)),
        OpCodeData::subu32(..) => fold_imm_binary::<u32, _>(a, b, |x, y| x.wrapping_sub(y)),
        OpCodeData::subu64(..) => fold_imm_binary::<u64, _>(a, b, |x, y| x.wrapping_sub(y)),
        OpCodeData::subs8(..) => fold_imm_binary::<i8, _>(a, b, |x, y| x.wrapping_sub(y)),
        OpCodeData::subs16(..) => fold_imm_binary::<i16, _>(a, b, |x, y| x.wrapping_sub(y)),
        OpCodeData::subs32(..) => fold_imm_binary::<i32, _>(a, b, |x, y| x.wrapping_sub(y)),
        OpCodeData::subs64(..) => fold_imm_binary::<i64, _>(a, b, |x, y| x.wrapping_sub(y)),
        OpCodeData::subf32(..) => fold_imm_binary::<f32, _>(a, b, |x, y| x - y),
        OpCodeData::subf64(..) => fold_imm_binary::<f64, _>(a, b, |x, y| x - y),

        // --- Integer multiplication ---
        OpCodeData::mulu8(..) => fold_imm_binary::<u8, _>(a, b, |x, y| x.wrapping_mul(y)),
        OpCodeData::mulu16(..) => fold_imm_binary::<u16, _>(a, b, |x, y| x.wrapping_mul(y)),
        OpCodeData::mulu32(..) => fold_imm_binary::<u32, _>(a, b, |x, y| x.wrapping_mul(y)),
        OpCodeData::mulu64(..) => fold_imm_binary::<u64, _>(a, b, |x, y| x.wrapping_mul(y)),
        OpCodeData::muls8(..) => fold_imm_binary::<i8, _>(a, b, |x, y| x.wrapping_mul(y)),
        OpCodeData::muls16(..) => fold_imm_binary::<i16, _>(a, b, |x, y| x.wrapping_mul(y)),
        OpCodeData::muls32(..) => fold_imm_binary::<i32, _>(a, b, |x, y| x.wrapping_mul(y)),
        OpCodeData::muls64(..) => fold_imm_binary::<i64, _>(a, b, |x, y| x.wrapping_mul(y)),
        OpCodeData::mulf32(..) => fold_imm_binary::<f32, _>(a, b, |x, y| x * y),
        OpCodeData::mulf64(..) => fold_imm_binary::<f64, _>(a, b, |x, y| x * y),

        // --- Division ---
        OpCodeData::divu8(..) => fold_imm_binary::<u8, _>(a, b, |x, y| if y == 0 { x } else { x / y }),
        OpCodeData::divu16(..) => fold_imm_binary::<u16, _>(a, b, |x, y| if y == 0 { x } else { x / y }),
        OpCodeData::divu32(..) => fold_imm_binary::<u32, _>(a, b, |x, y| if y == 0 { x } else { x / y }),
        OpCodeData::divu64(..) => fold_imm_binary::<u64, _>(a, b, |x, y| if y == 0 { x } else { x / y }),
        OpCodeData::divs8(..) => fold_imm_binary::<i8, _>(a, b, |x, y| {
            if y == 0 || (x == i8::MIN && y == -1) { x } else { x / y }
        }),
        OpCodeData::divs16(..) => fold_imm_binary::<i16, _>(a, b, |x, y| {
            if y == 0 || (x == i16::MIN && y == -1) { x } else { x / y }
        }),
        OpCodeData::divs32(..) => fold_imm_binary::<i32, _>(a, b, |x, y| {
            if y == 0 || (x == i32::MIN && y == -1) { x } else { x / y }
        }),
        OpCodeData::divs64(..) => fold_imm_binary::<i64, _>(a, b, |x, y| {
            if y == 0 || (x == i64::MIN && y == -1) { x } else { x / y }
        }),
        OpCodeData::divf32(..) => fold_imm_binary::<f32, _>(a, b, |x, y| x / y),
        OpCodeData::divf64(..) => fold_imm_binary::<f64, _>(a, b, |x, y| x / y),

        // --- Remainder ---
        OpCodeData::remu8(..) => fold_imm_binary::<u8, _>(a, b, |x, y| if y == 0 { x } else { x % y }),
        OpCodeData::remu16(..) => fold_imm_binary::<u16, _>(a, b, |x, y| if y == 0 { x } else { x % y }),
        OpCodeData::remu32(..) => fold_imm_binary::<u32, _>(a, b, |x, y| if y == 0 { x } else { x % y }),
        OpCodeData::remu64(..) => fold_imm_binary::<u64, _>(a, b, |x, y| if y == 0 { x } else { x % y }),
        OpCodeData::rems8(..) => fold_imm_binary::<i8, _>(a, b, |x, y| if y == 0 { x } else { x % y }),
        OpCodeData::rems16(..) => fold_imm_binary::<i16, _>(a, b, |x, y| if y == 0 { x } else { x % y }),
        OpCodeData::rems32(..) => fold_imm_binary::<i32, _>(a, b, |x, y| if y == 0 { x } else { x % y }),
        OpCodeData::rems64(..) => fold_imm_binary::<i64, _>(a, b, |x, y| if y == 0 { x } else { x % y }),
        OpCodeData::remf32(..) => fold_imm_binary::<f32, _>(a, b, |x, y| x % y),
        OpCodeData::remf64(..) => fold_imm_binary::<f64, _>(a, b, |x, y| x % y),

        // --- Bitwise ---
        OpCodeData::bitandu8(..) => fold_imm_binary::<u8, _>(a, b, |x, y| x & y),
        OpCodeData::bitandu16(..) => fold_imm_binary::<u16, _>(a, b, |x, y| x & y),
        OpCodeData::bitandu32(..) => fold_imm_binary::<u32, _>(a, b, |x, y| x & y),
        OpCodeData::bitandu64(..) => fold_imm_binary::<u64, _>(a, b, |x, y| x & y),
        OpCodeData::bitands8(..) => fold_imm_binary::<i8, _>(a, b, |x, y| x & y),
        OpCodeData::bitands16(..) => fold_imm_binary::<i16, _>(a, b, |x, y| x & y),
        OpCodeData::bitands32(..) => fold_imm_binary::<i32, _>(a, b, |x, y| x & y),
        OpCodeData::bitands64(..) => fold_imm_binary::<i64, _>(a, b, |x, y| x & y),
        OpCodeData::bitoru8(..) => fold_imm_binary::<u8, _>(a, b, |x, y| x | y),
        OpCodeData::bitoru16(..) => fold_imm_binary::<u16, _>(a, b, |x, y| x | y),
        OpCodeData::bitoru32(..) => fold_imm_binary::<u32, _>(a, b, |x, y| x | y),
        OpCodeData::bitoru64(..) => fold_imm_binary::<u64, _>(a, b, |x, y| x | y),
        OpCodeData::bitors8(..) => fold_imm_binary::<i8, _>(a, b, |x, y| x | y),
        OpCodeData::bitors16(..) => fold_imm_binary::<i16, _>(a, b, |x, y| x | y),
        OpCodeData::bitors32(..) => fold_imm_binary::<i32, _>(a, b, |x, y| x | y),
        OpCodeData::bitors64(..) => fold_imm_binary::<i64, _>(a, b, |x, y| x | y),
        OpCodeData::bitxoru8(..) => fold_imm_binary::<u8, _>(a, b, |x, y| x ^ y),
        OpCodeData::bitxoru16(..) => fold_imm_binary::<u16, _>(a, b, |x, y| x ^ y),
        OpCodeData::bitxoru32(..) => fold_imm_binary::<u32, _>(a, b, |x, y| x ^ y),
        OpCodeData::bitxoru64(..) => fold_imm_binary::<u64, _>(a, b, |x, y| x ^ y),
        OpCodeData::bitxors8(..) => fold_imm_binary::<i8, _>(a, b, |x, y| x ^ y),
        OpCodeData::bitxors16(..) => fold_imm_binary::<i16, _>(a, b, |x, y| x ^ y),
        OpCodeData::bitxors32(..) => fold_imm_binary::<i32, _>(a, b, |x, y| x ^ y),
        OpCodeData::bitxors64(..) => fold_imm_binary::<i64, _>(a, b, |x, y| x ^ y),

        // --- Shifts ---
        OpCodeData::shlu8(..) => {
            if let (OpCodeData::immediate8(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate8(x.wrapping_shl(*s) as u8))
            } else { None }
        }
        OpCodeData::shlu16(..) => {
            if let (OpCodeData::immediate16(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate16(x.wrapping_shl(*s) as u16))
            } else { None }
        }
        OpCodeData::shlu32(..) => {
            if let (OpCodeData::immediate32(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate32(x.wrapping_shl(*s)))
            } else { None }
        }
        OpCodeData::shlu64(..) => {
            if let (OpCodeData::immediate64(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate64(x.wrapping_shl(*s)))
            } else { None }
        }
        OpCodeData::shls8(..) => {
            if let (OpCodeData::immediate8(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate8(((*x as i8).wrapping_shl(*s)) as u8))
            } else { None }
        }
        OpCodeData::shls16(..) => {
            if let (OpCodeData::immediate16(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate16(((*x as i16).wrapping_shl(*s)) as u16))
            } else { None }
        }
        OpCodeData::shls32(..) => {
            if let (OpCodeData::immediate32(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate32((*x as i32).wrapping_shl(*s) as u32))
            } else { None }
        }
        OpCodeData::shls64(..) => {
            if let (OpCodeData::immediate64(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate64((*x as i64).wrapping_shl(*s) as u64))
            } else { None }
        }
        OpCodeData::shru8(..) => {
            if let (OpCodeData::immediate8(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate8(x.wrapping_shr(*s) as u8))
            } else { None }
        }
        OpCodeData::shru16(..) => {
            if let (OpCodeData::immediate16(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate16(x.wrapping_shr(*s) as u16))
            } else { None }
        }
        OpCodeData::shru32(..) => {
            if let (OpCodeData::immediate32(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate32(x.wrapping_shr(*s)))
            } else { None }
        }
        OpCodeData::shru64(..) => {
            if let (OpCodeData::immediate64(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate64(x.wrapping_shr(*s)))
            } else { None }
        }
        OpCodeData::shrs8(..) => {
            if let (OpCodeData::immediate8(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate8(((*x as i8).wrapping_shr(*s)) as u8))
            } else { None }
        }
        OpCodeData::shrs16(..) => {
            if let (OpCodeData::immediate16(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate16(((*x as i16).wrapping_shr(*s)) as u16))
            } else { None }
        }
        OpCodeData::shrs32(..) => {
            if let (OpCodeData::immediate32(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate32((*x as i32).wrapping_shr(*s) as u32))
            } else { None }
        }
        OpCodeData::shrs64(..) => {
            if let (OpCodeData::immediate64(x), OpCodeData::immediate32(s)) = (a, b) {
                Some(OpCodeData::immediate64((*x as i64).wrapping_shr(*s) as u64))
            } else { None }
        }

        // --- Comparisons — always produce u8 (0 or 1) ---
        OpCodeData::ceq8(..) => fold_imm_compare::<u8, _>(a, b, |x, y| x == y),
        OpCodeData::ceq16(..) => fold_imm_compare::<u16, _>(a, b, |x, y| x == y),
        OpCodeData::ceq32(..) => fold_imm_compare::<u32, _>(a, b, |x, y| x == y),
        OpCodeData::ceq64(..) => fold_imm_compare::<u64, _>(a, b, |x, y| x == y),
        OpCodeData::cneq8(..) => fold_imm_compare::<u8, _>(a, b, |x, y| x != y),
        OpCodeData::cneq16(..) => fold_imm_compare::<u16, _>(a, b, |x, y| x != y),
        OpCodeData::cneq32(..) => fold_imm_compare::<u32, _>(a, b, |x, y| x != y),
        OpCodeData::cneq64(..) => fold_imm_compare::<u64, _>(a, b, |x, y| x != y),
        OpCodeData::cltu8(..) => fold_imm_compare::<u8, _>(a, b, |x, y| x < y),
        OpCodeData::cltu16(..) => fold_imm_compare::<u16, _>(a, b, |x, y| x < y),
        OpCodeData::cltu32(..) => fold_imm_compare::<u32, _>(a, b, |x, y| x < y),
        OpCodeData::cltu64(..) => fold_imm_compare::<u64, _>(a, b, |x, y| x < y),
        OpCodeData::clts8(..) => fold_imm_compare::<i8, _>(a, b, |x, y| x < y),
        OpCodeData::clts16(..) => fold_imm_compare::<i16, _>(a, b, |x, y| x < y),
        OpCodeData::clts32(..) => fold_imm_compare::<i32, _>(a, b, |x, y| x < y),
        OpCodeData::clts64(..) => fold_imm_compare::<i64, _>(a, b, |x, y| x < y),
        OpCodeData::cltf32(..) => fold_imm_compare::<f32, _>(a, b, |x, y| x < y),
        OpCodeData::cltf64(..) => fold_imm_compare::<f64, _>(a, b, |x, y| x < y),
        OpCodeData::clteu8(..) => fold_imm_compare::<u8, _>(a, b, |x, y| x <= y),
        OpCodeData::clteu16(..) => fold_imm_compare::<u16, _>(a, b, |x, y| x <= y),
        OpCodeData::clteu32(..) => fold_imm_compare::<u32, _>(a, b, |x, y| x <= y),
        OpCodeData::clteu64(..) => fold_imm_compare::<u64, _>(a, b, |x, y| x <= y),
        OpCodeData::cltes8(..) => fold_imm_compare::<i8, _>(a, b, |x, y| x <= y),
        OpCodeData::cltes16(..) => fold_imm_compare::<i16, _>(a, b, |x, y| x <= y),
        OpCodeData::cltes32(..) => fold_imm_compare::<i32, _>(a, b, |x, y| x <= y),
        OpCodeData::cltes64(..) => fold_imm_compare::<i64, _>(a, b, |x, y| x <= y),
        OpCodeData::cltef32(..) => fold_imm_compare::<f32, _>(a, b, |x, y| x <= y),
        OpCodeData::cltef64(..) => fold_imm_compare::<f64, _>(a, b, |x, y| x <= y),
        OpCodeData::cgtu8(..) => fold_imm_compare::<u8, _>(a, b, |x, y| x > y),
        OpCodeData::cgtu16(..) => fold_imm_compare::<u16, _>(a, b, |x, y| x > y),
        OpCodeData::cgtu32(..) => fold_imm_compare::<u32, _>(a, b, |x, y| x > y),
        OpCodeData::cgtu64(..) => fold_imm_compare::<u64, _>(a, b, |x, y| x > y),
        OpCodeData::cgts8(..) => fold_imm_compare::<i8, _>(a, b, |x, y| x > y),
        OpCodeData::cgts16(..) => fold_imm_compare::<i16, _>(a, b, |x, y| x > y),
        OpCodeData::cgts32(..) => fold_imm_compare::<i32, _>(a, b, |x, y| x > y),
        OpCodeData::cgts64(..) => fold_imm_compare::<i64, _>(a, b, |x, y| x > y),
        OpCodeData::cgtf32(..) => fold_imm_compare::<f32, _>(a, b, |x, y| x > y),
        OpCodeData::cgtf64(..) => fold_imm_compare::<f64, _>(a, b, |x, y| x > y),
        OpCodeData::cgteu8(..) => fold_imm_compare::<u8, _>(a, b, |x, y| x >= y),
        OpCodeData::cgteu16(..) => fold_imm_compare::<u16, _>(a, b, |x, y| x >= y),
        OpCodeData::cgteu32(..) => fold_imm_compare::<u32, _>(a, b, |x, y| x >= y),
        OpCodeData::cgteu64(..) => fold_imm_compare::<u64, _>(a, b, |x, y| x >= y),
        OpCodeData::cgtes8(..) => fold_imm_compare::<i8, _>(a, b, |x, y| x >= y),
        OpCodeData::cgtes16(..) => fold_imm_compare::<i16, _>(a, b, |x, y| x >= y),
        OpCodeData::cgtes32(..) => fold_imm_compare::<i32, _>(a, b, |x, y| x >= y),
        OpCodeData::cgtes64(..) => fold_imm_compare::<i64, _>(a, b, |x, y| x >= y),
        OpCodeData::cgtef32(..) => fold_imm_compare::<f32, _>(a, b, |x, y| x >= y),
        OpCodeData::cgtef64(..) => fold_imm_compare::<f64, _>(a, b, |x, y| x >= y),

        // --- Logical ops (always u8) ---
        OpCodeData::and(..) => {
            if let (OpCodeData::immediate8(x), OpCodeData::immediate8(y)) = (a, b) {
                Some(OpCodeData::immediate8((*x != 0 && *y != 0) as u8))
            } else {
                None
            }
        }
        OpCodeData::or(..) => {
            if let (OpCodeData::immediate8(x), OpCodeData::immediate8(y)) = (a, b) {
                Some(OpCodeData::immediate8((*x != 0 || *y != 0) as u8))
            } else {
                None
            }
        }

        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Unary folding
// ---------------------------------------------------------------------------

fn try_fold_unary(a: &OpCodeData, op: &OpCodeData) -> Option<OpCodeData> {
    match op {
        // --- Negation ---
        OpCodeData::negs8(..) => fold_imm_unary::<i8, _>(a, |x| x.wrapping_neg()),
        OpCodeData::negs16(..) => fold_imm_unary::<i16, _>(a, |x| x.wrapping_neg()),
        OpCodeData::negs32(..) => fold_imm_unary::<i32, _>(a, |x| x.wrapping_neg()),
        OpCodeData::negs64(..) => fold_imm_unary::<i64, _>(a, |x| x.wrapping_neg()),
        OpCodeData::negf32(..) => fold_imm_unary::<f32, _>(a, |x| -x),
        OpCodeData::negf64(..) => fold_imm_unary::<f64, _>(a, |x| -x),

        // --- Bitwise NOT ---
        OpCodeData::bitnotu8(..) => fold_imm_unary::<u8, _>(a, |x| !x),
        OpCodeData::bitnotu16(..) => fold_imm_unary::<u16, _>(a, |x| !x),
        OpCodeData::bitnotu32(..) => fold_imm_unary::<u32, _>(a, |x| !x),
        OpCodeData::bitnotu64(..) => fold_imm_unary::<u64, _>(a, |x| !x),
        OpCodeData::bitnots8(..) => fold_imm_unary::<i8, _>(a, |x| !x),
        OpCodeData::bitnots16(..) => fold_imm_unary::<i16, _>(a, |x| !x),
        OpCodeData::bitnots32(..) => fold_imm_unary::<i32, _>(a, |x| !x),
        OpCodeData::bitnots64(..) => fold_imm_unary::<i64, _>(a, |x| !x),

        // --- Logical NOT (always u8) ---
        OpCodeData::not(..) => {
            match a {
                OpCodeData::immediate8(v) => Some(OpCodeData::immediate8((*v == 0) as u8)),
                OpCodeData::immediate16(v) => Some(OpCodeData::immediate8((*v == 0) as u8)),
                OpCodeData::immediate32(v) => Some(OpCodeData::immediate8((*v == 0) as u8)),
                OpCodeData::immediate64(v) => Some(OpCodeData::immediate8((*v == 0) as u8)),
                _ => None,
            }
        }

        // --- Zero clamp ---
        OpCodeData::zclampi8(..) => fold_imm_unary::<i8, _>(a, |x| if x >= 0 { x } else { 0 }),
        OpCodeData::zclampi16(..) => fold_imm_unary::<i16, _>(a, |x| if x >= 0 { x } else { 0 }),
        OpCodeData::zclampi32(..) => fold_imm_unary::<i32, _>(a, |x| if x >= 0 { x } else { 0 }),
        OpCodeData::zclampi64(..) => fold_imm_unary::<i64, _>(a, |x| if x >= 0 { x } else { 0 }),
        OpCodeData::zclampf32(..) => fold_imm_unary::<f32, _>(a, |x| if x >= 0.0 { x } else { 0.0 }),
        OpCodeData::zclampf64(..) => fold_imm_unary::<f64, _>(a, |x| if x >= 0.0 { x } else { 0.0 }),

        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Helpers — trait-based generic fold functions
// ---------------------------------------------------------------------------

/// Type-specific immediate extraction and wrapping for constant folding.
trait FoldImm: Copy {
    /// Extract the logical value from an OpCodeData, or None if the variant doesn't match.
    fn extract(data: &OpCodeData) -> Option<Self>;
    /// Wrap a value back into an OpCodeData.
    fn wrap(value: Self) -> OpCodeData;
}

macro_rules! impl_fold_imm {
    // Unsigned: stored directly, no casting
    ($($t:ty => $imm:ident),*) => {$(
        impl FoldImm for $t {
            fn extract(data: &OpCodeData) -> Option<Self> {
                if let OpCodeData::$imm(v) = data { Some(*v) } else { None }
            }
            fn wrap(value: Self) -> OpCodeData { OpCodeData::$imm(value) }
        }
    )*};
    // Signed: stored as unsigned bits, cast to/from signed
    (signed $($t:ty => $imm:ident),*) => {$(
        impl FoldImm for $t {
            fn extract(data: &OpCodeData) -> Option<Self> {
                if let OpCodeData::$imm(v) = data { Some(*v as $t) } else { None }
            }
            fn wrap(value: Self) -> OpCodeData { OpCodeData::$imm(value as _) }
        }
    )*};
    // Float: stored as bits, cast to/from float
    (float $($t:ty => $imm:ident),*) => {$(
        impl FoldImm for $t {
            fn extract(data: &OpCodeData) -> Option<Self> {
                if let OpCodeData::$imm(v) = data { Some(*v as $t) } else { None }
            }
            fn wrap(value: Self) -> OpCodeData { OpCodeData::$imm(value as _) }
        }
    )*};
}

impl_fold_imm!(u8 => immediate8, u16 => immediate16, u32 => immediate32, u64 => immediate64);
impl_fold_imm!(signed i8 => immediate8, i16 => immediate16, i32 => immediate32, i64 => immediate64);
impl_fold_imm!(float f32 => immediate32, f64 => immediate64);

/// Fold two same-size immediates with a binary operation, producing a same-size immediate.
fn fold_imm_binary<T: FoldImm, F: Fn(T, T) -> T>(a: &OpCodeData, b: &OpCodeData, f: F) -> Option<OpCodeData> {
    match (T::extract(a), T::extract(b)) {
        (Some(x), Some(y)) => Some(T::wrap(f(x, y))),
        _ => None,
    }
}

/// Comparison: operands are same-size immediates, result is always immediate8 (u8).
fn fold_imm_compare<T: FoldImm, F: Fn(T, T) -> bool>(a: &OpCodeData, b: &OpCodeData, f: F) -> Option<OpCodeData> {
    match (T::extract(a), T::extract(b)) {
        (Some(x), Some(y)) => Some(OpCodeData::immediate8(f(x, y) as u8)),
        _ => None,
    }
}

/// Fold one immediate with a unary operation, producing a same-size immediate.
fn fold_imm_unary<T: FoldImm, F: Fn(T) -> T>(a: &OpCodeData, f: F) -> Option<OpCodeData> {
    T::extract(a).map(|v| T::wrap(f(v)))
}
