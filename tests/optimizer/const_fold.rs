//! const_fold — constant arithmetic folding
//!
//! The compiler already emits compact `addxc*` / `mulxc*` etc. opcodes for
//! simple constant arithmetic, so the const_fold pass (which folds
//! `immediate + immediate + stack_op` triples) mainly benefits nested
//! expressions and the fixpoint loop that re-scans after other passes.
//! These tests verify that the combined compiler+optimizer pipeline produces
//! compact bytecode with the expected constants.

use super::*;

/// `100 + 200` — compiler emits `immediate32(100) + addxcu32(200)`.
/// No separate `immediate32(200) + addu32` pattern should appear.
#[test]
fn add() {
    let (ok, prog) = compile_and_run("ret_u32(100 + 200)");
    assert!(ok);
    let instrs = iter_instructions(&prog);
    // Should have exactly one immediate32 (the left operand);
    // the right operand is embedded in addxcu32.
    let imm32_count = instrs.iter().filter(|(_, d)| matches!(d, OpCodeData::immediate32(_))).count();
    assert_eq!(imm32_count, 1, "expected 1 immediate32, got {} — bytecode not compact", imm32_count);
    // No stack-based addu32 should appear (compiler uses addxcu32 instead)
    assert!(
        !instrs.iter().any(|(_, d)| matches!(d, OpCodeData::addu32(..))),
        "addu32 present — compiler/optimizer should use addxcu32"
    );
}

/// Chained arithmetic: `(10 + 20) * (30 - 5)` — sub-expression constants
/// should be embedded in xc opcodes. The final multiply is stack-based
/// (both operands are expression results), which is correct.
#[test]
fn chain() {
    let (ok, prog) = compile_and_run("ret_u32((10 + 20) * (30 - 5))");
    assert!(ok);
    let instrs = iter_instructions(&prog);
    // Sub-expressions should use xc variants (no addu32 or subu32)
    assert!(
        !instrs.iter().any(|(_, d)| matches!(d, OpCodeData::addu32(..) | OpCodeData::subu32(..))),
        "addu32/subu32 present — sub-expressions should use xc variants"
    );
    // The mulu32 is expected (both operands are expression results on the stack)
    let mul_count = instrs.iter().filter(|(_, d)| matches!(d, OpCodeData::mulu32(..))).count();
    assert_eq!(mul_count, 1, "expected exactly 1 mulu32 for the final multiply");
}

/// `42 == 42` — comparison should fold/compile to a single immediate8(1).
#[test]
fn compare() {
    let (ok, prog) = compile_and_run("ret_bool(42 == 42)");
    assert!(ok);
    let imms: Vec<u8> = immediate8_values(&prog);
    assert!(imms.contains(&1), "expected folded constant 1, got immediates: {:?}", imms);
}

/// `0xFF & 0x0F` — bitwise should use compact xc form.
#[test]
fn bitwise() {
    let (ok, prog) = compile_and_run("ret_u32(0xFF & 0x0F)");
    assert!(ok);
    let instrs = iter_instructions(&prog);
    // No stack-based bitwise op should appear
    assert!(
        !instrs.iter().any(|(_, d)| matches!(d, OpCodeData::bitandu32(..))),
        "bitandu32 present — should use bitandxcu32"
    );
}

/// `1 << 8` — shift should use compact xc form.
#[test]
fn shift() {
    let (ok, prog) = compile_and_run("ret_u32(1 << 8)");
    assert!(ok);
    let instrs = iter_instructions(&prog);
    assert!(
        !instrs.iter().any(|(_, d)| matches!(d, OpCodeData::shlu32(..))),
        "shlu32 present — should use shlxcu32"
    );
}

/// `true && false` — logical and should produce a single immediate8(0).
#[test]
fn logical() {
    let (ok, prog) = compile_and_run("ret_bool(true && false)");
    assert!(ok);
    let imms: Vec<u8> = immediate8_values(&prog);
    assert!(imms.contains(&0), "expected folded constant 0, got immediates: {:?}", imms);
}

/// `-42` — negation. The bytecode should not contain a stack-based negs32
/// followed by a separate immediate; the compiler embeds the constant.
#[test]
fn negate() {
    let (ok, prog) = compile_and_run("ret_i32(-42)");
    assert!(ok);
    let instrs = iter_instructions(&prog);
    assert!(
        !instrs.iter().any(|(_, d)| matches!(d, OpCodeData::negs32(..))),
        "negs32 present — compiler should embed negated constant"
    );
}

/// `!true` — logical not should fold to immediate8(0).
#[test]
fn not() {
    let (ok, prog) = compile_and_run("ret_bool(!true)");
    assert!(ok);
    let imms: Vec<u8> = immediate8_values(&prog);
    assert!(imms.contains(&0), "expected folded constant 0, got immediates: {:?}", imms);
}

// ---------------------------------------------------------------------------
// Helpers

/// Collect all immediate8 values from the instruction stream.
fn immediate8_values(program: &itsy::Program<TestFns>) -> Vec<u8> {
    iter_instructions(program)
        .into_iter()
        .filter_map(|(_, data)| {
            if let OpCodeData::immediate8(v) = data {
                Some(v)
            } else {
                None
            }
        })
        .collect()
}
