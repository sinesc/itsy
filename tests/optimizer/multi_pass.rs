//! Multi-pass interaction — ensure passes compose correctly

use super::*;

/// Long chain of additions — the compiler should use xc variants throughout,
/// producing a compact instruction stream with only one separate immediate32
/// (the leftmost operand). No redundant stack-based adds should appear.
#[test]
fn fixpoint_loop() {
    let (ok, prog) = compile_and_run("ret_u32(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10)");
    assert!(ok);
    let instrs = iter_instructions(&prog);
    // Only one immediate32 (the leftmost operand); rest embedded in addxcu32
    let imm32_count = instrs.iter().filter(|(_, d)| matches!(d, OpCodeData::immediate32(_))).count();
    assert_eq!(imm32_count, 1, "expected 1 immediate32, got {} — bytecode not compact", imm32_count);
    // No stack-based addu32 should appear
    assert!(
        !instrs.iter().any(|(_, d)| matches!(d, OpCodeData::addu32(..))),
        "addu32 present — should use addxcu32"
    );
}

/// Mixed expression: arithmetic + string literal.
/// Arithmetic should be compact (xc variant), string refcount ops should cancel.
#[test]
fn mixed() {
    let (ok, prog) = compile_and_run("ret_u32(100 + 200); ret_string(\"x\")");
    assert!(ok);
    let instrs = iter_instructions(&prog);
    // Arithmetic: one immediate32 + addxcu32, no addu32
    let imm32_count = instrs.iter().filter(|(_, d)| matches!(d, OpCodeData::immediate32(_))).count();
    assert_eq!(imm32_count, 1, "expected 1 immediate32, got {}", imm32_count);
    assert!(
        !instrs.iter().any(|(_, d)| matches!(d, OpCodeData::addu32(..))),
        "addu32 present — should use addxcu32"
    );
    // Refcount ops for the string should cancel
    let cnt = count_cnt_ops(&prog);
    assert_eq!(cnt, 0, "expected 0 cnt ops after cancellation, got {}", cnt);
}

// ---------------------------------------------------------------------------
// Helpers

/// Count refcount instructions in the stream.
fn count_cnt_ops(program: &itsy::Program<TestFns>) -> usize {
    iter_instructions(program).into_iter().filter(|(_, data)| {
        matches!(
            data,
            OpCodeData::cnt_16(..)
                | OpCodeData::cnt_sa(..)
                | OpCodeData::cnt_16_nc(..)
                | OpCodeData::cnt_sa_nc(..)
        )
    }).count()
}
