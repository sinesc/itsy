//! Tests that validate optimizer passes are active and effective.
//!
//! Each test compiles code designed to trigger a specific optimization pass and
//! inspects the resulting bytecode to verify the pass fired correctly.

#[macro_use]
mod util;
use util::*;
use itsy::optimizer::{OpCodeData, read_opcode_data};

#[path = "optimizer/const_fold.rs"]
mod const_fold;
#[path = "optimizer/cnt_cancel.rs"]
mod cnt_cancel;
#[path = "optimizer/multi_pass.rs"]
mod multi_pass;

/// Helper: compile code, run it, and return (success, program).
fn compile_and_run(code: &str) -> (bool, itsy::Program<TestFns>) {
    let input = format!("{} fn main() {{ {} }}", TEST_PRELUDE, code);
    let program = build_str::<TestFns>(&input).unwrap_or_else(|e| panic!("compile failed: {}", e));
    let mut ctx = Vec::new();
    let ok = itsy_run(program.clone(), &mut ctx).is_ok();
    (ok, program)
}

/// Iterate over all instructions in a program, yielding `(pc, OpCodeData)` pairs.
fn iter_instructions(program: &itsy::Program<TestFns>) -> Vec<(usize, OpCodeData)> {
    let mut list = Vec::new();
    let mut pc = 0usize;
    while let Some((data, next_pc)) = read_opcode_data(program.instructions(), pc) {
        list.push((pc, data));
        pc = next_pc as usize;
    }
    list
}
