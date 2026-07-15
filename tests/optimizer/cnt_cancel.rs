//! cnt_cancel — refcount cancellation

use super::*;

/// Create a string and return it — refcount ops should cancel.
#[test]
fn string() {
    let (ok, prog) = compile_and_run("ret_string(\"hello\")");
    assert!(ok);
    let cnt = count_cnt_ops(&prog);
    assert_eq!(cnt, 0, "expected 0 cnt ops after cancellation, got {}", cnt);
}

/// Create a struct instance and return it — refcount ops should cancel.
#[test]
fn struct_return() {
    let code = format!(
        r#"{} struct Point {{ x: u32, y: u32 }} fn main() {{ ret_u32(Point {{ x: 1, y: 2 }}.x); }}"#,
        TEST_PRELUDE
    );
    let program = build_str::<TestFns>(&code).unwrap_or_else(|e| panic!("compile failed: {}", e));
    let mut ctx = Vec::new();
    assert!(itsy_run(program.clone(), &mut ctx).is_ok());
    let cnt = count_cnt_ops(&program);
    assert_eq!(cnt, 0, "expected 0 cnt ops after cancellation, got {}", cnt);
}

// ---------------------------------------------------------------------------
// Helpers

/// Count refcount instructions (cnt_16, cnt_sa, cnt_16_nc, cnt_sa_nc) in the stream.
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
