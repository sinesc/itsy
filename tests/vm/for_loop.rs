use crate::util::*;

#[test]
fn for_in_negative() {
    let result = run(stringify!(
        for i in -3..3 {
            ret_i32(i);
        }
    ));
    assert_all(&result, &[ -3i32, -2, -1, 0, 1, 2 ]);
}

#[test]
fn for_in_none() {
    let result = run(stringify!(
        for i in 1..1 {
            ret_u16(i);
        }
    ));
    assert!(result.len() == 0);
}

#[test]
fn for_in_dec() {
    let result = run(stringify!(
        for i in 1..0 {
            ret_i8(i);
        }
    ));
    assert!(result.len() == 0);
}

#[test]
fn for_in_negative_inclusive() {
    let result = run(stringify!(
        for i in -3..=3 {
            ret_i8(i);
        }
    ));
    assert_all(&result, &[ -3i8, -2, -1, 0, 1, 2, 3 ]);
}

#[test]
fn for_in_equal_inclusive() {
    let result = run(stringify!(
        for i in 1..=1 {
            ret_u16(i);
        }
    ));
    assert_all(&result, &[ 1u16 ]);
}

#[test]
fn for_in_dec_inclusive() {
    let result = run(stringify!(
        for i in 1..=0 {
            ret_i8(i);
        }
    ));
    assert!(result.len() == 0);
}

#[test]
fn for_in_inference_let() {
    let result = run(stringify!(
        for x in 1..4 {
            let y = x * 2;
            ret_u8(y);
        }
    ));
    assert_all(&result, &[ 2u8, 4, 6 ]);
}

#[test]
fn for_in_inference_assign() {
    let result = run(stringify!(
        let mut y;
        for x in 1..4 {
            y = x * 2;
            ret_u8(y);
        }
        ret_u8(y);
    ));
    assert_all(&result, &[ 2u8, 4, 6, 6 ]);
}

#[test]
fn for_in_array() {
    let result = run(stringify!(
        for x in [ 7u8, 11, 13, 17 ] {
            ret_u8(x);
        }
    ));
    assert_all(&result, &[ 7u8, 11, 13, 17 ]);
}

#[test]
fn for_in_array_var() {
    let result = run(stringify!(
        let array = [ 7u8, 11, 13, 17 ];
        for x in array {
            ret_u8(x);
        }
    ));
    assert_all(&result, &[ 7u8, 11, 13, 17 ]);
}

#[test]
fn for_in_array_struct() {
    let result = run(stringify!(
        struct Test {
            inner: u8,
        }
        fn main() {
            let array = [ Test { inner: 7 }, Test { inner: 17 }];
            for x in array {
                ret_u8(x.inner);
            }
        }
    ));
    assert_all(&result, &[ 7u8, 17 ]);
}

#[test]
fn for_in_array_string() {
    let result = run(stringify!(
        let array = [ "Hello", "World" ];
        for x in array {
            ret_string(x);
        }
    ));
    assert_all(&result, &[ "Hello".to_string(), "World".to_string() ]);
}

#[test]
fn for_in_array_assign_to_outer_var() {
    // Regression: assigning the loop element to a variable declared outside the loop must release the
    // previously held element each iteration. Previously every store used new (no decrement) semantics,
    // leaking all but the last iterated heap element, which surfaced as a heap-corruption error on exit.
    let result = run(stringify!(
        struct S { a: u16 }
        fn main() {
            let data = [ S { a: 33 }, S { a: 44 }, S { a: 55 } ];
            let x;
            for i in data {
                x = i;
                ret_u16(x.a);
            }
        }
    ));
    assert_all(&result, &[ 33u16, 44, 55 ]);
}

#[test]
fn for_in_inference() {
    let result = run(stringify!(
        let array = [ 1, 2, 3, 4 ];
        for i in array {
            ret_u8(i);
        }
    ));
    assert_all(&result, &[ 1u8, 2, 3, 4 ]);
}

#[test]
fn for_in_array_value_pop_during_iteration() {
    // Iterating a clone means popping the original during iteration does not cut the loop short: the
    // full snapshot taken at loop entry is visited, and the shared elements survive being dropped from
    // the original until the loop releases its clone.
    let result = run(stringify!(
        let a = [ "one", "two", "three", "four" ];
        for v in a {
            ret_str(v);
            if a.len() > 0 {
                a.pop();
            }
        }
        ret_str("len={a.len() as u64}");
    ));
    assert_all(&result, &[ "one".to_string(), "two".to_string(), "three".to_string(), "four".to_string(), "len=0".to_string() ]);
}

#[test]
fn for_in_array_index_value_pop_during_iteration() {
    // as above for the two-binding form: index and value both reflect the full entry snapshot
    let result = run(stringify!(
        let a = [ "one", "two", "three", "four" ];
        for k, v in a {
            ret_str("{k as u64}:{v}");
            if a.len() > 0 {
                a.pop();
            }
        }
    ));
    assert_all(&result, &[ "0:one".to_string(), "1:two".to_string(), "2:three".to_string(), "3:four".to_string() ]);
}

#[test]
fn for_range_min_max() {
    let result = run(stringify!(
        let c = 0;
        for i in -128i8..=127 {
            ret_i8(i);
            c += 1;
            if c > 256 {
                break; // stop loop if it won't on its own
            }
        }
    ));
    if result.len() > 256 {
        panic!("Loop failed to stop looping");
    } else if result.len() < 256 {
        panic!("Loop ended prematurely");
    }
    let mut expected = Vec::new();
    for i in -128i8..=127 {
        expected.push(i);
    }
    assert_all(&result, &expected);
}

// ---------------------------------------------------------------------------
// Regression: return inside for-loops must clean up loop artifacts
// ---------------------------------------------------------------------------

#[test]
fn return_in_for_in_array() {
    let result = run(stringify!(
        for i in [1u8, 2, 3] {
            ret_u8(42);
            return;
        }
        ret_u8(99); // dead code
    ));
    assert_all(&result, &[ 42u8 ]);
}

#[test]
fn return_in_for_in_map() {
    let result = run(stringify!(
        let m = [ 1i64 => 2i64, 3i64 => 4i64 ];
        for v in m {
            ret_u8(42);
            return;
        }
        ret_u8(99);
    ));
    assert_all(&result, &[ 42u8 ]);
}

#[test]
fn return_in_for_in_range() {
    let result = run(stringify!(
        for i in 0..10 {
            ret_u8(42);
            return;
        }
        ret_u8(99);
    ));
    assert_all(&result, &[ 42u8 ]);
}

#[test]
fn return_in_for_in_range_inclusive() {
    let result = run(stringify!(
        for i in 0..=10 {
            ret_u8(42);
            return;
        }
        ret_u8(99);
    ));
    assert_all(&result, &[ 42u8 ]);
}

#[test]
fn return_in_nested_for_array_range() {
    // array loop outer, range loop inner
    let result = run(stringify!(
        for i in [1u8, 2] {
            for j in 0..5 {
                ret_u8(42);
                return;
            }
        }
        ret_u8(99);
    ));
    assert_all(&result, &[ 42u8 ]);
}

#[test]
fn return_in_nested_for_range_array() {
    // range loop outer, array loop inner
    let result = run(stringify!(
        for i in 0..5 {
            for j in [1u8, 2] {
                ret_u8(42);
                return;
            }
        }
        ret_u8(99);
    ));
    assert_all(&result, &[ 42u8 ]);
}

#[test]
fn return_in_triple_nested_for() {
    let result = run(stringify!(
        for i in [1u8] {
            for j in 0..2 {
                for k in [1u8, 2] {
                    ret_u8(42);
                    return;
                }
            }
        }
        ret_u8(99);
    ));
    assert_all(&result, &[ 42u8 ]);
}

#[test]
fn return_in_while_with_for() {
    let result = run(stringify!(
        let data = [1u8, 2];
        let cond = true;
        while true {
            for item in data {
                if cond {
                    ret_u8(42);
                    return;
                }
            }
        }
    ));
    assert_all(&result, &[ 42u8 ]);
}

#[test]
fn return_in_for_in_struct_array() {
    let result = run(stringify!(
        struct S { x: i32 }
        fn main() {
            let arr = [ S { x: 1 }, S { x: 2 } ];
            for s in arr {
                ret_u8(42);
                return;
            }
        }
    ));
    assert_all(&result, &[ 42u8 ]);
}

#[test]
fn return_in_for_in_string_array() {
    let result = run(stringify!(
        let arr = [ "hello", "world" ];
        for s in arr {
            ret_string(s);
            return;
        }
        ret_string("dead");
    ));
    assert_all(&result, &[ "hello".to_string() ]);
}