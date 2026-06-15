use crate::util::*;

#[test]
fn recursion() {
    let result = run(stringify!(
        fn fib(n: i32) -> i32 {
            if n < 2 {
                n
            } else {
                fib(n - 1) + fib(n - 2)
            }
        }
        fn main() {
            ret_i32(fib(1));
            ret_i32(fib(2));
            ret_i32(fib(5));
            ret_i32(fib(7));
        }
    ));
    assert_all(&result, &[ 1i32, 1, 5, 13 ]);
}

#[test]
fn branching() {
    let result = run(stringify!(
        let mut x = 1;
        let y = 2;
        while x <= 3 {
            if x < y {
                ret_i32(x);
            } else if x > y {
                ret_i32(y);
            } else {
                ret_i32(x + y);
            }
            x = x + 1;
        }
    ));
    assert_all(&result, &[ 1i32, 4, 2 ]);
}

#[test]
fn branching_ref_cleanup() {
    let result = run(stringify!(
        if true {
            let a = "Hello";
            ret_string(a);
        } else {
            let b = "World";
            ret_string(b);
        }
    ));
    assert_all(&result, &[ "Hello".to_string() ]);
}

#[test]
fn explicit_return() {
    // todo: add bytecode test, check dead code was removed
    let result = run(stringify!(
        fn test(x: i32) -> i32 {
            ret_i32(x);
            if x == 1 {
                ret_i32(1);
                return 1;
                ret_i32(-1);
            } else if x == 2 {
                ret_i32(2);
                // return from sub-block
                {
                    return 2;
                }
                ret_i32(-2);
            } else {
                if x == 3 {
                    ret_i32(3);
                    return 3;
                    ret_i32(-3);
                } else if x == 4 {
                    ret_i32(4);
                    return 4;
                    ret_i32(-4);
                }
                // else fall through
                ret_i32(5);
                return 5;
                ret_i32(-5);
            }
            ret_i32(-6);
        }
        fn main() {
            ret_i32(test(1));
            ret_i32(test(2));
            ret_i32(test(3));
            ret_i32(test(4));
            ret_i32(test(5));
            ret_i32(test(6));
        }
    ));
    assert_all(&result, &[
        1i32,   1, 1,
        2,      2, 2,
        3,      3, 3,
        4,      4, 4,
        5,      5, 5,
        6,      5, 5,
    ]);
}

#[test]
fn block_result() {
    let result = run(stringify!(
        fn test(x: i32) -> i32 {
            ret_i32(x);
            if x == 1 {
                ret_i32(1);
                1
            } else if x == 2 {
                ret_i32(2);
                // result from sub-block
                {
                    2
                }
            } else {
                if x == 3 {
                    ret_i32(3);
                    return 3;
                } else if x == 4 {
                    ret_i32(4);
                    return 4;
                }
                // if no explicit return happened above
                ret_i32(5);
                5
            }
        }
        fn main() {
            ret_i32(test(1));
            ret_i32(test(2));
            ret_i32(test(3));
            ret_i32(test(4));
            ret_i32(test(5));
            ret_i32(test(6));
        }
    ));
    assert_all(&result, &[
        1i32,   1, 1,
        2,      2, 2,
        3,      3, 3,
        4,      4, 4,
        5,      5, 5,
        6,      5, 5,
    ]);
}

#[test]
fn unused_result() {
    let result = run(stringify!(
        fn test1(x: i32) -> i64 {
            ret_i32(x);
            return x as i64;
        }
        fn test2(x: i32) -> i64 {
            ret_i32(x);
            x as i64
        }
        fn test3(x: i32) -> i64 {
            ret_i32(x);
            { x as i64 }
        }
        fn main() {
            for i in 0..7 {
                test1(i);
                test2(i);
                test3(i);
            }
        }
    ));
    assert_all(&result, &[
        0i32, 0, 0,
        1, 1, 1,
        2, 2, 2,
        3, 3, 3,
        4, 4, 4,
        5, 5, 5,
        6, 6, 6,
    ]);
}

#[test]
fn dead_code_result() {
    let result = run(stringify!(
        fn result() -> u32 {
            if true {
                return 1;
            } else if true {
                return 2;
            } else {
                return 3;
            }
            let dead_code: f64 = 3.14;
            dead_code
        }
        fn return_() -> u32 {
            if false {
                return 1;
            } else if true {
                return 2;
            } else {
                return 3;
            }
            let dead_code: f64 = 3.14;
            return dead_code;
        }
        fn main() {
            ret_u32(result());
            ret_u32(return_());
        }
    ));
    assert_all(&result, &[
        1u32, 2u32
    ]);
}

#[test]
fn return_void() {
    let result = run(stringify!(
        fn main() {
            ret_u8(1);
            return;
        }
    ));
    assert_all(&result, &[ 1u8 ]);
}

#[test]
fn maybe_return_with_refs() {
    let result = run(stringify!(
        fn test() -> String {
            let x = "Heapref";
            if x == "Heapref" {
                return x;
            }
            x
        }

        fn main() {
            ret_string(test());
        }
    ));
    assert_all(&result, &[ "Heapref".to_string() ]);
}

#[test]
fn assign_to_maybe_uninitialized() {
    let result = run(stringify!(
        let t: String;
        let i = 0;
        if i == 1 {
            t = "Initialized";
            ret_string(t);
        } else {
            //t = "The road not taken";
        }
        // t is now maybe uninitialized
        t = "Overruling";
        ret_string(t);
    ));
    assert_all(&result, &[ "Overruling".to_string() ]);
}

#[test]
fn block_exit_with_maybe_unitialized() {
    let result = run(stringify!(
        fn test(i: u8) {
            let x;
            if i == 1 {
                x = "Hello";
                ret_u8(1);
            } else if i == 2 {
                x = "World";
                ret_u8(2);
                return;
            } else if i == 3 {
                ret_u8(3);
                return;
            }
        }
        fn main() {
            test(1);
            test(2);
            test(3);
        }
    ));
    assert_all(&result, &[ 1u8, 2u8, 3u8 ]);
}

#[test]
fn while_break() {
    let result = run(stringify!(
        let i = 3;
        let x = "Test";
        while i > 0 {
            i -= 1;
            let y = "y"; // testing correct refcount handling for broken-out-of parent scopes
            if i < 2 {
                let z = "z";
                x += y + z;
                break;
            }
        }
        ret_string("{i} {x}");
    ));
    assert_all(&result, &[ "1 Testyz".to_string() ]);
}

#[test]
fn while_continue() {
    let result = run(stringify!(
        let i = 100;
        let j = 5;
        let x = "Test";
        while i > 0 {
            let y = "y";            // testing correct refcount handling for continued-over parent scopes
            if i == 50 && j > 0 {
                j -= 1;
                let z = "z";
                x += y + z;
                continue;
            }
            i -= 1;
        }
        ret_string("{i} {x}");
    ));
    assert_all(&result, &[ "0 Testyzyzyzyzyz".to_string() ]);
}

#[test]
fn for_range_break() {
    let result = run(stringify!(
        let x = "Test";
        let max_i = 0;
        for i in 0..10 {
            max_i = i;
            let y = "y";
            if i > 5 {
                let z = "z";
                x += y + z;
                break;
            }
        }
        ret_string("{max_i} {x}");
    ));
    assert_all(&result, &[ "6 Testyz".to_string() ]);
}

#[test]
fn for_range_continue() {
    let result = run(stringify!(
        let x = "Test";
        let max_i = 0;
        for i in 0..10 {
            max_i = i;
            let y = "y";
            if i > 5 {
                let z = "z";
                x += y + z;
                continue;
            }
            x += "-";
        }
        ret_string("{max_i} {x}");
    ));
    assert_all(&result, &[ "9 Test------yzyzyzyz".to_string() ]);
}

#[test]
fn for_in_continue() {
    let result = run(stringify!(
        let array = [ "a", "b", "c", "d", "e", "f" ];
        let x = "Test";
        let i = 0;
        for e in array {
            let y = "y";
            if i > 2 {
                let z = "z";
                x += e + y + z;
                continue;
            }
            x += "-";
            i += 1;
        }
        ret_string("{i} {x}");
    ));
    assert_all(&result, &[ "3 Test---dyzeyzfyz".to_string() ]);
}

#[test]
fn for_in_break() {
    let result = run(stringify!(
        let array = [ "a", "b", "c" ];
        let x = "Test";
        let i = 0;
        for e in array {
            let y = "y";
            if i > 1 {
                let z = "z";
                x += e + y + z;
                break;
            }
            x += "-";
            i += 1;
        }
        ret_string("{i} {x}");
    ));
    assert_all(&result, &[ "2 Test--cyz".to_string() ]);
}

#[test]
fn for_in_break_noref() {
    let result = run(stringify!(
        let array = [ 1, 2, 3 ];
        let x = "Test";
        let i = 0;
        for e in array {
            let y = "y";
            if i > 1 {
                let z = "z";
                x += (e as String) + y + z;
                break;
            }
            x += "-";
            i += 1;
        }
        ret_string("{i} {x}");
    ));
    assert_all(&result, &[ "2 Test--3yz".to_string() ]);
}

#[test]
fn return_if_expr_with_local() {
    // if expression in tail position with local variable
    let result = run(stringify!(
        fn pick(c: bool) -> u8 {
            if c {
                let y = 7u8;
                y + 1
            } else {
                let z = 3u8;
                z
            }
        }
        fn main() {
            ret_u8(pick(true));
            ret_u8(pick(false));
        }
    ));
    assert_all(&result, &[ 8u8, 3u8 ]);
}

#[test]
fn match_bool_non_exhaustive_rejected() {
    // a bool match covering only one value (no catch-all) must report the missing value as a witness
    let err = build_err(stringify!(
        fn main() {
            ret_i32(match true {
                true => 1,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive") && err.contains("false"), "unexpected error: {}", err);
}

#[test]
fn match_bool_exhaustive_ok() {
    let result = run(stringify!(
        let b = false;
        ret_i32(match b {
            true => 1,
            false => 0,
        });
    ));
    assert_all(&result, &[ 0i32 ]);
}

#[test]
fn match_integer_requires_catchall() {
    // integers have no finite signature, so a literal-only match is never exhaustive
    let err = build_err(stringify!(
        fn main() {
            ret_i32(match 5i32 {
                1 => 1,
                5 => 5,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive") && err.contains("_"), "unexpected error: {}", err);
}

#[test]
fn match_integer_catchall_ok() {
    let result = run(stringify!(
        let x = 5i32;
        ret_i32(match x {
            1 => 1,
            _ => 99,
        });
    ));
    assert_all(&result, &[ 99i32 ]);
}

#[test]
fn match_string_literal_catchall_ok() {
    // strings have no finite signature, so a literal-only string match needs a catch-all arm
    let result = run(stringify!(
        let s = "green";
        ret_i32(match s {
            "red" => 0,
            "green" => 1,
            "blue" => 2,
            _ => 99,
        });
        // the subject survives the match and remains usable afterwards (refcount stayed balanced)
        ret_str(s);
    ));
    assert(&result[0], 1i32);
    assert(&result[1], "green".to_string());
    assert!(result.len() == 2, "unexpected result length {}", result.len());
}

#[test]
fn match_string_requires_catchall() {
    let err = build_err(stringify!(
        fn main() {
            ret_i32(match "x" {
                "x" => 1,
                "y" => 2,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive") && err.contains("_"), "unexpected error: {}", err);
}

#[test]
fn match_string_interpolation_rejected() {
    // an interpolated string is not a literal and must not be accepted as a pattern
    let err = build_err(stringify!(
        fn main() {
            let n = 1;
            ret_i32(match "x" {
                "{n}" => 1,
                _ => 0,
            });
        }
    ));
    assert!(err.len() > 0, "expected parse error for interpolated string pattern");
}

#[test]
fn match_string_nested_in_variant() {
    // exercises the nested-access path: a string sub-pattern inside a data variant, plus a binding arm
    // that keeps the payload alive, validating refcounting across heap boundaries
    let result = run(stringify!(
        enum Msg { Text(String), Quit }
        fn main() {
            let m = Msg::Text("hello");
            ret_i32(match m {
                Msg::Text("hello") => 1,
                Msg::Text(other) => 2,
                Msg::Quit => 3,
            });
        }
    ));
    assert_all(&result, &[ 1i32 ]);
}

#[test]
fn match_range_inclusive() {
    let result = run(stringify!(
        fn classify(n: i32) -> i32 {
            match n {
                0..=9 => 1,
                10..=99 => 2,
                _ => 3,
            }
        }
        fn main() {
            ret_i32(classify(0));   // lower boundary
            ret_i32(classify(9));   // upper boundary (inclusive)
            ret_i32(classify(10));
            ret_i32(classify(99));
            ret_i32(classify(100)); // above
            ret_i32(classify(-1));  // below
        }
    ));
    assert_all(&result, &[ 1i32, 1, 2, 2, 3, 3 ]);
}

#[test]
fn match_range_exclusive() {
    let result = run(stringify!(
        fn classify(n: i32) -> i32 {
            match n {
                0..10 => 1,
                _ => 2,
            }
        }
        fn main() {
            ret_i32(classify(0));
            ret_i32(classify(9));
            ret_i32(classify(10)); // exclusive upper bound does not match
        }
    ));
    assert_all(&result, &[ 1i32, 1, 2 ]);
}

#[test]
fn match_range_float() {
    let result = run(stringify!(
        let x = 2.5f64;
        ret_i32(match x {
            0.0..=1.0 => 1,
            1.0..=3.0 => 2,
            _ => 3,
        });
    ));
    assert_all(&result, &[ 2i32 ]);
}

#[test]
fn match_range_nested_in_struct() {
    // exercises range comparison against a numeric value reached through a heap boundary
    let result = run(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let p = Point { x: 5, y: 50 };
            ret_i32(match p {
                Point { x: 0..=9, y: 0..=9 } => 1,
                Point { x: 0..=9, y: 10..=99 } => 2,
                _ => 3,
            });
        }
    ));
    assert_all(&result, &[ 2i32 ]);
}

#[test]
fn match_range_requires_catchall() {
    // ranges cover an opaque numeric space, so a range-only match is never exhaustive
    let err = build_err(stringify!(
        fn main() {
            ret_i32(match 5i32 {
                0..=9 => 1,
                10..=20 => 2,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive"), "unexpected error: {}", err);
}

#[test]
fn match_range_non_numeric_rejected() {
    let err = build_err(stringify!(
        fn main() {
            ret_i32(match "hello" {
                1..=5 => 1,
                _ => 0,
            });
        }
    ));
    assert!(err.contains("numeric"), "unexpected error: {}", err);
}

#[test]
fn match_or_literals() {
    let result = run(stringify!(
        fn classify(n: i32) -> i32 {
            match n {
                1 | 2 | 3 => 10,
                4 | 5 => 20,
                _ => 0,
            }
        }
        fn main() {
            ret_i32(classify(1));
            ret_i32(classify(3));
            ret_i32(classify(4));
            ret_i32(classify(5));
            ret_i32(classify(6));
        }
    ));
    assert_all(&result, &[ 10i32, 10, 20, 20, 0 ]);
}

#[test]
fn match_or_bool_exhaustive() {
    // `true | false` covers all bool values, so no catch-all is needed
    let result = run(stringify!(
        let b = true;
        ret_i32(match b {
            true | false => 7,
        });
    ));
    assert_all(&result, &[ 7i32 ]);
}

#[test]
fn match_or_enum_variants_exhaustive() {
    // an or-pattern over enum variants plus the remaining variant covers the type without a catch-all
    let result = run(stringify!(
        enum E { A, B, C }
        fn pick(e: E) -> i32 {
            match e {
                E::A | E::B => 1,
                E::C => 2,
            }
        }
        fn main() {
            ret_i32(pick(E::A));
            ret_i32(pick(E::B));
            ret_i32(pick(E::C));
        }
    ));
    assert_all(&result, &[ 1i32, 1, 2 ]);
}

#[test]
fn match_or_enum_non_exhaustive_rejected() {
    let err = build_err(stringify!(
        enum E { A, B, C }
        fn main() {
            ret_i32(match E::A {
                E::A | E::B => 1,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive") && err.contains("C"), "unexpected error: {}", err);
}

#[test]
fn match_or_nested_in_variant() {
    // an or-pattern nested inside a data-variant sub-pattern
    let result = run(stringify!(
        enum E { Num(i32), Other }
        fn classify(e: E) -> i32 {
            match e {
                E::Num(1 | 2 | 3) => 1,
                E::Num(_) => 2,
                E::Other => 3,
            }
        }
        fn main() {
            ret_i32(classify(E::Num(2)));
            ret_i32(classify(E::Num(9)));
            ret_i32(classify(E::Other));
        }
    ));
    assert_all(&result, &[ 1i32, 2, 3 ]);
}

#[test]
fn match_or_leading_pipe() {
    let result = run(stringify!(
        let n = 2;
        ret_i32(match n {
            | 1 | 2 => 5,
            _ => 0,
        });
    ));
    assert_all(&result, &[ 5i32 ]);
}

#[test]
fn match_or_int_requires_catchall() {
    // integers are opaque, so an or of literals is not exhaustive on its own
    let err = build_err(stringify!(
        fn main() {
            ret_i32(match 3i32 {
                1 | 2 => 1,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive"), "unexpected error: {}", err);
}

#[test]
fn match_or_binding_rejected() {
    // a binding (here the catch-all identifier `other`) inside an or-pattern is not allowed in v1
    let err = build_err(stringify!(
        fn main() {
            ret_i32(match 3i32 {
                1 | other => 1,
            });
        }
    ));
    assert!(err.contains("Bindings are not allowed"), "unexpected error: {}", err);
}
