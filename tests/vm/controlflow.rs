use crate::util::*;

#[test]
fn recursion() {
    let result = run("
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
    ");
    assert_all(&result, &[ 1i32, 1, 5, 13 ]);
}

#[test]
fn branching() {
    let result = run("
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
    ");
    assert_all(&result, &[ 1i32, 4, 2 ]);
}

#[test]
fn branching_ref_cleanup() {
    let result = run("
        if true {
            let a = \"Hello\";
            ret_string(a);
        } else {
            let b = \"World\";
            ret_string(b);
        }
    ");
    assert_all(&result, &[ "Hello".to_string() ]);
}

#[test]
fn explicit_return() {
    // todo: add bytecode test, check dead code was removed
    let result = run("
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
    ");
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
    let result = run("
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
    ");
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
    let result = run("
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
    ");
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
    let result = run("
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
    ");
    assert_all(&result, &[
        1u32, 2u32
    ]);
}

#[test]
fn return_void() {
    let result = run("
        fn main() {
            ret_u8(1);
            return;
        }
    ");
    assert_all(&result, &[ 1u8 ]);
}

#[test]
fn maybe_return_with_refs() {
    let result = run("
        fn test() -> String {
            let x = \"Heapref\";
            if x == \"Heapref\" {
                return x;
            }
            x
        }

        fn main() {
            ret_string(test());
        }
    ");
    assert_all(&result, &[ "Heapref".to_string() ]);
}

#[test]
fn assign_to_maybe_uninitialized() {
    let result = run("
        let t: String;
        let i = 0;
        if (i == 1) {
            t = \"Initialized\";
            ret_string(t);
        } else {
            //t = \"The road not taken\";
        }
        // t is now maybe uninitialized
        t = \"Overruling\";
        ret_string(t);
    ");
    assert_all(&result, &[ "Overruling".to_string() ]);

}

#[test]
fn block_exit_with_maybe_unitialized() {
    let result = run("
        fn test(i: u8) {
            let x;
            if i == 1 {
                x = \"Hello\";
                ret_u8(1);
            } else if i == 2 {
                x = \"World\";
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
    ");
    assert_all(&result, &[ 1u8, 2u8, 3u8 ]);
}