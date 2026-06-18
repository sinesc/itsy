use crate::util::*;

#[test]
fn fnref_array_loop() {
    let result = run(stringify!(
        fn one(v: u32) -> u32 { v * 3 }
        fn two(v: u32) -> u32 { v * 7 }
        fn three(v: u32) -> u32 { v * 11 }
        fn main() {
            for current in [ one, two, three ] {
                ret_u32(current(3));
            }
        }
    ));
    assert_all(&result, &[ 9u32, 21, 33 ]);
}

#[test]
#[should_panic(expected = "Resolver error: Expected type `fn(u32) -> u32`, got `fn(i32) -> i32` in line 3, column 29.")]
fn fnref_array_mismatch() {
    let result = run(stringify!(
        fn one(v: u32) -> u32 { v * 3 }
        fn two(v: u32) -> u32 { v * 7 }
        fn three(v: i32) -> i32 { v * 11 }
        fn main() {
            for current in [ one, two, three ] {
                ret_u32(current(3));
            }
        }
    ));
    assert_all(&result, &[ 9u32, 21, 33 ]);
}

#[test]
fn fnref_inference() {
    let result = run(stringify!(
        fn main() {
            let a = one;
            let b = a;
            let c = b;
            ret_u32(c(7));
            ret_u32(b(3));
        }
        fn one(v: u32) -> u32 { v * 3 }
    ));
    assert_all(&result, &[ 21u32, 9 ]);
}

#[test]
fn fnref_access_index() {
    let result = run(stringify!(
        fn a(v: String) {
            ret_str("a via {v}");
        }
        fn b(v: String) {
            ret_str("b via {v}");
        }
        fn c(v: String) -> u8  {
            ret_str("c via {v}");
            123
        }
        struct MyFns {
            myvoid: fn(String),
            myret: fn(String) -> u8,
        }
        fn main() {
            let x = [ a, b ];
            let y = MyFns { myvoid: a, myret: c };
            x[0]("x[0]");
            x[1]("x[1]");
            y.myvoid("y.myvoid");
            ret_str(y.myret("y.myret") as String);
        }
    ));
    assert_all(&result, &[
        "a via x[0]".to_string(), "b via x[1]".to_string(),
        "a via y.myvoid".to_string(), "c via y.myret".to_string(),
        "123".to_string() ]);
}

#[test]
fn anonymous() {
    let result = run(stringify!(
        fn apply(operation: fn(u32) -> u32, value: u32) -> u32 {
            operation(value)
        }
        fn main() {
            let double = fn(x: u32) -> u32 { x * 2 };
            let get_double = fn() -> fn(u32) -> u32 { fn(x: u32) -> u32 { x * 2 } };
            ret_u32(double(7));
            ret_u32(apply(double, 13));
            ret_u32(apply(get_double(), 2));
        }
    ));
    assert_all(&result, &[ 14u32, 26, 4 ]);
}

#[test]
fn shadowing() {
    let result = run(stringify!(
        let a: u8 = 1;
        ret_u8(a);
        let capture1 = || -> u8 a;
        let capture2 = {
            let a: u8 = 2;
            ret_u8(a);
            || -> u8 a
        };
        let a: u8 = 3;
        ret_u8(a);
        let capture3 = || -> u8 a;
        ret_u8(capture1());
        ret_u8(capture2());
        ret_u8(capture3());
    ));
    assert_all(&result, &[ 1u8, 2, 3, 1, 2, 3 ]);
}

#[test]
fn capture_ref() {
    let result = run(stringify!(
        let captured_name = "c";
        let c = |x: u8| -> String "{captured_name}: {x}";
        ret_str(c(123));
    ));
    assert_all(&result, &[ "c: 123".to_string() ]);
}

#[test]
fn infer_return_from_body() {
    // closure return type is inferred from the (parser-synthesized) return statement
    let result = run(stringify!(
        fn main() {
            let f = |m: u8| 2 * m;
            ret_u8(f(3));
        }
    ));
    assert_all(&result, &[ 6u8 ]);
}

#[test]
fn infer_param_from_body() {
    // closure parameter type is inferred from how it is used in the body
    let result = run(stringify!(
        fn main() {
            let f = |m| 2.0f32 * m;
            ret_f32(f(3.0f32));
        }
    ));
    assert_all(&result, &[ 6.0f32 ]);
}

#[test]
fn infer_param_from_annotation() {
    // closure parameter and return types are inferred from the let binding's callable type annotation
    let result = run(stringify!(
        fn main() {
            let f: fn(f32) -> f32 = |m| 2.0f32 * m;
            ret_f32(f(3.0f32));
        }
    ));
    assert_all(&result, &[ 6.0f32 ]);
}

#[test]
fn infer_param_from_call_argument() {
    // closure parameter type is inferred from the callable type expected by the called function
    let result = run(stringify!(
        fn apply(op: fn(u32) -> u32, value: u32) -> u32 {
            op(value)
        }
        fn main() {
            ret_u32(apply(|x| x * 2, 5));
        }
    ));
    assert_all(&result, &[ 10u32 ]);
}

#[test]
#[should_panic(expected = "Resolver error: Cannot resolve")]
fn infer_param_ambiguous() {
    // a parameter that cannot be inferred from body or context must produce a hard error
    run(stringify!(
        fn main() {
            let f = |x| x;
            f(3);
        }
    ));
}

#[test]
#[should_panic(expected = "Resolver error")]
fn infer_param_call_argument_mismatch() {
    // an explicitly typed closure parameter that conflicts with the expected callable type must error
    run(stringify!(
        fn apply(op: fn(u32) -> u32, value: u32) -> u32 {
            op(value)
        }
        fn main() {
            ret_u32(apply(|x: i32| x * 2, 5));
        }
    ));
}

#[test]
fn block_explicit_return() {
    // closure with a block body and an explicit return statement, return type inferred from the returned expression
    let result = run(stringify!(
        fn main() {
            let f = |x: u8| { return x * 2; };
            ret_u8(f(3));
        }
    ));
    assert_all(&result, &[ 6u8 ]);
}

#[test]
fn block_explicit_return_annotated() {
    // closure with a block body, explicit return statement and explicit return type annotation
    let result = run(stringify!(
        fn main() {
            let f = |x: u8| -> u8 { return x * 2; };
            ret_u8(f(3));
        }
    ));
    assert_all(&result, &[ 6u8 ]);
}

#[test]
fn block_multiple_statements_return() {
    // closure with a multi-statement block body ending in a return statement
    let result = run(stringify!(
        fn main() {
            let f = |x: u8| {
                let y = x * 2;
                return y + 1;
            };
            ret_u8(f(3));
        }
    ));
    assert_all(&result, &[ 7u8 ]);
}

#[test]
fn block_early_return() {
    // closure with a block body containing an early return; both return statements share the inferred return type
    let result = run(stringify!(
        fn main() {
            let f = |x: u8| {
                if x > 5 {
                    return 100;
                }
                return x;
            };
            ret_u8(f(9));
            ret_u8(f(2));
        }
    ));
    assert_all(&result, &[ 100u8, 2u8 ]);
}

#[test]
fn block_return_infer_param_from_call_argument() {
    // closure with a block body and return statement, parameter type inferred from the expected callable type
    let result = run(stringify!(
        fn apply(op: fn(u32) -> u32, value: u32) -> u32 {
            op(value)
        }
        fn main() {
            ret_u32(apply(|x| { return x * 2; }, 5));
        }
    ));
    assert_all(&result, &[ 10u32 ]);
}

#[test]
#[should_panic(expected = "Resolver error")]
fn block_conflicting_returns() {
    // a closure block whose return statements disagree on the return type must error
    run(stringify!(
        fn main() {
            let f = |x: u8| {
                if x > 5 {
                    return 100;
                }
                return "no";
            };
            ret_u8(f(9));
        }
    ));
}

#[test]
#[should_panic(expected = "Type `i32` is not callable")]
fn local_shadows_function_then_called() {
    // a local binding shadowing a function and then being called must produce "not callable" error
    run(stringify!(
        fn foo() -> u32 { 5 }
        fn main() {
            let foo = 0;
            foo();
        }
    ));
}
