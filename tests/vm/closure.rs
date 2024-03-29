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
#[should_panic(expected = "Resolver error: Expected type fn(u32) -> u32, got fn(i32) -> i32 in line 3, column 29.")]
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