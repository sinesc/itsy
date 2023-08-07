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