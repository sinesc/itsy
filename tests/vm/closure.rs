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