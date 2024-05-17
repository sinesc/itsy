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