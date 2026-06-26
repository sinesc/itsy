use crate::util::*;

#[test]
fn isqrt_unsigned() {
    let result = run(stringify!(
        ret_u32(0u32.isqrt());
        ret_u32(1u32.isqrt());
        ret_u32(16u32.isqrt());
        ret_u32(17u32.isqrt());   // rounds down
        ret_u32(15u32.isqrt());   // rounds down
        ret_u32(4294967295u32.isqrt()); // u32::MAX -> 65535
        ret_u8(255u8.isqrt());    // -> 15
    ));
    assert_all!(&result, [ 0u32, 1u32, 4u32, 4u32, 3u32, 65535u32, 15u8 ]);
}

#[test]
fn isqrt_signed() {
    let result = run(stringify!(
        ret_i32(0i32.isqrt());
        ret_i32(100i32.isqrt());
        ret_i32(99i32.isqrt());   // rounds down -> 9
        ret_i64(9223372036854775807i64.isqrt()); // i64::MAX -> 3037000499
    ));
    assert_all!(&result, [ 0i32, 10i32, 9i32, 3037000499i64 ]);
}

#[test]
#[should_panic(expected = "Invalid argument")]
fn isqrt_negative_is_runtime_error() {
    run(stringify!(
        let n = -4;
        ret_i32(n.isqrt());
    ));
}

#[test]
fn ilog_unsigned() {
    let result = run(stringify!(
        ret_u32(8u32.ilog(2u32));   // -> 3
        ret_u32(9u32.ilog(3u32));   // -> 2
        ret_u32(7u32.ilog(2u32));   // rounds down -> 2
        ret_u32(1u32.ilog(2u32));   // -> 0
        ret_u32(1000u32.ilog(10u32)); // -> 3
    ));
    assert_all!(&result, [ 3u32, 2u32, 2u32, 0u32, 3u32 ]);
}

#[test]
fn ilog_signed() {
    let result = run(stringify!(
        ret_u32(8i32.ilog(2i32));   // -> 3
        ret_u32(81i32.ilog(3i32));  // -> 4
    ));
    assert_all!(&result, [ 3u32, 4u32 ]);
}

#[test]
#[should_panic(expected = "Invalid argument")]
fn ilog_zero_is_runtime_error() {
    run(stringify!(
        ret_u32(0u32.ilog(2u32));
    ));
}

#[test]
#[should_panic(expected = "Invalid argument")]
fn ilog_negative_is_runtime_error() {
    run(stringify!(
        let n = -8;
        ret_u32(n.ilog(2));
    ));
}

#[test]
#[should_panic(expected = "Invalid argument")]
fn ilog_base_below_two_is_runtime_error() {
    run(stringify!(
        ret_u32(8u32.ilog(1u32));
    ));
}

#[test]
fn ilog2_and_ilog10() {
    let result = run(stringify!(
        ret_u32(8u32.ilog2());      // -> 3
        ret_u32(7u32.ilog2());      // rounds down -> 2
        ret_u32(1u32.ilog2());      // -> 0
        ret_u32(16i32.ilog2());     // signed -> 4
        ret_u32(1000u32.ilog10());  // -> 3
        ret_u32(999u32.ilog10());   // rounds down -> 2
        ret_u32(1i32.ilog10());     // signed -> 0
    ));
    assert_all!(&result, [ 3u32, 2u32, 0u32, 4u32, 3u32, 2u32, 0u32 ]);
}

#[test]
#[should_panic(expected = "Invalid argument")]
fn ilog2_zero_is_runtime_error() {
    run(stringify!(
        ret_u32(0u32.ilog2());
    ));
}

#[test]
#[should_panic(expected = "Invalid argument")]
fn ilog10_negative_is_runtime_error() {
    run(stringify!(
        let n = -10;
        ret_u32(n.ilog10());
    ));
}
