use crate::util::*;

// Note: the parser currently only supports decimal literals, so bit patterns below are written in decimal
// with their binary value noted in comments.

#[test]
fn binary_unsigned() {
    let result = run(stringify!(
        let a = 12u8;       // 1100
        let b = 10u8;       // 1010
        ret_u8(a & b);      // 1000 = 8
        ret_u8(a | b);      // 1110 = 14
        ret_u8(a ^ b);      // 0110 = 6
    ));
    assert_all(&result, &[ 8u8, 14, 6 ]);
}

#[test]
fn binary_signed() {
    let result = run(stringify!(
        let a = 12i32;
        let b = 10i32;
        ret_i32(a & b);
        ret_i32(a | b);
        ret_i32(a ^ b);
    ));
    assert_all(&result, &[ 8i32, 14, 6 ]);
}

#[test]
fn binary_wide() {
    let result = run(stringify!(
        let a = 4042322160u64;  // 0xF0F0F0F0
        let b = 252645135u64;   // 0x0F0F0F0F
        ret_u64(a & b);
        ret_u64(a | b);
        ret_u64(a ^ b);
    ));
    assert_all(&result, &[ 0u64, 4294967295, 4294967295 ]);
}

#[test]
fn shifts_unsigned() {
    let result = run(stringify!(
        ret_u32(1u32 << 0);
        ret_u32(1u32 << 16);
        ret_u32(4278190080u32 >> 24);  // 0xFF000000 >> 24
        ret_u32(128u32 >> 2);
    ));
    assert_all(&result, &[ 1u32, 65536, 255, 32 ]);
}

#[test]
fn shift_right_arithmetic_signed() {
    // signed `>>` sign-extends (arithmetic shift)
    let result = run(stringify!(
        ret_i8(-128i8 >> 1);
        ret_i8(-8i8 >> 1);
    ));
    assert_all(&result, &[ -64i8, -4 ]);
}

#[test]
fn shift_right_logical_unsigned() {
    // unsigned `>>` zero-fills (logical shift)
    let result = run(stringify!(
        ret_u8(128u8 >> 1);
        ret_u8(255u8 >> 4);
    ));
    assert_all(&result, &[ 64u8, 15 ]);
}

#[test]
fn shift_amount_independent_type() {
    // the shift amount can be a different integer type than the value being shifted
    let result = run(stringify!(
        let amount = 4u8;
        let wide = 3u64;
        ret_u32(1u32 << amount);
        ret_u32(256u32 >> wide);
    ));
    assert_all(&result, &[ 16u32, 32 ]);
}

#[test]
fn bitnot_unsigned() {
    let result = run(stringify!(
        ret_u8(!0u8);
        ret_u8(!5u8);
    ));
    assert_all(&result, &[ 255u8, 250 ]);
}

#[test]
fn bitnot_unsigned_wide() {
    let result = run(stringify!( ret_u32(!0u32); ));
    assert_all(&result, &[ 4294967295u32 ]);
}

#[test]
fn bitnot_signed() {
    let result = run(stringify!(
        ret_i8(!0i8);
        ret_i8(!5i8);
    ));
    assert_all(&result, &[ -1i8, -6 ]);
}

#[test]
fn not_still_logical_on_bool() {
    let result = run(stringify!(
        ret_bool(!true);
        ret_bool(!false);
        ret_bool(!(1 < 2));
    ));
    assert_all(&result, &[ false, true, false ]);
}

#[test]
fn precedence() {
    // shift > & > ^ > | > comparison, matching Rust
    let result = run(stringify!(
        let a = 1u8 << 3;           // 8
        let b = 1u8 << 1;           // 2
        ret_u8(a | b & (7 >> 1));   // 8 | (2 & 3) = 8 | 2 = 10
        ret_u8(1u8 | 2 ^ 3);        // 1 | (2 ^ 3) = 1 | 1 = 1
        ret_u8(3u8 & 5 ^ 6);        // (3 & 5) ^ 6 = 1 ^ 6 = 7
    ));
    assert_all(&result, &[ 10u8, 1, 7 ]);
}

#[test]
fn shift_binds_tighter_than_compare() {
    let result = run(stringify!(
        ret_bool(1u8 << 2 == 4);    // (1 << 2) == 4
        ret_bool(4u8 == 1 << 2);
    ));
    assert_all(&result, &[ true, true ]);
}

#[test]
fn logical_operators_still_parse() {
    // ensure adding `&`/`|` did not break `&&` / `||`
    let result = run(stringify!(
        ret_bool(true && false);
        ret_bool(true || false);
        ret_bool(false || (1 < 2));
    ));
    assert_all(&result, &[ false, true, true ]);
}

#[test]
fn compound_assign_var() {
    let result = run(stringify!(
        let mut x = 12u8;   // 1100
        x &= 10;            // & 1010 -> 1000 = 8
        ret_u8(x);
        x |= 1;             // | 0001 -> 1001 = 9
        ret_u8(x);
        x ^= 15;            // ^ 1111 -> 0110 = 6
        ret_u8(x);
        x <<= 2;            // -> 11000 = 24
        ret_u8(x);
        x >>= 1;            // -> 01100 = 12
        ret_u8(x);
    ));
    assert_all(&result, &[ 8u8, 9, 6, 24, 12 ]);
}

#[test]
fn compound_assign_shift_amount_type() {
    let result = run(stringify!(
        let mut x = 1u32;
        let by = 4u8;
        x <<= by;
        ret_u32(x);
    ));
    assert_all(&result, &[ 16u32 ]);
}

#[test]
fn compound_assign_array_element() {
    let result = run(stringify!(
        let mut a = [ 12u8, 10 ];
        a[0] &= 10;     // 1100 & 1010 -> 1000 = 8
        a[1] |= 1;      // 1010 | 0001 -> 1011 = 11
        ret_u8(a[0]);
        ret_u8(a[1]);
        a[0] <<= 1;     // -> 10000 = 16
        ret_u8(a[0]);
    ));
    assert_all(&result, &[ 8u8, 11, 16 ]);
}

#[test]
#[should_panic(expected = "Integer overflow")]
fn overshift_is_runtime_error() {
    run(stringify!(
        let by = 9u32;
        ret_u8(1u8 << by);
    ));
}

#[test]
fn bitwise_on_float_rejected() {
    let err = build_err(stringify!(
        fn main() {
            let a = 1.0f32;
            ret_f32(a & a);
        }
    ));
    assert!(err.contains("integer"), "unexpected error: {}", err);
}

#[test]
fn shift_on_float_rejected() {
    let err = build_err(stringify!(
        fn main() {
            let a = 1.0f32;
            ret_f32(a << 1);
        }
    ));
    assert!(err.contains("integer"), "unexpected error: {}", err);
}
