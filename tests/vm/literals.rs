use crate::util::*;

#[test]
fn hex_unsigned() {
    let result = run(stringify!(
        ret_u8(0xff);       // 255
        ret_u8(0x0a);       // 10
        ret_u8(0x00);       // 0
    ));
    assert_all(&result, &[ 255u8, 10, 0 ]);
}

#[test]
fn hex_wide_unsigned() {
    let result = run(stringify!(
        ret_u64(0xbeef);
        ret_u64(0xdeadbeef);
        ret_u64(0xf0f0f0f0f0f0f0f0);
    ));
    assert_all(&result, &[ 0xbeefu64, 0xdeadbeef, 0xf0f0f0f0f0f0f0f0 ]);
}

#[test]
fn mixed_widths() {
    // exercises the heterogeneous assert_all! macro across several result types
    let result = run(stringify!(
        ret_u8(0xff);
        ret_u16(0xbeef);
        ret_u32(0xdeadbeef);
        ret_i16(-0x80);
        ret_u64(0xf0f0f0f0f0f0f0f0);
    ));
    assert_all!(&result, [ 0xffu8, 0xbeefu16, 0xdeadbeefu32, -128i16, 0xf0f0f0f0f0f0f0f0u64 ]);
}

#[test]
fn binary_unsigned() {
    let result = run(stringify!(
        ret_u8(0b1100_1010);    // 202
        ret_u8(0b11111111);     // 255
        ret_u8(0b0);            // 0
    ));
    assert_all(&result, &[ 202u8, 255, 0 ]);
}

#[test]
fn octal_unsigned() {
    let result = run(stringify!(
        ret_u16(0o1234);        // 668
        ret_u16(0o0);           // 0
        ret_u16(0o7_7);         // 63
    ));
    assert_all(&result, &[ 668u16, 0, 63 ]);
}

#[test]
fn typed_suffix() {
    let result = run(stringify!(
        ret_u8(0xffu8);
        ret_u8(0b11001010u8);
        ret_u8(0o77u8);         // 63
    ));
    assert_all(&result, &[ 255u8, 202, 63 ]);
}

#[test]
fn signed_suffix() {
    let result = run(stringify!(
        ret_i32(0x7fffi32);     // 32767
        ret_i32(0b1010i32);     // 10
    ));
    assert_all(&result, &[ 32767i32, 10 ]);
}

#[test]
fn negated() {
    // magnitude is evaluated first, then negated
    let result = run(stringify!(
        ret_i32(-0x7f);         // -127
        ret_i32(-0xff);         // -255
        ret_i32(-0b1000_0000);  // -128 (magnitude 128 then negate)
        ret_i32(-0o10);         // -8
    ));
    assert_all(&result, &[ -127i32, -255, -128, -8 ]);
}

#[test]
fn negated_min_fits_typed() {
    // -0x80i8 == -128 is in range even though the magnitude 128 alone is not a valid i8
    let result = run(stringify!(
        ret_i8(-0x80i8);
        ret_i8(-0b1000_0000i8);
    ));
    assert_all(&result, &[ -128i8, -128 ]);
}

#[test]
fn in_range_and_arithmetic() {
    let result = run(stringify!(
        let mut sum = 0u32;
        for i in 0x0..0x4 { sum += i; }     // 0+1+2+3 = 6
        ret_u32(sum);
        ret_u32(0xf0 + 0x0f);               // 255
        ret_u32(0b1111 & 0b1010);           // 10
    ));
    assert_all(&result, &[ 6u32, 255, 10 ]);
}

#[test]
fn suffix_letters_are_hex_digits() {
    // `f` is a hex digit, so `0xfff32` is the value 0xfff32, not `0xf` with an `f32` suffix
    let result = run(stringify!(
        ret_u32(0xfff32);       // 1048370
        ret_u32(0xface);        // 64206
    ));
    assert_all(&result, &[ 0xfff32u32, 0xface ]);
}

#[test]
fn decimal_zero_unaffected() {
    let result = run(stringify!(
        ret_u8(0);
        ret_u8(0u8);
    ));
    assert_all(&result, &[ 0u8, 0 ]);
}

#[test]
fn underscores() {
    let result = run(stringify!(
        ret_u32(0xdead_beef);
        ret_u32(0b1010_1010);
    ));
    assert_all(&result, &[ 0xdead_beefu32, 0b1010_1010 ]);
}
