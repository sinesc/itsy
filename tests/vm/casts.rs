use crate::util::*;

#[test]
fn compound_i8() {
    let result = run(stringify!(
        let mut x = 1;
        x += 1;
        ret_i8(x);
        x *= 6;
        ret_i8(x);
        x -= 3;
        ret_i8(x);
        x /= 3;
        ret_i8(x);
        x %= 2;
        ret_i8(x);
    ));
    assert_all(&result, &[ 2i8, 12, 9, 3, 1 ]);
}

#[test]
fn compound_i64() {
    let result = run(stringify!(
        let mut x = 10000000000;
        x += 1;
        ret_i64(x);
        x *= 6;
        ret_i64(x);
        x -= 6;
        ret_i64(x);
        x /= 3;
        ret_i64(x);
        x %= 2;
        ret_i64(x);
    ));
    assert_all(&result, &[ 10000000001i64, 60000000006, 60000000000, 20000000000, 0 ]);
}

#[test]
fn compound_f64() {
    let result = run(stringify!(
        let mut x = 1.000001;
        x += 1.0;
        ret_f64(x);
        x *= 6.0;
        ret_f64(x);
        x -= 3.0;
        ret_f64(x);
        x /= 3.0;
        ret_f64(x);
        x %= 3.0;
        ret_f64(x);
    ));
    assert_all(&result, &[
        1.000001f64 + 1.0,
        (1.000001 + 1.0) * 6.0,
        ((1.000001 + 1.0) * 6.0) - 3.,
        (((1.000001 + 1.0) * 6.0) - 3.) / 3.,
        ((((1.000001 + 1.0) * 6.0) - 3.) / 3.) % 3.,
    ]);
}

// --- cast unsigned to unsigend ---

#[allow(overflowing_literals)]
#[test]
fn cast_u8_u16() {
    let result = run(stringify!(
        ret_u16(23u8 as u16);
        ret_u16(255u8 as u16);
        ret_u16(243u8 as u16);
    ));
    assert_all(&result, &[ 23u16, 255, 243 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_u64_u16() {
    let result = run(stringify!(
        ret_u16(23u64 as u16);
        ret_u16(280u64 as u16);
        ret_u16(243u64 as u16);
    ));
    assert_all(&result, &[ 23u64 as u16, 280u64 as u16, 243u64 as u16 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_u8_u64() {
    let result = run(stringify!(
        ret_u64(23u8 as u64);
        ret_u64(255u8 as u64);
        ret_u64(7u8 as u64);
    ));
    assert_all(&result, &[ 23u64, 255, 7 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_u64_u8() {
    let result = run(stringify!(
        ret_u8(23u64 as u8);
        ret_u8(280u64 as u8);
        ret_u8(243u64 as u8);
    ));
    assert_all(&result, &[ 23u8, 255, 243 ]);
}

// --- cast signed to signed ---

#[allow(overflowing_literals)]
#[test]
fn cast_i8_i16() {
    let result = run(stringify!(
        ret_i16(-23i8 as i16);
        ret_i16(-125i8 as i16);
        ret_i16(127i8 as i16);
    ));
    assert_all(&result, &[ -23i16, -125, 127 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_i16() {
    let result = run(stringify!(
        ret_i16(-23i64 as i16);
        ret_i16(-280i64 as i16);
        ret_i16(243i64 as i16);
    ));
    assert_all(&result, &[ -23i64 as i16, -280i64 as i16, 243i64 as i16 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i8_i64() {
    let result = run(stringify!(
        ret_i64(-23i8 as i64);
        ret_i64(-128i8 as i64);
        ret_i64(125i8 as i64);
    ));
    assert_all(&result, &[ -23i64, -128, 125 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_i8() {
    let result = run(stringify!(
        ret_i8(-23i64 as i8);
        ret_i8(-280i64 as i8);
        ret_i8(243i64 as i8);
    ));
    assert_all(&result, &[ -23i8, -128, 127 ]);
}

// --- cast signed to unsigned ---

#[allow(overflowing_literals)]
#[test]
fn cast_i8_u16() {
    let result = run(stringify!(
        ret_u16(-23i8 as u16);
        ret_u16(-128i8 as u16);
        ret_u16(127i8 as u16);
    ));
    assert_all(&result, &[ 0u16, 0, 127 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_u16() {
    let result = run(stringify!(
        ret_u16(-23i64 as u16);
        ret_u16(-280i64 as u16);
        ret_u16(243i64 as u16);
    ));
    assert_all(&result, &[ 0u16, 0, 243 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i8_u64() {
    let result = run(stringify!(
        ret_u64(-23i8 as u64);
        ret_u64(-128i8 as u64);
        ret_u64(127i8 as u64);
    ));
    assert_all(&result, &[ 0u64, 0, 127 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_u8() {
    let result = run(stringify!(
        ret_u8(-23i64 as u8);
        ret_u8(-280i64 as u8);
        ret_u8(243i64 as u8);
    ));
    assert_all(&result, &[ 0u8, 0, 243 ]);
}

// --- cast float to float ---

#[test]
fn cast_f64_f32() {
    let result = run(stringify!(
        ret_f32(3.1415f64 as f32);
        ret_f32(-3.1415f64 as f32);
        ret_f32(68123.45f64 as f32);
    ));
    assert_all(&result, &[ 3.1415f64 as f32, -3.1415f64 as f32, 68123.45f64 as f32 ]);
}

#[test]
fn cast_f32_f64() {
    let result = run(stringify!(
        ret_f64(3.1415f32 as f64);
        ret_f64(-3.1415f32 as f64);
        ret_f64(68123.45f32 as f64);
    ));
    assert_all(&result, &[ 3.1415f32 as f64, -3.1415f32 as f64, 68123.45f32 as f64 ]);
}

// --- cast signed to float ---

#[test]
fn cast_i16_f64() {
    let result = run(stringify!(
        ret_f64(3i16 as f64);
        ret_f64(-3i16 as f64);
        ret_f64(-9731i16 as f64);
    ));
    assert_all(&result, &[ 3i16 as f64, -3i16 as f64, -9731i16 as f64 ]);
}

#[test]
fn cast_i32_f32() {
    let result = run(stringify!(
        ret_f32(3i32 as f32);
        ret_f32(-3i32 as f32);
        ret_f32(-9731i32 as f32);
    ));
    assert_all(&result, &[ 3i32 as f32, -3i32 as f32, -9731i32 as f32 ]);
}

// --- cast float to signed ---

#[test]
fn cast_f64_i16() {
    let result = run(stringify!(
        ret_i16(3.1415f64 as i16);
        ret_i16(-3.1415f64 as i16);
        ret_i16(68123.45f64 as i16);
        ret_i16(2147483657f64 as i16);
        ret_i16(-2147483657f64 as i16);
    ));
    assert_all(&result, &[ 3i16, -3, 32767, 32767, -32768 ]);
}

#[test]
fn cast_f32_i8() {
    let result = run(stringify!(
        ret_i8(3.1415f32 as i8);
        ret_i8(-3.1415f32 as i8);
        ret_i8(68123.45f32 as i8);
        ret_i8(2147483657f64 as i8);
        ret_i8(-2147483657f64 as i8);
    ));
    assert_all(&result, &[ 3i8, -3, 127, 127, -128 ]);
}

// --- cast float to unsigned ---

#[test]
fn cast_f32_u16() {
    let result = run(stringify!(
        ret_u16(3.1415f32 as u16);
        ret_u16(-3.1415f32 as u16);
        ret_u16(68123.45f32 as u16);
        ret_u16(2147483657f32 as u16);
        ret_u16(-2147483657f32 as u16);
    ));
    assert_all(&result, &[ 3u16, 0, 65535, 65535, 0 ]);
}