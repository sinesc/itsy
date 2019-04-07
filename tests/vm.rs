mod util;
use util::*;

#[test]
fn op_native_stack_value() {
    let result = run("
        ret_i32(1 + 4);
        ret_i32(1 + 4 * 2);
        ret_i32((1 + 4) * 2);
        ret_i32(5 - 7);
        ret_i32(5 - 7 * 2);
        ret_i32((5 - 7 * 2) / 3);
        ret_i32(5 - 8 * 2 / 4);
    ");
    assert_all(&result, &[ 5i32, 9, 10, -2, -9, -3, 1 ]);
}

#[test]
fn op_numerics() {
    let result = run("
        ret_u8( 255 - 1 );
        ret_u16( 65535 - 2 );
        ret_u32( 4294967295 - 3 );
        ret_u64( 18446744073709551615 - 4 );

        ret_i8( 127 - 5 );
        ret_i16( 32767 - 6 );
        ret_i32( 2147483647 - 7 );
        ret_i64( 9223372036854775807 - 8 );

        ret_f32( 1234567.0 * 7654321.0 );
        ret_f64( 123456789.0 * 987654321.0 );
    ");

    assert(&result[0], 255u8 - 1);
    assert(&result[1], 65535u16 - 2);
    assert(&result[2], 4294967295u32 - 3);
    assert(&result[3], 18446744073709551615u64 - 4);

    assert(&result[4], 127i8 - 5);
    assert(&result[5], 32767i16 - 6);
    assert(&result[6], 2147483647i32 - 7);
    assert(&result[7], 9223372036854775807i64 - 8);

    assert(&result[8], 1234567.0f32 * 7654321.0);
    assert(&result[9], 123456789.0f64 * 987654321.0);
}

#[test]
fn op_bool() {
    let result = run("
        ret_bool(true && true);
        ret_bool(true && false);
        ret_bool(false && true);
        ret_bool(false && false);

        ret_bool(true || true);
        ret_bool(true || false);
        ret_bool(false || true);
        ret_bool(false || false);

        ret_bool(!false);
        ret_bool(!true);

        ret_bool(true && !false);
        ret_bool(!false && !false);
        ret_bool(!true || false);
    ");
    assert_all(&result, &[
        true, false, false, false,
        true, true, true, false,
        true, false,
        true && !false,
        !false && !false,
        !true || false
    ]);
}

#[test]
fn compound_i8() {
    let result = run("
        let x = 1;
        x += 1;
        ret_i8(x);
        x *= 6;
        ret_i8(x);
        x -= 3;
        ret_i8(x);
        x /= 3;
        ret_i8(x);
    ");
    assert_all(&result, &[ 2i8, 12, 9, 3 ]);
}

#[test]
fn compound_i64() {
    let result = run("
        let x = 10000000000;
        x += 1;
        ret_i64(x);
        x *= 6;
        ret_i64(x);
        x -= 6;
        ret_i64(x);
        x /= 3;
        ret_i64(x);
    ");
    assert_all(&result, &[ 10000000001i64, 60000000006, 60000000000, 20000000000 ]);
}

#[test]
fn compound_f64() {
    let result = run("
        let x = 1.000001;
        x += 1.0;
        ret_f64(x);
        x *= 6.0;
        ret_f64(x);
        x -= 3.0;
        ret_f64(x);
        x /= 3.0;
        ret_f64(x);
    ");
    assert_all(&result, &[
        1.000001f64 + 1.0,
        (1.000001 + 1.0) * 6.0,
        ((1.000001 + 1.0) * 6.0) - 3.,
        (((1.000001 + 1.0) * 6.0) - 3.) / 3.
    ]);
}

// --- cast signed to signed ---

#[allow(overflowing_literals)]
#[test]
fn cast_i8_i16() {
    let result = run("
        ret_i16(-23i8 as i16);
        ret_i16(-280i8 as i16);
        ret_i16(243i8 as i16);
    ");
    assert_all(&result, &[ -23i8 as i16, -280i8 as i16, 243i8 as i16 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_i16() {
    let result = run("
        ret_i16(-23i64 as i16);
        ret_i16(-280i64 as i16);
        ret_i16(243i64 as i16);
    ");
    assert_all(&result, &[ -23i64 as i16, -280i64 as i16, 243i64 as i16 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i8_i64() {
    let result = run("
        ret_i64(-23i8 as i64);
        ret_i64(-280i8 as i64);
        ret_i64(243i8 as i64);
    ");
    assert_all(&result, &[ -23i8 as i64, -280i8 as i64, 243i8 as i64 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_i8() {
    let result = run("
        ret_i8(-23i64 as i8);
        ret_i8(-280i64 as i8);
        ret_i8(243i64 as i8);
    ");
    assert_all(&result, &[ -23i64 as i8, -280i64 as i8, 243i64 as i8 ]);
}

// --- cast signed to unsigned ---

#[allow(overflowing_literals)]
#[test]
fn cast_i8_u16() {
    let result = run("
        ret_u16(-23i8 as u16);
        ret_u16(-280i8 as u16);
        ret_u16(243i8 as u16);
    ");
    assert_all(&result, &[ -23i8 as u16, -280i8 as u16, 243i8 as u16 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_u16() {
    let result = run("
        ret_u16(-23i64 as u16);
        ret_u16(-280i64 as u16);
        ret_u16(243i64 as u16);
    ");
    assert_all(&result, &[ -23i64 as u16, -280i64 as u16, 243i64 as u16 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i8_u64() {
    let result = run("
        ret_u64(-23i8 as u64);
        ret_u64(-280i8 as u64);
        ret_u64(243i8 as u64);
    ");
    assert_all(&result, &[ -23i8 as u64, -280i8 as u64, 243i8 as u64 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_u8() {
    let result = run("
        ret_u8(-23i64 as u8);
        ret_u8(-280i64 as u8);
        ret_u8(243i64 as u8);
    ");
    assert_all(&result, &[ -23i64 as u8, -280i64 as u8, 243i64 as u8 ]);
}

// --- cast float to float ---

#[test]
fn cast_f64_f32() {
    let result = run("
        ret_f32(3.1415f64 as f32);
        ret_f32(-3.1415f64 as f32);
        ret_f32(68123.45f64 as f32);
    ");
    assert_all(&result, &[ 3.1415f64 as f32, -3.1415f64 as f32, 68123.45f64 as f32 ]);
}

#[test]
fn cast_f32_f64() {
    let result = run("
        ret_f64(3.1415f32 as f64);
        ret_f64(-3.1415f32 as f64);
        ret_f64(68123.45f32 as f64);
    ");
    assert_all(&result, &[ 3.1415f32 as f64, -3.1415f32 as f64, 68123.45f32 as f64 ]);
}

// --- cast signed to float ---

#[test]
fn cast_i16_f64() {
    let result = run("
        ret_f64(3i16 as f64);
        ret_f64(-3i16 as f64);
        ret_f64(-9731i16 as f64);
    ");
    assert_all(&result, &[ 3i16 as f64, -3i16 as f64, -9731i16 as f64 ]);
}

#[test]
fn cast_i32_f32() {
    let result = run("
        ret_f32(3i32 as f32);
        ret_f32(-3i32 as f32);
        ret_f32(-9731i32 as f32);
    ");
    assert_all(&result, &[ 3i32 as f32, -3i32 as f32, -9731i32 as f32 ]);
}

// --- cast float to signed ---

#[test]
fn cast_f64_i16() {
    let result = run("
        ret_i16(3.1415f64 as i16);
        ret_i16(-3.1415f64 as i16);
        ret_i16(68123.45f64 as i16);
        ret_i16(2147483657f64 as i16);
        ret_i16(-2147483657f64 as i16);
    ");
    // this is actually buggy in rust: https://github.com/rust-lang/rust/issues/10184
    // so we need to assign the value to a variable first, then cast it. additionally
    // on debug compiles values > i32::MAX require a cast to i64 first
    let values = [ 3.1415f64, -3.1415f64, 68123.45f64, 2147483657f64, -2147483657f64 ];
    assert_all(&result, &[ values[0] as i16, values[1] as i16, values[2] as i16, (values[3] as i64) as i16, (values[4] as i64) as i16 ]);
}

#[test]
fn cast_f32_i8() {
    let result = run("
        ret_i8(3.1415f32 as i8);
        ret_i8(-3.1415f32 as i8);
        ret_i8(68123.45f32 as i8);
        ret_i8(2147483657f64 as i8);
        ret_i8(-2147483657f64 as i8);
    ");
    // this is actually buggy in rust: https://github.com/rust-lang/rust/issues/10184
    // so we need to assign the value to a variable first, then cast it. additionally
    // on debug compiles values > i32::MAX require a cast to i64 first
    let values = [ 3.1415f64, -3.1415f64, 68123.45f64, 2147483657f64, -2147483657f64 ];
    assert_all(&result, &[ values[0] as i8, values[1] as i8, values[2] as i8, (values[3] as i64) as i8, (values[4] as i64) as i8 ]);
}

// --- cast float to unsigned ---

#[test]
fn cast_f32_u16() {
    let result = run("
        ret_u16(3.1415f32 as u16);
        ret_u16(-3.1415f32 as u16);
        ret_u16(68123.45f32 as u16);
        ret_u16(2147483657f32 as u16);
        ret_u16(-2147483657f32 as u16);
    ");
    // this is actually buggy in rust: https://github.com/rust-lang/rust/issues/10184
    // so we need to assign the value to a variable first, then cast it:
    let values = [ 3.1415f32, -3.1415f32, 68123.45f32, 2147483657f32, -2147483657f32 ];
    assert_all(&result, &[ values[0] as u16, values[1] as u16, values[2] as u16, (values[3] as i64) as u16, (values[4] as i64) as u16 ]);
}

#[test]
fn op_order_gt() {
    let result = run("
        let x = 1u8;
        ret_bool(x*3 > --x*3);
        ret_bool(x == 0);
    ");
    assert_all(&result, &[ true, true ]);
}
/*
#[test]
fn op_order_lt() {
    let result = run("
        let y = 1u8;
        ret_bool(y*3 < ++y*3);
        ret_bool(y == 2);
    ");
    assert_all(&result, &[ true, true ]);
}
*/
#[test]
fn branching() {
    let result = run("
        let x = 1;
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
fn array_literal() {
    let result = run("
        let x = [ 1, 2, 3, 4 ];
        ret_i32(x[0]);
        ret_i32(x[1]);
        ret_i32(x[2]);
        ret_i32(x[3]);
    ");
    assert_all(&result, &[ 1i32, 2, 3, 4 ]);
}

#[test]
fn array_nesting() {
    let result = run("
        let x = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
        ret_i32(x[0][0]);
        ret_i32(x[0][1]);
        ret_i32(x[0][2]);
        ret_i32(x[1][0]);
        ret_i32(x[1][1]);
        ret_i32(x[1][2]);
    ");
    assert_all(&result, &[ 1i32, 2, 3, 4, 5, 6 ]);
}

#[test]
fn array_subindex() {
    let result = run("
        let x = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
        let y = x[0];
        let z = x[1];
        ret_i32(y[0]);
        ret_i32(y[1]);
        ret_i32(y[2]);
        ret_i32(z[0]);
        ret_i32(z[1]);
        ret_i32(z[2]);
    ");
    assert_all(&result, &[ 1i32, 2, 3, 4, 5, 6 ]);
}

#[test]
fn array_inference1() {
    let result = run("
        let x = [ [ [ 1, 2, 3, 4 ] ] ];
        let y = x[0];
        let z = y[0];
        let u = z[1];
        ret_i8(u);
    ");
    assert_all(&result, &[ 2i8 ]);
}

#[test]
fn array_inference2() {
    let result = run("
        let x = [ [ [ 1, 2, 3, 4 ] ] ];
        let y = x[0];
        let z = y[0];
        ret_i8(z[0]);
        ret_i8(z[1]);
        ret_i8(z[2]);
    ");
    assert_all(&result, &[ 1i8, 2, 3 ]);
}

#[test]
fn struct_access() {
    let result = run("
        struct Test {
            ba: bool,
            bb: bool,
            ia: i8,
            ib: i16,
            ic: i32,
            id: i64,
            ua: u8,
            ub: u16,
            uc: u32,
            ud: u64,
            fa: f32,
            fb: f64,
        }
        fn main() {
            let x: Test = Test {
                ba: true, bb: false,
                ia: -1, ib: 2, ic: -3, id: 4,
                ua: 1, ub: 2, uc: 3, ud: 4,
                fa: 0.01, fb: 0.001
            };
            ret_i8(x.ia);
            ret_i16(x.ib);
            ret_i32(x.ic);
            ret_i64(x.id);
            ret_u8(x.ua);
            ret_u16(x.ub);
            ret_u32(x.uc);
            ret_u64(x.ud);
            ret_f32(x.fa);
            ret_f64(x.fb);
            ret_bool(x.ba);
            ret_bool(x.bb);
        }
    ");
    assert(&result[0], -1i8);
    assert(&result[1], 2i16);
    assert(&result[2], -3i32);
    assert(&result[3], 4i64);
    assert(&result[4], 1u8);
    assert(&result[5], 2u16);
    assert(&result[6], 3u32);
    assert(&result[7], 4u64);
    assert(&result[8], 0.01f32);
    assert(&result[9], 0.001f64);
    assert(&result[10], true);
    assert(&result[11], false);
}

#[test]
fn struct_nesting() {
    let result = run("
        struct MoreInner {
            one: i8,
            two: i16,
            three: i32,
        }
        struct Inner {
            a: f32,
            b: i64,
            c: MoreInner,
        }
        struct Outer {
            first: u8,
            second: u16,
            third: Inner,
        }
        fn main() {
            let x: Outer = Outer { first: 3, second: 345, third: Inner { a: 3.1415, b: -23, c: MoreInner { one: -1, two: -378, three: -123 } } };
            ret_u8(x.first);
            ret_u16(x.second);
            ret_f32(x.third.a);
            ret_i64(x.third.b);
            ret_i8(x.third.c.one);
            ret_i16(x.third.c.two);
            ret_i32(x.third.c.three);
        }
    ");
    assert(&result[0], 3u8);
    assert(&result[1], 345u16);
    assert(&result[2], 3.1415f32);
    assert(&result[3], -23i64);
    assert(&result[4], -1i8);
    assert(&result[5], -378i16);
    assert(&result[6], -123i32);
}

#[test]
fn struct_array() {
    let result = run("
        struct Inner {
            ia: u8,
            ib: [ u64; 3 ],
        }
        struct Outer {
            oa: i16,
            ob: Inner,
        }
        fn main() {
            let x: Outer = Outer {
                oa: -369,
                ob: Inner {
                    ia: 8,
                    ib: [ 3, 333, 333333 ],
                }
            };
            ret_i16(x.oa);
            ret_u8(x.ob.ia);
            ret_u64(x.ob.ib[0]);
            ret_u64(x.ob.ib[1]);
            ret_u64(x.ob.ib[2]);
        }
    ");
    assert(&result[0], -369i16);
    assert(&result[1], 8u8);
    assert(&result[2], 3u64);
    assert(&result[3], 333u64);
    assert(&result[4], 333333u64);
}

#[test]
fn string_literal() {
    let result = run("
        let hello = \"Hello World!\";
        ret_str(hello);
        ret_string(hello);
    ");
    assert_all(&result, &[ "Hello World!".to_string(), "Hello World!".to_string() ]);
}

#[test]
fn for_in_negative() {
    let result = run("
        for i in -3..3 {
            ret_i32(i);
        }
    ");
    assert_all(&result, &[ -3i32, -2, -1, 0, 1, 2 ]);
}

#[test]
fn for_in_none() {
    let result = run("
        for i in 1..1 {
            ret_u16(i);
        }
    ");
    assert!(result.len() == 0);
}

#[test]
fn for_in_dec() {
    let result = run("
        for i in 1..0 {
            ret_i8(i);
        }
    ");
    assert!(result.len() == 0);
}

#[test]
fn for_in_negative_inclusive() {
    let result = run("
        for i in -3..=3 {
            ret_i8(i);
        }
    ");
    assert_all(&result, &[ -3i8, -2, -1, 0, 1, 2, 3 ]);
}

#[test]
fn for_in_equal_inclusive() {
    let result = run("
        for i in 1..=1 {
            ret_u16(i);
        }
    ");
    assert_all(&result, &[ 1u16 ]);
}

#[test]
fn for_in_dec_inclusive() {
    let result = run("
        for i in 1..=0 {
            ret_i8(i);
        }
    ");
    assert!(result.len() == 0);
}

#[test]
fn for_in_inference_let() {
    let result = run("
        for x in 1..4 {
            let y = x * 2;
            ret_u8(y);
        }
    ");
    assert_all(&result, &[ 2u8, 4, 6 ]);
}

#[test]
fn for_in_inference_assign() {
    let result = run("
        let y;
        for x in 1..4 {
            y = x * 2;
            ret_u8(y);
        }
        ret_u8(y);
    ");
    assert_all(&result, &[ 2u8, 4, 6, 6 ]);
}

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
