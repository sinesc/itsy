mod util;
use util::*;

#[test]
fn precedence() {
    let result = run("
        let a = true;
        let b = false;
        let c = true;
        let d = false;

        ret_bool((a || b && c) == (a || (b && c)));
        ret_bool((a && b || c && d) == ((a && b) || (c && d)));
        ret_bool((a && b && c || d) == (((a && b) && c) || d));
        ret_bool((!a && b || c) == (((!a) && b) || c));

        ret_bool((!a && b || c) == !(a && b || c));
    ");
    assert_all(&result, &[ true, true, true, true, false ]);
}

#[test]
fn expression_eval_order() {
    let result = run("
        fn one() -> i32 { ret_i32(1); 1 }
        fn two() -> i32 { ret_i32(2); 2 }
        fn main() {
            ret_i32(one() + two());
            ret_i32(one() - two() + one());
            ret_i32(one() - (two() + one()));
            ret_i32(one() - two() * two());
            ret_i32((two() + two()) / two());
        }
    ");
    assert_all(&result, &[
        1i32, 2,    3,
        1, 2, 1,    0,
        1, 2, 1,    -2,
        1, 2, 2,    -3,
        2, 2, 2,    2,
    ]);
}

#[test]
fn expression_short_circuit() {
    let result = run("
        fn t() -> bool { ret_bool(true); true }
        fn f() -> bool { ret_bool(false); false }
        fn main() {
            ret_bool(t() && f());
            ret_bool(t() && f() || t());
            ret_bool(f() && t() || t());
            ret_bool(t() || f() && t());
            ret_bool(t() && f() || t() && f());
            ret_bool(f() && t() || f() && t());
        }
    ");
    assert_all(&result, &[
        true, false,                false,
        true, false, true,          true,
        false,       true,          true,
        true,                       true,
        true, false, true, false,   false,
        false,       false,         false,
    ]);
}

#[test]
fn stack_value_expressions() {
    let result = run("
        ret_i32(1 + 4);
        ret_i32(1 + 4 * 2);
        ret_i32((1 + 4) * 2);
        ret_i32(5 - 7);
        ret_i32(5 - 7 * 2);
        ret_i32((5 - 7 * 2) / 3);
        ret_i32(5 - 8 * 2 / 4);
        ret_i32(5 % 9 * 2);
        ret_i32(9 % 9 * 2);
        ret_i32(5 % 7 -2 * 2);
        ret_i32(1 + -5 % 7 -2 * 2);
    ");
    assert_all(&result, &[
        1i32 + 4,
        1 + 4 * 2,
        (1 + 4) * 2,
        5 - 7,
        5 - 7 * 2,
        (5 - 7 * 2) / 3,
        5 - 8 * 2 / 4,
        5 % 9 * 2,
        9 % 9 * 2,
        5 % 7 -2 * 2,
        1 + -5 % 7 -2 * 2,
    ]);
}

#[test]
fn numerics() {
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
fn compound_i8() {
    let result = run("
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
    ");
    assert_all(&result, &[ 2i8, 12, 9, 3, 1 ]);
}

#[test]
fn compound_i64() {
    let result = run("
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
    ");
    assert_all(&result, &[ 10000000001i64, 60000000006, 60000000000, 20000000000, 0 ]);
}

#[test]
fn compound_f64() {
    let result = run("
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
    ");
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
    let result = run("
        ret_u16(23u8 as u16);
        ret_u16(255u8 as u16);
        ret_u16(243u8 as u16);
    ");
    assert_all(&result, &[ 23u16, 255, 243 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_u64_u16() {
    let result = run("
        ret_u16(23u64 as u16);
        ret_u16(280u64 as u16);
        ret_u16(243u64 as u16);
    ");
    assert_all(&result, &[ 23u64 as u16, 280u64 as u16, 243u64 as u16 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_u8_u64() {
    let result = run("
        ret_u64(23u8 as u64);
        ret_u64(255u8 as u64);
        ret_u64(7u8 as u64);
    ");
    assert_all(&result, &[ 23u64, 255, 7 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_u64_u8() {
    let result = run("
        ret_u8(23u64 as u8);
        ret_u8(280u64 as u8);
        ret_u8(243u64 as u8);
    ");
    assert_all(&result, &[ 23u8, 255, 243 ]);
}

// --- cast signed to signed ---

#[allow(overflowing_literals)]
#[test]
fn cast_i8_i16() {
    let result = run("
        ret_i16(-23i8 as i16);
        ret_i16(-125i8 as i16);
        ret_i16(127i8 as i16);
    ");
    assert_all(&result, &[ -23i16, -125, 127 ]);
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
        ret_i64(-128i8 as i64);
        ret_i64(125i8 as i64);
    ");
    assert_all(&result, &[ -23i64, -128, 125 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_i8() {
    let result = run("
        ret_i8(-23i64 as i8);
        ret_i8(-280i64 as i8);
        ret_i8(243i64 as i8);
    ");
    assert_all(&result, &[ -23i8, -128, 127 ]);
}

// --- cast signed to unsigned ---

#[allow(overflowing_literals)]
#[test]
fn cast_i8_u16() {
    let result = run("
        ret_u16(-23i8 as u16);
        ret_u16(-128i8 as u16);
        ret_u16(127i8 as u16);
    ");
    assert_all(&result, &[ 0u16, 0, 127 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_u16() {
    let result = run("
        ret_u16(-23i64 as u16);
        ret_u16(-280i64 as u16);
        ret_u16(243i64 as u16);
    ");
    assert_all(&result, &[ 0u16, 0, 243 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i8_u64() {
    let result = run("
        ret_u64(-23i8 as u64);
        ret_u64(-128i8 as u64);
        ret_u64(127i8 as u64);
    ");
    assert_all(&result, &[ 0u64, 0, 127 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_u8() {
    let result = run("
        ret_u8(-23i64 as u8);
        ret_u8(-280i64 as u8);
        ret_u8(243i64 as u8);
    ");
    assert_all(&result, &[ 0u8, 0, 243 ]);
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
    assert_all(&result, &[ 3i16, -3, 32767, 32767, -32768 ]);
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
    assert_all(&result, &[ 3i8, -3, 127, 127, -128 ]);
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
    assert_all(&result, &[ 3u16, 0, 65535, 65535, 0 ]);
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

#[test]
fn op_order_lt() {
    let result = run("
        let y = 1u8;
        ret_bool(y*3 < ++y*3);
        ret_bool(y == 2);
    ");
    assert_all(&result, &[ true, true ]);
}

#[test]
fn branching() {
    let result = run("
        let mut x = 1;
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
fn branching_ref_cleanup() {
    let result = run("
        if true {
            let a = \"Hello\";
            ret_string(a);
        } else {
            let b = \"World\";
            ret_string(b);
        }
    ");
    assert_all(&result, &[ "Hello".to_string() ]);
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
fn array_mixed_construction() {
    let result = run("
        let x = \"Hello\";
        for w in [ x, \"World\" ] {
            ret_str(w);
        }
    ");
    assert_all(&result, &[ "Hello".to_string(), "World".to_string() ]);
}

#[test]
fn array_simple_dynamic_constructor() {
    let result = run("
        let abc = \"abc\";
        let cba = \"cba\";
        let x = [ abc, cba ];
        ret_str(x[0]);
        ret_str(x[1]);
    ");
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_flattened_dynamic_constructor() {
    let result = run("
        let ghj = \"ghj\";
        let abc = \"abc\";
        let cba = \"cba\";
        let jhg = \"jhg\";
        let x = [ [ ghj, abc ], [ cba, jhg ] ];
        ret_str(x[0][1]);
        ret_str(x[1][0]);
    ");
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_nested_dynamic_constructor() {
    let result = run("
        let ghj_abc = [ \"ghj\", \"abc\" ];
        let cba_jhg = [ \"cba\", \"jhg\" ];
        let x = [ ghj_abc, cba_jhg ];
        ret_str(x[0][1]);
        ret_str(x[1][0]);
    ");
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_nested_var_dynamic_constructor() {
    let result = run("
        let ghj = \"ghj\";
        let abc = \"abc\";
        let cba = \"cba\";
        let jhg = \"jhg\";
        let ghj_abc = [ ghj, abc ];
        let cba_jhg = [ cba, jhg ];
        let x = [ ghj_abc, cba_jhg ];
        ret_str(x[0][1]);
        ret_str(x[1][0]);
    ");
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_casting() {
    let result = run("
        fn accept(x: [ u8 ], y: [ u16 ]) {
            ret_u16((x[0] as u16) + y[0]);
            ret_u16((x[1] as u16) + y[1]);
            ret_u16((x[2] as u16) + y[2]);
            ret_u16((x[3] as u16) + y[3]);
        }
        fn main() {
            let x: [ u8 ] = [ 1, 2, 3, 4 ];
            let y: [ u16 ] = [ 4, 3, 2, 1, 0 ];
            accept(x, y);
        }
    ");
    assert_all(&result, &[ 5u16, 5, 5, 5 ]);
}
/*
#[test]
#[should_panic(expected = "Resolver error: Expected type <[ _; 4 ]>, got <[ _; 5 ]> in line 7, column 20.")]
fn array_type_fail() {
    run("
        fn accept(x: [ u8 ]) {
            ret_u8(x[3]);
        }
        fn main() {
            let x: [ u16 ] = [ 1, 2, 3, 4, 5 ];
            accept(x);
        }
    ");
}
*/
#[test]
fn array_len() {
    let result = run("
        let array_u16: [ u16 ] = [ 1, 2, 3, 4 ];
        let array_u8: [ u8 ] = [ 4, 3, 2, 1, 0 ];
        ret_u64(array_u16.len());
        ret_u64(array_u8.len());
        ret_u64([ 5, 4, 3, 2, 1, 0 ].len());
    ");
    assert_all(&result, &[ 4u64, 5, 6 ]);
}

#[test]
fn array_infer_dynamic() {
    let result = run("
        let dynamic_array = [ 1u16, 2, 3 ];
        dynamic_array.push(4u16);
        ret_u64(dynamic_array.len());
        ret_u64(dynamic_array[3] as u64);
    ");
    assert_all(&result, &[ 4u64, 4 ]);
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
            ib: [ u64 ],
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
fn member_assign_primitive() {
    let result = run("
        struct Inner {
            ia: u8,
            ib: [ u64 ],
        }
        struct Outer {
            oa: i16,
            ob: Inner,
        }
        fn main() {
            let mut x: Outer = Outer {
                oa: 0,
                ob: Inner {
                    ia: 0,
                    ib: [ 0, 0, 0 ],
                }
            };
            x.oa = -369;
            x.ob.ia = 8;
            ret_i16(x.oa);
            ret_u8(x.ob.ia);
        }
    ");
    assert(&result[0], -369i16);
    assert(&result[1], 8u8);
}

#[test]
fn index_assign_primitive() {
    let result = run("
        struct Inner {
            ia: u8,
            ib: [ u64 ],
        }
        struct Outer {
            oa: [ u8 ],
            ob: Inner,
        }
        fn main() {
            let x: Outer = Outer {
                oa: [ 0, 0, 0 ],
                ob: Inner {
                    ia: 0,
                    ib: [ 0, 0, 0 ],
                }
            };
            x.oa[0] = 13;
            x.oa[1] = 133;
            x.oa[2] = 255;
            x.ob.ib[0] = 12345678901;
            x.ob.ib[1] = 12345678902;
            x.ob.ib[2] = 12345678903;
            ret_u8(x.oa[0]);
            ret_u8(x.oa[1]);
            ret_u8(x.oa[2]);
            ret_u64(x.ob.ib[0]);
            ret_u64(x.ob.ib[1]);
            ret_u64(x.ob.ib[2]);
        }
    ");
    assert(&result[0], 13u8);
    assert(&result[1], 133u8);
    assert(&result[2], 255u8);
    assert(&result[3], 12345678901u64);
    assert(&result[4], 12345678902u64);
    assert(&result[5], 12345678903u64);
}

#[test]
fn heap_compound_assign() {
    let result = run(&format!("
        fn left() -> {:?} {{
            ret_u8(9);
            0
        }}
        fn right() -> u8 {{
            ret_u8(6);
            1
        }}
        fn main() {{
            let test = [ 0u8 ];
            ret_u8(test[0]);
            test[left()] += right();
            ret_u8(test[0]);
        }}
    ", itsy::STACK_ADDRESS_TYPE));
    assert_all(&result, &[ 0u8, 9, 6, 1 ]);
}

#[test]
fn heap_compound_assign64() {
    let result = run(&format!("
        fn left() -> {:?} {{
            ret_u64(9);
            0
        }}
        fn right() -> u64 {{
            ret_u64(6);
            1
        }}
        fn main() {{
            let test = [ 0u64 ];
            ret_u64(test[0]);
            test[left()] += right();
            ret_u64(test[0]);
            test[left()] += right();
            ret_u64(test[0]);
            test[left()] -= right();
            ret_u64(test[0]);
        }}
    ", itsy::STACK_ADDRESS_TYPE));
    assert_all(&result, &[ 0u64, 9, 6, 1, 9, 6, 2, 9, 6, 1 ]);
}

#[test]
fn assign_heap_index_target() {
    let result = run("
        let array = [ [ 0u8, 1 ] ];
        array[0] = [ 2u8, 3 ];
        ret_u8(array[0][1]);
        array[0][1] = 4;
        ret_u8(array[0][1]);
        array[0][1] += 4;
        ret_u8(array[0][1]);
        array[0][1] -= 1;
        ret_u8(array[0][1]);
    ");
    assert_all(&result, &[ 3u8, 4, 8, 7 ]);
}

#[test]
fn assign_heap_var_target() {
    let result = run("
        let mut values = [ 5u8, 6 ];
        let morevalues = [ [ 15u8, 16 ], [ 12, 13 ] ];
        values = [ 7u8, 8 ];
        ret_u8(values[1]);
        values = morevalues[1];
        ret_u8(values[0]);
        // compound ops not defined for arrays
    ");
    assert_all(&result, &[ 8u8, 12 ]);
}

#[test]
fn assign_stack_target() {
    let result = run("
        let mut value = 1u8;
        let values = [ 0u8, 10 ];
        value = 2;
        ret_u8(value);
        value = values[1];
        ret_u8(value);
        value += 2;
        ret_u8(value);
        value += values[1];
        ret_u8(value);
    ");
    assert_all(&result, &[ 2u8, 10, 12, 22 ]);
}

#[test]
fn string_literal() {
    let result = run("
        let hello = \"Hello World!\";
        let echo = \"Hello Echo!\";
        ret_str(hello);
        ret_string(hello);
        ret_str(echo);
        ret_string(echo);
    ");
    assert_all(&result, &[
        "Hello World!".to_string(), "Hello World!".to_string() ,
        "Hello Echo!".to_string(), "Hello Echo!".to_string() ,
    ]);
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
        let mut y;
        for x in 1..4 {
            y = x * 2;
            ret_u8(y);
        }
        ret_u8(y);
    ");
    assert_all(&result, &[ 2u8, 4, 6, 6 ]);
}

#[test]
fn for_in_array() {
    let result = run("
        for x in [ 7u8, 11, 13, 17 ] {
            ret_u8(x);
        }
    ");
    assert_all(&result, &[ 7u8, 11, 13, 17 ]);
}

#[test]
fn for_in_array_var() {
    let result = run("
        let array = [ 7u8, 11, 13, 17 ];
        for x in array {
            ret_u8(x);
        }
    ");
    assert_all(&result, &[ 7u8, 11, 13, 17 ]);
}

#[test]
fn for_in_array_struct() {
    let result = run("
        struct Test {
            inner: u8,
        }
        fn main() {
            let array = [ Test { inner: 7 }, Test { inner: 17 }];
            for x in array {
                ret_u8(x.inner);
            }
        }
    ");
    assert_all(&result, &[ 7u8, 17 ]);
}

#[test]
fn for_in_array_string() {
    let result = run("
        let array = [ \"Hello\", \"World\" ];
        for x in array {
            ret_string(x);
        }
    ");
    assert_all(&result, &[ "Hello".to_string(), "World".to_string() ]);
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

#[test]
fn unary_op() {
    let result = run("
        let val = true;
        ret_bool(val);
        ret_bool(!val);
        ret_bool(!!val);
        ret_bool(!!!val);
    ");
    assert_all(&result, &[ true, false, true, false ]);
}

#[test]
fn postfix_suffix() {
    let result = run("
        let val = 0i8;
        ret_i8(val++);
        ret_i8(val--);
        ret_i8(++val);
        ret_i8(--val);
    ");
    assert_all(&result, &[ 0i8, 1, 1, 0 ]);
}

#[test]
fn heap_postfix_suffix() {
    let result = run("
        struct Test {
            field: u8,
        }
        fn main() {
            let x = [ 0u8 ];
            let y = x[0]++;     // used result
            let z = ++x[0];
            x[0]++;             // unused result
            ++x[0];
            ret_u8(x[0]);
            ret_u8(y);
            ret_u8(z);

            let a = Test { field: 0 };
            let b = a.field++;  // used result
            let c = ++a.field;
            a.field++;          // unused result
            ++a.field;
            ret_u8(a.field);
            ret_u8(b);
            ret_u8(c);
        }
    ");
    assert_all(&result, &[
        4u8, 0, 2,
        4u8, 0, 2,
    ]);
}

#[test]
fn explicit_return() {
    // todo: add bytecode test, check dead code was removed
    let result = run("
        fn test(x: i32) -> i32 {
            ret_i32(x);
            if x == 1 {
                ret_i32(1);
                return 1;
                ret_i32(-1);
            } else if x == 2 {
                ret_i32(2);
                // return from sub-block
                {
                    return 2;
                }
                ret_i32(-2);
            } else {
                if x == 3 {
                    ret_i32(3);
                    return 3;
                    ret_i32(-3);
                } else if x == 4 {
                    ret_i32(4);
                    return 4;
                    ret_i32(-4);
                }
                // else fall through
                ret_i32(5);
                return 5;
                ret_i32(-5);
            }
            ret_i32(-6);
        }
        fn main() {
            ret_i32(test(1));
            ret_i32(test(2));
            ret_i32(test(3));
            ret_i32(test(4));
            ret_i32(test(5));
            ret_i32(test(6));
        }
    ");
    assert_all(&result, &[
        1i32,   1, 1,
        2,      2, 2,
        3,      3, 3,
        4,      4, 4,
        5,      5, 5,
        6,      5, 5,
    ]);
}

#[test]
fn block_result() {
    let result = run("
        fn test(x: i32) -> i32 {
            ret_i32(x);
            if x == 1 {
                ret_i32(1);
                1
            } else if x == 2 {
                ret_i32(2);
                // result from sub-block
                {
                    2
                }
            } else {
                if x == 3 {
                    ret_i32(3);
                    return 3;
                } else if x == 4 {
                    ret_i32(4);
                    return 4;
                }
                // if no explicit return happened above
                ret_i32(5);
                5
            }
        }
        fn main() {
            ret_i32(test(1));
            ret_i32(test(2));
            ret_i32(test(3));
            ret_i32(test(4));
            ret_i32(test(5));
            ret_i32(test(6));
        }
    ");
    assert_all(&result, &[
        1i32,   1, 1,
        2,      2, 2,
        3,      3, 3,
        4,      4, 4,
        5,      5, 5,
        6,      5, 5,
    ]);
}

#[test]
fn unused_result() {
    let result = run("
        fn test1(x: i32) -> i64 {
            ret_i32(x);
            return x as i64;
        }
        fn test2(x: i32) -> i64 {
            ret_i32(x);
            x as i64
        }
        fn test3(x: i32) -> i64 {
            ret_i32(x);
            { x as i64 }
        }
        fn main() {
            for i in 0..7 {
                test1(i);
                test2(i);
                test3(i);
            }
        }
    ");
    assert_all(&result, &[
        0i32, 0, 0,
        1, 1, 1,
        2, 2, 2,
        3, 3, 3,
        4, 4, 4,
        5, 5, 5,
        6, 6, 6,
    ]);
}

#[test]
fn heap_return() {
    let result = run("
        struct Inner {
            c: u32,
        }
        struct Outer {
            a: u32,
            b: Inner,
        }
        fn test() -> Inner {
            let x = Outer {
                a: 11,
                b: Inner {
                    c: 13,
                }
            };
            return x.b;
        }
        fn main() {
            let mut y = test();
            ret_u32(y.c);
            y.c += 1;
            ret_u32(y.c);
        }
    ");
    assert_all(&result, &[
        13u32, 14
    ]);
}

#[test]
fn heap_compare() {
    let result = run("
        let array = [ 7u8, 7 ];

        ret_bool(array != [ 7u8, 7 ]);
        ret_bool(array == [ 7u8, 7 ]);

        ret_bool(array != [ 8u8, 7 ]);
        ret_bool(array == [ 8u8, 7 ]);

        ret_bool(array != [ 7u8, 8 ]);
        ret_bool(array == [ 7u8, 8 ]);
    ");
    assert_all(&result, &[
        false, true,
        true, false,
        true, false,
    ]);
}

#[test]
fn string_compare() {
    let result = run("
        let string = \"abc\";

        let cmp_a = \"abb\";
        ret_bool(string != cmp_a);
        ret_bool(string == cmp_a);
        ret_bool(string < cmp_a);
        ret_bool(string <= cmp_a);
        ret_bool(string > cmp_a);
        ret_bool(string >= cmp_a);

        let cmp_b = \"abc\";
        ret_bool(string != cmp_b);
        ret_bool(string == cmp_b);
        ret_bool(string < cmp_b);
        ret_bool(string <= cmp_b);
        ret_bool(string > cmp_b);
        ret_bool(string >= cmp_b);

        let cmp_c = \"abd\";
        ret_bool(string != cmp_c);
        ret_bool(string == cmp_c);
        ret_bool(string < cmp_c);
        ret_bool(string <= cmp_c);
        ret_bool(string > cmp_c);
        ret_bool(string >= cmp_c);

    ");
    assert_all(&result, &[
        true, false, false, false, true, true,
        false, true, false, true, false, true,
        true, false, true, true, false, false,
    ]);
}

#[test]
fn string_len_compare() {
    let result = run("
        let string = \"abc\";

        let cmp_a = \"ab\";
        ret_bool(string != cmp_a);
        ret_bool(string == cmp_a);
        ret_bool(string < cmp_a);
        ret_bool(string <= cmp_a);
        ret_bool(string > cmp_a);
        ret_bool(string >= cmp_a);

        let cmp_b = \"abcd\";
        ret_bool(string != cmp_b);
        ret_bool(string == cmp_b);
        ret_bool(string < cmp_b);
        ret_bool(string <= cmp_b);
        ret_bool(string > cmp_b);
        ret_bool(string >= cmp_b);


    ");
    assert_all(&result, &[
        true, false, false, false, true, true,
        true, false, true, true, false, false,
    ]);
}

#[test]
fn string_concat() {
    let result = run("
        let a = \"Hello\";
        let b = \"World\";
        let result = a + \" \" + b;
        ret_bool(result == \"Hello World\");
        ret_bool(result != \"Hello World\");
        ret_bool(result == \"Hello World fake\");
        ret_bool(result != \"Hello World fake\");
    ");
    assert_all(&result, &[
        true, false,
        false, true
    ]);
}

#[test]
fn string_compound_concat() {
    let result = run("
        let mut result = \"Hello\";
        result += \"World\";
        ret_bool(result == \"HelloWorld\");
        ret_bool(result != \"Something else\");
        ret_bool(result != \"HelloWorld\");
    ");
    assert_all(&result, &[ true, true, false ]);
}

#[test]
fn string_compound_heap_concat() {
    let result = run("
        struct Test {
            s: String
        }
        fn main() {
            let mut result = Test { s: \"Hello\" };
            result.s += \"World\";
            ret_bool(result.s == \"HelloWorld\");
            ret_bool(result.s != \"Something else\");
            ret_bool(result.s != \"HelloWorld\");
        }
    ");
    assert_all(&result, &[ true, true, false ]);
}

#[test]
fn string_loop_concat() {
    let result = run("
        let test = \"Hello World\";
        for i in 0..=5 {
            test = test + (i as String);
        }
        ret_string(test);
        ret_str(test);
    ");
    assert_all(&result, &[ "Hello World012345".to_string(), "Hello World012345".to_string() ]);
}

#[test]
fn dead_code_result() {
    let result = run("
        fn result() -> u32 {
            if true {
                return 1;
            } else if true {
                return 2;
            } else {
                return 3;
            }
            let dead_code: f64 = 3.14;
            dead_code
        }
        fn return_() -> u32 {
            if false {
                return 1;
            } else if true {
                return 2;
            } else {
                return 3;
            }
            let dead_code: f64 = 3.14;
            return dead_code;
        }
        fn main() {
            ret_u32(result());
            ret_u32(return_());
        }
    ");
    assert_all(&result, &[
        1u32, 2u32
    ]);
}

#[test]
fn temporary_heap_objects() {
    let result = run("
        fn main() {
            ret_string(\"Temporary\");
            ret_string(\"Heap\");
            ret_string(\"Objects\");
        }
    ");
    assert_all(&result, &[ "Temporary".to_string(), "Heap".to_string(), "Objects".to_string() ]);
}

#[test]
fn dynamic_constructor() {
    let result = run("
        struct Test {
            a: u32,
            b: String,
        }
        fn main() {
            let x = Test { a: 1100101, b: \"Hello\" + \"World\" };
            ret_string(x.b);
            ret_u32(x.a);
        }
    ");
    assert(&result[0], "HelloWorld".to_string());
    assert(&result[1], 1100101u32);
}