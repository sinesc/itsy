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
