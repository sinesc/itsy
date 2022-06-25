use crate::util::*;

#[test]
fn precedence() {
    let result = run(stringify!(
        let a = true;
        let b = false;
        let c = true;
        let d = false;

        ret_bool((a || b && c) == (a || (b && c)));
        ret_bool((a && b || c && d) == ((a && b) || (c && d)));
        ret_bool((a && b && c || d) == (((a && b) && c) || d));
        ret_bool((!a && b || c) == (((!a) && b) || c));

        ret_bool((!a && b || c) == !(a && b || c));
    ));
    assert_all(&result, &[ true, true, true, true, false ]);
}

#[test]
fn expression_eval_order() {
    let result = run(stringify!(
        fn one() -> i32 { ret_i32(1); 1 }
        fn two() -> i32 { ret_i32(2); 2 }
        fn main() {
            ret_i32(one() + two());
            ret_i32(one() - two() + one());
            ret_i32(one() - (two() + one()));
            ret_i32(one() - two() * two());
            ret_i32((two() + two()) / two());
        }
    ));
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
    let result = run(stringify!(
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
    ));
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
    let result = run(stringify!(
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
    ));
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
    let result = run(stringify!(
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
    ));

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
fn unary_op() {
    let result = run(stringify!(
        let val = true;
        ret_bool(val);
        ret_bool(!val);
        ret_bool(!!val);
        ret_bool(!!!val);
    ));
    assert_all(&result, &[ true, false, true, false ]);
}

#[test]
fn assign_stack_target() {
    let result = run(stringify!(
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
    ));
    assert_all(&result, &[ 2u8, 10, 12, 22 ]);
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