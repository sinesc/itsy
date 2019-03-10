use itsy::*;
use std::{any::Any, fmt::Debug};
use std::{u8, u16, u32, u64, i8, i16, i32, i64, f32, f64};

type ContextElement = Box<Any>;
type Context = Vec<ContextElement>;

/// Compare a ContextElement with given value.
fn assert<T>(result: &ContextElement, expected: T) where T: PartialEq+Debug+'static {
    if let Some(result) = result.downcast_ref::<T>() {
        assert_eq!(result, &expected, "Result {:?} did not match expected {:?}", result, &expected);
    } else {
        panic!("Result-type did not match type of expected value {:?}", &expected);
    }
}

/// Compare Context with given values.
fn assert_all<T>(result: &Context, expected: &[ T ]) where T: PartialEq+Debug+Copy+'static {
    for index in 0..expected.len() {
        assert(&result[index], expected[index]);
    }
}

/// Implement some VM methods to write values of specific types to the VM context.
extern_rust!(TestFns, Context, {
    fn ret_u8(&mut context, value: u8) {
        context.push(Box::new(value));
    }
    fn ret_u16(&mut context, value: u16) {
        context.push(Box::new(value));
    }
    fn ret_u32(&mut context, value: u32) {
        context.push(Box::new(value));
    }
    fn ret_u64(&mut context, value: u64) {
        context.push(Box::new(value));
    }
    fn ret_i8(&mut context, value: i8) {
        context.push(Box::new(value));
    }
    fn ret_i16(&mut context, value: i16) {
        context.push(Box::new(value));
    }
    fn ret_i32(&mut context, value: i32) {
        context.push(Box::new(value));
    }
    fn ret_i64(&mut context, value: i64) {
        context.push(Box::new(value));
    }
    fn ret_f32(&mut context, value: f32) {
        context.push(Box::new(value));
    }
    fn ret_f64(&mut context, value: f64) {
        context.push(Box::new(value));
    }
    fn ret_bool(&mut context, value: bool) {
        context.push(Box::new(value));
    }
});

/// Run a bit of itsy code and return the vm's custom field (populated by the code).
fn run(code: &str) -> Context {
    let mut vm = if code.find("fn main()").is_some() {
        vm::<TestFns, Context>(code)
    } else {
        vm::<TestFns, Context>(&format!("fn main() {{ {} }}", code))
    };
    let mut context = Vec::new();
    vm.run(&mut context);
    context
}

#[test]
fn binary_op_native_stack_value() {
    let result = run("
        ret_i32(1 + 4);
        ret_i32(1 + 4 * 2);
        ret_i32((1 + 4) * 2);
        ret_i32(5 - 7);
        ret_i32(5 - 7 * 2);
    ");
    assert_all(&result, &[ 5i32, 9, 10, -2, -9 ]);
}

#[test]
fn binary_op_numerics() {
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
fn binary_op_bool() {
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
    ");
    assert_all(&result, &[
        true, false, false, false,
        true, true, true, false,
        true, false
    ]);
}

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
