use itsy::*;
use std::any::Any;
use std::fmt::Debug;

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

/// Implement some VM methods to write values to the VM context.
extern_rust!(TestFns, Context, {
    /// Pushes an i32 to the context.
    fn ret_i32(vm: &mut VM, value: i32) {
        vm.context().push(Box::new(value));
    }
    /// Pushes an f32 to the context.
    fn ret_f32(vm: &mut VM, value: f32) {
        vm.context().push(Box::new(value));
    }
});

/// Run a bit of itsy code and return the vm's custom field (populated by the code).
fn run(code: &str) -> Context {
    let mut vm = if code.find("fn main()").is_some() {
        vm::<TestFns, Context>(code)
    } else {
        vm::<TestFns, Context>(&format!("fn main() {{ {} }}", code))
    };
    vm.run();
    vm.into_context()
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
fn binary_op() {
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
fn floats() {
    let result = run("
        let x = 2.34;
        let y = 1.23;
        ret_f32(x + y);
    ");
    assert_all(&result, &[ 3.57f32 ]);
}