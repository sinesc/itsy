pub use itsy::{vm, frontend::parse};
pub use std::{any::Any, fmt::Debug};
pub use std::{u8, u16, u32, u64, i8, i16, i32, i64, f32, f64};

pub type ContextElement = Box<dyn Any>;
pub type Context = Vec<ContextElement>;

use itsy::extern_rust;

/// Compare a ContextElement with given value.
#[allow(dead_code)]
pub fn assert<T>(result: &ContextElement, expected: T) where T: PartialEq+Debug+'static {
    if let Some(result) = result.downcast_ref::<T>() {
        assert!(result == &expected, "Result <{:?}> did not match expected <{:?}>", result, &expected);
    } else {
        panic!("Result-type did not match type of expected value <{:?}>", &expected);
    }
}

/// Compare Context with given values.
#[allow(dead_code)]
pub fn assert_all<T>(result: &Context, expected: &[ T ]) where T: PartialEq+Debug+'static {
    for index in 0..expected.len().min(result.len()) {
        if let Some(result) = result[index].downcast_ref::<T>() {
            assert!(result == &expected[index], "Result <{:?}> did not match expected <{:?}> at index {}", result, &expected[index], index);
        } else {
            panic!("Result-type did not match type of expected value <{:?}>", &expected);
        }
    }
    assert!(result.len() == expected.len(), "Result length {} did not match expected length {}", result.len(), expected.len());
}

// Implement some VM methods to write values of specific types to the VM context.
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
    fn ret_string(&mut context, value: String) {
        context.push(Box::new(value));
    }
    fn ret_str(&mut context, value: &str) {
        context.push(Box::new(value.to_string()));
    }
});

/// Run a bit of itsy code and return the vm's custom field (populated by the code).
#[allow(dead_code)]
pub fn run(code: &str) -> Context {
    let mut vm = if code.find("fn main()").is_some() {
        vm::<TestFns, Context>(code)
    } else {
        vm::<TestFns, Context>(&format!("fn main() {{ {} }}", code))
    };
    let mut context = Vec::new();
    vm.run(&mut context);
    assert!(vm.heap.len() == 1); // const pool remains
    context
}
