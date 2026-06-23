pub use itsy::{build_str, run as itsy_run, parser::{parse_module, ParsedModule, ParseResult, LinkState}, internals::binary::sizes::STACK_ADDRESS_TYPE, internals::resolved::Type};
pub use std::{any::Any, fmt::Debug};
pub use std::{u8, u16, u32, u64, i8, i16, i32, i64, f32, f64};

pub type ContextElement = Box<dyn Any>;
pub type Context = Vec<ContextElement>;

use itsy::itsy_api;

const TEST_PRELUDE: &str = "use TestFns::{ret_u8, ret_u16, ret_u32, ret_u64, ret_i8, ret_i16, ret_i32, ret_i64, ret_f32, ret_f64, ret_bool, ret_string, ret_str};";

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

/// Compares the Context element at `index` against an expected value of any supported type. Backs the
/// [`assert_all!`] macro, which (unlike the [`assert_all`] function) allows the expected values to be of
/// differing types.
#[allow(dead_code)]
pub fn assert_one<T>(result: &Context, index: usize, expected: T) where T: PartialEq+Debug+'static {
    assert!(index < result.len(), "Result has {} element(s) but expected at least {}", result.len(), index + 1);
    if let Some(value) = result[index].downcast_ref::<T>() {
        assert!(value == &expected, "Result <{:?}> did not match expected <{:?}> at index {}", value, &expected, index);
    } else {
        panic!("Result-type did not match type of expected value <{:?}> at index {}", &expected, index);
    }
}

/// Like the [`assert_all`] function, but accepts a bracketed list of expected values that may be of
/// differing types, e.g. `assert_all!(&result, [ 255u8, -1i32, String::from("x") ]);`. Useful when a test
/// pushes results of several types into the Context. 
#[allow(unused_macros)]
macro_rules! assert_all {
    ($result:expr, [ $( $expected:expr ),* $(,)? ]) => {{
        let result = $result;
        let mut count = 0usize;
        $(
            $crate::util::assert_one(result, count, $expected);
            count += 1;
        )*
        assert!(result.len() == count, "Result length {} did not match expected length {}", result.len(), count);
    }};
}

#[allow(dead_code)]
pub fn assert_all_sa(result: &Context, expected: &[ u64 ]) {
    match STACK_ADDRESS_TYPE {
        Type::u8 => assert_all(&result, &expected.iter().map(|u| *u as u8).collect::<Vec<_>>()),
        Type::u16 => assert_all(&result, &expected.iter().map(|u| *u as u16).collect::<Vec<_>>()),
        Type::u32 => assert_all(&result, &expected.iter().map(|u| *u as u32).collect::<Vec<_>>()),
        Type::u64 => assert_all(&result, expected),
        _ => panic!("this test does not support the selected StackAddress type"),
    }
}

// Implement some VM methods to write values of specific types to the VM context.
itsy_api! {
    pub TestFns<Context> {
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
        fn ret_str(&mut context, value: str) {
            context.push(Box::new(value.to_string()));
        }
    }
}

/// Run a bit of itsy code and return the vm's custom field (populated by the code).
#[allow(dead_code)]
pub fn run(code: &str) -> Context {
    let input = if code.find("main()").is_some() {
        format!("{} {}", TEST_PRELUDE, code)
    } else {
        format!("{} fn main() {{ {} }}", TEST_PRELUDE, code)
    };
    let program = match build_str::<TestFns>(&input) {
        Ok(program) => program,
        Err(err) => {
            let loc =  err.loc(&input);
            panic!("{} in line {}, column {}.", err, loc.0, loc.1);
        }
    };
    let mut context = Vec::new();
    match itsy_run(program, &mut context) {
        Ok(_) => { },
        Err(err) => {
            panic!("{}", err);
        }
    }
    context
}

/// Build a bit of itsy code that is expected to fail and return the formatted error message.
#[allow(dead_code)]
pub fn build_err(code: &str) -> String {
    let input = if code.find("main()").is_some() {
        format!("{} {}", TEST_PRELUDE, code)
    } else {
        format!("{} fn main() {{ {} }}", TEST_PRELUDE, code)
    };
    match build_str::<TestFns>(&input) {
        Ok(_) => panic!("expected build to fail, but it succeeded"),
        Err(err) => format!("{}", err),
    }
}

#[allow(dead_code)]
pub fn parse(src: &str) -> ParseResult<ParsedModule> {
    let mut linkstate = LinkState::new();
    parse_module(&mut linkstate, src, "")
}