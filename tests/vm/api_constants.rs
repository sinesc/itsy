//! Tests for primitive constants declared in `itsy_api!`.
//!
//! Constants are defined once in Rust (where they become associated constants on the generated API
//! enum) and registered under the API namespace, so scripts reference them as `Api::CURSOR_LEFT`.
//! These tests cover every supported type with edge values, the use sites (expressions, function
//! arguments, `use` imports, match patterns, const expressions), the Rust-side access, and the
//! resolver's non-primitive rejection.

use itsy::{build_str, itsy_api, run as itsy_run};
use crate::util::{Context, assert_all};

itsy_api! {
    Api<Context> {
        // one constant per supported primitive type, with edge values
        const CURSOR_LEFT: u8 = 1;
        const CURSOR_RIGHT: u8 = 2;
        const KEY_A: u16 = 65;
        const SHIFTED: u32 = 1 << 4;
        const MAX_64: u64 = u64::MAX;
        const MIN_8: i8 = -128;
        const NEG_16: i16 = -300;
        const MIN_32: i32 = i32::MIN;
        const MIN_64: i64 = i64::MIN;
        const PI: f32 = 3.14159;
        const E: f64 = 2.718281828;
        const FLAG: bool = true;

        // mirrors the motivating use case: Rust and script code share the same definition
        fn is_key_pressed(&mut context, key: u8) -> bool {
            key == Api::CURSOR_LEFT
        }
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
    }
}

const PRELUDE: &str = "
    use Api::{is_key_pressed, ret_u8, ret_u16, ret_u32, ret_u64, ret_i8, ret_i16, ret_i32, ret_i64, ret_f32, ret_f64, ret_bool};
";

/// Like util's `run`, but with this module's API.
fn run(code: &str) -> Context {
    let input = format!("{} fn main() {{ {} }}", PRELUDE, code);
    let program = match build_str::<Api>(&input) {
        Ok(program) => program,
        Err(err) => {
            let loc = err.loc(&input);
            panic!("{} in line {}, column {}.", err, loc.0, loc.1);
        }
    };
    let mut context = Vec::new();
    if let Err(err) = itsy_run(program, &mut context) {
        panic!("{}", err);
    }
    context
}

/// Runs with `use Api::CURSOR_LEFT;` in effect, so the constant is available unqualified.
fn run_imported(code: &str) -> Context {
    run(format!("use Api::CURSOR_LEFT; {}", code).as_str())
}

#[test]
fn constant_values_reach_rust() {
    let result = run(stringify!(
        ret_u8(Api::CURSOR_LEFT);
        ret_u16(Api::KEY_A);
        ret_u32(Api::SHIFTED);
        ret_u64(Api::MAX_64);
        ret_i8(Api::MIN_8);
        ret_i16(Api::NEG_16);
        ret_i32(Api::MIN_32);
        ret_i64(Api::MIN_64);
        ret_f32(Api::PI);
        ret_f64(Api::E);
        ret_bool(Api::FLAG);
    ));
    assert_all!(&result, [1u8, 65u16, 16u32, u64::MAX, -128i8, -300i16, i32::MIN, i64::MIN, 3.14159f32, 2.718281828f64, true]);
}

#[test]
fn constant_passed_as_function_argument() {
    let result = run(stringify!(
        ret_bool(is_key_pressed(Api::CURSOR_LEFT));
        ret_bool(is_key_pressed(Api::CURSOR_RIGHT));
    ));
    assert_all(&result, &[true, false]);
}

#[test]
fn constant_imported_via_use() {
    let result = run_imported(stringify!(
        ret_u8(CURSOR_LEFT);
    ));
    assert_all(&result, &[1u8]);
}

#[test]
fn constant_used_in_match_pattern() {
    let result = run(stringify!(
        match Api::CURSOR_RIGHT {
            Api::CURSOR_LEFT => ret_u8(0),
            Api::CURSOR_RIGHT => ret_u8(1),
            _ => ret_u8(2),
        }
    ));
    assert_all(&result, &[1u8]);
}

#[test]
fn constant_used_in_const_expression() {
    let result = run(stringify!(
        const NEXT: u8 = Api::CURSOR_LEFT + 1;
        ret_u8(NEXT);
    ));
    assert_all(&result, &[2u8]);
}

#[test]
fn rust_side_access_matches_itsy_side() {
    assert_eq!(Api::CURSOR_LEFT, 1u8);
    assert_eq!(Api::KEY_A, 65u16);
    assert_eq!(Api::SHIFTED, 16u32);
    assert_eq!(Api::MAX_64, u64::MAX);
    assert_eq!(Api::MIN_8, -128i8);
    assert_eq!(Api::MIN_64, i64::MIN);
    assert_eq!(Api::PI, 3.14159f32);
    assert_eq!(Api::FLAG, true);
}

// A constant and a function with the same name: on the Rust side the enum variant shadows the
// associated constant (legal Rust), so the script build must reject the ambiguous name instead of
// silently picking one.
itsy_api! {
    ClashingApi<Context> {
        const KEY: u8 = 7;
        fn KEY(&mut context) {
            // unreachable from the script
        }
    }
}

#[test]
fn clashing_fn_and_constant_name_rejected() {
    let err = build_str::<ClashingApi>("fn main() { }").expect_err("expected the duplicate name to be rejected");
    assert!(err.to_string().contains("duplicate definition"), "unexpected error: {}", err);
}

// Hand-written VMFunc impl to exercise the resolver's guard against non-primitive constant types
// (the macro can't produce one, since Rust rejects non-const-constructible values at expansion).
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug)]
enum RawApi { _dummy }

impl itsy::internals::binary::VMFunc<RawApi> for RawApi {
    fn from_index(_index: itsy::internals::binary::sizes::RustFnIndex) -> Self {
        Self::_dummy
    }
    fn to_index(self: Self) -> itsy::internals::binary::sizes::RustFnIndex {
        0
    }
    fn resolve_info() -> std::collections::HashMap<&'static str, (itsy::internals::binary::sizes::RustFnIndex, Option<itsy::internals::marshal::ApiType>, Vec<itsy::internals::marshal::ApiType>)> {
        std::collections::HashMap::new()
    }
    fn api_name() -> &'static str {
        "RawApi"
    }
    fn resolve_constants() -> Vec<(&'static str, itsy::internals::marshal::ApiType, itsy::internals::marshal::ConstValue)> {
        vec![( "X", itsy::internals::marshal::ApiType::Name("String"), itsy::internals::marshal::ConstValue::Unsigned(0) )]
    }
}

#[test]
fn non_primitive_constant_type_rejected() {
    let err = build_str::<RawApi>("fn main() { }").expect_err("expected the non-primitive constant to be rejected");
    assert!(err.to_string().contains("must be a primitive type"), "unexpected error: {}", err);
}
