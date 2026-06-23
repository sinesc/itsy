use crate::util::*;

#[test]
fn option_type_resolves_in_signature() {
    // Option<T> parses and resolves to its synthesized Some/None enum
    let result = run(stringify!(
        fn unused(o: Option<i32>) -> Option<i32> { o }
        fn main() { ret_u8(1); }
    ));
    assert_all(&result, &[ 1u8 ]);
}

#[test]
fn some_construction_and_match() {
    let result = run(stringify!(
        fn make() -> Option<i32> { Some(42) }
        fn main() {
            match make() {
                Some(v) => ret_i32(v),
                None => ret_i32(0),
            }
        }
    ));
    assert_all(&result, &[ 42i32 ]);
}

#[test]
fn none_construction_and_match() {
    let result = run(stringify!(
        fn make() -> Option<i32> { None }
        fn main() {
            match make() {
                Some(v) => ret_i32(v),
                None => ret_i32(-1),
            }
        }
    ));
    assert_all(&result, &[ -1i32 ]);
}

#[test]
fn option_let_binding_and_branch() {
    let result = run(stringify!(
        fn check(n: i32) -> Option<i32> {
            if n < 0 { None } else { Some(n * 2) }
        }
        fn main() {
            let a = check(5);
            let b = check(-1);
            let ra = match a { Some(v) => v, None => -100 };
            let rb = match b { Some(v) => v, None => -100 };
            ret_i32(ra);
            ret_i32(rb);
        }
    ));
    assert_all(&result, &[ 10i32, -100 ]);
}

#[test]
fn bare_none_outside_option_context_rejected() {
    // bare `None` cannot infer T, and i32 is not an Option, so it must error with the targeted message
    let err = build_err(stringify!(
        fn bad() -> i32 { None }
        fn main() { ret_i32(bad()); }
    ));
    assert!(err.contains("`Some`/`None`"), "unexpected error: {}", err);
}

#[test]
fn option_renders_legibly_in_diagnostics() {
    // Some(1) binds to Option<i32>; passing it where i32 is expected yields a type mismatch that
    // must render the synthesized type as `Option<i32>`
    let err = build_err(stringify!(
        fn ok() -> Option<i32> { Some(1) }
        fn double(x: i32) -> i32 { x * 2 }
        fn main() {
            let v = ok();
            ret_i32(double(v));
        }
    ));
    assert!(err.contains("Option<i32>"), "unexpected error: {}", err);
}
