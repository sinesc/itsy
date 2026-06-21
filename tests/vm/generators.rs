use crate::util::*;

// Milestone 1: generators parse and resolve (type synthesis + validation), but codegen is not
// implemented yet. A well-formed generator therefore builds past resolution and fails only at the
// compile stage with a "Not yet implemented" error — asserting that message confirms acceptance by
// parser and resolver.

#[test]
fn value_generator_parses_and_resolves() {
    let err = build_err(stringify!(
        fn count() -> Generator<i32> {
            yield 1;
            yield 2;
        }
        fn main() { }
    ));
    assert!(err.contains("Not yet implemented"), "unexpected error: {}", err);
}

#[test]
fn key_value_generator_parses_and_resolves() {
    let err = build_err(stringify!(
        fn pairs() -> Generator<String, i32> {
            yield "a", 1;
            yield "b", 2;
        }
        fn main() { }
    ));
    assert!(err.contains("Not yet implemented"), "unexpected error: {}", err);
}

#[test]
fn valueless_return_in_generator_accepted() {
    // `return;` is allowed as an early exit and must not be rejected at resolution.
    let err = build_err(stringify!(
        fn g() -> Generator<i32> {
            if false { return; }
            yield 1;
        }
        fn main() { }
    ));
    assert!(err.contains("Not yet implemented"), "unexpected error: {}", err);
}

#[test]
fn yield_outside_generator_rejected() {
    // a function that does not return `Generator<..>` may not use `yield`.
    let err = build_err(stringify!(
        fn not_a_gen() -> i32 {
            yield 1;
            0
        }
        fn main() { }
    ));
    assert!(err.contains("generator"), "unexpected error: {}", err);
}

#[test]
fn yield_key_in_value_only_generator_rejected() {
    let err = build_err(stringify!(
        fn g() -> Generator<i32> {
            yield 1, 2;
        }
        fn main() { }
    ));
    assert!(err.contains("values only"), "unexpected error: {}", err);
}

#[test]
fn yield_without_key_in_key_value_generator_rejected() {
    let err = build_err(stringify!(
        fn g() -> Generator<i32, i32> {
            yield 1;
        }
        fn main() { }
    ));
    assert!(err.contains("key/value"), "unexpected error: {}", err);
}

#[test]
fn yield_value_type_mismatch_rejected() {
    let err = build_err(stringify!(
        fn g() -> Generator<i32> {
            yield "nope";
        }
        fn main() { }
    ));
    assert!(err.contains("i32") && err.contains("String"), "unexpected error: {}", err);
}

#[test]
fn return_with_value_in_generator_rejected() {
    let err = build_err(stringify!(
        fn g() -> Generator<i32> {
            yield 1;
            return 5;
        }
        fn main() { }
    ));
    assert!(err.contains("early exit"), "unexpected error: {}", err);
}
