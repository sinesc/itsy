use crate::util::*;

#[test]
fn error_trait_impl_and_dispatch() {
    // the built-in `Error` trait can be implemented by a custom type, and an implementor is accepted
    // wherever `Error` is accepted (structural trait dispatch), with `description` dispatched dynamically.
    let result = run(stringify!(
        struct ParseError {
            at: u32,
        }
        impl Error for ParseError {
            fn description(self: Self) -> String {
                "parse error at {self.at}"
            }
        }
        fn report(e: Error) {
            ret_string(e.description());
        }
        fn main() {
            report(ParseError { at: 7 });
        }
    ));
    assert(&result[0], "parse error at 7".to_string());
}

#[test]
fn result_type_resolves_in_signature() {
    // `Result<T>` parses and resolves to its synthesized Ok/Err enum; a function using it in its
    // signature compiles even though Ok/Err construction is not available yet.
    let result = run(stringify!(
        fn unused(r: Result<i32>) -> Result<i32> {
            r
        }
        fn main() {
            ret_u8(1);
        }
    ));
    assert_all(&result, &[ 1u8 ]);
}

#[test]
fn ok_construction_and_match() {
    // `Ok(x)` infers `Result<typeof x>` from the argument; `match` discriminates the variants and binds
    // the payload.
    let result = run(stringify!(
        fn make() -> Result<i32> {
            Ok(42)
        }
        fn main() {
            match make() {
                Ok(v) => ret_i32(v),
                Err(e) => ret_string(e.description()),
            }
        }
    ));
    assert_all(&result, &[ 42i32 ]);
}

#[test]
fn err_construction_and_match() {
    // `Err(e)` takes `T` from the function's declared `Result<T>` return type; a concrete error is
    // accepted as the `Error` payload and `description` dispatches dynamically.
    let result = run(stringify!(
        struct MyError {
            code: i32,
        }
        impl Error for MyError {
            fn description(self: Self) -> String {
                "error {self.code}"
            }
        }
        fn make() -> Result<i32> {
            Err(MyError { code: 7 })
        }
        fn main() {
            match make() {
                Ok(v) => ret_i32(v),
                Err(e) => ret_string(e.description()),
            }
        }
    ));
    assert(&result[0], "error 7".to_string());
}

#[test]
fn result_let_binding_and_branch() {
    // a `Result` can be stored in a let binding and inspected later; both arms of the same result type
    // unify.
    let result = run(stringify!(
        struct E {
            kind: u8,
        }
        impl Error for E {
            fn description(self: Self) -> String { "e" }
        }
        fn check(n: i32) -> Result<i32> {
            if n < 0 {
                Err(E { kind: 1 })
            } else {
                Ok(n * 2)
            }
        }
        fn main() {
            let a = check(5);
            let b = check(0 - 1);
            let ra = match a {
                Ok(v) => v,
                Err(_) => 0 - 100,
            };
            let rb = match b {
                Ok(v) => v,
                Err(_) => 0 - 100,
            };
            ret_i32(ra);
            ret_i32(rb);
        }
    ));
    assert_all(&result, &[ 10i32, -100 ]);
}

#[test]
fn try_operator_ok_passthrough() {
    // `?` on an Ok result yields the contained value and execution continues normally.
    let result = run(stringify!(
        fn half(n: i32) -> Result<i32> {
            Ok(n / 2)
        }
        fn compute() -> Result<i32> {
            let a = half(20)?;
            let b = half(10)?;
            Ok(a + b)
        }
        fn main() {
            let r = match compute() {
                Ok(v) => v,
                Err(_) => 0 - 1,
            };
            ret_i32(r);
        }
    ));
    assert_all(&result, &[ 15i32 ]);
}

#[test]
fn try_operator_err_propagates() {
    // `?` on an Err result returns early from the enclosing function, propagating the error (as the
    // built-in Error trait) without any conversion. The later `?` and the `Ok` tail never run.
    let result = run(stringify!(
        struct DivByZero {
            marker: u8,
        }
        impl Error for DivByZero {
            fn description(self: Self) -> String { "division by zero" }
        }
        fn checked_div(a: i32, b: i32) -> Result<i32> {
            if b == 0 {
                Err(DivByZero { marker: 0 })
            } else {
                Ok(a / b)
            }
        }
        fn compute(a: i32, b: i32) -> Result<i32> {
            let q = checked_div(a, b)?;
            Ok(q + 1)
        }
        fn report(r: Result<i32>) {
            let s = match r {
                Ok(v) => "ok {v}",
                Err(e) => e.description(),
            };
            ret_string(s);
        }
        fn main() {
            report(compute(10, 2));
            report(compute(10, 0));
        }
    ));
    assert(&result[0], "ok 6".to_string());
    assert(&result[1], "division by zero".to_string());
}

#[test]
fn try_operator_in_non_result_function_rejected() {
    // `?` desugars to `return Err(..)`, which can only construct the error in a function that itself
    // returns a `Result`; using it elsewhere must fail to build rather than miscompile.
    let err = build_err(stringify!(
        fn ok() -> Result<i32> { Ok(1) }
        fn bad() -> i32 {
            let x = ok()?;
            x
        }
        fn main() {
            ret_i32(bad());
        }
    ));
    assert!(err.len() > 0);
}

#[test]
fn result_renders_legibly_in_diagnostics() {
    // a forgotten `?` leaves a `Result<T>` where `T` was expected; the diagnostic must name the type
    // legibly as `Result<i32>` rather than the anonymous-enum fallback `?`, so the mistake is obvious.
    let err = build_err(stringify!(
        fn ok() -> Result<i32> { Ok(1) }
        fn double(x: i32) -> i32 { x * 2 }
        fn main() {
            let v = ok();
            ret_i32(double(v));
        }
    ));
    assert!(err.contains("Result<i32>"), "unexpected error: {}", err);
}
