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
