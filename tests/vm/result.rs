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
