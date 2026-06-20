mod util;
use util::*;

#[test]
fn line_comment() {
    parse("// test\n").unwrap();
}

#[test]
fn unterminated_line_comment() {
    parse("// test").unwrap();
}

#[test]
fn unterminated_empty_line_comment() {
    parse("//").unwrap();
}

#[test]
fn block_comment() {
    parse("/* test */").unwrap();
}

#[test]
fn multi_line_block_comment() {
    parse("/*
    test
    */").unwrap();
}

#[test]
fn multi_line_block_comment_followed() {
    parse("/*
    test
    */ //").unwrap();
}

#[test]
fn comments_in_expression() {
    parse("
        fn test() {
            let x = 1;
            let y = 1;
            x + /* ml */ y // sl
        }
    ").unwrap();
}

#[test]
fn comments_in_statement() {
    parse("
        fn /* test */ test() {
        }
    ").unwrap();
}

#[test]
fn comments_without_whitespace() {
    parse("
        fn/*ml*/test(){//sl
        }
    ").unwrap();
}

#[test]
fn binary_as() {
    parse("
        fn test() -> u8 {
            let a = 1;
            a as u8 + 1
        }\n").unwrap();
}
#[test]
fn match_patterns() {
    // exercises every pattern form: path (unit variant), variant-tuple with nested
    // literal/binding/wildcard sub-patterns, and a top-level wildcard arm.
    parse("
        enum TestEnum {
            SimpleA,
            SimpleB,
            Data(i32, String),
        }
        fn main() {
            let e = TestEnum::SimpleA;
            let _r = match e {
                TestEnum::SimpleA => 1,
                TestEnum::SimpleB => 2,
                TestEnum::Data(123, val) => val,
                TestEnum::Data(_, val) => val,
                _ => \"x\",
            };
        }
    ").unwrap();
}

#[test]
fn match_pattern_binding_in_scope() {
    // a binding introduced by a pattern must be visible in that arm's body
    parse("
        enum E { Data(i32) }
        fn main() {
            let _r = match E::Data(1) {
                E::Data(inner) => inner + 1,
                _ => 0,
            };
        }
    ").unwrap();
}

#[test]
fn result_type_in_signature() {
    // `Result<T>` parses in type position (return type and parameter), including a nested generic-ish
    // success type, without colliding with the `<`/`>` comparison operators.
    parse("
        fn a() -> Result<i32> { }
        fn b(r: Result<String>) { }
        fn c() -> Result<[ u8 ]> { }
    ").unwrap();
}
