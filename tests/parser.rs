mod util;
use util::*;

#[test]
fn line_comment() {
    parse_module("// test\n").unwrap();
}

#[test]
fn unterminated_line_comment() {
    parse_module("// test").unwrap();
}

#[test]
fn unterminated_empty_line_comment() {
    parse_module("//").unwrap();
}

#[test]
fn block_comment() {
    parse_module("/* test */").unwrap();
}

#[test]
fn multi_line_block_comment() {
    parse_module("/*
    test
    */").unwrap();
}

#[test]
fn multi_line_block_comment_followed() {
    parse_module("/*
    test
    */ //").unwrap();
}

#[test]
fn comments_in_expression() {
    parse_module("
        fn test() {
            x + /* ml */ y // sl
        }
    ").unwrap();
}

#[test]
fn comments_in_statement() {
    parse_module("
        fn /* test */ test() {
        }
    ").unwrap();
}

#[test]
fn comments_without_whitespace() {
    parse_module("
        fn/*ml*/test(){//sl
        }
    ").unwrap();
}
