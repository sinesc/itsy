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
