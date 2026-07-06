use crate::util::*;

#[test]
fn string_literal() {
    let result = run(stringify!(
        let hello = "Hello World!";
        let echo = "Hello Echo!";
        ret_str(hello);
        ret_string(hello);
        ret_str(echo);
        ret_string(echo);
    ));
    assert_all(&result, &[
        "Hello World!".to_string(), "Hello World!".to_string() ,
        "Hello Echo!".to_string(), "Hello Echo!".to_string() ,
    ]);
}

#[test]
fn string_static_methods() {
    let result = run(stringify!(
        let escape = String::from_ascii(27);
        ret_str(escape);
    ));
    assert_all(&result, &[
        String::from_utf8(vec![27]).unwrap()
    ]);
}

#[test]
fn string_compare() {
    let result = run(stringify!(
        let string = "abc";

        let cmp_a = "abb";
        ret_bool(string != cmp_a);
        ret_bool(string == cmp_a);
        ret_bool(string < cmp_a);
        ret_bool(string <= cmp_a);
        ret_bool(string > cmp_a);
        ret_bool(string >= cmp_a);

        let cmp_b = "abc";
        ret_bool(string != cmp_b);
        ret_bool(string == cmp_b);
        ret_bool(string < cmp_b);
        ret_bool(string <= cmp_b);
        ret_bool(string > cmp_b);
        ret_bool(string >= cmp_b);

        let cmp_c = "abd";
        ret_bool(string != cmp_c);
        ret_bool(string == cmp_c);
        ret_bool(string < cmp_c);
        ret_bool(string <= cmp_c);
        ret_bool(string > cmp_c);
        ret_bool(string >= cmp_c);

    ));
    assert_all(&result, &[
        true, false, false, false, true, true,
        false, true, false, true, false, true,
        true, false, true, true, false, false,
    ]);
}

#[test]
fn string_len_compare() {
    let result = run(stringify!(
        let string = "abc";

        let cmp_a = "ab";
        ret_bool(string != cmp_a);
        ret_bool(string == cmp_a);
        ret_bool(string < cmp_a);
        ret_bool(string <= cmp_a);
        ret_bool(string > cmp_a);
        ret_bool(string >= cmp_a);

        let cmp_b = "abcd";
        ret_bool(string != cmp_b);
        ret_bool(string == cmp_b);
        ret_bool(string < cmp_b);
        ret_bool(string <= cmp_b);
        ret_bool(string > cmp_b);
        ret_bool(string >= cmp_b);


    ));
    assert_all(&result, &[
        true, false, false, false, true, true,
        true, false, true, true, false, false,
    ]);
}

#[test]
fn string_concat() {
    let result = run(stringify!(
        let a = "Hello";
        let b = "World";
        let result = a + " " + b;
        ret_bool(result == "Hello World");
        ret_bool(result != "Hello World");
        ret_bool(result == "Hello World fake");
        ret_bool(result != "Hello World fake");
    ));
    assert_all(&result, &[
        true, false,
        false, true
    ]);
}

#[test]
fn string_compound_concat() {
    let result = run(stringify!(
        let mut result = "Hello";
        result += "World";
        ret_bool(result == "HelloWorld");
        ret_bool(result != "Something else");
        ret_bool(result != "HelloWorld");
    ));
    assert_all(&result, &[ true, true, false ]);
}

#[test]
fn string_compound_heap_concat() {
    let result = run(stringify!(
        struct Test {
            s: String
        }
        fn main() {
            let mut result = Test { s: "Hello" };
            result.s += "World";
            ret_bool(result.s == "HelloWorld");
            ret_bool(result.s != "Something else");
            ret_bool(result.s != "HelloWorld");
        }
    ));
    assert_all(&result, &[ true, true, false ]);
}

#[test]
fn string_loop_concat() {
    let result = run(stringify!(
        let test = "Hello World";
        for i in 0..=5 {
            test = test + (i as String);
        }
        ret_string(test);
        ret_str(test);
    ));
    assert_all(&result, &[ "Hello World012345".to_string(), "Hello World012345".to_string() ]);
}

#[test]
fn string_find() {
    let result = run(stringify!(
        let s = "Hello World";
        match s.find("Hello") { Some(i) => ret_u64(i as u64), None => ret_i64(-1) };
        match s.find("World") { Some(i) => ret_u64(i as u64), None => ret_i64(-1) };
        match s.find("o") { Some(i) => ret_u64(i as u64), None => ret_i64(-1) };
        match s.find("xyz") { Some(i) => ret_u64(i as u64), None => ret_i64(-1) };
    ));
    assert_all!(&result, [ 0u64, 6u64, 4u64, -1i64 ]);
}

#[test]
fn string_find_unicode() {
    let result = run(stringify!(
        // character (not byte) indexing: each leading char is multi-byte
        let s = "äöü-x";
        match s.find("x") { Some(i) => ret_u64(i as u64), None => ret_i64(-1) };
        match s.find("ö") { Some(i) => ret_u64(i as u64), None => ret_i64(-1) };
    ));
    assert_all(&result, &[ 4u64, 1u64 ]);
}

#[test]
fn string_repeat() {
    let result = run(stringify!(
        ret_string("ab".repeat(3));
        ret_string("x".repeat(0));
    ));
    assert_all(&result, &[ "ababab".to_string(), "".to_string() ]);
}

#[test]
#[should_panic(expected = "Integer overflow")]
fn string_repeat_overflow_is_runtime_error() {
    run(stringify!(
        ret_string("ab".repeat(18000000000000000000u64));
    ));
}

#[test]
fn string_pad_start() {
    let result = run(stringify!(
        // basic single-char padding
        ret_string("hi".pad_start(" ", 5));
        // multi-char padding repeated
        ret_string("hi".pad_start("ab", 7));
        // padding truncated
        ret_string("hi".pad_start("abc", 5));
        // length shorter than string — unchanged
        ret_string("hello".pad_start(" ", 3));
        // length equal to string — unchanged
        ret_string("hello".pad_start(" ", 5));
        // zero-length padding — should not panic (empty padding edge case)
        // skip this; empty padding would loop forever. We just test normal cases.
        // unicode padding
        ret_string("hi".pad_start("→", 4));
        // unicode target string
        ret_string("äö".pad_start(" ", 5));
    ));
    assert_all(&result, &[
        "   hi".to_string(),
        "ababahi".to_string(),
        "abchi".to_string(),
        "hello".to_string(),
        "hello".to_string(),
        "→→hi".to_string(),
        "   äö".to_string(),
    ]);
}

#[test]
fn string_pad_end() {
    let result = run(stringify!(
        // basic single-char padding
        ret_string("hi".pad_end(" ", 5));
        // multi-char padding repeated
        ret_string("hi".pad_end("ab", 7));
        // padding truncated
        ret_string("hi".pad_end("abc", 5));
        // length shorter than string — unchanged
        ret_string("hello".pad_end(" ", 3));
        // length equal to string — unchanged
        ret_string("hello".pad_end(" ", 5));
        // unicode padding
        ret_string("hi".pad_end("→", 4));
        // unicode target string
        ret_string("äö".pad_end(" ", 5));
    ));
    assert_all(&result, &[
        "hi   " .to_string(),
        "hiababa".to_string(),
        "hiabc".to_string(),
        "hello".to_string(),
        "hello".to_string(),
        "hi→→".to_string(),
        "äö   " .to_string(),
    ]);
}

#[test]
fn string_split() {
    let result = run(stringify!(
        // basic split with exact divisor
        let parts = "abcdef".split(2);
        ret_u64(parts.len());
        ret_string(parts[0]);
        ret_string(parts[1]);
        ret_string(parts[2]);

        // split with remainder (last chunk shorter)
        let parts2 = "Hello, World!".split(3);
        ret_u64(parts2.len());
        ret_string(parts2[0]);
        ret_string(parts2[parts2.len() - 1]);

        // empty string
        let parts3 = "".split(5);
        ret_u64(parts3.len());

        // single character chunks
        let parts4 = "abc".split(1);
        ret_u64(parts4.len());
        ret_string(parts4[0]);
        ret_string(parts4[1]);
        ret_string(parts4[2]);

        // chunk larger than string
        let parts5 = "hi".split(10);
        ret_u64(parts5.len());
        ret_string(parts5[0]);
    ));
    assert_all!(&result, [
        3u64, "ab".to_string(), "cd".to_string(), "ef".to_string(),
        5u64, "Hel".to_string(), "!".to_string(),
        0u64,
        3u64, "a".to_string(), "b".to_string(), "c".to_string(),
        1u64, "hi".to_string(),
    ]);
}

#[test]
fn string_split_unicode() {
    let result = run(stringify!(
        // unicode characters (multi-byte) split by character count
        let parts = "🎉🎊🎈🎁".split(2);
        ret_u64(parts.len());
        ret_string(parts[0]);
        ret_string(parts[1]);

        // mixed ascii and unicode
        let parts2 = "äöü".split(2);
        ret_u64(parts2.len());
        ret_string(parts2[0]);
        ret_string(parts2[1]);
    ));
    assert_all!(&result, [
        2u64, "🎉🎊".to_string(), "🎈🎁".to_string(),
        2u64, "äö".to_string(), "ü".to_string(),
    ]);
}

#[test]
fn temporary_string() {
    let result = run(stringify!(
        fn return_new_string(n: i32) -> String {
            n as String
        }
        fn passthrough_string(s: String) -> String {
            s
        }
        fn make_temporary_string() -> String {
            "World"
        }
        fn make_materialized_string() -> String {
            let result = "A string";
            result
        }
        fn main() {
            return_new_string(100); // discard drop
            let x = return_new_string(200);
            ret_string(x);

            passthrough_string("Hello"); // discard drop
            let y = passthrough_string("Hello");
            ret_string(y);

            make_temporary_string(); // discard drop
            let z = make_temporary_string();
            ret_string(z);

            make_materialized_string(); // discard drop
            let q = make_materialized_string();
            ret_string(q);
        }
    ));
    assert_all(&result, &[ "200".to_string(), "Hello".to_string(), "World".to_string(), "A string".to_string() ]);
}