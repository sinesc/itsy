use crate::util::*;

#[test]
fn string_literal() {
    let result = run("
        let hello = \"Hello World!\";
        let echo = \"Hello Echo!\";
        ret_str(hello);
        ret_string(hello);
        ret_str(echo);
        ret_string(echo);
    ");
    assert_all(&result, &[
        "Hello World!".to_string(), "Hello World!".to_string() ,
        "Hello Echo!".to_string(), "Hello Echo!".to_string() ,
    ]);
}

#[test]
fn string_compare() {
    let result = run("
        let string = \"abc\";

        let cmp_a = \"abb\";
        ret_bool(string != cmp_a);
        ret_bool(string == cmp_a);
        ret_bool(string < cmp_a);
        ret_bool(string <= cmp_a);
        ret_bool(string > cmp_a);
        ret_bool(string >= cmp_a);

        let cmp_b = \"abc\";
        ret_bool(string != cmp_b);
        ret_bool(string == cmp_b);
        ret_bool(string < cmp_b);
        ret_bool(string <= cmp_b);
        ret_bool(string > cmp_b);
        ret_bool(string >= cmp_b);

        let cmp_c = \"abd\";
        ret_bool(string != cmp_c);
        ret_bool(string == cmp_c);
        ret_bool(string < cmp_c);
        ret_bool(string <= cmp_c);
        ret_bool(string > cmp_c);
        ret_bool(string >= cmp_c);

    ");
    assert_all(&result, &[
        true, false, false, false, true, true,
        false, true, false, true, false, true,
        true, false, true, true, false, false,
    ]);
}

#[test]
fn string_len_compare() {
    let result = run("
        let string = \"abc\";

        let cmp_a = \"ab\";
        ret_bool(string != cmp_a);
        ret_bool(string == cmp_a);
        ret_bool(string < cmp_a);
        ret_bool(string <= cmp_a);
        ret_bool(string > cmp_a);
        ret_bool(string >= cmp_a);

        let cmp_b = \"abcd\";
        ret_bool(string != cmp_b);
        ret_bool(string == cmp_b);
        ret_bool(string < cmp_b);
        ret_bool(string <= cmp_b);
        ret_bool(string > cmp_b);
        ret_bool(string >= cmp_b);


    ");
    assert_all(&result, &[
        true, false, false, false, true, true,
        true, false, true, true, false, false,
    ]);
}

#[test]
fn string_concat() {
    let result = run("
        let a = \"Hello\";
        let b = \"World\";
        let result = a + \" \" + b;
        ret_bool(result == \"Hello World\");
        ret_bool(result != \"Hello World\");
        ret_bool(result == \"Hello World fake\");
        ret_bool(result != \"Hello World fake\");
    ");
    assert_all(&result, &[
        true, false,
        false, true
    ]);
}

#[test]
fn string_compound_concat() {
    let result = run("
        let mut result = \"Hello\";
        result += \"World\";
        ret_bool(result == \"HelloWorld\");
        ret_bool(result != \"Something else\");
        ret_bool(result != \"HelloWorld\");
    ");
    assert_all(&result, &[ true, true, false ]);
}

#[test]
fn string_compound_heap_concat() {
    let result = run("
        struct Test {
            s: String
        }
        fn main() {
            let mut result = Test { s: \"Hello\" };
            result.s += \"World\";
            ret_bool(result.s == \"HelloWorld\");
            ret_bool(result.s != \"Something else\");
            ret_bool(result.s != \"HelloWorld\");
        }
    ");
    assert_all(&result, &[ true, true, false ]);
}

#[test]
fn string_loop_concat() {
    let result = run("
        let test = \"Hello World\";
        for i in 0..=5 {
            test = test + (i as String);
        }
        ret_string(test);
        ret_str(test);
    ");
    assert_all(&result, &[ "Hello World012345".to_string(), "Hello World012345".to_string() ]);
}

#[test]
fn temporary_string() {
    let result = run("
        fn return_new_string(n: i32) -> String {
            n as String
        }
        fn passthrough_string(s: String) -> String {
            s
        }
        fn make_temporary_string() -> String {
            \"World\"
        }
        fn make_materialized_string() -> String {
            let result = \"A string\";
            result
        }
        fn main() {
            return_new_string(100); // discard drop
            let x = return_new_string(200);
            ret_string(x);

            passthrough_string(\"Hello\"); // discard drop
            let y = passthrough_string(\"Hello\");
            ret_string(y);

            make_temporary_string(); // discard drop
            let z = make_temporary_string();
            ret_string(z);

            make_materialized_string(); // discard drop
            let q = make_materialized_string();
            ret_string(q);
        }
    ");
    assert_all(&result, &[ "200".to_string(), "Hello".to_string(), "World".to_string(), "A string".to_string() ]);
}