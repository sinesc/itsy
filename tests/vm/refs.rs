use crate::util::*;

#[test]
fn heap_compound_assign() {
    let sa_type = STACK_ADDRESS_TYPE;
    let result = run(&format!("
        fn left() -> {sa_type} {{
            ret_u8(9);
            0
        }}
        fn right() -> u8 {{
            ret_u8(6);
            1
        }}
        fn main() {{
            let test = [ 0u8 ];
            ret_u8(test[0]);
            test[left()] += right();
            ret_u8(test[0]);
        }}
    "));
    assert_all(&result, &[ 0u8, 9, 6, 1 ]);
}

#[test]
fn heap_compound_assign64() {
    let sa_type = STACK_ADDRESS_TYPE;
    let result = run(&format!("
        fn left() -> {sa_type} {{
            ret_u64(9);
            0
        }}
        fn right() -> u64 {{
            ret_u64(6);
            1
        }}
        fn main() {{
            let test = [ 0u64 ];
            ret_u64(test[0]);
            test[left()] += right();
            ret_u64(test[0]);
            test[left()] += right();
            ret_u64(test[0]);
            test[left()] -= right();
            ret_u64(test[0]);
        }}
    "));
    assert_all(&result, &[ 0u64, 9, 6, 1, 9, 6, 2, 9, 6, 1 ]);
}

#[test]
fn assign_heap_index_target() {
    let result = run(stringify!(
        let array = [ [ 0u8, 1 ] ];
        array[0] = [ 2u8, 3 ];
        ret_u8(array[0][1]);
        array[0][1] = 4;
        ret_u8(array[0][1]);
        array[0][1] += 4;
        ret_u8(array[0][1]);
        array[0][1] -= 1;
        ret_u8(array[0][1]);
    ));
    assert_all(&result, &[ 3u8, 4, 8, 7 ]);
}

#[test]
fn assign_heap_var_target() {
    let result = run(stringify!(
        let mut values = [ 5u8, 6 ];
        let morevalues = [ [ 15u8, 16 ], [ 12, 13 ] ];
        values = [ 7u8, 8 ];
        ret_u8(values[1]);
        values = morevalues[1];
        ret_u8(values[0]);
        // compound ops not defined for arrays
    ));
    assert_all(&result, &[ 8u8, 12 ]);
}

#[test]
fn heap_return() {
    let result = run(stringify!(
        struct Inner {
            c: u32,
        }
        struct Outer {
            a: u32,
            b: Inner,
        }
        fn test() -> Inner {
            let x = Outer {
                a: 11,
                b: Inner {
                    c: 13,
                }
            };
            return x.b;
        }
        fn main() {
            let mut y = test();
            ret_u32(y.c);
            y.c += 1;
            ret_u32(y.c);
        }
    ));
    assert_all(&result, &[
        13u32, 14
    ]);
}

#[test]
fn heap_result() {
    let result = run(stringify!(
        struct Inner {
            c: u32,
        }
        struct Outer {
            a: u32,
            b: Inner,
        }
        fn main() {
            let mut y = {
                let x = Outer {
                    a: 11,
                    b: Inner {
                        c: 13,
                    }
                };
                x.b
            };
            ret_u32(y.c);
            y.c += 1;
            ret_u32(y.c);
        }
    ));
    assert_all(&result, &[
        13u32, 14
    ]);
}

#[test]
#[ignore]
fn heap_compare() {
    let result = run(stringify!(
        let array = [ 7u8, 7 ];

        ret_bool(array != [ 7u8, 7 ]);
        ret_bool(array == [ 7u8, 7 ]);

        ret_bool(array != [ 8u8, 7 ]);
        ret_bool(array == [ 8u8, 7 ]);

        ret_bool(array != [ 7u8, 8 ]);
        ret_bool(array == [ 7u8, 8 ]);
    ));
    assert_all(&result, &[
        false, true,
        true, false,
        true, false,
    ]);
}

#[test]
fn temporary_heap_objects() {
    let result = run(stringify!(
        fn main() {
            ret_string("Temporary");
            ret_string("Heap");
            ret_string("Objects");
        }
    ));
    assert_all(&result, &[ "Temporary".to_string(), "Heap".to_string(), "Objects".to_string() ]);
}

#[test]
fn tempory_access() {
    let result = run(stringify!(
        struct Test {
            val: u8,
        }
        fn test(i: u8) -> Test {
            Test { val: i }
        }
        fn main() {
            test(3); // test discard drop
            ret_u8(test(1).val);
            ret_u8(test(2).val);
            ret_u8(test(3).val);
        }
    ));
    assert_all(&result, &[ 1u8, 2, 3 ]);
}

#[test]
fn nested_temporaries() {
    let result = run(stringify!(
        struct Outer {
            inner: Inner,
        }
        struct Inner {
            value: u16,
        }
        fn main() {
            let a = [ Outer { inner: Inner { value: 3 }} ][0].inner.value;
            let b = Outer { inner: Inner { value: 4 }}.inner.value; // temporary intermediate results
            ret_u16(a);
            ret_u16(b);

            let c = [ Outer { inner: Inner { value: 5 }} ];
            let d = Outer { inner: Inner { value: 6 }}; // non-temporary
            c[0] = d;
            ret_u16(c[0].inner.value);

            let e = [ Outer { inner: Inner { value: 7 }} ];
            e[0] = Outer { inner: Inner { value: 8 }};
            ret_u16(e[0].inner.value);

            let f = [ Outer { inner: Inner { value: 9 }} ][0];
            f = Outer { inner: Inner { value: 10 } };
            ret_u16(f.inner.value);
        }
    ));
    assert_all(&result, &[ 3u16, 4, 6, 8, 10 ]);
}


#[test]
fn string_copy_vs_reference() {
    let result = run(stringify!(
        let s = "Hello"; // original
        let t = [ s ];   // array with copy of s
        let u = t;       // array referencing array with copy of s
        ret_string(t[0]);
        ret_string(s);

        t[0] += " brave";
        s += " World";
        ret_string(t[0]);
        ret_string(s);

        t[0] += " new world";
        ret_string(t[0]);
        ret_string(u[0]);
    ));
    assert_all(&result, &[ "Hello", "Hello", "Hello brave", "Hello World", "Hello brave new world", "Hello brave new world" ].map(|s| s.to_string()));
}