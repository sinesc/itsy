use crate::util::*;

#[test]
fn array_literal() {
    let result = run(stringify!(
        let x = [ 1, 2, 3, 4 ];
        ret_i32(x[0]);
        ret_i32(x[1]);
        ret_i32(x[2]);
        ret_i32(x[3]);
    ));
    assert_all(&result, &[ 1i32, 2, 3, 4 ]);
}

#[test]
fn array_nesting() {
    let result = run(stringify!(
        let x = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
        ret_i32(x[0][0]);
        ret_i32(x[0][1]);
        ret_i32(x[0][2]);
        ret_i32(x[1][0]);
        ret_i32(x[1][1]);
        ret_i32(x[1][2]);
    ));
    assert_all(&result, &[ 1i32, 2, 3, 4, 5, 6 ]);
}

#[test]
fn array_subindex() {
    let result = run(stringify!(
        let x = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
        let y = x[0];
        let z = x[1];
        ret_i32(y[0]);
        ret_i32(y[1]);
        ret_i32(y[2]);
        ret_i32(z[0]);
        ret_i32(z[1]);
        ret_i32(z[2]);
    ));
    assert_all(&result, &[ 1i32, 2, 3, 4, 5, 6 ]);
}

#[test]
fn array_inference1() {
    let result = run(stringify!(
        let x = [ [ [ 1, 2, 3, 4 ] ] ];
        let y = x[0];
        let z = y[0];
        let u = z[1];
        ret_i8(u);
    ));
    assert_all(&result, &[ 2i8 ]);
}

#[test]
fn array_inference2() {
    let result = run(stringify!(
        let x = [ [ [ 1, 2, 3, 4 ] ] ];
        let y = x[0];
        let z = y[0];
        ret_i8(z[0]);
        ret_i8(z[1]);
        ret_i8(z[2]);
    ));
    assert_all(&result, &[ 1i8, 2, 3 ]);
}

#[test]
fn array_mixed_construction() {
    let result = run(stringify!(
        let x = "Hello";
        for w in [ x, "World" ] {
            ret_str(w);
        }
    ));
    assert_all(&result, &[ "Hello".to_string(), "World".to_string() ]);
}

#[test]
fn array_simple_dynamic_constructor() {
    let result = run(stringify!(
        let abc = "abc";
        let cba = "cba";
        let x = [ abc, cba ];
        ret_str(x[0]);
        ret_str(x[1]);
    ));
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_flattened_dynamic_constructor() {
    let result = run(stringify!(
        let ghj = "ghj";
        let abc = "abc";
        let cba = "cba";
        let jhg = "jhg";
        let x = [ [ ghj, abc ], [ cba, jhg ] ];
        ret_str(x[0][1]);
        ret_str(x[1][0]);
    ));
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_nested_dynamic_constructor() {
    let result = run(stringify!(
        let ghj_abc = [ "ghj", "abc" ];
        let cba_jhg = [ "cba", "jhg" ];
        let x = [ ghj_abc, cba_jhg ];
        ret_str(x[0][1]);
        ret_str(x[1][0]);
    ));
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_nested_var_dynamic_constructor() {
    let result = run(stringify!(
        let ghj = "ghj";
        let abc = "abc";
        let cba = "cba";
        let jhg = "jhg";
        let ghj_abc = [ ghj, abc ];
        let cba_jhg = [ cba, jhg ];
        let x = [ ghj_abc, cba_jhg ];
        ret_str(x[0][1]);
        ret_str(x[1][0]);
    ));
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_casting() {
    let result = run(stringify!(
        fn accept(x: [ u8 ], y: [ u16 ]) {
            ret_u16((x[0] as u16) + y[0]);
            ret_u16((x[1] as u16) + y[1]);
            ret_u16((x[2] as u16) + y[2]);
            ret_u16((x[3] as u16) + y[3]);
        }
        fn main() {
            let x: [ u8 ] = [ 1, 2, 3, 4 ];
            let y: [ u16 ] = [ 4, 3, 2, 1, 0 ];
            accept(x, y);
        }
    ));
    assert_all(&result, &[ 5u16, 5, 5, 5 ]);
}

#[test]
#[should_panic(expected = "Resolver error: Expected type `[ u8 ]`, got `[ u16 ]` in line 7, column 20.")]
fn array_type_fail() {
    run("
        fn accept(x: [ u8 ]) {
            ret_u8(x[3]);
        }
        fn main() {
            let x: [ u16 ] = [ 1, 2, 3, 4, 5 ];
            accept(x);
        }
    ");
}

#[test]
fn array_len() {
    let sa_type = STACK_ADDRESS_TYPE;
    let result = run(&format!("
        let array_u16: [ u16 ] = [ 1, 2, 3, 4 ];
        let array_u8: [ u8 ] = [ 4, 3, 2, 1, 0 ];
        ret_{sa_type}(array_u16.len());
        ret_{sa_type}(array_u8.len());
        ret_{sa_type}([ 5, 4, 3, 2, 1, 0 ].len());
    "));
    assert_all_sa(&result, &[ 4, 5, 6 ]);
}

#[test]
fn array_push() {
    let sa_type = STACK_ADDRESS_TYPE;
    let result = run(&format!("
        let dynamic_array = [ 1u16, 2, 3 ];
        dynamic_array.push(4u16);
        ret_{sa_type}(dynamic_array.len());
        ret_{sa_type}(dynamic_array[3] as {sa_type});
    "));
    assert_all_sa(&result, &[ 4, 4 ]);
}

#[test]
fn array_ref_elements() {
    let result = run(stringify!(
        struct Struct {
            value: u8,
        }
        fn main() {
            let a: [ Struct ] = [ ];
            a.push(Struct { value: 1 });
            a.push(Struct { value: 2 });
            a.push(Struct { value: 3 });
            a.push(Struct { value: 4 });
            a.push(Struct { value: 5 });
            a.push(Struct { value: 6 });

            let b = a.pop(); // Some(Struct { value: 6 })
            match b { Some(v) => ret_u8(v.value), None => ret_u8(0) };

            a.truncate(4);

            let c = a.pop(); // Some(Struct { value: 4 })
            match c { Some(v) => ret_u8(v.value), None => ret_u8(0) };

            let d = a.remove(0); // Some(Struct { value: 1 })
            match d { Some(v) => ret_u8(v.value), None => ret_u8(0) };

            for x in a { // 2, 3
                ret_u8(x.value);
            }

            ret_u8(a.len() as u8); // 2
        }
    ));
    assert_all(&result, &[ 6u8, 4, 1, 2, 3, 2 ]);
}

#[test]
fn array_eq() {
    // arrays compare structurally by length and element values
    let result = run(stringify!(
        fn main() {
            let a = [ 1i32, 2, 3 ];
            ret_bool(a == [ 1, 2, 3 ]);
            ret_bool(a == [ 1, 2, 4 ]);
            ret_bool(a == [ 1, 2 ]); // length mismatch
            ret_bool(a != [ 1, 2, 4 ]);
        }
    ));
    assert_all(&result, &[ true, false, false, true ]);
}

#[test]
fn array_eq_infers_untyped_operand() {
    // the untyped `[ ? ]` literal/binding must take its element type from the fully typed operand
    let result = run(stringify!(
        fn main() {
            let typed = [ 1i32, 2, 3 ];
            let untyped = [ 1, 2, 3 ];
            ret_bool(typed == untyped);
        }
    ));
    assert_all(&result, &[ true ]);
}

#[test]
fn array_eq_nested() {
    // nested arrays compare element-wise recursively
    let result = run(stringify!(
        fn main() {
            let a = [ [ 1i32, 2 ], [ 3, 4 ] ];
            ret_bool(a == [ [ 1, 2 ], [ 3, 4 ] ]);
            ret_bool(a == [ [ 1, 2 ], [ 3, 5 ] ]);
        }
    ));
    assert_all(&result, &[ true, false ]);
}

#[test]
fn for_in_array_values() {
    // the single-binding form iterates values
    let result = run(stringify!(
        let a = [ 10u64, 20u64, 30u64 ];
        for v in a {
            ret_u64(v);
        }
    ));
    assert_all(&result, &[ 10u64, 20, 30 ]);
}

#[test]
fn for_in_array_index_value() {
    // two-binding form binds the index and the value
    let result = run(stringify!(
        let a = [ 10u64, 20u64, 30u64 ];
        for k, v in a {
            ret_u64(k as u64);
            ret_u64(v);
        }
    ));
    assert_all(&result, &[ 0u64, 10, 1, 20, 2, 30 ]);
}

#[test]
fn for_in_array_index_only() {
    // `index, _` iterates indices alone
    let result = run(stringify!(
        let a = [ 10u64, 20u64, 30u64 ];
        for k, _ in a {
            ret_u64(k as u64);
        }
    ));
    assert_all(&result, &[ 0u64, 1, 2 ]);
}

#[test]
fn for_in_array_ignore_index() {
    // `_, value` iterates values, ignoring indices
    let result = run(stringify!(
        let a = [ 10u64, 20u64, 30u64 ];
        for _, v in a {
            ret_u64(v);
        }
    ));
    assert_all(&result, &[ 10u64, 20, 30 ]);
}

#[test]
fn for_in_array_index_value_reference_elements() {
    // value lookup for reference-typed elements goes through array indexing; exercises its refcounting
    let result = run(stringify!(
        let a = [ "a", "b", "c" ];
        for k, v in a {
            ret_str("{k as u64}:{v}");
        }
    ));
    assert_all(&result, &[ "0:a".to_string(), "1:b".to_string(), "2:c".to_string() ]);
}

#[test]
fn for_in_array_index_value_empty() {
    let result = run(stringify!(
        let a: [ u64 ] = [ ];
        for k, v in a {
            ret_u64(v);
        }
        ret_u64(99u64);
    ));
    assert_all(&result, &[ 99u64 ]);
}

#[test]
fn for_in_array_index_value_snapshot_len() {
    // iteration walks a clone taken at loop entry, so growing the original during iteration does not
    // extend the loop; the bound index and value reflect the snapshot
    let result = run(stringify!(
        let a = [ 1u64, 2u64, 3u64 ];
        let mut count = 0u64;
        for k, v in a {
            a.push(v + 100u64);
            count += 1u64;
        }
        ret_u64(count);
        ret_u64(a.len() as u64);
    ));
    assert_all(&result, &[ 3u64, 6 ]);
}

#[test]
fn for_in_array_break_continue() {
    let result = run(stringify!(
        let a = [ 10u64, 20u64, 30u64, 40u64 ];
        for k, v in a {
            if k == 1u64 {
                continue;
            }
            if k == 3u64 {
                break;
            }
            ret_u64(v);
        }
    ));
    assert_all(&result, &[ 10u64, 30 ]);
}

#[test]
fn for_in_index_rejects_non_collection() {
    // key/index iteration requires a map or array; a non-collection must name itself in the error
    let err = build_err(stringify!(
        for k, _ in 5 {
            ret_i32(k);
        }
    ));
    assert!(err.contains("key/index iteration") && !err.contains("for$iter"), "unexpected error: {}", err);
}
