use crate::util::*;

#[test]
fn array_literal() {
    let result = run("
        let x = [ 1, 2, 3, 4 ];
        ret_i32(x[0]);
        ret_i32(x[1]);
        ret_i32(x[2]);
        ret_i32(x[3]);
    ");
    assert_all(&result, &[ 1i32, 2, 3, 4 ]);
}

#[test]
fn array_nesting() {
    let result = run("
        let x = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
        ret_i32(x[0][0]);
        ret_i32(x[0][1]);
        ret_i32(x[0][2]);
        ret_i32(x[1][0]);
        ret_i32(x[1][1]);
        ret_i32(x[1][2]);
    ");
    assert_all(&result, &[ 1i32, 2, 3, 4, 5, 6 ]);
}

#[test]
fn array_subindex() {
    let result = run("
        let x = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
        let y = x[0];
        let z = x[1];
        ret_i32(y[0]);
        ret_i32(y[1]);
        ret_i32(y[2]);
        ret_i32(z[0]);
        ret_i32(z[1]);
        ret_i32(z[2]);
    ");
    assert_all(&result, &[ 1i32, 2, 3, 4, 5, 6 ]);
}

#[test]
fn array_inference1() {
    let result = run("
        let x = [ [ [ 1, 2, 3, 4 ] ] ];
        let y = x[0];
        let z = y[0];
        let u = z[1];
        ret_i8(u);
    ");
    assert_all(&result, &[ 2i8 ]);
}

#[test]
fn array_inference2() {
    let result = run("
        let x = [ [ [ 1, 2, 3, 4 ] ] ];
        let y = x[0];
        let z = y[0];
        ret_i8(z[0]);
        ret_i8(z[1]);
        ret_i8(z[2]);
    ");
    assert_all(&result, &[ 1i8, 2, 3 ]);
}

#[test]
fn array_mixed_construction() {
    let result = run("
        let x = \"Hello\";
        for w in [ x, \"World\" ] {
            ret_str(w);
        }
    ");
    assert_all(&result, &[ "Hello".to_string(), "World".to_string() ]);
}

#[test]
fn array_simple_dynamic_constructor() {
    let result = run("
        let abc = \"abc\";
        let cba = \"cba\";
        let x = [ abc, cba ];
        ret_str(x[0]);
        ret_str(x[1]);
    ");
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_flattened_dynamic_constructor() {
    let result = run("
        let ghj = \"ghj\";
        let abc = \"abc\";
        let cba = \"cba\";
        let jhg = \"jhg\";
        let x = [ [ ghj, abc ], [ cba, jhg ] ];
        ret_str(x[0][1]);
        ret_str(x[1][0]);
    ");
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_nested_dynamic_constructor() {
    let result = run("
        let ghj_abc = [ \"ghj\", \"abc\" ];
        let cba_jhg = [ \"cba\", \"jhg\" ];
        let x = [ ghj_abc, cba_jhg ];
        ret_str(x[0][1]);
        ret_str(x[1][0]);
    ");
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_nested_var_dynamic_constructor() {
    let result = run("
        let ghj = \"ghj\";
        let abc = \"abc\";
        let cba = \"cba\";
        let jhg = \"jhg\";
        let ghj_abc = [ ghj, abc ];
        let cba_jhg = [ cba, jhg ];
        let x = [ ghj_abc, cba_jhg ];
        ret_str(x[0][1]);
        ret_str(x[1][0]);
    ");
    assert_all(&result, &[ "abc".to_string(), "cba".to_string() ]);
}

#[test]
fn array_casting() {
    let result = run("
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
    ");
    assert_all(&result, &[ 5u16, 5, 5, 5 ]);
}

#[test]
#[should_panic(expected = "Resolver error: Expected type [ u8 ], got [ u16 ] in line 7, column 20.")]
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
        ret_{sa_type:?}(array_u16.len());
        ret_{sa_type:?}(array_u8.len());
        ret_{sa_type:?}([ 5, 4, 3, 2, 1, 0 ].len());
    ", ));
    assert_all_sa(&result, &[ 4, 5, 6 ]);
}

#[test]
fn array_push() {
    let sa_type = STACK_ADDRESS_TYPE;
    let result = run(&format!("
        let dynamic_array = [ 1u16, 2, 3 ];
        dynamic_array.push(4u16);
        ret_{sa_type:?}(dynamic_array.len());
        ret_{sa_type:?}(dynamic_array[3] as {sa_type:?});
    "));
    assert_all_sa(&result, &[ 4, 4 ]);
}

#[test]
fn array_ref_elements() {
    let result = run("
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

            let b = a.pop(); // 6
            ret_u8(b.value);

            a.truncate(4);

            let c = a.pop(); // 4
            ret_u8(c.value);

            let d = a.remove(0); // 1
            ret_u8(d.value);

            for x in a { // 2, 3
                ret_u8(x.value);
            }

            ret_u8(a.len() as u8); // 2
        }
    ");
    assert_all(&result, &[ 6u8, 4, 1, 2, 3, 2 ]);
}
