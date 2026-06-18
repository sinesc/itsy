use crate::util::*;

#[test]
fn struct_access() {
    let result = run(stringify!(
        struct Test {
            ba: bool,
            bb: bool,
            ia: i8,
            ib: i16,
            ic: i32,
            id: i64,
            ua: u8,
            ub: u16,
            uc: u32,
            ud: u64,
            fa: f32,
            fb: f64,
        }
        fn main() {
            let x: Test = Test {
                ba: true, bb: false,
                ia: -1, ib: 2, ic: -3, id: 4,
                ua: 1, ub: 2, uc: 3, ud: 4,
                fa: 0.01, fb: 0.001
            };
            ret_i8(x.ia);
            ret_i16(x.ib);
            ret_i32(x.ic);
            ret_i64(x.id);
            ret_u8(x.ua);
            ret_u16(x.ub);
            ret_u32(x.uc);
            ret_u64(x.ud);
            ret_f32(x.fa);
            ret_f64(x.fb);
            ret_bool(x.ba);
            ret_bool(x.bb);
        }
    ));
    assert(&result[0], -1i8);
    assert(&result[1], 2i16);
    assert(&result[2], -3i32);
    assert(&result[3], 4i64);
    assert(&result[4], 1u8);
    assert(&result[5], 2u16);
    assert(&result[6], 3u32);
    assert(&result[7], 4u64);
    assert(&result[8], 0.01f32);
    assert(&result[9], 0.001f64);
    assert(&result[10], true);
    assert(&result[11], false);
}

#[test]
fn struct_nesting() {
    let result = run(stringify!(
        struct MoreInner {
            one: i8,
            two: i16,
            three: i32,
        }
        struct Inner {
            a: f32,
            b: i64,
            c: MoreInner,
        }
        struct Outer {
            first: u8,
            second: u16,
            third: Inner,
        }
        fn main() {
            let x: Outer = Outer { first: 3, second: 345, third: Inner { a: 3.1415, b: -23, c: MoreInner { one: -1, two: -378, three: -123 } } };
            ret_u8(x.first);
            ret_u16(x.second);
            ret_f32(x.third.a);
            ret_i64(x.third.b);
            ret_i8(x.third.c.one);
            ret_i16(x.third.c.two);
            ret_i32(x.third.c.three);
        }
    ));
    assert(&result[0], 3u8);
    assert(&result[1], 345u16);
    assert(&result[2], 3.1415f32);
    assert(&result[3], -23i64);
    assert(&result[4], -1i8);
    assert(&result[5], -378i16);
    assert(&result[6], -123i32);
}

#[test]
fn struct_array() {
    let result = run(stringify!(
        struct Inner {
            ia: u8,
            ib: [ u64 ],
        }
        struct Outer {
            oa: i16,
            ob: Inner,
        }
        fn main() {
            let x: Outer = Outer {
                oa: -369,
                ob: Inner {
                    ia: 8,
                    ib: [ 3, 333, 333333 ],
                }
            };
            ret_i16(x.oa);
            ret_u8(x.ob.ia);
            ret_u64(x.ob.ib[0]);
            ret_u64(x.ob.ib[1]);
            ret_u64(x.ob.ib[2]);
        }
    ));
    assert(&result[0], -369i16);
    assert(&result[1], 8u8);
    assert(&result[2], 3u64);
    assert(&result[3], 333u64);
    assert(&result[4], 333333u64);
}

#[test]
fn member_assign_primitive() {
    let result = run(stringify!(
        struct Inner {
            ia: u8,
            ib: [ u64 ],
        }
        struct Outer {
            oa: i16,
            ob: Inner,
        }
        fn main() {
            let mut x: Outer = Outer {
                oa: 0,
                ob: Inner {
                    ia: 0,
                    ib: [ 0, 0, 0 ],
                }
            };
            x.oa = -369;
            x.ob.ia = 8;
            ret_i16(x.oa);
            ret_u8(x.ob.ia);
        }
    ));
    assert(&result[0], -369i16);
    assert(&result[1], 8u8);
}

#[test]
fn index_assign_primitive() {
    let result = run(stringify!(
        struct Inner {
            ia: u8,
            ib: [ u64 ],
        }
        struct Outer {
            oa: [ u8 ],
            ob: Inner,
        }
        fn main() {
            let x: Outer = Outer {
                oa: [ 0, 0, 0 ],
                ob: Inner {
                    ia: 0,
                    ib: [ 0, 0, 0 ],
                }
            };
            x.oa[0] = 13;
            x.oa[1] = 133;
            x.oa[2] = 255;
            x.ob.ib[0] = 12345678901;
            x.ob.ib[1] = 12345678902;
            x.ob.ib[2] = 12345678903;
            ret_u8(x.oa[0]);
            ret_u8(x.oa[1]);
            ret_u8(x.oa[2]);
            ret_u64(x.ob.ib[0]);
            ret_u64(x.ob.ib[1]);
            ret_u64(x.ob.ib[2]);
        }
    ));
    assert(&result[0], 13u8);
    assert(&result[1], 133u8);
    assert(&result[2], 255u8);
    assert(&result[3], 12345678901u64);
    assert(&result[4], 12345678902u64);
    assert(&result[5], 12345678903u64);
}

#[test]
fn struct_self() {
    let result = run(stringify!(
        struct Test {
            current: i32,
        }
        impl Test {
            fn max() -> i32 {
                123
            }
            fn get(self: Self) {
                ret_i32(Self::max());
                ret_i32(self.current);
            }
        }
        fn main() {
            let test = Test { current: 37 };
            test.get();
        }
    ));
    assert_all(&result, &[ 123i32, 37 ]);
}
#[test]
fn struct_eq() {
    // structs compare structurally, field by field
    let result = run(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let p = Point { x: 1, y: 2 };
            ret_bool(p == Point { x: 1, y: 2 });
            ret_bool(p == Point { x: 1, y: 9 });
            ret_bool(p != Point { x: 9, y: 2 });
        }
    ));
    assert_all(&result, &[ true, false, true ]);
}

#[test]
fn match_struct_pattern() {
    // field literal test + binding extraction (incl. heap String), field shorthand, and field reordering
    let result = run(stringify!(
        struct Point { x: i32, label: String }
        fn describe(p: Point) -> String {
            match p {
                Point { x: 0, label: l } => "origin:" + l,
                Point { label: name, x: got } => "got:" + got as String + ":" + name,
            }
        }
        fn shorthand(p: Point) -> String {
            match p {
                Point { x, label } => x as String + ":" + label,
            }
        }
        fn main() {
            ret_string(describe(Point { x: 0, label: "a" }));
            ret_string(describe(Point { x: 7, label: "b" }));
            ret_string(shorthand(Point { x: 5, label: "c" }));
        }
    ));
    assert_all(&result, &[
        "origin:a".to_string(), "got:7:b".to_string(), "5:c".to_string(),
    ]);
}

#[test]
fn match_struct_non_exhaustive_rejected() {
    // a struct is a product type: leaving a field value uncovered (and no catch-all) is non-exhaustive
    let err = build_err(stringify!(
        struct Flags { a: bool }
        fn main() {
            ret_i32(match Flags { a: true } {
                Flags { a: true } => 1,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive") && err.contains("Flags") && err.contains("false"), "unexpected error: {}", err);
}

#[test]
fn match_struct_exhaustive_product_ok() {
    // covering every combination of a struct's field values is exhaustive without a catch-all
    let result = run(stringify!(
        struct Flags { a: bool, b: bool }
        fn classify(f: Flags) -> i32 {
            match f {
                Flags { a: true, b: true } => 3,
                Flags { a: true, b: false } => 2,
                Flags { a: false, b: true } => 1,
                Flags { a: false, b: false } => 0,
            }
        }
        fn main() {
            ret_i32(classify(Flags { a: true, b: false }));
            ret_i32(classify(Flags { a: false, b: true }));
        }
    ));
    assert_all(&result, &[ 2i32, 1i32 ]);
}

#[test]
fn match_struct_pattern_nested() {
    // struct nested in struct and struct nested in enum, navigated across heap boundaries
    let result = run(stringify!(
        struct Inner { v: i32, s: String }
        struct Outer { inner: Inner, tag: i32 }
        enum E { Wrap(Inner) }
        fn outer(o: Outer) -> String {
            match o {
                Outer { inner: Inner { v: 1, s: msg }, tag: t } => "one:" + msg + ":" + t as String,
                Outer { inner: Inner { v, s }, tag } => v as String + ":" + s + ":" + tag as String,
            }
        }
        fn wrapped(x: E) -> String {
            match x {
                E::Wrap(Inner { v: 9, s }) => "nine:" + s,
                E::Wrap(Inner { v, s }) => v as String + ":" + s,
            }
        }
        fn main() {
            ret_string(outer(Outer { inner: Inner { v: 1, s: "hi" }, tag: 5 }));
            ret_string(outer(Outer { inner: Inner { v: 2, s: "yo" }, tag: 6 }));
            ret_string(wrapped(E::Wrap(Inner { v: 9, s: "deep" })));
            ret_string(wrapped(E::Wrap(Inner { v: 3, s: "shallow" })));
        }
    ));
    assert_all(&result, &[
        "one:hi:5".to_string(), "2:yo:6".to_string(), "nine:deep".to_string(), "3:shallow".to_string(),
    ]);
}

#[test]
fn match_struct_pattern_missing_field_rejected() {
    // without a trailing `..`, every struct field must be matched
    let err = build_err(stringify!(
        struct P { x: i32, y: i32 }
        fn main() {
            let p = P { x: 1, y: 2 };
            match p {
                P { x: 1 } => {},
                _ => {},
            }
        }
    ));
    assert!(err.contains("field 'y'"), "unexpected error: {}", err);
}

#[test]
fn match_struct_pattern_unknown_field_rejected() {
    let err = build_err(stringify!(
        struct P { x: i32, y: i32 }
        fn main() {
            let p = P { x: 1, y: 2 };
            match p {
                P { x: 1, y: 2, z: 3 } => {},
                _ => {},
            }
        }
    ));
    assert!(err.contains("`z`"), "unexpected error: {}", err);
}

#[test]
fn match_struct_pattern_rest() {
    // a trailing `..` lets a struct pattern ignore unlisted fields, both in arms that bind a field and
    // arms that match a literal; the field-less `P { .. }` form matches any value of the struct
    let result = run(stringify!(
        struct P { x: i32, y: i32, z: i32 }
        fn main() {
            let p = P { x: 2, y: 5, z: 9 };
            ret_string(match p {
                P { x: 3, .. } => "three",
                P { x: 2, y, .. } => "two:{y}",
                P { .. } => "any",
            });
        }
    ));
    assert(&result[0], "two:5".to_string());
}

#[test]
fn let_destructure_struct_rest() {
    // `..` in a let destructure ignores the remaining fields while still being irrefutable
    let result = run(stringify!(
        struct P { x: i32, y: i32, z: String }
        fn main() {
            let p = P { x: 1, y: 2, z: "hi" };
            let P { x, .. } = p;
            ret_i32(x);
        }
    ));
    assert(&result[0], 1i32);
}

#[test]
fn match_struct_pattern_rest_exhaustive() {
    // `P { .. }` covers every value of the struct, so no wildcard arm is required
    let result = run(stringify!(
        struct P { x: i32, y: i32 }
        fn main() {
            let p = P { x: 1, y: 2 };
            ret_i32(match p {
                P { .. } => 42,
            });
        }
    ));
    assert(&result[0], 42i32);
}

#[test]
fn match_struct_pattern_rest_duplicate_field_rejected() {
    // `..` allows missing fields but a listed field still may not be repeated
    let err = build_err(stringify!(
        struct P { x: i32, y: i32 }
        fn main() {
            let p = P { x: 1, y: 2 };
            match p {
                P { x: 1, x: 2, .. } => {},
                _ => {},
            }
        }
    ));
    assert!(err.contains("field 'x'"), "unexpected error: {}", err);
}

#[test]
fn let_destructure_struct() {
    // let destructuring: explicit binding, shorthand, wildcard, and nested struct across a heap boundary
    let result = run(stringify!(
        struct Inner { v: i32, s: String }
        struct Outer { inner: Inner, tag: i32 }
        fn main() {
            let o = Outer { inner: Inner { v: 7, s: "deep" }, tag: 99 };
            let Outer { inner: Inner { v, s: msg }, tag: _ } = o;
            ret_i32(v);
            ret_string(msg);
            let Outer { inner, tag } = o; // shorthand
            ret_i32(tag);
            let Inner { v: vv, s: ss } = inner;
            ret_i32(vv);
            ret_string(ss);
        }
    ));
    assert(&result[0], 7i32);
    assert(&result[1], "deep".to_string());
    assert(&result[2], 99i32);
    assert(&result[3], 7i32);
    assert(&result[4], "deep".to_string());
}

#[test]
fn let_destructure_refutable_literal_rejected() {
    let err = build_err(stringify!(
        struct P { x: i32 }
        fn main() {
            let P { x: 1 } = P { x: 1 };
        }
    ));
    assert!(err.contains("Refutable"), "unexpected error: {}", err);
}

#[test]
fn let_destructure_refutable_enum_rejected() {
    let err = build_err(stringify!(
        enum E { A(i32), B }
        fn main() {
            let E::A(v) = E::A(1);
        }
    ));
    assert!(err.contains("Refutable"), "unexpected error: {}", err);
}

#[test]
fn struct_eq_nested() {
    // nested struct/array fields are compared recursively
    let result = run(stringify!(
        struct Inner { values: [ i32 ] }
        struct Outer { inner: Inner, tag: i32 }
        fn main() {
            let a = Outer { inner: Inner { values: [ 1, 2, 3 ] }, tag: 7 };
            ret_bool(a == Outer { inner: Inner { values: [ 1, 2, 3 ] }, tag: 7 });
            ret_bool(a == Outer { inner: Inner { values: [ 1, 2, 4 ] }, tag: 7 });
            ret_bool(a == Outer { inner: Inner { values: [ 1, 2, 3 ] }, tag: 8 });
        }
    ));
    assert_all(&result, &[ true, false, false ]);
}

#[test]
fn recursive_struct_direct_rejected() {
    // a struct containing itself by value has infinite size; reject instead of overflowing the compiler's stack
    let err = build_err(stringify!(
        struct Test { recurse: Test }
        fn main() {}
    ));
    assert!(err.contains("Recursive type `Test`"), "unexpected error: {}", err);
}

#[test]
fn recursive_struct_via_array_rejected() {
    // recursion through an array element is detected too
    let err = build_err(stringify!(
        struct Test { children: [ Test ] }
        fn main() {}
    ));
    assert!(err.contains("Recursive type `Test`"), "unexpected error: {}", err);
}

#[test]
fn recursive_struct_mutual_rejected() {
    // mutually recursive structs form a cycle and are rejected as well
    let err = build_err(stringify!(
        struct A { b: B }
        struct B { a: A }
        fn main() {}
    ));
    assert!(err.contains("Recursive type"), "unexpected error: {}", err);
}
