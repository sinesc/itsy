use crate::util::*;

#[test]
fn primitive_enum() {
    let result = run(stringify!(
        enum Simple {
            A, B, C
        }
        fn accept(ty: Simple) {
            if ty == Simple::A {
                ret_u8(1);
            } else if ty == Simple::B {
                ret_u8(2);
            } else if ty == Simple::C {
                ret_u8(3);
            }
        }
        fn main() {
            accept(Simple::A);
            accept(Simple::B);
            accept(Simple::C);
            let b = Simple::B;
            if b != Simple::B {
                ret_u8(101);
            } else {
                ret_u8(4);
            }
        }
    ));
    assert_all(&result, &[ 1u8, 2, 3, 4 ]);
}

#[test]
fn primitive_enum_cast() {
    let result = run(stringify!(
        enum Simple {
            A = 3, B, C
        }
        fn main() {
            let a = Simple::A;
            ret_u8(a as u8);
            ret_u8(Simple::B as u8);
            if a as u8 == 3 {
                ret_u8(1)
            } else {
                ret_u8(0);
            }
        }
    ));
    assert_all(&result, &[ 3u8, 4, 1 ]);
}

#[test]
fn data_enum_unit_variant_eq() {
    // unit variants of an enum that also has data variants compare by variant identity
    let result = run(stringify!(
        enum Test { SimpleA, SimpleB, Data(i32) }
        fn main() {
            let a = Test::SimpleA;
            ret_bool(a == Test::SimpleA);
            ret_bool(a == Test::SimpleB);
            ret_bool(a != Test::SimpleB);
            ret_bool(a == Test::Data(0));
        }
    ));
    assert_all(&result, &[ true, false, true, false ]);
}

#[test]
fn data_enum_unit_variant_index_not_discriminant() {
    // data variant precedes unit variants: heap tag must be the variant index, not the discriminant,
    // otherwise SimpleA (discriminant 0) would collide with Data (variant index 0)
    let result = run(stringify!(
        enum Test { Data(i32), SimpleA, SimpleB }
        fn main() {
            ret_bool(Test::SimpleA == Test::SimpleA);
            ret_bool(Test::SimpleA == Test::Data(0));
            ret_bool(Test::Data(0) == Test::Data(0));
        }
    ));
    assert_all(&result, &[ true, false, true ]);
}

#[test]
fn data_enum_data_variant_eq() {
    // data-carrying variants compare deeply, including the payload
    let result = run(stringify!(
        enum Test { SimpleA, Data(i32) }
        fn main() {
            ret_bool(Test::Data(23) == Test::Data(23));
            ret_bool(Test::Data(23) == Test::Data(7));
            ret_bool(Test::Data(23) != Test::Data(7));
            ret_bool(Test::Data(23) == Test::SimpleA);
        }
    ));
    assert_all(&result, &[ true, false, true, false ]);
}

#[test]
fn data_enum_multi_field_variant_eq() {
    let result = run(stringify!(
        enum Test { Pair(i32, String), Solo }
        fn main() {
            ret_bool(Test::Pair(1, "x") == Test::Pair(1, "x"));
            ret_bool(Test::Pair(1, "x") == Test::Pair(1, "y"));
            ret_bool(Test::Pair(1, "x") == Test::Pair(2, "x"));
            ret_bool(Test::Pair(1, "x") == Test::Solo);
        }
    ));
    assert_all(&result, &[ true, false, false, false ]);
}

#[test]
fn data_enum_nested_heap_payload_eq() {
    // deep equality recurses through string, array and nested-enum payloads
    let result = run(stringify!(
        enum Inner { A, B(i32) }
        enum Test { Str(String), Arr([i32]), Nest(Inner) }
        fn main() {
            ret_bool(Test::Str("hello") == Test::Str("hello"));
            ret_bool(Test::Str("hello") == Test::Str("world"));
            ret_bool(Test::Arr([1, 2, 3]) == Test::Arr([1, 2, 3]));
            ret_bool(Test::Arr([1, 2, 3]) == Test::Arr([1, 2, 4]));
            ret_bool(Test::Arr([1, 2, 3]) == Test::Arr([1, 2]));
            ret_bool(Test::Nest(Inner::B(5)) == Test::Nest(Inner::B(5)));
            ret_bool(Test::Nest(Inner::B(5)) == Test::Nest(Inner::B(6)));
            ret_bool(Test::Nest(Inner::A) == Test::Nest(Inner::B(5)));
        }
    ));
    assert_all(&result, &[ true, false, true, false, false, true, false, false ]);
}


#[test]
fn match_pattern_unknown_variant_rejected() {
    // a pattern naming a variant the enum doesn't have must be a resolver error
    let err = build_err(stringify!(
        enum E { A, Data(i32) }
        fn main() {
            ret_i32(match E::A {
                E::Nope => 1,
                _ => 0,
            });
        }
    ));
    assert!(err.contains("Resolver error") && err.contains("Nope"), "unexpected error: {}", err);
}

#[test]
fn match_pattern_wrong_arity_rejected() {
    // a data-variant pattern with the wrong number of sub-patterns must be a resolver error
    let err = build_err(stringify!(
        enum E { A, Data(i32) }
        fn main() {
            ret_i32(match E::Data(1) {
                E::Data(a, b) => a + b,
                _ => 0,
            });
        }
    ));
    assert!(err.contains("Resolver error") && err.contains("E::Data"), "unexpected error: {}", err);
}

#[test]
fn match_variant_pattern_on_non_enum_rejected() {
    // a variant pattern used against a non-enum subject must be a resolver error
    let err = build_err(stringify!(
        enum E { Data(i32) }
        fn main() {
            ret_i32(match 5i32 {
                E::Data(x) => x,
                _ => 0,
            });
        }
    ));
    assert!(err.contains("Resolver error") && err.contains("non-enum"), "unexpected error: {}", err);
}

#[test]
fn match_non_exhaustive_missing_variant_rejected() {
    // a match that omits a variant and has no catch-all must be a resolver error naming the uncovered variant
    let err = build_err(stringify!(
        enum E { A, B, C }
        fn main() {
            ret_i32(match E::A {
                E::A => 0,
                E::B => 1,
            });
        }
    ));
    assert!(err.contains("Resolver error") && err.contains("Non-exhaustive") && err.contains("E::C"), "unexpected error: {}", err);
}

#[test]
fn match_non_exhaustive_data_variant_rejected() {
    // the witness for a missing data variant carries a wildcard sub-pattern
    let err = build_err(stringify!(
        enum E { A, Data(i32) }
        fn main() {
            ret_i32(match E::A {
                E::A => 0,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive") && err.contains("E::Data(_)"), "unexpected error: {}", err);
}

#[test]
fn match_exhaustive_all_variants_ok() {
    // covering every variant (without a catch-all) is exhaustive
    let result = run(stringify!(
        enum E { A, B, Data(i32) }
        fn main() {
            let e = E::Data(7);
            ret_i32(match e {
                E::A => 0,
                E::B => 1,
                E::Data(n) => n,
            });
        }
    ));
    assert_all(&result, &[ 7i32 ]);
}

#[test]
fn match_non_exhaustive_nested_enum_rejected() {
    // exhaustiveness recurses into a covered variant's data: the inner enum is not fully covered here
    let err = build_err(stringify!(
        enum Inner { X, Y }
        enum Outer { Wrap(Inner) }
        fn main() {
            ret_i32(match Outer::Wrap(Inner::X) {
                Outer::Wrap(Inner::X) => 0,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive") && err.contains("Inner::Y"), "unexpected error: {}", err);
}

#[test]
fn match_reference_enum() {
    // unit and data variants, literal sub-pattern, field binding (incl. heap String extraction) and wildcard
    let result = run(stringify!(
        enum E { A, B, Data(i32, String) }
        fn describe(e: E) -> String {
            match e {
                E::A => "a",
                E::B => "b",
                E::Data(123, val) => "special:" + val,
                E::Data(_, val) => "data:" + val,
            }
        }
        fn main() {
            ret_string(describe(E::A));
            ret_string(describe(E::B));
            ret_string(describe(E::Data(123, "x")));
            ret_string(describe(E::Data(7, "y")));
        }
    ));
    assert_all(&result, &[
        "a".to_string(), "b".to_string(), "special:x".to_string(), "data:y".to_string(),
    ]);
}

#[test]
fn match_nested_enum_pattern() {
    // nested variant patterns: literal sub-pattern, binding extraction across a heap boundary, and a nested unit variant
    let result = run(stringify!(
        enum Inner { Simple, Data(i64) }
        enum Outer { A, With(Inner) }
        fn describe(o: Outer) -> String {
            match o {
                Outer::A => "a",
                Outer::With(Inner::Data(3)) => "three",
                Outer::With(Inner::Data(val)) => "data:" + val as String,
                Outer::With(Inner::Simple) => "simple",
            }
        }
        fn main() {
            ret_string(describe(Outer::A));
            ret_string(describe(Outer::With(Inner::Data(3))));
            ret_string(describe(Outer::With(Inner::Data(42))));
            ret_string(describe(Outer::With(Inner::Simple)));
        }
    ));
    assert_all(&result, &[
        "a".to_string(), "three".to_string(), "data:42".to_string(), "simple".to_string(),
    ]);
}

#[test]
fn match_primitive_enum() {
    // C-like enum dispatched by discriminant value
    let result = run(stringify!(
        enum Dir { North = 1, East, South, West }
        fn main() {
            let d = Dir::South;
            ret_i32(match d {
                Dir::North => 1,
                Dir::East => 2,
                Dir::South => 3,
                _ => 99,
            });
        }
    ));
    assert_all(&result, &[ 3i32 ]);
}

#[test]
fn match_integer_literals() {
    // matching a non-enum (integer) subject with literal patterns and a catch-all binding
    let result = run(stringify!(
        fn classify(n: i32) -> i32 {
            match n {
                0 => 100,
                1 => 200,
                other => other + 1,
            }
        }
        fn main() {
            ret_i32(classify(0));
            ret_i32(classify(1));
            ret_i32(classify(41));
        }
    ));
    assert_all(&result, &[ 100i32, 200, 42 ]);
}

#[test]
fn match_binds_whole_subject() {
    // a bare-identifier pattern binds the entire (heap) subject, which is then reused (refcount must hold)
    let result = run(stringify!(
        enum E { A, B, Data(i32, String) }
        fn main() {
            let x = E::Data(1, "hi");
            ret_bool(match x {
                E::A => false,
                bound => bound == E::Data(1, "hi"),
            });
        }
    ));
    assert_all(&result, &[ true ]);
}

#[test]
fn recursive_enum_rejected() {
    // a data variant referencing the enum by value makes it infinitely sized; reject instead of overflowing
    let err = build_err(stringify!(
        enum E { Nil, Cons(E) }
        fn main() {}
    ));
    assert!(err.contains("Recursive type 'E'"), "unexpected error: {}", err);
}
