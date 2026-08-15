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
    assert!(err.contains("Recursive type `E`"), "unexpected error: {}", err);
}

// ===== struct variants =====

#[test]
fn struct_variant_definition_and_literals() {
    // an enum may mix struct, tuple and unit variants; the struct variant is constructible both as a
    // struct literal (`E::A { x, y }`) and via the positional constructor call (`E::A(x, y)`)
    let result = run(stringify!(
        enum E {
            A { x: i32, y: i32 },
            B(i32),
            C,
        }
        fn main() {
            let a = E::A { x: 1, y: 2 };
            let a2 = E::A(1, 2);
            ret_bool(a == a2);
            ret_bool(a == E::A { x: 1, y: 2 });
            ret_bool(a != E::A { x: 1, y: 3 });
            ret_bool(a != E::A { x: 2, y: 2 });
            ret_bool(a != E::B(1));
            ret_bool(a != E::C);
            ret_bool(E::B(1) == E::B(1));
            ret_bool(E::C == E::C);
        }
    ));
    assert_all(&result, &[ true, true, true, true, true, true, true, true ]);
}

#[test]
fn struct_variant_literal_field_order() {
    // struct-variant literal fields are matched by name, so their order is independent of the definition
    let result = run(stringify!(
        enum E { A { x: i32, y: i32, z: i32 } }
        fn main() {
            let a = E::A { x: 1, y: 2, z: 3 };
            ret_bool(a == E::A { z: 3, x: 1, y: 2 });
            ret_bool(a == E::A { y: 2, z: 3, x: 1 });
            ret_bool(a != E::A { z: 3, x: 2, y: 2 });
        }
    ));
    assert_all(&result, &[ true, true, true ]);
}

#[test]
fn struct_variant_literal_heap_fields() {
    // heap (String) fields compare deeply and can be extracted in match arms
    let result = run(stringify!(
        enum S { Named { name: String, age: i32 }, Other }
        fn main() {
            let s1 = S::Named { name: "bob", age: 30 };
            let s2 = S::Named { name: "bob", age: 30 };
            let s3 = S::Named { name: "bob", age: 31 };
            let s4 = S::Named { name: "alice", age: 30 };
            ret_bool(s1 == s2);
            ret_bool(s1 != s3);
            ret_bool(s1 != s4);
            ret_bool(s1 != S::Other);
            ret_string(match s1 {
                S::Named { name, age } => name + ":" + age as String,
                S::Other => "other",
            });
        }
    ));
    assert_all!(&result, [ true, true, true, true, "bob:30".to_string() ]);
}

#[test]
fn struct_variant_empty_fields() {
    // a struct variant may declare no fields; it matches with the fieldless `E::A {}` pattern and
    // compares by variant identity like a unit variant
    let result = run(stringify!(
        enum E { A {}, B { x: i32 } }
        fn main() {
            let a = E::A {};
            let b = E::B { x: 5 };
            ret_bool(a != b);
            ret_bool(a == E::A {});
            ret_bool(E::A {} == E::A {});
            ret_i32(match a {
                E::A {} => 1,
                E::B { x } => x,
            });
            ret_i32(match b {
                E::A {} => -1,
                E::B { x } => x,
            });
        }
    ));
    assert_all!(&result, [ true, true, true, 1i32, 5i32 ]);
}

#[test]
fn struct_variant_nested_and_by_value() {
    // struct variants may hold nested enums and be nested inside other variants; passing one by value
    // keeps the heap object alive across the call
    let result = run(stringify!(
        enum Inner { X, Y(i32) }
        enum Outer { Wrap { inner: Inner, tag: i32 }, Solo }
        enum T { P(Outer) }
        fn f(o: Outer) -> i32 {
            match o {
                Outer::Wrap { inner, .. } => match inner { Inner::X => 1, Inner::Y(n) => n },
                Outer::Solo => 0,
            }
        }
        fn main() {
            let o = Outer::Wrap { inner: Inner::Y(7), tag: 3 };
            ret_i32(match o {
                Outer::Wrap { inner: Inner::Y(n), tag } => n * 10 + tag,
                Outer::Wrap { inner: Inner::X, tag } => tag,
                Outer::Solo => -1,
            });
            ret_i32(match T::P(Outer::Wrap { inner: Inner::X, tag: 42 }) {
                T::P(Outer::Wrap { inner: Inner::X, tag }) => tag,
                T::P(Outer::Wrap { .. }) => -1,
                T::P(Outer::Solo) => -2,
            });
            ret_i32(f(Outer::Wrap { inner: Inner::Y(5), tag: 1 }));
            ret_bool(Outer::Wrap { inner: Inner::X, tag: 1 } == Outer::Wrap { inner: Inner::X, tag: 1 });
            ret_bool(Outer::Wrap { inner: Inner::X, tag: 1 } != Outer::Wrap { inner: Inner::Y(2), tag: 1 });
        }
    ));
    assert_all!(&result, [ 73i32, 42i32, 5i32, true, true ]);
}

#[test]
fn struct_variant_mixed_field_sizes() {
    // fields of different primitive sizes pack tightly in the heap object
    let result = run(stringify!(
        enum E { A { big: i64, small: u16 } }
        fn main() {
            let a = E::A { big: 42, small: 7 };
            ret_bool(a == E::A { big: 42, small: 7 });
            ret_bool(a != E::A { big: 42, small: 8 });
            ret_i64(match a {
                E::A { big, small } => big + small as i64,
            });
        }
    ));
    assert_all!(&result, [ true, true, 49i64 ]);
}

#[test]
fn match_struct_variant() {
    // exhaustive match mixing a struct-variant arm with field bindings, a tuple arm and a unit arm
    let result = run(stringify!(
        enum E { A { x: i32, y: i32 }, B(i32), C }
        fn describe(e: E) -> i32 {
            match e {
                E::A { x, y } => x * 10 + y,
                E::B(n) => n,
                E::C => -1,
            }
        }
        fn main() {
            ret_i32(describe(E::A { x: 1, y: 2 }));
            ret_i32(describe(E::B(9)));
            ret_i32(describe(E::C));
        }
    ));
    assert_all(&result, &[ 12i32, 9, -1 ]);
}

#[test]
fn match_struct_variant_literal_subpattern() {
    // literal sub-patterns inside struct-variant arms (numeric and String), with `..` rest
    let result = run(stringify!(
        enum E { A { x: i32, y: i32 } }
        enum S { Named { name: String, age: i32 }, Other }
        fn main() {
            ret_i32(match E::A { x: 1, y: 2 } {
                E::A { x: 1, y } => y,
                E::A { x, .. } => x,
            });
            ret_i32(match E::A { x: 3, y: 4 } {
                E::A { x: 1, y } => y,
                E::A { x, .. } => x,
            });
            ret_string(match S::Named { name: "bob", age: 30 } {
                S::Named { name: "bob", age } => "age:" + age as String,
                S::Named { name, .. } => "other:" + name,
                S::Other => "other",
            });
        }
    ));
    assert_all!(&result, [ 2i32, 3i32, "age:30".to_string() ]);
}

#[test]
fn match_struct_variant_string_subpattern_refcount() {
    // regression test: literal sub-patterns compare the subject's String field with an opcode that
    // frees its operands; the subject's fields must survive the test and be released exactly once
    // by the matched arm (fresh temporaries, borrowed locals, function results, nested variants,
    // and repeated matches with heap slot recycling)
    let result = run(stringify!(
        enum S { Named { name: String, age: i32 }, Other }
        enum N { Wrap(S, i32), None }
        fn make() -> S { S::Named { name: "bob", age: 30 } }
        fn main() {
            let local = S::Named { name: "bob", age: 30 };
            ret_i32(match local {
                S::Named { name: "bob", age, .. } => age,
                S::Named { .. } => -1,
                S::Other => -2,
            });
            ret_string(match make() {
                S::Named { name: "bob", age, .. } => "age:" + age as String,
                S::Named { name, .. } => "nm:" + name,
                S::Other => "other",
            });
            ret_i32(match N::Wrap(S::Named { name: "bob", age: 30 }, 9) {
                N::Wrap(S::Named { name: "bob", age, .. }, _) => age,
                N::Wrap(S::Named { .. }, _) => -1,
                N::Wrap(S::Other, _) => -2,
                N::None => -3,
            });
            let l2 = S::Named { name: "x", age: 1 };
            ret_i32(match l2 {
                S::Named { age, .. } => age,
                S::Other => -1,
            });
            ret_i32(match l2 {
                S::Named { name: "x", age, .. } => age,
                S::Named { .. } => -1,
                S::Other => -2,
            });
            ret_i32(match S::Named { name: "bob", age: 30 } { S::Named { name: "bob", .. } => 1, S::Named { .. } => 0, S::Other => -1 });
            ret_i32(match S::Named { name: "bob", age: 31 } { S::Named { name: "bob", .. } => 1, S::Named { .. } => 0, S::Other => -1 });
            ret_i32(match S::Named { name: "amy", age: 32 } { S::Named { name: "bob", .. } => 1, S::Named { .. } => 0, S::Other => -1 });
        }
    ));
    assert_all!(&result, [ 30i32, "age:30".to_string(), 30i32, 1i32, 1i32, 1i32, 1i32, 0i32 ]);
}

#[test]
fn match_struct_variant_range_and_or_patterns() {
    // range and or-patterns against struct-variant fields on fresh literal subjects
    let result = run(stringify!(
        enum S { Named { name: String, age: i32 }, Other }
        fn main() {
            ret_i32(match S::Named { name: "bob", age: 30 } {
                S::Named { age: 25..40, .. } => 1,
                S::Named { .. } => 0,
                S::Other => -1,
            });
            ret_i32(match S::Named { name: "bob", age: 30 } {
                S::Named { name: "bob", .. } | S::Named { name: "alice", .. } => 1,
                S::Named { .. } => 0,
                S::Other => -1,
            });
            ret_i32(match S::Named { name: "amy", age: 30 } {
                S::Named { name: "bob", .. } | S::Named { name: "alice", .. } => 1,
                S::Named { .. } => 0,
                S::Other => -1,
            });
        }
    ));
    assert_all!(&result, [ 1i32, 1i32, 0i32 ]);
}

#[test]
fn match_struct_variant_rest() {
    // a trailing `..` lets a struct-variant arm omit fields; the fieldless `E::A { .. }` matches any
    // value of the variant
    let result = run(stringify!(
        enum E { A { x: i32, y: i32, z: i32 }, B }
        fn main() {
            let a = E::A { x: 1, y: 2, z: 3 };
            ret_i32(match a {
                E::A { x: 9, .. } => -1,
                E::A { x, z, .. } => x * 10 + z,
                E::B => -2,
            });
            ret_bool(match a {
                E::A { .. } => true,
                E::B => false,
            });
        }
    ));
    assert_all!(&result, [ 13i32, true ]);
}

#[test]
fn match_struct_variant_other_variant_not_matched() {
    // struct-variant arms are gated on the variant tag: a value of another variant must fall through
    // to its own arm, even when payload layouts overlap (B's i32 sits at A's `x` offset)
    let result = run(stringify!(
        enum E { A { x: i32, y: i32 }, B(i32), C }
        fn main() {
            ret_i32(match E::B(9) {
                E::A { x, y } => 1000 + x * 10 + y,
                E::B(n) => n,
                E::C => -1,
            });
            ret_i32(match E::C {
                E::A { .. } => 777,
                E::B(_) => -1,
                E::C => -2,
            });
            ret_i32(match E::A { x: 5, y: 6 } {
                E::A { x, y } => x + y,
                E::B(_) => -1,
                E::C => -2,
            });
        }
    ));
    assert_all(&result, &[ 9i32, -2, 11 ]);
}

#[test]
fn struct_variant_illegal_variant_mix_rejected() {
    // struct variants carry data, so mixing them with value (C-like) variants is rejected
    let err = build_err(stringify!(
        enum E { A { x: i32 }, B = 1 }
        fn main() {}
    ));
    assert!(err.contains("Illegal mix of enum data and value variants"), "unexpected error: {}", err);
}

#[test]
fn struct_variant_literal_unknown_field_rejected() {
    // a literal naming a field the variant doesn't have must be a resolver error
    let err = build_err(stringify!(
        enum E { A { x: i32, y: i32 } }
        fn main() {
            let a = E::A { x: 1, y: 2, z: 3 };
            ret_i32(0);
        }
    ));
    assert!(err.contains("Undefined struct member") && err.contains("`z`"), "unexpected error: {}", err);
}

#[test]
fn match_struct_variant_non_exhaustive_rejected() {
    // the witness for a missing struct variant renders its fields as `name: _`
    let err = build_err(stringify!(
        enum E { A { x: i32, y: i32 }, B }
        fn main() {
            ret_i32(match E::B {
                E::B => 0,
            });
        }
    ));
    assert!(err.contains("Non-exhaustive") && err.contains("E::A {x: _, y: _}"), "unexpected error: {}", err);
}

#[test]
fn match_struct_variant_pattern_missing_field_rejected() {
    // without a trailing `..`, every field of a struct variant must be matched
    let err = build_err(stringify!(
        enum E { A { x: i32, y: i32 }, B }
        fn main() {
            ret_i32(match E::A { x: 1, y: 2 } {
                E::A { x } => x,
                E::B => 0,
            });
        }
    ));
    assert!(err.contains("Struct variant pattern must match field 'y' exactly once"), "unexpected error: {}", err);
}

#[test]
fn match_struct_variant_pattern_unknown_field_rejected() {
    // a sub-pattern naming a field the variant doesn't have must be a resolver error
    let err = build_err(stringify!(
        enum E { A { x: i32, y: i32 }, B }
        fn main() {
            ret_i32(match E::A { x: 1, y: 2 } {
                E::A { x, nope } => x,
                E::B => 0,
            });
        }
    ));
    assert!(err.contains("Undefined struct member") && err.contains("`nope`"), "unexpected error: {}", err);
}

#[test]
fn match_struct_variant_pattern_unqualified_rejected() {
    // a struct-variant pattern must be qualified with the enum type name
    let err = build_err(stringify!(
        enum E { A { x: i32, y: i32 }, B }
        fn main() {
            ret_i32(match E::A { x: 1, y: 2 } {
                A { x, y } => x,
                E::B => 0,
            });
        }
    ));
    assert!(err.contains("Struct variant pattern must be qualified with the enum type name"), "unexpected error: {}", err);
}

#[test]
fn match_struct_variant_pattern_wrong_type_rejected() {
    // a struct-variant pattern must name a variant of the subject enum
    let err = build_err(stringify!(
        enum E { A { x: i32, y: i32 }, B }
        enum F { A { x: i32, y: i32 }, C }
        fn main() {
            ret_i32(match F::A { x: 1, y: 2 } {
                E::A { x, y } => x,
                F::C => 0,
            });
        }
    ));
    assert!(err.contains("does not match subject type"), "unexpected error: {}", err);
}

#[test]
fn match_struct_variant_pattern_on_tuple_variant_rejected() {
    // a braced pattern against a tuple (non-struct) variant is not a valid pattern
    let err = build_err(stringify!(
        enum E { A { x: i32, y: i32 }, B(i32) }
        fn main() {
            ret_i32(match E::B(1) {
                E::B { x } => x,
                E::A { .. } => 0,
            });
        }
    ));
    assert!(err.contains("Cannot resolve `E::B`"), "unexpected error: {}", err);
}

#[test]
fn let_destructure_struct_variant_rejected() {
    // a struct-variant pattern is refutable (the value may hold a different variant), so it cannot
    // be used in a let binding; unlike `let P { x, y } = p` on a struct, which is irrefutable
    let err = build_err(stringify!(
        enum E { A { x: i32, y: i32 }, B(i32) }
        fn main() {
            let e = E::B(9);
            let E::A { x, y } = e;
            ret_i32(x + y);
        }
    ));
    assert!(err.contains("Refutable enum pattern not allowed in let binding"), "unexpected error: {}", err);
}
