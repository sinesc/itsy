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

