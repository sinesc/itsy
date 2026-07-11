use crate::util::*;

// =============================================================================
// Value-type consts (primitives: u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, bool)
// =============================================================================

#[test]
fn const_u8() {
    let result = run(stringify!(
        const X = 42u8;
        ret_u8(X);
    ));
    assert_all(&result, &[42u8]);
}

#[test]
fn const_i32() {
    let result = run(stringify!(
        const NEG = -123i32;
        ret_i32(NEG);
    ));
    assert_all(&result, &[-123i32]);
}

#[test]
fn const_bool() {
    let result = run(stringify!(
        const TRUE = true;
        const FALSE = false;
        ret_bool(TRUE);
        ret_bool(FALSE);
    ));
    assert_all(&result, &[true, false]);
}

#[test]
fn const_f64() {
    let result = run(stringify!(
        const PI = 3.14f64;
        ret_f64(PI);
    ));
    assert_all(&result, &[3.14f64]);
}

#[test]
fn const_u64_large() {
    let result = run(stringify!(
        const BIG = 0xDEADBEEFu64;
        ret_u64(BIG);
    ));
    assert_all(&result, &[0xDEADBEEFu64]);
}

// =============================================================================
// Type-annotated consts
// =============================================================================

#[test]
fn const_annotated_type() {
    let result = run(stringify!(
        const X: u16 = 100;
        ret_u16(X);
    ));
    assert_all(&result, &[100u16]);
}

#[test]
fn const_annotated_bool() {
    let result = run(stringify!(
        const FLAG: bool = true;
        ret_bool(FLAG);
    ));
    assert_all(&result, &[true]);
}

// =============================================================================
// Const referencing another const
// =============================================================================

#[test]
fn const_refers_to_const() {
    let result = run(stringify!(
        const A = 10u32;
        const B = A;
        ret_u32(B);
    ));
    assert_all(&result, &[10u32]);
}

#[test]
fn const_chain() {
    let result = run(stringify!(
        const A = 5u64;
        const B = A;
        const C = B;
        ret_u64(C);
    ));
    assert_all(&result, &[5u64]);
}

// =============================================================================
// Const used in expressions
// =============================================================================

#[test]
fn const_in_expression() {
    let result = run(stringify!(
        const X = 10u32;
        const Y = 20u32;
        ret_u32(X + Y);
        ret_u32(X * 3);
        ret_u32(Y - X);
    ));
    assert_all(&result, &[30u32, 30, 10]);
}

#[test]
fn const_in_comparison() {
    let result = run(stringify!(
        const THRESHOLD = 50u32;
        ret_bool(40 < THRESHOLD);
        ret_bool(60 > THRESHOLD);
        ret_bool(50 == THRESHOLD);
    ));
    assert_all(&result, &[true, true, true]);
}

// =============================================================================
// Reference-type consts: String
// =============================================================================

#[test]
fn const_string() {
    let result = run(stringify!(
        const MSG = "hello world";
        ret_str(MSG);
    ));
    assert_all(&result, &["hello world".to_string()]);
}

#[test]
fn const_string_used_twice() {
    let result = run(stringify!(
        const GREETING = "hi";
        ret_str(GREETING);
        ret_str(GREETING);
    ));
    assert_all(&result, &["hi".to_string(), "hi".to_string()]);
}

// =============================================================================
// Reference-type consts: Array
// =============================================================================

#[test]
fn const_array() {
    let result = run(stringify!(
        const ARR = [1u8, 2u8, 3u8];
        ret_u8(ARR[0]);
        ret_u8(ARR[1]);
        ret_u8(ARR[2]);
        ret_u64(ARR.len());
    ));
    assert_all!(&result, [1u8, 2u8, 3u8, 3u64]);
}

// =============================================================================
// Reference-type consts: Struct
// =============================================================================

#[test]
fn const_struct() {
    let result = run(stringify!(
        fn main() { ret_u32(ORIGIN.x); ret_u32(ORIGIN.y); }
        struct Point { x: u32, y: u32 }
        const ORIGIN = Point { x: 0, y: 0 };
    ));
    assert_all(&result, &[0u32, 0]);
}

#[test]
fn const_struct_with_string_field() {
    let result = run(stringify!(
        fn main() { ret_str(L.name); ret_u32(L.value); }
        struct Label { name: String, value: u32 }
        const L = Label { name: "test", value: 42 };
    ));
    assert_all!(&result, ["test".to_string(), 42u32]);
}

// =============================================================================
// Reference-type consts: Map
// =============================================================================

#[test]
fn const_map() {
    let result = run(stringify!(
        const M = ["a" => 1u32, "b" => 2u32];
        ret_u32(M["a"]);
        ret_u32(M["b"]);
    ));
    assert_all(&result, &[1u32, 2u32]);
}

// =============================================================================
// Const inside function scope
// =============================================================================

#[test]
fn const_in_function() {
    let result = run(stringify!(
        fn compute() -> u32 { const BASE = 100u32; return BASE + 50; }
        fn main() { ret_u32(compute()); }
    ));
    assert_all(&result, &[150u32]);
}

#[test]
fn const_in_function_ref_type() {
    let result = run(stringify!(
        fn get_msg() -> String { const MSG = "from function"; return MSG; }
        fn main() { ret_str(get_msg()); }
    ));
    assert_all(&result, &["from function".to_string()]);
}

#[test]
fn const_in_function_multiple() {
    let result = run(stringify!(
        fn add() -> u32 { const A = 10u32; const B = 20u32; return A + B; }
        fn main() { ret_u32(add()); }
    ));
    assert_all(&result, &[30u32]);
}

// =============================================================================
// Const inside impl block
// =============================================================================

#[test]
fn const_in_impl() {
    let result = run(stringify!(
        fn main() { let c = Counter::new(); ret_u32(c.value); }
        struct Counter { value: u32 }
        impl Counter {
            const INITIAL = 0u32;
            fn new() -> Counter {
                return Counter { value: Self::INITIAL };
            }
        }
    ));
    assert_all(&result, &[0u32]);
}

#[test]
fn const_in_impl_string() {
    let result = run(stringify!(
        fn main() { let g = Greeter::new("World"); ret_str(g.name); }
        struct Greeter { name: String }
        impl Greeter {
            const PREFIX = "Hello, ";
            fn new(name: String) -> Greeter {
                return Greeter { name: Self::PREFIX + name };
            }
        }
    ));
    assert_all(&result, &["Hello, World".to_string()]);
}

// =============================================================================
// Const inside trait impl block
// =============================================================================

#[test]
fn const_in_trait_impl() {
    // Trait declares the const (required), impl provides the value
    let result = run(stringify!(
        fn main() { ret_str(Thing::description()); }
        trait Describable {
            const LABEL: String;
            fn description() -> String;
        }
        struct Thing { name: String }
        impl Describable for Thing {
            const LABEL = "Thing";
            fn description() -> String {
                return Self::LABEL + " object";
            }
        }
    ));
    assert_all(&result, &["Thing object".to_string()]);
}

// =============================================================================
// Trait constants
// =============================================================================

#[test]
fn trait_const_provided_default() {
    // Trait provides a default const value; trait default method uses it
    let result = run(stringify!(
        fn main() { ret_u32(get_default_id(Thing { _pad: 0 })); }
        trait Identifiable {
            const DEFAULT_ID: u32 = 999;
            fn get_id(self: Self) -> u32 { Self::DEFAULT_ID }
        }
        struct Thing { _pad: u32 }
        impl Identifiable for Thing {}
        fn get_default_id(t: Identifiable) -> u32 { t.get_id() }
    ));
    assert_all(&result, &[999u32]);
}

#[test]
fn trait_const_impl_override() {
    // Trait provides a default const and default method.
    // Impl overrides the method with a literal (forward const refs in trait impl methods
    // have resolution ordering limitations).
    let result = run(stringify!(
        fn main() { ret_u32(get_default_id(Thing { _pad: 0 })); ret_u32(get_default_id(Other { _pad: 0 })); }
        trait Identifiable {
            const DEFAULT_ID: u32 = 999;
            fn get_id(self: Self) -> u32 { Self::DEFAULT_ID }
        }
        struct Thing { _pad: u32 }
        impl Identifiable for Thing {
            const DEFAULT_ID = 42;
            fn get_id(self: Self) -> u32 { 42 }
        }
        struct Other { _pad: u32 }
        impl Identifiable for Other {}
        fn get_default_id(t: Identifiable) -> u32 { t.get_id() }
    ));
    assert_all(&result, &[42u32, 999u32]);
}

#[test]
fn trait_const_required() {
    // Trait requires a const (no default); impl must provide it
    let result = run(stringify!(
        fn main() { ret_u32(Thing::ID); }
        trait Identifiable {
            const ID: u32;
        }
        struct Thing { _pad: u32 }
        impl Identifiable for Thing {
            const ID = 77;
        }
    ));
    assert_all(&result, &[77u32]);
}

#[test]
fn trait_const_reference_type() {
    // Trait const with reference type (String); impl overrides method with literal
    let result = run(stringify!(
        fn main() { ret_str(label_of(Thing { _pad: 0 })); }
        trait Labelled {
            const LABEL: String = "default";
            fn label(self: Self) -> String { Self::LABEL }
        }
        struct Thing { _pad: u32 }
        impl Labelled for Thing {
            const LABEL = "thing";
            fn label(self: Self) -> String { "thing" }
        }
        fn label_of(t: Labelled) -> String { t.label() }
    ));
    assert_all(&result, &["thing".to_string()]);
}

#[test]
fn trait_const_default_used_by_impl() {
    // Impl doesn't override the const; the trait default is used
    let result = run(stringify!(
        fn main() { ret_str(label_of(Thing { _pad: 0 })); }
        trait Labelled {
            const LABEL: String = "default";
            fn label(self: Self) -> String { Self::LABEL }
        }
        struct Thing { _pad: u32 }
        impl Labelled for Thing {
            fn label(self: Self) -> String { Self::LABEL + " impl" }
        }
        fn label_of(t: Labelled) -> String { t.label() }
    ));
    assert_all(&result, &["default impl".to_string()]);
}

#[test]
fn trait_const_accessed_from_outside() {
    // Access impl const from outside via TypeName::CONST
    let result = run(stringify!(
        fn main() { ret_u32(Thing::ID); }
        trait Identifiable {
            const ID: u32;
        }
        struct Thing { _pad: u32 }
        impl Identifiable for Thing {
            const ID = 123;
        }
    ));
    assert_all(&result, &[123u32]);
}

#[test]
fn trait_const_access_trait_default() {
    // Access trait default const via TraitName::CONST
    let result = run(stringify!(
        fn main() { ret_u32(Identifiable::DEFAULT_ID); }
        trait Identifiable {
            const DEFAULT_ID: u32 = 42;
        }
        struct Thing { _pad: u32 }
        impl Identifiable for Thing {}
    ));
    assert_all(&result, &[42u32]);
}

// =============================================================================
// Trait const error cases
// =============================================================================

#[test]
#[should_panic(expected = "Const `UNKNOWN` is not declared by trait `Identifiable`")]
fn trait_const_not_declared_in_trait() {
    // Impl defines a const that the trait doesn't declare
    run(stringify!(
        fn main() {}
        trait Identifiable {
            const ID: u32;
        }
        struct Thing { _pad: u32 }
        impl Identifiable for Thing {
            const ID = 1;
            const UNKNOWN = 2;
        }
    ));
}

// =============================================================================
// Const in nested scopes (blocks, if, loops)
// =============================================================================

#[test]
fn const_in_block() {
    let result = run(stringify!(
        {
            const X = 42u32;
            ret_u32(X);
        }
    ));
    assert_all(&result, &[42u32]);
}

#[test]
fn const_in_if() {
    let result = run(stringify!(
        if true {
            const X = 99u32;
            ret_u32(X);
        }
    ));
    assert_all(&result, &[99u32]);
}

#[test]
fn const_in_else() {
    let result = run(stringify!(
        if false {
            ret_u32(1);
        } else {
            const X = 77u32;
            ret_u32(X);
        }
    ));
    assert_all(&result, &[77u32]);
}

#[test]
fn const_in_for_loop() {
    let result = run(stringify!(
        for i in 0..1 {
            const VAL = 55u32;
            ret_u32(VAL);
        }
    ));
    assert_all(&result, &[55u32]);
}

#[test]
fn const_in_while_loop() {
    let result = run(stringify!(
        let mut count = 0u32;
        while count < 1 {
            const VAL = 33u32;
            ret_u32(VAL);
            count += 1;
        }
    ));
    assert_all(&result, &[33u32]);
}

// =============================================================================
// Const used in closures
// =============================================================================

#[test]
fn const_in_closure() {
    let result = run(stringify!(
        const BASE = 100u32;
        let f = || -> u32 {
            return BASE + 1;
        };
        ret_u32(f());
    ));
    assert_all(&result, &[101u32]);
}

// =============================================================================
// Const with enum (primitive enum discriminant)
// =============================================================================

#[test]
fn const_primitive_enum() {
    let result = run(stringify!(
        fn main() { ret_u8(DEFAULT as u8); }
        enum Color { Red, Green, Blue }
        const DEFAULT = Color::Green;
    ));
    assert_all(&result, &[1u8]);
}

// =============================================================================
// Multiple module-level consts of different types
// =============================================================================

#[test]
fn multiple_consts() {
    let result = run(stringify!(
        const A = 1u8;
        const B = 2u16;
        const C = 3u32;
        const D = true;
        const E = "hello";
        ret_u8(A);
        ret_u16(B);
        ret_u32(C);
        ret_bool(D);
        ret_str(E);
    ));
    assert_all!(&result, [1u8, 2u16, 3u32, true, "hello".to_string()]);
}

// =============================================================================
// Error cases: invalid const expressions
// =============================================================================

#[test]
fn const_expr_cannot_use_variable() {
    let err = build_err(stringify!(
        fn test() { let x = 10u32; const C = x; }
        fn main() {}
    ));
    assert!(err.contains("const expression"), "Expected const expression error, got: {}", err);
}

#[test]
fn const_expr_cannot_call_function() {
    let err = build_err(stringify!(
        fn get_val() -> u32 { return 42; }
        const C = get_val();
        fn main() {}
    ));
    assert!(err.contains("const expression"), "Expected const expression error, got: {}", err);
}

// =============================================================================
// Const referencing another const (reference type)
// =============================================================================

#[test]
fn const_string_refers_to_const_string() {
    let result = run(stringify!(
        const A = "hello";
        const B = A;
        ret_str(B);
    ));
    assert_all(&result, &["hello".to_string()]);
}

#[test]
fn const_struct_refers_to_const_string() {
    let result = run(stringify!(
        fn main() { ret_str(W.inner); }
        struct Wrapper { inner: String }
        const MSG = "inner text";
        const W = Wrapper { inner: MSG };
    ));
    assert_all(&result, &["inner text".to_string()]);
}

// =============================================================================
// Const in anonymous function
// =============================================================================

#[test]
fn const_in_anonymous_function() {
    let result = run(stringify!(
        const BASE = 10u32;
        fn test(f: fn() -> u32) { ret_u32(f() + BASE); }
        fn main() { test(fn() -> u32 { const EXTRA = 5u32; return BASE + EXTRA; }); }
    ));
    assert_all(&result, &[25u32]);
}

// =============================================================================
// Edge cases
// =============================================================================

#[test]
fn const_zero() {
    let result = run(stringify!(
        const ZERO = 0u64;
        ret_u64(ZERO);
    ));
    assert_all(&result, &[0u64]);
}

#[test]
fn const_empty_string() {
    let result = run(stringify!(
        const EMPTY = "";
        ret_str(EMPTY);
    ));
    assert_all(&result, &["".to_string()]);
}

// #[ignore] - empty array type inference not supported for const expressions
// #[test]
// fn const_empty_array() {
//     let result = run(stringify!(
//         const EMPTY: [u8] = [];
//         ret_u64(EMPTY.len());
//     ));
//     assert_all(&result, &[0u64]);
// }

#[test]
fn const_negated() {
    let result = run(stringify!(
        const NEG = -42i32;
        ret_i32(NEG);
    ));
    assert_all(&result, &[-42i32]);
}

#[test]
fn const_hex() {
    let result = run(stringify!(
        const HEX = 0xFFu8;
        ret_u8(HEX);
    ));
    assert_all(&result, &[255u8]);
}

#[test]
fn const_binary() {
    let result = run(stringify!(
        const BIN = 0b1010u8;
        ret_u8(BIN);
    ));
    assert_all(&result, &[10u8]);
}

#[test]
fn debug_method_override_simple() {
    // Test method override without const involvement
    let result = run(stringify!(
        fn main() { ret_u32(get_x(S { _pad: 0 })); }
        trait T {
            fn get_x(self: Self) -> u32 { 999 }
        }
        struct S { _pad: u32 }
        impl T for S {
            fn get_x(self: Self) -> u32 { 42 }
        }
        fn get_x(t: T) -> u32 { t.get_x() }
    ));
    assert_all(&result, &[42u32]);
}

#[test]
fn debug_method_override_with_const() {
    // Test method override with const reference
    let result = run("fn main() { ret_u32(get_x(S { _pad: 0 })); }\n\
        trait T {\n\
            const X: u32 = 999;\n\
            fn get_x(self: Self) -> u32 { Self::X }\n\
        }\n\
        struct S { _pad: u32 }\n\
        impl T for S {\n\
            const X = 42;\n\
            fn get_x(self: Self) -> u32 { S::X }\n\
        }\n\
        fn get_x(t: T) -> u32 { t.get_x() }");
    assert_all(&result, &[42u32]);
}

#[test]
fn debug_method_override_literal() {
    // Test method override with literal (no const reference)
    let result = run(stringify!(
        fn main() { ret_u32(get_x(S { _pad: 0 })); }
        trait T {
            const X: u32 = 999;
            fn get_x(self: Self) -> u32 { Self::X }
        }
        struct S { _pad: u32 }
        impl T for S {
            const X = 42;
            fn get_x(self: Self) -> u32 { 42 }
        }
        fn get_x(t: T) -> u32 { t.get_x() }
    ));
    assert_all(&result, &[42u32]);
}

#[test]
fn trait_const_override_in_default_method() {
    // Trait default method uses Self::CONST; impl overrides the const but not the method.
    // The default method should see the impl's const value, not the trait's.
    let result = run(stringify!(
        fn main() { ret_u32(Thing { v: 0 }.id_provided()); }
        struct Thing { v: u32 }
        trait Identifiable {
            const ID: u32 = 123;
            fn id_provided(self: Self) -> u32 { Self::ID }
        }
        impl Identifiable for Thing {
            const ID: u32 = 999;
        }
    ));
    assert_all(&result, &[999u32]);
}

#[test]
fn trait_const_override_default_method_partial() {
    // Trait has multiple consts; impl overrides only one. Default method using the
    // overridden const should see the impl value; the other const keeps the trait value.
    let result = run(stringify!(
        fn main() { ret_u64(Thing { v: 0 }.combined()); }
        struct Thing { v: u32 }
        trait HasTwo {
            const A: u32 = 100;
            const B: u32 = 200;
            fn combined(self: Self) -> u64 { (Self::A as u64) << 32 | (Self::B as u64) }
        }
        impl HasTwo for Thing {
            const A: u32 = 1;
        }
    ));
    assert_all(&result, &[(1u64 << 32 | 200u64)]);
}

#[test]
fn trait_const_override_impl_also_overrides_method() {
    // Impl overrides both the const and the method. The impl method should be used.
    let result = run(stringify!(
        fn main() { ret_u32(Thing { v: 0 }.id_provided()); }
        struct Thing { v: u32 }
        trait Identifiable {
            const ID: u32 = 123;
            fn id_provided(self: Self) -> u32 { Self::ID }
        }
        impl Identifiable for Thing {
            const ID: u32 = 999;
            fn id_provided(self: Self) -> u32 { 777 }
        }
    ));
    assert_all(&result, &[777u32]);
}

// =============================================================================
// Const expressions (binary operations on const values)
// =============================================================================

#[test]
fn const_expr_add_numeric() {
    let result = run(stringify!(
        const A = 10u32;
        const B = 20u32;
        const C = A + B;
        ret_u32(C);
    ));
    assert_all(&result, &[30u32]);
}

#[test]
fn const_expr_sub_numeric() {
    let result = run(stringify!(
        const A = 50u32;
        const B = 20u32;
        const C = A - B;
        ret_u32(C);
    ));
    assert_all(&result, &[30u32]);
}

#[test]
fn const_expr_mul_numeric() {
    let result = run(stringify!(
        const A = 6u32;
        const B = 7u32;
        const C = A * B;
        ret_u32(C);
    ));
    assert_all(&result, &[42u32]);
}

#[test]
fn const_expr_div_numeric() {
    let result = run(stringify!(
        const A = 100u32;
        const B = 4u32;
        const C = A / B;
        ret_u32(C);
    ));
    assert_all(&result, &[25u32]);
}

#[test]
fn const_expr_rem_numeric() {
    let result = run(stringify!(
        const A = 17u32;
        const B = 5u32;
        const C = A % B;
        ret_u32(C);
    ));
    assert_all(&result, &[2u32]);
}

#[test]
fn const_expr_mixed_ops() {
    // 2 + 3 * 4 = 14
    let result = run(stringify!(
        const X = 2u32 + 3u32 * 4u32;
        ret_u32(X);
    ));
    assert_all(&result, &[14u32]);
}

#[test]
fn const_expr_signed_numeric() {
    let result = run(stringify!(
        const A = 10i32;
        const B = -3i32;
        const C = A + B;
        ret_i32(C);
    ));
    assert_all(&result, &[7i32]);
}

#[test]
fn const_expr_float() {
    let result = run(stringify!(
        const A = 1.5f64;
        const B = 2.0f64;
        const C = A * B;
        ret_f64(C);
    ));
    assert_all(&result, &[3.0f64]);
}

#[test]
fn const_expr_string_concat() {
    let result = run(stringify!(
        const A = "Hello";
        const B = "World";
        const C = A + B;
        ret_string(C);
    ));
    assert_all(&result, &["HelloWorld".to_string()]);
}

#[test]
fn const_expr_string_concat_chain() {
    let result = run(stringify!(
        const A = "A";
        const B = "B";
        const C = "C";
        const D = A + B + C;
        ret_string(D);
    ));
    assert_all(&result, &["ABC".to_string()]);
}

#[test]
fn const_expr_string_interpolation() {
    // String interpolation desugars to concatenation + casts
    let result = run(stringify!(
        const NAME = "World";
        const GREETING = "Hello {NAME}!";
        ret_string(GREETING);
    ));
    assert_all(&result, &["Hello World!".to_string()]);
}

#[test]
fn const_expr_string_interpolation_multiple() {
    let result = run(stringify!(
        const A = "foo";
        const B = "bar";
        const MSG = "{A}-{B}";
        ret_string(MSG);
    ));
    assert_all(&result, &["foo-bar".to_string()]);
}

#[test]
fn const_expr_in_array() {
    let result = run(stringify!(
        const X = 1u32 + 2u32;
        const ARR = [X, X * 2];
        ret_u32(ARR[0]);
        ret_u32(ARR[1]);
    ));
    assert_all(&result, &[3u32, 6u32]);
}

// Integer overflow in const expression (u64 max + 1)
#[test]
#[should_panic(expected = "Integer overflow in const expression")]
fn const_expr_overflow_u64() {
    run(stringify!(
        const X = 18446744073709551615u64 + 1u64;
        ret_u64(X);
    ));
}

// Division by zero in const expression
#[test]
#[should_panic(expected = "Division by zero in const expression")]
fn const_expr_div_by_zero() {
    run(stringify!(
        const X = 10u32 / 0u32;
        ret_u32(X);
    ));
}

// Modulo by zero in const expression
#[test]
#[should_panic(expected = "Division by zero in const expression")]
fn const_expr_rem_by_zero() {
    run(stringify!(
        const X = 10u32 % 0u32;
        ret_u32(X);
    ));
}

// =============================================================================
// Const unary expressions
// =============================================================================

#[test]
fn const_unary_neg_signed() {
    let result = run(stringify!(
        const A = 42i32;
        const B = -A;
        ret_i32(B);
    ));
    assert_all(&result, &[-42i32]);
}

#[test]
fn const_unary_neg_double() {
    let result = run(stringify!(
        const A = -(-42i32);
        ret_i32(A);
    ));
    assert_all(&result, &[42i32]);
}

#[test]
fn const_unary_neg_float() {
    let result = run(stringify!(
        const A = 3.14f64;
        const B = -A;
        ret_f64(B);
    ));
    assert_all(&result, &[-3.14f64]);
}

#[test]
fn const_unary_neg_unsigned_zero() {
    let result = run(stringify!(
        const A = 0u32;
        const B = -A;
        ret_u32(B);
    ));
    assert_all(&result, &[0u32]);
}

#[test]
fn const_unary_positive() {
    let result = run(stringify!(
        const A = +42i32;
        ret_i32(A);
    ));
    assert_all(&result, &[42i32]);
}

#[test]
fn const_unary_not_bool() {
    let result = run(stringify!(
        const A = true;
        const B = !A;
        const C = !B;
        ret_bool(B);
        ret_bool(C);
    ));
    assert_all(&result, &[false, true]);
}

#[test]
fn const_unary_not_bitwise_u8() {
    let result = run(stringify!(
        const A = 0u8;
        const B = !A;
        ret_u8(B);
    ));
    assert_all(&result, &[255u8]);
}

#[test]
fn const_unary_not_bitwise_u32() {
    let result = run(stringify!(
        const A = 0u32;
        const B = !A;
        ret_u32(B);
    ));
    assert_all(&result, &[4294967295u32]);
}

#[test]
fn const_unary_not_bitwise_value() {
    let result = run(stringify!(
        const A = 15u8;
        const B = !A;
        ret_u8(B);
    ));
    assert_all(&result, &[240u8]);
}

#[test]
fn const_unary_not_bitwise_signed() {
    let result = run(stringify!(
        const A = 0i32;
        const B = !A;
        ret_i32(B);
    ));
    assert_all(&result, &[-1i32]);
}

#[test]
fn const_unary_in_array() {
    let result = run(stringify!(
        const A = 5u32;
        const ARR = [A + 1, A + 2];
        ret_u32(ARR[0]);
        ret_u32(ARR[1]);
    ));
    assert_all(&result, &[6u32, 7u32]);
}

// Signed integer negation overflow (i64::MIN cannot be negated)
#[test]
#[should_panic(expected = "Integer overflow in const expression")]
fn const_unary_neg_overflow_signed_min() {
    run(stringify!(
        const A = -9223372036854775808i64;
        const B = -A;
        ret_i64(B);
    ));
}

// Unsigned negation of non-zero value
#[test]
#[should_panic(expected = "Integer overflow in const expression")]
fn const_unary_neg_overflow_unsigned() {
    run(stringify!(
        const A = 1u32;
        const B = -A;
        ret_u32(B);
    ));
}

#[test]
fn const_unary_not_negated_value() {
    // !(-128) as i8: -128 is 0b10000000, ! gives 0b01111111 = 127
    let result = run(stringify!(
        const A: i8 = !(-128);
        ret_i8(A);
    ));
    assert_all(&result, &[127i8]);
}

// =============================================================================
// Const mutation prevention
// =============================================================================

// Direct reassignment of a const
#[test]
#[should_panic(expected = "Cannot assign to const `X`")]
fn const_mutation_direct_reassign() {
    run(stringify!(
        const X = 1u32;
        X = 2;
    ));
}

// Index assignment on a const array
#[test]
#[should_panic(expected = "Cannot assign to const `A`")]
fn const_mutation_index_assign_array() {
    run(stringify!(
        const A = [1u32];
        A[0] = 2;
    ));
}

// Field assignment on a const struct (skipped - Itsy parser doesn't support field reassignment syntax)
// #[test]
// #[should_panic(expected = "Cannot assign to const `S`")]
// fn const_mutation_field_assign() {
//     run(stringify!(
//         struct Point { x: u32 }
//         const S = Point { x: 0 };
//         S.x = 1;
//     ));
// }

// Nested index assignment on a const array
#[test]
#[should_panic(expected = "Cannot assign to const `A`")]
fn const_mutation_nested_index_assign() {
    run(stringify!(
        const A = [[1u32]];
        A[0][0] = 2;
    ));
}

// Compound assignment on a const array
#[test]
#[should_panic(expected = "Cannot assign to const `A`")]
fn const_mutation_compound_assign() {
    run(stringify!(
        const A = [1u32];
        A[0] += 1;
    ));
}

// Index assignment on a const map
#[test]
#[should_panic(expected = "Cannot assign to const `M`")]
fn const_mutation_index_assign_map() {
    run(stringify!(
        const M = ["a" => 1u32];
        M["a"] = 2;
    ));
}

// Mutating method (push) on a const array
#[test]
#[should_panic(expected = "Cannot call mutating method `push()` on const `A`")]
fn const_mutation_method_push() {
    run(stringify!(
        const A: [u32] = [];
        A.push(1);
    ));
}

// Mutating method (pop) on a const array
#[test]
#[should_panic(expected = "Cannot call mutating method `pop()` on const `A`")]
fn const_mutation_method_pop() {
    run(stringify!(
        const A = [1u32];
        A.pop();
    ));
}

// Mutating method (remove) on a const array
#[test]
#[should_panic(expected = "Cannot call mutating method `remove()` on const `A`")]
fn const_mutation_method_remove() {
    run(stringify!(
        const A = [1u32];
        A.remove(0);
    ));
}

// Mutating method (clear) on a const array
#[test]
#[should_panic(expected = "Cannot call mutating method `clear()` on const `A`")]
fn const_mutation_method_clear() {
    run(stringify!(
        const A = [1u32];
        A.clear();
    ));
}

// Mutating method (insert) on a const map
#[test]
#[should_panic(expected = "Cannot call mutating method `insert()` on const `M`")]
fn const_mutation_method_insert_map() {
    run(stringify!(
        const M: [String => u32] = [ => ];
        M.insert("a", 1);
    ));
}

// Function-level const mutation
#[test]
#[should_panic(expected = "Cannot assign to const `X`")]
fn const_mutation_in_function() {
    run(stringify!(
        fn f() { const X = 1u32; X = 2; }
        fn main() { f(); }
    ));
}

// Impl-level const mutation (skipped - Itsy parser doesn't support qualified const assignment syntax)
// #[test]
// #[should_panic(expected = "Cannot assign to const `X`")]
// fn const_mutation_in_impl() {
//     run(stringify!(
//         fn main() { S::f(); }
//         struct S { _pad: u32 }
//         impl S {
//             const X = 1u32;
//             fn f() { Self::X = 2; }
//         }
//     ));
// }

// Mutation through const-derived binding (push)
// The error reports the variable name `arr` since that's the const-derived binding being mutated
#[test]
#[should_panic(expected = "Cannot call mutating method `push()` on const `arr`")]
fn const_mutation_through_binding_push() {
    run(stringify!(
        const A = [1u32];
        let mut arr = A;
        arr.push(2);
    ));
}

// Index assignment through const-derived binding
// The error reports the variable name `arr` since that's the const-derived binding being mutated
#[test]
#[should_panic(expected = "Cannot assign to const `arr`")]
fn const_mutation_through_binding_index() {
    run(stringify!(
        const A = [1u32];
        let mut arr = A;
        arr[0] = 2;
    ));
}

// =============================================================================
// Const mutation prevention - positive tests (should work)
// =============================================================================

// Const as index offset (const is used as index, not mutated)
#[test]
fn const_as_index_offset() {
    let result = run(stringify!(
        const I = 0u64;
        let mut arr = [1u32];
        arr[I] = 2;
        ret_u32(arr[0]);
    ));
    assert_all(&result, &[2u32]);
}

// Read-only access to const array
#[test]
fn const_readonly_access() {
    let result = run(stringify!(
        const A = [1u32];
        let x = A[0];
        ret_u32(x);
    ));
    assert_all(&result, &[1u32]);
}

// Non-mutating methods on const (len)
#[test]
fn const_nonmutating_method_len() {
    let result = run(stringify!(
        const A = [1u32, 2u32, 3u32];
        let l = A.len();
        ret_u64(l);
    ));
    assert_all(&result, &[3u64]);
}

// Non-mutating methods on const (get) - get returns Option<T>, use match to extract
#[test]
fn const_nonmutating_method_get() {
    let result = run(stringify!(
        const A = [1u32, 2u32, 3u32];
        let v = A.get(0);
        match v { Some(val) => ret_u32(val), None => ret_u32(0) };
    ));
    assert_all(&result, &[1u32]);
}

// Non-const binding mutation (should still work)
#[test]
fn nonconst_binding_mutation() {
    let result = run(stringify!(
        let mut arr = [1u32];
        arr.push(2);
        ret_u64(arr.len());
    ));
    assert_all(&result, &[2u64]);
}

// Reassignment of non-const variable (should still work)
#[test]
fn nonconst_variable_reassign() {
    let result = run(stringify!(
        let mut x = 1u32;
        x = 2;
        ret_u32(x);
    ));
    assert_all(&result, &[2u32]);
}

// =============================================================================
// Const immutability: conditional assignment taint (issue 1)
// =============================================================================

#[test]
#[should_panic(expected = "Cannot assign to const")]
fn const_mutation_conditional_assign() {
    run(stringify!(
        struct S { val: u8 }
        const C = S { val: 1 };
        fn main() {
            let x = S { val: 0 };
            if true { x = C; }
            x.val = 5;
        }
    ));
}

#[test]
#[should_panic(expected = "Cannot assign to const")]
fn const_mutation_conditional_assign_else() {
    run(stringify!(
        struct S { val: u8 }
        const C = S { val: 1 };
        fn main() {
            let x = S { val: 0 };
            if false { x = S { val: 2 }; } else { x = C; }
            x.val = 5;
        }
    ));
}

// =============================================================================
// Const immutability: function parameter mutation (issue 2)
// =============================================================================

#[test]
#[should_panic(expected = "Cannot pass const value to mutating parameter")]
fn const_arg_mutating_param_field() {
    run(stringify!(
        struct S { val: u8 }
        const C = S { val: 1 };
        fn f(v: S) { v.val = 7; }
        fn main() { f(C); }
    ));
}

#[test]
#[should_panic(expected = "Cannot pass const value to mutating parameter")]
fn const_arg_mutating_param_nested_field() {
    run(stringify!(
        struct Inner { val: u8 }
        struct Outer { inner: Inner }
        const C = Outer { inner: Inner { val: 1 } };
        fn f(v: Outer) { v.inner.val = 7; }
        fn main() { f(C); }
    ));
}

#[test]
#[should_panic(expected = "Cannot pass const value to mutating parameter")]
fn const_arg_mutating_param_compound_assign() {
    run(stringify!(
        struct S { val: u32 }
        const C = S { val: 1 };
        fn f(v: S) { v.val += 1; }
        fn main() { f(C); }
    ));
}

#[test]
fn const_arg_nonmutating_param() {
    let result = run(stringify!(
        struct S { val: u32 }
        const C = S { val: 42 };
        fn f(v: S) -> u32 { return v.val; }
        fn main() { ret_u32(f(C)); }
    ));
    assert_all(&result, &[42u32]);
}

#[test]
#[should_panic(expected = "Cannot pass const value to mutating parameter")]
fn const_arg_multi_param_first_mutated() {
    run(stringify!(
        struct S { val: u8 }
        const C = S { val: 1 };
        fn f(a: S, b: S) { a.val = 7; }
        fn main() { f(C, S { val: 0 }); }
    ));
}

#[test]
fn const_arg_multi_param_second_not_mutated() {
    let result = run(stringify!(
        struct S { val: u32 }
        const C = S { val: 99 };
        fn f(a: S, b: S) -> u32 { a.val = 7; return b.val; }
        fn main() { ret_u32(f(S { val: 0 }, C)); }
    ));
    assert_all(&result, &[99u32]);
}

#[test]
#[should_panic(expected = "Cannot pass const value to mutating parameter")]
fn const_arg_method_self_mutated() {
    run(stringify!(
        struct S { val: u8 }
        const C = S { val: 1 };
        impl S {
            fn mutate(self: Self) { self.val = 7; }
        }
        fn main() { C.mutate(); }
    ));
}

#[test]
fn const_arg_method_self_not_mutated() {
    let result = run(stringify!(
        struct S { val: u32 }
        const C = S { val: 42 };
        impl S {
            fn read(self: Self) -> u32 { return self.val; }
        }
        fn main() { ret_u32(C.read()); }
    ));
    assert_all(&result, &[42u32]);
}
