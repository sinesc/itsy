//! Const definitions across module boundaries: const initializers that reference consts defined
//! in other modules (root, siblings, child), in the same module (including forward references),
//! and `itsy_api!` constants (qualified and via `use` imports at any module level).
//!
//! Regression tests for a resolver ICE where const initializers were evaluated eagerly on the
//! first resolution pass, before referenced consts in not-yet-processed modules were registered.

use crate::util::*;

fn files<'a>(src: &'a [(&'a str, &'a str)]) -> std::collections::BTreeMap<&'a str, &'a str> {
    src.iter().copied().collect()
}

// ---------------------------------------------------------------------------
// struct consts with fields that reference api consts
// ---------------------------------------------------------------------------

// root const struct with an api const field, `use` import at root.
#[test]
fn root_const_struct_api_field() {
    let result = run_modules(files(&[
        ("", r#"
            use TestFns::KEY_A;
            struct EntDef { a: u16, b: f32 }
            const D: EntDef = EntDef { a: KEY_A, b: 1.5 };
            fn main() {
                TestFns::ret_u16(D.a);
                TestFns::ret_f32(D.b);
            }
        "#),
    ]));
    assert_all!(&result, [10u16, 1.5f32]);
}

// child impl const struct, api const field referenced by bare name via an import at ROOT only.
#[test]
fn child_const_struct_api_field_root_import() {
    let result = run_modules(files(&[
        ("", r#"
            mod m;
            use TestFns::KEY_A;
            struct EntDef { a: u16, b: f32 }
            fn main() { TestFns::ret_u16(m::C::DEF.a); }
        "#),
        ("m", r#"
            struct C { v: u8 }
            impl C {
                const DEF: EntDef = EntDef { a: KEY_A, b: 1.5 };
            }
        "#),
    ]));
    assert(&result[0], 10u16);
}

// child impl const struct, api const field referenced with a fully qualified name, no imports.
#[test]
fn child_const_struct_api_field_qualified() {
    let result = run_modules(files(&[
        ("", r#"
            mod m;
            struct EntDef { a: u16, b: f32 }
            fn main() { TestFns::ret_u16(m::C::DEF.a); }
        "#),
        ("m", r#"
            struct C { v: u8 }
            impl C {
                const DEF: EntDef = EntDef { a: TestFns::KEY_A, b: 1.5 };
            }
        "#),
    ]));
    assert(&result[0], 10u16);
}

// child impl const struct whose field references a child-module const that itself is built from
// an api const (api const -> module const -> struct field chain).
#[test]
fn child_const_struct_local_const_chain() {
    let result = run_modules(files(&[
        ("", r#"
            mod m;
            struct EntDef { a: u16, b: f32 }
            fn main() { TestFns::ret_u16(m::C::DEF.a); }
        "#),
        ("m", r#"
            use TestFns::KEY_A;
            const LOCAL: u16 = KEY_A + 1;
            struct C { v: u8 }
            impl C {
                const DEF: EntDef = EntDef { a: LOCAL, b: 1.5 };
            }
        "#),
    ]));
    assert(&result[0], 11u16);
}

// ---------------------------------------------------------------------------
// value consts across module boundaries
// ---------------------------------------------------------------------------

// child const referencing a root-module const.
#[test]
fn child_value_const_refs_root_const() {
    let result = run_modules(files(&[
        ("", r#"
            mod m;
            const FOO: u16 = 5;
            fn main() { TestFns::ret_u16(m::BAR); }
        "#),
        ("m", r#"
            const BAR: u16 = FOO;
        "#),
    ]));
    assert(&result[0], 5u16);
}

// root const referencing a child-module const.
#[test]
fn root_value_const_refs_child_const() {
    let result = run_modules(files(&[
        ("", r#"
            mod m;
            const BAR: u16 = m::FOO + 1;
            fn main() { TestFns::ret_u16(BAR); }
        "#),
        ("m", r#"
            const FOO: u16 = 5;
        "#),
    ]));
    assert(&result[0], 6u16);
}

// const in one module referencing a const in a sibling module.
#[test]
fn sibling_value_const_ref() {
    let result = run_modules(files(&[
        ("", r#"
            mod a;
            mod b;
            fn main() { TestFns::ret_u16(a::BAR); }
        "#),
        ("a", r#"
            const BAR: u16 = b::BAZ + 2;
        "#),
        ("b", r#"
            const BAZ: u16 = 7;
        "#),
    ]));
    assert(&result[0], 9u16);
}

// forward reference within the same module.
#[test]
fn same_module_forward_const_ref() {
    let result = run_modules(files(&[
        ("", r#"
            const BAR: u16 = FOO;
            const FOO: u16 = 5;
            fn main() { TestFns::ret_u16(BAR); }
        "#),
    ]));
    assert(&result[0], 5u16);
}

// child const referencing an api const imported at ROOT (bare name via ancestor alias).
#[test]
fn child_value_const_refs_api_const_root_import() {
    let result = run_modules(files(&[
        ("", r#"
            mod m;
            use TestFns::KEY_A;
            fn main() { TestFns::ret_u16(m::BAR); }
        "#),
        ("m", r#"
            const BAR: u16 = KEY_A;
        "#),
    ]));
    assert(&result[0], 10u16);
}

// child const referencing an api const by fully qualified name, no imports.
#[test]
fn child_value_const_refs_api_const_qualified() {
    let result = run_modules(files(&[
        ("", r#"
            mod m;
            fn main() { TestFns::ret_u16(m::BAR); }
        "#),
        ("m", r#"
            const BAR: u16 = TestFns::KEY_A;
        "#),
    ]));
    assert(&result[0], 10u16);
}

// ---------------------------------------------------------------------------
// error cases
// ---------------------------------------------------------------------------

// directly recursive const.
#[test]
fn recursive_const_rejected() {
    let err = build_modules_err(files(&[
        ("", r#"
            const FOO: u16 = FOO + 1;
            fn main() {}
        "#),
    ]));
    assert!(err.contains("Const `FOO` is (mutually) recursive"), "unexpected error: {}", err);
}

// mutually recursive consts.
#[test]
fn mutual_recursive_const_rejected() {
    let err = build_modules_err(files(&[
        ("", r#"
            mod m;
            fn main() {}
        "#),
        ("m", r#"
            const A: u16 = B + 1;
            const B: u16 = A + 1;
        "#),
    ]));
    assert!(err.contains("is (mutually) recursive"), "unexpected error: {}", err);
}

// undefined reference in a const initializer (must report the reference, not ICE).
#[test]
fn undefined_ref_in_const_initializer_rejected() {
    let err = build_modules_err(files(&[
        ("", r#"
            const BAR: u16 = NOPE;
            fn main() {}
        "#),
    ]));
    assert!(err.contains("Undefined identifier `NOPE`"), "unexpected error: {}", err);
}
