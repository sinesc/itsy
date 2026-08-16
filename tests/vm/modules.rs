use crate::util::*;

/// Convenience wrapper for the run_modules file map.
fn files<'a>(src: &'a [(&'a str, &'a str)]) -> std::collections::BTreeMap<&'a str, &'a str> {
    src.iter().copied().collect()
}

/// A submodule can reference root-level API functions via their fully qualified path.
#[test]
fn api_function_from_submodule() {
    let result = run_modules(files(&[
        ("", "mod sub; fn main() { sub::f(); }"),
        ("sub", "fn f() { TestFns::ret_string(\"hello\"); }"),
    ]));
    assert_all(&result, &[String::from("hello")]);
}

/// A submodule can call a function defined in the root module.
#[test]
fn root_function_from_submodule() {
    let result = run_modules(files(&[
        ("", "mod sub; fn helper() { TestFns::ret_u8(42); } fn main() { sub::f(); }"),
        ("sub", "fn f() { helper(); }"),
    ]));
    assert_all(&result, &[42u8]);
}

/// A `use` import in a submodule binds the bare name within that module.
#[test]
fn use_import_in_submodule() {
    let result = run_modules(files(&[
        ("", "mod sub; fn main() { sub::f(); }"),
        ("sub", "use TestFns::{ ret_u16 }; fn f() { ret_u16(7); }"),
    ]));
    assert_all(&result, &[7u16]);
}

/// Items defined in a submodule still resolve locally within the module and by qualified
/// path from the root (structs, enums, functions).
#[test]
fn local_items_in_submodule() {
    let result = run_modules(files(&[
        ("", "mod sub; fn main() { sub::f(); let p = sub::Point { x: 3 }; TestFns::ret_u8(p.x); }"),
        ("sub", "struct Point { x: u8 } enum Color { Red, Green } fn g() -> u8 { 5 } fn f() { TestFns::ret_u8(g()); TestFns::ret_bool(match Color::Red { Color::Red => true, Color::Green => false }); }"),
    ]));
    assert_all!(&result, [ 5u8, true, 3u8 ]);
}

/// A name defined at the top level of the current module shadows a same-named item in the root.
#[test]
fn local_name_shadows_root_name() {
    let result = run_modules(files(&[
        ("", "mod sub; fn pick() -> u8 { 1 } fn main() { sub::f(); }"),
        ("sub", "fn pick() -> u8 { 2 } fn f() { TestFns::ret_u8(pick()); }"),
    ]));
    assert_all(&result, &[2u8]);
}

/// A module can be imported via `use` and its items referenced through the import.
#[test]
fn use_module_import() {
    let result = run_modules(files(&[
        ("", "mod sub; use sub; fn main() { sub::f(); }"),
        ("sub", "fn f() { TestFns::ret_bool(true); }"),
    ]));
    assert_all(&result, &[true]);
}

/// Nested modules: items resolve along the module path chain up to root.
#[test]
fn nested_modules() {
    let result = run_modules(files(&[
        ("", "mod outer; fn helper() { TestFns::ret_bool(true); } fn main() { outer::f(); }"),
        ("outer", "mod inner; fn f() { inner::g(); }"),
        ("outer::inner", "fn g() { helper(); }"),
    ]));
    assert_all(&result, &[true]);
}

/// A submodule's types can be used in the root module's signatures and literals.
#[test]
fn submodule_type_in_root() {
    let result = run_modules(files(&[
        ("", "mod sub; fn main() { TestFns::ret_u8(sub::f()); }"),
        ("sub", "struct S { x: u8 } fn f() -> u8 { let s = S { x: 9 }; s.x }"),
    ]));
    assert_all(&result, &[9u8]);
}

/// `Self::CONST` and `Type::CONST` resolve for impl blocks defined in a submodule.
#[test]
fn impl_consts_in_submodule() {
    let result = run_modules(files(&[
        ("", "mod sub; fn main() { TestFns::ret_u8(sub::C::f()); TestFns::ret_u8(sub::C::X); }"),
        ("sub", "struct C {} impl C { const X: u8 = 3; fn f() -> u8 { Self::X } }"),
    ]));
    assert_all(&result, &[3u8, 3u8]);
}

/// Methods on submodule types resolve and dispatch (e.g. `ToString`-style impls in modules).
#[test]
fn methods_in_submodule() {
    let result = run_modules(files(&[
        ("", "mod sub; fn main() { let s = sub::make(); TestFns::ret_u8(s.doubled()); }"),
        ("sub", "struct N { v: u8 } impl N { fn doubled(self: Self) -> u8 { self.v + self.v } } fn make() -> N { N { v: 21 } }"),
    ]));
    assert_all(&result, &[42u8]);
}

/// Enum variants of submodule enums work in match patterns from the root module.
#[test]
fn submodule_enum_variant_patterns() {
    let result = run_modules(files(&[
        ("", "mod sub; fn main() { let c = sub::Color::Green; match c { sub::Color::Red => TestFns::ret_u8(1), sub::Color::Green => TestFns::ret_u8(2), _ => TestFns::ret_u8(3) } }"),
        ("sub", "enum Color { Red, Green, Blue }"),
    ]));
    assert_all(&result, &[2u8]);
}

/// A submodule can use a type from the root module.
#[test]
fn root_type_from_submodule() {
    let result = run_modules(files(&[
        ("", "mod sub; struct P { x: u8 } fn main() { TestFns::ret_u8(sub::f()); }"),
        ("sub", "fn f() -> u8 { let p = P { x: 6 }; p.x }"),
    ]));
    assert_all(&result, &[6u8]);
}

/// Unknown identifiers in a submodule still produce a proper error.
#[test]
fn undefined_identifier_in_submodule() {
    let err = build_modules_err(files(&[
        ("", "mod sub; fn main() { sub::f(); }"),
        ("sub", "fn f() { nosuch(); }"),
    ]));
    assert!(err.contains("nosuch"), "unexpected error: {}", err);
}
