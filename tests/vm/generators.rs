use crate::util::*;

// Milestone 2: the VM coroutine core. `Generator<V>` of primitive values can be constructed and driven
// manually via next()/value(). Keyed generators (`Generator<K, V>`, key()) and reference-typed yields
// are deferred to milestone 3 and still report a clean "not yet implemented" compile error.

#[test]
fn value_generator_manual_drive() {
    let result = run(stringify!(
        fn count() -> Generator<i32> {
            yield 10;
            yield 20;
            yield 30;
        }
        fn main() {
            let g = count();
            while g.next() {
                ret_i32(g.value());
            }
        }
    ));
    assert_all(&result, &[ 10i32, 20, 30 ]);
}

#[test]
fn generator_with_args_and_locals() {
    let result = run(stringify!(
        fn range(start: i32, count: i32) -> Generator<i32> {
            let mut i = 0;
            while i < count {
                yield start + i;
                i += 1;
            }
        }
        fn main() {
            let g = range(100, 3);
            while g.next() {
                ret_i32(g.value());
            }
        }
    ));
    assert_all(&result, &[ 100i32, 101, 102 ]);
}

#[test]
fn generator_early_return() {
    let result = run(stringify!(
        fn nums(stop: bool) -> Generator<i32> {
            yield 1;
            if stop { return; }
            yield 2;
        }
        fn main() {
            let g = nums(true);
            while g.next() { ret_i32(g.value()); }
            let h = nums(false);
            while h.next() { ret_i32(h.value()); }
        }
    ));
    assert_all(&result, &[ 1i32, 1, 2 ]);
}

#[test]
fn generator_partial_consumption_no_leak() {
    // a generator left suspended when it goes out of scope must not corrupt the heap (its object is
    // freed at scope end; a primitive frame holds no nested references to leak)
    let result = run(stringify!(
        fn count() -> Generator<i32> {
            yield 1;
            yield 2;
            yield 3;
        }
        fn main() {
            let g = count();
            g.next();
            ret_i32(g.value());
        }
    ));
    assert_all(&result, &[ 1i32 ]);
}

#[test]
fn generators_interleaved() {
    // two independent generators driven alternately exercise the coroutine control stack
    let result = run(stringify!(
        fn count() -> Generator<i32> {
            yield 1;
            yield 2;
        }
        fn main() {
            let a = count();
            let b = count();
            a.next(); ret_i32(a.value());
            b.next(); ret_i32(b.value());
            a.next(); ret_i32(a.value());
            b.next(); ret_i32(b.value());
        }
    ));
    assert_all(&result, &[ 1i32, 1, 2, 2 ]);
}

#[test]
fn generator_calls_helper() {
    // an ordinary call inside a generator body fully returns before the yield, so its frame does not
    // become part of the suspended generator frame
    let result = run(stringify!(
        fn helper(x: i32) -> i32 { x * 2 }
        fn doubler() -> Generator<i32> {
            yield helper(5);
            yield helper(10);
        }
        fn main() {
            let g = doubler();
            while g.next() { ret_i32(g.value()); }
        }
    ));
    assert_all(&result, &[ 10i32, 20 ]);
}

#[test]
fn generator_u8_values() {
    // value sizes other than the StackAddress size are stored/read correctly
    let result = run(stringify!(
        fn bytes() -> Generator<u8> {
            yield 1u8;
            yield 255u8;
        }
        fn main() {
            let g = bytes();
            while g.next() { ret_u8(g.value()); }
        }
    ));
    assert_all(&result, &[ 1u8, 255 ]);
}

#[test]
fn generator_never_yields() {
    // a generator whose body yields nothing completes immediately: next() returns false at once
    let result = run(stringify!(
        fn empty(go: bool) -> Generator<i32> {
            if go { yield 1; }
        }
        fn main() {
            let g = empty(false);
            let mut count = 0;
            while g.next() { count += 1; }
            ret_i32(count);
        }
    ));
    assert_all(&result, &[ 0i32 ]);
}

// --- resolver/compiler rejections (still enforced) ---

#[test]
fn keyed_generator_not_yet_implemented() {
    // parses and resolves, but milestone 2 does not implement keyed-generator codegen
    let err = build_err(stringify!(
        fn pairs() -> Generator<String, i32> {
            yield "a", 1;
        }
        fn main() { }
    ));
    assert!(err.contains("not yet implemented") || err.contains("Not yet implemented"), "unexpected error: {}", err);
}

#[test]
fn yield_outside_generator_rejected() {
    let err = build_err(stringify!(
        fn not_a_gen() -> i32 {
            yield 1;
            0
        }
        fn main() { }
    ));
    assert!(err.contains("generator"), "unexpected error: {}", err);
}

#[test]
fn yield_value_type_mismatch_rejected() {
    let err = build_err(stringify!(
        fn g() -> Generator<i32> {
            yield "nope";
        }
        fn main() { }
    ));
    assert!(err.contains("i32") && err.contains("String"), "unexpected error: {}", err);
}

#[test]
fn return_with_value_in_generator_rejected() {
    let err = build_err(stringify!(
        fn g() -> Generator<i32> {
            yield 1;
            return 5;
        }
        fn main() { }
    ));
    assert!(err.contains("early exit"), "unexpected error: {}", err);
}
