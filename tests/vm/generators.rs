use crate::util::*;

// Milestone 2: the VM coroutine core. `Generator<V>` of primitive values can be constructed and driven
// manually via next()/value().
//
// Milestone 3: keyed generators (`Generator<K, V>`, key()) of primitive key/value types, plus for-loop
// iteration (`for v in g` and `for k, v in g`). Reference-typed yields remain deferred to milestone 5.

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

// --- milestone 3: keyed generators ---

#[test]
fn keyed_generator_manual_drive() {
    let result = run(stringify!(
        fn pairs() -> Generator<i32, i32> {
            yield 1, 10;
            yield 2, 20;
            yield 3, 30;
        }
        fn main() {
            let g = pairs();
            while g.next() {
                ret_i32(g.key());
                ret_i32(g.value());
            }
        }
    ));
    assert_all(&result, &[ 1i32, 10, 2, 20, 3, 30 ]);
}

#[test]
fn keyed_generator_mixed_sizes() {
    // a u8 key and an i64 value exercise key/value slots of differing, non-StackAddress sizes
    let result = run(stringify!(
        fn pairs() -> Generator<u8, i64> {
            yield 1u8, 1000i64;
            yield 255u8, -5i64;
        }
        fn main() {
            let g = pairs();
            while g.next() {
                ret_u8(g.key());
            }
        }
    ));
    assert_all(&result, &[ 1u8, 255 ]);
}

#[test]
fn keyed_generator_args_and_locals() {
    let result = run(stringify!(
        fn enumerate(start: i32, count: i32) -> Generator<i32, i32> {
            let mut i = 0;
            while i < count {
                yield i, start + i * 10;
                i += 1;
            }
        }
        fn main() {
            let g = enumerate(5, 3);
            while g.next() {
                ret_i32(g.key());
                ret_i32(g.value());
            }
        }
    ));
    assert_all(&result, &[ 0i32, 5, 1, 15, 2, 25 ]);
}

// --- milestone 3: for-loop iteration ---

#[test]
fn for_loop_value_generator_call() {
    let result = run(stringify!(
        fn count() -> Generator<i32> {
            yield 1;
            yield 2;
            yield 3;
        }
        fn main() {
            for v in count() {
                ret_i32(v);
            }
        }
    ));
    assert_all(&result, &[ 1i32, 2, 3 ]);
}

#[test]
fn for_loop_value_generator_variable() {
    let result = run(stringify!(
        fn count() -> Generator<i32> {
            yield 10;
            yield 20;
        }
        fn main() {
            let g = count();
            for v in g {
                ret_i32(v);
            }
        }
    ));
    assert_all(&result, &[ 10i32, 20 ]);
}

#[test]
fn for_loop_keyed_generator() {
    let result = run(stringify!(
        fn pairs() -> Generator<i32, i32> {
            yield 10, 100;
            yield 20, 200;
            yield 30, 300;
        }
        fn main() {
            for k, v in pairs() {
                ret_i32(k);
                ret_i32(v);
            }
        }
    ));
    assert_all(&result, &[ 10i32, 100, 20, 200, 30, 300 ]);
}

#[test]
fn for_loop_keyed_generator_key_only() {
    let result = run(stringify!(
        fn pairs() -> Generator<i32, i32> {
            yield 5, 50;
            yield 6, 60;
        }
        fn main() {
            for k, _ in pairs() {
                ret_i32(k);
            }
        }
    ));
    assert_all(&result, &[ 5i32, 6 ]);
}

#[test]
fn for_loop_break() {
    // break leaves the generator suspended; a primitive frame holds no nested refs, so no heap corruption
    let result = run(stringify!(
        fn count() -> Generator<i32> {
            yield 1;
            yield 2;
            yield 3;
            yield 4;
        }
        fn main() {
            for v in count() {
                if v == 3 { break; }
                ret_i32(v);
            }
        }
    ));
    assert_all(&result, &[ 1i32, 2 ]);
}

#[test]
fn for_loop_continue() {
    let result = run(stringify!(
        fn count() -> Generator<i32> {
            yield 1;
            yield 2;
            yield 3;
            yield 4;
        }
        fn main() {
            for v in count() {
                if v == 2 { continue; }
                ret_i32(v);
            }
        }
    ));
    assert_all(&result, &[ 1i32, 3, 4 ]);
}

#[test]
fn for_loop_nested_generators() {
    let result = run(stringify!(
        fn count(n: i32) -> Generator<i32> {
            let mut i = 0;
            while i < n {
                yield i;
                i += 1;
            }
        }
        fn main() {
            for a in count(2) {
                for b in count(3) {
                    ret_i32(a * 10 + b);
                }
            }
        }
    ));
    assert_all(&result, &[ 0i32, 1, 2, 10, 11, 12 ]);
}

#[test]
fn for_loop_empty_generator() {
    let result = run(stringify!(
        fn empty(go: bool) -> Generator<i32> {
            if go { yield 1; }
        }
        fn main() {
            let mut count = 0;
            for _v in empty(false) {
                count += 1;
            }
            ret_i32(count);
        }
    ));
    assert_all(&result, &[ 0i32 ]);
}

// --- resolver/compiler rejections (still enforced) ---

#[test]
fn reference_typed_key_not_yet_implemented() {
    // a String (reference-typed) key needs refcount handling in the generator object: milestone 5
    let err = build_err(stringify!(
        fn pairs() -> Generator<String, i32> {
            yield "a", 1;
        }
        fn main() { }
    ));
    assert!(err.contains("reference-typed"), "unexpected error: {}", err);
}

#[test]
fn value_only_generator_key_iteration_rejected() {
    // a `Generator<V>` yields values only and cannot be iterated by key
    let err = build_err(stringify!(
        fn count() -> Generator<i32> {
            yield 1;
        }
        fn main() {
            for k, v in count() {
                ret_i32(k);
                ret_i32(v);
            }
        }
    ));
    assert!(err.contains("key"), "unexpected error: {}", err);
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
