use crate::util::*;

// Milestone 2: the VM coroutine core. `Generator<V>` of primitive values can be constructed and driven
// manually via next()/value().
//
// Milestone 3: keyed generators (`Generator<K, V>`, key()) of primitive key/value types, plus for-loop
// iteration (`for v in g` and `for k, v in g`). Reference-typed yields remain deferred to milestone 5.
//
// Milestone 4: drop-cleanup of unfinished generators. A generator dropped while NotStarted or Suspended
// releases the heap references held by its frozen frame (ref-typed args/locals) via the per-suspension
// live-ref-map. The `run` helper turns any leak into a HeapCorruption panic and any double-free into a
// refcount-underflow panic, so these tests fail loudly on a regression even without explicit assertions.

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

// --- Milestone 4: drop-cleanup of unfinished generators ---

#[test]
fn drop_suspended_releases_ref_local() {
    // a generator holding a ref-typed local, partially consumed then dropped while suspended: the frozen
    // frame's array must be released (otherwise HeapCorruption fires at end of run).
    let result = run(stringify!(
        fn gen() -> Generator<i32> {
            let arr = [10, 20, 30];
            let mut i = 0;
            while i < 3 { yield arr[i]; i += 1; }
        }
        fn main() {
            let g = gen();
            g.next();
            ret_i32(g.value());
            // g dropped here while suspended; arr must be released
        }
    ));
    assert_all(&result, &[ 10i32 ]);
}

#[test]
fn drop_not_started_releases_ref_args() {
    // a generator constructed with ref-typed arguments but never started: the captured array must be
    // released when the generator is dropped.
    let result = run(stringify!(
        fn gen(data: [i32]) -> Generator<i32> {
            let mut i = 0;
            while i < 3 { yield data[i]; i += 1; }
        }
        fn main() {
            let g = gen([7, 8, 9]);
            // never started; the captured array must be released on drop
        }
    ));
    assert_all(&result, &[] as &[ i32 ]);
}

#[test]
fn for_loop_break_releases_ref_local() {
    // breaking out of a for-loop leaves the generator suspended mid-iteration; its ref-typed local must
    // still be released.
    let result = run(stringify!(
        fn gen() -> Generator<i32> {
            let arr = [10, 20, 30, 40];
            let mut i = 0;
            while i < 4 { yield arr[i]; i += 1; }
        }
        fn main() {
            for v in gen() {
                ret_i32(v);
                if v == 20 { break; }
            }
        }
    ));
    assert_all(&result, &[ 10i32, 20 ]);
}

#[test]
fn partial_consumption_releases_ref_arg_and_local() {
    // both a ref-typed argument and a ref-typed local are live across the suspension point at which the
    // generator is dropped; both must be released.
    let result = run(stringify!(
        fn gen(prefix: [i32]) -> Generator<i32> {
            let extra = [100, 200];
            yield prefix[0];
            yield extra[0];
            yield extra[1];
        }
        fn main() {
            let g = gen([1, 2]);
            g.next();
            ret_i32(g.value());
            g.next();
            ret_i32(g.value());
            // dropped after two of three yields; prefix and extra must be released
        }
    ));
    assert_all(&result, &[ 1i32, 100 ]);
}

#[test]
fn full_consumption_with_refs_no_double_free() {
    // the happy path with ref-typed arg and local: the body's normal scope destructors release the
    // frame's refs, and the drop-cleanup must not release them a second time (no refcount underflow).
    let result = run(stringify!(
        fn gen(prefix: [i32]) -> Generator<i32> {
            let extra = [100, 200];
            yield prefix[0];
            yield extra[0];
            yield extra[1];
        }
        fn main() {
            let g = gen([1, 2]);
            let mut sum = 0;
            while g.next() { sum += g.value(); }
            ret_i32(sum);
        }
    ));
    assert_all(&result, &[ 301i32 ]);
}

#[test]
fn drop_maybe_initialized_ref_local_present() {
    // a ref-typed local initialized on only one branch is MaybeInitialized at the suspension point. When
    // the branch ran, the live-ref-map's runtime null-check sees a real reference and releases it.
    let result = run(stringify!(
        fn gen(flag: bool) -> Generator<i32> {
            let arr;
            if flag { arr = [1, 2, 3]; }
            yield 7;
            yield 8;
        }
        fn main() {
            let g = gen(true);
            g.next();
            ret_i32(g.value());
        }
    ));
    assert_all(&result, &[ 7i32 ]);
}

#[test]
fn drop_maybe_initialized_ref_local_absent() {
    // same generator, but the initializing branch did not run: the slot holds a null reference and the
    // drop-cleanup must skip it rather than touch heap object 0.
    let result = run(stringify!(
        fn gen(flag: bool) -> Generator<i32> {
            let arr;
            if flag { arr = [1, 2, 3]; }
            yield 7;
            yield 8;
        }
        fn main() {
            let g = gen(false);
            g.next();
            ret_i32(g.value());
        }
    ));
    assert_all(&result, &[ 7i32 ]);
}

#[test]
fn drop_string_typed_local() {
    // strings are reference types too; a suspended generator holding one must release it on drop.
    let result = run(stringify!(
        fn gen() -> Generator<i32> {
            let s = "hello";
            yield s.len() as i32;
            yield 0;
        }
        fn main() {
            let g = gen();
            g.next();
            ret_i32(g.value());
        }
    ));
    assert_all(&result, &[ 5i32 ]);
}

#[test]
fn interleaved_generators_with_refs_partial() {
    // two independent generators each holding ref-typed locals, advanced out of order and both dropped
    // while suspended: each frame's refs must be released independently.
    let result = run(stringify!(
        fn gen(base: i32) -> Generator<i32> {
            let arr = [base, base + 1, base + 2];
            let mut i = 0;
            while i < 3 { yield arr[i]; i += 1; }
        }
        fn main() {
            let a = gen(10);
            let b = gen(20);
            a.next();
            ret_i32(a.value());
            b.next();
            ret_i32(b.value());
            a.next();
            ret_i32(a.value());
            // both dropped while suspended
        }
    ));
    assert_all(&result, &[ 10i32, 20, 11 ]);
}

#[test]
fn drop_nested_ref_local() {
    // a nested reference type (array of arrays) held across the suspension point must be released
    // recursively on drop.
    let result = run(stringify!(
        fn gen() -> Generator<i32> {
            let grid = [[1, 2], [3, 4]];
            yield grid[0][0];
            yield grid[1][1];
        }
        fn main() {
            let g = gen();
            g.next();
            ret_i32(g.value());
        }
    ));
    assert_all(&result, &[ 1i32 ]);
}

// --- Milestone 5: reference-typed yields ---
//
// The yielded value/key may now be a reference type (String, arrays, ...). The generator's header
// value/key slot owns a refcount on the yielded reference (replace semantics on each yield, released on
// completion or drop); value()/key() return a borrow. The `run` helper turns any leak into a
// HeapCorruption panic and any double-free into a refcount-underflow panic.

#[test]
fn string_value_generator_manual_drive() {
    let result = run(stringify!(
        fn names() -> Generator<String> {
            yield "alice";
            let local = "bob";
            yield local;
        }
        fn main() {
            let g = names();
            while g.next() { ret_string(g.value()); }
        }
    ));
    assert_all(&result, &[ "alice".to_string(), "bob".to_string() ]);
}

#[test]
fn array_value_generator_manual_drive() {
    let result = run(stringify!(
        fn rows() -> Generator<[i32]> {
            let mut i = 0;
            while i < 3 {
                yield [i, i * i];
                i += 1;
            }
        }
        fn main() {
            let g = rows();
            while g.next() {
                let row = g.value();
                ret_i32(row[0]);
                ret_i32(row[1]);
            }
        }
    ));
    assert_all(&result, &[ 0i32, 0, 1, 1, 2, 4 ]);
}

#[test]
fn keyed_string_value_generator() {
    // String key, i32 value; the string key's length is returned alongside the value so both are checked
    // through a single (i32) result sequence.
    let result = run(stringify!(
        fn scores() -> Generator<String, i32> {
            yield "aa", 1;
            yield "bbb", 2;
        }
        fn main() {
            let g = scores();
            while g.next() {
                ret_i32(g.key().len() as i32);
                ret_i32(g.value());
            }
        }
    ));
    assert_all(&result, &[ 2i32, 1, 3, 2 ]);
}

#[test]
fn keyed_ref_key_and_value_generator() {
    // both key and value are reference types
    let result = run(stringify!(
        fn pairs() -> Generator<String, [i32]> {
            yield "x", [10, 11];
            yield "yy", [20, 21];
        }
        fn main() {
            let g = pairs();
            while g.next() {
                ret_i32(g.key().len() as i32);
                let v = g.value();
                ret_i32(v[1]);
            }
        }
    ));
    assert_all(&result, &[ 1i32, 11, 2, 21 ]);
}

#[test]
fn for_loop_string_value_generator() {
    let result = run(stringify!(
        fn names() -> Generator<String> {
            yield "one";
            yield "two";
            yield "three";
        }
        fn main() {
            for v in names() { ret_string(v); }
        }
    ));
    assert_all(&result, &[ "one".to_string(), "two".to_string(), "three".to_string() ]);
}

#[test]
fn for_loop_array_value_generator() {
    let result = run(stringify!(
        fn rows() -> Generator<[i32]> {
            yield [1, 2];
            yield [3, 4];
        }
        fn main() {
            for row in rows() {
                ret_i32(row[0]);
                ret_i32(row[1]);
            }
        }
    ));
    assert_all(&result, &[ 1i32, 2, 3, 4 ]);
}

#[test]
fn for_loop_keyed_ref_generator() {
    let result = run(stringify!(
        fn pairs() -> Generator<String, [i32]> {
            yield "aa", [1];
            yield "bbb", [2];
        }
        fn main() {
            for k, v in pairs() {
                ret_i32(k.len() as i32);
                ret_i32(v[0]);
            }
        }
    ));
    assert_all(&result, &[ 2i32, 1, 3, 2 ]);
}

#[test]
fn for_loop_break_ref_value() {
    // breaking out leaves the generator suspended holding a ref value: heap-clean drop
    let result = run(stringify!(
        fn rows() -> Generator<[i32]> {
            let mut i = 0;
            while i < 5 { yield [i]; i += 1; }
        }
        fn main() {
            for row in rows() {
                ret_i32(row[0]);
                if row[0] == 1 { break; }
            }
        }
    ));
    assert_all(&result, &[ 0i32, 1 ]);
}

#[test]
fn drop_suspended_ref_value() {
    // partially consume a ref-yielding generator then drop it while suspended: the value slot and the
    // frozen frame's ref local must both be released.
    let result = run(stringify!(
        fn gen() -> Generator<[i32]> {
            let kept = [100, 200];
            yield kept;
            yield [1];
            yield [2];
        }
        fn main() {
            let g = gen();
            g.next();
            ret_i32(g.value()[0]);
            g.next();
            // dropped while suspended after two yields
        }
    ));
    assert_all(&result, &[ 100i32 ]);
}

#[test]
fn drop_not_started_ref_value_with_ref_arg() {
    // a ref-yielding generator with a ref-typed argument, never started: the captured argument is
    // released on drop (the value slot is still null).
    let result = run(stringify!(
        fn echo(data: [i32]) -> Generator<[i32]> {
            yield data;
            yield [0];
        }
        fn main() {
            let g = echo([1, 2, 3]);
            // never started
        }
    ));
    assert_all(&result, &[] as &[ i32 ]);
}

#[test]
fn full_consumption_ref_value_no_double_free() {
    // running a ref-yielding generator to completion: gen_return releases the last value slot, and the
    // body's scope destructors release the locals — neither may double-free.
    let result = run(stringify!(
        fn gen() -> Generator<[i32]> {
            let local = [9];
            yield local;
            yield [8];
            yield [7];
        }
        fn main() {
            let g = gen();
            let mut sum = 0;
            while g.next() { sum += g.value()[0]; }
            ret_i32(sum);
        }
    ));
    assert_all(&result, &[ 24i32 ]);
}

#[test]
fn reyield_same_ref_value() {
    // yielding the same reference repeatedly exercises the replace-semantics next==prev fast path; then
    // drop while suspended must release it exactly once.
    let result = run(stringify!(
        fn same() -> Generator<[i32]> {
            let shared = [7, 8];
            yield shared;
            yield shared;
            yield shared;
        }
        fn main() {
            let g = same();
            g.next();
            g.next();
            ret_i32(g.value()[1]);
        }
    ));
    assert_all(&result, &[ 8i32 ]);
}

#[test]
fn ref_value_passed_to_function() {
    // value() returns a borrow; passing it to a function increments it as an argument, so the value
    // survives the call and the generator's slot ownership stays balanced.
    let result = run(stringify!(
        fn one() -> Generator<[i32]> { yield [42, 43]; }
        fn first(a: [i32]) -> i32 { a[0] }
        fn main() {
            let g = one();
            g.next();
            ret_i32(first(g.value()));
        }
    ));
    assert_all(&result, &[ 42i32 ]);
}

#[test]
fn ref_value_returned_from_wrapper() {
    // returning value() out of an enclosing function: the return inc/dec dance plus the generator's
    // drop keep the array alive for the caller and free it exactly once.
    let result = run(stringify!(
        fn one() -> Generator<[i32]> { yield [5, 6]; }
        fn get() -> [i32] {
            let g = one();
            g.next();
            return g.value();
        }
        fn main() {
            let x = get();
            ret_i32(x[1]);
        }
    ));
    assert_all(&result, &[ 6i32 ]);
}
