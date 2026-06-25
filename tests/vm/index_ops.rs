use crate::util::*;

// --- intrinsic `Index` trait: overloadable `[]` get/set on custom types ---

// Reusable Grid impl over a backing array, used by several tests below.
const GRID: &str = stringify!(
    struct Grid { data: [i64] }
    impl Index for Grid {
        fn get(self: Self, index: u64) -> i64 { self.data[index] }
        fn set(self: Self, index: u64, value: i64) { self.data[index] = value; }
    }
);

#[test]
fn index_read() {
    // `grid[i]` lowers to `grid.get(i)`.
    let result = run(&format!("{} {}", GRID, stringify!(
        fn main() {
            let grid = Grid { data: [10, 20, 30] };
            ret_i64(grid[1]);
        }
    )));
    assert_all(&result, &[ 20i64 ]);
}

#[test]
fn index_write() {
    // `grid[i] = v` lowers to `grid.set(i, v)`.
    let result = run(&format!("{} {}", GRID, stringify!(
        fn main() {
            let grid = Grid { data: [10, 20, 30] };
            grid[1] = 99;
            ret_i64(grid[1]);
        }
    )));
    assert_all(&result, &[ 99i64 ]);
}

#[test]
fn index_compound_assign() {
    // `grid[i] += v` reads via get, applies the operator, writes via set.
    let result = run(&format!("{} {}", GRID, stringify!(
        fn main() {
            let grid = Grid { data: [10, 20, 30] };
            grid[1] += 5;
            grid[2] *= 2;
            ret_i64(grid[1]);
            ret_i64(grid[2]);
        }
    )));
    assert_all(&result, &[ 25i64, 60i64 ]);
}

#[test]
fn index_compound_evaluates_index_once() {
    // `a[i] += v` must evaluate the index `i` exactly once (a desugaring to
    // `a.set(i, a.get(i) + v)` that re-evaluated `i` would advance a side-effecting index twice).
    // `idx()` pushes a marker each call; a double evaluation would push it twice.
    let result = run(&format!("{} {}", GRID, stringify!(
        fn idx() -> u64 { ret_i64(2); 1 }
        fn main() {
            let grid = Grid { data: [10, 20, 30] };
            grid[idx()] += 5;
            ret_i64(grid[1]);
        }
    )));
    assert_all(&result, &[ 2i64, 25i64 ]);
}

#[test]
fn index_option_read_bare_write() {
    // The map-like asymmetry: `get` returns `Option<T>` while `set` accepts bare `T`.
    let result = run(stringify!(
        struct Grid { data: [i64] }
        impl Index for Grid {
            fn get(self: Self, index: u64) -> Option<i64> { Some(self.data[index]) }
            fn set(self: Self, index: u64, value: i64) { self.data[index] = value; }
        }
        fn main() {
            let grid = Grid { data: [10, 20, 30] };
            grid[1] = 99;               // set: bare i64
            match grid[1] {             // get: Option<i64>
                Some(v) => ret_i64(v),
                None => ret_i64(-1),
            }
        }
    ));
    assert_all(&result, &[ 99i64 ]);
}

#[test]
fn index_compound_assign_mismatch_error() {
    // Compound assignment requires `get`'s return type to equal `set`'s value type. An
    // `Option<i64>` read against a bare `i64` write must be rejected with a clear message.
    let err = build_err(stringify!(
        struct Grid { data: [i64] }
        impl Index for Grid {
            fn get(self: Self, index: u64) -> Option<i64> { Some(self.data[index]) }
            fn set(self: Self, index: u64, value: i64) { self.data[index] = value; }
        }
        fn main() {
            let grid = Grid { data: [10, 20, 30] };
            grid[1] += 5;
        }
    ));
    assert!(err.contains("compound assignment through `[]`"), "unexpected error: {}", err);
}

#[test]
fn index_compound_assign_missing_operator_error() {
    // Compound assignment on a custom value type that does not implement the matching operator trait
    // (`Add`) must report the missing implementation rather than silently mis-compiling.
    let err = build_err(stringify!(
        struct V { n: i64 }
        struct Box { data: [V] }
        impl Index for Box {
            fn get(self: Self, index: u64) -> V { self.data[index] }
            fn set(self: Self, index: u64, value: V) { self.data[index] = value; }
        }
        fn main() {
            let b = Box { data: [ V { n: 1 } ] };
            b[0] += V { n: 10 };
        }
    ));
    assert!(err.contains("Add") || err.contains("does not implement"), "unexpected error: {}", err);
}

#[test]
fn index_missing_impl_error() {
    // Indexing a type without an `Index` impl gives the existing error.
    let err = build_err(stringify!(
        struct NoIndex { value: i64 }
        fn main() {
            let x = NoIndex { value: 1 };
            ret_i64(x[0]);
        }
    ));
    assert!(err.contains("does not implement index access"), "unexpected error: {}", err);
}

#[test]
fn index_string_key() {
    // The canonical map-like use: a String-keyed grid backed by a map. Exercises read, write, a
    // missing key, and compound assignment with a heap-allocated (String) index type.
    let result = run(stringify!(
        struct Grid { data: [String => i64] }
        impl Index for Grid {
            fn get(self: Self, index: String) -> i64 {
                match self.data.get(index) {
                    Some(v) => v,
                    None => -1,
                }
            }
            fn set(self: Self, index: String, value: i64) {
                self.data[index] = value;
            }
        }
        fn main() {
            let grid = Grid { data: [ => ] };
            grid["a1"] = 42;
            grid["b2"] = 7;
            ret_i64(grid["a1"]);
            ret_i64(grid["b2"]);
            ret_i64(grid["missing"]);
            grid["a1"] += 8;
            ret_i64(grid["a1"]);
        }
    ));
    assert_all(&result, &[ 42i64, 7i64, -1i64, 50i64 ]);
}

#[test]
fn index_string_key_and_value() {
    // Both the index and the value are heap-allocated (String): compound assignment must ref-count
    // the String key correctly as it flows through the single-evaluation temp bindings.
    let result = run(stringify!(
        struct Dict { data: [String => String] }
        impl Index for Dict {
            fn get(self: Self, index: String) -> String {
                match self.data.get(index) {
                    Some(v) => v,
                    None => "?",
                }
            }
            fn set(self: Self, index: String, value: String) {
                self.data[index] = value;
            }
        }
        fn main() {
            let d = Dict { data: [ => ] };
            d["greeting"] = "hello";
            ret_string(d["greeting"]);
            d["greeting"] += " world";
            ret_string(d["greeting"]);
            ret_string(d["nope"]);
        }
    ));
    assert_all(&result, &[ "hello".to_string(), "hello world".to_string(), "?".to_string() ]);
}

#[test]
fn index_compound_assign_heap_value() {
    // Compound assignment where the value type is heap-allocated (`String`): exercises the
    // built-in-operator refcounting path (operands collected as temporaries, result re-counted).
    let result = run(stringify!(
        struct Bag { data: [String] }
        impl Index for Bag {
            fn get(self: Self, index: u64) -> String { self.data[index] }
            fn set(self: Self, index: u64, value: String) { self.data[index] = value; }
        }
        fn main() {
            let bag = Bag { data: [ "a", "b" ] };
            bag[0] += "xyz";
            ret_string(bag[0]);
            ret_string(bag[1]);
        }
    ));
    assert_all(&result, &[ "axyz".to_string(), "b".to_string() ]);
}

#[test]
fn index_compound_assign_custom_operator_value() {
    // Compound assignment where the value type overloads `+` via the `Add` trait: exercises the
    // operator-method dispatch path (both operands incremented for the call, result re-counted).
    let result = run(stringify!(
        struct V { n: i64 }
        impl Add for V { fn add(self: Self, rhs: Self) -> Self { V { n: self.n + rhs.n } } }
        struct Box { data: [V] }
        impl Index for Box {
            fn get(self: Self, index: u64) -> V { self.data[index] }
            fn set(self: Self, index: u64, value: V) { self.data[index] = value; }
        }
        fn main() {
            let b = Box { data: [ V { n: 1 }, V { n: 2 } ] };
            b[0] += V { n: 10 };
            ret_i64(b[0].n);
            ret_i64(b[1].n);
        }
    ));
    assert_all(&result, &[ 11i64, 2i64 ]);
}

#[test]
fn index_nested_write_through_heap_reference() {
    // `grid[i][j] = v` where the outer `get` returns a non-primitive (heap reference) type: the
    // nested write must mutate the live inner object, not a copy.
    let result = run(stringify!(
        struct Row { data: [i64] }
        impl Index for Row {
            fn get(self: Self, index: u64) -> i64 { self.data[index] }
            fn set(self: Self, index: u64, value: i64) { self.data[index] = value; }
        }
        struct Grid { rows: [Row] }
        impl Index for Grid {
            fn get(self: Self, index: u64) -> Row { self.rows[index] }
            fn set(self: Self, index: u64, value: Row) { self.rows[index] = value; }
        }
        fn main() {
            let grid = Grid { rows: [ Row { data: [1, 2, 3] }, Row { data: [4, 5, 6] } ] };
            grid[0][1] = 99;
            ret_i64(grid[0][1]);
            ret_i64(grid[1][2]);
        }
    ));
    assert_all(&result, &[ 99i64, 6i64 ]);
}
