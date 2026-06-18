use crate::util::*;

// --- binary arithmetic operators via operator traits ---

#[test]
fn add_struct() {
    let result = run(stringify!(
        struct V { x: i64, y: i64 }
        impl Add for V {
            fn add(self: Self, rhs: Self) -> Self {
                V { x: self.x + rhs.x, y: self.y + rhs.y }
            }
        }
        fn main() {
            let a = V { x: 1, y: 2 };
            let b = V { x: 10, y: 20 };
            let c = a + b;
            ret_i64(c.x);
            ret_i64(c.y);
        }
    ));
    assert_all(&result, &[ 11i64, 22 ]);
}

#[test]
fn all_operators_struct() {
    let result = run(stringify!(
        struct N { v: i64 }
        impl Add for N { fn add(self: Self, rhs: Self) -> Self { N { v: self.v + rhs.v } } }
        impl Sub for N { fn sub(self: Self, rhs: Self) -> Self { N { v: self.v - rhs.v } } }
        impl Mul for N { fn mul(self: Self, rhs: Self) -> Self { N { v: self.v * rhs.v } } }
        impl Div for N { fn div(self: Self, rhs: Self) -> Self { N { v: self.v / rhs.v } } }
        impl Rem for N { fn rem(self: Self, rhs: Self) -> Self { N { v: self.v % rhs.v } } }
        fn main() {
            let a = N { v: 20 };
            let b = N { v: 6 };
            ret_i64((a + b).v);
            ret_i64((a - b).v);
            ret_i64((a * b).v);
            ret_i64((a / b).v);
            ret_i64((a % b).v);
        }
    ));
    assert_all(&result, &[ 26i64, 14, 120, 3, 2 ]);
}

#[test]
fn chained_operators() {
    let result = run(stringify!(
        struct N { v: i64 }
        impl Add for N { fn add(self: Self, rhs: Self) -> Self { N { v: self.v + rhs.v } } }
        fn main() {
            let a = N { v: 1 };
            let b = N { v: 2 };
            let c = N { v: 3 };
            ret_i64((a + b + c).v);
        }
    ));
    assert_all(&result, &[ 6i64 ]);
}

#[test]
fn operator_enum() {
    let result = run(stringify!(
        enum E { A(i64), B(i64) }
        impl Add for E {
            fn add(self: Self, rhs: Self) -> Self {
                match self {
                    E::A(x) => match rhs { E::A(y) => E::A(x + y), E::B(y) => E::A(x + y) },
                    E::B(x) => match rhs { E::A(y) => E::B(x + y), E::B(y) => E::B(x + y) },
                }
            }
        }
        fn main() {
            let r = E::A(3) + E::B(4);
            match r { E::A(v) => ret_i64(v), E::B(v) => ret_i64(v) }
        }
    ));
    assert_all(&result, &[ 7i64 ]);
}

// --- compound assignment via operator traits ---

#[test]
fn compound_assign_variable() {
    let result = run(stringify!(
        struct N { v: i64 }
        impl Add for N { fn add(self: Self, rhs: Self) -> Self { N { v: self.v + rhs.v } } }
        impl Mul for N { fn mul(self: Self, rhs: Self) -> Self { N { v: self.v * rhs.v } } }
        fn main() {
            let mut a = N { v: 5 };
            a += N { v: 3 };
            ret_i64(a.v);
            a *= N { v: 4 };
            ret_i64(a.v);
        }
    ));
    assert_all(&result, &[ 8i64, 32 ]);
}

#[test]
fn compound_assign_field() {
    let result = run(stringify!(
        struct N { v: i64 }
        struct Holder { n: N }
        impl Sub for N { fn sub(self: Self, rhs: Self) -> Self { N { v: self.v - rhs.v } } }
        fn main() {
            let mut h = Holder { n: N { v: 50 } };
            h.n -= N { v: 8 };
            ret_i64(h.n.v);
        }
    ));
    assert_all(&result, &[ 42i64 ]);
}

#[test]
fn compound_assign_index() {
    let result = run(stringify!(
        struct N { v: i64 }
        impl Add for N { fn add(self: Self, rhs: Self) -> Self { N { v: self.v + rhs.v } } }
        fn main() {
            let arr = [ N { v: 10 }, N { v: 20 }, N { v: 30 } ];
            arr[1] += N { v: 5 };
            arr[1] += N { v: 100 };
            ret_i64(arr[0].v);
            ret_i64(arr[1].v);
            ret_i64(arr[2].v);
        }
    ));
    assert_all(&result, &[ 10i64, 125, 30 ]);
}

#[test]
fn compound_assign_index_single_eval() {
    // the indexed target is dispatched in-place, so the index expression `left()` and the value
    // expression `right()` are each evaluated exactly once (each logs a marker exactly once)
    let sa_type = STACK_ADDRESS_TYPE;
    let result = run(&format!("
        struct N {{ v: i64 }}
        impl Add for N {{ fn add(self: Self, rhs: Self) -> Self {{ N {{ v: self.v + rhs.v }} }} }}
        fn left() -> {sa_type} {{
            ret_i64(99);
            0
        }}
        fn right() -> N {{
            ret_i64(7);
            N {{ v: 5 }}
        }}
        fn main() {{
            let arr = [ N {{ v: 10 }} ];
            arr[left()] += right();
            ret_i64(arr[0].v);
        }}
    "));
    assert_all(&result, &[ 99i64, 7, 15 ]);
}

// --- order independence and error reporting ---

#[test]
fn forward_impl() {
    // the operator trait is implemented after the expression that uses it
    let result = run(stringify!(
        fn main() {
            let a = N { v: 1 };
            let b = N { v: 2 };
            ret_i64((a + b).v);
        }
        struct N { v: i64 }
        impl Add for N { fn add(self: Self, rhs: Self) -> Self { N { v: self.v + rhs.v } } }
    ));
    assert_all(&result, &[ 3i64 ]);
}

#[test]
fn missing_impl() {
    // a type that does not implement Add cannot be used with `+`
    let err = build_err(stringify!(
        struct N { v: i64 }
        fn main() {
            let a = N { v: 1 };
            let b = N { v: 2 };
            ret_i64((a + b).v);
        }
    ));
    assert!(err.contains("does not implement 'Add'"), "unexpected error: {}", err);
}

#[test]
fn missing_impl_compound() {
    // a type that does not implement Add cannot be used with `+=`
    let err = build_err(stringify!(
        struct N { v: i64 }
        fn main() {
            let mut a = N { v: 1 };
            a += N { v: 2 };
            ret_i64(a.v);
        }
    ));
    assert!(err.contains("does not implement 'Add'"), "unexpected error: {}", err);
}

#[test]
fn numeric_unaffected() {
    // built-in numeric arithmetic and string concatenation still work alongside the operator traits
    let result = run(stringify!(
        fn main() {
            ret_i64(2 + 3 * 4 - 1);
            ret_string("a" + "b" + "c");
            let mut x = 10;
            x += 5;
            ret_i64(x);
        }
    ));
    assert(&result[0], 13i64);
    assert(&result[1], "abc".to_string());
    assert(&result[2], 15i64);
}
