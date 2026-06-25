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
    assert!(err.contains("does not implement required trait `Add`"), "unexpected error: {}", err);
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
    assert!(err.contains("does not implement required trait `Add`"), "unexpected error: {}", err);
}

// --- equality via the Eq trait ---

#[test]
fn eq_struct() {
    // `Eq` overrides the built-in deep comparison: this `eq` ignores the `note` field, so values that
    // differ only in `note` compare equal and `!=` is the negation of that
    let result = run(stringify!(
        struct Money { cents: i64, note: String }
        impl Eq for Money {
            fn eq(self: Self, rhs: Self) -> bool {
                self.cents == rhs.cents
            }
        }
        fn main() {
            let a = Money { cents: 500, note: "rent" };
            let b = Money { cents: 500, note: "gift" };
            let c = Money { cents: 750, note: "rent" };
            ret_bool(a == b);
            ret_bool(a != b);
            ret_bool(a == c);
            ret_bool(a != c);
        }
    ));
    assert_all(&result, &[ true, false, false, true ]);
}

#[test]
fn eq_enum() {
    let result = run(stringify!(
        enum E { A(i64), B(i64) }
        impl Eq for E {
            fn eq(self: Self, rhs: Self) -> bool {
                // equal only when both are the A variant carrying the same value
                match self {
                    E::A(x) => match rhs { E::A(y) => x == y, E::B(y) => false },
                    E::B(x) => false,
                }
            }
        }
        fn main() {
            ret_bool(E::A(3) == E::A(3));
            ret_bool(E::A(3) == E::A(4));
            ret_bool(E::A(3) == E::B(3));
            ret_bool(E::A(3) != E::B(3));
        }
    ));
    assert_all(&result, &[ true, false, false, true ]);
}

#[test]
fn eq_in_condition() {
    // the lowered comparison works anywhere a bool is expected, e.g. an `if` condition
    let result = run(stringify!(
        struct N { v: i64 }
        impl Eq for N { fn eq(self: Self, rhs: Self) -> bool { self.v == rhs.v } }
        fn main() {
            let a = N { v: 1 };
            let b = N { v: 1 };
            if a == b {
                ret_i64(10);
            }
            if a != b {
                ret_i64(20);
            } else {
                ret_i64(30);
            }
        }
    ));
    assert_all(&result, &[ 10i64, 30 ]);
}

#[test]
fn eq_forward_impl() {
    // the Eq trait is implemented after the expression that uses it
    let result = run(stringify!(
        fn main() {
            ret_bool(N { v: 1 } == N { v: 1 });
            ret_bool(N { v: 1 } == N { v: 2 });
        }
        struct N { v: i64 }
        impl Eq for N { fn eq(self: Self, rhs: Self) -> bool { self.v == rhs.v } }
    ));
    assert_all(&result, &[ true, false ]);
}

#[test]
fn eq_dynamic_dispatch() {
    // `==` through trait-object-typed operands dispatches `eq` virtually, like the arithmetic traits
    let result = run(stringify!(
        struct N { v: i64 }
        impl Eq for N { fn eq(self: Self, rhs: Self) -> bool { self.v == rhs.v } }
        fn check(a: Eq, b: Eq) -> bool {
            a == b
        }
        fn main() {
            ret_bool(check(N { v: 5 }, N { v: 5 }));
            ret_bool(check(N { v: 5 }, N { v: 6 }));
        }
    ));
    assert_all(&result, &[ true, false ]);
}

#[test]
fn eq_without_impl_uses_builtin() {
    // a type that does not implement Eq still compares via the built-in deep comparison (field by field)
    let result = run(stringify!(
        struct P { x: i64, y: i64 }
        fn main() {
            ret_bool(P { x: 1, y: 2 } == P { x: 1, y: 2 });
            ret_bool(P { x: 1, y: 2 } == P { x: 1, y: 9 });
            ret_bool(P { x: 1, y: 2 } != P { x: 9, y: 2 });
        }
    ));
    assert_all(&result, &[ true, false, true ]);
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

// --- bitwise/shift operators via operator traits ---

#[test]
fn bitwise_traits_struct() {
    let result = run(stringify!(
        struct N { v: i64 }
        impl BitAnd for N { fn bitand(self: Self, rhs: Self) -> Self { N { v: self.v & rhs.v } } }
        impl BitOr for N { fn bitor(self: Self, rhs: Self) -> Self { N { v: self.v | rhs.v } } }
        impl BitXor for N { fn bitxor(self: Self, rhs: Self) -> Self { N { v: self.v ^ rhs.v } } }
        fn main() {
            let a = N { v: 12 };
            let b = N { v: 10 };
            ret_i64((a & b).v);
            ret_i64((a | b).v);
            ret_i64((a ^ b).v);
        }
    ));
    assert_all(&result, &[ 8i64, 14, 6 ]);
}

#[test]
fn shift_traits_struct() {
    let result = run(stringify!(
        struct N { v: i64 }
        impl Shl for N { fn shl(self: Self, rhs: i64) -> Self { N { v: self.v << rhs } } }
        impl Shr for N { fn shr(self: Self, rhs: i64) -> Self { N { v: self.v >> rhs } } }
        fn main() {
            let a = N { v: 1 };
            ret_i64((a << 4).v);
            let b = N { v: 16 };
            ret_i64((b >> 2).v);
        }
    ));
    assert_all(&result, &[ 16i64, 4 ]);
}

#[test]
fn bitwise_compound_assign_variable() {
    let result = run(stringify!(
        struct N { v: i64 }
        impl BitAnd for N { fn bitand(self: Self, rhs: Self) -> Self { N { v: self.v & rhs.v } } }
        impl BitOr for N { fn bitor(self: Self, rhs: Self) -> Self { N { v: self.v | rhs.v } } }
        impl BitXor for N { fn bitxor(self: Self, rhs: Self) -> Self { N { v: self.v ^ rhs.v } } }
        fn main() {
            let mut a = N { v: 15 };
            a &= N { v: 10 };
            ret_i64(a.v);
            a |= N { v: 5 };
            ret_i64(a.v);
            a ^= N { v: 12 };
            ret_i64(a.v);
        }
    ));
    assert_all(&result, &[ 10i64, 15, 3 ]);
}

#[test]
fn shift_compound_assign_variable() {
    let result = run(stringify!(
        struct N { v: i64 }
        impl Shl for N { fn shl(self: Self, rhs: i64) -> Self { N { v: self.v << rhs } } }
        impl Shr for N { fn shr(self: Self, rhs: i64) -> Self { N { v: self.v >> rhs } } }
        fn main() {
            let mut a = N { v: 1 };
            a <<= 3;
            ret_i64(a.v);
            a >>= 2;
            ret_i64(a.v);
        }
    ));
    assert_all(&result, &[ 8i64, 2 ]);
}

#[test]
fn bitwise_on_builtin_integers() {
    let result = run(stringify!(
        fn main() {
            ret_i64(12 & 10);
            ret_i64(12 | 10);
            ret_i64(12 ^ 10);
            ret_i64(1 << 4);
            ret_i64(16 >> 2);
            let mut x: i64 = 15;
            x &= 10;
            ret_i64(x);
            x |= 5;
            ret_i64(x);
        }
    ));
    assert_all(&result, &[ 8i64, 14, 6, 16, 4, 10, 15 ]);
}

#[test]
fn bitwise_trait_missing_error() {
    let err = build_err(stringify!(
        struct S { v: i64 }
        fn main() {
            let a = S { v: 1 };
            let b = S { v: 2 };
            ret_i64((a & b).v);
        }
    ));
    assert!(err.contains("BitAnd"), "unexpected error: {}", err);
}

#[test]
fn shift_trait_missing_error() {
    let err = build_err(stringify!(
        struct S { v: i64 }
        fn main() {
            let a = S { v: 1 };
            ret_i64((a << 2).v);
        }
    ));
    assert!(err.contains("Shl"), "unexpected error: {}", err);
}

// --- unary operators via the Neg / Not traits ---

#[test]
fn neg_struct() {
    let result = run(stringify!(
        struct V { x: i64, y: i64 }
        impl Neg for V {
            fn neg(self: Self) -> Self {
                V { x: -self.x, y: -self.y }
            }
        }
        fn main() {
            let a = V { x: 1, y: -2 };
            let b = -a;
            ret_i64(b.x);
            ret_i64(b.y);
        }
    ));
    assert_all(&result, &[ -1i64, 2 ]);
}

#[test]
fn not_struct() {
    let result = run(stringify!(
        struct V { x: i64, y: i64 }
        impl Not for V {
            fn not(self: Self) -> Self {
                V { x: !self.x, y: !self.y }
            }
        }
        fn main() {
            let a = V { x: 0, y: -1 };
            let b = !a;
            ret_i64(b.x);
            ret_i64(b.y);
        }
    ));
    assert_all(&result, &[ -1i64, 0 ]);
}

#[test]
fn neg_enum() {
    let result = run(stringify!(
        enum E { A(i64), B(i64) }
        impl Neg for E {
            fn neg(self: Self) -> Self {
                match self {
                    E::A(x) => E::A(-x),
                    E::B(x) => E::B(-x),
                }
            }
        }
        fn main() {
            let r = -E::A(3);
            match r { E::A(v) => ret_i64(v), E::B(v) => ret_i64(v) }
        }
    ));
    assert_all(&result, &[ -3i64 ]);
}

#[test]
fn neg_chained() {
    // negating a previously negated value dispatches through the trait method each time
    let result = run(stringify!(
        struct N { v: i64 }
        impl Neg for N { fn neg(self: Self) -> Self { N { v: -self.v } } }
        fn main() {
            let a = N { v: 5 };
            let b = -a;
            ret_i64((-b).v);
        }
    ));
    assert_all(&result, &[ 5i64 ]);
}

#[test]
fn neg_forward_impl() {
    // the unary operator trait is implemented after the expression that uses it
    let result = run(stringify!(
        fn main() {
            let a = N { v: 7 };
            ret_i64((-a).v);
        }
        struct N { v: i64 }
        impl Neg for N { fn neg(self: Self) -> Self { N { v: -self.v } } }
    ));
    assert_all(&result, &[ -7i64 ]);
}

#[test]
fn neg_missing_impl() {
    // a type that does not implement Neg cannot be used with unary `-`
    let err = build_err(stringify!(
        struct N { v: i64 }
        fn main() {
            let a = N { v: 1 };
            ret_i64((-a).v);
        }
    ));
    assert!(err.contains("Neg"), "unexpected error: {}", err);
}

#[test]
fn not_missing_impl() {
    // a type that does not implement Not cannot be used with unary `!`
    let err = build_err(stringify!(
        struct N { v: i64 }
        fn main() {
            let a = N { v: 1 };
            ret_i64((!a).v);
        }
    ));
    assert!(err.contains("Not"), "unexpected error: {}", err);
}
