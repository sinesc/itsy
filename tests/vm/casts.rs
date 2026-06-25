use crate::util::*;

#[test]
fn compound_i8() {
    let result = run(stringify!(
        let mut x = 1;
        x += 1;
        ret_i8(x);
        x *= 6;
        ret_i8(x);
        x -= 3;
        ret_i8(x);
        x /= 3;
        ret_i8(x);
        x %= 2;
        ret_i8(x);
    ));
    assert_all(&result, &[ 2i8, 12, 9, 3, 1 ]);
}

#[test]
fn compound_i64() {
    let result = run(stringify!(
        let mut x = 10000000000;
        x += 1;
        ret_i64(x);
        x *= 6;
        ret_i64(x);
        x -= 6;
        ret_i64(x);
        x /= 3;
        ret_i64(x);
        x %= 2;
        ret_i64(x);
    ));
    assert_all(&result, &[ 10000000001i64, 60000000006, 60000000000, 20000000000, 0 ]);
}

#[test]
fn compound_f64() {
    let result = run(stringify!(
        let mut x = 1.000001;
        x += 1.0;
        ret_f64(x);
        x *= 6.0;
        ret_f64(x);
        x -= 3.0;
        ret_f64(x);
        x /= 3.0;
        ret_f64(x);
        x %= 3.0;
        ret_f64(x);
    ));
    assert_all(&result, &[
        1.000001f64 + 1.0,
        (1.000001 + 1.0) * 6.0,
        ((1.000001 + 1.0) * 6.0) - 3.,
        (((1.000001 + 1.0) * 6.0) - 3.) / 3.,
        ((((1.000001 + 1.0) * 6.0) - 3.) / 3.) % 3.,
    ]);
}

// --- cast unsigned to unsigend ---

#[allow(overflowing_literals)]
#[test]
fn cast_u8_u16() {
    let result = run(stringify!(
        ret_u16(23u8 as u16);
        ret_u16(255u8 as u16);
        ret_u16(243u8 as u16);
    ));
    assert_all(&result, &[ 23u16, 255, 243 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_u64_u16() {
    let result = run(stringify!(
        ret_u16(23u64 as u16);
        ret_u16(280u64 as u16);
        ret_u16(243u64 as u16);
    ));
    assert_all(&result, &[ 23u64 as u16, 280u64 as u16, 243u64 as u16 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_u8_u64() {
    let result = run(stringify!(
        ret_u64(23u8 as u64);
        ret_u64(255u8 as u64);
        ret_u64(7u8 as u64);
    ));
    assert_all(&result, &[ 23u64, 255, 7 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_u64_u8() {
    let result = run(stringify!(
        ret_u8(23u64 as u8);
        ret_u8(280u64 as u8);
        ret_u8(243u64 as u8);
    ));
    assert_all(&result, &[ 23u8, 255, 243 ]);
}

// --- cast signed to signed ---

#[allow(overflowing_literals)]
#[test]
fn cast_i8_i16() {
    let result = run(stringify!(
        ret_i16(-23i8 as i16);
        ret_i16(-125i8 as i16);
        ret_i16(127i8 as i16);
    ));
    assert_all(&result, &[ -23i16, -125, 127 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_i16() {
    let result = run(stringify!(
        ret_i16(-23i64 as i16);
        ret_i16(-280i64 as i16);
        ret_i16(243i64 as i16);
    ));
    assert_all(&result, &[ -23i64 as i16, -280i64 as i16, 243i64 as i16 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i8_i64() {
    let result = run(stringify!(
        ret_i64(-23i8 as i64);
        ret_i64(-128i8 as i64);
        ret_i64(125i8 as i64);
    ));
    assert_all(&result, &[ -23i64, -128, 125 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_i8() {
    let result = run(stringify!(
        ret_i8(-23i64 as i8);
        ret_i8(-280i64 as i8);
        ret_i8(243i64 as i8);
    ));
    assert_all(&result, &[ -23i8, -128, 127 ]);
}

// --- cast signed to unsigned ---

#[allow(overflowing_literals)]
#[test]
fn cast_i8_u16() {
    let result = run(stringify!(
        ret_u16(-23i8 as u16);
        ret_u16(-128i8 as u16);
        ret_u16(127i8 as u16);
    ));
    assert_all(&result, &[ 0u16, 0, 127 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_u16() {
    let result = run(stringify!(
        ret_u16(-23i64 as u16);
        ret_u16(-280i64 as u16);
        ret_u16(243i64 as u16);
    ));
    assert_all(&result, &[ 0u16, 0, 243 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i8_u64() {
    let result = run(stringify!(
        ret_u64(-23i8 as u64);
        ret_u64(-128i8 as u64);
        ret_u64(127i8 as u64);
    ));
    assert_all(&result, &[ 0u64, 0, 127 ]);
}

#[allow(overflowing_literals)]
#[test]
fn cast_i64_u8() {
    let result = run(stringify!(
        ret_u8(-23i64 as u8);
        ret_u8(-280i64 as u8);
        ret_u8(243i64 as u8);
    ));
    assert_all(&result, &[ 0u8, 0, 243 ]);
}

// --- cast float to float ---

#[test]
fn cast_f64_f32() {
    let result = run(stringify!(
        ret_f32(3.1415f64 as f32);
        ret_f32(-3.1415f64 as f32);
        ret_f32(68123.45f64 as f32);
    ));
    assert_all(&result, &[ 3.1415f64 as f32, -3.1415f64 as f32, 68123.45f64 as f32 ]);
}

#[test]
fn cast_f32_f64() {
    let result = run(stringify!(
        ret_f64(3.1415f32 as f64);
        ret_f64(-3.1415f32 as f64);
        ret_f64(68123.45f32 as f64);
    ));
    assert_all(&result, &[ 3.1415f32 as f64, -3.1415f32 as f64, 68123.45f32 as f64 ]);
}

// --- cast signed to float ---

#[test]
fn cast_i16_f64() {
    let result = run(stringify!(
        ret_f64(3i16 as f64);
        ret_f64(-3i16 as f64);
        ret_f64(-9731i16 as f64);
    ));
    assert_all(&result, &[ 3i16 as f64, -3i16 as f64, -9731i16 as f64 ]);
}

#[test]
fn cast_i32_f32() {
    let result = run(stringify!(
        ret_f32(3i32 as f32);
        ret_f32(-3i32 as f32);
        ret_f32(-9731i32 as f32);
    ));
    assert_all(&result, &[ 3i32 as f32, -3i32 as f32, -9731i32 as f32 ]);
}

// --- cast float to signed ---

#[test]
fn cast_f64_i16() {
    let result = run(stringify!(
        ret_i16(3.1415f64 as i16);
        ret_i16(-3.1415f64 as i16);
        ret_i16(68123.45f64 as i16);
        ret_i16(2147483657f64 as i16);
        ret_i16(-2147483657f64 as i16);
    ));
    assert_all(&result, &[ 3i16, -3, 32767, 32767, -32768 ]);
}

#[test]
fn cast_f32_i8() {
    let result = run(stringify!(
        ret_i8(3.1415f32 as i8);
        ret_i8(-3.1415f32 as i8);
        ret_i8(68123.45f32 as i8);
        ret_i8(2147483657f64 as i8);
        ret_i8(-2147483657f64 as i8);
    ));
    assert_all(&result, &[ 3i8, -3, 127, 127, -128 ]);
}

// --- cast to String via the intrinsic ToString trait ---

#[test]
fn to_string_struct() {
    let result = run(stringify!(
        struct Point { x: i32, y: i32 }
        impl ToString for Point {
            fn to_string(self: Self) -> String {
                "({self.x}, {self.y})"
            }
        }
        fn main() {
            let p = Point { x: 3, y: 7 };
            ret_string(p as String);
        }
    ));
    assert_all(&result, &[ "(3, 7)".to_string() ]);
}

#[test]
fn to_string_enum() {
    let result = run(stringify!(
        enum Shape { Circle(i32), Square(i32) }
        impl ToString for Shape {
            fn to_string(self: Self) -> String {
                if self == Shape::Circle(3) { "circle" } else { "square" }
            }
        }
        fn main() {
            ret_string(Shape::Circle(3) as String);
            ret_string(Shape::Square(5) as String);
        }
    ));
    assert_all(&result, &[ "circle".to_string(), "square".to_string() ]);
}

#[test]
fn to_string_via_interpolation() {
    // string interpolation lowers `{expr}` to `expr as String`, which routes custom types through ToString
    let result = run(stringify!(
        struct Point { x: i32, y: i32 }
        impl ToString for Point {
            fn to_string(self: Self) -> String {
                "({self.x}, {self.y})"
            }
        }
        fn main() {
            let p = Point { x: 1, y: 2 };
            ret_string("point: {p}!");
        }
    ));
    assert_all(&result, &[ "point: (1, 2)!".to_string() ]);
}

#[test]
fn to_string_dynamic_dispatch() {
    // casting a trait-object-typed value to String dispatches through the vtable to the concrete impl
    let result = run(stringify!(
        struct Dog { name: String }
        struct Cat { age: i32 }
        impl ToString for Dog {
            fn to_string(self: Self) -> String {
                "dog {self.name}"
            }
        }
        impl ToString for Cat {
            fn to_string(self: Self) -> String {
                "cat {self.age}"
            }
        }
        fn stringify(thing: ToString) -> String {
            thing as String
        }
        fn main() {
            ret_string(stringify(Dog { name: "Rex" }));
            ret_string(stringify(Cat { age: 3 }));
        }
    ));
    assert_all(&result, &[ "dog Rex".to_string(), "cat 3".to_string() ]);
}

#[test]
fn to_string_forward_impl() {
    // the ToString impl is declared after the cast that uses it; resolution must not prematurely
    // conclude the trait is unimplemented
    let result = run(stringify!(
        fn main() {
            let p = Point { x: 4, y: 9 };
            ret_string(p as String);
        }
        struct Point { x: i32, y: i32 }
        impl ToString for Point {
            fn to_string(self: Self) -> String {
                "({self.x}, {self.y})"
            }
        }
    ));
    assert_all(&result, &[ "(4, 9)".to_string() ]);
}

#[test]
fn to_string_missing_impl() {
    // a type that does not implement ToString cannot be cast to String and yields a dedicated error
    let err = build_err(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let p = Point { x: 3, y: 7 };
            ret_string(p as String);
        }
    ));
    assert!(err.contains("does not implement required trait `ToString`"), "unexpected error: {}", err);
}

// --- cast float to unsigned ---

#[test]
fn cast_f32_u16() {
    let result = run(stringify!(
        ret_u16(3.1415f32 as u16);
        ret_u16(-3.1415f32 as u16);
        ret_u16(68123.45f32 as u16);
        ret_u16(2147483657f32 as u16);
        ret_u16(-2147483657f32 as u16);
    ));
    assert_all(&result, &[ 3u16, 0, 65535, 65535, 0 ]);
}

// --- cast custom types to integer via ToUnsigned / ToSigned ---

#[test]
fn to_unsigned_struct_full_width() {
    // ToUnsigned returns u64; cast to u64 is the full-width path (no trailing cast)
    let result = run(stringify!(
        struct Color { r: u8, g: u8, b: u8 }
        impl ToUnsigned for Color {
            fn to_unsigned(self: Self) -> u64 {
                (self.r as u64 << 16) + (self.g as u64 << 8) + (self.b as u64)
            }
        }
        fn main() {
            let c = Color { r: 0x12, g: 0x34, b: 0x56 };
            ret_u64(c as u64);
        }
    ));
    assert_all(&result, &[ 0x123456u64 ]);
}

#[test]
fn to_unsigned_struct_truncation() {
    // Cast to narrower targets: the u64 returned by to_unsigned is cast down to the target width.
    // The VM saturates (clamps) when the value exceeds the target range.
    let result = run(stringify!(
        struct Rgb { r: u8, g: u8, b: u8 }
        impl ToUnsigned for Rgb {
            fn to_unsigned(self: Self) -> u64 {
                (self.r as u64 << 16) + (self.g as u64 << 8) + (self.b as u64)
            }
        }
        fn main() {
            let c = Rgb { r: 0x00, g: 0x00, b: 0x56 };
            ret_u32(c as u32);   // 0x56 fits in u32
            ret_u16(c as u16);   // 0x56 fits in u16
            ret_u8(c as u8);     // 0x56 fits in u8
        }
    ));
    assert_all!(&result, [ 0x56u32, 0x56u16, 0x56u8 ]);
}

#[test]
fn to_signed_struct_full_width() {
    // ToSigned returns i64; cast to i64 is the full-width path
    let result = run(stringify!(
        struct Color { r: u8, g: u8, b: u8 }
        impl ToSigned for Color {
            fn to_signed(self: Self) -> i64 {
                (self.r as i64 << 16) + (self.g as i64 << 8) + (self.b as i64)
            }
        }
        fn main() {
            let c = Color { r: 0x12, g: 0x34, b: 0x56 };
            ret_i64(c as i64);
        }
    ));
    assert_all(&result, &[ 0x123456i64 ]);
}

#[test]
fn to_signed_struct_truncation() {
    // Cast to narrower signed targets: the i64 returned by to_signed is cast down to the target width.
    // The VM saturates (clamps) when the value exceeds the target range.
    let result = run(stringify!(
        struct Rgb { r: u8, g: u8, b: u8 }
        impl ToSigned for Rgb {
            fn to_signed(self: Self) -> i64 {
                (self.r as i64 << 16) + (self.g as i64 << 8) + (self.b as i64)
            }
        }
        fn main() {
            let c = Rgb { r: 0x00, g: 0x00, b: 0x56 };
            ret_i32(c as i32);   // 0x56 fits in i32
            ret_i16(c as i16);   // 0x56 fits in i16
            ret_i8(c as i8);     // 0x56 fits in i8
        }
    ));
    assert_all!(&result, [ 0x56i32, 0x56i16, 0x56i8 ]);
}

#[test]
fn to_signed_negative_value() {
    // Negative values from to_signed should sign-truncate correctly
    let result = run(stringify!(
        struct Neg { val: i32 }
        impl ToSigned for Neg {
            fn to_signed(self: Self) -> i64 {
                self.val as i64
            }
        }
        fn main() {
            let n = Neg { val: -1 };
            ret_i64(n as i64);
            ret_i8(n as i8);
        }
    ));
    assert_all!(&result, [ -1i64, -1i8 ]);
}

#[test]
fn to_unsigned_enum() {
    // Data-carrying enum implementing ToUnsigned
    let result = run(stringify!(
        enum Shape { Circle(i32), Square(i32) }
        impl ToUnsigned for Shape {
            fn to_unsigned(self: Self) -> u64 {
                match self {
                    Shape::Circle(r) => r as u64,
                    Shape::Square(s) => (s as u64) * 2,
                }
            }
        }
        fn main() {
            ret_u64(Shape::Circle(5) as u64);
            ret_u64(Shape::Square(3) as u64);
            ret_u8(Shape::Circle(255) as u8);
        }
    ));
    assert_all!(&result, [ 5u64, 6u64, 255u8 ]);
}

#[test]
fn both_traits_dispatch() {
    // A type implementing both ToSigned and ToUnsigned; signed targets use ToSigned, unsigned use ToUnsigned
    let result = run(stringify!(
        struct MyType { val: i32 }
        impl ToUnsigned for MyType {
            fn to_unsigned(self: Self) -> u64 {
                self.val as u64
            }
        }
        impl ToSigned for MyType {
            fn to_signed(self: Self) -> i64 {
                self.val as i64 * 10
            }
        }
        fn main() {
            let m = MyType { val: 7 };
            ret_u16(m as u16);
            ret_i16(m as i16);
        }
    ));
    assert_all!(&result, [ 7u16, 70i16 ]);
}

#[test]
fn to_unsigned_missing_impl() {
    // A type that does not implement ToSigned/ToUnsigned cannot be cast to integer
    let err = build_err(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let p = Point { x: 3, y: 7 };
            ret_i32(p as i32);
        }
    ));
    assert!(err.contains("does not implement required trait `ToSigned`"), "unexpected error: {}", err);
}

#[test]
fn to_unsigned_missing_impl_unsigned() {
    let err = build_err(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let p = Point { x: 3, y: 7 };
            ret_u32(p as u32);
        }
    ));
    assert!(err.contains("does not implement required trait `ToUnsigned`"), "unexpected error: {}", err);
}

// --- cast custom types to float via ToFloat ---

#[test]
fn to_float_struct_full_width() {
    // ToFloat returns f64; cast to f64 is the full-width path (no trailing cast)
    let result = run(stringify!(
        struct Point { x: i32, y: i32 }
        impl ToFloat for Point {
            fn to_float(self: Self) -> f64 {
                (self.x as f64 * self.x as f64 + self.y as f64 * self.y as f64).sqrt()
            }
        }
        fn main() {
            let p = Point { x: 3, y: 4 };
            ret_f64(p as f64);
        }
    ));
    assert_all(&result, &[ 5.0f64 ]);
}

#[test]
fn to_float_struct_truncation() {
    // Cast to f32: the f64 returned by to_float is cast down to f32 precision
    let result = run(stringify!(
        struct Coord { val: i32 }
        impl ToFloat for Coord {
            fn to_float(self: Self) -> f64 {
                self.val as f64 * 1.5
            }
        }
        fn main() {
            let c = Coord { val: 10 };
            ret_f32(c as f32);
            ret_f64(c as f64);
        }
    ));
    assert_all!(&result, [ 15.0f32, 15.0f64 ]);
}

#[test]
fn to_float_negative_value() {
    // Negative values from to_float should work correctly
    let result = run(stringify!(
        struct Neg { val: i32 }
        impl ToFloat for Neg {
            fn to_float(self: Self) -> f64 {
                self.val as f64 * -0.5
            }
        }
        fn main() {
            let n = Neg { val: 10 };
            ret_f64(n as f64);
            ret_f32(n as f32);
        }
    ));
    assert_all!(&result, [ -5.0f64, -5.0f32 ]);
}

#[test]
fn to_float_enum() {
    // Data-carrying enum implementing ToFloat
    let result = run(stringify!(
        enum Shape { Circle(i32), Square(i32) }
        impl ToFloat for Shape {
            fn to_float(self: Self) -> f64 {
                match self {
                    Shape::Circle(r) => r as f64 * 3.14159 * 2.0,
                    Shape::Square(s) => s as f64 * 4.0,
                }
            }
        }
        fn main() {
            ret_f64(Shape::Circle(1) as f64);
            ret_f64(Shape::Square(3) as f64);
            ret_f32(Shape::Square(2) as f32);
        }
    ));
    assert_all!(&result, [ 6.28318f64, 12.0f64, 8.0f32 ]);
}

#[test]
fn to_float_both_traits_dispatch() {
    // A type implementing both ToSigned and ToFloat; signed targets use ToSigned, float uses ToFloat
    let result = run(stringify!(
        struct MyType { val: i32 }
        impl ToSigned for MyType {
            fn to_signed(self: Self) -> i64 {
                self.val as i64 * 10
            }
        }
        impl ToFloat for MyType {
            fn to_float(self: Self) -> f64 {
                self.val as f64 * 0.5
            }
        }
        fn main() {
            let m = MyType { val: 7 };
            ret_i32(m as i32);
            ret_f64(m as f64);
            ret_f32(m as f32);
        }
    ));
    assert_all!(&result, [ 70i32, 3.5f64, 3.5f32 ]);
}

#[test]
fn to_float_missing_impl() {
    // A type that does not implement ToFloat cannot be cast to float
    let err = build_err(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let p = Point { x: 3, y: 7 };
            ret_f64(p as f64);
        }
    ));
    assert!(err.contains("does not implement required trait `ToFloat`"), "unexpected error: {}", err);
}

#[test]
fn to_float_missing_impl_f32() {
    let err = build_err(stringify!(
        struct Point { x: i32, y: i32 }
        fn main() {
            let p = Point { x: 3, y: 7 };
            ret_f32(p as f32);
        }
    ));
    assert!(err.contains("does not implement required trait `ToFloat`"), "unexpected error: {}", err);
}

// --- regression: existing casts still work ---

#[test]
fn regression_numeric_cast() {
    // Plain numeric cast still works
    let result = run(stringify!(
        ret_u8(5u32 as u8);
        ret_i8(-280i64 as i8);
    ));
    assert_all!(&result, [ 5u8, -128i8 ]);
}

#[test]
fn regression_to_string_still_works() {
    // Existing as String path is unchanged
    let result = run(stringify!(
        struct Point { x: i32, y: i32 }
        impl ToString for Point {
            fn to_string(self: Self) -> String {
                "({self.x}, {self.y})"
            }
        }
        fn main() {
            let p = Point { x: 3, y: 7 };
            ret_string(p as String);
        }
    ));
    assert_all(&result, &[ "(3, 7)".to_string() ]);
}