use crate::util::*;

#[test]
fn view_primitive_element() {
    let result = run(stringify!(
        let v: View<i32> = View::new(5);
        v[0] = 42;
        v[1] = 100;
        ret_i32(v[0]);
        ret_i32(v[1]);
        ret_u64(v.len());
    ));
    assert_all!(&result, [42i32, 100i32, 5u64]);
}

#[test]
fn view_wrap() {
    let result = run(stringify!(
        let bytes: [u8] = [1, 2, 3, 4, 5, 6, 7, 8];
        let v: View<i64> = View::wrap(bytes);
        ret_u64(v.len());
    ));
    assert_all(&result, &[1u64]);
}

#[test]
fn view_struct_element() {
    let result = run(stringify!(
        struct Point {
            x: i32,
            y: i32,
        }

        fn main() {
            let v: View<Point> = View::new(3);
            v[0].x = 10;
            v[0].y = 20;
            v[1].x = 30;
            v[1].y = 40;

            ret_i32(v[0].x);
            ret_i32(v[0].y);
            ret_i32(v[1].x);
            ret_i32(v[1].y);
            ret_u64(v.len());
        }
    ));
    assert_all!(&result, [10i32, 20i32, 30i32, 40i32, 3u64]);
}

#[test]
fn view_struct_read_modify() {
    let result = run(stringify!(
        struct Vec2 {
            x: i32,
            y: i32,
        }

        fn main() {
            let v: View<Vec2> = View::new(2);
            v[0].x = 5;
            v[0].y = 10;

            let ax = v[0].x;
            let ay = v[0].y;

            ret_i32(ax + ay);
        }
    ));
    assert_all(&result, &[15i32]);
}

#[test]
fn view_nested_struct() {
    let result = run(stringify!(
        struct Inner {
            a: i32,
            b: i32,
        }

        struct Outer {
            inner: Inner,
            c: i32,
        }

        fn main() {
            let v: View<Outer> = View::new(1);
            v[0].inner.a = 100;
            v[0].inner.b = 200;
            v[0].c = 300;

            ret_i32(v[0].inner.a);
            ret_i32(v[0].inner.b);
            ret_i32(v[0].c);
        }
    ));
    assert_all(&result, &[100i32, 200, 300]);
}

#[test]
fn view_enum_element() {
    let result = run(stringify!(
        enum Color {
            Red,
            Green,
            Blue,
        }

        fn main() {
            let v: View<Color> = View::new(3);
            v[0] = Color::Red;
            v[1] = Color::Green;
            v[2] = Color::Blue;

            ret_i32(v[0] as i32);
            ret_i32(v[1] as i32);
            ret_i32(v[2] as i32);
        }
    ));
    assert_all(&result, &[0i32, 1, 2]);
}

#[test]
#[should_panic]
fn view_string_field_rejected() {
    run(stringify!(
        struct Bad {
            name: String,
        }

        fn main() {
            let v: View<Bad> = View::new(1);
        }
    ));
}

#[test]
fn view_u8_element() {
    let result = run(stringify!(
        let v: View<u8> = View::new(4);
        v[0] = 10;
        v[1] = 20;
        v[2] = 30;
        v[3] = 40;

        ret_u8(v[0] + v[1] + v[2] + v[3]);
    ));
    assert_all(&result, &[100u8]);
}

#[test]
fn view_u64_element() {
    let result = run(stringify!(
        let v: View<u64> = View::new(2);
        v[0] = 1000;
        v[1] = 2000;

        ret_u64(v[0] + v[1]);
    ));
    assert_all(&result, &[3000u64]);
}

#[test]
fn view_i16_element() {
    let result = run(stringify!(
        let v: View<i16> = View::new(3);
        v[0] = 100;
        v[1] = 200;
        v[2] = 300;

        ret_i16(v[0] + v[1] + v[2]);
    ));
    assert_all(&result, &[600i16]);
}

#[test]
fn view_f32_element() {
    let result = run(stringify!(
        let v: View<f32> = View::new(2);
        v[0] = 1.5;
        v[1] = 2.5;

        ret_f32(v[0] + v[1]);
    ));
    assert_all(&result, &[4.0f32]);
}

#[test]
fn view_turbofish_new() {
    let result = run(stringify!(
        let v = View<i32>::new(3);
        v[0] = 10;
        v[1] = 20;
        v[2] = 30;
        ret_i32(v[0] + v[1] + v[2]);
        ret_u64(v.len());
    ));
    assert_all!(&result, [60i32, 3u64]);
}

#[test]
fn view_turbofish_wrap() {
    let result = run(stringify!(
        let bytes: [u8] = [1, 2, 3, 4, 5, 6, 7, 8];
        let v = View<i64>::wrap(bytes);
        ret_u64(v.len());
    ));
    assert_all(&result, &[1u64]);
}

#[test]
fn view_wrap_i16_backing_struct() {
    let result = run(stringify!(
        struct Repr {
            a: u32,
            b: u16,
            c: u8,
            d: u8,
        }

        fn main() {
            let backing: [i16] = [1, 2, 3, 4];
            let v: View<Repr> = View::wrap(backing);
            ret_u64(v.len());
            ret_u32(v[0].a);
            ret_u16(v[0].b);
            ret_u8(v[0].c);
            ret_u8(v[0].d);
        }
    ));
    assert_all!(&result, [1u64, 131073u32, 3u16, 4u8, 0u8]);
}

#[test]
fn view_data_enum_compile() {
    // View<MyDataEnum> compiles and View::new allocates the correct size
    // (discriminant: 2 bytes + max variant payload: i32 = 4 bytes => packed_size = 6)
    let result = run(stringify!(
        enum Shape {
            Circle(i32),
            Rect(i32, i32),
            Point,
        }

        fn main() {
            let v: View<Shape> = View::new(5);
            ret_u64(v.len());
        }
    ));
    assert_all!(&result, [5u64]);
}

#[test]
fn view_data_enum_wrap() {
    // View::wrap on a byte array containing data enum data
    let result = run(stringify!(
        enum Shape {
            Circle(i32),
            Point,
        }

        fn main() {
            // packed_size = 6 (2 discriminant + 4 max payload from Circle(i32))
            // Element 0: Circle with radius 42
            // discriminant = 0 (little-endian: 0x00 0x00)
            // payload = 42 (little-endian: 0x2a 0x00 0x00 0x00)
            let backing: [u8] = [
                0x00, 0x00, 0x2a, 0x00, 0x00, 0x00,  // Circle(42)
            ];
            let v: View<Shape> = View::wrap(backing);
            ret_u64(v.len());
        }
    ));
    assert_all!(&result, [1u64]);
}

#[test]
fn view_struct_with_data_enum_field() {
    // Struct containing a data enum as a field is a valid view element type
    // packed_size = data_enum_packed_size(6) + i32(4) = 10
    let result = run(stringify!(
        enum Status {
            Active(i32),
            Idle,
        }

        struct Entry {
            status: Status,
            id: i32,
        }

        fn main() {
            let v: View<Entry> = View::new(3);
            ret_u64(v.len());
        }
    ));
    assert_all!(&result, [3u64]);
}

#[test]
#[should_panic]
fn view_full_element_struct_error() {
    // Loading a full struct element from a view should be a compile error
    run(stringify!(
        struct Point {
            x: i32,
            y: i32,
        }

        fn main() {
            let v: View<Point> = View::new(1);
            let p = v[0];
        }
    ));
}

#[test]
#[should_panic]
fn view_full_element_data_enum_error() {
    // Loading a full data enum element from a view should be a compile error
    run(stringify!(
        enum Shape {
            Circle(i32),
            Point,
        }

        fn main() {
            let v: View<Shape> = View::new(1);
            let s = v[0];
        }
    ));
}

#[test]
fn view_data_enum_match() {
    // match view[i] on a data enum exercises the dedicated view-match compilation path
    let result = run(stringify!(
        enum Shape {
            Circle(i32),
            Point,
        }

        fn main() {
            // packed_size = 6 (2 discriminant + 4 max payload)
            // Element 0: Circle(42)
            let backing: [u8] = [
                0x00, 0x00, 0x2a, 0x00, 0x00, 0x00,
            ];
            let v: View<Shape> = View::wrap(backing);

            match v[0] {
                Shape::Circle(r) => ret_i32(r),
                Shape::Point => ret_i32(-1),
            }
        }
    ));
    assert_all!(&result, [42i32]);
}

#[test]
fn view_data_enum_match_all_variants() {
    // Exhaustive match covering all variants including unit variants
    let result = run("enum Color { Red(i32), Green(i32), Blue } fn main() { let backing: [u8] = [0x00, 0x00, 0x64, 0x00, 0x00, 0x00, 0x01, 0x00, 0xc8, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00]; let v: View<Color> = View::wrap(backing); let r0 = match v[0] { Color::Red(_) => 1i32, Color::Green(_) => 2i32, Color::Blue => 3i32 }; let r1 = match v[1] { Color::Red(_) => 1i32, Color::Green(_) => 2i32, Color::Blue => 3i32 }; let r2 = match v[2] { Color::Red(_) => 1i32, Color::Green(_) => 2i32, Color::Blue => 3i32 }; ret_i32(r0); ret_i32(r1); ret_i32(r2); }");
    assert_all!(&result, [1i32, 2i32, 3i32]);
}

#[test]
fn view_data_enum_match_binding() {
    // Match binds variant fields and uses them in the arm body
    let result = run("enum Shape { Circle(i32), Rect(i32, i32), Point } fn main() { let backing: [u8] = [0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00]; let v: View<Shape> = View::wrap(backing); let r0 = match v[0] { Shape::Circle(r) => r * 2, Shape::Rect(_, _) => -1, Shape::Point => -2 }; let r1 = match v[1] { Shape::Circle(_) => -1, Shape::Rect(w, h) => w + h, Shape::Point => -2 }; ret_i32(r0); ret_i32(r1); }");
    assert_all!(&result, [14i32, 7i32]);
}

#[test]
fn view_data_enum_store() {
    // Assign a data enum value to a view slot, then read it back via match
    let result = run(stringify!(
        enum Shape {
            Circle(i32),
            Rect(i32, i32),
            Point,
        }

        fn main() {
            let v: View<Shape> = View::new(3);
            v[0] = Shape::Circle(42);
            v[1] = Shape::Rect(10, 20);
            v[2] = Shape::Point;

            let r0 = match v[0] {
                Shape::Circle(r) => r,
                Shape::Rect(_, _) => -1,
                Shape::Point => -2,
            };
            let r1 = match v[1] {
                Shape::Circle(_) => -1,
                Shape::Rect(w, h) => w + h,
                Shape::Point => -2,
            };
            let r2 = match v[2] {
                Shape::Circle(_) => -1,
                Shape::Rect(_, _) => -2,
                Shape::Point => 99,
            };
            ret_i32(r0);
            ret_i32(r1);
            ret_i32(r2);
        }
    ));
    assert_all!(&result, [42i32, 30i32, 99i32]);
}

#[test]
fn view_data_enum_store_unit_variant() {
    // Assign a unit variant to a view slot
    let result = run(stringify!(
        enum Status {
            Active(i32),
            Idle,
        }

        fn main() {
            let v: View<Status> = View::new(2);
            v[0] = Status::Idle;
            v[1] = Status::Active(7);

            let r0 = match v[0] {
                Status::Active(_) => 1i32,
                Status::Idle => 0i32,
            };
            let r1 = match v[1] {
                Status::Active(n) => n,
                Status::Idle => -1i32,
            };
            ret_i32(r0);
            ret_i32(r1);
        }
    ));
    assert_all!(&result, [0i32, 7i32]);
}

#[test]
fn view_data_enum_store_overwrite() {
    // Overwrite a view slot with a different variant
    let result = run(stringify!(
        enum Shape {
            Circle(i32),
            Point,
        }

        fn main() {
            let v: View<Shape> = View::new(1);
            v[0] = Shape::Circle(42);
            // Overwrite with unit variant
            v[0] = Shape::Point;

            let r = match v[0] {
                Shape::Circle(_) => 1i32,
                Shape::Point => 0i32,
            };
            ret_i32(r);
        }
    ));
    assert_all!(&result, [0i32]);
}

#[test]
fn view_data_enum_store_from_binding() {
    // Store a data enum value from a binding (not a literal)
    let result = run(stringify!(
        enum Shape {
            Circle(i32),
            Point,
        }

        fn main() {
            let v: View<Shape> = View::new(2);
            let s: Shape = Shape::Circle(99);
            v[0] = s;
            v[1] = Shape::Point;

            let r = match v[0] {
                Shape::Circle(r) => r,
                Shape::Point => -1,
            };
            ret_i32(r);
        }
    ));
    assert_all!(&result, [99i32]);
}

#[test]
fn view_data_enum_store_struct_field() {
    // Store a data enum with struct fields to a view
    // (reading back struct fields from view match is a separate path)
    let result = run(stringify!(
        struct Point {
            x: i32,
            y: i32,
        }

        enum Shape {
            Circle(Point, i32),
            None,
        }

        fn main() {
            let v: View<Shape> = View::new(1);
            v[0] = Shape::Circle(Point { x: 10, y: 20 }, 5);
            ret_u64(v.len());
        }
    ));
    assert_all!(&result, [1u64]);
}

#[test]
fn view_data_enum_store_nested_data_enum() {
    // Store a data enum containing another data enum to a view
    // (reading back nested enums from view match is a separate path)
    let result = run(stringify!(
        enum Status {
            Active(i32),
            Idle,
        }

        enum Entry {
            Item(Status, i32),
            Empty,
        }

        fn main() {
            let v: View<Entry> = View::new(2);
            v[0] = Entry::Item(Status::Active(42), 7);
            v[1] = Entry::Empty;
            ret_u64(v.len());
        }
    ));
    assert_all!(&result, [2u64]);
}
