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
