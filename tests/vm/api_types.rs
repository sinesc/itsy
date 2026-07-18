//! Tests for passing `#[derive(VMValue)]` structs/enums and arrays (`Vec<T>`) across the Itsy API
//! boundary.
//!
//! Each test builds and runs a small program against a custom API that constructs, consumes and
//! round-trips these types. Results are pushed into the `Context` via `ret_*` helpers and asserted with
//! `assert_all`. Because `VM::run` reports `HeapCorruption` when objects are leaked at termination,
//! these tests also catch reference-counting mistakes (leaks and double-frees).

use itsy::{itsy_api, build_str, run as itsy_run, VMValue};
use crate::util::{Context, assert_all};

#[derive(VMValue)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(VMValue)]
struct Person {
    name: String,
    age: u8,
}

#[derive(VMValue)]
struct Line {
    from: Point,
    to: Point,
}

// primitive/C-like enum (implicit discriminants 0,1,2,3)
#[derive(VMValue, PartialEq, Debug)]
enum Direction {
    North,
    East,
    South,
    West,
}

// primitive enum with explicit discriminants
#[derive(VMValue, PartialEq, Debug)]
enum Status {
    Ok = 200,
    NotFound = 404,
}

// primitive enum with non-default repr type (u8 instead of i32)
#[derive(VMValue, PartialEq, Debug)]
#[itsy(repr(u8))]
enum Color {
    Red,
    Green,
    Blue,
}

// data-carrying enum, incl. a unit variant and a String-carrying variant
#[derive(VMValue, PartialEq, Debug)]
enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Named(String),
    Empty,
}

// struct with an enum field
#[derive(VMValue)]
struct Tagged {
    dir: Direction,
    value: i32,
}

// enum carrying a struct payload
#[derive(VMValue)]
enum Wrap {
    Pt(Point),
    None,
}

itsy_api! {
    Api<Context> {
        // returns a struct constructed in Rust
        fn make_point(&mut context, x: i32, y: i32) -> Point {
            Point { x, y }
        }
        // consumes a struct, returns a primitive
        fn point_sum(&mut context, p: Point) -> i32 {
            p.x + p.y
        }
        // struct with a String field
        fn make_person(&mut context, name: String, age: u8) -> Person {
            Person { name, age }
        }
        fn person_summary(&mut context, p: Person) -> String {
            format!("{}:{}", p.name, p.age)
        }
        // nested struct
        fn make_line(&mut context, x1: i32, y1: i32, x2: i32, y2: i32) -> Line {
            Line { from: Point { x: x1, y: y1 }, to: Point { x: x2, y: y2 } }
        }
        fn line_dx(&mut context, l: Line) -> i32 {
            l.to.x - l.from.x
        }
        // primitive enums
        fn dir_index(&mut context, d: Direction) -> i32 {
            d as i32
        }
        fn opposite(&mut context, d: Direction) -> Direction {
            match d {
                Direction::North => Direction::South,
                Direction::South => Direction::North,
                Direction::East => Direction::West,
                Direction::West => Direction::East,
            }
        }
        fn status_code(&mut context, s: Status) -> i32 {
            s as i32
        }
        fn classify(&mut context, code: i32) -> Status {
            if code == 200 { Status::Ok } else { Status::NotFound }
        }
        // primitive enum with custom repr type (u8)
        fn color_index(&mut context, c: Color) -> u8 {
            c as u8
        }
        fn next_color(&mut context, c: Color) -> Color {
            match c {
                Color::Red => Color::Green,
                Color::Green => Color::Blue,
                Color::Blue => Color::Red,
            }
        }
        // data enums
        fn area(&mut context, s: Shape) -> f64 {
            match s {
                Shape::Circle(r) => 3.0 * r * r,
                Shape::Rect(w, h) => w * h,
                Shape::Named(_) => -1.0,
                Shape::Empty => 0.0,
            }
        }
        fn make_circle(&mut context, r: f64) -> Shape {
            Shape::Circle(r)
        }
        fn shape_name(&mut context, s: Shape) -> String {
            match s {
                Shape::Named(name) => name,
                _ => String::from("?"),
            }
        }
        // enum as struct field
        fn tag_index(&mut context, t: Tagged) -> i32 {
            (t.dir as i32) + t.value
        }
        fn make_tagged(&mut context, value: i32) -> Tagged {
            Tagged { dir: Direction::West, value }
        }
        // struct as enum payload
        fn wrap_sum(&mut context, w: Wrap) -> i32 {
            match w {
                Wrap::Pt(p) => p.x + p.y,
                Wrap::None => -1,
            }
        }
        fn wrap_point(&mut context, x: i32, y: i32) -> Wrap {
            Wrap::Pt(Point { x, y })
        }
        // arrays of primitives
        fn make_range(&mut context, n: u16) -> [ u16 ] {
            (0..n).collect()
        }
        fn sum_u16(&mut context, values: [ u16 ]) -> i32 {
            values.iter().map(|v| *v as i32).sum()
        }
        // arrays of strings
        fn make_words(&mut context) -> [ String ] {
            vec![String::from("a"), String::from("bb"), String::from("ccc")]
        }
        fn join(&mut context, parts: [ String ]) -> String {
            parts.join("-")
        }
        // arrays of structs/enums
        fn make_points(&mut context, n: i32) -> [ Point ] {
            (0..n).map(|i| Point { x: i, y: i * 2 }).collect()
        }
        fn points_sum(&mut context, ps: [ Point ]) -> i32 {
            ps.iter().map(|p| p.x + p.y).sum()
        }
        fn shapes_area(&mut context, ss: [ Shape ]) -> f64 {
            ss.iter().map(|s| match s {
                Shape::Circle(r) => 3.0 * r * r,
                Shape::Rect(w, h) => w * h,
                Shape::Named(_) => -1.0,
                Shape::Empty => 0.0,
            }).sum()
        }
        // nested arrays
        fn make_grid(&mut context, rows: u16, cols: u16) -> [ [ u16 ] ] {
            (0..rows).map(|_| (0..cols).collect()).collect()
        }
        fn grid_sum(&mut context, g: [ [ u16 ] ]) -> i32 {
            g.iter().flatten().map(|v| *v as i32).sum()
        }
        fn flatten_words(&mut context, g: [ [ String ] ]) -> String {
            g.into_iter().flatten().collect::<Vec<String>>().join(",")
        }
        // assertion helpers
        fn ret_i32(&mut context, value: i32) {
            context.push(Box::new(value));
        }
        fn ret_f64(&mut context, value: f64) {
            context.push(Box::new(value));
        }
        fn ret_string(&mut context, value: String) {
            context.push(Box::new(value));
        }
    }
}

const PRELUDE: &str = "
    use Api::{make_point, point_sum, make_person, person_summary, make_line, line_dx, dir_index, opposite, status_code, classify, color_index, next_color, area, make_circle, shape_name, tag_index, make_tagged, wrap_sum, wrap_point, make_range, sum_u16, make_words, join, make_points, points_sum, shapes_area, make_grid, grid_sum, flatten_words, ret_i32, ret_f64, ret_string};
    use Api::{Point, Person, Line, Direction, Status, Color, Shape, Tagged, Wrap};
";

fn run(code: &str) -> Context {
    let input = format!("{} fn main() {{ {} }}", PRELUDE, code);
    let program = match build_str::<Api>(&input) {
        Ok(program) => program,
        Err(err) => {
            let loc = err.loc(&input);
            panic!("{} in line {}, column {}.", err, loc.0, loc.1);
        }
    };
    let mut context = Vec::new();
    if let Err(err) = itsy_run(program, &mut context) {
        panic!("{}", err);
    }
    context
}

#[test]
fn rust_constructed_struct_returned_to_script() {
    let result = run(stringify!(
        ret_i32(point_sum(make_point(3, 4)));
    ));
    assert_all(&result, &[7_i32]);
}

#[test]
fn script_constructed_struct_passed_to_rust() {
    let result = run(stringify!(
        let p = Point { x: 10, y: 20 };
        ret_i32(point_sum(p));
    ));
    assert_all(&result, &[30_i32]);
}

#[test]
fn struct_with_string_field_roundtrip() {
    let result = run(stringify!(
        ret_string(person_summary(make_person("Alice", 30)));
    ));
    assert_all(&result, &[String::from("Alice:30")]);
}

#[test]
fn script_constructed_struct_with_string_field() {
    let result = run(stringify!(
        let p = Person { name: "Bob", age: 7 };
        ret_string(person_summary(p));
    ));
    assert_all(&result, &[String::from("Bob:7")]);
}

#[test]
fn nested_struct_rust_constructed() {
    let result = run(stringify!(
        ret_i32(line_dx(make_line(1, 2, 5, 8)));
    ));
    assert_all(&result, &[4_i32]);
}

#[test]
fn nested_struct_script_constructed() {
    let result = run(stringify!(
        let l = Line { from: Point { x: 0, y: 0 }, to: Point { x: 100, y: 1 } };
        ret_i32(line_dx(l));
    ));
    assert_all(&result, &[100_i32]);
}

#[test]
fn multiple_values_and_field_access_in_script() {
    let result = run(stringify!(
        let p = make_point(2, 5);
        ret_i32(p.x);
        ret_i32(p.y);
        ret_i32(point_sum(p));
    ));
    assert_all(&result, &[2_i32, 5_i32, 7_i32]);
}

#[test]
fn primitive_enum_script_to_rust() {
    let result = run(stringify!(
        ret_i32(dir_index(Direction::North));
        ret_i32(dir_index(Direction::West));
    ));
    assert_all(&result, &[0_i32, 3_i32]);
}

#[test]
fn primitive_enum_rust_to_script() {
    // opposite() returns an enum to the script, which then casts and re-passes it
    let result = run(stringify!(
        let d = opposite(Direction::North);
        ret_i32(dir_index(d));
        ret_i32(d as i32);
    ));
    assert_all(&result, &[2_i32, 2_i32]);
}

#[test]
fn primitive_enum_explicit_discriminants() {
    let result = run(stringify!(
        ret_i32(status_code(Status::NotFound));
        ret_i32(classify(200) as i32);
    ));
    assert_all(&result, &[404_i32, 200_i32]);
}

#[test]
fn primitive_enum_custom_repr() {
    // Color uses #[itsy(repr(u8))] so it marshals as u8, not i32
    let result = run(stringify!(
        ret_i32(color_index(Color::Red) as i32);
        ret_i32(color_index(Color::Blue) as i32);
        let c = next_color(Color::Blue);
        ret_i32(color_index(c) as i32);
        ret_i32(c as i32);
    ));
    assert_all(&result, &[0_i32, 2_i32, 0_i32, 0_i32]);
}

#[test]
fn data_enum_script_constructed() {
    let result = run(stringify!(
        ret_f64(area(Shape::Circle(2.0)));
        ret_f64(area(Shape::Rect(3.0, 4.0)));
        ret_f64(area(Shape::Empty));
    ));
    assert_all(&result, &[12.0_f64, 12.0_f64, 0.0_f64]);
}

#[test]
fn data_enum_rust_constructed_roundtrip() {
    let result = run(stringify!(
        ret_f64(area(make_circle(5.0)));
    ));
    assert_all(&result, &[75.0_f64]);
}

#[test]
fn data_enum_string_payload() {
    let result = run(stringify!(
        ret_string(shape_name(Shape::Named("hexagon")));
    ));
    assert_all(&result, &[String::from("hexagon")]);
}

#[test]
fn enum_as_struct_field() {
    // Tagged { dir: West(3), value: 10 } -> 13; round-trips through make_tagged too
    let result = run(stringify!(
        ret_i32(tag_index(Tagged { dir: Direction::East, value: 10 }));
        ret_i32(tag_index(make_tagged(5)));
    ));
    assert_all(&result, &[11_i32, 8_i32]);
}

#[test]
fn struct_as_enum_payload() {
    let result = run(stringify!(
        ret_i32(wrap_sum(Wrap::Pt(Point { x: 4, y: 9 })));
        ret_i32(wrap_sum(wrap_point(2, 3)));
        ret_i32(wrap_sum(Wrap::None));
    ));
    assert_all(&result, &[13_i32, 5_i32, -1_i32]);
}

#[test]
fn primitive_array_rust_to_script() {
    // make_range returns [u16] to the script, which indexes and re-passes it
    let result = run(stringify!(
        let r = make_range(5);
        ret_i32(r[0] as i32);
        ret_i32(r[4] as i32);
        ret_i32(sum_u16(r));
    ));
    assert_all(&result, &[0_i32, 4_i32, 10_i32]);
}

#[test]
fn primitive_array_script_to_rust() {
    let result = run(stringify!(
        let a: [ u16 ] = [ 3, 4, 5 ];
        ret_i32(sum_u16(a));
    ));
    assert_all(&result, &[12_i32]);
}

#[test]
fn empty_primitive_array() {
    let result = run(stringify!(
        ret_i32(sum_u16(make_range(0)));
        let a: [ u16 ] = [ ];
        ret_i32(sum_u16(a));
    ));
    assert_all(&result, &[0_i32, 0_i32]);
}

#[test]
fn string_array_roundtrip() {
    let result = run(stringify!(
        ret_string(join(make_words()));
        let parts = [ "x", "y", "z" ];
        ret_string(join(parts));
    ));
    assert_all(&result, &[String::from("a-bb-ccc"), String::from("x-y-z")]);
}

#[test]
fn struct_array_roundtrip() {
    let result = run(stringify!(
        ret_i32(points_sum(make_points(3)));
        let ps = [ Point { x: 1, y: 2 }, Point { x: 3, y: 4 } ];
        ret_i32(points_sum(ps));
    ));
    // make_points(3): (0,0),(1,2),(2,4) -> 0+3+6 = 9; literal: 3 + 7 = 10
    assert_all(&result, &[9_i32, 10_i32]);
}

#[test]
fn enum_array_passed_to_rust() {
    let result = run(stringify!(
        let shapes = [ Shape::Circle(2.0), Shape::Rect(3.0, 4.0), Shape::Empty ];
        ret_f64(shapes_area(shapes));
    ));
    assert_all(&result, &[24.0_f64]);
}

#[test]
fn nested_primitive_array_rust_to_script() {
    let result = run(stringify!(
        let g = make_grid(2, 3);
        ret_i32(g[0][2] as i32);
        ret_i32(grid_sum(g));
    ));
    // 2 rows of [0,1,2]; element [0][2] = 2; sum = 2 * (0+1+2) = 6
    assert_all(&result, &[2_i32, 6_i32]);
}

#[test]
fn nested_primitive_array_script_to_rust() {
    let result = run(stringify!(
        let g: [ [ u16 ] ] = [ [ 1, 2 ], [ 3 ], [ ] ];
        ret_i32(grid_sum(g));
    ));
    assert_all(&result, &[6_i32]);
}

#[test]
fn nested_string_array_script_to_rust() {
    let result = run(stringify!(
        let g = [ [ "a", "b" ], [ "c" ] ];
        ret_string(flatten_words(g));
    ));
    assert_all(&result, &[String::from("a,b,c")]);
}
