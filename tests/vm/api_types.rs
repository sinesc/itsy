//! Tests for passing `#[derive(VMValue)]` structs across the Itsy API boundary.
//!
//! Each test builds and runs a small program against a custom API that constructs, consumes and
//! round-trips structs. Results are pushed into the `Context` via `ret_*` helpers and asserted with
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

const PRELUDE: &str = "use Api::{make_point, point_sum, make_person, person_summary, make_line, line_dx, dir_index, opposite, status_code, classify, area, make_circle, shape_name, tag_index, make_tagged, wrap_sum, wrap_point, ret_i32, ret_f64, ret_string};";

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
