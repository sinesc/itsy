//! Tests for the host->script call direction: top-level Itsy functions invoked from Rust via the
//! typed wrappers generated from an `itsy_api!` `callables { ... }` block. These exercise marshalling
//! `#[derive(VMValue)]` structs/enums and arrays (`Vec<T>`) *into* a call and reading them back *out*.
//!
//! Because `VMValue::to_stack` allocates reference types at refcount 0 and the callee epilogue releases
//! reference parameters, these also catch reference-counting mistakes: after a clean round-trip the heap
//! must be back to its initial size (only the reserved element 0), asserted via `vm.heap.len()`.

use itsy::{itsy_api, build_str, VMValue};
use itsy::runtime::VM;
use crate::util::Context;

#[derive(VMValue, PartialEq, Debug)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(VMValue, PartialEq, Debug)]
struct Person {
    name: String,
    age: u8,
}

#[derive(VMValue, PartialEq, Debug)]
struct Line {
    from: Point,
    to: Point,
}

#[derive(VMValue, PartialEq, Debug)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(VMValue, PartialEq, Debug)]
enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Named(String),
    Empty,
}

itsy_api! {
    Api<Context> {
        // at least one regular host function is required by the generated dispatch
        fn host_noop(&mut context) { }

        callables {
            // primitives
            fn add(a: i32, b: i32) -> i32;
            fn negate(v: bool) -> bool;
            fn no_return(x: i32);
            // strings
            fn greet(name: String) -> String;
            // structs (struct arg, struct return, nested)
            fn point_sum(p: Point) -> i32;
            fn make_point(x: i32, y: i32) -> Point;
            fn make_person(name: String, age: u8) -> Person;
            fn line_dx(l: Line) -> i32;
            fn make_line(x1: i32, y1: i32, x2: i32, y2: i32) -> Line;
            // enums
            fn opposite(d: Direction) -> Direction;
            fn area(s: Shape) -> f64;
            fn make_circle(r: f64) -> Shape;
            fn shape_name(s: Shape) -> String;
            // arrays
            fn sum(values: [ i32 ]) -> i32;
            fn range(n: i32) -> [ i32 ];
            fn points_sum(ps: [ Point ]) -> i32;
            fn make_points(n: i32) -> [ Point ];
            // runtime error propagation
            fn divide(a: i32, b: i32) -> i32;
        }
    }
}

const SCRIPT: &str = "
    use Api::{Point, Person, Line, Direction, Shape};

    fn add(a: i32, b: i32) -> i32 { a + b }
    fn negate(v: bool) -> bool { v == false }
    fn no_return(x: i32) { }
    fn greet(name: String) -> String { \"Hello, {name}!\" }
    fn point_sum(p: Point) -> i32 { p.x + p.y }
    fn make_point(x: i32, y: i32) -> Point { Point { x: x, y: y } }
    fn make_person(name: String, age: u8) -> Person { Person { name: name, age: age } }
    fn line_dx(l: Line) -> i32 { l.to.x - l.from.x }
    fn opposite(d: Direction) -> Direction {
        match d {
            Direction::North => Direction::South,
            Direction::South => Direction::North,
            Direction::East => Direction::West,
            Direction::West => Direction::East,
        }
    }
    fn area(s: Shape) -> f64 {
        match s {
            Shape::Circle(r) => 3.0 * r * r,
            Shape::Rect(w, h) => w * h,
            Shape::Named(n) => 0.0 - 1.0,
            Shape::Empty => 0.0,
        }
    }
    fn make_circle(r: f64) -> Shape { Shape::Circle(r) }
    fn shape_name(s: Shape) -> String {
        match s {
            Shape::Named(n) => n,
            _ => \"?\",
        }
    }
    fn make_line(x1: i32, y1: i32, x2: i32, y2: i32) -> Line {
        Line { from: Point { x: x1, y: y1 }, to: Point { x: x2, y: y2 } }
    }
    fn divide(a: i32, b: i32) -> i32 { a / b }
    fn make_points(n: i32) -> [ Point ] {
        let mut result = [ ];
        let mut i = 0;
        while i < n { result.push(Point { x: i, y: i * 2 }); i += 1; }
        result
    }
    fn sum(values: [ i32 ]) -> i32 {
        let mut total = 0;
        for v in values { total += v; }
        total
    }
    fn range(n: i32) -> [ i32 ] {
        let mut result = [ ];
        let mut i = 0;
        while i < n { result.push(i); i += 1; }
        result
    }
    fn points_sum(ps: [ Point ]) -> i32 {
        let mut total = 0;
        for p in ps { total += p.x + p.y; }
        total
    }

    fn main() { }
";

fn build() -> VM<Api, Context> {
    let program = match build_str::<Api>(SCRIPT) {
        Ok(program) => program,
        Err(err) => {
            let loc = err.loc(SCRIPT);
            panic!("{} in line {}, column {}.", err, loc.0, loc.1);
        }
    };
    VM::new(program)
}

#[test]
fn primitive_args_and_return() {
    let mut vm = build();
    let mut ctx = Vec::new();
    assert_eq!(vm.add(&mut ctx, 2, 40).unwrap(), 42);
    assert_eq!(vm.negate(&mut ctx, true).unwrap(), false);
    assert_eq!(vm.negate(&mut ctx, false).unwrap(), true);
}

#[test]
fn void_return() {
    let mut vm = build();
    let mut ctx = Vec::new();
    vm.no_return(&mut ctx, 7).unwrap();
    assert_eq!(vm.heap.len(), 1, "heap leaked after void call");
}

#[test]
fn string_roundtrip_clean_heap() {
    let mut vm = build();
    let mut ctx = Vec::new();
    let result = vm.greet(&mut ctx, "World".to_string()).unwrap();
    assert_eq!(result, "Hello, World!".to_string());
    assert_eq!(vm.heap.len(), 1, "heap leaked after string call");
}

#[test]
fn struct_argument() {
    let mut vm = build();
    let mut ctx = Vec::new();
    assert_eq!(vm.point_sum(&mut ctx, Point { x: 3, y: 4 }).unwrap(), 7);
    assert_eq!(vm.heap.len(), 1, "heap leaked after struct-arg call");
}

#[test]
fn struct_return() {
    let mut vm = build();
    let mut ctx = Vec::new();
    assert_eq!(vm.make_point(&mut ctx, 5, 9).unwrap(), Point { x: 5, y: 9 });
    assert_eq!(vm.heap.len(), 1, "heap leaked after struct-return call");
}

#[test]
fn struct_with_string_field() {
    let mut vm = build();
    let mut ctx = Vec::new();
    let p = vm.make_person(&mut ctx, "Ada".to_string(), 36).unwrap();
    assert_eq!(p, Person { name: "Ada".to_string(), age: 36 });
    assert_eq!(vm.heap.len(), 1, "heap leaked after struct-with-string call");
}

#[test]
fn nested_struct_argument() {
    let mut vm = build();
    let mut ctx = Vec::new();
    let line = Line { from: Point { x: 1, y: 1 }, to: Point { x: 8, y: 3 } };
    assert_eq!(vm.line_dx(&mut ctx, line).unwrap(), 7);
    assert_eq!(vm.heap.len(), 1, "heap leaked after nested-struct call");
}

#[test]
fn enum_roundtrip() {
    let mut vm = build();
    let mut ctx = Vec::new();
    assert_eq!(vm.opposite(&mut ctx, Direction::North).unwrap(), Direction::South);
    assert_eq!(vm.opposite(&mut ctx, Direction::East).unwrap(), Direction::West);
}

#[test]
fn data_enum_argument_and_return() {
    let mut vm = build();
    let mut ctx = Vec::new();
    assert_eq!(vm.area(&mut ctx, Shape::Rect(2.0, 5.0)).unwrap(), 10.0);
    assert_eq!(vm.area(&mut ctx, Shape::Circle(2.0)).unwrap(), 12.0);
    assert_eq!(vm.make_circle(&mut ctx, 3.0).unwrap(), Shape::Circle(3.0));
    assert_eq!(vm.heap.len(), 1, "heap leaked after data-enum call");
}

#[test]
fn array_argument() {
    let mut vm = build();
    let mut ctx = Vec::new();
    assert_eq!(vm.sum(&mut ctx, vec![1, 2, 3, 4]).unwrap(), 10);
    assert_eq!(vm.heap.len(), 1, "heap leaked after array-arg call");
}

#[test]
fn array_return() {
    let mut vm = build();
    let mut ctx = Vec::new();
    assert_eq!(vm.range(&mut ctx, 4).unwrap(), vec![0, 1, 2, 3]);
    assert_eq!(vm.heap.len(), 1, "heap leaked after array-return call");
}

#[test]
fn array_of_structs_argument() {
    let mut vm = build();
    let mut ctx = Vec::new();
    let points = vec![Point { x: 1, y: 2 }, Point { x: 3, y: 4 }];
    assert_eq!(vm.points_sum(&mut ctx, points).unwrap(), 10);
    assert_eq!(vm.heap.len(), 1, "heap leaked after array-of-structs call");
}

#[test]
fn nested_struct_return() {
    let mut vm = build();
    let mut ctx = Vec::new();
    let line = vm.make_line(&mut ctx, 1, 1, 8, 3).unwrap();
    assert_eq!(line, Line { from: Point { x: 1, y: 1 }, to: Point { x: 8, y: 3 } });
    assert_eq!(vm.heap.len(), 1, "heap leaked after nested-struct-return call");
}

#[test]
fn array_of_structs_return() {
    let mut vm = build();
    let mut ctx = Vec::new();
    let points = vm.make_points(&mut ctx, 3).unwrap();
    assert_eq!(points, vec![Point { x: 0, y: 0 }, Point { x: 1, y: 2 }, Point { x: 2, y: 4 }]);
    assert_eq!(vm.heap.len(), 1, "heap leaked after array-of-structs-return call");
}

#[test]
fn data_enum_string_payload_argument() {
    let mut vm = build();
    let mut ctx = Vec::new();
    let name = vm.shape_name(&mut ctx, Shape::Named("triangle".to_string())).unwrap();
    assert_eq!(name, "triangle".to_string());
    assert_eq!(vm.heap.len(), 1, "heap leaked after data-enum-string-payload call");
}

#[test]
fn runtime_error_propagates() {
    use itsy::runtime::CallError;
    let mut vm = build();
    let mut ctx = Vec::new();
    match vm.divide(&mut ctx, 10, 0) {
        Err(CallError::Runtime(_)) => {},
        other => panic!("expected runtime error, got {:?}", other),
    }
}

// --- Phase 2: compile-time validation that the script defines each callable with a matching signature.

// A small second API whose callables are used to build deliberately-mismatched scripts.
itsy_api! {
    Mini<Context> {
        fn mini_noop(&mut context) { }
        callables {
            fn add(a: i32, b: i32) -> i32;
            fn describe(p: Point) -> String;
        }
    }
}

/// Builds `script` against `Mini`, expecting failure, and returns the rendered error.
fn mini_err(script: &str) -> String {
    match build_str::<Mini>(script) {
        Ok(_) => panic!("expected build to fail for script:\n{}", script),
        Err(err) => err.to_string(),
    }
}

#[test]
fn rejects_missing_callable() {
    let err = mini_err("
        use Mini::Point;
        fn describe(p: Point) -> String { \"x\" }
        fn main() { }
    ");
    assert!(err.contains("add"), "got: {}", err);
}

#[test]
fn rejects_wrong_primitive_type() {
    let err = mini_err("
        use Mini::Point;
        fn add(a: f32, b: f32) -> f32 { a + b }
        fn describe(p: Point) -> String { \"x\" }
        fn main() { }
    ");
    assert!(err.contains("f32") && err.contains("i32"), "got: {}", err);
}

#[test]
fn rejects_wrong_arity() {
    let err = mini_err("
        use Mini::Point;
        fn add(a: i32) -> i32 { a }
        fn describe(p: Point) -> String { \"x\" }
        fn main() { }
    ");
    assert!(err.to_lowercase().contains("argument"), "got: {}", err);
}

#[test]
fn rejects_wrong_return_type() {
    let err = mini_err("
        use Mini::Point;
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn describe(p: Point) -> i32 { p.x }
        fn main() { }
    ");
    assert!(err.contains("String"), "got: {}", err);
}

#[test]
fn rejects_wrong_struct_type() {
    let err = mini_err("
        struct Other { a: i32 }
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn describe(p: Other) -> String { \"x\" }
        fn main() { }
    ");
    assert!(err.contains("Point") && err.contains("Other"), "got: {}", err);
}

#[test]
fn accepts_matching_signatures() {
    let program = build_str::<Mini>("
        use Mini::Point;
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn describe(p: Point) -> String { \"({p.x}, {p.y})\" }
        fn main() { }
    ");
    assert!(program.is_ok(), "expected build to succeed: {:?}", program.err().map(|e| e.to_string()));
}
