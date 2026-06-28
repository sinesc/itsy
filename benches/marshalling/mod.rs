//! Marshalling benchmark types and API.
//!
//! `#[derive(VMValue)]` structs/enums and the `itsy_api!` functions that exercise
//! cross-boundary marshalling for the `marshalling/*` benchmark cases.

use itsy::VMValue;

// --- Marshalling benchmark types ---

#[derive(VMValue)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

#[derive(VMValue)]
pub struct Record {
    pub id: u32,
    pub name: String,
    pub value: f64,
}

#[derive(VMValue)]
pub struct Nested {
    pub pos: Point,
    pub label: String,
    pub score: u32,
}

#[derive(VMValue, PartialEq)]
pub enum Status {
    Ok,
    Error,
    Pending,
}

#[derive(VMValue)]
pub enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Named(String),
    Empty,
}
