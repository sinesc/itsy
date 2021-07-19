//! Shared code for frontend and bytecode

pub mod numeric;
pub mod typed_ids;
pub mod types;
pub mod error;
pub mod info;

use crate::frontend::ast::Position;

/// Compute line/column number from absolute offset in string
pub fn compute_loc(input: &str, offset: Position) -> (Position, Position) {
    let mut parsed = &input[0..offset as usize];
    let mut line = 1;
    while { // can't use let parsed.lines() here as a line-break at the end is ignored
        let mut break_char = '\0';
        if let Some(nl) = parsed.find(|c| if c == '\n' || c == '\r' { break_char = c; true } else { false }) {
            parsed = &parsed[nl+1..];
            if break_char == '\r' && parsed.starts_with('\n') { // skip \n after \r on windows
                parsed = &parsed[1..];
            }
            line += 1;
            true
        } else {
            false
        }
    } {}
    (line as Position, parsed.len() as Position + 1)
}

/// References two elements of a slice mutably
pub fn index_twice<T>(slice: &mut [T], a: usize, b: usize) -> (&mut T, &mut T) {
    if a == b {
        panic!("tried to index element {} twice", a);
    } else if a >= slice.len() || b >= slice.len() {
        panic!("index ({}, {}) out of bounds", a, b);
    } else {
        if a > b {
            let (left, right) = slice.split_at_mut(a);
            (&mut right[0], &mut left[b])
        } else {
            let (left, right) = slice.split_at_mut(b);
            (&mut left[a], &mut right[0])
        }
    }
}

#[test]
fn test_index_twice() {
    let mut data = [ 1i32, 2, 3, 4, 5];

    {
        let (a, b) = index_twice(&mut data, 0, 4);  // b > a at bounds
        *a = -1;
        *b = -5;
    }
    assert!(data == [ -1i32, 2, 3, 4, -5 ]);

    {
        let (a, b) = index_twice(&mut data, 4, 0);  // a > b at bounds
        *a = -10;
        *b = -5;
    }
    assert!(data == [ -5i32, 2, 3, 4, -10 ]);

    {
        let (a, b) = index_twice(&mut data, 3, 2);  // a > b adjacent
        *a = 11;
        *b = 22;
    }
    assert!(data == [ -5i32, 2, 22, 11, -10 ]);

    {
        let (a, b) = index_twice(&mut data, 1, 2);  // b > a adjacent
        *a = 33;
        *b = 44;
    }
    assert!(data == [ -5i32, 33, 44, 11, -10 ]);
}
