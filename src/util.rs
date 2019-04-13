//! Misc. utility code.

mod numeric;
pub use self::numeric::*;

mod typed_ids;
pub use self::typed_ids::*;

mod types;
pub use self::types::*;

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn array1(s: &[u8]) -> [u8; 1] {
    [ s[0] ]
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn array2(s: &[u8]) -> [u8; 2] {
    [ s[0], s[1] ]
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn array4(s: &[u8]) -> [u8; 4] {
    [ s[0], s[1], s[2], s[3] ]
}

#[cfg_attr(not(debug_assertions), inline(always))]
pub(crate) fn array8(s: &[u8]) -> [u8; 8] {
    [ s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7] ]
}

/// Compute line/column number from absolute position in string
pub(crate) fn compute_position(input: &str, position: u32) -> (u32, u32) {
    let mut parsed = &input[0..position as usize];
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
    (line as u32, parsed.len() as u32 + 1)
}