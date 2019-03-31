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
