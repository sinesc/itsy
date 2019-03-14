//! Misc. utility code.

mod numeric;
pub use self::numeric::*;

mod typed_ids;
pub use self::typed_ids::*;

mod types;
pub use self::types::*;

pub(crate) fn array2(s: &[u8]) -> [u8; 2] {
    let mut array: [u8; 2] = unsafe { std::mem::uninitialized() };
    array.copy_from_slice(&s[0..2]);
    array
}

pub(crate) fn array4(s: &[u8]) -> [u8; 4] {
    let mut array: [u8; 4] = unsafe { std::mem::uninitialized() };
    array.copy_from_slice(&s[0..4]);
    array
}

pub(crate) fn array8(s: &[u8]) -> [u8; 8] {
    let mut array: [u8; 8] = unsafe { std::mem::uninitialized() };
    array.copy_from_slice(&s[0..8]);
    array
}
