//! A type that can represent a signed or unsigned integer value.

use std::cmp::Ordering;
use std::fmt::{self, Debug};
use std::ops::{Add, Sub, Mul, Div, Rem};
//use std::{i64, u64, f64};

/// Signed integer value.
pub(crate) type Signed = i64;

/// Unsigned integer value.
pub(crate) type Unsigned = u64;

// A float value.
pub(crate) type Float = f64;

/// Numeric type supporting signed and unsigned integers as well as float values. Used to represent literal values in the AST.
#[derive(Clone, Copy)]
pub enum Numeric {
    Signed(Signed),
    Unsigned(Unsigned),
    Float(Float),
    Overflow,
}

impl Numeric {
    pub fn is_integer(self: &Self) -> bool {
        match self {
            Numeric::Signed(_) => true,
            Numeric::Unsigned(_) => true,
            _ => false,
        }
    }
    pub fn is_signed(self: &Self) -> bool {
        match self {
            Numeric::Signed(_) => true,
            _ => false,
        }
    }
    pub fn is_unsigned(self: &Self) -> bool {
        match self {
            Numeric::Unsigned(_) => true,
            _ => false,
        }
    }
    pub fn is_float(self: &Self) -> bool {
        match self {
            Numeric::Float(_) => true,
            _ => false,
        }
    }
    pub fn as_signed(self: &Self) -> Option<i64> {
        match self {
            Numeric::Signed(v) => Some(*v as i64),
            Numeric::Unsigned(v) => Some(*v as i64),
            _ => None,
        }
    }
    pub fn as_unsigned(self: &Self) -> Option<u64> {
        match self {
            Numeric::Signed(v) => Some(*v as u64),
            Numeric::Unsigned(v) => Some(*v as u64),
            _ => None,
        }
    }
    pub fn as_float(self: &Self) -> Option<f64> {
        match self {
            Numeric::Float(v) => Some(*v as f64),
            _ => None,
        }
    }
    fn as_f64(self: &Self) -> Option<f64> {
        match self {
            Numeric::Float(v) => Some(*v as f64),
            _ => None,
        }
    }
    fn as_i128(self: &Self) -> Option<i128> {
        match self {
            Numeric::Signed(v) => Some(*v as i128),
            Numeric::Unsigned(v) => Some(*v as i128),
            _ => None,
        }
    }
}

impl PartialEq for Numeric {
    fn eq(self: &Self, other: &Numeric) -> bool {
        if let Some(ordering) = self.partial_cmp(other) {
            ordering == Ordering::Equal
        } else {
            false
        }
    }
}

impl Eq for Numeric { }

impl PartialOrd for Numeric {
    fn partial_cmp(&self, other: &Numeric) -> Option<Ordering> {
        match self {
            Numeric::Signed(s) => {
                match other {
                    Numeric::Signed(o) => {
                        s.partial_cmp(o)
                    },
                    Numeric::Unsigned(o) => {
                        if *s < 0 {
                            Some(Ordering::Less)
                        } else {
                            (*s as Unsigned).partial_cmp(o)
                        }
                    },
                    _ => None,
                }
            },
            Numeric::Unsigned(s) => {
                match other {
                    Numeric::Signed(o) => {
                        if *o < 0 {
                            Some(Ordering::Greater)
                        } else {
                            s.partial_cmp(&(*o as Unsigned))
                        }
                    },
                    Numeric::Unsigned(o) => {
                        s.partial_cmp(o)
                    }
                    _ => None,
                }
            },
            Numeric::Float(s) => {
                match other {
                    Numeric::Float(o) => {
                        s.partial_cmp(o)
                    }
                    _ => None,
                }
            },
            Numeric::Overflow => None
        }
    }
}

impl Ord for Numeric {
    fn cmp(&self, other: &Numeric) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Add for Numeric {
    type Output = Numeric;
    fn add(self: Self, other: Numeric) -> Numeric {
        if self.is_integer() {
            let result = self.as_i128().unwrap() + other.as_i128().unwrap();
            if result >= 0 && result <= std::u64::MAX as i128 {
                Numeric::Unsigned(result as Unsigned)
            } else if result <= 0 && result >= std::i64::MIN as i128 {
                Numeric::Signed(result as Signed)
            } else {
                Numeric::Overflow
            }
        } else {
            Numeric::Float(self.as_f64().unwrap() + other.as_f64().unwrap())
        }
    }
}

impl Sub for Numeric {
    type Output = Numeric;
    fn sub(self: Self, other: Numeric) -> Numeric {
        if self.is_integer() {
            let result = self.as_i128().unwrap() - other.as_i128().unwrap();
            if result >= 0 && result <= std::u64::MAX as i128 {
                Numeric::Unsigned(result as Unsigned)
            } else if result <= 0 && result >= std::i64::MIN as i128 {
                Numeric::Signed(result as Signed)
            } else {
                Numeric::Overflow
            }
        } else {
            Numeric::Float(self.as_f64().unwrap() - other.as_f64().unwrap())
        }
    }
}

impl Mul for Numeric {
    type Output = Numeric;
    fn mul(self: Self, other: Numeric) -> Numeric {
        if self.is_integer() {
            let result = self.as_i128().unwrap() * other.as_i128().unwrap();
            if result >= 0 && result <= std::u64::MAX as i128 {
                Numeric::Unsigned(result as Unsigned)
            } else if result <= 0 && result >= std::i64::MIN as i128 {
                Numeric::Signed(result as Signed)
            } else {
                Numeric::Overflow
            }
        } else {
            Numeric::Float(self.as_f64().unwrap() * other.as_f64().unwrap())
        }
    }
}

impl Div for Numeric {
    type Output = Numeric;
    fn div(self: Self, other: Numeric) -> Numeric {
        if self.is_integer() {
            let result = self.as_i128().unwrap() / other.as_i128().unwrap();
            if result >= 0 && result <= std::u64::MAX as i128 {
                Numeric::Unsigned(result as Unsigned)
            } else if result <= 0 && result >= std::i64::MIN as i128 {
                Numeric::Signed(result as Signed)
            } else {
                Numeric::Overflow
            }
        } else {
            Numeric::Float(self.as_f64().unwrap() / other.as_f64().unwrap())
        }
    }
}

impl Rem for Numeric {
    type Output = Numeric;
    fn rem(self: Self, other: Numeric) -> Numeric {
        if self.is_integer() {
            let result = self.as_i128().unwrap() % other.as_i128().unwrap();
            if result >= 0 && result <= std::u64::MAX as i128 {
                Numeric::Unsigned(result as Unsigned)
            } else if result <= 0 && result >= std::i64::MIN as i128 {
                Numeric::Signed(result as Signed)
            } else {
                Numeric::Overflow
            }
        } else {
            Numeric::Float(self.as_f64().unwrap() % other.as_f64().unwrap())
        }
    }
}

impl Debug for Numeric {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Numeric::Signed(v) => write!(f, "Signed({})", v),
            Numeric::Unsigned(v) => write!(f, "Unsigned({})", v),
            Numeric::Float(v) => write!(f, "Float({})", v),
            Numeric::Overflow => write!(f, "Overflow"),
        }
    }
}