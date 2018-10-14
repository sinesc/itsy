//! A type that can represent a signed or unsigned integer value.

use std::cmp::Ordering;
use std::fmt::{self, Debug};

/// Signed integer value.
pub type Signed = i64;

/// Unsigned integer value.
pub type Unsigned = u64;

// A float value.
pub type Float = f64;

/// A type supporting signed and unsigned integral values. Used in the AST to store literal values.
#[derive(Clone, Copy)]
pub enum Numeric {
    Signed(Signed),
    Unsigned(Unsigned),
    Float(Float),
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
        }
    }
}

impl Ord for Numeric {
    fn cmp(&self, other: &Numeric) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Debug for Numeric {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Numeric::Signed(v) => write!(f, "Signed({})", v),
            Numeric::Unsigned(v) => write!(f, "Unsigned({})", v),
            Numeric::Float(v) => write!(f, "Float({})", v),
        }
    }
}