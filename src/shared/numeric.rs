//! A type that can represent a signed or unsigned integer value.

use crate::prelude::*;

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
    //Overflow,
}

impl Hash for Numeric {
    fn hash<H: Hasher>(self: &Self, state: &mut H) {
        match self {
            Self::Signed(s) => s.hash(state),
            Self::Unsigned(u) => u.hash(state),
            Self::Float(f) => {
                if f.is_nan() {
                    f64::NAN.to_ne_bytes().hash(state)
                } else {
                    f.to_ne_bytes().hash(state)
                }
            },
        }
    }
}

impl Numeric {
    pub const fn is_integer(self: &Self) -> bool {
        match self {
            Numeric::Signed(_) => true,
            Numeric::Unsigned(_) => true,
            _ => false,
        }
    }
    pub const fn is_float(self: &Self) -> bool {
        match self {
            Numeric::Float(_) => true,
            _ => false,
        }
    }
    pub fn inc(self: Self) -> Self {
        match self {
            Self::Signed(v) => Self::Signed(v + 1),
            Self::Unsigned(v) => Self::Unsigned(v + 1),
            Self::Float(v) => Self::Float(v + 1.0),
        }
    }
    pub const fn as_signed(self: &Self) -> Option<i64> {
        match self {
            &Numeric::Signed(v) => Some(v as i64),
            &Numeric::Unsigned(v) if v <= i64::MAX as u64 => Some(v as i64),
            _ => None,
        }
    }
    pub const fn as_unsigned(self: &Self) -> Option<u64> {
        match self {
            &Numeric::Signed(v) if v >= 0 => Some(v as u64),
            &Numeric::Unsigned(v) => Some(v as u64),
            _ => None,
        }
    }
    pub const fn as_float(self: &Self) -> Option<f64> {
        match self {
            Numeric::Float(v) => Some(*v as f64),
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
            //Numeric::Overflow => None
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
            Numeric::Signed(v) => write!(f, "{v} (signed)"),
            Numeric::Unsigned(v) => write!(f, "{v} (unsigned)"),
            Numeric::Float(v) => write!(f, "{v} (float)"),
            //Numeric::Overflow => write!(f, "/ (numeric value too large)"),
        }
    }
}

impl Display for Numeric {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Numeric::Signed(v) => write!(f, "{v}"),
            Numeric::Unsigned(v) => write!(f, "{v}"),
            Numeric::Float(v) => write!(f, "{v}"),
            //Numeric::Overflow => write!(f, "/ (numeric value too large)"),
        }
    }
}