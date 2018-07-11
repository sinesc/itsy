use std::cmp::Ordering;

pub type Signed = i64;
pub type Unsigned = u64;

/// A type supporting signed and unsigned integral values.
#[derive(Clone, Copy, Debug)]
pub enum Integer {
    Signed(Signed),
    Unsigned(Unsigned),
}

impl Integer {
    pub fn is_signed(self: &Self) -> bool {
        match self {
            Integer::Signed(_) => true,
            _ => false,
        }
    }
    pub fn is_unsigned(self: &Self) -> bool {
        match self {
            Integer::Unsigned(_) => true,
            _ => false,
        }
    }
}

impl PartialEq for Integer {
    fn eq(self: &Self, other: &Integer) -> bool {
        if let Some(ordering) = self.partial_cmp(other) {
            ordering == Ordering::Equal
        } else {
            false
        }
    }
}

impl Eq for Integer { }

impl PartialOrd for Integer {
    fn partial_cmp(&self, other: &Integer) -> Option<Ordering> {
        match self {
            Integer::Signed(s) => {
                match other {
                    Integer::Signed(o) => {
                        s.partial_cmp(o)
                    },
                    Integer::Unsigned(o) => {
                        if *s < 0 {
                            Some(Ordering::Less)
                        } else {
                            (*s as Unsigned).partial_cmp(o)
                        }
                    }
                }
            },
            Integer::Unsigned(s) => {
                match other {
                    Integer::Signed(o) => {
                        if *o < 0 {
                            Some(Ordering::Greater)
                        } else {
                            s.partial_cmp(&(*o as Unsigned))
                        }
                    },
                    Integer::Unsigned(o) => {
                        s.partial_cmp(o)
                    }
                }
            }
        }
    }
}

impl Ord for Integer {
    fn cmp(&self, other: &Integer) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}