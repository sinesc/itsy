
use crate::StackAddress;
use crate::frontend::ast::Pattern;

#[derive(Clone, Copy)]
pub struct CallInfo {
    pub arg_size: StackAddress,
    pub addr: StackAddress,
}

impl CallInfo {
    pub const PLACEHOLDER: Self = Self { addr: 123, arg_size: 0 };
}

#[derive(Debug)]
pub enum CoverageRangeKind<'p, T> {
    Gap,
    Match(&'p Pattern),
    Partial(&'p Pattern, Box<Coverage<'p, T>>),
}

#[derive(Debug)]
pub struct CoverageRange<'p, T> {
    left: T,
    right: T,
    kind: CoverageRangeKind<'p, T>,
}

#[derive(Debug)]
pub struct Coverage<'p, T> {
    left: T,
    right: T,
    ranges: Vec<CoverageRange<'p, T>>,
}

impl<'p, T> Coverage<'p, T> where T: Copy + Ord + std::ops::Add<Output = T> + From<u8> {
    /// Creates a new coverage with given inclusive left and exclusive right bound.
    pub fn new(left: T, right: T) -> Self {
        Self {
            left,
            right,
            ranges: Vec::new(),
        }
    }
    /// Adds a range to the coverage.
    pub fn add_range(self: &mut Self, left: T, right: T, kind: CoverageRangeKind<'p, T>) {
        self.ranges.push(CoverageRange { left, right, kind });
    }
    /// Adds an integer value to the coverage (range with right = left + 1).
    pub fn add_integer(self: &mut Self, value: T, kind: CoverageRangeKind<'p, T>) {
        // note: limiting right bound to self.right should be ok since it is explicitly checked in validate via self.right > max_covered
        // if we didn't limit here we'd need a larger datatype to be able to add 1.
        self.ranges.push(CoverageRange { left: value, right: if value != self.right { value + T::from(1) } else { value }, kind });
    }
    /// Validates that the ranges cover the entire coverage.
    pub fn validate(self: &Self) -> Option<CoverageRange<T>> {
        // sort ranges
        let ranges = {
            let mut ranges: Vec<_> = self.ranges.iter().collect();
            ranges.sort_by(|&a, &b| a.left.cmp(&b.left));
            ranges
        };
        // check if left is covered
        let first = *ranges.first().unwrap();
        if self.left < first.left {
            return Some(CoverageRange { left: self.left, right: first.left, kind: CoverageRangeKind::Gap });
        }
        // check for gaps
        let mut max_covered = first.right;
        if ranges.len() > 1 {
            for i in 1..ranges.len() {
                if ranges[i].left > max_covered {
                    return Some(CoverageRange { left: max_covered, right: ranges[i].left, kind: CoverageRangeKind::Gap });
                }
                if ranges[i].right > max_covered {
                    max_covered = ranges[i].right;
                }
            }
        }
        // check if right is covered
        if self.right > max_covered {
            return Some(CoverageRange { left: max_covered, right: self.right, kind: CoverageRangeKind::Gap });
        }
        // check partial ranges recursively
        for range in ranges {
            if let CoverageRangeKind::Partial(index, partial) = &range.kind {
                let result = partial.validate();
                if result.is_some() {
                    return result;
                }
            }
        }
        None
    }
}