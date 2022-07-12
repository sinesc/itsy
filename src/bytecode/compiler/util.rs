#![allow(unused_variables)]
#![allow(dead_code)]

use crate::prelude::*;
use crate::{FrameAddress, StackAddress};
use crate::frontend::ast::Pattern;
use crate::shared::typed_ids::FunctionId;
use std::cmp::Ordering;
use core::fmt::Debug;

#[derive(Clone, Copy)]
pub struct CallInfo {
    pub arg_size: FrameAddress,
    pub addr: StackAddress,
}

impl CallInfo {
    pub const PLACEHOLDER: Self = Self { addr: 123, arg_size: 0 };
}

/// Tracks call and function addresses so that calls made before the function address was known can be fixed.
pub struct Functions {
    // Maps functions to their call address (and arg_size required to write the call).
    functions: UnorderedMap<FunctionId, CallInfo>,
    /// Bytecode locations of function call instructions that need their target address fixed (because the target wasn't written yet).
    call_placeholder: UnorderedMap<FunctionId, Vec<StackAddress>>,
}

impl Functions {
    /// Creates a new instance.
    pub fn new() -> Self {
        Self {
            functions: UnorderedMap::new(),
            call_placeholder: UnorderedMap::new(),
        }
    }
    /// Registers an absolute function address and returns a list of placeholder positions that need to be replaced with calls to this function.
    pub fn register_function(self: &mut Self, function_id: FunctionId, arg_size: FrameAddress, addr: StackAddress) -> Option<Vec<StackAddress>> {
        let call_info = CallInfo { addr, arg_size };
        self.functions.insert(function_id, call_info);
        self.call_placeholder.remove(&function_id)
    }
    /// Either returns actual function address and arg_size or registers the call for later replacement and returns placeholder data.
    pub fn register_call(self: &mut Self, function_id: FunctionId, writer_position: StackAddress) -> CallInfo {
        if let Some(&target) = self.functions.get(&function_id) {
            target
        } else {
            self.call_placeholder.entry(function_id).or_insert(Vec::new()).push(writer_position);
            CallInfo::PLACEHOLDER
        }
    }
    /// Returns the call information for the given function, if available.
    pub fn get(self: &Self, function_id: FunctionId) -> Option<&CallInfo> {
        self.functions.get(&function_id)
    }
}

/// Loop break/continue jump instruction position.
pub(crate) enum LoopControl {
    Continue(StackAddress),
    Break(StackAddress),
}

/// Tracks loop break/continue jump instruction placeholders for later replacement (once addresses are known)
pub(crate) struct LoopControlStack {
    stack: Vec<Vec<LoopControl>>,
}

impl LoopControlStack {
    /// Creates a new loop control stack.
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
        }
    }
    /// Push another loop structure onto the loopcontrol stack
    pub fn push(self: &mut Self) {
        self.stack.push(Vec::new());
    }
    /// Pop the top loop structure off the loopcontrol stack
    pub fn pop(self: &mut Self) -> Vec<LoopControl> {
        self.stack.pop().unwrap()
    }
    /// Register a loop control instruction placceholder.
    pub fn add_jump(self: &mut Self, item: LoopControl) {
        self.stack.last_mut().unwrap().push(item);
    }
}

#[derive(Debug)]
pub enum CoverageRangeType<'p> {
    Unsigned(CoverageRange<'p, u64>),
    Signed(CoverageRange<'p, i64>),
    Float(CoverageRange<'p, f64>),
}

impl<'p> CoverageRangeType<'p> {
    fn kind(self: &'p Self) -> &'p CoverageRangeKind {
        match self {
            Self::Unsigned(coverage) => &coverage.kind,
            Self::Signed(coverage) => &coverage.kind,
            Self::Float(coverage) => &coverage.kind,
        }
    }
}

impl<'p> From<CoverageRange<'p, u64>> for CoverageRangeType<'p> {
    fn from(source: CoverageRange<'p, u64>) -> Self {
        CoverageRangeType::Unsigned(source)
    }
}

impl<'p> From<CoverageRange<'p, i64>> for CoverageRangeType<'p> {
    fn from(source: CoverageRange<'p, i64>) -> Self {
        CoverageRangeType::Signed(source)
    }
}

impl<'p> From<CoverageRange<'p, f64>> for CoverageRangeType<'p> {
    fn from(source: CoverageRange<'p, f64>) -> Self {
        CoverageRangeType::Float(source)
    }
}

#[derive(Debug)]
pub enum CoverageRangeKind<'p> {
    /// Input pattern fully covering this range.
    Match(&'p Pattern),
    /// Input pattern partially covering this range, recursion required (e.g. into enum data)
    Partial(&'p Pattern, Vec<CoverageType<'p>>),
    /// A gap discovered in validation.
    Gap,
    /// Reported by validation if the coverage is complete.
    Covered,
}

#[derive(Debug)]
pub struct CoverageRange<'p, T> {
    left: T,
    right: Option<T>,
    kind: CoverageRangeKind<'p>,
}

#[derive(Debug)]
pub enum CoverageType<'p> {
    Unsigned(Coverage<'p, u64>),
    Signed(Coverage<'p, i64>),
    Float(Coverage<'p, f64>),
}

impl<'p> CoverageType<'p> {
    fn validate(self: &'p Self) -> CoverageRangeType<'p> {
        match self {
            CoverageType::Unsigned(coverage) => coverage.validate(),
            CoverageType::Signed(coverage) => coverage.validate(),
            CoverageType::Float(coverage) => coverage.validate(),
        }
    }
}

#[derive(Debug)]
pub struct Coverage<'p, T> {
    left: T,
    right: T,
    ranges: Vec<CoverageRange<'p, T>>,
}

impl<'p, T> Coverage<'p, T>
where
    T: Debug + Copy + PartialOrd + std::ops::Add<Output = T> + From<u8>,
    CoverageRangeType<'p>: From<CoverageRange<'p, T>>
{
    /// Creates a new coverage with given inclusive left and right bound.
    pub fn new(left: T, right: T) -> Self {
        Self {
            left,
            right,
            ranges: Vec::new(),
        }
    }
    /// Adds a range to the coverage.
    pub fn add_range(self: &mut Self, left: Option<T>, right: Option<T>, kind: CoverageRangeKind<'p>) {
        self.ranges.push(CoverageRange { left: left.unwrap_or(self.left), right, kind });
    }
    /// Adds an integer value to the coverage.
    pub fn add_integer(self: &mut Self, value: T, kind: CoverageRangeKind<'p>) {
        // note: limiting right bound to self.right should be ok since it is explicitly checked in validate via self.right > max_covered
        // if we didn't limit here we'd need a larger datatype to be able to add 1.
        self.ranges.push(CoverageRange { left: value, right: if value == self.right { None } else { Some(value + T::from(1)) }, kind });
    }
    /// Validates that the ranges cover the entire coverage.
    pub fn validate(self: &Self) -> CoverageRangeType {
        if self.ranges.len() == 0 {
            return CoverageRangeType::from(CoverageRange { left: self.left, right: None, kind: CoverageRangeKind::Gap });
        }
        // sort ranges
        let ranges = {
            let mut ranges: Vec<_> = self.ranges.iter().collect();
            ranges.sort_by(|&a, &b| a.left.partial_cmp(&b.left).unwrap_or(Ordering::Equal));
            ranges
        };
        // check if left is covered
        let first = *ranges.first().unwrap();
        if first.left > self.left {
            return CoverageRangeType::from(CoverageRange { left: self.left, right: Some(first.left), kind: CoverageRangeKind::Gap });
        }
        // check for gaps
        let mut max_covered = first.right;
        if ranges.len() > 1 {
            for i in 1..ranges.len() {
                if max_covered.is_some() && ranges[i].left > max_covered.unwrap() {
                    return CoverageRangeType::from(CoverageRange { left: max_covered.unwrap(), right: Some(ranges[i].left), kind: CoverageRangeKind::Gap });
                }
                if max_covered.is_some() && (ranges[i].right.is_none() || ranges[i].right.unwrap() > max_covered.unwrap()) {
                    max_covered = ranges[i].right;
                }
            }
        }
        // check if right is covered
        if max_covered.is_some() && self.right > max_covered.unwrap() /* second part shouldn't be required */ {
            return CoverageRangeType::from(CoverageRange { left: max_covered.unwrap(), right: None, kind: CoverageRangeKind::Gap });
        }
        // check partial ranges recursively
        for range in ranges {
            if let CoverageRangeKind::Partial(pattern, partial) = &range.kind {
                for coverage_type in partial {
                    let result = coverage_type.validate();
                    match result.kind() {
                        CoverageRangeKind::Gap => return result,
                        _ => {},
                    }
                }
            }
        }
        CoverageRangeType::from(CoverageRange { left: self.left, right: None, kind: CoverageRangeKind::Covered })
    }
}