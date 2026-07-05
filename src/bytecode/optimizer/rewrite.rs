//! Helper for bytecode rewriting during optimization passes.

use crate::bytecode::opcodes::OpCodeData;
use crate::StackAddress;
use std::collections::HashSet;

/// Tracks which original instruction addresses are jump targets.
/// A pass should not remove an instruction that is a jump target
/// (another instruction jumps to it).
#[allow(dead_code)]
pub struct JumpTargetSet<'a> {
    targets: &'a HashSet<StackAddress>,
}

#[allow(dead_code)]
impl<'a> JumpTargetSet<'a> {
    pub fn new(targets: &'a HashSet<StackAddress>) -> Self {
        Self { targets }
    }

    /// Check if the given original address is a jump target.
    pub fn is_target(&self, pc: StackAddress) -> bool {
        self.targets.contains(&pc)
    }
}

/// Returns `true` if the given opcode data is a comment instruction.
/// Comment instructions are no-ops inserted by the compiler for debug output;
/// they should be skipped by optimizer passes.
#[inline]
pub fn is_comment(data: &OpCodeData) -> bool {
    #[cfg(all(feature = "symbols", feature = "comments"))]
    {
        matches!(data, OpCodeData::comment(_))
    }
    #[cfg(not(all(feature = "symbols", feature = "comments")))]
    {
        let _ = data; // unused
        false
    }
}

/// Find the index of the next non-comment instruction at or after `start`.
/// Returns `None` if no real instruction is found.
#[inline]
pub fn next_real<'a>(output: &'a [(StackAddress, StackAddress, Option<OpCodeData>)], start: usize) -> Option<usize>
where 'a: 'a {
    output.iter().skip(start).position(|(_, _, d)| {
        d.as_ref().map_or(false, |data| !is_comment(data))
    })
    .map(|offset| start + offset)
}

/// Find the index of the next non-comment instruction strictly after `idx`.
/// Returns `None` if no real instruction follows.
#[inline]
pub fn next_real_after(output: &[(StackAddress, StackAddress, Option<OpCodeData>)], idx: usize) -> Option<usize> {
    next_real(output, idx + 1)
}
