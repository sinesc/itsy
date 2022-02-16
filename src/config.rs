
/// Type representing a stack address.
pub(crate) type StackAddress = usize;

/// Type representing a stack offset, must be same size as addresses.
pub(crate) type StackOffset = isize;

/// Type representing a heap address.
pub(crate) type HeapAddress = usize;

/// Number of bits of the heap address to allocate for internal offsets. The remaining bits are used to represent the index into the heap vector.
pub(crate) const HEAP_OFFSET_BITS: usize = 36;

/// Type used to index static elements in code, e.g. struct members.
pub(crate) type ItemIndex = u16;