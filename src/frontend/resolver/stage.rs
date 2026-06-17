
#[derive(Copy, Clone, Debug)]
pub struct Stage(u8);

impl Stage {
    /// Construct stage.
    pub fn new() -> Self {
        Stage(0)
    }
    /// Proceed to next stage.
    pub fn next(self: &mut Self) {
        self.0 += 1;
    }
    /// Resets back to first stage.
    pub fn reset(self: &mut Self) {
        self.0 = 0;
    }
    /// First pass completed, all identifiers must be known at this point.
    /// note: variable should always identify immediately since the let has to be above the first use
    pub fn must_exist(self: &Self) -> bool {
        self.0 == 1
    }
    /// First structural pass completed: a binding has now had a chance to resolve from its own
    /// declaration, so it may be pinned to a (concrete) type expected by one of its uses. Kept before
    /// `infer_literals` so a genuinely usage-typed binding (e.g. `let mut y;`) still constrains numeric
    /// literals before they fall back to their default types.
    pub fn infer_from_usage(self: &Self) -> bool {
        self.0 >= 1
    }
    /// Assume unresolved numeric literals to be their default types.
    pub fn infer_literals(self: &Self) -> bool {
        self.0 == 2
    }
    /// Allow unresolved types to be inferred from inconcrete types like traits.
    pub fn infer_as_concrete(self: &Self) -> bool {
        self.0 == 3
    }
    /// Previous stages failed, next resolution failure must trigger a resolution error.
    pub fn must_resolve(self: &Self) -> bool {
        self.0 >= 4
    }
}