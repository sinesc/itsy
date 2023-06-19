
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
    /// Assume unresolved numeric literals to be their default types.
    pub fn infer_literals(self: &Self) -> bool {
        self.0 == 1
    }
    /// Allow unresolved types to be inferred from inconcrete types.
    pub fn infer_as_concrete(self: &Self) -> bool {
        self.0 == 2
    }
    /// Previous stages failed, next resolution failure must trigger a resolution error.
    pub fn must_resolve(self: &Self) -> bool {
        self.0 >= 3
    }
}