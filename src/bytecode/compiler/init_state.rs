use crate::prelude::*;
use crate::shared::typed_ids::BindingId;

/// Kind of the branching, either one block or two mutually exclusive blocks.
#[derive(Copy, Clone, PartialEq)]
pub enum BranchingKind {
    Single,
    Double,
}

/// Path within a branching, either A (if or block) or B (else)
#[derive(Copy, Clone, PartialEq)]
pub enum BranchingPath {
    A,
    B,
}

/// State of a binding within a Branching.
struct BranchingBinding {
    /// Whether the binding was activated exactly in this Branching.
    activated: bool,
    /// Whether path a (if or block) is initialized.
    path_a: bool,
    /// Whether path b (else) is initialized.
    path_b: bool,
}

/// One block or two mutually exclusive blocks.
struct Branching {
    /// Bindings activated or initialized in this branching.
    bindings: UnorderedMap<BindingId, BranchingBinding>,
    /// Type of this branching.
    kind: BranchingKind,
    /// The current path within a double branching.
    current_path: BranchingPath,
}

impl Branching {
    fn new(kind: BranchingKind) -> Self {
        Self {
            bindings: UnorderedMap::new(),
            kind,
            current_path: BranchingPath::A,
        }
    }
}

/// Binding activation and initialization tracking.
pub struct InitState {
    branchings: Vec<Branching>,
}

impl InitState {
    /// Returns a new empty instance.
    pub fn new() -> Self {
        Self { branchings: Vec::new() }
    }

    /// Pushes a branching. A branching represents either a single unconditional scope or (up to) two conditional scopes (i.e. if/else).
    pub fn push(self: &mut Self, kind: BranchingKind) {
        self.branchings.push(Branching::new(kind));
    }

    /// Sets the current branching path for subsequent inits.
    pub fn set_path(self: &mut Self, path: BranchingPath) {
        self.branchings.last_mut().unwrap().current_path = path;
    }

    /// Pops branching and propagates unconditionally initialized bindings to parent.
    pub fn pop(self: &mut Self) {
        let branch = self.branchings.pop().unwrap();
        for (binding_id, binding_state) in branch.bindings {
            let init_parent = binding_state.path_a && (branch.kind == BranchingKind::Single || (branch.kind == BranchingKind::Double && binding_state.path_b));
            if init_parent && self.branchings.len() > 0 {
                self.initialize(binding_id);
            }
        }
    }

    /// Activates (declares the binding) a binding in this (Single) branching.
    pub fn activate(self: &mut Self, binding_id: BindingId) {
        self.current_mut(binding_id).activated = true;
    }

    /// Returns current branching depth.
    pub fn len(self: &Self) -> usize {
        self.branchings.len()
    }

    /// Whether the binding was activated in exactly this branching (will be a Single block).
    pub fn activated(self: &Self, binding_id: BindingId) -> bool {
        let branching = self.branchings.last().unwrap();
        match branching.bindings.get(&binding_id) {
            Some(binding) => binding.activated,
            _ => false
        }
    }

    /// Initializes a binding in the current branching path.
    pub fn initialize(self: &mut Self, binding_id: BindingId) {
        match self.branchings.last().unwrap().current_path {
            BranchingPath::A => self.current_mut(binding_id).path_a = true,
            BranchingPath::B => self.current_mut(binding_id).path_b = true,
        }
    }

    /// Whether a binding is initialized in the current branching path at the current code position (ephemeral data)
    pub fn initialized(self: &Self, binding_id: BindingId) -> bool {
        for branching in self.branchings.iter().rev() {
            if let Some(binding) = branching.bindings.get(&binding_id) {
                if match branching.current_path {
                    BranchingPath::A => binding.path_a,
                    BranchingPath::B => binding.path_b,
                } { return true; } // woah, ugly
            }
        }
        return false;
    }

    /// Returns a mutable reference to the state of a binding.
    fn current_mut(self: &mut Self, binding_id: BindingId) -> &mut BranchingBinding {
        self.branchings.last_mut().unwrap().bindings.entry(binding_id).or_insert(BranchingBinding { activated: false, path_a: false, path_b: false })
    }
}