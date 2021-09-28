use std::collections::HashMap;
use crate::shared::typed_ids::BindingId;

#[derive(Copy, Clone, PartialEq)]
pub enum BranchingKind {
    Single,
    Double,
}

#[derive(Copy, Clone, PartialEq)]
pub enum BranchingPath {
    A,
    B,
}

struct BindingInitState {
    path_a: bool,
    path_b: bool,
}

struct Branching {
    bindings: HashMap<BindingId, BindingInitState>,
    kind: BranchingKind,
    current_path: BranchingPath,
}

impl Branching {
    fn new(kind: BranchingKind) -> Self {
        Self {
            bindings: HashMap::new(),
            kind,
            current_path: BranchingPath::A,
        }
    }
}

pub struct InitState {
    branchings: Vec<Branching>,
}

impl InitState {
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
                self.init(binding_id);
            }
        }
    }

    pub fn init(self: &mut Self, binding_id: BindingId) {
        match self.branchings.last().unwrap().current_path {
            BranchingPath::A => self.current_mut(binding_id).path_a = true,
            BranchingPath::B => self.current_mut(binding_id).path_b = true,
        }
    }

    pub fn is_init(self: &Self, binding_id: BindingId) -> bool {
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

    fn current_mut(self: &mut Self, binding_id: BindingId) -> &mut BindingInitState {
        self.branchings.last_mut().unwrap().bindings.entry(binding_id).or_insert(BindingInitState { path_a: false, path_b: false })
    }
}