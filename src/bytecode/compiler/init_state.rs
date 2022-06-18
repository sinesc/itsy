use crate::prelude::*;
use crate::shared::typed_ids::BindingId;

/// Kind of the branching, either one block or two mutually exclusive blocks.
#[derive(Copy, Clone, PartialEq)]
pub enum BranchingKind {
    Single,
    Double,
}

/// Scope type of this branching.
///
/// break/continue/return need to recursively consider branches until they hit the
/// appropriate exit scope, e.g. break needs to drop all initialized references
/// of all branches up to the closest Loop branch.
#[derive(Copy, Clone, PartialEq)]
pub enum BranchingScope {
    Function,
    Block,
    Loop,
}

/// Path within a branching, either A (if or block) or B (else)
#[derive(Copy, Clone, PartialEq)]
pub enum BranchingPath {
    A,
    B,
}

/// Initialization state of a binding within a specific branch.
#[derive(Clone, Copy, PartialEq)]
pub enum BranchingState {
    Uninitialized = 0,
    MaybeInitialized = 1,
    Initialized = 2,
}

/// State of a binding within a Branching.
struct BranchingBinding {
    /// Whether the binding was activated exactly in this Branching.
    declared: bool,
    /// Whether path a (if or block) is initialized.
    path_a: BranchingState,
    /// Whether path b (else) is initialized.
    path_b: BranchingState,
}

/// One block or two mutually exclusive blocks.
struct Branching {
    /// Bindings activated or initialized in this branching.
    bindings: UnorderedMap<BindingId, BranchingBinding>,
    /// Single/Double branching
    kind: BranchingKind,
    /// The current path within a double branching.
    current_path: BranchingPath,
    /// The type of the branching scope.
    scope: BranchingScope,
}

impl Branching {
    fn new(kind: BranchingKind, scope: BranchingScope) -> Self {
        Self {
            bindings: UnorderedMap::new(),
            kind,
            current_path: BranchingPath::A,
            scope,
        }
    }
}

/// Binding declaration and initialization tracking.
/// Reminder: This data is ephemeral and represents only the current state during compilation
/// as pop() merges the state of ended scopes into the now current scope.
pub struct InitState {
    branchings: Vec<Branching>,
}

impl InitState {
    /// Returns a new empty instance.
    pub fn new() -> Self {
        Self { branchings: Vec::new() }
    }

    /// Pushes a branching. A branching represents either a single unconditional scope or (up to) two conditional scopes (i.e. if/else).
    pub fn push(self: &mut Self, kind: BranchingKind, scope: BranchingScope) {
        self.branchings.push(Branching::new(kind, scope));
    }

    /// Sets the current branching path for subsequent inits.
    pub fn set_path(self: &mut Self, path: BranchingPath) {
        self.branchings.last_mut().unwrap().current_path = path;
    }

    /// Pops branching and propagates unconditionally initialized bindings to parent. // TODO: add state for 'maybe initialized', those refs may have to be decref'd
    pub fn pop(self: &mut Self) {
        use BranchingState as BS;
        let branch = self.branchings.pop().unwrap();
        if self.branchings.len() > 0 {
            for (binding_id, binding) in branch.bindings {
                let branching_state = match branch.kind {
                    BranchingKind::Single => binding.path_a,
                    BranchingKind::Double if binding.path_a == BS::Initialized && binding.path_b == BS::Initialized => BS::Initialized,
                    BranchingKind::Double if binding.path_a == BS::Uninitialized && binding.path_b == BS::Uninitialized => BS::Uninitialized,
                    BranchingKind::Double => BS::MaybeInitialized,
                };
                match self.branchings.last().unwrap().current_path {
                    BranchingPath::A => {
                        let current_state = &mut self.current_mut(binding_id).path_a;
                        if branching_state as usize > *current_state as usize {
                            *current_state = branching_state;
                        }
                    },
                    BranchingPath::B => {
                        let current_state = &mut self.current_mut(binding_id).path_b;
                        if branching_state as usize > *current_state as usize {
                            *current_state = branching_state;
                        }
                    },
                }
            }
        }
    }

    /// Returns current branching depth.
    pub fn len(self: &Self) -> usize {
        self.branchings.len()
    }

    /// Declares a binding in this (Single) branching.
    pub fn declare(self: &mut Self, binding_id: BindingId) {
        self.current_mut(binding_id).declared = true;
    }

    /// Whether the binding was declared in exactly this scope or recursively up to the given type of scope (required when returning/breaking out of multiple scopes)
    pub fn declared(self: &Self, binding_id: BindingId, scope: Option<BranchingScope>) -> bool {
        for branching in self.branchings.iter().rev() {
            match branching.bindings.get(&binding_id) {
                Some(binding) if binding.declared => return true,
                _ => { },
            }
            // don't recursively scan outer scopes unless a search scope type is specified and not yet found.
            if scope.is_none() || branching.scope == scope.unwrap() {
                break;
            }
        }
        false
    }

    /// Initializes a binding in the current branching path.
    pub fn initialize(self: &mut Self, binding_id: BindingId) {
        match self.branchings.last().unwrap().current_path {
            BranchingPath::A => self.current_mut(binding_id).path_a = BranchingState::Initialized,
            BranchingPath::B => self.current_mut(binding_id).path_b = BranchingState::Initialized,
        }
    }

    /// Whether a binding is initialized in the current branching path at the current code position.
    pub fn initialized(self: &Self, binding_id: BindingId) -> BranchingState {
        for branching in self.branchings.iter().rev() {
            if let Some(binding) = branching.bindings.get(&binding_id) {
                // this used to continue looping if both branches were false, now immediately returns. this should be fine since there is no way to uninitialize a binding after it was already initialized
                return match branching.current_path {
                    BranchingPath::A => binding.path_a,
                    BranchingPath::B => binding.path_b,
                };
            }
        }
        return BranchingState::Uninitialized;
    }

    /// Returns a mutable reference to the state of a binding.
    fn current_mut(self: &mut Self, binding_id: BindingId) -> &mut BranchingBinding {
        self.branchings.last_mut().unwrap()
            .bindings.entry(binding_id)
            .or_insert(BranchingBinding { declared: false, path_a: BranchingState::Uninitialized, path_b: BranchingState::Uninitialized })
    }
}