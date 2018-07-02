use util::{ScopeId, TypeId};
use super::scopes;

/// Internal state of the ResolvedProgram during type/binding resolution.
/// Not a member of ResolvedProgram since this data is no longer useful after resolution.
pub struct ResolverState<'a, 'b> where 'a: 'b {
    pub counter : &'b mut u32,
    pub scope_id: ScopeId,
    pub scopes  : &'b mut scopes::Scopes<'a>,
    pub unsigned: &'b [ TypeId; 4 ],
    pub signed  : &'b [ TypeId; 4 ],
    pub float   : &'b [ TypeId; 2 ],
}