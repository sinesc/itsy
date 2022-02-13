use crate::shared::typed_ids::TypeId;

/// Binding meta information.
pub struct BindingInfo {
    pub mutable: bool,
    pub type_id: Option<TypeId>,
}
