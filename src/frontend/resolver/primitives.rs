use crate::util::{Type, TypeId, ScopeId, Numeric, Signed, Unsigned};
use crate::frontend::resolver::scopes::Scopes;

pub struct IntegerRange {
    pub type_id: TypeId,
    pub min: Numeric,
    pub max: Numeric,
}

// todo: remove
#[allow(dead_code)]
/// Utility structure to handle primitive type information and casting.
pub(crate) struct Primitives {
    /// Void type
    void        : TypeId,
    /// Boolean type
    pub bool    : TypeId,           // todo: temp pub
    /// String type
    pub string  : TypeId,
    /// Unsigned types ordered by bit count.
    pub unsigned: [ IntegerRange; 4 ],
    /// Signed types ordered by bit count.
    signed      : [ IntegerRange; 4 ],
    /// Floating point types ordered by bit count.
    pub float   : [ TypeId; 2 ],        // todo: temp pub
}

#[allow(dead_code)]
impl Primitives {

    /// Create new instance.
    pub fn new(scopes: &mut Scopes, root_scope_id: ScopeId) -> Self {
        use std::{u8, u16, u32, u64, i8, i16, i32, i64};
        Primitives {
            void: scopes.insert_type(root_scope_id, None, Type::void), // FIXME: don't actually want void here
            bool: scopes.insert_type(root_scope_id, Some("bool"), Type::bool),
            string: scopes.insert_type(root_scope_id, Some("String"), Type::String),
            unsigned: [
                IntegerRange {
                    type_id: scopes.insert_type(root_scope_id, Some("u8"), Type::u8),
                    min: Numeric::Unsigned(u8::MIN as Unsigned),
                    max: Numeric::Unsigned(u8::MAX as Unsigned),
                },
                IntegerRange {
                    type_id: scopes.insert_type(root_scope_id, Some("u16"), Type::u16),
                    min: Numeric::Unsigned(u16::MIN as Unsigned),
                    max: Numeric::Unsigned(u16::MAX as Unsigned),
                },
                IntegerRange {
                    type_id: scopes.insert_type(root_scope_id, Some("u32"), Type::u32),
                    min: Numeric::Unsigned(u32::MIN as Unsigned),
                    max: Numeric::Unsigned(u32::MAX as Unsigned),
                },
                IntegerRange {
                    type_id: scopes.insert_type(root_scope_id, Some("u64"), Type::u64),
                    min: Numeric::Unsigned(u64::MIN as Unsigned),
                    max: Numeric::Unsigned(u64::MAX as Unsigned),
                },
            ],
            signed: [
                IntegerRange {
                    type_id: scopes.insert_type(root_scope_id, Some("i8"), Type::i8),
                    min: Numeric::Signed(i8::MIN as Signed),
                    max: Numeric::Signed(i8::MAX as Signed),
                },
                IntegerRange {
                    type_id: scopes.insert_type(root_scope_id, Some("i16"), Type::i16),
                    min: Numeric::Signed(i16::MIN as Signed),
                    max: Numeric::Signed(i16::MAX as Signed),
                },
                IntegerRange {
                    type_id: scopes.insert_type(root_scope_id, Some("i32"), Type::i32),
                    min: Numeric::Signed(i32::MIN as Signed),
                    max: Numeric::Signed(i32::MAX as Signed),
                },
                IntegerRange {
                    type_id: scopes.insert_type(root_scope_id, Some("i64"), Type::i64),
                    min: Numeric::Signed(i64::MIN as Signed),
                    max: Numeric::Signed(i64::MAX as Signed),
                },
            ],
            float: [
                scopes.insert_type(root_scope_id, Some("f32"), Type::f32),
                scopes.insert_type(root_scope_id, Some("f64"), Type::f64),
            ],
        }
    }

    /// Check if the given type is unsigned.
    pub fn is_unsigned(self: &Self, type_id: TypeId) -> bool {
        type_id >= self.unsigned.first().unwrap().type_id && type_id <= self.unsigned.last().unwrap().type_id
    }

    /// Check if the given type is signed.
    pub fn is_signed(self: &Self, type_id: TypeId) -> bool {
        type_id >= self.signed.first().unwrap().type_id && type_id <= self.signed.last().unwrap().type_id
    }

    /// Check if the given type is integral.
    pub fn is_integer(self: &Self, type_id: TypeId) -> bool {
        self.is_unsigned(type_id) || self.is_signed(type_id)
    }

    /// Check if the given type is a float.
    pub fn is_float(self: &Self, type_id: TypeId) -> bool {
        type_id >= *self.float.first().unwrap() && type_id <= *self.float.last().unwrap()
    }

    /// Check if it is possible to represent source as target-type.
    pub fn is_compatible(self: &Self, source_type_id: TypeId, target_type_id: TypeId) -> bool {
        if source_type_id == target_type_id {
            true
        } else if self.is_unsigned(source_type_id) {
            if self.is_unsigned(target_type_id) && source_type_id <= target_type_id {
                // unsigned -> unsigned: valid if target has equal or more bits (primitive type_ids are ordered)
                true
            } else if self.is_signed(target_type_id) {
                // unsigned -> signed: valid if target has more bits (compare index within group of signed / unsigned types)
                Self::group_index(source_type_id, &self.unsigned) < Self::group_index(target_type_id, &self.signed)
            } else {
                // unsigned -> everything else: invalid
                false
            }
        } else if self.is_signed(source_type_id) && self.is_signed(target_type_id) && source_type_id <= target_type_id {
            // signed -> signed: valid if target has equal or more bits (primitive type_ids are ordered)
            true
        } else if self.is_float(source_type_id) && self.is_float(target_type_id) && source_type_id <= target_type_id {
            true
        } else {
            // everything else: invalid
            false
        }
    }

    /// Check if it is possible to store given numeric using given target type.
    pub fn is_compatible_numeric(self: &Self, source_value: Numeric, target_type_id: TypeId) -> bool {
        if source_value.is_float() && self.is_float(target_type_id) {
            true
        } else if source_value.is_integer() && self.is_integer(target_type_id) {
            let range = self.integer_range(target_type_id);
            range.min <= source_value && range.max >= source_value
        } else {
            false
        }
    }

    /// Returns integer min/max information for given type. Panics if not an integer type.
    fn integer_range(self: &Self, type_id: TypeId) -> &IntegerRange {
        if self.is_signed(type_id) {
            let index = Self::group_index(type_id, &self.signed);
            &self.signed[index]
        } else if self.is_unsigned(type_id) {
            let index = Self::group_index(type_id, &self.unsigned);
            &self.unsigned[index]
        } else {
            panic!("integer_range type argument is not an integer")
        }
    }

    /// Returns the index of a type within its type group or panics.
    fn group_index(type_id: TypeId, type_group: &[ IntegerRange; 4 ]) -> usize {
        let type_usize: usize = type_id.into();
        let group_usize: usize = type_group.first().unwrap().type_id.into();
        debug_assert!(type_usize >= group_usize);
        let index = type_usize - group_usize;
        debug_assert!(index < type_group.len());
        index
    }


















/*
    /// Tries to find a type capable of holding both given types.
    pub fn cast(self: &Self, type_id_a: TypeId, type_id_b: TypeId) -> Option<TypeId> {
        if self.is_compatible(type_id_a, type_id_b) {
            Some(type_id_b)
        } else if self.is_compatible(type_id_b, type_id_a) {
            Some(type_id_a)
        } else if let Some(type_id) = self.promote(type_id_a, type_id_b) {
            Some(type_id)
        } else {
            None
        }
    }
*/
    pub fn classify_numeric(self: &Self, value: Numeric) -> Option<TypeId> {
        if value.is_integer() {
            let allowed_types = [ &self.signed[2], &self.signed[3], &self.unsigned[3] ];
            allowed_types.iter().find(|s| {
                value >= s.min && value <= s.max
            }).map(|t| t.type_id)
        } else if value.is_float() {
            Some(self.float[1])
        } else {
            None
        }
    }
/*
    /// Tries to find a type capable of holding both given literals.
    pub fn cast_literal(self: &Self, literal_a: &ast::Literal<'_>, literal_b: &ast::Literal<'_>) -> Option<TypeId> {

        if let (Some(value_a), Some(value_b)) = (literal_a.value.as_numeric(), literal_b.value.as_numeric()) {
            if (value_a.is_signed() && value_b.is_integer()) || (value_a.is_integer() && value_b.is_signed()) {
                // at least one is signed: need common signed type
                self.signed.iter().find(|s| {
                    value_a >= s.min && value_a <= s.max && value_b >= s.min && value_b <= s.max
                }).map(|t| t.type_id)
            } else if value_a.is_integer() && value_b.is_integer() {
                // both unsigned
                self.unsigned.iter().find(|s| {
                    value_a >= s.min && value_a <= s.max && value_b >= s.min && value_b <= s.max // todo: could just check max(value_a, value_b) < s.max
                }).map(|t| t.type_id)
            } else if value_a.is_float() && value_b.is_float() {
                Some(self.float[1]) // todo: somehow compute actually required precision for given float. quick google => nothing
            } else {
                None
            }
        } else {
            None // todo: implement more
        }
    }

    /// Tries to find a type suitable to represent both given types.
    fn promote(self: &Self, type_id_a: TypeId, type_id_b: TypeId) -> Option<TypeId> {

        let do_cast = |unsigned_type_id: TypeId, signed_type_id: TypeId| -> Option<TypeId> {
            use std::cmp::max;
            let required_index = max(Self::group_index(unsigned_type_id, &self.unsigned) + 1, Self::group_index(signed_type_id, &self.signed));
            if required_index < self.signed.len() {
                Some(self.signed[required_index].type_id)
            } else {
                None
            }
        };

        if self.is_signed(type_id_a) && self.is_unsigned(type_id_b) {
            do_cast(type_id_b, type_id_a)
        } else if self.is_unsigned(type_id_a) && self.is_signed(type_id_b) {
            do_cast(type_id_a, type_id_b)
        } else {
            None
        }
    }
*/
}
