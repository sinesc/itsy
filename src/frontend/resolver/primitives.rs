use util::{TypeId, Integer};
use frontend::ast;

// todo: see fn resolve(), primitives should be statically defined here with functions to look up their ids.

pub struct IntegerRange {
    pub type_id: TypeId,
    pub min: Integer,
    pub max: Integer,
}

pub struct Primitives {
    /// Boolean type
    pub bool    : TypeId,
    /// String type
    pub string  : TypeId,
    /// Unsigned types ordered by bit count.
    pub unsigned: [ IntegerRange; 4 ],
    /// Signed types ordered by bit count.
    pub signed  : [ IntegerRange; 4 ],
    /// Floating point types ordered by bit count.
    pub float   : [ TypeId; 2 ],
}

impl Primitives {

    pub fn is_unsigned(self: &Self, type_id: TypeId) -> bool {
        type_id >= self.unsigned.first().unwrap().type_id && type_id <= self.unsigned.last().unwrap().type_id
    }

    pub fn is_signed(self: &Self, type_id: TypeId) -> bool {
        type_id >= self.signed.first().unwrap().type_id && type_id <= self.signed.last().unwrap().type_id
    }

    pub fn is_float(self: &Self, type_id: TypeId) -> bool {
        type_id >= *self.float.first().unwrap() && type_id <= *self.float.last().unwrap()
    }

    pub fn is_valid_cast(self: &Self, from_type_id: TypeId, to_type_id: TypeId) -> bool {
        if self.is_unsigned(from_type_id) {
            if self.is_unsigned(to_type_id) && from_type_id <= to_type_id {
                // unsigned -> unsigned: valid if target has equal or more bits (primitive type_ids are ordered)
                true
            } else if self.is_signed(to_type_id) {
                // unsigned -> signed: valid if target has more bits (compare index within group of signed / unsigned types)
                Self::group_index(from_type_id, &self.unsigned) < Self::group_index(to_type_id, &self.signed)
            } else {
                // unsigned -> everything else: invalid
                false
            }
        } else if self.is_signed(from_type_id) && self.is_signed(to_type_id) && from_type_id <= to_type_id {
            // signed -> signed: valid if target has equal or more bits (primitive type_ids are ordered)
            true
        } else if self.is_float(from_type_id) && self.is_float(to_type_id) && from_type_id <= to_type_id {
            true
        } else {
            // everything else: invalid
            false
        }
    }

    /// Tries to find a type capable of holding both given types.
    pub fn cast(self: &Self, type_id_a: TypeId, type_id_b: TypeId) -> Option<TypeId> {
        if self.is_valid_cast(type_id_a, type_id_b) {
            Some(type_id_b)
        } else if self.is_valid_cast(type_id_b, type_id_a) {
            Some(type_id_a)
        } else if let Some(type_id) = self.promote(type_id_a, type_id_b) {
            Some(type_id)
        } else {
            None
        }
    }

    /// Tries to find a type capable of holding both given literals.
    pub fn cast_literal(self: &Self, literal_a: &ast::Literal, literal_b: &ast::Literal) -> Option<TypeId> {

        if let (Some(integer_a), Some(integer_b)) = (literal_a.value.as_integer(), literal_b.value.as_integer()) {
            self.common_integer_type_id(integer_a, integer_b)
        } else {
            None // todo: implement more
        }
    }

    /// Tries to find a type capable of holding both given integer.
    pub fn common_integer_type_id(self: &Self, value_a: Integer, value_b: Integer) -> Option<TypeId> {

        if value_a.is_signed() || value_b.is_signed() {
            // at least one is signed: need common signed type
            self.signed.iter().find(|s| {
                value_a >= s.min && value_a <= s.max && value_b >= s.min && value_b <= s.max
            }).map(|t| t.type_id)
        } else {
            // both unsigned
            self.unsigned.iter().find(|s| {
                value_a >= s.min && value_a <= s.max && value_b >= s.min && value_b <= s.max // todo: could just check max(value_a, value_b) < s.max
            }).map(|t| t.type_id)
        }
    }

    /// Tries to find a type capable of holding the given integer.
    pub fn integer_type_id(self: &Self, item: Integer) -> Option<TypeId> {
        if item.is_signed() {
            self.signed.iter().find(|s| item >= s.min && item <= s.max).map(|t| t.type_id)
        } else {
            self.unsigned.iter().find(|s| item >= s.min && item <= s.max).map(|t| t.type_id)
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

    /// Returns the index of a type within its type group or panics.
    fn group_index(type_id: TypeId, type_group: &[ IntegerRange; 4 ]) -> usize {
        let type_usize: usize = type_id.into();
        let group_usize: usize = type_group.first().unwrap().type_id.into();
        debug_assert!(type_usize >= group_usize);
        let index = type_usize - group_usize;
        debug_assert!(index < type_group.len());
        index
    }
}
