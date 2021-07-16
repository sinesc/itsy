// future home of the bytecode optimizer


/*

notes:
    push+load = store
    swap+add = add
    swap+mul = mul

    /// Computes the result of a literal x literal binary expression and alters the item variant to literal.
    fn precompute_binary_op(self: &mut Self, item: &mut ast::Expression<'ast>, expected_result: Option<TypeId>) -> ResolveResult {
// fixme: this should really just be another bytecode optimization
        use crate::frontend::ast::BinaryOperator as O;

        // set nonsense type for left/right binding ids (we can't delete ids and they need to be resolved to something in the resolved program)
        let binary_op = item.as_binary_op_mut().expect("precompute_expression_binary_op received non-literal expression");
        self.set_bindingtype_id(&mut binary_op.left, TypeId::void())?;
        self.set_bindingtype_id(&mut binary_op.right, TypeId::void())?;

        match binary_op.op {
            O::And | O::Or => {
                self.resolve_expression(&mut binary_op.left, Some(self.primitives.bool))?;
                self.resolve_expression(&mut binary_op.right, Some(self.primitives.bool))?;
                let lval = binary_op.left.as_literal().unwrap().value.as_bool().unwrap();
                let rval = binary_op.right.as_literal().unwrap().value.as_bool().unwrap();
                let result = if binary_op.op == O::And {
                    lval && rval
                } else {
                    lval || rval
                };
                *item = ast::Expression::Literal(ast::Literal {
                    position    : binary_op.left.position(),
                    value       : ast::LiteralValue::Bool(result),
                    type_name   : None,
                    binding_id  : None,
                });
                self.set_bindingtype_id(item, self.primitives.bool)?;
            }
            O::Less | O::Greater | O::LessOrEq | O::GreaterOrEq | O::Equal | O::NotEqual => {
                self.resolve_expression(&mut binary_op.left, None)?;
                self.resolve_expression(&mut binary_op.right, None)?;
                let result;
                if binary_op.left.as_literal().unwrap().value.as_bool().is_some() {
                    let lval = binary_op.left.as_literal().unwrap().value.as_bool().unwrap();
                    let rval = binary_op.right.as_literal().unwrap().value.as_bool().unwrap();
                    result = match binary_op.op {
                        O::Less         => lval < rval,
                        O::Greater      => lval > rval,
                        O::LessOrEq     => lval <= rval,
                        O::GreaterOrEq  => lval >= rval,
                        O::Equal        => lval == rval,
                        O::NotEqual     => lval != rval,
                        _ => unreachable!(),
                    };
                } else if binary_op.left.as_literal().unwrap().value.as_numeric().is_some() {
                    let lval = binary_op.left.as_literal().unwrap().value.as_numeric().unwrap();
                    let rval = binary_op.right.as_literal().unwrap().value.as_numeric().unwrap();
                    result = match binary_op.op {
                        O::Less         => lval < rval,
                        O::Greater      => lval > rval,
                        O::LessOrEq     => lval <= rval,
                        O::GreaterOrEq  => lval >= rval,
                        O::Equal        => lval == rval,
                        O::NotEqual     => lval != rval,
                        _ => unreachable!(),
                    };
                } else {
                    let lval = binary_op.left.as_literal().unwrap().value.as_string().unwrap();
                    let rval = binary_op.right.as_literal().unwrap().value.as_string().unwrap();
                    result = match binary_op.op {
                        O::Less         => lval < rval,
                        O::Greater      => lval > rval,
                        O::LessOrEq     => lval <= rval,
                        O::GreaterOrEq  => lval >= rval,
                        O::Equal        => lval == rval,
                        O::NotEqual     => lval != rval,
                        _ => unreachable!(),
                    };
                }
                *item = ast::Expression::Literal(ast::Literal {
                    position    : binary_op.left.position(),
                    value       : ast::LiteralValue::Bool(result),
                    type_name   : None,
                    binding_id  : None,
                });
                self.set_bindingtype_id(item, self.primitives.bool)?;
            },
            O::Add | O::Sub | O::Mul | O::Div | O::Rem => {
                self.resolve_expression(&mut binary_op.left, expected_result)?;
                self.resolve_expression(&mut binary_op.right, expected_result)?;
                if binary_op.left.as_literal().unwrap().value.as_numeric().is_some() {
                    let lval = binary_op.left.as_literal().unwrap().value.as_numeric().unwrap();
                    let rval = binary_op.right.as_literal().unwrap().value.as_numeric().unwrap();
                    let result = match binary_op.op {
                        O::Add  => lval + rval,
                        O::Sub  => lval - rval,
                        O::Mul  => lval * rval,
                        O::Div  => lval / rval,
                        O::Rem  => lval % rval,
                        _ => unreachable!(),
                    };
                    *item = ast::Expression::Literal(ast::Literal {
                        position    : binary_op.left.position(),
                        value       : ast::LiteralValue::Numeric(result),
                        type_name   : None,
                        binding_id  : None,
                    });
                    self.try_create_anon_binding(item);
                } else {
                    panic!("todo");
                }
            },
            O::Range | O::Index | O::IndexWrite | O::RangeInclusive | O::Access | O::AccessWrite => {
                unimplemented!("range/index");
            },
            O::Assign | O::AddAssign | O::SubAssign | O::MulAssign | O::DivAssign | O::RemAssign => {
                unimplemented!("assignments");
            },
        }
        Ok(())
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