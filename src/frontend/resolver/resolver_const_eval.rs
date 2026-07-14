//! Const expression validation and evaluation.

use crate::prelude::{Set, UnorderedMap};
use crate::frontend::ast;
use crate::frontend::resolver::error::{OptionToResolveError, ResolveError, ResolveErrorKind, ResolveResult};
use crate::frontend::ast_visitor::{AstVisitor, AstVisitorMut};
use crate::shared::meta::{ConstantValue, Type, UserConstValue};
use crate::shared::numeric::Numeric;
use crate::shared::typed_ids::{BindingId, ConstantId, TypeId};
use crate::shared::MetaContainer;

use super::Resolver;

/// Visitor that checks if any target constant_ids are referenced in the AST.
struct ConstUsageChecker {
    target_ids: Set<ConstantId>,
    found: bool,
}

impl<'ast> AstVisitor<'ast> for ConstUsageChecker {
    fn visit_expression(&mut self, expr: &'ast ast::Expression) -> bool {
        if self.found {
            return false; // short-circuit
        }
        if let ast::Expression::Constant(c) = expr {
            if let Some(id) = c.constant_id {
                if self.target_ids.contains(&id) {
                    self.found = true;
                    return false;
                }
            }
        }
        true // keep walking
    }
}

/// Visitor that replaces constant_ids in Constant expressions.
struct ConstRefPatcher<'a> {
    replacements: &'a UnorderedMap<ConstantId, ConstantId>,
}

impl<'a, 'ast> AstVisitorMut<'ast> for ConstRefPatcher<'a> {
    fn visit_expression(&mut self, expr: &mut ast::Expression) -> bool {
        if let ast::Expression::Constant(c) = expr {
            if let Some(id) = c.constant_id {
                if let Some(&new_id) = self.replacements.get(&id) {
                    c.constant_id = Some(new_id);
                }
            }
        }
        true // keep walking
    }
}

/// Visitor that collects `BindingId`s mutated by assignments in a function body.
struct MutationCollector<'a> {
    candidate_ids: &'a Set<BindingId>,
    mutated: Set<BindingId>,
}

impl<'a, 'ast> AstVisitor<'ast> for MutationCollector<'a> {
    fn visit_statement(&mut self, stmt: &'ast ast::Statement) -> bool {
        // Only recurse into statement types that can contain expressions with assignments
        matches!(stmt,
            ast::Statement::Expression(_)
            | ast::Statement::Return(_)
            | ast::Statement::Block(_)
            | ast::Statement::IfBlock(_)
            | ast::Statement::ForLoop(_)
            | ast::Statement::WhileLoop(_)
            | ast::Statement::Function(_)
        )
    }

    fn visit_expression(&mut self, expr: &'ast ast::Expression) -> bool {
        if let ast::Expression::Assignment(a) = expr {
            if let Some(root_id) = extract_root_binding_id(&a.left) {
                if self.candidate_ids.contains(&root_id) {
                    self.mutated.insert(root_id);
                }
            }
        }
        // Also detect mutating method calls on parameter bindings (e.g. `v.push(...)`, `v.set(...)`)
        if let ast::Expression::BinaryOp(bo) = expr {
            if bo.right.as_argument_list().is_some()
                && let ast::BinaryOperand::Expression(ref call_func) = bo.left
                && let ast::Expression::BinaryOp(access) = call_func
                && access.op == ast::BinaryOperator::Access
                && let ast::BinaryOperand::Member(ref member) = access.right
                && Resolver::is_mutating_method(&member.ident.name)
                && let ast::BinaryOperand::Expression(ref receiver) = access.left
            {
                if let Some(root_id) = extract_root_binding_id(receiver) {
                    if self.candidate_ids.contains(&root_id) {
                        self.mutated.insert(root_id);
                    }
                }
            }
        }
        true // keep walking
    }
}

/// Extract the root `BindingId` from an assignment LHS expression.
/// Handles plain variables, field access (`x.field`), and index access (`x[i]`).
pub(crate) fn extract_root_binding_id(expr: &ast::Expression) -> Option<BindingId> {
    match expr {
        ast::Expression::Variable(v) => Some(v.binding_id),
        ast::Expression::BinaryOp(bo) => {
            if matches!(bo.op, ast::BinaryOperator::Access | ast::BinaryOperator::AccessWrite | ast::BinaryOperator::Index | ast::BinaryOperator::IndexWrite) {
                if let ast::BinaryOperand::Expression(ref left) = bo.left {
                    return extract_root_binding_id(left);
                }
            }
            None
        }
        _ => None,
    }
}

/// Const-related resolver methods.
impl<'ctx> Resolver<'ctx> {
    /// Returns whether a const expression is valid: every sub-expression is a literal, const reference, supported binary op, or cast.
    pub(crate) fn is_const_expr(self: &Self, expr: &ast::Expression) -> bool {
        match expr {
            ast::Expression::Literal(l) => l.value.is_const(),
            ast::Expression::Constant(_) => true,  // resolved const reference — always static
            ast::Expression::BinaryOp(bo) => {
                matches!(bo.op, ast::BinaryOperator::Add | ast::BinaryOperator::Sub | ast::BinaryOperator::Mul | ast::BinaryOperator::Div | ast::BinaryOperator::Rem)
                    && bo.left.as_expression().map(|e| self.is_const_expr(e)).unwrap_or(false)
                    && bo.right.as_expression().map(|e| self.is_const_expr(e)).unwrap_or(false)
            },
            ast::Expression::UnaryOp(u) => self.is_const_expr(&u.expr),
            ast::Expression::Cast(c) => self.is_const_expr(&c.expr),
            _ => false,
        }
    }

    /// Try to resolve a trait const default value.
    ///
    /// When `Self::CONST` is used in a trait impl method and the impl doesn't define the const,
    /// this falls back to the trait's provided (default) const. Returns the constant id of the
    /// trait default, or None if no fallback applies.
    pub(super) fn try_resolve_trait_const_default(self: &Self, resolved_segments: &[String]) -> Option<ConstantId> {
        // Need at least 2 segments: TypeName::CONST
        if resolved_segments.len() < 2 {
            return None;
        }
        let type_name = &resolved_segments[0];
        let const_name = &resolved_segments[1];

        // Look up the type
        let type_id = self.scopes.type_id(self.scope_id, type_name)?;
        let ty = self.type_by_id(type_id);

        // If it's a trait, look in its provided consts
        if let Some(trt) = ty.as_trait() {
            if let Some(&const_id) = trt.provided_consts.get(const_name) {
                return const_id;
            }
        }

        // If it's a concrete type, check its impl'd traits for provided consts with matching names
        if let Some(impl_traits) = ty.impl_traits_map() {
            for (&trait_type_id, _) in impl_traits {
                let trait_ty = self.type_by_id(trait_type_id);
                if let Some(trt) = trait_ty.as_trait() {
                    if let Some(&const_id) = trt.provided_consts.get(const_name) {
                        return const_id;
                    }
                }
            }
        }

        None
    }

    /// Evaluate a resolved const expression to a `UserConstValue`.
    /// ICE if the expression was not validated by `is_const_expr` first.
    pub(crate) fn evaluate_const_expr(self: &Self, expr: &ast::Expression, position: ast::Position, module_path: &str, expected_type: Option<TypeId>) -> Result<UserConstValue, ResolveError> {
        match expr {
            ast::Expression::Literal(l) => Self::const_eval_literal(l),
            ast::Expression::Constant(c) => self.const_eval_constant(c),
            ast::Expression::Cast(c) => self.const_eval_cast(c, position, module_path, expected_type),
            ast::Expression::BinaryOp(bo) => self.const_eval_binary_op(bo, position, module_path, expected_type),
            ast::Expression::UnaryOp(u) => self.const_eval_unary_op(u, position, module_path, expected_type),
            _ => Self::ice("Unexpected expression in const evaluation"),
        }
    }

    /// Evaluate a literal expression to a const value.
    fn const_eval_literal(l: &ast::Literal) -> Result<UserConstValue, ResolveError> {
        match &l.value {
            ast::LiteralValue::Bool(b) => Ok(UserConstValue::Bool(*b)),
            ast::LiteralValue::Numeric(n) => Ok(UserConstValue::Numeric(*n)),
            ast::LiteralValue::String(s) => Ok(UserConstValue::String(s.clone())),
            _ => Self::ice("Non-primitive literal in const expression"),
        }
    }

    /// Evaluate a const reference expression by looking up the referenced constant.
    fn const_eval_constant(self: &Self, c: &ast::Constant) -> Result<UserConstValue, ResolveError> {
        let constant_id = c.constant_id.ice()?;
        let constant = self.scopes.constant_ref(constant_id);
        match &constant.value {
            ConstantValue::UserConst(uv) => Ok(uv.clone()),
            ConstantValue::Discriminant(n) => Ok(UserConstValue::Numeric(*n)),
            _ => Self::ice("Unexpected constant value in const expression"),
        }
    }

    /// Evaluate a cast expression: String pass-through or numeric signedness coercion.
    fn const_eval_cast(self: &Self, c: &ast::Cast, position: ast::Position, module_path: &str, expected_type: Option<TypeId>) -> Result<UserConstValue, ResolveError> {
        let target_type_id = c.type_id.ice()?;
        let target_ty = self.type_by_id(target_type_id);
        let inner_value = self.evaluate_const_expr(&c.expr, position, module_path, expected_type)?;

        if target_ty.is_string() {
            // Cast to String from String: pass-through
            match &inner_value {
                UserConstValue::String(s) => Ok(UserConstValue::String(s.clone())),
                _ => Self::ice("Cast to String from non-string in const expression"),
            }
        } else if target_ty.is_numeric() {
            // Numeric cast: coerce signedness
            match inner_value {
                UserConstValue::Numeric(n) => {
                    let coerced = if target_ty.is_signed() {
                        Self::const_coerce_to_signed(n)?
                    } else if target_ty.is_float() {
                        Self::const_coerce_to_float(n)
                    } else {
                        Self::const_coerce_to_unsigned(n)?
                    };
                    Ok(UserConstValue::Numeric(coerced))
                },
                _ => Self::ice("Non-numeric value in numeric const cast"),
            }
        } else {
            Self::ice("Unsupported cast target in const expression")
        }
    }

    /// Coerce a numeric const value to a signed integer.
    fn const_coerce_to_signed(n: Numeric) -> ResolveResult<Numeric> {
        match n {
            Numeric::Signed(s) => Ok(Numeric::Signed(s)),
            Numeric::Unsigned(u) => Ok(Numeric::Signed(u as i64)),
            Numeric::Float(_) => Self::ice("Cannot cast float to integer in const expression"),
        }
    }

    /// Coerce a numeric const value to a float.
    fn const_coerce_to_float(n: Numeric) -> Numeric {
        match n {
            Numeric::Signed(s) => Numeric::Float(s as f64),
            Numeric::Unsigned(u) => Numeric::Float(u as f64),
            Numeric::Float(f) => Numeric::Float(f),
        }
    }

    /// Coerce a numeric const value to an unsigned integer.
    fn const_coerce_to_unsigned(n: Numeric) -> ResolveResult<Numeric> {
        match n {
            Numeric::Signed(s) => Ok(Numeric::Unsigned(s as u64)),
            Numeric::Unsigned(u) => Ok(Numeric::Unsigned(u)),
            Numeric::Float(_) => Self::ice("Cannot cast float to integer in const expression"),
        }
    }

    /// Evaluate a binary operation: string concatenation for `+` on strings, numeric otherwise.
    fn const_eval_binary_op(self: &Self, bo: &ast::BinaryOp, position: ast::Position, module_path: &str, expected_type: Option<TypeId>) -> Result<UserConstValue, ResolveError> {
        let left_value = self.evaluate_const_expr(bo.left.as_expression().ice()?, position, module_path, expected_type)?;
        let right_value = self.evaluate_const_expr(bo.right.as_expression().ice()?, position, module_path, expected_type)?;

        // `+` on strings: concatenation
        if matches!(bo.op, ast::BinaryOperator::Add)
            && matches!(&left_value, UserConstValue::String(_))
            && matches!(&right_value, UserConstValue::String(_))
        {
            let l = match left_value { UserConstValue::String(s) => s, _ => unreachable!() };
            let r = match right_value { UserConstValue::String(s) => s, _ => unreachable!() };
            return Ok(UserConstValue::String(format!("{l}{r}")));
        }

        // All supported operators on numeric values
        self.const_numeric_op(left_value, right_value, bo.op, position, module_path)
    }

    /// Evaluate a unary operation: negation, positive, or not.
    fn const_eval_unary_op(self: &Self, u: &ast::UnaryOp, position: ast::Position, module_path: &str, expected_type: Option<TypeId>) -> Result<UserConstValue, ResolveError> {
        let inner_value = self.evaluate_const_expr(&u.expr, position, module_path, expected_type)?;
        let type_id = u.type_id.or(expected_type).ice()?;
        let ty = self.type_by_id(type_id);

        match u.op {
            ast::UnaryOperator::Minus => Self::const_eval_unary_negate(inner_value, ty, position, module_path),
            ast::UnaryOperator::Plus => Ok(inner_value),
            ast::UnaryOperator::Not => Self::const_eval_unary_not(inner_value, ty),
        }
    }

    /// Numeric negation for const evaluation.
    fn const_eval_unary_negate(value: UserConstValue, ty: &Type, position: ast::Position, module_path: &str) -> Result<UserConstValue, ResolveError> {
        let usr_err = |kind: ResolveErrorKind| ResolveError::at(position, kind, module_path);

        match value {
            UserConstValue::Numeric(n) => {
                let negated = match n {
                    Numeric::Signed(s) => {
                        s.checked_neg().map(Numeric::Signed)
                            .ok_or(usr_err(ResolveErrorKind::IntegerOverflow))?
                    },
                    Numeric::Unsigned(val) => {
                        if ty.is_signed() {
                            let s = val as i64;
                            s.checked_neg().map(Numeric::Signed)
                                .ok_or(usr_err(ResolveErrorKind::IntegerOverflow))?
                        } else if val == 0 {
                            Numeric::Unsigned(0)
                        } else {
                            return Err(usr_err(ResolveErrorKind::IntegerOverflow));
                        }
                    },
                    Numeric::Float(f) => Numeric::Float(-f),
                };
                Ok(UserConstValue::Numeric(negated))
            },
            _ => Self::ice("Non-numeric operand in const negation"),
        }
    }

    /// Logical not (bool) or bitwise not (integer) for const evaluation.
    fn const_eval_unary_not(value: UserConstValue, ty: &Type) -> Result<UserConstValue, ResolveError> {
        if ty.is_integer() {
            // Bitwise not for integers
            match value {
                UserConstValue::Numeric(n) => {
                    let bit_width = ty.primitive_size() as u32 * 8;
                    let mask = if bit_width == 64 { !0i64 } else { (1i64 << bit_width) - 1 };
                    let val = match n {
                        Numeric::Signed(s) => s,
                        Numeric::Unsigned(u) => u as i64,
                        Numeric::Float(_) => Self::ice("Bitwise not on float in const expression")?,
                    };
                    let result = (!val) & mask;
                    if ty.is_signed() {
                        Ok(UserConstValue::Numeric(Numeric::Signed(result)))
                    } else {
                        Ok(UserConstValue::Numeric(Numeric::Unsigned(result as u64)))
                    }
                },
                _ => Self::ice("Non-numeric operand in const bitwise not"),
            }
        } else {
            // Logical not for bool
            match value {
                UserConstValue::Bool(b) => Ok(UserConstValue::Bool(!b)),
                _ => Self::ice("Non-bool operand in const logical not"),
            }
        }
    }

    /// Perform a numeric binary operation on const values with overflow checking.
    fn const_numeric_op(self: &Self, left: UserConstValue, right: UserConstValue, op: ast::BinaryOperator, position: ast::Position, module_path: &str) -> Result<UserConstValue, ResolveError> {
        let (left_num, right_num) = match (left, right) {
            (UserConstValue::Numeric(l), UserConstValue::Numeric(r)) => (l, r),
            _ => Self::ice("Non-numeric operands in const numeric operation")?,
        };

        let usr_err = |kind: ResolveErrorKind| ResolveError::at(position, kind, module_path);

        // Use checked arithmetic for integer overflow detection
        let result = match (left_num, right_num) {
            (Numeric::Signed(l), Numeric::Signed(r)) => {
                let res = match op {
                    ast::BinaryOperator::Add => l.checked_add(r),
                    ast::BinaryOperator::Sub => l.checked_sub(r),
                    ast::BinaryOperator::Mul => l.checked_mul(r),
                    ast::BinaryOperator::Div => if r == 0 { return Err(usr_err(ResolveErrorKind::DivisionByZero)); } else { Some(l / r) },
                    ast::BinaryOperator::Rem => if r == 0 { return Err(usr_err(ResolveErrorKind::DivisionByZero)); } else { Some(l % r) },
                    _ => Self::ice("Unsupported operator in const numeric operation")?,
                };
                match res {
                    Some(v) => Numeric::Signed(v),
                    None => return Err(usr_err(ResolveErrorKind::IntegerOverflow)),
                }
            },
            (Numeric::Unsigned(l), Numeric::Unsigned(r)) => {
                let res = match op {
                    ast::BinaryOperator::Add => l.checked_add(r),
                    ast::BinaryOperator::Sub => l.checked_sub(r),
                    ast::BinaryOperator::Mul => l.checked_mul(r),
                    ast::BinaryOperator::Div => if r == 0 { return Err(usr_err(ResolveErrorKind::DivisionByZero)); } else { Some(l / r) },
                    ast::BinaryOperator::Rem => if r == 0 { return Err(usr_err(ResolveErrorKind::DivisionByZero)); } else { Some(l % r) },
                    _ => Self::ice("Unsupported operator in const numeric operation")?,
                };
                match res {
                    Some(v) => Numeric::Unsigned(v),
                    None => return Err(usr_err(ResolveErrorKind::IntegerOverflow)),
                }
            },
            (Numeric::Float(l), Numeric::Float(r)) => {
                match op {
                    ast::BinaryOperator::Add => Numeric::Float(l + r),
                    ast::BinaryOperator::Sub => Numeric::Float(l - r),
                    ast::BinaryOperator::Mul => Numeric::Float(l * r),
                    ast::BinaryOperator::Div => Numeric::Float(l / r),
                    ast::BinaryOperator::Rem => Numeric::Float(l % r),
                    _ => Self::ice("Unsupported operator in const numeric operation")?,
                }
            },
            _ => {
                // Mixed signed/unsigned: convert to signed (the resolver should have unified types)
                let l = left_num.as_signed().unwrap_or(left_num.as_unsigned().unwrap() as i64);
                let r = right_num.as_signed().unwrap_or(right_num.as_unsigned().unwrap() as i64);
                let res = match op {
                    ast::BinaryOperator::Add => l.checked_add(r),
                    ast::BinaryOperator::Sub => l.checked_sub(r),
                    ast::BinaryOperator::Mul => l.checked_mul(r),
                    ast::BinaryOperator::Div => if r == 0 { return Err(usr_err(ResolveErrorKind::DivisionByZero)); } else { Some(l / r) },
                    ast::BinaryOperator::Rem => if r == 0 { return Err(usr_err(ResolveErrorKind::DivisionByZero)); } else { Some(l % r) },
                    _ => Self::ice("Unsupported operator in const numeric operation")?,
                };
                match res {
                    Some(v) => Numeric::Signed(v),
                    None => return Err(usr_err(ResolveErrorKind::IntegerOverflow)),
                }
            },
        };
        Ok(UserConstValue::Numeric(result))
    }

    /// Check if a block references any of the constant_ids in the replacement map.
    pub(crate) fn block_uses_consts(block: &ast::Block, target_ids: &UnorderedMap<ConstantId, ConstantId>) -> bool {
        let mut checker = ConstUsageChecker {
            target_ids: target_ids.keys().copied().collect(),
            found: false,
        };
        checker.walk_block(block);
        checker.found
    }

    /// Walk a block and replace constant_ids according to the replacement map.
    pub(crate) fn patch_const_refs(block: &mut ast::Block, replacements: &UnorderedMap<ConstantId, ConstantId>) {
        let mut patcher = ConstRefPatcher { replacements };
        patcher.walk_block(block);
    }

    /// Walk the receiver chain of an expression looking for user const references
    /// or const-derived variable bindings. Returns the first const/const-derived name found, or None.
    ///
    /// Only walks the receiver/base of each access operation. The index/key side of `[]` is NOT
    /// part of the receiver chain. For example, `arr[MY_INDEX] = value` should be allowed
    /// (`MY_INDEX` is used as an offset, not mutated), but `MY_CONST[i] = value` should be rejected.
    pub(crate) fn find_const_in_receiver_chain(self: &Self, expr: &ast::Expression) -> Option<String> {
        use ast::{Expression, BinaryOperand};
        match expr {
            Expression::Constant(c) => {
                if let Some(constant_id) = c.constant_id {
                    let constant = self.scopes.constant_ref(constant_id);
                    if matches!(constant.value, ConstantValue::UserConst(_)) {
                        // Extract the const name from the path
                        if let Some(segment) = c.path.segments.last() {
                            return Some(segment.name.clone());
                        }
                    }
                }
                None
            }
            Expression::Variable(v) => {
                let binding = self.scopes.binding_ref(v.binding_id);
                if binding.from_const {
                    return Some(v.ident.name.clone());
                }
                None
            }
            Expression::BinaryOp(bo) => {
                match bo.op {
                    // For access/index operations, only recurse into the left (receiver) side
                    ast::BinaryOperator::Index
                    | ast::BinaryOperator::IndexWrite
                    | ast::BinaryOperator::Access
                    | ast::BinaryOperator::AccessWrite => {
                        if let BinaryOperand::Expression(ref left) = bo.left {
                            self.find_const_in_receiver_chain(left)
                        } else {
                            None
                        }
                    }
                    _ => None, // other operators shouldn't appear in receiver chains
                }
            }
            Expression::UnaryOp(u) => self.find_const_in_receiver_chain(&u.expr),
            Expression::Cast(c) => self.find_const_in_receiver_chain(&c.expr),
            _ => None,
        }
    }

    /// Check if an expression's receiver chain references a user const.
    /// Used for setting the `from_const` flag on let bindings.
    pub(crate) fn expr_references_user_const(self: &Self, expr: &ast::Expression) -> bool {
        use ast::{Expression, BinaryOperand};
        match expr {
            Expression::Constant(c) => {
                if let Some(constant_id) = c.constant_id {
                    let constant = self.scopes.constant_ref(constant_id);
                    matches!(constant.value, ConstantValue::UserConst(_))
                } else {
                    false
                }
            }
            Expression::Variable(v) => self.scopes.binding_ref(v.binding_id).from_const,
            Expression::BinaryOp(bo) => {
                match bo.op {
                    ast::BinaryOperator::Index
                    | ast::BinaryOperator::Access => {
                        if let BinaryOperand::Expression(ref left) = bo.left {
                            self.expr_references_user_const(left)
                        } else {
                            false
                        }
                    }
                    _ => false,
                }
            }
            Expression::UnaryOp(u) => self.expr_references_user_const(&u.expr),
            Expression::Cast(c) => self.expr_references_user_const(&c.expr),
            _ => false,
        }
    }

    /// Set of method names that mutate their receiver.
    pub(crate) fn is_mutating_method(name: &str) -> bool {
        matches!(name,
            // Array mutating methods
            "push" | "pop" | "truncate" | "insert" | "remove" | "reverse" | "clear" | "swap" | "resize" | "swap_remove"
            // Index trait mutating method
            | "set"
        )
    }

    /// Walk a function body and collect the parameter indices whose bindings are mutation targets.
    pub(crate) fn collect_mutated_param_indices(self: &Self, block: &ast::Block, param_binding_ids: &[BindingId]) -> Set<usize> {
        let param_set: Set<BindingId> = param_binding_ids.iter().copied().collect();
        let mut collector = MutationCollector { candidate_ids: &param_set, mutated: Set::new() };
        collector.walk_block(block);
        param_binding_ids.iter().enumerate()
            .filter_map(|(idx, bid)| if collector.mutated.contains(bid) { Some(idx) } else { None })
            .collect()
    }
}

