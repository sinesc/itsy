//! Generic AST walker trait.
//!
//! Provides default recursive implementations for walking the full AST tree.
//! Callers override specific `visit_*` methods to handle the node types they care about.
//! Returning `false` from a `visit_*` method stops recursion into that node's children.

use crate::frontend::ast;

/// Trait for visiting AST nodes (immutable traversal).
///
/// Default implementations recursively visit all child nodes. Override specific
/// `visit_*` methods to handle leaf nodes or short-circuit traversal.
///
/// # Short-circuiting
///
/// Return `false` from any `visit_*` method to stop recursing into children of
/// that node. The default return value is `true` (continue walking).
pub trait AstVisitor<'ast> {
    /// Called when a statement node is visited. Return `false` to stop recursing.
    fn visit_statement(&mut self, _stmt: &'ast ast::Statement) -> bool { true }

    /// Called when an expression node is visited. Return `false` to stop recursing.
    fn visit_expression(&mut self, _expr: &'ast ast::Expression) -> bool { true }

    /// Called when a binary operand node is visited. Return `false` to stop recursing.
    fn visit_operand(&mut self, _op: &'ast ast::BinaryOperand) -> bool { true }

    // -- Default recursive implementations --

    /// Walk a statement and all its descendants.
    fn walk_statement(&mut self, stmt: &'ast ast::Statement) {
        if !self.visit_statement(stmt) {
            return;
        }
        match stmt {
            ast::Statement::Expression(e) => self.walk_expression(e),
            ast::Statement::Block(b) => self.walk_block(b),
            ast::Statement::IfBlock(ib) => {
                self.walk_expression(&ib.cond);
                self.walk_block(&ib.if_block);
                if let Some(eb) = &ib.else_block {
                    self.walk_block(eb);
                }
            },
            ast::Statement::ForLoop(fl) => {
                self.walk_expression(&fl.expr);
                self.walk_block(&fl.block);
            },
            ast::Statement::WhileLoop(wl) => {
                self.walk_expression(&wl.expr);
                self.walk_block(&wl.block);
            },
            ast::Statement::Return(r) => self.walk_expression(&r.expr),
            ast::Statement::Yield(y) => {
                if let Some(k) = &y.key {
                    self.walk_expression(k);
                }
                self.walk_expression(&y.value);
            },
            ast::Statement::Function(f) => {
                if let Some(b) = &f.shared.block {
                    self.walk_block(b);
                }
            },
            ast::Statement::ImplBlock(ib) => {
                for c in &ib.consts {
                    if let Some(e) = &c.expr {
                        self.walk_expression(e);
                    }
                }
            },
            ast::Statement::TraitDef(td) => {
                for c in &td.consts {
                    if let Some(e) = &c.expr {
                        self.walk_expression(e);
                    }
                }
            },
            ast::Statement::ConstDef(c) => {
                if let Some(e) = &c.expr {
                    self.walk_expression(e);
                }
            },
            // Leaf nodes — no child expressions to walk:
            // LetBinding, LetPattern, Break, Continue, Suspend, Module, UseDecl, EnumDef, StructDef
            _ => {},
        }
    }

    /// Walk an expression and all its descendants.
    fn walk_expression(&mut self, expr: &'ast ast::Expression) {
        if !self.visit_expression(expr) {
            return;
        }
        match expr {
            ast::Expression::BinaryOp(b) => {
                self.walk_operand(&b.left);
                self.walk_operand(&b.right);
            },
            ast::Expression::UnaryOp(u) => self.walk_expression(&u.expr),
            ast::Expression::Cast(c) => self.walk_expression(&c.expr),
            ast::Expression::Block(b) => self.walk_block(b),
            ast::Expression::IfBlock(ib) => {
                self.walk_expression(&ib.cond);
                self.walk_block(&ib.if_block);
                if let Some(eb) = &ib.else_block {
                    self.walk_block(eb);
                }
            },
            ast::Expression::MatchBlock(mb) => {
                for (_, b) in &mb.branches {
                    self.walk_block(b);
                }
            },
            ast::Expression::Closure(cl) => {
                if let Some(b) = &cl.shared.block {
                    self.walk_block(b);
                }
            },
            ast::Expression::AnonymousFunction(f) => {
                if let Some(b) = &f.shared.block {
                    self.walk_block(b);
                }
            },
            ast::Expression::Assignment(a) => {
                self.walk_expression(&a.left);
                self.walk_expression(&a.right);
            },
            // Leaf nodes — no child expressions to walk:
            // Literal, Constant, Variable
            _ => {},
        }
    }

    /// Walk a binary operand and all its descendants.
    fn walk_operand(&mut self, op: &'ast ast::BinaryOperand) {
        if !self.visit_operand(op) {
            return;
        }
        match op {
            ast::BinaryOperand::Expression(e) => self.walk_expression(e),
            ast::BinaryOperand::ArgumentList(al) => {
                for arg in &al.args {
                    self.walk_expression(arg);
                }
            },
            ast::BinaryOperand::Member(_) => {},
        }
    }

    /// Walk a block (all statements and the optional result expression).
    fn walk_block(&mut self, block: &'ast ast::Block) {
        for stmt in &block.statements {
            self.walk_statement(stmt);
        }
        if let Some(r) = &block.result {
            self.walk_expression(r);
        }
    }
}

/// Trait for visiting AST nodes (mutable traversal).
///
/// Same as [`AstVisitor`] but takes `&mut` references to AST nodes, allowing
/// in-place modification during traversal.
pub trait AstVisitorMut<'ast> {
    /// Called when a statement node is visited. Return `false` to stop recursing.
    fn visit_statement(&mut self, _stmt: &mut ast::Statement) -> bool { true }

    /// Called when an expression node is visited. Return `false` to stop recursing.
    fn visit_expression(&mut self, _expr: &mut ast::Expression) -> bool { true }

    /// Called when a binary operand node is visited. Return `false` to stop recursing.
    fn visit_operand(&mut self, _op: &mut ast::BinaryOperand) -> bool { true }

    // -- Default recursive implementations --

    /// Walk a statement and all its descendants (mutable).
    fn walk_statement(&mut self, stmt: &mut ast::Statement) {
        if !self.visit_statement(stmt) {
            return;
        }
        match stmt {
            ast::Statement::Expression(e) => self.walk_expression(e),
            ast::Statement::Block(b) => self.walk_block(b),
            ast::Statement::IfBlock(ib) => {
                self.walk_expression(&mut ib.cond);
                self.walk_block(&mut ib.if_block);
                if let Some(eb) = &mut ib.else_block {
                    self.walk_block(eb);
                }
            },
            ast::Statement::ForLoop(fl) => {
                self.walk_expression(&mut fl.expr);
                self.walk_block(&mut fl.block);
            },
            ast::Statement::WhileLoop(wl) => {
                self.walk_expression(&mut wl.expr);
                self.walk_block(&mut wl.block);
            },
            ast::Statement::Return(r) => self.walk_expression(&mut r.expr),
            ast::Statement::Yield(y) => {
                if let Some(k) = &mut y.key {
                    self.walk_expression(k);
                }
                self.walk_expression(&mut y.value);
            },
            ast::Statement::Function(f) => {
                if let Some(b) = &mut f.shared.block {
                    self.walk_block(b);
                }
            },
            ast::Statement::ImplBlock(ib) => {
                for c in &mut ib.consts {
                    if let Some(e) = &mut c.expr {
                        self.walk_expression(e);
                    }
                }
            },
            ast::Statement::TraitDef(td) => {
                for c in &mut td.consts {
                    if let Some(e) = &mut c.expr {
                        self.walk_expression(e);
                    }
                }
            },
            ast::Statement::ConstDef(c) => {
                if let Some(e) = &mut c.expr {
                    self.walk_expression(e);
                }
            },
            _ => {},
        }
    }

    /// Walk an expression and all its descendants (mutable).
    fn walk_expression(&mut self, expr: &mut ast::Expression) {
        if !self.visit_expression(expr) {
            return;
        }
        match expr {
            ast::Expression::BinaryOp(b) => {
                self.walk_operand(&mut b.left);
                self.walk_operand(&mut b.right);
            },
            ast::Expression::UnaryOp(u) => self.walk_expression(&mut u.expr),
            ast::Expression::Cast(c) => self.walk_expression(&mut c.expr),
            ast::Expression::Block(b) => self.walk_block(b),
            ast::Expression::IfBlock(ib) => {
                self.walk_expression(&mut ib.cond);
                self.walk_block(&mut ib.if_block);
                if let Some(eb) = &mut ib.else_block {
                    self.walk_block(eb);
                }
            },
            ast::Expression::MatchBlock(mb) => {
                for (_, b) in &mut mb.branches {
                    self.walk_block(b);
                }
            },
            ast::Expression::Closure(cl) => {
                if let Some(b) = &mut cl.shared.block {
                    self.walk_block(b);
                }
            },
            ast::Expression::AnonymousFunction(f) => {
                if let Some(b) = &mut f.shared.block {
                    self.walk_block(b);
                }
            },
            ast::Expression::Assignment(a) => {
                self.walk_expression(&mut a.left);
                self.walk_expression(&mut a.right);
            },
            _ => {},
        }
    }

    /// Walk a binary operand and all its descendants (mutable).
    fn walk_operand(&mut self, op: &mut ast::BinaryOperand) {
        if !self.visit_operand(op) {
            return;
        }
        match op {
            ast::BinaryOperand::Expression(e) => self.walk_expression(e),
            ast::BinaryOperand::ArgumentList(al) => {
                for arg in &mut al.args {
                    self.walk_expression(arg);
                }
            },
            ast::BinaryOperand::Member(_) => {},
        }
    }

    /// Walk a block (all statements and the optional result expression, mutable).
    fn walk_block(&mut self, block: &mut ast::Block) {
        for stmt in &mut block.statements {
            self.walk_statement(stmt);
        }
        if let Some(r) = &mut block.result {
            self.walk_expression(r);
        }
    }
}
