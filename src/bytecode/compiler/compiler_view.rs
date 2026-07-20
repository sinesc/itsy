//! View type compilation support.

use crate::StackAddress;
use crate::ItemIndex;
use crate::shared::{MetaContainer, meta::ViewType, typed_ids::TypeId};
use crate::frontend::ast::{self, Typeable};
use crate::bytecode::{Constructor, VMFunc, HeapRefOp, builtins::{Builtin, builtin_types::View}};
use crate::bytecode::writer::StoreConst;
use super::{Compiler, CompileResult};
use super::error::{OptionToCompileError, CompileError, CompileErrorKind};
use super::macros::comment;

impl<'ast, T> Compiler<'ast, T> where T: VMFunc<T> {
    /// Try to compile a view-load from a BinaryOp offsetting operation.
    /// Handles both simple view indexing (`view[i]`) and view access chains (`view[i].field`).
    /// Returns true if the operation was a view load and was compiled.
    pub(super) fn try_compile_view_load(self: &mut Self, item: &ast::BinaryOp) -> CompileResult<bool> {
        use crate::frontend::ast::BinaryOperator::*;
        let compare_type = self.ty(&item.left);

        // View access chain read: `view[i].field` (possibly nested: `view[i].a.b`)
        if item.op == Access
            && let Some(index_bin) = Self::find_view_index_in_chain(item)
            && let Some(stride) = self.ty(&index_bin.left).as_view().map(|v| v.packed_size as u16)
        {
            let result_size = self.ty(item).primitive_size();
            let left_expr = index_bin.left.as_expression().ice().unwrap().clone();
            let right_expr = index_bin.right.as_expression().ice().unwrap().clone();
            let offset = self.compute_view_chain_offset(item, index_bin)?;
            comment!(self, "view chain load offset={}", offset);
            self.compile_expression(&left_expr)?;  // stack: view_ref
            self.compile_expression(&right_expr)?; // stack: view_ref index
            self.compile_view_load_size(result_size, stride, offset)?;
            return Ok(true);
        }

        // View index: `view[i]` where the element is primitive
        if let Some(view_ty) = compare_type.as_view()
            && item.op == Index
        {
            // Struct and data enum elements cannot be loaded as a whole — users must use field access
            let elem = self.type_by_id(view_ty.element_type_id);
            let is_complex = elem.as_struct().is_some()
                || elem.as_enum().map(|e| e.primitive.is_none()).unwrap_or(false);
            let stride = view_ty.packed_size as u16;
            if is_complex {
                return Err(CompileError::new(
                    item,
                    CompileErrorKind::Unsupported(format!(
                        "cannot load full {} element from view; use field access (e.g. `view[i].field`) instead",
                        if elem.as_struct().is_some() { "struct" } else { "data enum" }
                    )),
                    &self.module_path,
                ));
            }
            let result_size = self.ty(item).primitive_size();
            self.compile_expression(item.left.as_expression().ice()?)?;  // stack: view_ref
            self.compile_expression(item.right.as_expression().ice()?)?; // stack: view_ref index
            self.compile_view_load_size(result_size, stride, 0)?;
            return Ok(true);
        }

        Ok(false)
    }

    /// Compile a view element load: `view[index]` where the element is primitive.
    /// The view reference and index are already on the stack.
    pub(super) fn compile_view_load_size(self: &mut Self, result_size: u8, stride: u16, field_offset: u16) -> CompileResult {
        match result_size {
            1 => self.writer.view_load8(stride, field_offset),
            2 => self.writer.view_load16(stride, field_offset),
            4 => self.writer.view_load32(stride, field_offset),
            8 => self.writer.view_load64(stride, field_offset),
            _ => Self::ice(&format!("view_load: Unsupported size {}", result_size))?,
        };
        Ok(())
    }

    /// Compile a view element store: `view[index] = value` where the element is primitive or a data enum.
    pub(super) fn compile_view_store_simple(self: &mut Self, bin: &ast::BinaryOp, right: &ast::Expression) -> CompileResult {
        let view_ty = self.ty(&bin.left).as_view().ice()?;
        let stride = view_ty.packed_size as u16;
        let elem_type_id = view_ty.element_type_id;

        // Data enum element: unpack the heap object into the view buffer
        let elem = self.type_by_id(elem_type_id);
        if let Some(enum_ty) = elem.as_enum() && enum_ty.primitive.is_none() {
            return self.compile_view_store_data_enum(
                bin.left.as_expression().ice()?,
                bin.right.as_expression().ice()?,
                right,
                stride,
                0u16,
                view_ty.packed_size as u16,
                elem_type_id,
            );
        }

        // Primitive element: direct store
        comment!(self, "view store");
        let result_size = self.ty(bin).primitive_size();
        let field_offset = 0u16;
        self.compile_expression(bin.left.as_expression().ice()?)?;  // stack: view_ref
        self.compile_expression(bin.right.as_expression().ice()?)?; // stack: view_ref index
        self.compile_expression(right)?;                              // stack: view_ref index value
        self.write_view_store(result_size, stride, field_offset)?;
        Ok(())
    }

    /// Compile a ViewAccess expression: `view[index].field` with pre-computed offset.
    pub(super) fn compile_view_access(self: &mut Self, item: &ast::ViewAccess) -> CompileResult {
        let stride = self.type_by_id(item.view_expr.type_id(self).ice()?).as_view().ice()?.packed_size as u16;
        let field_size = self.type_by_id(item.field_type_id).primitive_size();

        comment!(self, "view access offset={}", item.field_offset);
        self.compile_expression(&item.view_expr)?;  // stack: view_ref
        self.compile_expression(&item.index_expr)?; // stack: view_ref index

        if item.is_write {
            // The value is already on the stack from the assignment compilation
            self.write_view_store(field_size, stride, item.field_offset)?;
        } else {
            self.compile_view_load_size(field_size, stride, item.field_offset)?;
        }
        Ok(())
    }

    /// Write a view store instruction based on the element size.
    fn write_view_store(self: &mut Self, result_size: u8, stride: u16, offset: u16) -> CompileResult {
        match result_size {
            1 => self.writer.view_store8(stride, offset),
            2 => self.writer.view_store16(stride, offset),
            4 => self.writer.view_store32(stride, offset),
            8 => self.writer.view_store64(stride, offset),
            _ => Self::ice(&format!("view_store: Unsupported size {}", result_size))?,
        };
        Ok(())
    }

    /// Check if the match subject is `view[i]` where the view element type is a data enum.
    /// Returns the view's packed_size as stride if so, None otherwise.
    pub(super) fn detect_view_data_enum_subject(self: &Self, expr: &ast::Expression) -> Option<u16> {
        let bin = expr.as_binary_op()?;
        if bin.op != ast::BinaryOperator::Index {
            return None;
        }
        let left_type = self.ty(&bin.left);
        let view_ty = left_type.as_view()?;
        let elem = self.type_by_id(view_ty.element_type_id);
        // Only data enums (non-primitive enums) need the view-aware match path
        elem.as_enum().and_then(|e| e.primitive.is_none().then_some(view_ty.packed_size as u16))
    }

    /// Walk a BinaryOp chain (Access/AccessWrite nodes) to find the innermost Index/IndexWrite node.
    /// Returns None if no Index node is found.
    pub(super) fn find_view_index_in_chain(bin: &ast::BinaryOp) -> Option<&ast::BinaryOp> {
        use crate::frontend::ast::BinaryOperator as BO;
        let mut current = bin;
        loop {
            if let Some(left_expr) = current.left.as_expression().and_then(|e| e.as_binary_op()) {
                if left_expr.op == BO::Index || left_expr.op == BO::IndexWrite {
                    return Some(left_expr);
                }
                if left_expr.op == BO::Access || left_expr.op == BO::AccessWrite {
                    current = left_expr;
                    continue;
                }
            }
            return None;
        }
    }

    /// Compile a view access chain store: `view[i].field = value`.
    /// `access_bin` is the Access/AccessWrite BinaryOp, `index_bin` is the inner Index BinaryOp.
    pub(super) fn compile_view_store_chain(self: &mut Self, access_bin: &ast::BinaryOp, index_bin: &ast::BinaryOp, right: &ast::Expression) -> CompileResult {
        let view_ty = self.ty(&index_bin.left).as_view().ice()?;
        let stride = view_ty.packed_size as u16;
        let field_type_id = access_bin.type_id(self).ice()?;
        let field_type = self.type_by_id(field_type_id);
        let offset = self.compute_view_chain_offset(access_bin, index_bin)?;

        // data enum field: unpack the heap object into the view buffer
        if let Some(enum_ty) = field_type.as_enum() && enum_ty.primitive.is_none() {
            return self.compile_view_store_data_enum(
                index_bin.left.as_expression().ice()?,
                index_bin.right.as_expression().ice()?,
                right,
                stride,
                offset,
                view_ty.packed_size as u16,
                field_type_id,
            );
        }

        // primitive field: direct store
        comment!(self, "view chain store offset={}", offset);
        let field_size = field_type.primitive_size();
        self.compile_expression(index_bin.left.as_expression().ice()?)?;  // stack: view_ref
        self.compile_expression(index_bin.right.as_expression().ice()?)?; // stack: view_ref index
        self.compile_expression(right)?;                                     // stack: view_ref index value
        self.write_view_store(field_size, stride, offset)?;
        Ok(())
    }

    /// Compile a view store for a data enum element or field.
    fn compile_view_store_data_enum(self: &mut Self, view_expr: &ast::Expression, index_expr: &ast::Expression, right: &ast::Expression, stride: u16, base_offset: u16, _packed_size: u16, enum_type_id: TypeId) -> CompileResult {
        // Compile RHS (heap_ref), then view_expr and index_expr.
        // Clone heap_ref so heap_to_view can consume it while the original stays for cleanup.
        self.compile_expression(right)?;              // stack: heap_ref
        self.write_cnt(self.type_by_id(enum_type_id), true, HeapRefOp::Inc)?;
        self.writer.clone(8);                         // stack: heap_ref heap_ref_c
        self.compile_expression(view_expr)?;          // stack: heap_ref heap_ref_c view_ref
        self.compile_expression(index_expr)?;         // stack: heap_ref heap_ref_c view_ref index

        let constructor = self.constructor(self.type_by_id(enum_type_id))?;
        self.writer.heap_to_view(stride, base_offset, constructor); // pops index view_ref heap_ref_c
                                                              // stack: heap_ref

        // cleanup: decrement heap_ref refcount
        self.write_cnt(self.type_by_id(enum_type_id), false, HeapRefOp::Dec)?;

        Ok(())
    }

    /// Compute the byte offset for a view access chain by walking the struct fields.
    fn compute_view_chain_offset(self: &Self, access_bin: &ast::BinaryOp, index_bin: &ast::BinaryOp) -> CompileResult<u16> {
        let view_type = self.ty(&index_bin.left);
        let view_ty = view_type.as_view().ice()?;
        let mut current_type_id = view_ty.element_type_id;
        let mut offset: u16 = 0;

        // Collect the access chain from outermost to innermost, then process in reverse
        let mut chain: Vec<&ast::BinaryOp> = Vec::new();
        let mut current = access_bin;
        loop {
            chain.push(current);
            if let Some(left_expr) = current.left.as_expression().and_then(|e| e.as_binary_op())
                && (left_expr.op == ast::BinaryOperator::Access || left_expr.op == ast::BinaryOperator::AccessWrite)
            {
                current = left_expr;
            } else {
                break;
            }
        }
        // Process from innermost to outermost
        for &node in chain.iter().rev() {
            if let Some(member) = node.right.as_member() {
                let struct_ty = self.type_by_id(current_type_id).as_struct().ice()?;
                let field_type_id = struct_ty.fields.get(&member.ident.name).ice()?.ice()?;
                offset += self.compute_member_offset(struct_ty, &member.ident.name) as u16;
                current_type_id = field_type_id;
            }
        }

        Ok(offset)
    }

    /// Store the constructor for a View type in the const pool.
    pub(super) fn store_constructor_view(self: &Self) {
        self.writer.store_const(Constructor::Array);
        let len_pos = self.writer.const_len();
        self.writer.store_const(123 as ItemIndex); // placeholder
        self.writer.store_const(Constructor::Primitive);
        self.writer.store_const(1 as ItemIndex); // primitive byte count (u8)
        let inner_len = self.writer.const_len() - len_pos - 1;
        self.writer.update_const(len_pos, inner_len as ItemIndex);
    }

    /// Write a view builtin call, passing packed_size as the second parameter.
    pub(super) fn write_view_builtin(self: &Self, view_ty: &ViewType, builtin: View) -> CompileResult<StackAddress> {
        let packed_size = view_ty.packed_size;
        #[allow(unreachable_patterns)]
        Ok(match builtin {
            View::len => self.writer.call_builtinx(Builtin::view_len, 0, packed_size as StackAddress),
            View::wrap => self.writer.call_builtinx(Builtin::view_wrap, 0, packed_size as StackAddress),
            View::new => self.writer.call_builtinx(Builtin::view_new, 0, packed_size as StackAddress),
        })
    }
}
