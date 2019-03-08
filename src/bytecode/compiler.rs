//! Bytecode emitter. Compiles bytecode from AST.

// todo: remove
#![allow(unused_variables)]

use std::collections::HashMap;
use std::fmt::Debug;
use std::cmp::max;
use crate::util::{Numeric, BindingId, FunctionId, Type, TypeId, TypeKind, Array};
use crate::frontend::{ast::{self, Bindable}, ResolvedProgram};
use crate::bytecode::{Writer, WriteConst, Program};
use crate::ExternRust;

/// Maps bindings and arguments to indices relative to the stackframe.
struct Locals {
    map     : HashMap<BindingId, i32>,
    next_arg: i32,
    next_var: i32,
}

impl Locals {
    pub fn new() -> Self {
        Locals {
            map     : HashMap::new(),
            next_arg: -4,
            next_var: 0,
        }
    }
}

/// A stack Locals mappings for nested structures.
struct LocalsStack(Vec<Locals>);

impl LocalsStack {
    fn new() -> Self {
        LocalsStack(Vec::new())
    }
    fn push(self: &mut Self, frame: Locals) {
        self.0.push(frame);
    }
    fn pop(self: &mut Self) -> Locals {
        self.0.pop().expect("Attempted to pop empty LocalsStack")
    }
    fn lookup(self: &Self, binding: &BindingId) -> Option<i32> {
        self.0.last().expect("Attempted to lookup stack item without LocalsStack").map.get(binding).map(|b| *b)
    }
}

#[derive(PartialEq)]
pub enum CompareOp {
    DontCare,
    Request,
    Eq(Type),
    Neq(Type),
    Lt(Type),
    Lte(Type),
}

impl CompareOp {
    fn write<T>(self: &Self, writer: &mut Writer<T>, jump_pos: u32) -> u32 where T: ExternRust<T> {
        match self {
            CompareOp::Eq(ty) => match ty.size().unwrap() { // todo: unwrap will fail on array
                1 | 2 | 4 => writer.jeqr32(jump_pos),
                8 => writer.jeqr64(jump_pos),
                _ => panic!("Unsupported type size"),
            }
            CompareOp::Neq(ty) => match ty.size().unwrap() {
                1 | 2 | 4 => writer.jneqr32(jump_pos),
                8 => writer.jneqr64(jump_pos),
                _ => panic!("Unsupported type size"),
            }
            CompareOp::Lt(ty) => match ty.size().unwrap() {
                1 | 2 | 4 => match ty.kind() {
                    TypeKind::Signed => writer.jlts32(jump_pos),
                    TypeKind::Unsigned => writer.jltu32(jump_pos),
                    TypeKind::Float => writer.jltf32(jump_pos),
                    _ => panic!("Unsupported type kind"),
                },
                8 => match ty.kind() {
                    TypeKind::Signed => writer.jlts64(jump_pos),
                    TypeKind::Unsigned => writer.jltu64(jump_pos),
                    TypeKind::Float => writer.jltf64(jump_pos),
                    _ => panic!("Unsupported type kind"),
                },
                _ => panic!("unsupported type size"),
            }
            CompareOp::Lte(ty) => match ty.size().unwrap() {
                1 | 2 | 4 => match ty.kind() {
                    TypeKind::Signed => writer.jltes32(jump_pos),
                    TypeKind::Unsigned => writer.jlteu32(jump_pos),
                    TypeKind::Float => writer.jltef32(jump_pos),
                    _ => panic!("Unsupported type kind"),
                },
                8 => match ty.kind() {
                    TypeKind::Signed => writer.jltes64(jump_pos),
                    TypeKind::Unsigned => writer.jlteu64(jump_pos),
                    TypeKind::Float => writer.jltef64(jump_pos),
                    _ => panic!("Unsupported type kind"),
                },
                _ => panic!("unsupported type size"),
            }
            _ => panic!("nothing to write"),
        }
    }
}

/// Bytecode emitter. Compiles bytecode from resolved program (AST).
pub struct Compiler<T> where T: ExternRust<T> {
    writer          : Writer<T>,
    /// List of bindings mapped to their TypeIds
    bindingtype_ids : Vec<TypeId>,
    /// List of registered types, effectively mapped via vector index = TypeId.
    types           : Vec<Type>,
    /// Maps from binding id to load-argument for each frame.
    locals          : LocalsStack,
    // Maps functions to their call index.
    functions       : HashMap<FunctionId, u32>,
    /// List of unresolved calls for each function.
    unresolved      : HashMap<FunctionId, Vec<u32>>,
}

/// Basic compiler functionality.
impl<'a, T> Compiler<T> where T: ExternRust<T> {

    /// Creates a new compiler.
    pub fn new() -> Self {
        Compiler {
            writer          : Writer::new(),
            types           : Vec::new(),
            bindingtype_ids : Vec::new(),
            locals          : LocalsStack::new(),
            functions       : HashMap::new(),
            unresolved      : HashMap::new(),
        }
    }

    /// Compiles the current program.
    pub fn compile(self: &mut Self, program: ResolvedProgram<'a, T>) {

        let ResolvedProgram { ast: statements, bindingtype_ids, types, entry_fn, .. } = program;

        // write placeholder jump to program entry
        let initial_pos = self.writer.position();
        self.writer.call(123, 0);
        self.writer.exit();

        // compile program
        self.types = types;
        self.bindingtype_ids = bindingtype_ids;
        for statement in statements.iter() {
            self.compile_statement(statement);
        }

        // overwrite placeholder with actual entry position
        let &entry_fn_pos = self.functions.get(&entry_fn).expect("Failed to locate entry function in generated code.");
        self.writer.overwrite(initial_pos, |w| w.call(entry_fn_pos, 0));
    }

    /// Returns compiled bytecode program.
    pub fn into_program(self: Self) -> Program<T> {
        self.writer.into_program()
    }

    /// Returns the type of the given binding.
    fn bindingtype<B>(self: &Self, item: &B) -> &Type where B: Bindable {
        let binding_id = Into::<usize>::into(item.binding_id().expect("Unresolved binding encountered."));
        let type_id = self.bindingtype_ids[binding_id];
        &self.types[Into::<usize>::into(type_id)]
    }

    /// Returns type for given type_id.
    pub fn get_type(self: &Self, type_id: Option<TypeId>) -> &Type {
        &self.types[Into::<usize>::into(type_id.expect("Unresolved type encountered."))]
    }

    /// Returns the size of the given type in bytes.
    pub fn bytesize(self: &Self, ty: &Type) -> usize {
        if let Some(size) = ty.size() {
            size as usize
        } else if let Type::Array(Array { len, type_id }) = ty {
            let ty = self.get_type(*type_id);
            self.bytesize(ty) * len.unwrap()
        } else {
            unimplemented!("sizeof {:?}", ty)
        }
    }

    /// Returns the size of the given type in stack elements.
    pub fn quadsize(self: &Self, ty: &Type) -> usize {
        (self.bytesize(ty) + 3) / 4
    }
}

/// Methods for compiling individual code structures.
impl<'a, T> Compiler<T> where T: ExternRust<T> {

    /// Compiles the given statement.
    pub fn compile_statement(self: &mut Self, item: &ast::Statement<'a>) {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.compile_function(function),
            S::Structure(structure)     => { }, // todo: handle
            S::Binding(binding)         => self.compile_binding(binding),
            S::IfBlock(if_block)        => self.compile_if_block(if_block),
            S::ForLoop(for_loop)        => { }, // todo: handle
            S::WhileLoop(while_loop)    => self.compile_while_loop(while_loop),
            S::Block(block)             => self.compile_block(block),
            S::Return(ret)              => self.compile_return(ret),
            S::Expression(expression)   => self.compile_expression(expression, &mut CompareOp::DontCare),
        }
    }

    /// Compiles the given expression.
    pub fn compile_expression(self: &mut Self, item: &ast::Expression<'a>, cond: &mut CompareOp) {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.compile_literal(literal),
            E::Variable(variable)       => self.compile_variable(variable),
            E::Call(call)               => self.compile_call(call),
            E::Assignment(assignment)   => self.compile_assignment(assignment),
            E::BinaryOp(binary_op)      => self.compile_binary_op(binary_op, cond),
            E::UnaryOp(unary_op)        => self.compile_unary_op(unary_op),
            E::Block(block)             => { }, // todo: handle,
            E::IfBlock(if_block)        => self.compile_if_block(if_block),
        };
    }

    /// Compiles the assignment operation.
    pub fn compile_assignment(self: &mut Self, item: &ast::Assignment<'a>) {
        use crate::frontend::ast::BinaryOperator as BO;
        let binding_id = item.left.binding_id.unwrap();
        let index = self.locals.lookup(&binding_id).expect("Unresolved binding encountered");
        match item.op {
            BO::Assign => {
                self.compile_expression(&item.right, &mut CompareOp::DontCare);
                let ty = self.bindingtype(&item.left).clone(); // todo sucks
                self.write_store(index, &ty);
            },
            compound_assign @ _ => {
                self.compile_expression(&item.right, &mut CompareOp::DontCare);
                let ty = self.bindingtype(&item.left).clone();  // todo sucks
                self.write_load(index, &ty);
                match compound_assign {
                    BO::Add => { self.write_add(&ty); },
                    BO::Sub => { self.write_sub(&ty); },
                    BO::Mul => { self.write_mul(&ty); },
                    BO::DivAssign => unimplemented!("divassign"),
                    BO::RemAssign => unimplemented!("remassign"),
                    _ => panic!("Unsupported assignment operator encountered"),
                };
                self.writer.storer32(index);
            },
        };
    }

    /// Compiles the given function.
    pub fn compile_function(self: &mut Self, item: &ast::Function<'a>) {

        // register function bytecode index, check if any bytecode needs fixing
        let position = self.writer.position();
        let function_id = item.function_id.unwrap();
        let num_args = item.sig.args.len() as u8;
        self.functions.insert(function_id, position);
        self.fix_targets(function_id, position, num_args);

        // create local environment
        let mut frame = Locals::new();

        for arg in item.sig.args.iter() {
            frame.next_arg -= max(0, self.bytesize(&self.bindingtype(arg)) as i32 / 4 - 1);
            frame.map.insert(arg.binding_id.unwrap(), frame.next_arg);
            frame.next_arg -= 1;
        }

        for statement in item.block.statements.iter() {
            if let ast::Statement::Binding(binding) = statement {
                let next_var = frame.next_var;
                frame.map.insert(binding.binding_id.unwrap(), next_var);
                frame.next_var += max(1, self.bytesize(&self.bindingtype(binding)) as i32 / 4);
            }
        }

        if frame.next_var > 0 {
            self.writer.reserve(frame.next_var as u8);
        }

        self.locals.push(frame);
        self.compile_block(&item.block);
        self.writer.ret(item.sig.ret.as_ref().map_or(0, |ret| self.quadsize(&self.get_type(ret.type_id)) as u8)); // todo: skip if last statement was "return"
        self.locals.pop();
    }

    /// Compiles the given call.
    pub fn compile_call(self: &mut Self, item: &ast::Call<'a>) {

        // put args on stack
        for arg in item.args.iter() { // todo: optional parameter? we need the exact signature
            self.compile_expression(arg, &mut CompareOp::DontCare);
        }

        if let Some(rust_fn_index) = item.rust_fn_index {

            // rust function
            self.writer.rustcall(T::from_u16(rust_fn_index));

        } else {

            // normal function: identify call target or write dummy
            let function_id = item.function_id.expect(&format!("Unresolved function \"{}\" encountered", item.path.0[0]));
            let call_position = self.writer.position();

            let target = if let Some(&target) = self.functions.get(&function_id) {
                target
            } else {
                self.unresolved.entry(function_id).or_insert(Vec::new()).push(call_position);
                123
            };

            self.writer.call(target, item.args.len() as u8);       // todo: get args from sig, not call (maybe?) should match once resolve does the checking
        }
    }

    /// Compiles a variable binding and optional assignment.
    pub fn compile_binding(self: &mut Self, item: &ast::Binding<'a>) {
        if let Some(expr) = &item.expr {
            self.compile_expression(expr, &mut CompareOp::DontCare);
            let binding_id = item.binding_id.unwrap();
            let index = self.locals.lookup(&binding_id).expect("Unresolved binding encountered");
            let ty = self.bindingtype(item).clone();  // todo sucks
            self.write_store(index, &ty);
        }
    }

    /// Compiles the given variable.
    pub fn compile_variable(self: &mut Self, item: &ast::Variable<'a>) {
        let load_index = {
            let binding_id = item.binding_id.expect("Unresolved binding encountered");
            self.locals.lookup(&binding_id).expect("Failed to look up local variable index")
        };
        let var_type = self.bindingtype(item).clone();  // todo sucks
        self.write_load(load_index, &var_type);
    }

    fn compile_if_only_block(self: &mut Self, item: &ast::IfBlock<'a>, cond: &mut CompareOp) {

        let exit_jump = cond.write(&mut self.writer, 123);
        self.compile_block(&item.if_block);
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| cond.write(w, exit_target));
    }

    fn compile_if_else_block(self: &mut Self, if_block: &ast::Block<'a>, else_block: &ast::Block<'a>, cond: &mut CompareOp) {

        let else_jump = cond.write(&mut self.writer, 123);
        self.compile_block(if_block);
        let exit_jump = self.writer.jmp(123);

        let else_target = self.writer.position();
        self.compile_block(else_block);

        let exit_target = self.writer.position();

        // go back and fix jump targets
        self.writer.overwrite(else_jump, |w| cond.write(w, else_target));
        self.writer.overwrite(exit_jump, |w| w.jmp(exit_target));
    }

    /// Compiles the given if block.
    pub fn compile_if_block(self: &mut Self, item: &ast::IfBlock<'a>) {

        // compile condition and jump placeholder
        let mut cmp_op = CompareOp::Request;
        self.compile_expression(&item.cond, &mut cmp_op);

        if item.else_block.is_none() {
            self.compile_if_only_block(item, &mut cmp_op);
        } else {
            self.compile_if_else_block(&item.if_block, item.else_block.as_ref().unwrap(), &mut cmp_op);
        }
    }

    /// Compiles a while loop.
    pub fn compile_while_loop(self: &mut Self, item: &ast::WhileLoop<'a>) {
        let start_target = self.writer.position;
        self.compile_expression(&item.expr, &mut CompareOp::DontCare);
        let exit_jump = self.writer.j0(123);
        self.compile_block(&item.block);
        self.writer.jmp(start_target);
        let exit_target = self.writer.position;
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
    }

    /// Compiles a return statement
    pub fn compile_return(self: &mut Self, item: &ast::Return<'a>) {
        if let Some(expr) = &item.expr {
            self.compile_expression(expr, &mut CompareOp::DontCare);
        } else {
            self.writer.lit0(); // todo: need to return something for now
        }
        self.writer.ret(item.fn_ret_type_id.map_or(0, |ret| self.quadsize(&self.get_type(Some(ret))) as u8));
    }

    /// Compiles the given block.
    pub fn compile_block(self: &mut Self, item: &ast::Block<'a>) {

        for statement in item.statements.iter() {
            self.compile_statement(statement);
        }

        if let Some(result) = &item.result {
            self.compile_expression(result, &mut CompareOp::DontCare);
        }
    }

    fn store_literal(self: &mut Self, item: &ast::Literal<'a>) -> u32 {
        use crate::frontend::ast::LiteralValue;
        let lit_type = self.bindingtype(item);
        match item.value {
            LiteralValue::Numeric(int) => {
                match int {
                    Numeric::Signed(v) => {
                        match lit_type {
                            Type::i8 => self.writer.store_const(v as i8), // todo: handle pos > 255
                            Type::i16 => self.writer.store_const(v as i16),
                            Type::i32 => self.writer.store_const(v as i32),
                            Type::i64 => self.writer.store_const(v as i64),
                            _ => panic!("Unexpected signed integer literal type: {:?}", lit_type)
                        }
                    },
                    Numeric::Unsigned(v) => {
                        match lit_type {
                            Type::i8 => self.writer.store_const(v as i8),
                            Type::i16 => self.writer.store_const(v as i16),
                            Type::i32 => self.writer.store_const(v as i32),
                            Type::i64 => self.writer.store_const(v as i64),
                            Type::u8 => self.writer.store_const(v as u8),
                            Type::u16 => self.writer.store_const(v as u16),
                            Type::u32 => self.writer.store_const(v as u32),
                            Type::u64 => self.writer.store_const(v as u64),
                            _ => panic!("Unexpected unsigned integer literal type: {:?}", lit_type)
                        }
                    },
                    Numeric::Float(v) => {
                        match lit_type {
                            Type::f32 => self.writer.store_const(v as f32),
                            Type::f64 => self.writer.store_const(v),
                            _ => panic!("Unexpected float literal type: {:?}", lit_type)
                        }
                    },
                    Numeric::Overflow => panic!("Literal computation overflow")
                }
            },
            LiteralValue::Bool(v) =>  {
                match lit_type {
                    Type::bool => self.writer.store_const(if v { 1 } else { 0 }),
                    _ => panic!("Unexpected boolean literal type: {:?}", lit_type)
                }
            },
            LiteralValue::String(v) => {
                match lit_type {
                    Type::String => self.writer.store_const(v),
                    _ => panic!("Unexpected string literal type: {:?}", lit_type)
                }
            },
            LiteralValue::Array(ref v) => {
                let ty = self.bindingtype(&v.items[0]); // todo: requires one item
                let mut pos = 0;
                if ty.is_primitive() || ty.is_array() {
                    for item in &v.items {
                        let item_pos = self.store_literal(item);
                        if pos == 0 {
                            pos = item_pos; // todo: ugly getitdone solution
                        }
                    }
                    pos
                } else {
                    for item in &v.items {
                        println!("non-primitive{:?}", item);
                    }
                    unimplemented!("non-primitive array")
                }
            },
        }
    }

    /// Compiles the given literal
    pub fn compile_literal(self: &mut Self, item: &ast::Literal<'a>) {
        use crate::frontend::ast::LiteralValue;
        let lit_type = self.bindingtype(item);
        match item.value {
            LiteralValue::Numeric(Numeric::Unsigned(0)) if lit_type.is_integer() && lit_type.size().unwrap() <= 4 => { self.writer.lit0(); }
            LiteralValue::Numeric(Numeric::Unsigned(1)) if lit_type.is_integer() && lit_type.size().unwrap() <= 4 => { self.writer.lit1(); }
            LiteralValue::Numeric(Numeric::Unsigned(2)) if lit_type.is_integer() && lit_type.size().unwrap() <= 4 => { self.writer.lit2(); }
            LiteralValue::Numeric(Numeric::Signed(-1)) if lit_type.is_signed() && lit_type.size().unwrap() <= 4 => { self.writer.litm1(); }
            LiteralValue::Numeric(int) => {
                match lit_type {
                    Type::i8 => { self.writer.lits(int.as_signed().unwrap() as i8); }
                    Type::u8 => { self.writer.litu(int.as_unsigned().unwrap() as u8); }
                    Type::i16 | Type::u16 => { let pos = self.store_literal(item); self.writer.constr16(pos as u8); } // todo: handle pos > 255
                    Type::i32 | Type::u32 | Type::f32 => { let pos = self.store_literal(item); self.writer.constr32(pos as u8); } // todo: handle pos > 255
                    Type::i64 | Type::u64 | Type::f64 => { let pos = self.store_literal(item); self.writer.constr64(pos as u8); } // todo: handle pos > 255
                    _ => panic!("Unexpected numeric literal type: {:?}", lit_type)
                }
            }
            LiteralValue::Bool(v) =>  {
                match lit_type {
                    Type::bool => { if v { self.writer.lit1(); } else { self.writer.lit0(); } },
                    _ => panic!("Unexpected boolean literal type: {:?}", lit_type)
                };
            },
            LiteralValue::String(v) => {
                match lit_type {
                    Type::String => { let pos = self.writer.store_const(v); self.writer.consto(pos as u8); },
                    _ => panic!("Unexpected string literal type: {:?}", lit_type)
                };
            },
            LiteralValue::Array(ref v) => {
                match lit_type {
                    Type::Array(_) => { let pos = self.store_literal(item); self.writer.consto(pos as u8); },
                    _ => panic!("Unexpected string literal type: {:?}", lit_type)
                };
            },
        }
    }

    /// Compiles the given unary operation.
    pub fn compile_unary_op(self: &mut Self, item: &ast::UnaryOp<'a>) {
        use crate::frontend::ast::UnaryOperator as UO;

        let exp_type = self.bindingtype(&item.expr).clone();  // todo sucks

        match item.op {
            // logical
            UO::Not => {
                self.compile_expression(&item.expr, &mut CompareOp::DontCare);
                self.writer.not();
            }
            // arithmetic
            UO::IncBefore | UO::DecBefore | UO::IncAfter | UO::DecAfter => {
                if let ast::Expression::Variable(var) = &item.expr {
                    let load_index = {
                        let binding_id = var.binding_id.expect("Unresolved binding encountered");
                        self.locals.lookup(&binding_id).expect("Failed to look up local variable index")
                    };
                    match item.op {
                        UO::IncBefore => self.write_preinc(load_index, &exp_type),
                        UO::DecBefore => self.write_predec(load_index, &exp_type),
                        UO::IncAfter => self.write_postinc(load_index, &exp_type),
                        UO::DecAfter => self.write_postdec(load_index, &exp_type),
                        _ => panic!("Internal error in operator handling"),
                    };
                } else {
                    panic!("Operator {:?} can only be used on variable bindings", item.op);
                }
            },
        }
    }

    /// Compiles the given binary operation.
    pub fn compile_binary_op(self: &mut Self, item: &ast::BinaryOp<'a>, cond: &mut CompareOp) {
        use crate::frontend::ast::BinaryOperator as BO;

        if (*cond == CompareOp::DontCare && (item.op == BO::Greater || item.op == BO::GreaterOrEq)) ||
           (*cond == CompareOp::Request && (item.op == BO::Less || item.op == BO::LessOrEq)) {
            // implement these via Less/LessOrEq + swapping arguments,
            // however: don't swap if a jump is to be generated since those actually have to be inverted
            //   for those, inversion takes place below
            self.compile_expression(&item.left, &mut CompareOp::DontCare);
            self.compile_expression(&item.right, &mut CompareOp::DontCare);
        } else {
            self.compile_expression(&item.right, &mut CompareOp::DontCare);
            self.compile_expression(&item.left, &mut CompareOp::DontCare);
        }

        let result_type = self.bindingtype(item).clone();
        let compare_type = self.bindingtype(&item.left).clone();

        match item.op {
            // arithmetic
            BO::Add => { self.write_add(&result_type); },
            BO::Sub => { self.write_sub(&result_type); },
            BO::Mul => { self.write_mul(&result_type); },
            BO::Div => unimplemented!("div"),
            BO::Rem => unimplemented!("rem"),
            // assigments
            BO::Assign => unimplemented!("assign"), // fixme: thesere are handled in compile_assignment!
            BO::AddAssign => unimplemented!("addassign"),
            BO::SubAssign => unimplemented!("subassign"),
            BO::MulAssign => unimplemented!("mulassign"),
            BO::DivAssign => unimplemented!("divassgi"),
            BO::RemAssign => unimplemented!("remassign"),
            // comparison
            BO::Less | BO::Greater => {
                // Less/Greater comparison. For Greater, arguments have been swapped above (unless jump). For jumps Lt becomes Lte.
                if *cond == CompareOp::Request { *cond = CompareOp::Lte(compare_type); } else { self.write_lt(&compare_type); }
            },
            BO::LessOrEq | BO::GreaterOrEq => {
                // LessOrEq/GreaterOrEq comparison. For Greater, arguments have been swapped above (unless jump). For jumps Lte becomes Lt.
                if *cond == CompareOp::Request { *cond = CompareOp::Lt(compare_type); } else { self.write_lte(&compare_type); }
            }
            BO::Equal => {
                // Eq comparison. for jumps, we have to invert this to Neq
                if *cond == CompareOp::Request { *cond = CompareOp::Neq(compare_type); } else { self.write_eq(&compare_type); }
            },
            BO::NotEqual => {
                // Neq comparison. for jumps, we have to invert this to Eq
                if *cond == CompareOp::Request { *cond = CompareOp::Eq(compare_type); } else { self.write_neq(&compare_type); }
            },
            // boolean
            BO::And => { self.writer.and(); },
            BO::Or => { self.writer.or(); },
            // special
            BO::Range => unimplemented!("range"),
            BO::Index => unimplemented!("index"),
        }
    }
}

impl<'a, T> Compiler<T> where T: ExternRust<T> {
    /// Fixes function call targets for previously not generated functions.
    fn fix_targets(self: &mut Self, function_id: FunctionId, position: u32, num_args: u8) {
        if let Some(targets) = self.unresolved.remove(&function_id) {
            let backup_position = self.writer.position();
            for &target in targets.iter() {
                self.writer.set_position(target);
                self.writer.call(position, num_args); // todo: may have to handle multiple call types
            }
            self.writer.set_position(backup_position);
        }
    }
    fn write_preinc(self: &mut Self, index: i32, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.preinci64(index),
            ref ty @ _ if ty.is_integer() && ty.size().unwrap() <= 4 => self.writer.preinci(index),
            ty @ _ => panic!("Unsupported Inc operand {:?}", ty),
        };
    }
    fn write_predec(self: &mut Self, index: i32, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.predeci64(index),
            ref ty @ _ if ty.is_integer() && ty.size().unwrap() <= 4 => self.writer.predeci(index),
            ty @ _ => panic!("Unsupported Dec operand {:?}", ty),
        };
    }
    fn write_postinc(self: &mut Self, index: i32, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.postinci64(index),
            ref ty @ _ if ty.is_integer() && ty.size().unwrap() <= 4 => self.writer.postinci(index),
            ty @ _ => panic!("Unsupported Inc operand {:?}", ty),
        };
    }
    fn write_postdec(self: &mut Self, index: i32, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.postdeci64(index),
            ref ty @ _ if ty.is_integer() && ty.size().unwrap() <= 4 => self.writer.postdeci(index),
            ty @ _ => panic!("Unsupported Dec operand {:?}", ty),
        };
    }
    fn write_sub(self: &mut Self, ty: &Type) {
        match ty {
            Type::f64 => self.writer.subf64(),
            Type::i64 | Type::u64 => self.writer.subi64(),
            Type::f32 => self.writer.subf(),
            ref ty @ _ if ty.is_integer() && ty.size().unwrap() <= 4 => self.writer.subi(),
            ty @ _ => panic!("Unsupported Sub operand {:?}", ty),
        };
    }
    fn write_add(self: &mut Self, ty: &Type) {
        match ty {
            Type::f64 => self.writer.addf64(),
            Type::i64 | Type::u64 => self.writer.addi64(),
            Type::f32 => self.writer.addf(),
            ref ty @ _ if ty.is_integer() && ty.size().unwrap() <= 4 => self.writer.addi(),
            ty @ _ => panic!("Unsupported Add operand {:?}", ty),
        };
    }
    fn write_mul(self: &mut Self, ty: &Type) {
        match ty {
            Type::f64 => self.writer.mulf64(),
            Type::i64 | Type::u64 => self.writer.muli64(),
            Type::f32 => self.writer.mulf(),
            ref ty @ _ if ty.is_integer() && ty.size().unwrap() <= 4 => self.writer.muli(),
            ty @ _ => panic!("Unsupported Mul operand {:?}", ty),
        };
    }
    fn write_eq(self: &mut Self, ty: &Type) {
        match self.bytesize(ty) {
            1 | 2 | 4 => self.writer.ceqr32(),
            8 => self.writer.ceqr64(),
            _ => panic!("Unsupported type size"),
        };
    }
    fn write_neq(self: &mut Self, ty: &Type) {
        match self.bytesize(ty) {
            1 | 2 | 4 => self.writer.cneqr32(),
            8 => self.writer.cneqr64(),
            _ => panic!("Unsupported type size"),
        };
    }
    fn write_lt(self: &mut Self, ty: &Type) {
        match self.bytesize(ty) {
            1 | 2 | 4 => match ty.kind() {
                TypeKind::Signed => self.writer.clts32(),
                TypeKind::Unsigned => self.writer.cltu32(),
                TypeKind::Float => self.writer.cltf32(),
                _ => panic!("Unsupported type kind"),
            },
            8 => match ty.kind() {
                TypeKind::Signed => self.writer.clts64(),
                TypeKind::Unsigned => self.writer.cltu64(),
                TypeKind::Float => self.writer.cltf64(),
                _ => panic!("Unsupported type kind"),
            },
            _ => panic!("unsupported type size"),
        };
    }
    fn write_lte(self: &mut Self, ty: &Type) {
        match self.bytesize(ty) {
            1 | 2 | 4 => match ty.kind() {
                TypeKind::Signed => self.writer.cltes32(),
                TypeKind::Unsigned => self.writer.clteu32(),
                TypeKind::Float => self.writer.cltef32(),
                _ => panic!("Unsupported type kind"),
            },
            8 => match ty.kind() {
                TypeKind::Signed => self.writer.cltes64(),
                TypeKind::Unsigned => self.writer.clteu64(),
                TypeKind::Float => self.writer.cltef64(),
                _ => panic!("Unsupported type kind"),
            },
            _ => panic!("unsupported type size"),
        };
    }
    /// Writes an appropriate variant of the store instruction.
    fn write_store(self: &mut Self, index: i32, ty: &Type) {
        let size = self.bytesize(ty);
        let kind = ty.kind();
        if kind == TypeKind::String || kind == TypeKind::Array {
            self.writer.storer32(index);
        } else if size <= 4 {
            self.writer.storer32(index);
        } else if size == 8 {
            self.writer.storer64(index);
        } else {
            panic!("Unsupported type {:?} for store operation", ty);
        }
    }
    /// Writes an appropriate variant of the load instruction.
    fn write_load(self: &mut Self, index: i32, ty: &Type) {
        let size = self.bytesize(ty);
        let kind = ty.kind();
        if kind == TypeKind::String || kind == TypeKind::Array {
            self.writer.loadr32(index);
        } else if size <= 4 {
            match index {
                -4 => self.writer.load_arg1(),
                -5 => self.writer.load_arg2(),
                -6 => self.writer.load_arg3(),
                _ => self.writer.loadr32(index),
            };
        } else if size == 8 {
            self.writer.loadr64(index);
        } else {
            panic!("Unsupported type {:?} for load operation", ty);
        }
    }
}

/// Compiles a resolved program into bytecode.
pub fn compile<'a, T>(program: ResolvedProgram<'a, T>) -> Program<T> where T: ExternRust<T>+Debug {
    let mut compiler = Compiler::new();
    compiler.compile(program);
    compiler.into_program()
}