//! Bytecode emitter. Compiles bytecode from AST.

// todo: remove
#![allow(unused_variables)]

use std::collections::HashMap;
use std::fmt::Debug;
use std::cmp::max;
use crate::frontend::{ast, ResolvedProgram, util::{Integer, BindingId, FunctionId, TypeSlot, Type, TypeKind}};
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
            CompareOp::Eq(ty) => match ty.size() {
                1 | 2 | 4 => writer.jeqr32(jump_pos),
                8 => writer.jeqr64(jump_pos),
                _ => panic!("Unsupported type size"),
            }
            CompareOp::Neq(ty) => match ty.size() {
                1 | 2 | 4 => writer.jneqr32(jump_pos),
                8 => writer.jneqr64(jump_pos),
                _ => panic!("Unsupported type size"),
            }
            CompareOp::Lt(ty) => match ty.size() {
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
            CompareOp::Lte(ty) => match ty.size() {
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
            writer      : Writer::new(),
            types       : Vec::new(),
            locals      : LocalsStack::new(),
            functions   : HashMap::new(),
            unresolved  : HashMap::new(),
        }
    }

    /// Compiles the current program.
    pub fn compile(self: &mut Self, program: ResolvedProgram<'a, T>) {

        let ResolvedProgram { ast: statements, types, entry_fn, .. } = program;

        // write placeholder jump to program entry
        let initial_pos = self.writer.position();
        self.writer.call(123, 0);
        self.writer.exit();

        // compile program
        self.types = types;
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

    /// Returns type for given AST type slot.
    pub fn get_type(self: &Self, type_id: TypeSlot) -> Type {
        self.types[Into::<usize>::into(type_id.unwrap())].clone()
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
            E::UnaryOp(unary_op)        => { }, // todo: handle,
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
                self.writer.storer32(index);
            },
            compound_assign @ _ => {
                self.compile_expression(&item.right, &mut CompareOp::DontCare);
                self.writer.loadr32(index);
                match compound_assign {
                    BO::AddAssign => self.writer.add(),
                    BO::SubAssign => self.writer.sub(),
                    BO::MulAssign => self.writer.mul(),
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
            frame.next_arg -= max(0, self.get_type(arg.type_id).size() as i32 / 4 - 1);
            frame.map.insert(arg.binding_id.unwrap(), frame.next_arg);
            frame.next_arg -= 1;
        }

        for statement in item.block.statements.iter() {
            if let ast::Statement::Binding(binding) = statement {
                let next_var = frame.next_var;
                frame.map.insert(binding.binding_id.unwrap(), next_var);
                frame.next_var += max(1, self.get_type(binding.type_id).size() as i32 / 4);
            }
        }

        if frame.next_var > 0 {
            self.writer.reserve(frame.next_var as u8);
        }

        self.locals.push(frame);
        self.compile_block(&item.block);
        self.writer.ret(); // todo: skip if last statement was "return"
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
            self.writer.storer32(self.locals.lookup(&binding_id).expect("Unresolved binding encountered"));
        }
    }

    /// Compiles the given variable.
    pub fn compile_variable(self: &mut Self, item: &ast::Variable<'a>) {
        let load_index = {
            let binding_id = item.binding_id.expect("Unresolved binding encountered");
            self.locals.lookup(&binding_id).expect("Failed to look up local variable index")
        };
        let var_type = self.get_type(item.type_id);
        self.write_load(load_index, var_type.size());
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
        self.writer.ret();
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

    /// Compiles the given literal
    pub fn compile_literal(self: &mut Self, item: &ast::Literal<'a>) {
        use crate::frontend::ast::LiteralValue;
        let lit_type = self.get_type(item.type_id);
        match item.value {
            LiteralValue::Integer(int) => {
                match int {
                    Integer::Signed(0) | Integer::Unsigned(0) => { self.writer.lit0(); }
                    Integer::Signed(1) | Integer::Unsigned(1) => { self.writer.lit1(); }
                    Integer::Signed(2) | Integer::Unsigned(2) => { self.writer.lit2(); }
                    Integer::Signed(-1) => { self.writer.litm1(); }
                    Integer::Signed(v) => {
                        match lit_type {
                            Type::i8 => { let pos = self.writer.write_const(v as i8); self.writer.constr32(pos as u8); }
                            Type::i16 => { let pos = self.writer.write_const(v as i16); self.writer.constr32(pos as u8); }
                            Type::i32 => { let pos = self.writer.write_const(v as i32); self.writer.constr32(pos as u8); }
                            Type::i64 => { let pos = self.writer.write_const(v as i64); self.writer.constr64(pos as u8); }
                            _ => panic!("Unexpected signed integer literal type")
                        }
                    }
                    Integer::Unsigned(v) => {
                        match lit_type {
                            Type::u8 | Type::u16 | Type::u32 => { let pos = self.writer.write_const(v as u32); self.writer.constr32(pos as u8); }
                            Type::u64 => { let pos = self.writer.write_const(v as u64); self.writer.constr64(pos as u8); }
                            _ => panic!("Unexpected signed integer literal type")
                        }
                    }
                }
            }
            LiteralValue::Float(v) => {
                match lit_type {
                    Type::f32 => { let pos = self.writer.write_const(v as f32); self.writer.constr32(pos as u8); },
                    Type::f64 => { let pos = self.writer.write_const(v); self.writer.constr64(pos as u8); },
                    _ => panic!("Encountered non-float literal with a float type-id")
                };

            },
            _ => unimplemented!(),
        };
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

        let result_type = self.get_type(item.type_id);
        let compare_type = self.get_type(item.left.get_type_id());

        match item.op {
            // arithmetic
            BO::Add => { match result_type {
                Type::f64 => self.writer.addf64(),
                Type::f32 => self.writer.addf32(),
                _ => self.writer.add(),
            }; },
            BO::Sub => { match result_type {
                Type::f64 => self.writer.subf64(),
                Type::f32 => self.writer.subf32(),
                _ => self.writer.sub(),
            }; },
            BO::Mul => unimplemented!("mul"),
            BO::Div => unimplemented!("div"),
            BO::Rem => unimplemented!("rem"),
            // assigments
            BO::Assign => unimplemented!("assign"),
            BO::AddAssign => unimplemented!("addassign"),
            BO::SubAssign => unimplemented!("subassign"),
            BO::MulAssign => unimplemented!("mulassign"),
            BO::DivAssign => unimplemented!("divassgi"),
            BO::RemAssign => unimplemented!("remassign"),
            // comparison
            BO::Less | BO::Greater => {
                // Less/Greater comparison. For Greater, arguments have been swapped above (unless jump). For jumps Lt becomes Lte.
                if *cond == CompareOp::Request { *cond = CompareOp::Lte(compare_type); } else { self.write_lt(compare_type); }
            },
            BO::LessOrEq | BO::GreaterOrEq => {
                // LessOrEq/GreaterOrEq comparison. For Greater, arguments have been swapped above (unless jump). For jumps Lte becomes Lt.
                if *cond == CompareOp::Request { *cond = CompareOp::Lt(compare_type); } else { self.write_lte(compare_type); }
            }
            BO::Equal => {
                // Eq comparison. for jumps, we have to invert this to Neq
                if *cond == CompareOp::Request { *cond = CompareOp::Neq(compare_type); } else { self.write_eq(compare_type); }
            },
            BO::NotEqual => {
                // Neq comparison. for jumps, we have to invert this to Eq
                if *cond == CompareOp::Request { *cond = CompareOp::Eq(compare_type); } else { self.write_neq(compare_type); }
            },
            // boolean
            BO::And => unimplemented!("and"),
            BO::Or => unimplemented!("or"),
            // special
            BO::Range => unimplemented!("range"),
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
    fn write_eq(self: &mut Self, ty: Type) {
        match ty.size() {
            1 | 2 | 4 => self.writer.ceqr32(),
            8 => self.writer.ceqr64(),
            _ => panic!("Unsupported type size"),
        };
    }
    fn write_neq(self: &mut Self, ty: Type) {
        match ty.size() {
            1 | 2 | 4 => self.writer.cneqr32(),
            8 => self.writer.cneqr64(),
            _ => panic!("Unsupported type size"),
        };
    }
    fn write_lt(self: &mut Self, ty: Type) {
        match ty.size() {
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
    fn write_lte(self: &mut Self, ty: Type) {
         match ty.size() {
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
    /// Writes an appropriate variant of the load instruction.
    fn write_load(self: &mut Self, index: i32, size: u8) {
        if size <= 4 {
            match index {
                -4 => self.writer.load_arg1(),
                -5 => self.writer.load_arg2(),
                -6 => self.writer.load_arg3(),
                _ => self.writer.loadr32(index),
            };
        } else if size == 8 {
            self.writer.loadr64(index);
        }
    }
}

/// Compiles a resolved program into bytecode.
pub fn compile<'a, T>(program: ResolvedProgram<'a, T>) -> Program<T> where T: ExternRust<T>+Debug {
    let mut compiler = Compiler::new();
    compiler.compile(program);
    compiler.into_program()
}