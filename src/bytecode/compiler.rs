//! Bytecode emitter. Compiles bytecode from AST.

mod locals;
#[macro_use]
mod macros;

use std::{cell::RefCell, cell::Cell, collections::HashMap, fmt::{self, Display}};
use crate::util::{Numeric, FunctionId, Type, TypeId, FnKind, Bindings, Constructor, compute_loc, TypeContainer, Struct, StackAddress, StackOffset, ItemCount, STACK_ADDRESS_TYPE};
use crate::frontend::{ast::{self, Bindable, Returns, Positioned, CallType, Position}, ResolvedProgram};
use crate::bytecode::{Writer, StoreConst, Program, ARG1, ARG2, ARG3};
use crate::runtime::{VMFunc, HeapRefOp};
use locals::{Local, Locals, LocalsStack};

/// Represents the various possible compiler error-kinds.
#[derive(Clone, Debug)]
pub enum CompileErrorKind {
    Error
}

/// An error reported by the compiler.
#[derive(Clone, Debug)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    position: Position, // this is the position from the end of the input
}

impl CompileError {
    fn new(item: &impl Positioned, kind: CompileErrorKind) -> CompileError {
        Self { kind: kind, position: item.position() }
    }
    /// Computes and returns the source code location of this error. Since the AST only stores byte
    /// offsets, the original source is required to recover line and column information.
    pub fn loc(self: &Self, input: &str) -> (Position, Position) {
        compute_loc(input, input.len() as Position - self.position)
    }
}

impl Display for CompileError {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            CompileErrorKind::Error => write!(f, "Internal error"),
            //_ => write!(f, "{:?}", self.kind),
        }
    }
}

type CompileResult = Result<(), CompileError>;

#[derive(Clone, Copy)]
struct CallInfo {
    arg_size: StackAddress,
    addr: StackAddress,
}

impl CallInfo {
    const PLACEHOLDER: Self = Self { addr: 123, arg_size: 0 };
}

/// Bytecode emitter. Compiles bytecode from resolved program (AST).
pub struct Compiler<'ty, T> where T: VMFunc<T> {
    /// Bytecode writer used to output to.
    writer: Writer<T>,
    /// Type and mutability data for each binding.
    bindings: Bindings,
    /// Maps from binding id to load-argument for each frame.
    locals: LocalsStack,
    /// Non-primitive type constructors.
    constructors: HashMap<&'ty Type, StackAddress>,
    // Maps functions to their call index.
    functions: RefCell<HashMap<FunctionId, CallInfo>>,
    /// Bytecode locations of function call instructions that need their target address fixed (because the target wasn't written yet).
    unfixed_function_calls: RefCell<HashMap<FunctionId, Vec<StackAddress>>>,
    /// Set to true while compiling expressions within a Struct/Array constructor. Prevents writing additional constructors for nested structures. TODO: Crappy solution.
    writing_protype_instructions: Cell<bool>,
}

/// Compiles a resolved program into bytecode.
pub fn compile<'ast, T>(program: ResolvedProgram<'ast, T>) -> Result<Program<T>, CompileError> where T: VMFunc<T> {

    let ResolvedProgram { ast: statements, bindings, entry_fn, .. } = program;

    let mut compiler = Compiler {
        writer      : Writer::new(),
        bindings    : bindings,
        locals      : LocalsStack::new(),
        functions   : RefCell::new(HashMap::new()),
        unfixed_function_calls: RefCell::new(HashMap::new()),
        constructors: HashMap::new(),
        writing_protype_instructions: Cell::new(false),
    };

    // serialize constructors onto const pool
    for ty in compiler.bindings.types().iter() {
        if !ty.is_primitive() {
            let position = compiler.store_constructor(ty);
            compiler.constructors.insert(ty, position);
        }
    }

    // write placeholder jump to program entry
    let initial_pos = compiler.writer.call(123, 0);
    compiler.writer.exit();

    // compile program
    for statement in statements.0.iter() {
        compiler.compile_statement(statement)?;
    }

    // overwrite placeholder with actual entry position
    let &entry_call = compiler.functions.borrow().get(&entry_fn).expect("Failed to locate entry function in generated code.");
    compiler.writer.overwrite(initial_pos, |w| w.call(entry_call.addr, entry_call.arg_size));

    // return generated program
    Ok(compiler.writer.into_program())
}

/// Methods for compiling individual code structures.
impl<'ast, 'ty, T> Compiler<'ty, T> where T: VMFunc<T> {

    /// Compiles the given statement.
    fn compile_statement(self: &Self, item: &ast::Statement<'ast>) -> CompileResult {
        use self::ast::Statement as S;
        match item {
            S::Function(function)       => self.compile_function(function),
            S::StructDef(_)             => Ok(()),
            S::Binding(binding)         => self.compile_binding(binding),
            S::IfBlock(if_block)        => {
                self.compile_if_block(if_block)?;
                if let Some(result) = &if_block.if_block.result {
                    let result_type = self.binding_type(result);
                    self.write_discard(result_type);
                }
                Ok(())
            }
            S::ForLoop(for_loop)        => self.compile_for_loop(for_loop),
            S::WhileLoop(while_loop)    => self.compile_while_loop(while_loop),
            S::Block(block)             => {
                self.compile_block(block)?;
                if let Some(result) = &block.result {
                    let result_type = self.binding_type(result);
                    self.write_discard(result_type);
                }
                Ok(())
            }
            S::Return(_)                => unreachable!("Return AST nodes should have been rewritten"),
            S::Expression(expression)   => {
                self.compile_expression(expression)?;
                let result_type = self.binding_type(expression);
                self.write_discard(result_type);
                Ok(())
            }
        }
    }

    /// Compiles the given expression.
    fn compile_expression(self: &Self, item: &ast::Expression<'ast>) -> CompileResult {
        use self::ast::Expression as E;
        match item {
            E::Literal(literal)         => self.compile_literal(literal),
            E::Variable(variable)       => self.compile_variable(variable),
            E::Member(_)                => Ok(()),
            E::Call(call)               => self.compile_call(call),
            E::Assignment(assignment)   => self.compile_assignment(assignment),
            E::BinaryOp(binary_op)      => self.compile_binary_op(binary_op),
            E::UnaryOp(unary_op)        => self.compile_unary_op(unary_op),
            E::Cast(cast)               => self.compile_cast(cast),
            E::Block(block)             => self.compile_block(block),
            E::IfBlock(if_block)        => self.compile_if_block(if_block),
        }
    }

    /// Compiles the assignment operation.
    fn compile_assignment(self: &Self, item: &ast::Assignment<'ast>) -> CompileResult {
        comment!(self, "{}", item);
        match item.left {
            ast::Expression::Variable(_) => self.compile_assignment_to_var(item),
            ast::Expression::BinaryOp(_) => self.compile_assignment_to_offset(item),
            _ => panic!("cannot assign to left expression"),
        }
    }

    /// Compiles an assignment to a variable.
    fn compile_assignment_to_var(self: &Self, item: &ast::Assignment<'ast>) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        comment!(self, "direct assignment");
        let binding_id = item.left.binding_id().unwrap();
        let index = self.locals.lookup(binding_id).index;
        let ty = self.binding_type(&item.left);
        let constructor = if ty.is_ref() { self.get_constructor(ty) } else { 0 };
        match item.op {
            BO::Assign => {
                self.compile_expression(&item.right)?;
                if ty.is_ref() {
                    self.write_cntinc(constructor); // inc new value before dec old value (incase it is the same reference)
                    self.write_load(index as StackOffset, ty);
                    self.write_cntdec(constructor);
                }
                self.write_store(index as StackOffset, ty);
            },
            _ => {
                self.write_load(index as StackOffset, ty); // stack: L
                self.compile_expression(&item.right)?; // stack: L R
                if ty.is_ref() {
                    self.writer.cntstore(); // cntinc temporary right operand
                    self.write_cntinc(constructor);
                }
                match item.op { // stack: Result
                    BO::AddAssign => self.write_add(ty),
                    BO::SubAssign => self.write_sub(ty),
                    BO::MulAssign => self.write_mul(ty),
                    BO::DivAssign => self.write_div(ty),
                    BO::RemAssign => self.write_rem(ty),
                    op @ _ => unreachable!("Invalid assignment operator {}", op),
                };
                if ty.is_ref() {
                    self.write_cntinc(constructor); // inc new value before dec old value (incase it is the same reference)
                    self.write_load(index as StackOffset, ty);
                    self.write_cntdec(constructor);
                    self.writer.cntpop(); // cntdec temporary right operand
                    self.write_cntdec(constructor);
                }
                self.write_store(index as StackOffset, ty); // stack -
            },
        };
        Ok(())
    }

    /// Compiles an assignment to an index- or access-write operation.
    fn compile_assignment_to_offset(self: &Self, item: &ast::Assignment<'ast>) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        comment!(self, "offset assignment");
        let ty = self.binding_type(&item.left);
        let constructor = if ty.is_ref() { self.get_constructor(ty) } else { 0 };
        match item.op {
            BO::Assign => {
                self.compile_expression(&item.right)?;  // stack: right
                if ty.is_ref() {
                    self.write_cntinc(constructor);
                }
                self.compile_expression(&item.left)?;   // stack: right &left
                if ty.is_ref() {
                    self.write_clone(ty, 0);            // stack: right &left &left
                    self.write_heap_fetch(ty);          // stack: right &left old
                    self.write_cntdec(constructor);    // stack: right &left
                }
                self.write_heap_put(ty);                // stack: -
            },
            _ => {
                // TODO: optimize this case
                self.compile_expression(&item.right)?;  // stack: right
                if ty.is_ref() {
                    self.write_cntinc(constructor);
                }
                self.compile_expression(&item.left)?;   // stack: right &left
                if ty.is_ref() {
                    self.write_clone(ty, 0);            // stack: right &left &left
                    self.write_heap_fetch(ty);          // stack: right &left left
                    self.write_cntdec(constructor);    // stack: right &left
                }
                self.writer.cntstore();              // stack: right &left, tmp: &left
                self.write_heap_fetch(ty);              // stack: right left, tmp: &left
                self.write_swap(ty);                    // stack: left right, tmp: &left
                match item.op {                         // stack: result, tmp: &left
                    BO::AddAssign => self.write_add(ty),
                    BO::SubAssign => self.write_sub(ty),
                    BO::MulAssign => self.write_mul(ty),
                    BO::DivAssign => self.write_div(ty),
                    BO::RemAssign => self.write_rem(ty),
                    _ => unreachable!("Unsupported assignment operator encountered"),
                };
                self.writer.cntpop();                // stack: result, &left
                self.write_heap_put(ty);                // stack -
            },
        }
        Ok(())
    }

    /// Compiles the given call.
    fn compile_call(self: &Self, item: &ast::Call<'ast>) -> CompileResult {
        comment!(self, "{}()", item.ident.name);

        // put args on stack, ensure temporaries are cleaned up later
        for (_index, arg) in item.args.iter().enumerate() {
            comment!(self, "{}() arg {}", item.ident.name, _index);
            self.compile_expression(arg)?;
            self.maybe_ref_temporary(HeapRefOp::Inc, arg);
        }

        if let FnKind::Rust(rust_fn_index) = item.call_kind {

            // rust function
            self.writer.rustcall(T::from_u16(rust_fn_index));

        } else if let FnKind::Intrinsic(_intrinsic) = &item.call_kind {

            // intrinsics // TODO: actually check which one once there are some
            if let CallType::Method(exp) = &item.call_type {
                let ty = self.binding_type(&**exp);
                self.write_intrinsic_len(ty);
            }

        } else {

            // normal function: identify call target or write dummy
            let function_id = item.function_id.expect(&format!("Unresolved function \"{}\" encountered", item.ident.name));
            let call_position = self.writer.position();

            let target = if let Some(&target) = self.functions.borrow().get(&function_id) {
                target
            } else {
                self.unfixed_function_calls.borrow_mut().entry(function_id).or_insert(Vec::new()).push(call_position);
                CallInfo::PLACEHOLDER
            };

            self.writer.call(target.addr, target.arg_size);
        }

        for arg in item.args.iter().rev() {
            self.maybe_ref_temporary(HeapRefOp::Dec, arg);
        }

        Ok(())
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_binding(self: &Self, item: &ast::Binding<'ast>) -> CompileResult {
        let binding_id = item.binding_id.expect("Unresolved binding encountered");
        if let Some(expr) = &item.expr {
            comment!(self, "let {} = ...", item.ident.name);
            self.compile_expression(expr)?;
            let index = self.locals.lookup(binding_id).index;
            let ty = self.binding_type(item);
            if ty.is_ref() {
                self.write_cntinc(self.get_constructor(ty));
            }
            self.write_store(index as StackOffset, ty);
        }
        self.locals.set_active(binding_id, true);
        Ok(())
    }

    /// Compiles the given variable.
    fn compile_variable(self: &Self, item: &ast::Variable<'ast>) -> CompileResult {
        comment!(self, "variable {}", item);
        let load_index = {
            let binding_id = item.binding_id.expect("Unresolved binding encountered");
            self.locals.lookup(binding_id).index
        };
        self.write_load(load_index as StackOffset, self.binding_type(item));
        Ok(())
    }

    /// Compiles an if block without else part.
    fn compile_if_only_block(self: &Self, item: &ast::IfBlock<'ast>) -> CompileResult {
        comment!(self, "{}", item);
        let exit_jump = self.writer.j0(123);
        let result = self.compile_block(&item.if_block);
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
        result
    }

    /// Compiles an if+else block.
    fn compile_if_else_block(self: &Self, if_block: &ast::Block<'ast>, else_block: &ast::Block<'ast>) -> CompileResult {

        let else_jump = self.writer.j0(123);
        let result = self.compile_block(if_block);
        let exit_jump = if !if_block.returns() {
            Some(self.writer.jmp(123))
        } else {
            None
        };

        let else_target = self.writer.position();
        self.compile_block(else_block)?;

        let exit_target = self.writer.position();

        // go back and fix jump targets
        self.writer.overwrite(else_jump, |w| w.j0(else_target));
        if let Some(exit_jump) = exit_jump {
            self.writer.overwrite(exit_jump, |w| w.jmp(exit_target));
        }

        result
    }

    /// Compiles the given if block.
    fn compile_if_block(self: &Self, item: &ast::IfBlock<'ast>) -> CompileResult {

        // compile condition and jump placeholder
        self.compile_expression(&item.cond)?;

        if item.else_block.is_none() {
            self.compile_if_only_block(item)
        } else {
            self.compile_if_else_block(&item.if_block, item.else_block.as_ref().unwrap())
        }
    }

    /// Compiles a while loop.
    fn compile_while_loop(self: &Self, item: &ast::WhileLoop<'ast>) -> CompileResult {
        comment!(self, "{}", item);
        let start_target = self.writer.position();
        self.compile_expression(&item.expr)?;
        let exit_jump = self.writer.j0(123);
        self.compile_block(&item.block)?;
        self.writer.jmp(start_target);
        let exit_target = self.writer.position();
        self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
        Ok(())
    }

    /// Compiles a for - in loop
    fn compile_for_loop(self: &Self, item: &ast::ForLoop<'ast>) -> CompileResult {
        if let Some(binary_op) = item.range.as_binary_op() {

            if binary_op.op == ast::BinaryOperator::Range || binary_op.op == ast::BinaryOperator::RangeInclusive {
                comment!(self, "for-loop");
                // todo: refactor this mess
                let (var_index, var_type) = self.range_info(item);
                // store lower range bound in iter variable
                self.compile_expression(&binary_op.left)?;
                self.write_store(var_index as StackOffset, var_type);
                // push upper range bound
                self.compile_expression(&binary_op.right)?;
                // precheck (could be avoided by moving condition to the end but not trivial due to stack top clone order) // TODO: tmp stack now?
                self.write_load(var_index as StackOffset, var_type);
                self.write_clone(var_type, var_type.primitive_size()); // clone upper bound for comparison, skip over iter inbetween
                if binary_op.op == ast::BinaryOperator::Range {
                    self.write_lt(var_type);
                } else {
                    self.write_lte(var_type);
                }
                let exit_jump = self.writer.j0(123);
                // compile block
                let start_target = self.writer.position();
                self.compile_block(&item.block)?;
                // load bounds, increment and compare
                self.write_preinc(var_index as StackOffset, var_type);
                self.write_clone(var_type, var_type.primitive_size()); // clone upper bound for comparison, skip over iter inbetween
                if binary_op.op == ast::BinaryOperator::Range {
                    self.write_lt(var_type);
                } else {
                    self.write_lte(var_type);
                }
                self.writer.jn0(start_target);
                // exit position
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.j0(exit_target));
                Ok(())
            } else {
                Err(CompileError::new(item, CompileErrorKind::Error))
            }

        } else {
            Err(CompileError::new(item, CompileErrorKind::Error))
        }
    }

    /// Compiles the given function.
    fn compile_function(self: &Self, item: &ast::Function<'ast>) -> CompileResult {
        // register function bytecode index, check if any bytecode needs fixing
        let position = self.writer.position();
        comment!(self, "\nfn {}", item.sig.ident.name);
        // create local environment
        let mut frame = Locals::new();
        frame.ret_size = item.sig.ret.as_ref().map_or(0, |ret| self.binding_type(ret).primitive_size());
        for arg in item.sig.args.iter() {
            frame.map.insert(arg.binding_id.unwrap(), Local::new(frame.arg_pos));
            frame.arg_pos += self.binding_type(arg).primitive_size() as StackAddress;
        }
        self.create_stack_frame_block(&item.block, &mut frame);
        // store call info required to compile calls to this function
        let function_id = item.function_id.unwrap();
        let call_info = CallInfo { addr: position, arg_size: frame.arg_pos };
        self.functions.borrow_mut().insert(function_id, call_info);
        self.fix_targets(function_id, call_info);
        // reserve space for local variables on the stack
        if frame.var_pos > 0 {
            self.writer.reserve(frame.var_pos as u8);
        }
        // push local environment on the locals stack so that it is accessible from nested compile_*
        self.locals.push(frame);
        self.compile_block(&item.block)?;
        // fix forward-to-exit jmps within the function
        let exit_address = self.writer.position();
        self.locals.borrow_mut(|frame| {
            while let Some(jmp_address) = frame.unfixed_exit_jmps.pop() {
                self.writer.overwrite(jmp_address, |w| w.jmp(exit_address));
            }
        });
        // handle exit
        comment!(self, "fn exiting");
        let constructor = item.sig.ret.as_ref().map_or(None, |ret| {
            let ty = self.binding_type(ret);
            if ty.is_ref() { Some(self.get_constructor(ty)) } else { None }
        });
        if let Some(constructor) = constructor {
            self.write_cntinc(constructor);
        }
        self.write_cntdec_all();
        if let Some(constructor) = constructor {
            self.write_cntzero(constructor);
        }
        self.write_ret();
        self.locals.pop();
        Ok(())
    }

    /// Compiles the given block.
    fn compile_block(self: &Self, item: &ast::Block<'ast>) -> CompileResult {
        for statement in item.statements.iter() {
            self.compile_statement(statement)?;
        }
        if let Some(returns) = &item.returns {
            comment!(self, "block returning");
            self.compile_expression(returns)?;
            let exit_jump = self.writer.jmp(123);
            self.locals.add_forward_jmp(exit_jump);
        } else if let Some(result) = &item.result {
            comment!(self, "block resulting");
            self.compile_expression(result)?;
        }
        Ok(())
    }

    /// Compiles the given literal
    fn compile_literal(self: &Self, item: &ast::Literal<'ast>) -> CompileResult {
        use crate::frontend::ast::LiteralValue;
        comment!(self, "{}", item);
        let ty = self.binding_type(item);
        match item.value {
            LiteralValue::Numeric(numeric) => self.write_numeric(numeric, ty),
            LiteralValue::Bool(v) =>  {
                match ty {
                    Type::bool => { if v { self.writer.one8(); } else { self.writer.zero8(); } },
                    _ => panic!("Unexpected boolean literal type: {:?}", ty)
                };
            },
            LiteralValue::Array(_) | LiteralValue::Struct(_) | LiteralValue::String(_) => {
                let constructor = self.get_constructor(ty);
                if !self.writing_protype_instructions.get() {
                    if item.value.is_const() {
                        // simple constant constructor, construct from const pool prototypes
                        let prototype = self.store_literal_prototype(item);
                        self.writer.construct(constructor, prototype);
                    } else {
                        // non-constant constructor, construct from stack, set flag to avoid writing additional constructor instructions during recursion
                        self.writing_protype_instructions.set(true);
                        self.compile_prototype_instructions(item)?;
                        self.writer.construct_dyn(constructor, self.bindings.type_size(ty)); //FIXME need to take size of references to strings into account
                        self.writing_protype_instructions.set(false);
                    }
                } else {
                    // continuing non-constant constructor
                    self.compile_prototype_instructions(item)?;
                }
            },
        }
        Ok(())
    }

    /// Writes instructions to assemble a prototype.
    fn compile_prototype_instructions(self: &Self, item: &ast::Literal<'ast>) -> CompileResult {
        use crate::frontend::ast::LiteralValue;
        let ty = self.binding_type(item);
        match &item.value {
            LiteralValue::Array(array_literal) => {
                for element in &array_literal.elements {
                    self.compile_expression(element)?;
                }
            }
            LiteralValue::Struct(struct_literal) => {
                let struct_def = ty.as_struct().expect("Expected struct, got something else");
                for (name, _) in struct_def.fields.iter() {
                    let field = struct_literal.fields.get(&name[..]).expect(&format!("Missing struct field {}", &name));
                    self.compile_expression(field)?;
                }
            }
            LiteralValue::String(_) => {
                // we cannot generate the string onto the stack since string operations (e.g. x = MyStruct { a: "hello" + "world" }) require references.
                // this would also cause a lot of copying from heap to stack and back. instead we treat strings normally and
                // later use a special constructor instruction that assumes strings are already on the heap.
                let constructor = self.get_constructor(ty);
                let prototype = self.store_literal_prototype(item);
                self.writer.construct(constructor, prototype);
            }
            _ => unreachable!("Invalid prototype type")
        };
        Ok(())
    }

    /// Compiles the given unary operation.
    fn compile_unary_op(self: &Self, item: &ast::UnaryOp<'ast>) -> CompileResult {
        use crate::frontend::ast::{UnaryOperator as UO, BinaryOperator};

        let exp_type = self.binding_type(&item.expr);

        match item.op {
            // logical
            UO::Not => {
                self.compile_expression(&item.expr)?;
                comment!(self, "{}", item);
                self.writer.not();
            }
            // arithmetic
            UO::IncBefore | UO::DecBefore | UO::IncAfter | UO::DecAfter => {
                if let ast::Expression::Variable(var) = &item.expr {
                    comment!(self, "{}", item);
                    let load_index = {
                        let binding_id = var.binding_id.expect("Unresolved binding encountered");
                        self.locals.lookup(binding_id).index
                    };
                    match item.op {
                        UO::IncBefore => self.write_preinc(load_index as StackOffset, &exp_type),
                        UO::DecBefore => self.write_predec(load_index as StackOffset, &exp_type),
                        UO::IncAfter => self.write_postinc(load_index as StackOffset, &exp_type),
                        UO::DecAfter => self.write_postdec(load_index as StackOffset, &exp_type),
                        _ => panic!("Internal error in operator handling"),
                    };
                } else if let ast::Expression::BinaryOp(binary_op) = &item.expr {
                    assert!(binary_op.op == BinaryOperator::IndexWrite || binary_op.op == BinaryOperator::AccessWrite, "Expected IndexWrite or AccessWrite operation");
                    self.compile_expression(&item.expr)?;
                    self.writer.cntstore();
                    self.write_heap_fetch(exp_type);
                    comment!(self, "{}", item);
                    if item.op == UO::IncAfter || item.op == UO::DecAfter {
                        self.write_clone(exp_type, 0);
                    }
                    match item.op {
                        UO::IncBefore | UO::IncAfter => self.write_inc(&exp_type),
                        UO::DecBefore | UO::DecAfter => self.write_dec(&exp_type),
                        _ => panic!("Internal error in operator handling"),
                    };
                    if item.op == UO::IncBefore || item.op == UO::DecBefore {
                        self.write_clone(exp_type, 0);
                    }
                    self.writer.cntpop();
                    self.write_heap_put(exp_type);
                } else {
                    panic!("Operator {:?} can not be used here", item.op);
                }
            },
        }
        Ok(())
    }

    /// Compiles a simple, non-shortcutting binary operation.
    fn compile_binary_op_simple(self: &Self, item: &ast::BinaryOp<'ast>) -> CompileResult {

        use crate::frontend::ast::BinaryOperator as BO;
        let ty_result = self.binding_type(item);
        let ty_left = self.binding_type(&item.left);

        // compile left, right and store references, left and right will be consumed
        self.compile_expression(&item.left)?;
        self.maybe_ref_temporary(HeapRefOp::Inc, &item.left);
        comment!(self, "{}", item.op);
        self.compile_expression(&item.right)?;
        self.maybe_ref_temporary(HeapRefOp::Inc, &item.right);

        match item.op {
            // arithmetic/concat
            BO::Add => self.write_add(ty_result),
            BO::Sub => self.write_sub(ty_result),
            BO::Mul => self.write_mul(ty_result),
            BO::Div => self.write_div(ty_result),
            BO::Rem => self.write_rem(ty_result),
            // comparison
            BO::Greater     => { self.write_swap(ty_left); self.write_lt(ty_left); },
            BO::GreaterOrEq => { self.write_swap(ty_left); self.write_lte(ty_left); },
            BO::Less        => self.write_lt(ty_left),
            BO::LessOrEq    => self.write_lte(ty_left),
            BO::Equal       => self.write_eq(ty_left),
            BO::NotEqual    => self.write_neq(ty_left),
            _ => unreachable!("Invalid simple-operation {:?} in compile_binary_op", item.op),
        }

        self.maybe_ref_temporary(HeapRefOp::Dec, &item.right);
        self.maybe_ref_temporary(HeapRefOp::Dec, &item.left);
        Ok(())
    }

    /// Compiles a short-circuiting binary operation (and/or)
    fn compile_binary_op_shortcircuiting(self: &Self, item: &ast::BinaryOp<'ast>) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        match item.op {
            BO::And => {
                self.compile_expression(&item.left)?;
                let exit_jump = self.writer.j0_top(123); // left is false, result cannot ever be true, skip right
                self.compile_expression(&item.right)?;
                self.writer.and();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.j0_top(exit_target));
            },
            BO::Or => {
                self.compile_expression(&item.left)?;
                let exit_jump = self.writer.jn0_top(123); // left is true, result cannot ever be false, skip right
                self.compile_expression(&item.right)?;
                self.writer.or();
                let exit_target = self.writer.position();
                self.writer.overwrite(exit_jump, |w| w.jn0_top(exit_target));
            },
            _ => unreachable!("Invalid shortcircuit-operation {:?} in compile_binary_op", item.op),
        }
        Ok(())
    }

    /// Compiles an offsetting binary operation, i.e.. indexing and member access.
    fn compile_binary_op_offseting(self: &Self, item: &ast::BinaryOp<'ast>) -> CompileResult {
        use crate::frontend::ast::BinaryOperator as BO;
        let result_type = self.binding_type(item);
        let compare_type = self.binding_type(&item.left);
        self.compile_expression(&item.left)?;
        self.compile_expression(&item.right)?;
        match item.op {
            BO::Index => {
                comment!(self, "[{}]", &item.right);
                // fetch heap value at reference-target
                self.write_heap_fetch_element(result_type);
            },
            BO::IndexWrite => {
                comment!(self, "[{}] (writing)", &item.right);
                // compute and push the address of the reference-target
                self.writer.index(result_type.primitive_size());
            },
            BO::Access => {
                comment!(self, ".{}", &item.right);
                // fetch heap value at reference-target
                let struct_ = compare_type.as_struct().unwrap();
                let offset = self.compute_member_offset(struct_, item.right.as_member().unwrap().index.unwrap());
                self.write_heap_fetch_member(result_type, offset);
            },
            BO::AccessWrite => {
                comment!(self, ".{} (writing)", &item.right);
                // compute and push the address of the reference-target
                let struct_ = compare_type.as_struct().unwrap();
                let offset = self.compute_member_offset(struct_, item.right.as_member().unwrap().index.unwrap());
                self.write_member_offset(offset);

            },
            _ => unreachable!("Invalid offset-operation {:?} in compile_binary_op", item.op),
        }
        Ok(())
    }

    /// Compiles the given binary operation.
    fn compile_binary_op(self: &Self, item: &ast::BinaryOp<'ast>) -> CompileResult {
        if item.op.is_simple() {
            self.compile_binary_op_simple(item)
        } else if item.op.is_shortcircuit() {
            self.compile_binary_op_shortcircuiting(item)
        } else if item.op.is_offset() {
            self.compile_binary_op_offseting(item)
        } else {
            unreachable!()
        }
    }

    /// Compiles a variable binding and optional assignment.
    fn compile_cast(self: &Self, item: &ast::Cast<'ast>) -> CompileResult {

        self.compile_expression(&item.expr)?;

        let from = self.binding_type(&item.expr);
        let to = self.binding_type(&item.ty);

        if from.is_signed() && !to.is_signed() && !to.is_float() && !to.is_string() {
            self.write_zclamp(from);
        }

        if from.is_integer() && to.is_integer() {
            self.write_integer_cast(from, to);
        } else if from.is_float() && to.is_float() {
            self.write_float_integer_cast(from, to);
        } else if from.is_float() && to.is_integer() {
            let temp_to = if to.is_signed() { &Type::i64 } else { &Type::u64 };
            self.write_float_integer_cast(from, temp_to);
            if to.primitive_size() != 8 {
                self.write_integer_cast(temp_to, to);
            }
        } else if from.is_integer() && to.is_float() {
            let temp_from = if from.is_signed() { &Type::i64 } else { &Type::u64 };
            if from.primitive_size() != 8 {
                self.write_integer_cast(from, temp_from);
            }
            self.write_float_integer_cast(temp_from, to);
        } else if from.is_integer() && to.is_string() {
            let temp_from = if from.is_signed() { &Type::i64 } else { &Type::u64 };
            if from.primitive_size() != 8 {
                self.write_integer_cast(from, temp_from);
            }
            match temp_from {
                Type::i64 => self.writer.i64_to_string(),
                Type::u64 => self.writer.u64_to_string(), // TODO: refcounting, this creates a heap ref
                _ => unreachable!(),
            };
        } else if from == &Type::f32 && to.is_string() {
            self.writer.f32_to_string();
        } else if from == &Type::f64 && to.is_string() {
            self.writer.f64_to_string();
        } else {
            unreachable!("Invalid cast {:?} to {:?}", from, to);
        }

        Ok(())
    }
}

impl<'ast, 'ty, T> Compiler<'ty, T> where T: VMFunc<T> {

    /// Returns the type of the given binding.
    fn binding_type(self: &Self, item: &impl Bindable) -> &Type {
        self.bindings.binding_type(item.binding_id().expect("Unresolved binding encountered."))
    }

    /// Returns type for given type_id.
    fn get_type(self: &Self, type_id: Option<TypeId>) -> &Type {
        self.bindings.type_by_id(type_id.expect("Unresolved binding encountered."))
    }

    /// Returns constructor index for given type.
    fn get_constructor(self: &Self, ty: &Type) -> StackAddress {
        *self.constructors.get(ty).expect(&format!("Undefined constructor for type {:?}", ty))
    }

    /// Fixes function call targets for previously not generated functions.
    fn fix_targets(self: &Self, function_id: FunctionId, info: CallInfo) {
        if let Some(targets) = self.unfixed_function_calls.borrow_mut().remove(&function_id) {
            let backup_position = self.writer.position();
            for &target in targets.iter() {
                self.writer.set_position(target);
                self.writer.call(info.addr, info.arg_size);
            }
            self.writer.set_position(backup_position);
        }
    }

    /// Retrieve for-in loop range variable index/type
    fn range_info(self: &Self, item: &ast::ForLoop<'ast>) -> (StackAddress, &Type) {
        let var_index = {
            let binding_id = item.iter.binding_id.expect("Unresolved binding encountered");
            self.locals.lookup(binding_id).index
        };
        let var_type = self.binding_type(&item.iter);
        (var_index, var_type)
    }

    /// Creates stack frame variables for expressions.
    fn create_stack_frame_exp(self: &Self, expression: &ast::Expression<'ast>, frame: &mut Locals) {
        if let ast::Expression::Block(block) = expression {
            self.create_stack_frame_block(block, frame);
        } else if let ast::Expression::Call(call) = expression {
            for arg in &call.args {
                if let ast::Expression::Block(block) = arg {
                    self.create_stack_frame_block(block, frame);
                }
            }
        } else if let ast::Expression::Assignment(assignment) = expression {
            if let ast::Expression::Block(block) = &assignment.right {
                self.create_stack_frame_block(block, frame);
            }
        } else if let ast::Expression::BinaryOp(binary_op) = expression {
            if let ast::Expression::Block(block) = &binary_op.left {
                self.create_stack_frame_block(block, frame);
            }
            if let ast::Expression::Block(block) = &binary_op.right {
                self.create_stack_frame_block(block, frame);
            }
        } else if let ast::Expression::UnaryOp(unary_op) = expression {
            if let ast::Expression::Block(block) = &unary_op.expr {
                self.create_stack_frame_block(block, frame);
            }
        } else if let ast::Expression::IfBlock(if_block) = expression {
            self.create_stack_frame_block(&if_block.if_block, frame);
            if let Some(block) = &if_block.else_block {
                self.create_stack_frame_block(block, frame);
            }
        }
    }

    /// Creates stack frame variables for blocks.
    fn create_stack_frame_block(self: &Self, item: &ast::Block<'ast>, frame: &mut Locals) {
        // todo: this is pretty bad. need to come up with better solution. trait on ast?
        for statement in item.statements.iter() {
            if let ast::Statement::Binding(binding) = statement {
                frame.map.insert(binding.binding_id.unwrap(), Local::new(frame.var_pos));
                frame.var_pos += self.binding_type(binding).primitive_size() as StackAddress;
                if let Some(expression) = &binding.expr {
                    self.create_stack_frame_exp(expression, frame);
                }
            } else if let ast::Statement::ForLoop(for_loop) = statement {
                frame.map.insert(for_loop.iter.binding_id.unwrap(), Local::new(frame.var_pos));
                frame.var_pos += self.binding_type(&for_loop.iter).primitive_size() as StackAddress;
                self.create_stack_frame_block(&for_loop.block, frame);
            } else if let ast::Statement::WhileLoop(while_loop) = statement {
                self.create_stack_frame_block(&while_loop.block, frame);
            } else if let ast::Statement::Block(block) = statement {
                self.create_stack_frame_block(block, frame);
            } else if let ast::Statement::IfBlock(if_block) = statement {
                self.create_stack_frame_block(&if_block.if_block, frame);
                if let Some(block) = &if_block.else_block {
                    self.create_stack_frame_block(block, frame);
                }
            } else if let ast::Statement::Expression(expression) = statement {
                self.create_stack_frame_exp(expression, frame);
            }
        }
        if let Some(result) = &item.result {
            self.create_stack_frame_exp(result, frame);
        }
    }

    /// Writes a constructor for non-primitive types.
    fn store_constructor(self: &Self, ty: &Type) -> StackAddress {
        let position = self.writer.const_len();
        //let prototype_size = 0; // TODO track size here?
        match ty {
            Type::Array(array) => {
                self.writer.store_const(Constructor::Array as u8);
                self.writer.store_const(array.len.unwrap() as ItemCount);
                self.store_constructor(self.get_type(array.type_id));
            }
            Type::Struct(structure) => {
                self.writer.store_const(Constructor::Struct as u8);
                self.writer.store_const(structure.fields.len() as ItemCount);
                for field in &structure.fields {
                    self.store_constructor(self.get_type(field.1));
                }
            }
            Type::String => {
                self.writer.store_const(Constructor::String as u8);
            }
            Type::Enum(_) => unimplemented!("enum constructor"),
            _ => {
                self.writer.store_const(Constructor::Primitive as u8);
                self.writer.store_const(ty.primitive_size() as ItemCount);
            }
        }
        position
    }

    /// Stores given literal on the const pool.
    fn store_literal_prototype(self: &Self, item: &ast::Literal<'ast>) -> StackAddress {
        use crate::frontend::ast::LiteralValue;
        let ty = self.binding_type(item);
        let pos = self.writer.const_len();
        match &item.value {
            &LiteralValue::Numeric(int) => {
                self.store_numeric_prototype(int, ty);
            },
            &LiteralValue::Bool(boolean) =>  {
                match ty {
                    Type::bool => self.writer.store_const(if boolean { 1u8 } else { 0u8 }),
                    _ => panic!("Unexpected boolean literal type: {:?}", ty)
                };
            },
            &LiteralValue::String(string_literal) => {
                self.writer.store_const(string_literal);
            },
            LiteralValue::Array(array_literal) => {
                for element in &array_literal.elements {
                    self.store_literal_prototype(element.as_literal().unwrap());
                }
            }
            LiteralValue::Struct(struct_literal) => {
                let struct_def = ty.as_struct().expect("Expected struct, got something else");
                for (name, _) in struct_def.fields.iter() {
                    let field = struct_literal.fields.get(&name[..]).expect(&format!("Missing struct field {}", &name));
                    self.store_literal_prototype(field.as_literal().unwrap());
                }
            }
        };
        pos
    }

    /// Stores given numeric on the const pool.
    fn store_numeric_prototype(self: &Self, numeric: Numeric, ty: &Type) -> StackAddress {
        match numeric {
            Numeric::Signed(v) => {
                match ty {
                    Type::i8 => self.writer.store_const(v as i8),
                    Type::i16 => self.writer.store_const(v as i16),
                    Type::i32 => self.writer.store_const(v as i32),
                    Type::i64 => self.writer.store_const(v as i64),
                    _ => panic!("Unexpected signed integer literal type: {:?}", ty)
                }
            },
            Numeric::Unsigned(v) => {
                match ty {
                    Type::i8 | Type::u8 => self.writer.store_const(v as u8),
                    Type::i16 | Type::u16 => self.writer.store_const(v as u16),
                    Type::i32 | Type::u32 => self.writer.store_const(v as u32),
                    Type::i64 | Type::u64 => self.writer.store_const(v as u64),
                    _ => panic!("Unexpected unsigned integer literal type: {:?}", ty)
                }
            },
            Numeric::Float(v) => {
                match ty {
                    Type::f32 => self.writer.store_const(v as f32),
                    Type::f64 => self.writer.store_const(v as f64),
                    _ => panic!("Unexpected float literal type: {:?}", ty)
                }
            },
            Numeric::Overflow => panic!("Literal computation overflow")
        }
    }

    /// Computes struct member offset in bytes.
    fn compute_member_offset(self: &Self, struct_: &Struct, member_index: ItemCount) -> StackAddress {
        let mut offset = 0;
        for index in 0 .. member_index {
            let field_type = self.get_type(struct_.fields[index as usize].1);
            // use reference size for references, otherwise shallow type size
            offset += field_type.primitive_size() as StackAddress;
        }
        offset
    }

    /// Writes a cast from one float to another or from/to a 64 bit integer.
    fn write_float_integer_cast(self: &Self, from: &Type, to: &Type) {
        match (from, to) {
            (Type::i64, Type::f32) => self.writer.i64_to_f32(),
            (Type::u64, Type::f32) => self.writer.u64_to_f32(),
            (Type::f64, Type::f32) => self.writer.f64_to_f32(),

            (Type::i64, Type::f64) => self.writer.i64_to_f64(),
            (Type::u64, Type::f64) => self.writer.u64_to_f64(),
            (Type::f32, Type::f64) => self.writer.f32_to_f64(),

            (Type::f32, Type::i64) => self.writer.f32_to_i64(),
            (Type::f64, Type::i64) => self.writer.f64_to_i64(),

            (Type::f32, Type::u64) => self.writer.f32_to_u64(),
            (Type::f64, Type::u64) => self.writer.f64_to_u64(),
            _ => unreachable!("Invalid float/int cast {:?} to {:?}", from, to),
        };
    }

    /// Writes a cast from one integer to another.
    fn write_integer_cast(self: &Self, from: &Type, to: &Type) {
        let from_size = (from.primitive_size() * 8) as u8;
        let to_size = (to.primitive_size() * 8) as u8;
        if to_size < from_size || (to_size == from_size && !from.is_signed() && to.is_signed()) {
            if to.is_signed() {
                match from_size {
                    64 => self.writer.trims64(to_size),
                    32 => self.writer.trims32(to_size),
                    16 => self.writer.trims16(to_size),
                    _ => unreachable!("Invalid integer cast {:?} to {:?}", from, to),
                };
            } else {
                match from_size {
                    64 => self.writer.trimu64(to_size),
                    32 => self.writer.trimu32(to_size),
                    16 => self.writer.trimu16(to_size),
                    _ => unreachable!("Invalid integer cast {:?} to {:?}", from, to),
                };
            }
        } else if to_size > from_size {
            if from.is_signed() {
                match from_size {
                    32 => self.writer.extends32(to_size),
                    16 => self.writer.extends16(to_size),
                    8 => self.writer.extends8(to_size),
                    _ => unreachable!("Invalid integer cast {:?} to {:?}", from, to),
                };
            } else {
                match from_size {
                    32 => self.writer.extendu32(to_size),
                    16 => self.writer.extendu16(to_size),
                    8 => self.writer.extendu8(to_size),
                    _ => unreachable!("Invalid integer cast {:?} to {:?}", from, to),
                };
            }
        }
    }

    /// Writes an appropriate variant of the cntinc instruction.
    fn write_cntinc(self: &Self, constructor: StackAddress) {
        opcode_unsigned!(self, cntinc_8, cntinc_16, cntinc_32, constructor);
    }

    /// Writes an appropriate variant of the cntdec instruction.
    fn write_cntdec(self: &Self, constructor: StackAddress) {
        opcode_unsigned!(self, cntdec_8, cntdec_16, cntdec_32, constructor);
    }

    /// Writes an appropriate variant of the cntzero instruction.
    fn write_cntzero(self: &Self, constructor: StackAddress) {
        opcode_unsigned!(self, cntzero_8, cntzero_16, cntzero_32, constructor);
    }

    // TODO: try to remove tmp64 handling
    fn maybe_ref_temporary(self: &Self, op: HeapRefOp, item: &ast::Expression<'ast>) {
        let ty = self.binding_type(item);
        if ty.is_ref() {
            let constructor = self.get_constructor(ty);
            match op {
                HeapRefOp::Inc => {
                    self.writer.cntstore();
                    self.write_cntinc(constructor);
                }
                HeapRefOp::Dec => {
                    self.writer.cntpop();
                    self.write_cntdec(constructor);
                }
                HeapRefOp::Zero => unreachable!("Invalid heap ref op")
            }

        }
    }

    /// Writes operations to unref local heap objects.
    fn write_cntdec_all(self: &Self) {
        self.locals.borrow_mut(|locals| {
            for (&binding_id, local) in locals.map.iter().filter(|(_, local)| local.in_scope) {
                let ty = self.bindings.binding_type(binding_id);
                if ty.is_ref() {
                    let constructor = self.get_constructor(ty);
                    self.write_load(local.index as StackOffset, ty);
                    self.write_cntdec(constructor);
                }
            }
        });
    }

    /// Writes instructions to compute member offset for access on a struct.
    fn write_member_offset(self: &Self, offset: StackAddress) {
        if offset == 0 {
            // nothing to do
        } else if offset == 1 {
            self.writer.inci32();
        } else {
            assert!(::std::mem::size_of::<StackAddress>() == ::std::mem::size_of::<u32>());
            let const_id = self.writer.store_const(offset);
            self.write_const(const_id, &STACK_ADDRESS_TYPE);
            self.write_add(&STACK_ADDRESS_TYPE)
        }
    }

    /// Writes an appropriate variant of the const instruction.
    fn write_const(self: &Self, index: StackAddress, ty: &Type) {
        match ty.primitive_size() {
            1 => opcode_unsigned!(self, const8_8, const8_16, const8_32, index),
            2 => opcode_unsigned!(self, const16_8, const16_16, const16_32, index),
            4 => opcode_unsigned!(self, const32_8, const32_16, const32_32, index),
            8 => opcode_unsigned!(self, const64_8, const64_16, const64_32, index),
            //16 => opcode_unsigned!(self, const128_8, const128_16, const128_32, index),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Writes instructions to fetch a value of the given size from the target of the top heap reference on the stack.
    fn write_heap_fetch(self: &Self, ty: &Type) {
        match ty.primitive_size() {
            1 => { self.writer.heap_fetch8(); },
            2 => { self.writer.heap_fetch16(); },
            4 => { self.writer.heap_fetch32(); },
            8 => { self.writer.heap_fetch64(); },
            //16 => { self.writer.heap_fetch128(); },
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        }
    }

    /// Writes instructions to put a value of the given size at the target of the top heap reference on the stack.
    fn write_heap_put(self: &Self, ty: &Type) {
        match ty.primitive_size() {
            1 => { self.writer.heap_put8(); },
            2 => { self.writer.heap_put16(); },
            4 => { self.writer.heap_put32(); },
            8 => { self.writer.heap_put64(); },
            //16 => { self.writer.heap_put128(); },
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        }
    }

    /// Writes instructions to fetch a member of the struct whose reference is at the top of the stack.
    fn write_heap_fetch_member(self: &Self, ty: &Type, offset: StackAddress) {
        match ty.primitive_size() {
            1 => { self.writer.heap_fetch_member8(offset); },
            2 => { self.writer.heap_fetch_member16(offset); },
            4 => { self.writer.heap_fetch_member32(offset); },
            8 => { self.writer.heap_fetch_member64(offset); },
            //16 => { self.writer.heap_fetch_member128(offset); },
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        }
    }

    /// Writes instructions to fetch an element of the array whose reference is at the top of the stack.
    fn write_heap_fetch_element(self: &Self, ty: &Type) {
        match ty.primitive_size() {
            1 => { self.writer.heap_fetch_element8(); },
            2 => { self.writer.heap_fetch_element16(); },
            4 => { self.writer.heap_fetch_element32(); },
            8 => { self.writer.heap_fetch_element64(); },
            //16 => { self.writer.heap_fetch_element128(); },
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        }
    }

    /// Writes an appropriate variant of the store instruction.
    fn write_store(self: &Self, index: StackOffset, ty: &Type) {
        match ty.primitive_size() {
            1 => opcode_signed!(self, store8_8, store8_16, store8_32, index),
            2 => opcode_signed!(self, store16_8, store16_16, store16_32, index),
            4 => opcode_signed!(self, store32_8, store32_16, store32_32, index),
            8 => opcode_signed!(self, store64_8, store64_16, store64_32, index),
            //16 => opcode_signed!(self, store128_8, store128_16, store128_32, index),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Writes an appropriate variant of the load instruction.
    fn write_load(self: &Self, index: StackOffset, ty: &Type) {
        match ty.primitive_size() {
            1 => opcode_signed!(self, load8_8, load8_16, load8_32, index),
            2 => opcode_signed!(self, load16_8, load16_16, load16_32, index),
            4 => match index {
                ARG1 => self.writer.load_arg1(),
                ARG2 => self.writer.load_arg2(),
                ARG3 => self.writer.load_arg3(),
                _ => opcode_signed!(self, load32_8, load32_16, load32_32, index),
            },
            8 => opcode_signed!(self, load64_8, load64_16, load64_32, index),
            //16 => opcode_signed!(self, load128_8, load128_16, load128_32, index),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Discard topmost stack value
    fn write_discard(self: &Self, ty: &Type) {
        match ty.primitive_size() {
            0 => 0,
            1 => self.writer.discard8(),
            2 => self.writer.discard16(),
            4 => self.writer.discard32(),
            8 => self.writer.discard64(),
            //16 => self.writer.discard128(),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Swap topmost 2 stack values.
    fn write_swap(self: &Self, ty: &Type) {
        match ty.primitive_size() {
            1 => self.writer.swap8(),
            2 => self.writer.swap16(),
            4 => self.writer.swap32(),
            8 => self.writer.swap64(),
            //16 => self.writer.swap128(),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Clone stack value at (negative of) given offset to the top of the stack.
    fn write_clone(self: &Self, ty: &Type, offset: u8) {
        match ty.primitive_size() {
            1 => self.writer.clone8(offset),
            2 => self.writer.clone16(offset),
            4 => self.writer.clone32(offset),
            8 => self.writer.clone64(offset),
            //16 => self.writer.clone128(offset),
            size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
        };
    }

    /// Writes instructions for build-in len method.
    fn write_intrinsic_len(self: &Self, ty: &Type) {
        if let Type::Array(array) = ty {
            self.write_numeric(Numeric::Unsigned(array.len.unwrap() as u64), &STACK_ADDRESS_TYPE);
        } else {
            unimplemented!("dynamic array len");
        }
    }

    /// Writes given numeric, using const pool if necessary.
    fn write_numeric(self: &Self, numeric: Numeric, ty: &Type) {
        match numeric { // todo: try to refactor this mess

            Numeric::Unsigned(0) if ty.is_integer() && ty.primitive_size() == 1 => { self.writer.zero8(); }
            Numeric::Unsigned(0) if ty.is_integer() && ty.primitive_size() == 4 => { self.writer.zero32(); }
            Numeric::Unsigned(0) if ty.is_integer() && ty.primitive_size() == 8 => { self.writer.zero64(); }

            Numeric::Unsigned(1) if ty.is_integer() && ty.primitive_size() == 1 => { self.writer.one8(); }
            Numeric::Unsigned(1) if ty.is_integer() && ty.primitive_size() == 4 => { self.writer.one32(); }
            Numeric::Unsigned(1) if ty.is_integer() && ty.primitive_size() == 8 => { self.writer.one64(); }

            Numeric::Signed(-1) if ty.is_signed() && ty.primitive_size() == 1 => { self.writer.fill8(); }
            Numeric::Signed(-1) if ty.is_signed() && ty.primitive_size() == 4 => { self.writer.fill32(); }
            Numeric::Signed(-1) if ty.is_signed() && ty.primitive_size() == 8 => { self.writer.fill64(); }

            Numeric::Unsigned(val) if ty.is_integer() && ty.primitive_size() == 1 => { self.writer.literali8(val as u8); }
            Numeric::Unsigned(val) if ty.is_integer() && ty.primitive_size() == 4 && val <= u8::MAX as u64 => { self.writer.literalu32(val as u8); }

            Numeric::Signed(val) if ty.is_signed() && ty.primitive_size() == 1 => { self.writer.literali8((val as i8) as u8); }
            Numeric::Signed(val) if ty.is_signed() && ty.primitive_size() == 4 && val >= i8::MIN as i64 && val <= i8::MAX as i64 => { self.writer.literals32(val as i8); }

            _ if ty.is_integer() || ty.is_float() => {
                let address = self.store_numeric_prototype(numeric, ty);
                self.write_const(address, ty);
            },
            _ => panic!("Unexpected numeric literal type: {:?}", ty),
        }
    }

    /// Writes zclamp instruction.
    fn write_zclamp(self: &Self, ty: &Type) {
        match ty {
            Type::f32 => self.writer.zclampf32(),
            Type::f64 => self.writer.zclampf64(),
            Type::i8 => self.writer.zclampi8(),
            Type::i16 => self.writer.zclampi16(),
            Type::i32 => self.writer.zclampi32(),
            Type::i64 => self.writer.zclampi64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write increment instruction.
    fn write_inc(self: &Self, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.inci64(),
            Type::i32 | Type::u32 => self.writer.inci32(),
            Type::i16 | Type::u16 => self.writer.inci16(),
            Type::i8 | Type::u8 => self.writer.inci8(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write decrement instruction.
    fn write_dec(self: &Self, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.deci64(),
            Type::i32 | Type::u32 => self.writer.deci32(),
            Type::i16 | Type::u16 => self.writer.deci16(),
            Type::i8 | Type::u8 => self.writer.deci8(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write pre-increment instruction.
    fn write_preinc(self: &Self, index: StackOffset, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.preinci64(index),
            Type::i32 | Type::u32 => self.writer.preinci32(index),
            Type::i16 | Type::u16 => self.writer.preinci16(index),
            Type::i8 | Type::u8 => self.writer.preinci8(index),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write pre-decrement instruction.
    fn write_predec(self: &Self, index: StackOffset, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.predeci64(index),
            Type::i32 | Type::u32 => self.writer.predeci32(index),
            Type::i16 | Type::u16 => self.writer.predeci16(index),
            Type::i8 | Type::u8 => self.writer.predeci8(index),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write post-increment instruction.
    fn write_postinc(self: &Self, index: StackOffset, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.postinci64(index),
            Type::i32 | Type::u32 => self.writer.postinci32(index),
            Type::i16 | Type::u16 => self.writer.postinci16(index),
            Type::i8 | Type::u8 => self.writer.postinci8(index),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write post-decrement instruction.
    fn write_postdec(self: &Self, index: StackOffset, ty: &Type) {
        match ty {
            Type::i64 | Type::u64 => self.writer.postdeci64(index),
            Type::i32 | Type::u32 => self.writer.postdeci32(index),
            Type::i16 | Type::u16 => self.writer.postdeci16(index),
            Type::i8 | Type::u8 => self.writer.postdeci8(index),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write subtraction instruction.
    fn write_sub(self: &Self, ty: &Type) {
        match ty {
            Type::i8 | Type::u8 => self.writer.subi8(),
            Type::i16 | Type::u16 => self.writer.subi16(),
            Type::i32 | Type::u32 => self.writer.subi32(),
            Type::i64 | Type::u64 => self.writer.subi64(),
            Type::f32 => self.writer.subf32(),
            Type::f64 => self.writer.subf64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write addition instruction.
    fn write_add(self: &Self, ty: &Type) {
        match ty {
            Type::i8 | Type::u8 => self.writer.addi8(),
            Type::i16 | Type::u16 => self.writer.addi16(),
            Type::i32 | Type::u32 => self.writer.addi32(),
            Type::i64 | Type::u64 => self.writer.addi64(),
            Type::f32 => self.writer.addf32(),
            Type::f64 => self.writer.addf64(),
            Type::String => self.writer.string_concat(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write multiplication instruction.
    fn write_mul(self: &Self, ty: &Type) {
        match ty {
            Type::i8 | Type::u8 => self.writer.muli8(),
            Type::i16 | Type::u16 => self.writer.muli16(),
            Type::i32 | Type::u32 => self.writer.muli32(),
            Type::i64 | Type::u64 => self.writer.muli64(),
            Type::f32 => self.writer.mulf32(),
            Type::f64 => self.writer.mulf64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write division instruction.
    fn write_div(self: &Self, ty: &Type) {
        match ty {
            Type::i8 => self.writer.divs8(),
            Type::u8 => self.writer.divu8(),
            Type::i16 => self.writer.divs16(),
            Type::u16 => self.writer.divu16(),
            Type::i32 => self.writer.divs32(),
            Type::u32 => self.writer.divu32(),
            Type::i64 => self.writer.divs64(),
            Type::u64 => self.writer.divu64(),
            Type::f32 => self.writer.divf32(),
            Type::f64 => self.writer.divf64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write remainder instruction.
    fn write_rem(self: &Self, ty: &Type) {
        match ty {
            Type::i8 => self.writer.rems8(),
            Type::u8 => self.writer.remu8(),
            Type::i16 => self.writer.rems16(),
            Type::u16 => self.writer.remu16(),
            Type::i32 => self.writer.rems32(),
            Type::u32 => self.writer.remu32(),
            Type::i64 => self.writer.rems64(),
            Type::u64 => self.writer.remu64(),
            Type::f32 => self.writer.remf32(),
            Type::f64 => self.writer.remf64(),
            _ => unreachable!("Unsupported operation for type {:?}", ty),
        };
    }

    /// Write compare equal instruction.
    fn write_eq(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty.primitive_size() {
                1 => self.writer.ceq8(),
                2 => self.writer.ceq16(),
                4 => self.writer.ceq32(),
                8 => self.writer.ceq64(),
                size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
            };
        } else if ty.is_string() {
            self.writer.string_ceq();
        } else {
            self.writer.heap_ceq(self.bindings.type_size(ty)); //FIXME: check this
        }
    }

    /// Write compare not equal instruction.
    fn write_neq(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty.primitive_size() {
                1 => self.writer.cneq8(),
                2 => self.writer.cneq16(),
                4 => self.writer.cneq32(),
                8 => self.writer.cneq64(),
                size @ _ => unreachable!("Unsupported size {} for type {:?}", size, ty),
            };
        } else if ty.is_string() {
            self.writer.string_cneq();
        } else {
            self.writer.heap_cneq(self.bindings.type_size(ty)); //FIXME: check this
        }
    }

    /// Write compare less than instruction.
    fn write_lt(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty {
                Type::i8 => self.writer.clts8(),
                Type::u8 => self.writer.cltu8(),
                Type::i16 => self.writer.clts16(),
                Type::u16 => self.writer.cltu16(),
                Type::i32 => self.writer.clts32(),
                Type::u32 => self.writer.cltu32(),
                Type::i64 => self.writer.clts64(),
                Type::u64 => self.writer.cltu64(),
                Type::f32 => self.writer.cltf32(),
                Type::f64 => self.writer.cltf64(),
                _ => unreachable!("Unsupported operation for type {:?}", ty),
            };
        } else if ty.is_string() {
            self.writer.string_clt();
        } else {
            panic!("unsupported type")
        }
    }

    /// Write compare less than or equal instruction.
    fn write_lte(self: &Self, ty: &Type) {
        if ty.is_primitive() {
            match ty {
                Type::i8 => self.writer.cltes8(),
                Type::u8 => self.writer.clteu8(),
                Type::i16 => self.writer.cltes16(),
                Type::u16 => self.writer.clteu16(),
                Type::i32 => self.writer.cltes32(),
                Type::u32 => self.writer.clteu32(),
                Type::i64 => self.writer.cltes64(),
                Type::u64 => self.writer.clteu64(),
                Type::f32 => self.writer.cltef32(),
                Type::f64 => self.writer.cltef64(),
                _ => unreachable!("Unsupported operation for type {:?}", ty),
            };
        } else if ty.is_string() {
            self.writer.string_clte();
        } else {
            panic!("unsupported type")
        }
    }

    /// Writes a return instruction with the correct arguments for the current stack frame.
    fn write_ret(self: &Self) {
        self.writer.ret(self.locals.ret_size());
    }
}