//! Opcode definitions. Implemented on Writer/VM.

use crate::bytecode::{Value, Value64, StackOp, StackOpFp};

impl_vm!{

    /// Does nothing. Uses as label when debugging.
    #[allow(unused_variables)]
    fn debug(self: &mut Self, label: u32) { }

    /// Moves the stack pointer by given number of items (32bit words) to make room for local variables.
    fn reserve(self: &mut Self, num_items: u8) {
        for _ in 0..num_items {
            self.stack.push(0);
        }
    }

    /// Load constant from constant pool onto stack.
    fn constr32(self: &mut Self, const_id: u8) {
        let tmp = self.program.consts[const_id as usize];
        self.stack.push(tmp);
    }
    /// Load constant from constant pool onto stack.
    fn constr32_16(self: &mut Self, const_id: u16) {
        let tmp = self.program.consts[const_id as usize];
        self.stack.push(tmp);
    }
    /// Load constant from constant pool onto stack.
    fn constr64(self: &mut Self, const_id: u8) {
        let l = self.program.consts[const_id as usize];         // todo: reuse stackops here
        let h = self.program.consts[(const_id + 1) as usize];
        self.stack.push(l);
        self.stack.push(h);
    }
    /// Load constant from constant pool onto stack.
    fn constr64_16(self: &mut Self, const_id: u16) {
        let l = self.program.consts[const_id as usize];         // todo: reuse stackops here
        let h = self.program.consts[(const_id + 1) as usize];
        self.stack.push(l);
        self.stack.push(h);
    }

    /// Push 0 onto stack.
    fn lit0(self: &mut Self) {
        self.stack.push(0u32);
    }
    /// Push 1 onto stack.
    fn lit1(self: &mut Self) {
        self.stack.push(1u32);
    }
    /// Push 2 onto stack.
    fn lit2(self: &mut Self) {
        self.stack.push(2u32);
    }
    /// Push -1 onto stack.
    fn litm1(self: &mut Self) {
        self.stack.push(-1i32);
    }

    /// Load stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn loadr32(self: &mut Self, offset: i32) {
        let local: Value = self.stack.load_fp(offset);
        self.stack.push(local);
    }
    /// Pop stackvalue and store it at the given offset (relative to the stackframe).
    fn storer32(self: &mut Self, offset: i32) {
        let local: Value = self.stack.pop();
        self.stack.store_fp(offset, local);
    }

    /// Load 64 bit stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn loadr64(self: &mut Self, offset: i32) {
        let local: i64 = self.stack.load_fp(offset);
        self.stack.push(local);
    }
    /// Pop 64 bit stackvalue and store it at the given offset (relative to the stackframe).
    fn storer64(self: &mut Self, offset: i32) {
        let local: i64 = self.stack.pop();
        self.stack.store_fp(offset, local);
    }

    /// Load function argument 1 and push it onto the stack. Equivalent to load -4.
    fn load_arg1(self: &mut Self) {
        let local: Value = self.stack.load_fp(-4);
        self.stack.push(local);
    }
    /// Load function argument 2 and push it onto the stack. Equivalent to load -5.
    fn load_arg2(self: &mut Self) {
        let local: Value = self.stack.load_fp(-5);
        self.stack.push(local);
    }
    /// Load function argument 3 and push it onto the stack. Equivalent to load -6.
    fn load_arg3(self: &mut Self) {
        let local: Value = self.stack.load_fp(-6);
        self.stack.push(local);
    }

    /// Calls the given Rust function.
    fn rustcall(self: &mut Self, func: RustFn) {
        T::from_u16(func).exec(self);
    }

    /// Function call. Saves state and sets programm counter to given addr. Expects
    /// callee arguments on the stack and number of arguments (in 32 bit words) as num_args.
    fn call(self: &mut Self, addr: u32, num_args: u8) {
        let next_pc = self.pc;
        let fp = self.stack.fp;
        self.stack.push(num_args);          // save number of arguments
        self.stack.push(fp);                // save frame pointer
        self.stack.push(next_pc);           // save program counter as it would be after this instruction
        self.stack.fp = self.stack.sp();    // set new frame pointer
        self.pc = addr;                     // set new program counter
    }
    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn ret(self: &mut Self) {

        // save return value
        let retval: Value = self.stack.top();

        // get previous state
        let prev_pc = self.stack.load_fp(-1);           // load program counter from before the call
        let prev_fp = self.stack.load_fp(-2);           // load old frame pointer
        let prev_num_args: u8 = self.stack.load_fp(-3); // load number of arguments that were on the stack prior to call

        // truncate stack back down to the start of the callframe minus 3 (the above three states) minus the number
        // of arguments pushed by the caller prior to call (so that the caller doesn't have to clean them up).
        let new_size = self.stack.fp - 3 - prev_num_args as u32;
        self.stack.truncate(new_size);

        // restore previous program counter and frame pointer
        self.stack.fp = prev_fp;
        self.pc = prev_pc;

        // push the return value back onto the stack
        self.stack.push(retval);
    }

    /// Pops 2 values from the stack and pushes their logical conjunction.
    fn and(self: &mut Self) {
        let a: bool = self.stack.pop();
        let b: bool = self.stack.pop();
        self.stack.push(a && b);
    }
    /// Pops 2 values from the stack and pushes their logical disjunction.
    fn or(self: &mut Self) {
        let a: bool = self.stack.pop();
        let b: bool = self.stack.pop();
        self.stack.push(a || b);
    }
    /// Pops a values from the stack and pushes its logical negation.
    fn not(self: &mut Self) {
        let a: bool = self.stack.pop();
        self.stack.push(!a);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addi(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their difference.
    fn subi(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn muli(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push(a * b);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addf(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their sum.
    fn subf(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn mulf(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push(a * b);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addi64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their difference.
    fn subi64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn muli64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push(a * b);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addf64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their sum.
    fn subf64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn mulf64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push(a * b);
    }

    /// Jumps unconditionally to the given address.
    fn jmp(self: &mut Self, addr: u32) {
        self.pc = addr;
    }
    /// Pops one values and jumps to given address if it is 0.
    fn j0(self: &mut Self, addr: u32) {
        let a: Value = self.stack.pop();
        if a == 0 {
            self.pc = addr;
        }
    }

    // todo: comparisons need to be generated somehow, this is bound to contain copy&paste errors

    /// Pops two values and jumps to given address it they equal.
    fn jeqr32(self: &mut Self, addr: u32) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        if a == b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address it they equal.
    fn jeqr64(self: &mut Self, addr: u32) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        if a == b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address it they are not equal.
    fn jneqr32(self: &mut Self, addr: u32) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        if a != b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address it they are not equal.
    fn jneqr64(self: &mut Self, addr: u32) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        if a != b {
            self.pc = addr;
        }
    }

    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jlts32(self: &mut Self, addr: u32) {
        let a: i32 = self.stack.pop();
        let b: i32 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltu32(self: &mut Self, addr: u32) {
        let a: u32 = self.stack.pop();
        let b: u32 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltf32(self: &mut Self, addr: u32) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jlts64(self: &mut Self, addr: u32) {
        let a: i64 = self.stack.pop();
        let b: i64 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltu64(self: &mut Self, addr: u32) {
        let a: u64 = self.stack.pop();
        let b: u64 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltf64(self: &mut Self, addr: u32) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }

    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltes32(self: &mut Self, addr: u32) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jlteu32(self: &mut Self, addr: u32) {
        let a: u32 = self.stack.pop();
        let b: u32 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltef32(self: &mut Self, addr: u32) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltes64(self: &mut Self, addr: u32) {
        let a: i64 = self.stack.pop();
        let b: i64 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jlteu64(self: &mut Self, addr: u32) {
        let a: u64 = self.stack.pop();
        let b: u64 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltef64(self: &mut Self, addr: u32) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }

    /// Pops two values and pushes a 1 if the first value equals the second, otherwise a 0.
    fn ceqr32(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push((a == b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value equals the second, otherwise a 0.
    fn ceqr64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push((a == b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value does not equal the second, otherwise a 0.
    fn cneqr32(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push((a != b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value does not equal the second, otherwise a 0.
    fn cneqr64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push((a != b) as Value);
    }

    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn clts32(self: &mut Self) {
        let a: i32 = self.stack.pop();
        let b: i32 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltu32(self: &mut Self) {
        let a: u32 = self.stack.pop();
        let b: u32 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltf32(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn clts64(self: &mut Self) {
        let a: i64 = self.stack.pop();
        let b: i64 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltu64(self: &mut Self) {
        let a: u64 = self.stack.pop();
        let b: u64 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltf64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }

    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltes32(self: &mut Self) {
        let a: i32 = self.stack.pop();
        let b: i32 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn clteu32(self: &mut Self) {
        let a: u32 = self.stack.pop();
        let b: u32 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltef32(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltes64(self: &mut Self) {
        let a: i64 = self.stack.pop();
        let b: i64 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn clteu64(self: &mut Self) {
        let a: u64 = self.stack.pop();
        let b: u64 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltef64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }

    /// Increments the value at the top of the stack.
    fn inci(self: &mut Self) {
        let a: Value = self.stack.pop();
        self.stack.push(a + 1);
    }
    /// Decrements the value at the top of the stack.
    fn deci(self: &mut Self) {
        let a: Value = self.stack.pop();
        self.stack.push(a - 1);
    }
    /// Increments the value at the top of the stack.
    fn inci64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        self.stack.push(a + 1);
    }
    /// Decrements the value at the top of the stack.
    fn deci64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        self.stack.push(a - 1);
    }

    /// Increments the stackvalue at given offset (relative to the stackframe) and pushes the result onto the stack.
    fn preinci(self: &mut Self, offset: i32) {
        let mut local: Value = self.stack.load_fp(offset);
        local += 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stackframe) and pushes the result onto the stack.
    fn predeci(self: &mut Self, offset: i32) {
        let mut local: Value = self.stack.load_fp(offset);
        local -= 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }
    /// Increments the stackvalue at given offset (relative to the stackframe) and pushes the result onto the stack.
    fn preinci64(self: &mut Self, offset: i32) {
        let mut local: Value64 = self.stack.load_fp(offset);
        local += 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stackframe) and pushes the result onto the stack.
    fn predeci64(self: &mut Self, offset: i32) {
        let mut local: Value64 = self.stack.load_fp(offset);
        local -= 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }

    /// Increments the stackvalue at given offset (relative to the stackframe) and pushes the previous value onto the stack.
    fn postinci(self: &mut Self, offset: i32) {
        let local: Value = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local + 1);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stackframe) and pushes the previous value onto the stack.
    fn postdeci(self: &mut Self, offset: i32) {
        let local: Value = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local - 1);
        self.stack.push(local);
    }
    /// Increments the stackvalue at given offset (relative to the stackframe) and pushes the previous value onto the stack.
    fn postinci64(self: &mut Self, offset: i32) {
        let local: Value64 = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local + 1);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stackframe) and pushes the previous value onto the stack.
    fn postdeci64(self: &mut Self, offset: i32) {
        let local: Value64 = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local - 1);
        self.stack.push(local);
    }

    /// Yield program execution.
    fn yld(self: &mut Self) {
        self.state = crate::bytecode::VMState::Yield;
    }
    /// Terminate program execution.
    fn exit(self: &mut Self) {
        self.state = crate::bytecode::VMState::Terminate;
    }
}
