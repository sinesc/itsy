//! Opcode definitions. Implemented on Writer/VM.

impl_vm!{

    /// Does nothing. Uses as label when debugging.
    #[allow(unused_variables)]
    fn debug(self: &mut Self, hint: u32) { }

    /// Load constant from constant pool onto stack.
    fn const32(self: &mut Self, const_id: u8) {
        let tmp = self.consts[const_id as usize];
        self.push(tmp);
    }
    /// Load constant from constant pool onto stack.
    fn const32_16(self: &mut Self, const_id: u16) {
        let tmp = self.consts[const_id as usize];
        self.push(tmp);
    }
    /// Load constant from constant pool onto stack.
    fn const64(self: &mut Self, const_id: u8) {
        let l = self.consts[const_id as usize];
        let h = self.consts[(const_id + 1) as usize];
        self.push(l);
        self.push(h);
    }
    /// Load constant from constant pool onto stack.
    fn const64_16(self: &mut Self, const_id: u16) {
        let l = self.consts[const_id as usize];
        let h = self.consts[(const_id + 1) as usize];
        self.push(l);
        self.push(h);
    }

    /// Moves the stack pointer by given number of items (32bit words) to make room for local variables.
    fn reserve(self: &mut Self, num_items: u8) {
        for _ in 0..num_items {
            self.push(0);
        }
    }

    // Push given u8 onto stack.
    fn lit_u8(self: &mut Self, val: u8) {
        self.pushu8(val);
    }
    // Push given u16 onto stack.
    fn lit_u16(self: &mut Self, val: u16) {
        self.pushu16(val);
    }
    // Push given u32 onto stack.
    fn lit_u32(self: &mut Self, val: u32) {
        self.pushu(val);
    }
    /// Push 0 onto stack.
    fn lit0(self: &mut Self) {
        self.push(0);
    }
    /// Push 1 onto stack.
    fn lit1(self: &mut Self) {
        self.push(1);
    }
    /// Push 2 onto stack.
    fn lit2(self: &mut Self) {
        self.push(2);
    }
    /// Push -1 onto stack.
    fn litm1(self: &mut Self) {
        self.push(-1);
    }

    /// Load stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn load(self: &mut Self, offset: i32) {
        let local = self.peek(offset);
        self.push(local);
    }
    /// Pop stackvalue and store it at the given offset (relative to the stackframe).
    fn store(self: &mut Self, offset: i32) {
        let local = self.pop();
        self.storei(offset, local);
    }

    /// Load function argument 1 and push it onto the stack. Equivalent to load -4.
    fn load_arg1(self: &mut Self) {
        let local = self.peek(-4);
        self.push(local);
    }
    /// Load function argument 2 and push it onto the stack. Equivalent to load -5.
    fn load_arg2(self: &mut Self) {
        let local = self.peek(-5);
        self.push(local);
    }
    /// Load function argument 3 and push it onto the stack. Equivalent to load -6.
    fn load_arg3(self: &mut Self) {
        let local = self.peek(-6);
        self.push(local);
    }

    /// Function call. Saves state and sets programm counter to given addr. Expects
    /// callee arguments on the stack and number of arguments as num_args.
    fn call(self: &mut Self, addr: u32, num_args: u8) {
        let next_pc = self.pc;
        let fp = self.fp;
        self.pushu8(num_args);    // save number of arguments
        self.pushu(fp);           // save frame pointer
        self.pushu(next_pc);      // save program counter as it would be after this instruction
        self.fp = self.sp();      // set new frame pointer
        self.pc = addr;           // set new program counter
    }
    /*fn call1_u8(self: &mut Self, addr: u8) {
        self.call(addr as u32, 1);
    }
    fn call_u8(self: &mut Self, addr: u8, num_args: u8) {
        self.call(addr as u32, num_args);
    }*/
    fn callp1(self: &mut Self) {
        let addr = self.popu();
        self.call(addr, 1);
    }
    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn ret(self: &mut Self) {

        // save return value
        let retval = self.top();

        // get previous state
        let prev_pc = self.peeku(-1);          // load program counter from before the call
        let prev_fp = self.peeku(-2);          // load old frame pointer
        let prev_num_args = self.peeku(-3);    // load number of arguments that were on the stack prior to call

        // truncate stack back down to the start of the callframe minus 3 (the above three states) minus the number
        // of arguments pushed by the caller prior to call (so that the caller doesn't have to clean them up).
        let new_size = self.fp - 3 - prev_num_args;
        self.truncate(new_size);

        // restore previous program counter and frame pointer
        self.fp = prev_fp;
        self.pc = prev_pc;

        // push the return value back onto the stack
        self.push(retval);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn add(self: &mut Self) {
        let a = self.pop();
        let b = self.pop();
        self.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their difference.
    fn sub(self: &mut Self) {
        let a = self.pop();
        let b = self.pop();
        self.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn mul(self: &mut Self) {
        let a = self.pop();
        let b = self.pop();
        self.push(a * b);
    }

    /// Jumps unconditionally to the given address.
    fn jmp(self: &mut Self, addr: u32) {
        self.pc = addr;
    }

    /// Pops one values and jumps to given address if it is 0.
    fn j0(self: &mut Self, addr: u32) {
        let a = self.pop();
        if a == 0 {
            self.pc = addr;
        }
    }

    /// Pops two values and jumps to given address it they equal.
    fn jeq(self: &mut Self, addr: u32) {
        let a = self.pop();
        let b = self.pop();
        if a == b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is greater than the second.
    fn jgts(self: &mut Self, addr: u32) {
        let a = self.pop();
        let b = self.pop();
        if a > b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is greater/equal the second.
    fn jgtes(self: &mut Self, addr: u32) {
        let a = self.pop();
        let b = self.pop();
        if a >= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jlts(self: &mut Self, addr: u32) {
        let a = self.pop();
        let b = self.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltes(self: &mut Self, addr: u32) {
        let a = self.pop();
        let b = self.pop();
        if a <= b {
            self.pc = addr;
        }
    }

    /// Pops two values and pushes a 1 if the first value equals the second, otherwise a 0.
    fn ceq(self: &mut Self) {
        let a = self.pop();
        let b = self.pop();
        self.push((a == b) as i32);
    }
    /// Pops two values and pushes a 1 if the first value is greater the second., otherwise a 0.
    fn cgts(self: &mut Self) {
        let a = self.pop();
        let b = self.pop();
        self.push((a > b) as i32);
    }
    /// Pops two values and pushes a 1 if the first value is greater than the second, otherwise a 0.
    fn clts(self: &mut Self) {
        let a = self.pop();
        let b = self.pop();
        self.push((a < b) as i32);
    }

    fn rustcall(self: &mut Self, func: RustFn) {
        //println!("{:?}", func.exec());
    }





    /// Negate current value on stack.
    fn negs(self: &mut Self) {
        let tmp = self.pop();
        self.push(-tmp);
    }
    /// Negate current value on stack.
    fn negf(self: &mut Self) {
        let tmp = self.pop();
        self.push(-tmp);
    }

    /// Increments the value at the top of the stack.
    fn inci(self: &mut Self) {
        let a = self.pop();
        self.push(a + 1);
    }
    /// Decrements the value at the top of the stack.
    fn deci(self: &mut Self) {
        let a = self.pop();
        self.push(a - 1);
    }

    /// Terminate program execution.
    fn exit(self: &mut Self) {
        self.state = ::bytecode::VMState::Terminate;
    }
}
