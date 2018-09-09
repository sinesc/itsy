//! Bytecode generation and execution.

#[macro_use]
mod macros;
mod vm;
mod writer;
mod utils;
pub mod gen;

pub use self::vm::{VM, VMState};
pub use self::writer::Writer;

type Value = i32;

opcodes!{

    /// Load constant from constant pool onto stack.
    fn const32(vm: &mut Self, const_id: u8) {
        let tmp = vm.consts[const_id as usize];
        vm.push(tmp);
    }
    /// Load constant from constant pool onto stack.
    fn const32_16(vm: &mut Self, const_id: u16) {
        let tmp = vm.consts[const_id as usize];
        vm.push(tmp);
    }
    /// Load constant from constant pool onto stack.
    fn const64(vm: &mut Self, const_id: u8) {
        let l = vm.consts[const_id as usize];
        let h = vm.consts[(const_id + 1) as usize];
        vm.push(l);
        vm.push(h);
    }
    /// Load constant from constant pool onto stack.
    fn const64_16(vm: &mut Self, const_id: u16) {
        let l = vm.consts[const_id as usize];
        let h = vm.consts[(const_id + 1) as usize];
        vm.push(l);
        vm.push(h);
    }

    /// Push 0 onto stack.
    fn val0(vm: &mut Self) {
        vm.push(0);
    }
    /// Push 1 onto stack.
    fn val1(vm: &mut Self) {
        vm.push(1);
    }
    /// Push 2 onto stack.
    fn val2(vm: &mut Self) {
        vm.push(2);
    }
    /// Push -1 onto stack.
    fn valm1(vm: &mut Self) {
        vm.push(-1);
    }

    /// Load stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn load(vm: &mut Self, offset: i32) {
        let local = vm.peek(offset);
        vm.push(local);
    }

    /// Pop stackvalue and store it at the given offset (relative to the stackframe).
    fn store(vm: &mut Self, offset: i32) {
        let local = vm.pop();
        vm.store(offset, local);
    }

    /// Load function argument 1 and push it onto the stack. Equivalent to load -4.
    fn load_arg1(vm: &mut Self) {
        let local = vm.peek(-4);
        vm.push(local);
    }

    /// Load function argument 2 and push it onto the stack. Equivalent to load -5.
    fn load_arg2(vm: &mut Self) {
        let local = vm.peek(-5);
        vm.push(local);
    }

    /// Load function argument 3 and push it onto the stack. Equivalent to load -6.
    fn load_arg3(vm: &mut Self) {
        let local = vm.peek(-6);
        vm.push(local);
    }

    /// Function call. Saves state and sets programm counter to given addr. Expects
    /// callee arguments on the stack and number of arguments as num_args.
    fn call(vm: &mut Self, addr: u32, num_args: u8) {
        let next_pc = vm.pc;
        let fp = vm.fp;
        vm.pushu8(num_args);    // save number of arguments
        vm.pushu(fp);           // save frame pointer
        vm.pushu(next_pc);      // save program counter as it would be after this instruction
        vm.fp = vm.sp();        // set new frame pointer
        vm.pc = addr;           // set new program counter
    }
    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn ret(vm: &mut Self) {

        // save return value
        let retval = vm.top();

        // get previous state
        let prev_pc = vm.peeku(-1);          // load program counter from before the call
        let prev_fp = vm.peeku(-2);          // load old frame pointer
        let prev_num_args = vm.peeku(-3);    // load number of arguments that were on the stack prior to call

        // truncate stack back down to the start of the callframe minus 3 (the above three states) minus the number
        // of arguments pushed by the caller prior to call (so that the caller doesn't have to clean them up).
        let new_size = vm.fp - 3 - prev_num_args;
        vm.truncate(new_size);

        // restore previous program counter and frame pointer
        vm.fp = prev_fp;
        vm.pc = prev_pc;

        // push the return value back onto the stack
        vm.push(retval);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn add(vm: &mut Self) {
        let a = vm.pop();
        let b = vm.pop();
        vm.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their difference.
    fn sub(vm: &mut Self) {
        let a = vm.pop();
        let b = vm.pop();
        vm.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn mul(vm: &mut Self) {
        let a = vm.pop();
        let b = vm.pop();
        vm.push(a * b);
    }

    /// Pops two values and jumps to given address it they equal.
    fn jeq(vm: &mut Self, addr: u32) {
        let a = vm.pop();
        let b = vm.pop();
        if a == b {
            vm.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is greater than the second.
    fn jgts(vm: &mut Self, addr: u32) {
        let a = vm.pop();
        let b = vm.pop();
        if a > b {
            vm.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is greater/equal the second.
    fn jgtes(vm: &mut Self, addr: u32) {
        let a = vm.pop();
        let b = vm.pop();
        if a >= b {
            vm.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jlts(vm: &mut Self, addr: u32) {
        let a = vm.pop();
        let b = vm.pop();
        if a < b {
            vm.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltes(vm: &mut Self, addr: u32) {
        let a = vm.pop();
        let b = vm.pop();
        if a <= b {
            vm.pc = addr;
        }
    }

    /// Negate current value on stack.
    fn negs(vm: &mut Self) {
        let tmp = vm.pop();
        vm.push(-tmp);
    }
    /// Negate current value on stack.
    fn negf(vm: &mut Self) {
        let tmp = vm.pop();
        vm.push(-tmp);
    }

    /// Increments the value at the top of the stack.
    fn inci(vm: &mut Self) {
        let a = vm.pop();
        vm.push(a + 1);
    }
    /// Decrements the value at the top of the stack.
    fn deci(vm: &mut Self) {
        let a = vm.pop();
        vm.push(a - 1);
    }

/*
    /// Compares if the the current stack value is greater than the given value. Sets reg_cond accordingly.
    fn cmp_gt(vm: &mut Self, other: i32) {
        let tmp = *vm.stack.last().unwrap();
        vm.reg_cond = tmp > other;
    }

    /// Compares if the the current stack value is less than the given value. Sets reg_cond accordingly.
    fn cmp_lt(vm: &mut Self, other: i32) {
        let tmp = *vm.stack.last().unwrap();
        vm.reg_cond = tmp < other;
    }

    /// Compares if the the current stack value is less than the given value. Sets reg_cond accordingly.
    fn cmp_eq(vm: &mut Self, other: i32) {
        let tmp = *vm.stack.last().unwrap();
        vm.reg_cond = tmp == other;
    }
*/
    /// Print current value on stack.
    fn print(vm: &mut Self) {
        println!("print: {:?}", vm.top());
    }

    /// Terminate program execution.
    fn exit(vm: &mut Self) {
        vm.state = VMState::Terminate;
    }
}
