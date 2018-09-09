//! Bytecode generation and execution.

pub mod gen;
mod vm;
mod writer;

#[macro_use]
mod macros;

pub use self::vm::{VM, VMState};
pub use self::writer::Writer;

opcodes!{

    /// Load constant from constant pool onto stack.
    fn load_const(vm: &mut Self, const_id: u8) {
        let tmp = vm.consts[const_id as usize];
        vm.stack.push(tmp);
    }

    /// Load constant from constant pool onto stack.
    fn push_const(vm: &mut Self, value: i32) {
        vm.stack.push(value);
    }

    /// Load stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn load(vm: &mut Self, offset: i32) {
        let local = vm.stack[(vm.fp as i32 + offset) as usize];  // todo: i32/u32 overflow
        vm.stack.push(local);
    }

    /// Pop stackvalue and store it at the given offset (relative to the stackframe).
    fn store(vm: &mut Self, offset: i32) {
        let local = vm.stack.pop().unwrap();
        vm.stack[(vm.fp as i32 + offset) as usize] = local;
    }

    /// Load function argument 1 and push it onto the stack. Equivalent to load -4.
    fn load_arg1(vm: &mut Self) {
        let local = vm.stack[(vm.fp as i32 -4) as usize];
        vm.stack.push(local);
    }

    /// Load function argument 2 and push it onto the stack. Equivalent to load -5.
    fn load_arg2(vm: &mut Self) {
        let local = vm.stack[(vm.fp as i32 -5) as usize];
        vm.stack.push(local);
    }

    /// Load function argument 3 and push it onto the stack. Equivalent to load -6.
    fn load_arg3(vm: &mut Self) {
        let local = vm.stack[(vm.fp as i32 -6) as usize];
        vm.stack.push(local);
    }

    /// Function call. Saves state and sets programm counter to given addr. Expects
    /// callee arguments on the stack and number of arguments as num_args.
    fn call(vm: &mut Self, addr: u32, num_args: u8) {
        let next_pc = vm.pc;
        let fp = vm.fp;
        vm.stack.push(num_args as i32);     // save number of arguments
        vm.stack.push(fp as i32);           // save frame pointer
        vm.stack.push(next_pc as i32);      // save program counter as it would be after this instruction
        vm.fp = vm.stack.len();             // set new frame pointer
        vm.pc = addr as usize;              // set new program counter
    }

    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn ret(vm: &mut Self) {

        // save return value
        let retval = *vm.stack.last().unwrap();

        // get previous state
        let prev_pc = vm.stack[vm.fp - 1];          // load program counter from before the call
        let prev_fp = vm.stack[vm.fp - 2];          // load old frame pointer
        let prev_num_args = vm.stack[vm.fp - 3];    // load number of arguments that were on the stack prior to call

        // truncate stack back down to the start of the callframe minus 3 (the above three states) minus the number
        // of arguments pushed by the caller prior to call (so that the caller doesn't have to clean them up).
        vm.stack.truncate((vm.fp as i32 - 3 - prev_num_args) as usize);

        // restore previous program counter and frame pointer
        vm.fp = prev_fp as usize;
        vm.pc = prev_pc as usize;

        // push the return value back onto the stack
        vm.stack.push(retval);
    }

    /// Negate current value on stack.
    fn negate(vm: &mut Self) {
        let tmp = vm.stack.pop().unwrap();
        vm.stack.push(-tmp);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn add(vm: &mut Self) {
        let b = vm.stack.pop().unwrap();
        let a = vm.stack.pop().unwrap();
        vm.stack.push(a + b);
    }

    /// Pops 2 values from the stack and pushes their difference.
    fn sub(vm: &mut Self) {
        let b = vm.stack.pop().unwrap();
        let a = vm.stack.pop().unwrap();
        vm.stack.push(a - b);
    }

    /// Increments the value at the top of the stack.
    fn iinc(vm: &mut Self) {
        let a = vm.stack.pop().unwrap();
        vm.stack.push(a + 1);
    }

    fn jmp_ieq(vm: &mut Self, addr: u32) {
        let a = vm.stack.pop().unwrap();
        let b = vm.stack.pop().unwrap();
        if a == b {
            vm.pc = addr as usize;
        }
    }

    fn jmp_igt(vm: &mut Self, addr: u32) {
        let a = vm.stack.pop().unwrap();
        let b = vm.stack.pop().unwrap();
        if a > b {
            vm.pc = addr as usize;
        }
    }

    fn jmp_igte(vm: &mut Self, addr: u32) {
        let a = vm.stack.pop().unwrap();
        let b = vm.stack.pop().unwrap();
        if a >= b {
            vm.pc = addr as usize;
        }
    }


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

    /// Jumps to the given instruction if reg_cond is true.
    fn jmp_true(vm: &mut Self, target: u32) {
        if vm.reg_cond {
            vm.pc = target as usize;
        }
    }

    /// Jumps to the given instruction if reg_cond is false.
    fn jmp_false(vm: &mut Self, target: u32) {
        if vm.reg_cond == false {
            vm.pc = target as usize;
        }
    }

    /// Print current value on stack.
    fn print(vm: &mut Self) {
        let tmp = vm.stack.last().unwrap();
        println!("print: {:?}", tmp);
    }

    /// Terminate program execution.
    fn exit(vm: &mut Self) {
        vm.state = VMState::Terminate;
    }
}
