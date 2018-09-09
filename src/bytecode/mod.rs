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
        let next_pc = vm.code.position();
        let fp = vm.fp;
        vm.stack.push(num_args as i32);     // save number of arguments
        vm.stack.push(fp as i32);           // save frame pointer
        vm.stack.push(next_pc as i32);      // save program counter as it would be after this instruction
        vm.fp = vm.stack.len();             // set new frame pointer
        vm.code.set_position(addr as u64);  // set new program counter
    }

    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn ret(vm: &mut Self) {
        let retval = vm.stack.pop().unwrap();   // get return value   todo: probably not useful

        vm.stack.truncate(vm.fp);           // restore stack to state of call
        vm.code.set_position(vm.stack.pop().unwrap() as u64);   // get previous program counter
        vm.fp = vm.stack.pop().unwrap() as usize;   // get previous frame pointer
        let argc = vm.stack.pop().unwrap(); // get number of arguments that were pushed prior to call and have to be removed

        let slen = vm.stack.len();
        vm.stack.truncate((slen as i32 - argc) as usize);      // remove the arguments todo: optimize to one truncate

        vm.stack.push(retval);              // push the return value back onto the stack
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

    /// Compares if the the current stack value is greater than the given value. Pushes 1 if it is, otherwise 0.
    fn cmp_gt(vm: &mut Self, other: i32) {
        let tmp = *vm.stack.last().unwrap();
        vm.stack.push( if tmp > other { 1 } else { 0 } );
    }

    /// Compares if the the current stack value is less than the given value. Pushes 1 if it is, otherwise 0.
    fn cmp_lt(vm: &mut Self, other: i32) {
        let tmp = *vm.stack.last().unwrap();
        vm.stack.push( if tmp < other { 1 } else { 0 } );
    }

    /// Pops a value from the stack and jumps to the given instruction if the stack value is not 0.
    fn jmp_nz(vm: &mut Self, target: u32) {
        let tmp = vm.stack.pop().unwrap();
        if tmp != 0 {
            vm.code.set_position(target as u64);
        }
    }

    /// Pops a value from the stack and jumps to the given instruction if the stack value is 0.
    fn jmp_z(vm: &mut Self, target: u32) {
        let tmp = vm.stack.pop().unwrap();
        if tmp == 0 {
            vm.code.set_position(target as u64);
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
        println!("exiting");
    }
}
