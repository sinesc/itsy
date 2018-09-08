//! Bytecode generation and execution.

pub mod gen;
mod vm;
mod writer;

#[macro_use]
mod macros;

pub use self::vm::VM;
pub use self::writer::Writer;

opcodes!{

    /// Load constant from constant pool onto stack.
    fn load_const(vm: &mut Self, const_id: u8) {
        let tmp = vm.consts[const_id as usize];
        vm.stack.push(tmp);
    }

    /// Negate current value on stack.
    fn negate(vm: &mut Self) {
        let tmp = vm.stack.pop().unwrap();
        vm.stack.push(-tmp);
    }

    fn add(vm: &mut Self) {
        let a = vm.stack.pop().unwrap();
        let b = vm.stack.pop().unwrap();
        vm.stack.push(a + b);
    }

    /// Print current value on stack.
    fn print(vm: &mut Self) {
        let tmp = vm.stack.last().unwrap();
        println!("print: {:?}", tmp);
    }

    /// Terminate program execution.
    fn exit(vm: &mut Self) {
        vm.exit = true;
        println!("exiting");
    }
}
