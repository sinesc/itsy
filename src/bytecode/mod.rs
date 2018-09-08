//! Bytecode generation and execution.

pub mod gen;
mod vm;
mod writer;

#[macro_use]
mod macros;

pub use self::vm::VM;
pub use self::writer::Writer;

opcodes!{

    /// Terminate program execution.
    fn exit=0(vm: &mut VM) {
        println!("exit");
    }

    /// Load constant from constant pool.
    fn load_const=1(vm: &mut VM, const_id: u8) {
        println!("load_const {:?}", const_id);
        println!("vm {:?}", vm);
    }

    /// Load constant from long constant pool.
    fn load_const_long=2(vm: &mut VM, const_id: u32) {
        println!("load_const_long{:?}", const_id);
    }
}
