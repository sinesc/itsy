//! Itsy, a tiny language for embedded use.

#[macro_use]
extern crate nom;
extern crate byteorder;

pub mod util;
pub mod frontend;
pub mod bytecode;

fn main() {

    // write some bytecodes

    let mut w = bytecode::Writer::new();

    let ret =
        w.load_arg1();          // arg
        w.ret();                // return arg

    let fn_fib =
        w.load_arg1();          // arg
        w.val2();               // 2
        w.jgts(ret);            // arg < 2

        w.load_arg1();          // arg
        w.deci();               // arg--
        w.call(fn_fib, 1);

        w.val2();               // 2
        w.load_arg1();          // arg
        w.sub();                // arg - 2
        w.call(fn_fib, 1);

        w.add();                // fib(...) + fib(...)
        w.ret();

    let main =
        w.const32(0);
        w.call(fn_fib, 1);
        w.print();
        w.exit();

    // initialize vm

    let mut vm = bytecode::VM::new(w.code, main);
    vm.add_const(37);

    // dump bytecode, run some instructions and dump them as we go

    println!("{:}", vm.dump_code());

    for _ in 0..15 {
        println!("{}", vm.dump_instruction().unwrap());
        vm.exec();
        println!("frame@{}: {}\n", vm.fp, vm.dump_frame());
        if vm.state != bytecode::VMState::Continue {
            break;
        }
    }

    if vm.state == bytecode::VMState::Continue {
        println!("...many more...\n");
    }

    vm.reset();

    // time and run vm

    let vm_start = ::std::time::Instant::now();
    vm.run();
    let vm_runtime = ::std::time::Instant::now() - vm_start;
    let vm_runtime = vm_runtime.as_secs() as f64 + vm_runtime.subsec_nanos() as f64 * 1e-9;
    println!("itsy: {:.4}s", vm_runtime);

/*
    let rust_start = ::std::time::Instant::now();
    println!("\nprint: {}", fib(46));
    let rust_runtime = ::std::time::Instant::now() - rust_start;
    let rust_runtime = rust_runtime.as_secs() as f64 + rust_runtime.subsec_nanos() as f64 * 1e-9;
    println!("rust: {:.4}s", rust_runtime);
    println!("\nfactor: {:.4}s", vm_runtime / rust_runtime);
*/
}
