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

    // fn add
    let fn_add =
        w.load_arg1();          // load first argument
        w.load_arg2();          // load second argument
        w.add();                // add arguments
        w.ret();                // return to caller

    // entry point
    let start =
        w.load_const(0);        // arg1 for call to fn_add (replaced with call result after call)
    let loop1 =
        w.load_const(1);        // arg2
        w.call(fn_add, 2);      // call add, leave result on stack
        w.cmp_lt(100_000_000);    // repeat if smaller than...
        w.jmp_nz(loop1);

    // end
        w.print();
        w.exit();

    // initialize vm

    let mut vm = bytecode::VM::new(w.code, start);
    vm.push_const(0);
    vm.push_const(1);

    // dump bytecode, run some instructions and dump them as we go

    println!("{:}", vm.dump_code());

    for _ in 0..25 {
        println!("{}", vm.dump_instruction().unwrap());
        vm.exec();
        println!("frame@{}: {}\n", vm.fp, vm.dump_frame());
        if vm.state != bytecode::VMState::Continue {
            break;
        }
    }

    println!("...many more...\n");
    vm.reset();

    // time and run vm

    let start = ::std::time::Instant::now();

    vm.run();

    let runtime = ::std::time::Instant::now() - start;
    println!("runtime: {:.4}s", runtime.as_secs() as f64 + runtime.subsec_nanos() as f64 * 1e-9);
}
