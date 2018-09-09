//! Itsy, a tiny language for embedded use.

#[macro_use]
extern crate nom;
extern crate byteorder;

pub mod util;
pub mod frontend;
pub mod bytecode;

fn fib(n: i32) -> i32 {

    let mut i = 0;
    let mut j = 1;
    let mut k = 1;
    let mut t;

    if n == 0 {
       return 0;
    }

    while k < n {
        t = i + j;
        i = j;
        j = t;
        k += 1;
    }

    return j;
}

fn main() {

    // write some bytecodes

    let mut w = bytecode::Writer::new();
    let fib_val = 46;

    // fn add
    let ret =
        w.ret();

    let fn_fib =
        w.load_arg1();          // n
        w.push_const(0);
        w.jmp_ieq(ret);         // n==0

    let init =
        w.push_const(0);        // i @ 0
        w.push_const(1);        // j @ 1
        w.push_const(1);        // k @ 2
        w.push_const(0);        // t @ 3

    let for_loop =
        w.load(0);              // i
        w.load(1);              // j
        w.add();                //    i + j
        w.store(3);             // t = ^
        w.load(1);              //     j
        w.store(0);             // i = ^
        w.load(3);              //     t
        w.store(1);             // j = ^

        w.load(2);
        w.iinc();
        w.store(2);

        w.load(2);              // k
        w.load_arg1();          // n
        w.jmp_igt(for_loop);    // n > k
        w.load(1);
        w.ret();

    let main =
        w.push_const(fib_val);
        w.call(fn_fib, 1);
        w.print();
        w.exit();

    // initialize vm

    let mut vm = bytecode::VM::new(w.code, main);

    // dump bytecode, run some instructions and dump them as we go

    println!("{:}", vm.dump_code());

    for _ in 0..10 {
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


    let rust_start = ::std::time::Instant::now();
    println!("\nprint: {}", fib(fib_val));
    let rust_runtime = ::std::time::Instant::now() - rust_start;
    let rust_runtime = rust_runtime.as_secs() as f64 + rust_runtime.subsec_nanos() as f64 * 1e-9;
    println!("rust: {:.4}s", rust_runtime);
    println!("\nfactor: {:.4}s", vm_runtime / rust_runtime);
}
