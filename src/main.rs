//! Itsy, a tiny language for embedded use.

#[macro_use]
extern crate nom;
extern crate byteorder;

pub mod frontend;
pub mod bytecode;

fn main() {

    let source = "
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
    ";

    use frontend::{parse, resolve};
    use bytecode::compile;

    if let Ok(parsed) = parse(source) {
        //println!("{:#?}", parsed); return;
        let resolved = resolve(parsed);
        //println!("{:#?}", resolved.ast);
        //println!("{:#?}", resolved.types);
        let mut writer = compile(resolved);

        // create entry point, call fib
        let start = writer.len();
        writer.lit_u8(37);
        writer.call(0, 1);
        writer.print();
        writer.exit();

        {
            // initialize vm

            let mut vm = bytecode::VM::new(writer.into_program(), vec![ ], start);

            // dump bytecode, run some instructions and dump them as we go

            println!("--- full program dump ---");
            println!("{:}", vm.dump_program());

            println!("--- tracing first few executed instructions ---");
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

            println!("--- untraced run ---");
            let vm_start = ::std::time::Instant::now();
            vm.run();
            let vm_runtime = ::std::time::Instant::now() - vm_start;
            let vm_runtime = vm_runtime.as_secs() as f64 + vm_runtime.subsec_nanos() as f64 * 1e-9;
            println!("time: {:.4}s", vm_runtime);

            /*
                let rust_start = ::std::time::Instant::now();
                println!("\nprint: {}", fib(46));
                let rust_runtime = ::std::time::Instant::now() - rust_start;
                let rust_runtime = rust_runtime.as_secs() as f64 + rust_runtime.subsec_nanos() as f64 * 1e-9;
                println!("rust: {:.4}s", rust_runtime);
                println!("\nfactor: {:.4}s", vm_runtime / rust_runtime);
            */
        }
    }

}
