//! Itsy, a tiny language for embedded use.

#[macro_use]
extern crate nom;
extern crate byteorder;

pub mod frontend;
pub mod bytecode;

fn main() {

    let source = "
        fn fib(n: i32) -> i32 {
            if n < 2 {
                n
            } else {
                fib(n - 1) + fib(n - 2)
            }
        }
    ";
    // print(fib(37));

    use nom::types::CompleteStr as Input;
    use frontend::{parse, resolve};
    use bytecode::compile;

    if let Ok(parsed) = parse(Input(source)) {
        let resolved = resolve(parsed.1);
        println!("{:#?}", resolved.ast);
        println!("{:#?}", resolved.types);
        let program = compile(resolved);

        {
            // initialize vm

            let mut vm = bytecode::VM::new(program, vec![ 30 ], 0);

            // dump bytecode, run some instructions and dump them as we go

            println!("{:}", vm.dump_program());

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
    }

}
