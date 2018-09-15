//! Itsy, a tiny language for embedded use.

#[macro_use]
extern crate nom;
extern crate byteorder;

pub mod frontend;
#[macro_use]
pub mod bytecode;

fn_map!(ItsyFn, {
    fn print(vm: &mut VM, value: u32) {
        println!("print:{}", value);
    }
    fn hello_world(vm: &mut VM) {
        println!("hello world!");
    }
});

/// One stop shop to `parse`, `resolve` and `compile` given Itsy source code.
///
/// Call `run` on the returned `VM` struct to execute the program.
pub fn exec<T>(program: &str) -> bytecode::VM<T> where T: bytecode::RustFnId {
    use frontend::{parse, resolve};
    use bytecode::{compile, VM};

    let parsed = parse(program).unwrap();
    let resolved = resolve::<T>(parsed);
    let mut writer = compile(resolved);

    let start = writer.len();
    writer.call(0, 0); // call first method todo: lookup given function
    writer.rustcall(ItsyFn::print);
    writer.exit();

    VM::new(writer.into_program(), start)
}

fn main() {

    let source = "
fn wrapper() -> i32 {
    print(fib(22));
    hello_world();
}
fn fib(n: i32) -> i32 {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}
    ";

    use frontend::{parse, resolve};
    use bytecode::{compile, Standalone};

    let parsed = parse(source).unwrap();
    //println!("{:#?}", parsed);
    let resolved = resolve::<ItsyFn>(parsed);
    //println!("{:#?}", resolved.ast);
    //println!("{:#?}", resolved.types);
    let mut writer = compile(resolved);

    // create entry point, call fib
    let start = writer.len();
    writer.call(0, 0); // call first method
    writer.exit();

    {
        // initialize vm

        let mut vm = bytecode::VM::new(writer.into_program(), start);

        // dump bytecode, run some instructions and dump them as we go

        println!("--- full program dump ---");
        println!("{:}", vm.dump_program());
/*
        println!("--- tracing first few executed instructions ---");
        use std::io::Read;
        let mut cont = false;
        let mut input = 0;
        for _ in 0..1500 {
            if !cont {
                input = ::std::io::stdin().bytes().next().and_then(|result| result.ok()).unwrap();
            }
            if input == 32 {
                cont = true;
            }
            if cont || input == 13 {
                println!("{}", vm.dump_instruction().unwrap());
                vm.exec();
                println!("frame@{}: {}", vm.fp, vm.dump_frame());
                if vm.state != bytecode::VMState::Continue {
                    break;
                }
            }
        }

        if vm.state == bytecode::VMState::Continue {
            println!("...many more...\n");
        }

        vm.reset();
*/
        // time and run vm

        println!("--- untraced run ---");
        let vm_start = ::std::time::Instant::now();
        vm.run();
        let vm_runtime = ::std::time::Instant::now() - vm_start;
        let vm_runtime = vm_runtime.as_secs() as f64 + vm_runtime.subsec_nanos() as f64 * 1e-9;
        println!("vm time: {:.4}s", vm_runtime);
/*
        let rust_start = ::std::time::Instant::now();
        println!("\nprint: {}", wrapper());
        let rust_runtime = ::std::time::Instant::now() - rust_start;
        let rust_runtime = rust_runtime.as_secs() as f64 + rust_runtime.subsec_nanos() as f64 * 1e-9;
        println!("rust time: {:.4}s", rust_runtime);
        println!("\nfactor: {:.4}s", vm_runtime / rust_runtime);
*/
    }

}
