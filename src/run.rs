use itsy::*;
use std::env;

/*
 * Demo script runner
 *
 * Usage from project root e.g. target/release/run itsy/examples/mandelbrot.itsy
 */

itsy_api!(MyAPI, (), {
    fn print(&mut context, value: String) {
        print!("{}", value);
    }
    fn println(&mut context, value: String) {
        println!("{}", value);
    }
});

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("usage: run <filename>");
    } else {
        match build::<MyAPI, _>(&args[1]) {
            Ok(program) => {
                let mut vm = runtime::VM::new(&program);
                vm.run(&mut ());
            }
            Err(err) => {
                let module_path = err.module_path();
                let loc =  err.loc();
                println!("{} in line {}, column {} in file {}.", err, loc.0, loc.1, module_path);
            }
        }
    }
}