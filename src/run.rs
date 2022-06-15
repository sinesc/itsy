use itsy::*;
use std::{env, io::{self, Write}};

/*
 * Demo script runner
 *
 * Usage from project root e.g. target/release/run itsy/examples/mandelbrot.itsy
 */

itsy_api!(MyAPI, (), {
    fn print(&mut context, value: String) {
        print!("{}", value);
        io::stdout().flush().unwrap();
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
                println!("{}", err);
            }
        }
    }
}