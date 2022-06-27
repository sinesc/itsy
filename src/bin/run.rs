use itsy::*;
use std::env;

/*
 * Demo script runner
 *
 * Usage from project root e.g. target/release/run itsy/examples/mandelbrot.itsy
 */

mod shared;
use shared::{Context, MyAPI};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("usage: run <filename>");
    } else {
        match build::<MyAPI, _>(&args[1]) {
            Ok(program) => {
                let mut context = Context { seed: 1.2345 };
                let mut vm = runtime::VM::new(&program);
                vm.run(&mut context);
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}