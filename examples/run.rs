use itsy::*;
use std::env;

/*
 * Demo script runner
 *
 * Usage from project root e.g. target/release/examples/run itsy/examples/mandelbrot.itsy
 */

mod shared;
use shared::{Context, MyAPI};

fn run(program: Program<MyAPI>) {
    let mut context = Context { seed: 1.2345 };
    let mut vm = runtime::VM::new(&program);
    vm.run(&mut context);
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("usage: run <filename>");
    } else {
        match build::<MyAPI, _>(&args[1]) {
            Ok(program) => {
                run(program);
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}