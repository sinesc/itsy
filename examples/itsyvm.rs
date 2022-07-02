use itsy::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

/*
 * Demo script runtime
 *
 * Usage from project root e.g.
 *      target/release/examples/itsyc itsy/examples/mandelbrot.itsy mandelbrot.bin  (to get a binary)
 *      target/release/examples/itsyvm mandelbrot.bin
 */

mod shared;
use shared::{Context, MyAPI};

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("usage: itsyvm <filename>");
    } else {
        let mut file = File::open(&args[1])?;
        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;
        match Program::<MyAPI>::from_bytes(&contents) {
            Some(program) => {
                let mut context = Context { seed: 1.2345 };
                let mut vm = runtime::VM::new(&program);
                vm.run(&mut context);
            }
            None => {
                println!("Error deserializing program");
            }
        }
    }
    Ok(())
}