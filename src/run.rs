use itsy::*;
use std::{env, io::{self, Write}};

/*
 * Demo script runner
 *
 * Usage from project root e.g. target/release/run itsy/examples/mandelbrot.itsy
 */

struct Context {
    seed: f64,
}

itsy_api!(MyAPI, Context, {
    /// Prints the given string to standard output.
    fn print(&mut context, value: String) {
        print!("{}", value);
        io::stdout().flush().unwrap();
    }
    /// Prints the given string followed by a newline to standard output.
    fn println(&mut context, value: String) {
        println!("{}", value);
    }
    /// Returns a random number between 0.0 and non-inclusive 1.0
    fn random(&mut context) -> f64 {
        context.seed += 1.0;
        let large = context.seed.sin() * 100000000.0;
        large - large.floor()
    }
});

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