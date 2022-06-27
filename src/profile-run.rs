use itsy::*;
use std::{env, collections::HashMap, io::{self, Write}};

/*
 * Opcode profiler
 *
 * Usage from project root e.g. target/release/profile-run itsy/examples/mandelbrot.itsy
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
        println!("usage: debug-run <filename>");
    } else {
        let mut opcode_stats = HashMap::new();
        match build::<MyAPI, _>(&args[1]) {
            Ok(program) => {
                let mut context = Context { seed: 1.2345 };
                let mut vm = runtime::VM::new(&program);
                loop {
                    let vmstate = {
                        let start_time = std::time::Instant::now();
                        let current_opcode = vm.get_instruction();
                        let vmstate = vm.step(&mut context);
                        let elapsed_time = std::time::Instant::now() - start_time;
                        let stats = opcode_stats.entry(current_opcode).or_insert((0, std::time::Duration::ZERO));
                        stats.0 += 1;
                        stats.1 += elapsed_time;
                        vmstate
                    };
                    if vmstate != runtime::VMState::Ready {
                        let mut counts: Vec<(_, _)> = opcode_stats.into_iter().collect();
                        counts.sort_by(|a, b| a.1.1.cmp(&b.1.1));
                        println!("\n{: >22} {: >12}   {: >12}   {: >12}", "OpCode", "Count", "Time/Op", "Total");
                        for (opcode, (count, time)) in counts {
                            println!("{: >22} {: >12} {: >12.4}ns {: >12.4}ms", format!("{:?}", opcode.unwrap()), count, time.as_secs_f64() / count as f64 * 1_000_000.0, time.as_secs_f64() * 1000.0);
                        }
                        break;
                    }
                }
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}
