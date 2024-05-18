use itsy::*;
use itsy::internals::binary::OpCode;
use std::{env, time::{Duration, Instant}};

/*
 * Opcode profiler
 *
 * Usage from project root e.g. target/release/examples/profile-run itsy/examples/mandelbrot.itsy
 */

mod shared;
use shared::{Context, MyAPI};

fn main() {
    shared::enable_virtual_terminal_processing();
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("usage: debug-run <filename>");
    } else {
        match build::<MyAPI, _>(&args[1]) {
            Ok(program) => {
                run(program, &args[2..]);
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
}

fn run(program: Program<MyAPI>, args: &[ String ]) {
    let mut context = Context::new(args);
    let mut vm = runtime::VM::new(program);
    let mut opcode_stats = Vec::new();
    for _ in 0..=OpCode::MAX {
        opcode_stats.push((0, Duration::ZERO));
    }

    let vm_start_time = Instant::now();
    loop {
        // time single step
        let start_time = Instant::now();
        let current_opcode = vm.get_instruction();
        vm.step(&mut context).unwrap();
        let elapsed_time = Instant::now() - start_time;
        let stats = &mut opcode_stats[current_opcode.unwrap() as usize];
        stats.0 += 1;
        stats.1 += elapsed_time;

        // check if vm is finished
        if vm.state() != runtime::VMState::Ready {
            break;
        }
    }

    let vm_elapsed_time = Instant::now() - vm_start_time;
    let mut stats: Vec<(_, (_, _))> = opcode_stats.into_iter().enumerate().filter(|(_, (c, _))| *c > 0).collect();
    stats.sort_by(|a, b| a.1.1.cmp(&b.1.1));

    println!("\n{: >22} {: >12}   {: >12}   {: >12}", "OpCode", "Count", "Time/Op", "Total");
    for &(opcode, (count, time)) in &stats {
        println!("{: >22} {: >12} {: >12.4}ns {: >12.4}ms", format!("{:?}", OpCode::try_from(opcode as u16).unwrap()), count, time.as_secs_f64() / count as f64 * 1_000_000.0, time.as_secs_f64() * 1000.0);
    }
    println!("{: >22} {: >12} {: >12.4}ns {: >12.4}ms", "Totals", stats.iter().fold(0, |acc, c| acc + c.1.0), "-", stats.iter().fold(Duration::ZERO, |acc, c| acc + c.1.1).as_secs_f64() * 1000.0);
    println!("{: >22} {: >12} {: >12.4}ns {: >12.4}ms", "All (incl. overhead)", "-", "-", vm_elapsed_time.as_secs_f64() * 1000.0);
}