use itsy::*;
use std::env;

/*
 * Step debugger for Itsy bytecode.
 *
 * Runs an .itsy source file one instruction at a time and stops after a
 * configurable number of steps or when a runtime error is encountered.
 * After stopping it prints the last N instructions, the total step count,
 * the active heap reference indices and reference count and optionally dumps
 * the stack and/or selected heap items.
 *
 * Usage:
 *   cargo run --example debug-step --features="compiler,runtime,debugging,symbols" -- itsy/examples/helloworld.itsy
 *   cargo run --example debug-step --features="compiler,runtime,debugging,symbols" -- itsy/examples/helloworld.itsy --steps 10
 *   cargo run --example debug-step --features="compiler,runtime,debugging,symbols" -- itsy/examples/helloworld.itsy --steps 10 --heap 1 --heap 4
 *   cargo run --example debug-step --features="compiler,runtime,debugging,symbols" -- itsy/examples/helloworld.itsy --steps 10 --stack
 *   cargo run --example debug-step --features="compiler,runtime,debugging,symbols" -- itsy/examples/helloworld.itsy --steps 10 --stack 32
 *   cargo run --example debug-step --features="compiler,runtime,debugging,symbols" -- itsy/examples/helloworld.itsy --steps 10 --instr 10
 *   cargo run --example debug-step --features="compiler,runtime,debugging,symbols" -- itsy/examples/helloworld.itsy --test --steps 10
 */

mod shared;
use shared::MyAPI;

/// Prelude inserted when `--test` is enabled, mirroring the test infrastructure.
const TEST_PRELUDE: &str = "use MyAPI::{ret_u8, ret_u16, ret_u32, ret_u64, ret_i8, ret_i16, ret_i32, ret_i64, ret_f32, ret_f64, ret_bool, ret_string, ret_str};";

/// Wrap source code the same way the test harness does: prepend the prelude
/// and, if there is no `main()` function, wrap the body in one.
fn wrap_test_prelude(source: &str) -> String {
    if source.find("main()").is_some() {
        format!("{} {}", TEST_PRELUDE, source)
    } else {
        format!("{} fn main() {{ {} }}", TEST_PRELUDE, source)
    }
}

struct Args {
    source_file: String,
    test_mode: bool,
    max_steps: Option<usize>,
    heap_indices: Vec<usize>,
    dump_stack: bool,
    stack_bytes: Option<usize>,
    instr_count: usize,
}

fn parse_args() -> Args {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.is_empty() {
        eprintln!("usage: debug-step <filename.itsy> [--test] [--steps <num>] [--heap <index>]... [--stack [<bytes>]] [--instr <num>]");
        std::process::exit(1);
    }

    let mut source_file: Option<String> = None;
    let mut test_mode = false;
    let mut max_steps: Option<usize> = None;
    let mut heap_indices: Vec<usize> = Vec::new();
    let mut dump_stack = false;
    let mut stack_bytes: Option<usize> = None;
    let mut instr_count: usize = 5;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--test" => test_mode = true,
            "--steps" => {
                max_steps = args.get(i + 1)
                    .map(|s| s.parse::<usize>().unwrap_or_else(|_| {
                        eprintln!("error: --steps requires a number, got '{}'", s);
                        std::process::exit(1);
                    }));
                i += 1;
            }
            "--heap" => {
                heap_indices.push(args.get(i + 1)
                    .map(|s| s.parse::<usize>().unwrap_or_else(|_| {
                        eprintln!("error: --heap requires an index, got '{}'", s);
                        std::process::exit(1);
                    }))
                    .unwrap_or_else(|| {
                        eprintln!("error: --heap requires an index");
                        std::process::exit(1);
                    }));
                i += 1;
            }
            "--stack" => {
                dump_stack = true;
                // --stack can be followed by an optional byte count
                if let Some(next) = args.get(i + 1) {
                    // If the next argument looks like a number, treat it as the byte count
                    if next.parse::<usize>().is_ok() {
                        stack_bytes = Some(next.parse::<usize>().unwrap());
                        i += 1;
                    }
                }
            }
            "--instr" => {
                instr_count = args.get(i + 1)
                    .map(|s| s.parse::<usize>().unwrap_or_else(|_| {
                        eprintln!("error: --instr requires a number, got '{}'", s);
                        std::process::exit(1);
                    }))
                    .unwrap_or_else(|| {
                        eprintln!("error: --instr requires a number");
                        std::process::exit(1);
                    });
                i += 1;
            }
            val if val.starts_with('-') => {
                eprintln!("unknown option: {}", val);
                eprintln!("usage: debug-step <filename.itsy> [--test] [--steps <num>] [--heap <index>]... [--stack [<bytes>]] [--instr <num>]");
                std::process::exit(1);
            }
            val => {
                if source_file.is_none() {
                    source_file = Some(val.to_string());
                } else {
                    eprintln!("unexpected argument: {}", val);
                    eprintln!("usage: debug-step <filename.itsy> [--test] [--steps <num>] [--heap <index>]... [--stack [<bytes>]] [--instr <num>]");
                    std::process::exit(1);
                }
            }
        }
        i += 1;
    }

    let source_file = match source_file {
        Some(path) => path,
        None => {
            eprintln!("no source file specified");
            eprintln!("usage: debug-step <filename.itsy> [--test] [--steps <num>] [--heap <index>]... [--stack [<bytes>]] [--instr <num>]");
            std::process::exit(1);
        }
    };

    Args {
        source_file,
        test_mode,
        max_steps,
        heap_indices,
        dump_stack,
        stack_bytes,
        instr_count,
    }
}

fn main() {
    let args = parse_args();

    let program = if args.test_mode {
        let source = std::fs::read_to_string(&args.source_file)
            .unwrap_or_else(|e| { eprintln!("could not read {}: {}", args.source_file, e); std::process::exit(1); });
        let wrapped = wrap_test_prelude(&source);
        build_str::<MyAPI>(&wrapped).unwrap_or_else(|err| {
            eprintln!("{}", err);
            std::process::exit(1);
        })
    } else {
        build::<MyAPI, _>(&args.source_file).unwrap_or_else(|err| {
            eprintln!("{}", err);
            std::process::exit(1);
        })
    };

    let mut context = shared::Context::new(&[]);
    let mut vm = runtime::VM::new(program);

    let mut last_instructions: Vec<String> = Vec::with_capacity(args.instr_count);
    let mut step_count: usize = 0;
    let mut stopped_with_error: Option<runtime::RuntimeError> = None;

    loop {
        // Capture the current instruction before stepping
        let instr = vm.format_instruction().unwrap_or_else(|| "<end of program>".to_string());

        // Step one instruction
        match vm.step(&mut context) {
            Ok(state) => {
                step_count += 1;
                last_instructions.push(instr);
                if last_instructions.len() > args.instr_count {
                    last_instructions.remove(0);
                }

                match state {
                    runtime::VMState::Ready => {
                        // Continue stepping
                        if let Some(max) = args.max_steps {
                            if step_count >= max {
                                break;
                            }
                        }
                    }
                    runtime::VMState::Terminated => {
                        break;
                    }
                    runtime::VMState::Suspended => {
                        break;
                    }
                    runtime::VMState::Error(_) => {
                        unreachable!("step() returns Err on error, not Ok(Error)");
                    }
                }
            }
            Err(e) => {
                step_count += 1;
                last_instructions.push(instr);
                if last_instructions.len() > args.instr_count {
                    last_instructions.remove(0);
                }
                stopped_with_error = Some(e);
                break;
            }
        }
    }

    // Print results
    println!("=== Step debugger ===");
    println!("Total instructions executed: {}", step_count);

    // Print active heap objects (ref count >= 1)
    let active: Vec<String> = vm.heap.data()
        .iter()
        .filter(|&(_, (refs, _))| *refs >= 1)
        .map(|(idx, (refs, _))| format!("{idx}({refs})"))
        .collect();
    if active.is_empty() {
        println!("Active heap objects: (none)");
    } else {
        println!("Active heap objects: {}", active.join(", "));
    }
    println!();

    // Print last N instructions
    println!("--- Last {} instruction(s) ---", last_instructions.len());
    for (i, instr) in last_instructions.iter().enumerate() {
        println!("  [{}] {}", step_count - last_instructions.len() + i + 1, instr);
    }
    println!();

    // Print error if applicable
    if let Some(ref err) = stopped_with_error {
        println!("Runtime error: {}", err);
        println!("Run with --steps {} to stop before this error", step_count - 1);
        println!();
    }

    // Print stack dump if requested
    if args.dump_stack {
        let stack_data = vm.stack.data();
        let frame_start = if stack_data.len() >= vm.stack.fp() as usize + 16 {
            vm.stack.fp() as usize + 16
        } else {
            vm.stack.fp() as usize
        };
        let stack_top = stack_data.len();

        println!("--- Stack dump ---");
        match args.stack_bytes {
            Some(bytes) => {
                let start = if stack_top >= bytes { stack_top - bytes } else { 0 };
                let data = &stack_data[start..stack_top];
                print_hex_dump(data, start as u64);
            }
            None => {
                // Dump current stack frame
                let data = &stack_data[frame_start..stack_top];
                print_hex_dump(data, frame_start as u64);
            }
        }
        println!();
    }

    // Print heap dumps if requested
    if !args.heap_indices.is_empty() {
        println!("--- Heap dump ---");
        for &index in &args.heap_indices {
            let heap_len = vm.heap.len() as usize;
            if index >= heap_len {
                println!("  Heap item {}: (out of bounds, heap has {} items)", index, heap_len);
            } else {
                let item = vm.heap.item(index);
                let refs = vm.heap.item_refs(index);
                let impl_idx = vm.heap.item_implementor_index(index);
                println!("  Heap item {} (refs: {}, impl_idx: {}):", index, refs, impl_idx);
                print_hex_dump(&item.data, 0);
            }
        }
        println!();
    }
}

/// Print a hex dump of the given data, with offset labels.
fn print_hex_dump(data: &[u8], base_offset: u64) {
    let chunk_size = 16;
    for chunk_start in (0..data.len()).step_by(chunk_size) {
        let chunk_end = std::cmp::min(chunk_start + chunk_size, data.len());
        let chunk = &data[chunk_start..chunk_end];

        print!("    {:04x}: ", base_offset + chunk_start as u64);

        // Hex bytes
        for (i, &byte) in chunk.iter().enumerate() {
            if i == chunk_size / 2 {
                print!(" ");
            }
            print!("{:02x} ", byte);
        }
        // Pad to align ASCII section
        let hex_width = chunk.len() * 3 + if chunk.len() > chunk_size / 2 { 1 } else { 0 };
        let padding = chunk_size * 3 + 1 - hex_width;
        print!("{}", " ".repeat(padding));

        // ASCII representation
        print!(" |");
        for &byte in chunk {
            if byte >= 0x20 && byte < 0x7f {
                print!("{}", byte as char);
            } else {
                print!(".");
            }
        }
        println!("|");
    }
}
