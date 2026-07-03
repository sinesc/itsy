//! Temporary helper: extract unique opcode names from benchmark cases.
//!
//! Usage:
//!   cargo run --example extract-opcodes --features="compiler,runtime,debugging,symbols" \
//!     itsy/bench/cases/loops/for_range.itsy
//!
//! Prints one opcode name per line (sorted, unique).

use itsy::{build_str, runtime, itsy_api};
use std::collections::BTreeSet;
use std::env;
use std::fs;
use std::path::Path;
use std::io::Write;

itsy_api! {
    BenchAPI<()> {
        fn print(&mut _context, value: String) {
            print!("{}", value);
            let _ = std::io::stdout().flush();
        }
        fn println(&mut _context, value: String) {
            println!("{}", value);
        }
        fn flush(&mut _context) {}
        fn random(&mut _context) -> f64 { 0.5 }
        fn exit(&mut _context, code: i32) {
            std::process::exit(code);
        }
        fn runtime(&mut _context) -> u64 { 0 }
        fn sleep(&mut _context, _milliseconds: u64) {}
        fn wait_frame(&mut _context, _milliseconds: u64) -> u64 { 0 }
        fn get_arg(&mut _context, _arg: u64) -> String { String::new() }
        fn has_arg(&mut _context, _arg: String) -> bool { false }
        fn fastbrot(&mut _context, depth: u32, x: f64, y: f64) -> u32 {
            let mut n = 0;
            let mut r = 0.0f64;
            let mut i = 0.0f64;
            while n < depth && 4.0 > r * r + i * i {
                let i_tmp = i * i;
                i = r * i * 2.0 + y;
                r = r * r - i_tmp + x;
                n += 1;
            }
            n
        }
        fn bench_iterations(&mut _context) -> u64 { 1 }
    }
}

fn assemble_source(case_source: &str, has_setup: bool) -> String {
    let (setup_line, bench_call) = if has_setup {
        ("    let state = setup();\n", "bench(state);")
    } else {
        ("", "bench();")
    };
    format!(
        "use BenchAPI::{{ bench_iterations }};\n\
         {case_source}\n\
         fn main() {{\n\
         {setup_line}\
         \x20   while true {{\n\
         \x20       suspend;\n\
         \x20       let n = bench_iterations();\n\
         \x20       for _ in 0..n {{\n\
         \x20           {bench_call}\n\
         \x20       }}\n\
         \x20   }}\n\
         }}\n"
    )
}

fn defines_setup(source: &str) -> bool {
    source.lines().any(|line| {
        let trimmed = line.trim_start();
        trimmed.strip_prefix("fn setup").map_or(false, |rest| {
            rest.chars().next().map_or(true, |c| c == '(' || c.is_whitespace())
        })
    })
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("usage: extract-opcodes <file.itsy> [<file2.itsy> ...]");
        std::process::exit(1);
    }

    let mut all_opcodes: BTreeSet<String> = BTreeSet::new();

    for arg in &args {
        let path = Path::new(arg);
        let case_source = fs::read_to_string(path).unwrap_or_else(|e| {
            eprintln!("Cannot read {}: {}", path.display(), e);
            std::process::exit(1);
        });

        let source = assemble_source(&case_source, defines_setup(&case_source));

        let program = build_str::<BenchAPI>(&source).unwrap_or_else(|e| {
            eprintln!("Cannot compile {}: {}", path.display(), e);
            std::process::exit(1);
        });

        let vm = runtime::VM::new(program);
        let raw = vm.format_program();

        let mut local_opcodes: BTreeSet<String> = BTreeSet::new();
        for line in raw.lines() {
            // Extract opcode name from line like "14 call 20 8 "
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 2 {
                // parts[0] is position, parts[1] is opcode name (may have Debug formatting)
                let op = parts[1].trim_matches(|c: char| c == '"' || c == ' ');
                local_opcodes.insert(op.to_string());
            }
        }

        println!("=== {} ===", path.display());
        for op in &local_opcodes {
            println!("  {}", op);
        }
        all_opcodes.extend(local_opcodes);
    }

    println!("\n=== ALL COVERAGE ===");
    for op in &all_opcodes {
        println!("{}", op);
    }
    println!("\nTotal unique opcodes: {}", all_opcodes.len());
}
