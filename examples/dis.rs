use itsy::*;
use std::collections::BTreeSet;
use std::env;
use std::io::Write;

/*
 * Disassembler for Itsy bytecode.
 *
 * Compiles an .itsy source file and prints a readable disassembly of the
 * resulting bytecode.  Offset labels are shown only at jump targets
 * (jmp, j0, jn0, j0_nc, jn0_nc, j0sa_nc, jn0sa_nc, call, call_builtinx,
 * call_virtual, loops*, loopu*, array_iter*).  Use the `comments` feature for source-level
 * annotations.
 *
 * Usage:
 *   cargo run --example dis --features="compiler,runtime,debugging,symbols" -- itsy/examples/helloworld.itsy
 *   cargo run --example dis --features="compiler,runtime,debugging,symbols,comments" -- itsy/examples/helloworld.itsy
 *   cargo run --example dis --features="compiler,runtime,debugging,symbols" -- itsy/examples/helloworld.itsy -o out.txt
 */

mod shared;
use shared::MyAPI;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.is_empty() {
        eprintln!("usage: dis <filename.itsy> [-o <output-file>]");
        std::process::exit(1);
    }

    let source_file = &args[0];
    let output_file: Option<&str> = if args.len() >= 3 && args[1] == "-o" {
        Some(&args[2])
    } else {
        None
    };

    match build::<MyAPI, _>(source_file) {
        Ok(program) => {
            let vm = runtime::VM::new(program);
            let raw = vm.format_program();
            let disassembly = format_disassembly(&raw);

            match output_file {
                Some(path) => {
                    let mut file = std::fs::File::create(path).expect("could not create output file");
                    file.write_all(disassembly.as_bytes()).expect("could not write output file");
                }
                None => {
                    print!("{}", disassembly);
                }
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}

/// Opcodes whose first address argument is a jump/call target.
const JUMP_OPS: &[&str] = &[
    "jmp", "j0", "jn0", "j0_nc", "jn0_nc", "j0sa_nc", "jn0sa_nc",
    "call", "call_builtinx", "call_virtual",
];

/// Opcodes whose **last** address argument is a jump target.
const JUMP_OPS_TAIL: &[&str] = &[
    "loops8", "loops16", "loops32", "loops64",
    "loopu8", "loopu16", "loopu32", "loopu64",
    "array_iter8", "array_iter16", "array_iter32", "array_iter64",
    "array_iter_iv8", "array_iter_iv16", "array_iter_iv32", "array_iter_iv64",
];

/// Reformat raw disassembly (from `VM::format_program`) into a readable
/// version that only shows offset labels at jump targets.
fn format_disassembly(raw: &str) -> String {
    let lines: Vec<&str> = raw.lines().collect();

    // Pass 1: collect all jump targets.
    let mut targets: BTreeSet<usize> = BTreeSet::new();
    for line in &lines {
        if let Some((_, opcode, rest)) = parse_instruction_line(line) {
            let args: Vec<&str> = rest.split_whitespace().collect();
            if JUMP_OPS.contains(&opcode) {
                // First argument is the target.
                if let Some(addr_str) = args.first() {
                    if let Ok(addr) = addr_str.parse::<usize>() {
                        targets.insert(addr);
                    }
                }
            } else if JUMP_OPS_TAIL.contains(&opcode) {
                // Last argument is the `exit` target.
                if let Some(addr_str) = args.last() {
                    if let Ok(addr) = addr_str.parse::<usize>() {
                        targets.insert(addr);
                    }
                }
            }
        }
    }

    // Pass 2: format output.
    let mut out = String::new();
    for line in &lines {
        if let Some((position, opcode, args)) = parse_instruction_line(line) {
            // Print offset label if this position is a jump target.
            if targets.contains(&position) {
                out.push_str(&format!("{}:\n", position));
            }
            if args.is_empty() {
                out.push_str(&format!("    {}\n", opcode));
            } else {
                out.push_str(&format!("    {} {}\n", opcode, args));
            }
        } else if let Some((position, comment_text)) = parse_comment_line_with_pos(line) {
            // Print offset label if this comment position is a jump target.
            if targets.contains(&position) {
                out.push_str(&format!("{}:\n", position));
            }
            out.push_str(&format!("    ;{}\n", comment_text));
        }
    }

    out
}

/// Parse an instruction line like "14 call 20 8 " into (position, opcode, args).
/// Returns None if the line is not an instruction (e.g. a comment).
fn parse_instruction_line(line: &str) -> Option<(usize, &str, &str)> {
    let trimmed = line.trim();
    let first_space = trimmed.find(' ')?;
    let position_str = &trimmed[..first_space];
    let position = position_str.parse::<usize>().ok()?;
    let rest = &trimmed[first_space + 1..];
    // Opcodes may have no arguments (e.g. `exit`).
    let (opcode, args) = rest.split_once(' ').unwrap_or((rest, ""));
    Some((position, opcode.trim_end(), args.trim_end()))
}

/// Parse a comment line like ";14 fn main" or "\n;14 fn main" into the
/// (position, comment_text) pair.  The position is extracted from the comment
/// so we can match it against jump targets.  Returns `None` if the line is not
/// a comment.
fn parse_comment_line_with_pos(line: &str) -> Option<(usize, &str)> {
    let trimmed = line.trim_start_matches('\n').trim();
    if !trimmed.starts_with(';') {
        return None;
    }
    let after_semicolon = &trimmed[1..];
    // Comments from `comments` feature have the format ";<pos> <text>".
    if let Some(first_space) = after_semicolon.find(' ') {
        let pos_str = &after_semicolon[..first_space];
        if let Ok(pos) = pos_str.parse::<usize>() {
            return Some((pos, &after_semicolon[first_space + 1..]));
        }
    }
    // No position number (e.g. a bare ";comment") — return position 0 as a fallback.
    Some((0, after_semicolon))
}
