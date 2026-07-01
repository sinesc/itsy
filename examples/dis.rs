use itsy::*;
use std::collections::{BTreeSet, HashMap};
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
 *   cargo run --example dis --features="compiler,runtime,debugging,symbols,comments" -- itsy/examples/helloworld.itsy --raw-offsets
 */

mod shared;
use shared::MyAPI;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.is_empty() {
        eprintln!("usage: dis <filename.itsy> [--raw-offsets] [-o <output-file>]");
        std::process::exit(1);
    }

    let source_file = &args[0];
    let raw_offsets = args.iter().any(|a| a == "--raw-offsets");
    let output_file: Option<&str> = args
        .iter()
        .position(|a| a == "-o")
        .and_then(|pos| args.get(pos + 1))
        .map(|s| s.as_str());

    match build::<MyAPI, _>(source_file) {
        Ok(program) => {
            let vm = runtime::VM::new(program);
            let raw = vm.format_program();
            let disassembly = format_disassembly(&raw, raw_offsets);

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
///
/// When `raw_offsets` is `true`, all jump targets are shown as absolute
/// bytecode offsets regardless of whether function names are available.
fn format_disassembly(raw: &str, raw_offsets: bool) -> String {
    let lines: Vec<&str> = raw.lines().collect();

    // Pass 1: collect all jump targets and function-name mappings.
    let mut targets: BTreeSet<usize> = BTreeSet::new();
    let mut fn_names: HashMap<usize, String> = HashMap::new();
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
        // Parse function declarations from comment lines (comments feature).
        if let Some((pos, name)) = parse_fn_decl(line) {
            fn_names.insert(pos, name);
        }
    }

    // Pass 2: format output.
    let mut out = String::new();
    for line in &lines {
        if let Some((position, opcode, args)) = parse_instruction_line(line) {
            // Print offset label if this position is a jump target.
            if targets.contains(&position) {
                let label = format_offset(position, &fn_names, raw_offsets);
                out.push_str(&format!("{}:\n", label));
            }
            if args.is_empty() {
                out.push_str(&format!("    {}\n", opcode));
            } else {
                // Replace call/jump target addresses with function names.
                let arg_list: Vec<&str> = args.split_whitespace().collect();
                let formatted_args = replace_jump_target_args(opcode, arg_list, &fn_names, raw_offsets);
                out.push_str(&format!("    {} {}\n", opcode, formatted_args));
            }
        } else if let Some((position, comment_text)) = parse_comment_line_with_pos(line) {
            // Print offset label if this comment position is a jump target.
            if targets.contains(&position) {
                let label = format_offset(position, &fn_names, raw_offsets);
                out.push_str(&format!("{}:\n", label));
            }
            out.push_str(&format!("    ;{}\n", comment_text));
        }
    }

    out
}

/// Format a bytecode offset for display.
///
/// If the offset is a function entry point, returns the function name.
/// If the offset is inside a function, returns `name+offset` relative to
/// the containing function start.  Falls back to the raw offset if no
/// containing function is known (e.g. comments feature disabled).
/// If `raw_offsets` is `true`, always returns the raw absolute offset.
fn format_offset(offset: usize, fn_names: &HashMap<usize, String>, raw_offsets: bool) -> String {
    if raw_offsets {
        return format!("{}", offset);
    }
    // If this offset IS a function entry, return the function name.
    if let Some(name) = fn_names.get(&offset) {
        return name.clone();
    }

    // Find the containing function (largest start position <= offset).
    let mut containing_start: Option<usize> = None;
    let mut containing_name: Option<String> = None;
    for (&start, name) in fn_names {
        if start <= offset {
            match containing_start {
                None => {
                    containing_start = Some(start);
                    containing_name = Some(name.clone());
                }
                Some(current) if start > current => {
                    containing_start = Some(start);
                    containing_name = Some(name.clone());
                }
                _ => {}
            }
        }
    }

    if let (Some(start), Some(name)) = (containing_start, containing_name) {
        let rel = offset - start;
        format!("{}+{}", name, rel)
    } else {
        format!("{}", offset)
    }
}

/// Replace jump/call target addresses in instruction arguments with
/// formatted labels.  For `JUMP_OPS` the first argument is the target;
/// for `JUMP_OPS_TAIL` the last argument is the target.
/// When `raw_offsets` is `true`, target addresses are left as raw offsets.
fn replace_jump_target_args(opcode: &str, args: Vec<&str>, fn_names: &HashMap<usize, String>, raw_offsets: bool) -> String {
    if JUMP_OPS.contains(&opcode) {
        // First argument is the target address.
        let mut new_args: Vec<String> = Vec::with_capacity(args.len());
        let mut first = true;
        for arg in args {
            if first {
                first = false;
                if let Ok(addr) = arg.parse::<usize>() {
                    new_args.push(format_offset(addr, fn_names, raw_offsets));
                    continue;
                }
            }
            new_args.push(arg.to_string());
        }
        new_args.join(" ")
    } else if JUMP_OPS_TAIL.contains(&opcode) {
        // Last argument is the target address.
        let mut new_args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
        if let Some(last) = new_args.last_mut() {
            if let Ok(addr) = last.parse::<usize>() {
                *last = format_offset(addr, fn_names, raw_offsets);
            }
        }
        new_args.join(" ")
    } else {
        args.join(" ")
    }
}

/// Try to parse a function declaration from a comment line.
///
/// Comment lines from the `comments` feature have the format
/// `;{pos} fn {name}`.  Returns `(position, name)` if this is a
/// function declaration, `None` otherwise.
#[cfg(feature = "comments")]
fn parse_fn_decl(line: &str) -> Option<(usize, String)> {
    let trimmed = line.trim_start_matches('\n').trim();
    if !trimmed.starts_with(';') {
        return None;
    }
    let after_semicolon = &trimmed[1..];
    // Comments from `comments` feature have the format ";<pos> <text>".
    let first_space = after_semicolon.find(' ')?;
    let pos_str = &after_semicolon[..first_space];
    let pos = pos_str.parse::<usize>().ok()?;
    let text = &after_semicolon[first_space + 1..];
    // Function declarations look like "fn name" or "fn name(...)".
    if let Some(name_part) = text.strip_prefix("fn ") {
        // Extract just the function name (before any '(' or '<').
        let name = name_part
            .chars()
            .take_while(|c| !matches!(c, '(' | '<'))
            .collect::<String>();
        if !name.is_empty() {
            return Some((pos, name));
        }
    }
    None
}

#[cfg(not(feature = "comments"))]
fn parse_fn_decl(_line: &str) -> Option<(usize, String)> {
    None
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
