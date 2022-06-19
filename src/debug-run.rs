use itsy::*;
use std::{env, path::PathBuf, collections::HashMap, io::{self, Write}};

/*
 * This binary is a simple debugging tool to run test scripts, log AST and bytecode and trace the VM.
 * To enable logging, create a "logs" directoy parallel to the "src" directory. To disable logging again, simply delete the directory.
 * Note that this script is *really* slow due to single-stepping through the bytecode and optionally writing the logs.
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
        let mut files: HashMap<String, (PathBuf, String)> = HashMap::new();
        let write_logs = std::path::Path::new("./logs/").is_dir();
        let mut instruction_counts = HashMap::new();
        match build(&args[1], &mut files, write_logs) {
            Ok(program) => {
                let mut context = Context { seed: 1.2345 };
                let mut vm = runtime::VM::new(&program);
                if write_logs {
                    log("logs/bytecode.ini", false, &vm.format_program());
                    log("logs/run.ini", false, "");
                }
                loop {
                    let mut instruction = None;
                    let count = instruction_counts.entry(vm.get_instruction()).or_insert(0);
                    *count += 1;
                    if write_logs {
                        instruction = Some(vm.format_instruction().unwrap_or("-".to_string()));
                        log("logs/run.ini", true, &format!("{}", instruction.as_ref().unwrap()));
                    }
                    let vmstate = vm.step(&mut context);
                    if let Some(instruction) = instruction {
                        if instruction.starts_with("[") == false && instruction.starts_with("\n") == false {
                            log("logs/run.ini", true, &format!(";    stack {:?}\n;    heap  {:?}", vm.stack.frame(), vm.heap.data()));
                        }
                    }
                    if vmstate != runtime::VMState::Ready {
                        let mut counts: Vec<(_, _)> = instruction_counts.into_iter().collect();
                        counts.sort_by(|a, b| a.1.cmp(&b.1));
                        println!("OpCode counts");
                        for (opcode, v) in &counts {
                            let opcode = format!("{:?}", opcode.unwrap());
                            println!("{: >22} {}", opcode, v);
                        }
                        break;
                    }
                }
            }
            Err(err) => {
                let module_path = err.module_path();
                let loc =  err.loc(&files[module_path].1);
                println!("{} in line {}, column {} in file {}", err, loc.0, loc.1, files[module_path].0.to_string_lossy());
            }
        }
    }
}

fn log(filename: &str, append: bool, data: &str) {
    use std::io::prelude::*;
    if append {
        let mut file = std::fs::OpenOptions::new().append(true).open(filename).unwrap();
        writeln!(file, "{}", data).unwrap();
    } else {
        std::fs::write(filename, data).unwrap();
    }
}

fn build<P: AsRef<std::path::Path>>(source_file: P, files: &mut HashMap<String, (PathBuf, String)>, write_logs: bool) -> Result<compiler::Program<MyAPI>, Error> {
    let source_file = source_file.as_ref();
    let parsed = parser::parse(|module_path| {
        let filename = parser::module_filename(source_file, module_path);
        let file = std::fs::read_to_string(&filename).unwrap();
        let module = parser::parse_module(&file, module_path);
        files.insert(module_path.to_string(), (filename, file));
        module
    })?;
    if write_logs {
        log("logs/ast.c", false, &format!("{:?}", parsed.modules().collect::<Vec<_>>()));
    }
    let resolved = resolver::resolve::<MyAPI>(parsed, "main")?;
    if write_logs {
        log("logs/ast.c", false, &format!("{:?}", resolved.modules));
    }
    Ok(compiler::compile(resolved)?)
}