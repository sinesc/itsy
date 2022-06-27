use itsy::*;
use std::{env, path::PathBuf, collections::HashMap};

/*
 * This binary is a simple debugging tool to run test scripts, log AST and bytecode and trace the VM.
 * To enable logging, create a "logs" directoy parallel to the "src" directory. To disable logging again, simply delete the directory.
 * Note that this script is *really* slow due to single-stepping through the bytecode and optionally writing the logs.
 */

mod shared;
use shared::{Context, MyAPI};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("usage: debug-run <filename>");
    } else {
        let mut files: HashMap<String, (PathBuf, String)> = HashMap::new();
        let write_logs = std::path::Path::new("./logs/").is_dir();
        match build(&args[1], &mut files, write_logs) {
            Ok(program) => {
                let mut context = Context { seed: 1.2345 };
                let mut vm = runtime::VM::new(&program);
                if write_logs {
                    log("logs/bytecode.ini", false, &vm.format_program());
                    log("logs/run.ini", false, "");
                }
                loop {
                    let mut opcode_label = None;
                    if write_logs {
                        opcode_label = Some(vm.format_instruction().unwrap_or("-".to_string()));
                        log("logs/run.ini", true, &format!("{}", opcode_label.as_ref().unwrap()));
                    }
                    let vmstate = vm.step(&mut context);
                    if let Some(instruction_label) = opcode_label {
                        if instruction_label.starts_with("[") == false && instruction_label.starts_with("\n") == false {
                            log("logs/run.ini", true, &format!(";    stack {:?}\n;    heap  {:?}", vm.stack.frame(), vm.heap.data()));
                        }
                    }
                    if vmstate != runtime::VMState::Ready {
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