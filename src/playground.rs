use itsy::*;
use std::{path::PathBuf, collections::HashMap};

/*
    This binary is a simple debugging tool to run test scripts, log AST and bytecode and trace the VM. It runs the code in itsy/playground/main.itsy
    To enable logging, create a "logs" directoy parallel to the "src" directory. To disable logging again, simply delete the directory.
    Note that this script is *really* slow due to single-stepping through the bytecode and optionally writing the logs.
*/

itsy_api!(MyAPI, (), {
    fn printi8(&mut context, value: i8) {
        println!("{}i8", value);
    }
    fn printi16(&mut context, value: i16) {
        println!("{}i16", value);
    }
    fn printi32(&mut context, value: i32) {
        println!("{}i32", value);
    }
    fn printi64(&mut context, value: i64) {
        println!("{}i64", value);
    }
    fn printu8(&mut context, value: u8) {
        println!("{}u8", value);
    }
    fn printu16(&mut context, value: u16) {
        println!("{}u16", value);
    }
    fn printu32(&mut context, value: u32) {
        println!("{}u32", value);
    }
    fn printu64(&mut context, value: u64) {
        println!("{}u64", value);
    }
    fn printf32(&mut context, value: f32) {
        println!("{}f32", value);
    }
    fn printf64(&mut context, value: f64) {
        println!("{}f64", value);
    }
    fn printb(&mut context, value: bool) {
        println!("{}", value);
    }
    fn prints(&mut context, value: &str) {
        println!("&\"{}\"", value);
    }
    fn printss(&mut context, value: String) {
        println!("&\"{}\"", value);
    }
});

fn main() {
    let mut files: HashMap<String, (PathBuf, String)> = HashMap::new();
    let write_logs = std::path::Path::new("./logs/").is_dir();
    match build("itsy/playground/main.itsy", &mut files, write_logs) {
        Ok(program) => {
            let mut vm = runtime::VM::new(&program);
            if write_logs {
                log("logs/bytecode.ini", false, &vm.format_program());
                log("logs/run.ini", false, "");
            }
            loop {
                let mut instruction = None;
                if write_logs {
                    instruction = Some(vm.format_instruction().unwrap_or("-".to_string()));
                    log("logs/run.ini", true, &format!("{}", instruction.as_ref().unwrap()));
                }
                let vmstate = vm.step(&mut ());
                if let Some(instruction) = instruction {
                    if instruction.starts_with("[") == false && instruction.starts_with("\n") == false {
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
            println!("{} in line {}, column {} in file {}.", err, loc.0, loc.1, files[module_path].0.to_str().unwrap());
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
    let resolved = resolver::resolve::<MyAPI>(parsed, "main")?;
    if write_logs {
        log("logs/ast.c", false, &format!("{:?}", resolved.modules));
    }
    Ok(compiler::compile(resolved)?)
}