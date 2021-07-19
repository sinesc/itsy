use itsy::*;

vm_func!(MyFns, (), {
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
    let source = std::fs::read_to_string("itsy/test.itsy").unwrap();
    match build(&source) {
        Ok(program) => {
            let mut vm = runtime::VM::new(&program);
            log("logs/bytecode.ini", false, &vm.format_program());
            log("logs/run.ini", false, "");
            loop {
                log("logs/run.ini", true, &format!("{}", &vm.format_instruction().unwrap_or("-".to_string())));
                let vmstate = vm.step(&mut ());
                log("logs/run.ini", true, &format!(";    stack {:?}\n;    cnt   {:?}\n;    heap  {:?}", vm.stack.frame(), vm.cnt.frame(), vm.heap.data()));
                if vmstate != runtime::VMState::Ready {
                    break;
                }
            }
        }
        Err(err) => {
            let loc =  err.loc(&source);
            println!("Error: {} in line {}, column {}.", err, loc.0, loc.1);
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

fn build(source: &str) -> Result<compiler::Program<MyFns>, Error> {
    let parsed = parser::parse(source)?;
    let resolved = resolver::resolve::<MyFns>(parsed, "main")?;
    log("logs/ast.c", false, &format!("{:?}", resolved.ast));
    Ok(compiler::compile(resolved)?)
}