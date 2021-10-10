use itsy::*;
use std::time::Instant;

struct Bench {
    time: Option<Instant>,
}

vm_func!(BenchFn, Bench, {
    fn start_time(&mut context) {
        context.time = Some(Instant::now());
    }
    fn stop_time(&mut context) -> f64 {
        let elapsed = context.time.expect("time started").elapsed();
        elapsed.as_secs_f64()
    }
    fn print(&mut context, value: &str) {
        println!("{}", value);
    }
    fn rust_fib_r(&mut context, n: i32) -> i32 {
        fib_r(n)
    }
    fn rust_fib_i(&mut context, n: i32) -> i32 {
        fib_i(n)
    }
});

fn main() {
    println!("Ballpark Bench™ - As accurate as football field units");
    let source = std::fs::read_to_string("itsy/bench/main.itsy").unwrap();
    let mut context = Bench { time: None };
    match build(&source) {
        Ok(program) => {
            let mut vm = runtime::VM::new(&program);
            vm.run(&mut context);
        }
        Err(err) => {
            let loc =  err.loc(&source);
            println!("{} in line {}, column {}.", err, loc.0, loc.1);
        }
    }
}
fn build(source: &str) -> Result<compiler::Program<BenchFn>, Error> {
    let parsed = parser::parse_module(source, "")?;
    let mut program = parser::ParsedProgram::new();
    program.add_module(parsed);
    let resolved = resolver::resolve::<BenchFn>(program, "main")?;
    Ok(compiler::compile(resolved)?)
}
fn fib_r(n: i32) -> i32 {
    if n < 2 {
        n
    } else {
        fib_r(n - 1) + fib_r(n - 2)
    }
}

fn fib_i(n: i32) -> i32 {

    let mut i = 0;
    let mut j = 1;
    let mut k = 1;
    let mut t;

    if n == 0 {
       return 0;
    }

    while k < n {
        t = i + j;
        i = j;
        j = t;
        k += 1;
    }

    return j;
}