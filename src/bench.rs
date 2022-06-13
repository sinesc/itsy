use itsy::*;
use std::{io::{self, Write}, time::Instant};

struct Bench {
    time: Option<Instant>,
}

itsy_api!(BenchFn, Bench, {
    fn print(&mut context, value: &str) {
        print!("{}", value);
        io::stdout().flush().unwrap();
    }
    fn println(&mut context, value: &str) {
        println!("{}", value);
    }
    fn start_time(&mut context) {
        context.time = Some(Instant::now());
    }
    fn stop_time(&mut context) -> f64 {
        let elapsed = context.time.expect("time started").elapsed();
        elapsed.as_secs_f64()
    }
    fn rust_fib_r(&mut context, n: i32) -> i32 {
        fib_r(n)
    }
    fn rust_mandelbrot(&mut context, columns: u32, rows: u32) -> String {
        mandelbrot(columns, rows)
    }
    fn rust_stringcat(&mut context, n: i32) -> String {
        stringcat(n)
    }
});

fn main() {
    println!("Ballpark Bench™ - Measuring in approximate baseball fields");
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

fn mandelbrot(columns: u32, rows: u32) -> String {

    let chars = [ ".", ",", "`", "´", "'", "~", "^", "°", "$", ";", "=", "o", "O", "%", "&", ":" ];
    let mut result = "".to_string();
    let max_x = columns as f32;
    let max_y = rows as f32;
    let mut y = -1.4f32;

    for _ in 0..rows {
        let mut x = -2.0f32;
        let mut line = "".to_string();
        for _ in 0..columns {
            let mut r = 0f32;
            let mut i = 0f32;
            let mut n = 0;
            while n < 16 && r * r + i * i <= 4.0 {
                r = r * r - i * i + x;
                i = 2.0 * r * i + y;
                n += 1;
            }
            line += chars[n-1];
            x += 3.6 / max_x;
        }
        result += &(line + "\n");
        y += 2.81 / max_y;
    }

    result
}

fn stringcat(n: i32) -> String {
    let mut s = n.to_string();
    if n > 0 {
        s += &stringcat(n - 1);
    }
    s
}