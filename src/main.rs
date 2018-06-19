#[macro_use]
extern crate nom;

mod ast;
mod parser;
use parser::*;

use nom::types::CompleteStr as Input;

fn main() {
    let tests = [
        "fn hello(name: u8, mut greeting: u16) -> u8 { let x = 1; placeholder }",
        "fn hello(name: u8, mut greeting: u16) -> u8 { let x = 1 + 9; 7 +9 * 6- 8 }",
        "struct mystruct { }",
        "struct mystruct { name: string }",
        "struct mystruct { name: string, count: u8 }",
        "if a { let x = 1; }",
        "let v = if a { let x = 1; x.y.z } else { let x = 2; x };",
        "let v = if a.lala.dud { let x = 1; x } else if b { let x = 2.1; x } else { let x = 3; x };",
        "let x =1;",
        "fn add(a: u32, b: u32) -> u32 {
            let result = a + b;
            result
        }
        let added = add(1, 1);",
        "a += b + c;"
    ];

    println!("Errors:");

    for test in tests.iter() {
        if let Err(err) = parse(Input(test)) {
            println!("{:?}", err);
        }
    }

    let final_test = parse(Input(tests[tests.len() - 1]));

    if let Ok(final_test) = final_test {
        println!("\nParsed: {:?}", final_test.1);
        println!("Remaining: {:?}", final_test.0);
    }
}
