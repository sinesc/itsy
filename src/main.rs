#[macro_use]
extern crate nom;

mod ast;
mod parser;
use parser::*;

use nom::types::CompleteStr as Input;

fn main() {
    let tests = [
        statement(Input("fn hello(name: u8, mut greeting: u16) -> u8 { let x = 1; placeholder }")),
        statement(Input("fn hello(name: u8, mut greeting: u16) -> u8 { let x = 1 + 9; 7 +9 * 6- 8 }")),
        statement(Input("struct mystruct { }")),
        statement(Input("struct mystruct { name: string }")),
        statement(Input("struct mystruct { name: string, count: u8 }")),
        statement(Input("if a { let x = 1; }")),
        statement(Input("let v = if a { let x = 1; x } else { let x = 2; x };")),
        statement(Input("let v = if a { let x = 1; x } else if b { let x = 2.1; x } else { let x = 3; x };")),
    ];

    println!("Errors:");

    for test in tests.iter() {
        if test.is_err() {
            println!("{:?}", test);
        }
    }

    println!("Final test:\n{:?}", tests[tests.len() - 1]);
}
