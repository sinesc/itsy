#[macro_use]
extern crate nom;

mod ast;
mod parser;
use parser::*;

use nom::types::CompleteStr as Input;

fn main() {
/*
    println!("{:?}", statement(Input("fn hello(name: u8, mut greeting: u16) -> u8 { let x = 1; placeholder }")));
    println!("{:?}", statement(Input("fn hello(name: u8, mut greeting: u16) -> u8 { let x = 1 + 9; 7 +9 * 6- 8 }")));
    println!("{:?}", statement(Input("struct mystruct { }")));
    println!("{:?}", statement(Input("struct mystruct { name: string }")));
    println!("{:?}", statement(Input("struct mystruct { name: string, count: u8 }")));
*/
    println!("{:?}", statement(Input("if a { let x = 1; }")));
    println!("{:?}", statement(Input("let v = if a { let x = 1; x } else { let x = 2; x };")));
    println!("{:?}", statement(Input("let v = if a { let x = 1; x } else if b { let x = 2; x } else { let x = 3; x };")));
}
