#[macro_use]
extern crate nom;

mod parser;
use parser::*;

use nom::types::CompleteStr as Input;

fn main() {
    println!("{:?}", function(Input("fn hello(name: u8, mut greeting: u16) -> u8 { placeholder }")));
    println!("{:?}", function(Input("fn hello(name: u8, mut greeting: u16) { placeholder }")));
    println!("{:?}", structure(Input("struct mystruct { }")));
    println!("{:?}", structure(Input("struct mystruct { name: string }")));
    println!("{:?}", structure(Input("struct mystruct { name: string, count: u8 }")));
    println!("{:?}", expression(Input("3+5*2-8")));
}
