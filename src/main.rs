#[macro_use]
extern crate nom;

mod parser;
use parser::*;

fn main() {
    println!("{:?}", function("fn hello(name: u8, mut greeting: u16) -> u8 { placeholder } ."));
    println!("{:?}", function("fn hello(name: u8, mut greeting: u16) { placeholder } ."));
}
