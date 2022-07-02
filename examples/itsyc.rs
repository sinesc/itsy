use itsy::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

/*
 * Demo script compiler
 *
 * Usage from project root e.g. target/release/examples/itsyc itsy/examples/mandelbrot.itsy mandelbrot.bin
 */

mod shared;
use shared::MyAPI;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        println!("usage: itsyc <input-filename> <output-filename>");
    } else {
        match build::<MyAPI, _>(&args[1]) {
            Ok(program) => {
                let mut file = File::create(&args[2])?;
                file.write_all(&program.to_bytes())?;
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }
    Ok(())
}