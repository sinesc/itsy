//! Itsy, a tiny language for embedded use.

#![allow(unused_variables)]
#![allow(dead_code)]

#[macro_use]
extern crate nom;
extern crate byteorder;

pub mod util;
pub mod frontend;
pub mod bytecode;

fn main() {

    let tests = [
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
        "a += b + c;",
        "a += 2.14 - 3 * 723456745675678987489098765487654;", // should fail
        "a += 2.14 - 3 * -727654;",
        "a += 2 - alpha - 3 * -5;",
        "let x = a < b * 3;",
        "let x = a == b && c;",
        "let x = 3 + { let x = 1 + 5; x };",
        "{ let y = 5 + 8; y };",
        "let x = 1; { let y = 5 + 8; y } let y = 3;",
        "let x = a && b || !c && d;",
        "let x = ++i + y;",
        "let a: u32 = 12;
        let b: u8 = 255;
        let x = a && b;
        let e: f32 = 12.0;
        let f: f64 = 14.0;
        f = f + e;",
        "let y = { { let x = 1; } };",
        "let x: u32 = 16;
        let y = x;",
        "let a = 1;
        let b = 256;
        let c = 65537;
        let d = a + b +c;",
        "for i in -1..100 { print(i); }",
        "fn hello(name: u8, mut greeting: u16) -> u8 { let x = 1; greeting }",
        "let x: u16; { x }",
        "fn hello(greeting: u16) { greeting }",
        "fn print(value: u32) -> u32 { value }
        let x = 3.14156;
        let y: f64 = 2.1476;
        print(x+y);",
        "let a = 5;
        let b = -7;
        let c = -357;
        let d = a + b + c;",
    ];

    let mut writer = bytecode::Writer::new();
    writer.load_const(0);
    writer.negate();
    writer.print();
    writer.load_const(1);
    writer.add();
    writer.print();
    writer.exit();


    let mut vm = bytecode::VM::new(writer.code);
    println!("{:}", vm.disassemble());

    vm.push_const(3);
    vm.push_const(7);
    vm.push_const(2);
    vm.run();

    /*
    {
        use nom::types::CompleteStr as Input;
        use frontend::parse;
        use frontend::resolve;

        println!("Succeeded:\n----------");

        for test in tests.iter() {
            if let Ok(ret) = parse(Input(test)) {
                if ret.0.len() == 0 {
                    println!("{:?}", ret.1); // fully parsed, no unparsed code remaining
                }
            }
        }

        println!("\nErrors:\n-------");

        for test in tests.iter() {
            let parsed = parse(Input(test));
            if let Err(err) = parsed {
                println!("ERR: {:?}", err); // flat out error
            } else if let Ok(ret) = parsed {
                if ret.0.len() != 0 {
                    println!("INC: {:?}", ret); // not parsed entirely
                }
            }
        }

        println!("\nLast Item in Detail:\n--------------------");

        let final_test = parse(Input(tests[tests.len() - 1]));

        if let Ok(final_test) = final_test {
            println!("Parsed: {:?}", final_test.1);
            println!("Remaining: {:?}", (final_test.0).0);

            let ready = resolve(final_test.1);
            println!("{:#?}", ready.ast);

            //let code = bytecode::gen::compile(ready);
            //println!("{:#?}", code);
        }
    }
    */
}
