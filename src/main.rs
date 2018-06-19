#[macro_use]
extern crate nom;

mod ast;
mod parser;
use parser::parse;

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
        "a += b + c;",
        "a += 2.14 - 3 * 723456745675678987489098765487654;",
        "a += 2.14 - 3 * -727654;",
        "a += 2 - alpha - 3 * -5;",
        "let x = a < b * 3;",
        "let x = a == b && c;",
        "let x = 3 + { let x = 1 + 5; x };",
        "{ let y = 5 + 8; y };",
        "let x = 1; { let y = 5 + 8; y } let y = 3;",
    ];

    {
        use nom::types::CompleteStr as Input;

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
                println!("{:?}", err); // flat out error
            } else if let Ok(ret) = parsed {
                if ret.0.len() != 0 {
                    println!("{:?}", ret); // not parsed entirely
                }
            }
        }

        println!("\nLast Item in Detail:\n--------------------");

        let final_test = parse(Input(tests[tests.len() - 1]));

        if let Ok(final_test) = final_test {
            println!("Parsed: {:?}", final_test.1);
            println!("Remaining: {:?}", (final_test.0).0);
        }
    }
}
