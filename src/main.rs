#![allow(unused_variables)]
#![allow(dead_code)]

#[macro_use]
extern crate nom;

mod util;
mod frontend;

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
        "a += 2.14 - 3 * 723456745675678987489098765487654;", // should fail
        "a += 2.14 - 3 * -727654;",
        "a += 2 - alpha - 3 * -5;",
        "let x = a < b * 3;",
        "let x = a == b && c;",
        "let x = 3 + { let x = 1 + 5; x };",
        "{ let y = 5 + 8; y };",
        "let x = 1; { let y = 5 + 8; y } let y = 3;",
        "for i in 1..100 { print(i); }",
        "let x = a && b || !c && d;",
        "let x = ++i + y;",
        "let x: bool = b && c;",
    ];

    {
        use nom::types::CompleteStr as Input;
        use frontend::parser::parse;
        use frontend::resolver::resolve;

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

            println!("{:?}", ready);
        }
    }
}
