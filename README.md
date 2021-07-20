# Itsy
Strongly typed scripting language with a rusty syntax and easy Rust integration written entirely in safe Rust.

***State of the project:** Very incomplete, maybe ok for some tinkering if you're bored. Please don't waste your time with this if you need to get something done though.*

**Crate reference:** [here](https://docs.rs/itsy/)\
**Language reference:** non-existent. Its basically Rust without lifetimes and generics (and lots of other stuff for now).

The following example exposes a Rust function `print` to Itsy. It compiles and runs
an Itsy script that calls the Rust function.

```rust
use itsy::{vm_func, build, run};

vm_func!(MyFns, (), {
    /// a rust function that prints given string
    fn print(&mut context, value: &str) {
        println!("print: {}", value);
    }
    // ... more functions ...
});

fn main() {
    let program = build::<MyFns>("
        /// an itsy function that calls a rust function a few times
        fn main() {
            for _ in 0..3 {
                print(\"Hello from Itsy!\");
            }
        }
    ").unwrap();

    run(&program, &mut ()).unwrap();
}
```

