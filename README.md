# Itsy
Strongly typed scripting language with a rusty syntax and easy Rust integration written entirely in safe Rust.

***State of the project:** Feature-incomplete, NOT ready for production, syntax and APIs may change, expect bugs and bad diagnostic. Possibly fun to play with.*

**Crate reference:** [here](https://docs.rs/itsy/)\
**Language reference:** non-existent, some differences listed [here](doc/differences.md)

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

