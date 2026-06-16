# Itsy
Strongly typed scripting language with a rusty syntax and easy Rust integration written entirely in safe Rust.

***State of the project:** Feature-incomplete, totally NOT ready for production, APIs may change, expect bugs and bad diagnostic. Possibly fun to play with.*

## Trying it

Clone the repository. It contains VSCode tasks to run the currently opened `.itsy`-source. Open a file (e.g. `itsy/examples/mandelbrot.itsy`) in the editor, run the task
*"cargo run file in runner"*. This will build the `run` application and use it to run the opened file.

## Installation

Add the following to our `Cargo.toml` to make Itsy avaialble in your project.

```
[dependencies]
itsy = "0.5"
```

Optionally add `itsy-derive = "0.5"` if you need custom types in your integration.

## Integration

Once installed it can be used in your project like this:

```rust
use itsy::{itsy_api, build_str, run};

// Define an API of Rust functions that are callable from the Itsy script.
itsy_api!(MyAPI<()> {
    fn print(&mut context, value: str) {
        println!("print: {}", value);
    }
    // ... more functions ...
});

fn main() {
    // Build the itsy program and link it to the MyAPI API we created above.
    let program = build_str::<MyAPI>(stringify!(
        // An Itsy program that calls the Rust 'print' function.
        fn main() {
            MyAPI::print("Hello from Itsy!");
        }
    )).unwrap();

    run(program, &mut ()).unwrap();
}
```

## Where to go from here

**Crate reference:** [here](https://docs.rs/itsy/)\
**Standard library/builtins reference** Linked on the index page of the crate documentation.\
**Language reference:** non-existent, some differences listed [here](doc/differences.md)