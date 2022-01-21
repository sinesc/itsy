# Differences

Itsy is designed to be used as a scripting language within Rust applications. Its syntax closely resembles what the Rust language could look like without generics or lifetimes.
This document lists some of the differences between the languages.

## Arrays

Itsy arrays are dynamically sized.

```rust
let x = [ 1, 2, 3 ];
printu8(x[0]); // -> 1
```

The above code is inferred as an array of `u8` elements (assuming `printu8`'s first argument is `u8`).

Arrays support operations like `push` and `pop`.

```rust
x.pop();
x.push(4);
x.push(8);

for i in x {
    printu8(i); // -> 1 2 4 8
}
```

Array operations are documented [here](builtin.array.md).

## References

All non-primitive types in Itsy are reference types. There is no support for explicit references. (Note: currently this does not require the use of `mut x` in arguments/binding. This will likely change in the future.)

```rust
struct Struct {
    value: u8,
}

fn modify(x: Struct) {
    x.value *= 2;
}

fn main() {
    let x = Struct { value: 10 };
    printu8(x.value); // -> 10
    modify(x);
    printu8(x.value); // -> 20
}
```

## Traits

Implementors of a trait are accepted wherever the trait is accepted. No `dyn` or `impl` keyword is required (or allowed).

```rust
pub trait Trait {
    fn required(self: Self, value: u8) -> u8;
    fn provided(self: Self, value: u8) -> u8 {
        self.required(value) * 2
    }
}

pub struct Struct {
    base: u8,
}

impl Trait for Struct {
    fn required(self: Self, value: u8) -> u8 {
        self.base + value
    }
}

fn use_trait(test: Trait) {
    printu8(test.provided(11)); // -> 42
    printu8(test.required(11)); // -> 21
}

fn main() {
    use_trait(Struct { base: 10 });
}
```