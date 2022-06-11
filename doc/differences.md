# Differences

Itsy is designed to be used as a scripting language within Rust applications. Its syntax closely resembles what the Rust language could look like without generics or lifetimes.
This document lists some of the differences between the languages.

## Arrays

Itsy arrays are dynamically sized.

```rust
let x = [ 1, 2, 3 ];
printu8(x[0]); // -> 1
```

The above code is inferred as an array of `u8` elements (assuming a `printu8` function exists and accepts a `u8` as first argument).

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

Implementors of a trait are accepted wherever the trait is accepted. The `dyn` or `impl` keywords are not required or supported.

```rust
// A trait that requires implementation of `mathificate` and provides a `print` function
pub trait Math {
    fn mathificate(self: Self, value: u8) -> u8;
    fn print(self: Self, value: u8) {
        printu8(self.mathificate(value));
    }
}

// A struct for which the `Math` trait will be implemented as a multiplication.
pub struct Multiply {
    with: u8,
}

impl Math for Multiply {
    fn mathificate(self: Self, value: u8) -> u8 {
        self.with * value
    }
}

// A struct for which the `Math` trait will be implemented as an addition.
pub struct Add {
    to: u8,
}

impl Math for Add {
    fn mathificate(self: Self, value: u8) -> u8 {
        self.to + value
    }
}

// A function that accepts a value together with a type that implements `Math`.
fn compute(value: u8, test: Math) {
    test.print(value);
}

fn main() {
    compute(11, Multiply { with: 2 });  // 2*11=22
    compute(9, Add { to: 7 });          // 7+9=16
    Add { to: 3 }.print(3);             // 3+3=6
}
```