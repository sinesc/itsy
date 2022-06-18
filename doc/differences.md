# Differences

Itsy is designed to be used as a scripting language within Rust applications. Its syntax closely resembles what the Rust language could look like without generics or lifetimes.
This document lists some of the differences between the languages.

## String interpolation

String literals support interpolation.


```rust
fn main() {
    let pi = 3.14159265359;
    print("\u{03C0} equals {pi}!\n"); // -> π equals 3.14159265359!!
}
```

The literal is parsed into the AST equivalent of `("π equals " + (pi as String) + "!\n")` and requires that the type of the interpolated value can be cast to `String` via `as`. (In the future custom types can also implement a Display trait to become compatible).

## Arrays

Arrays are dynamically sized.

```rust
fn main() {
    let x = [ 1u8, 2, 3 ];

    x.pop();
    x.push(4);
    x.push(8);

    for i in x {
        print("{i}\n"); // -> 1 -> 2 -> 4 -> 8
    }
}
```

## References

All non-primitive types are reference types. There is no support for explicit references.

```rust
struct Struct {
    value: u8,
}

fn main() {
    let x = Struct { value: 1 };
    let y = x;
    print("x: {x.value}, y: {y.value}\n");  // -> x: 1, y: 1
    x.value *= 2;                           // (currently works without "mut", will likely change)
    print("x: {x.value}, y: {y.value}\n");  // -> x: 2, y: 2
}
```

## Traits

Implementors of a trait are accepted wherever the trait is accepted. The `dyn` or `impl` keywords are not required or supported.

```rust
// A trait that requires implementation of `mathificate` and provides a `print` function
pub trait Math {
    fn mathificate(self: Self, value: u8) -> u8;
    fn print(self: Self, value: u8) {
        print("{self.mathificate(value)}\n"); // interpolated expression
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
    compute(11, Multiply { with: 2 });  // performs 2 * 11 -> 22
    compute(9, Add { to: 7 });          // performs 7 + 9 -> 16
    Add { to: 3 }.print(3);             // performs 3 + 3 -> 6
}
```