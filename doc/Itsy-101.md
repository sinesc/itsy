# Itsy 101 — Language Cheat Sheet

Rusty syntax, statically-typed scripting language. Embedded into Rust apps via the `itsy_api!` macro.

## Declarations

### `const`

Compile-time constants with optional type annotation:

```itsy
const MAX: u32 = 100;
const NAME = "hello";
```
Supports `+`, `-`, `*`, `/`, `%`, unary `-`/`+`/`!`, casts, and string interpolation.

Allowed in module level, trait definitions, inside functions.

### `let`

All `let` bindings must be inside functions - module-level variables are NOT allowed:

```itsy
fn main() {
    let x: i32 = 0;       // OK
}
let y: i32 = 0;           // ERROR: parser error
```

## Types

### Overview

#### Primitives

`u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `bool`

No `usize`/`isize` — use `u64` for indexing (View, arrays, maps).

Opaque `HeapRef` type obtained by casting a reference (e.g. `arr as HeapRef`) can be passed to Rust code for direct heap access.

#### Literals and type inference

Integer literals default to `i32` when unannotated, but are inferred from context:

```itsy
let x: u32 = 5;      // 5 inferred as u32
let y = 2u8 * 10;   // 10 inferred as u8, result is u8
```

#### Reference types

All non-primitive types are reference types (heap-allocated, refcounted):
- `String` — immutable, use `+=` for concatenation
- `[T]` — dynamic arrays (`.push`, `.pop`, `.len`)
- `[K: V]` — maps
- `struct` / `enum` — user-defined types
- `View<T>` — zero-copy buffer view
- `Option<T>`, `Result<T>` — optional result/error wrappers

No explicit `&`/`mut` references. Reference types pass by reference automatically.

### Strings

#### Interpolation
```itsy
let name = "world";
let msg = "hello {name}";
```

#### Concatenation
`str += other` for mutable append. `str1 + str2` for expression concat.

### Structs and enums

Definition must be at top level, never inside functions:

```itsy
struct Point { x: f32, y: f32 }
enum Color { Red, Green, Blue }
```

### Data enums

Tuple variants take bare types as positional fields:

```itsy
enum Command {
    Move(f32, f32),
    Stop,
}
```

Struct variants take named fields in braces:

```itsy
enum Shape {
    Rect { w: f32, h: f32 },
    Circle { r: f32 },
}
```

Construct with `Command::Move(1.0, 2.0)`, `Shape::Rect { w: 1.0, h: 2.0 }` or positionally `Shape::Rect(1.0, 2.0)`. Match with `match` (exhaustive); struct-variant arms use field patterns with a `..` rest:

```itsy
let area = match shape {
    Shape::Rect { w, h, .. } => w * h,
    Shape::Circle { r, .. } => 3.14 * r * r,
};
```

Field order is part of the value layout — positional construction and heap offsets follow declaration order.

### Arrays

Dynamic, indexed with `u64`:
```itsy
let arr: [i32] = [1, 2, 3];
arr.push(4);
let val = arr[0]; // 0 inferred as u64
```

### Maps

```itsy
let m = [ "a" => 1u64, "b" => 2 ];
m["c"] = 3;
let empty = [ => ];  // empty map, types inferred from insert below
empty.insert(1i8, "one");
```

### View<T>

Zero-copy flat byte buffers for IPC with the host:

```itsy
let v: View<i32> = View::new(10 as u64);
let v = View<i32>::new(10);  // turbofish, no annotation needed
```

Can be passed `as HeapRef` to Rust code.

### Result<T> and Option<T>

`Result<T>` is a built-in. `Ok(x)` and `Err(e)` construct the variants; match to inspect. `?` unwraps `Ok` or returns `Err` from the current function — it may only be used inside functions returning `Result`.

Any type implementing the built-in `Error` trait can be used as an error value. Different concrete error types flow through a single `Result<T>` without conversion:

```itsy
struct NotFound { key: String }

impl Error for NotFound {
    fn description(self: Self) -> String {
        "not found: {self.key}"
    }
}

fn lookup(key: String) -> Result<i32> {
    if key == "answer" {
        Ok(42)
    } else {
        Err(NotFound { key: key })
    }
}

fn main() {
    match lookup("answer") {
        Ok(value) => print("{value}\n"),
        Err(e) => print("error: {e.description()}\n"),
    }
}
```

`Option<T>` with `Some(val)` / `None`. Array `.get(index)` and map `.get(key)` return `Option<T>`.

## Traits

Traits define interfaces; `impl Trait for Type` provides implementations. No `dyn` or `impl` keywords needed — trait implementors are accepted wherever the trait is expected:

```itsy
trait Math {
    fn calc(self: Self, value: u8) -> u8;
}

struct Multiply { with: u8 }

impl Math for Multiply {
    fn calc(self: Self, value: u8) -> u8 {
        self.with * value
    }
}

fn compute(value: u8, op: Math) {
    print("{op.calc(value)}\n");
}

fn main() {
    compute(11, Multiply { with: 2 });  // -> 22
}
```

## Functions

### Signature rules

- No tuple return types — use a struct instead
- `Self` usable inside `impl` blocks

```itsy
fn foo(x: i32, y: i32, z: i32) -> i32 {
    return x + y + z;
}
```

### Fallback return after infinite loops

`while true` with only `return` inside is not proven exhaustive. Add a fallback:

```itsy
fn loop_forever() -> i32 {
    while (true) {
        return 42;
    }
    return -1;  // needed to satisfy type checker
}
```

## Control flow

### `while` loops

```itsy
let i: i32 = 0;
while (i < 10) {
    i = i + 1;
}
```

### `if` / `else if` / `else`

Standard syntax. Boolean expressions only (no truthy/falsy).

### `match` expression (exhaustive)

```itsy
let result = match color {
    Color::Red => 1,
    Color::Green => 2,
    Color::Blue => 3,
};
```

Use `_` as a default arm to catch remaining cases:

```itsy
let a = 1;
match a {
    1 => println("one"),
    _ => println("not one"),
}; // semicolon required, `match` is an expression (here `void`)
```

### `for` loops

```itsy
for i in arr {
    print("{i}\n");
}
for i in 0u64..10 {
    print("{i}\n");
}
```

### `return`

Early return from functions. Implicit return on last expression (no semicolon).

## Operators

### Arithmetic

`+`, `-`, `*`, `/`, `%` — standard. Compound `+=`, `-=`, `*=`, `/=`, `%=`.

### Comparison

`==`, `!=`, `<`, `<=`, `>`, `>=`

### Logical

`&&`, `||`, `!`

### Assignment

`=` for reassignment. Use compound `+=`, `-=`, `*=`, `/=`, `%=` where convenient.

## API integration

### Host-calls

Host-exposed functions are namespaced under the API type name:

```itsy
use MyApi::{print, get_value, set_flag};
```
The namespace must match the Rust `itsy_api!` type name exactly (`MyApi`, not `Api`).

`itsy_api!` primitive constants are namespaced the same way (e.g. `MyApi::CURSOR_LEFT`) and can be
imported, used in match patterns and const expressions like any other constant.

### suspend

Yields control back to the host, preserving VM state (stack + heap). Only inside functions:

```itsy
fn main() {
    // ... per-frame logic ...
    suspend;
}
```

## Common pitfalls summary

| Issue | Fix |
|-------|-----|
| No `let` outside functions | Move all bindings into `fn main()` |
| No `usize` | Use `u64` for all indexing |
| Named variant fields need braces | `Variant { x: i32, y: i32 }` not `Variant(x: i32, y: i32)` |
| Literals default to `i32` | Cast explicitly when required: `5 as u32` |
| `while true` needs fallback | Add `return` after the loop |
| View/array indexing needs `u64` | Cast: `arr[idx as u64]` |
| `View::new(n)` needs `u64` | Pass `n as u64` or turbofish |
