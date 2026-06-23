
//! Itsy language reference
//!
//! # Basic types
//!
//! [Unsigned numbers](crate::documentation::Integer): `u8`, `u16`, `u32`, `u64`.\
//! [Signed numbers](crate::documentation::Integer): `i8`, `i16`, `i32`, `i64`.\
//! [Floating point numbers](crate::documentation::Float): `f32`, `f64`.\
//! [Character strings](crate::documentation::String): `String`
//!
//! # Compound types
//!
//! [Arrays](crate::documentation::Array), variable-length lists of values of the same type: `[ v, ... ]`.\
//! [Maps](crate::documentation::Map), variable-length associations of keys to values: `[ k => v, ... ]`.\
//! [Structs](crate::documentation::structs), product of multiple types: `struct { field1: Type1, ... }`.\
//! [Enums](crate::documentation::enums), disjoint union (one of n variants, variants can carry data): `enum { A, B(p1, p2, ...), ... }`.
//!
//! # Built-in container types
//!
//! [Result], success-or-error type: `Result<T>` with `Ok(value)` and `Err(error)`.\
//! [Option], nullable type: `Option<T>` with `Some(value)` and `None`.
//!
//! # Traits
//!
//! [Traits](crate::documentation::traits) define shared behavior that structs and enums can implement.\
//! [Intrinsic traits](crate::documentation::intrinsic_traits) are a special subset recognized by the compiler:
//! implementing one for a custom type makes the language functionality it backs work on that type
//! (e.g. `Add` backs `+`, `ToString` backs `as String`).
//!
//! # Generators
//!
//! [Generators](crate::documentation::generators) are functions that lazily produce a sequence of values with
//! the `yield` keyword, returning a [`Generator<V>`](crate::documentation::Generator) that can be iterated with a `for` loop.

#[doc(inline)]
pub use crate::bytecode::builtins::builtin_type_documentation::*;

/// Success-or-error type, written `Result<T>`.
///
/// A `Result<T>` is either `Ok(value)` carrying a success value of type `T`, or `Err(error)`
/// carrying any value implementing the [`Error`](self::intrinsic_traits::Error) trait. Unlike Rust's
/// `Result<T, E>` the error type is not a parameter: every error is an `Error`, which is what lets
/// errors of different concrete types flow through one `Result<T>`.
///
/// Inspect a result by matching it; both variants are part of the language:
///
/// ``` ignore
/// match parse(text) {
///     Ok(value) => print("got {value}"),
///     Err(e) => print("failed: {e.description()}"),
/// }
/// ```
///
/// # The `?` operator
///
/// Postfix `?` on a `Result<T>` evaluates to the contained `T` when the result is `Ok`, and
/// otherwise returns the `Err` from the enclosing function unchanged. It may only be used inside a
/// function that itself returns a `Result`. This makes chaining fallible calls concise:
///
/// ``` ignore
/// fn total(a: String, b: String) -> Result<i32> {
///     let x = parse(a)?;   // returns early if parse fails
///     let y = parse(b)?;
///     Ok(x + y)
/// }
/// ```
pub struct Result { }

/// Nullable type, written `Option<T>`.
///
/// An `Option<T>` is either `Some(value)` carrying a value of type `T`, or `None` indicating
/// the absence of a value. This lets functions express that a result may or may not exist
/// without resorting to sentinel values or errors.
///
/// Inspect an option by matching it; both variants are part of the language:
///
/// ``` ignore
/// match find(items, "hello") {
///     Some(item) => print("found: {item}"),
///     None => print("not found"),
/// }
/// ```
///
/// `Some(value)` is written as a call and `None` is written bare (without parentheses). Both
/// are only valid where an `Option<T>` is expected:
///
/// ``` ignore
/// fn first(items: [i32]) -> Option<i32> {
///     if items.len() == 0 {
///         None
///     } else {
///         Some(items[0])
///     }
/// }
/// ```
///
/// Like `Result`, an `Option<T>` can be used anywhere a type annotation or return type is
/// accepted. The compiler infers the inner type from context when possible:
///
/// ``` ignore
/// fn double_opt(n: Option<i32>) -> Option<i32> {
///     match n {
///         Some(v) => Some(v * 2),
///         None => None,
///     }
/// }
/// ```
pub struct Option { }

/// Structs: fixed collections of named fields.
///
/// A struct groups multiple values of potentially different types under one name. Fields are
/// accessed with dot notation (`value.field`). Structs are reference types: binding a struct to
/// another variable shares the data rather than copying it.
///
/// # Defining a struct
///
/// ``` ignore
/// struct Point {
///     x: i32,
///     y: i32,
/// }
/// ```
///
/// # Constructing a struct
///
/// Use the struct name followed by a brace-delimited list of `field: value` pairs:
///
/// ``` ignore
/// let origin = Point { x: 0, y: 0 };
/// let p = Point { x: 3, y: 7 };
/// ```
///
/// Fields can be of any type, including other structs, arrays, strings, or enums. Nested
/// construction is supported:
///
/// ``` ignore
/// struct Inner {
///     v: i32,
///     label: String,
/// }
/// struct Outer {
///     inner: Inner,
///     tag: i32,
/// }
///
/// let o = Outer {
///     inner: Inner { v: 42, label: "hello" },
///     tag: 1,
/// };
/// ```
///
/// # Accessing fields
///
/// Use dot notation to read fields. For mutable bindings, fields can also be assigned to:
///
/// ``` ignore
/// let p = Point { x: 1, y: 2 };
/// print("{p.x}, {p.y}");   // 1, 2
///
/// let mut p2 = Point { x: 1, y: 2 };
/// p2.x = 10;
/// ```
///
/// # Struct methods
///
/// Methods are defined inside `impl` blocks on the struct. `Self` refers to the struct type,
/// and `self: Self` gives access to the instance:
///
/// ``` ignore
/// impl Point {
///     fn distance(self: Self) -> f64 {
///         (self.x as f64 * self.x as f64 + self.y as f64 * self.y as f64).sqrt()
///     }
///     fn new(x: i32, y: i32) -> Self {
///         Point { x: x, y: y }
///     }
/// }
///
/// let p = Point::new(3, 4);
/// print("{p.distance()}");  // 5.0
/// ```
///
/// # Matching structs
///
/// Structs can be deconstructed in `match` patterns. All fields must be accounted for unless
/// a trailing `..` is used:
///
/// ``` ignore
/// match p {
///     Point { x: 0, y: 0 } => print("origin"),
///     Point { x, y } => print("({x}, {y})"),  // shorthand binding
/// }
/// ```
///
/// The `..` rest pattern ignores unlisted fields:
///
/// ``` ignore
/// struct Flags { a: bool, b: bool, c: bool }
///
/// match f {
///     Flags { a: true, .. } => print("a is set"),
///     Flags { .. } => print("a is not set"),
/// }
/// ```
///
/// # Destructuring in `let`
///
/// Struct fields can be extracted directly in a `let` binding:
///
/// ``` ignore
/// let Point { x, y } = p;            // shorthand
/// let Point { x: px, y: py } = p;    // explicit renaming
/// let Point { x, .. } = p;           // extract only x
/// ```
///
/// # Equality
///
/// Two structs are equal (`==`) when all their fields are equal, compared recursively.
/// Implementing the [`Eq`](crate::documentation::intrinsic_traits::Eq) intrinsic trait overrides
/// this default with a custom notion of equality.
///
/// # Recursion
///
/// A struct cannot contain a field of its own type (direct or indirect) because that would make it
/// infinitely sized. Use arrays or other container types to build recursive data structures.
pub mod structs { }

/// Enums: disjoint unions with named variants.
///
/// An enum represents a value that is exactly one of several possible *variants*. Variants can be
/// simple (unit variants, like tags) or can carry data (data variants).
///
/// # Defining an enum
///
/// ``` ignore
/// enum Direction {
///     North,
///     East,
///     South,
///     West,
/// }
/// ```
///
/// # Data variants
///
/// Variants can carry values of any type. Multiple values are separated by commas:
///
/// ``` ignore
/// enum Shape {
///     Circle(i32),
///     Square(i32),
///     Rect(i32, i32),
/// }
/// ```
///
/// # Constructing enum values
///
/// Use `EnumName::Variant` for unit variants and `EnumName::Variant(value)` for data variants:
///
/// ``` ignore
/// let dir = Direction::North;
/// let circle = Shape::Circle(5);
/// let rect = Shape::Rect(3, 4);
/// ```
///
/// # Discriminant values
///
/// Like Rust, variants can have explicit discriminant values. Unspecified variants get the next
/// integer after the previous one:
///
/// ``` ignore
/// enum Status {
///     Pending = 1,
///     Active,    // 2
///     Done,      // 3
/// }
///
/// let s = Status::Active;
/// print("{s as u8}");  // 2
/// ```
///
/// # Matching enums
///
/// The `match` expression is the primary way to inspect an enum. Every variant must be covered,
/// either explicitly or with a wildcard (`_`):
///
/// ``` ignore
/// match dir {
///     Direction::North => print("up"),
///     Direction::South => print("down"),
///     _ => print("sideways"),
/// }
/// ```
///
/// Data variant patterns bind the payload to names:
///
/// ``` ignore
/// match shape {
///     Shape::Circle(r) => print("circle radius {r}"),
///     Shape::Square(s) => print("square side {s}"),
///     Shape::Rect(w, h) => print("rect {w}x{h}"),
/// }
/// ```
///
/// # Equality
///
/// Two enum values are equal (`==`) when they are the same variant with equal payloads. Unit
/// variants compare by identity; data variants compare their payloads recursively.
///
/// # Enums with traits
///
/// Like structs, enums can implement traits. This lets you define shared behavior across
/// different enum variants:
///
/// ``` ignore
/// trait Describe {
///     fn kind(self: Self) -> String;
/// }
///
/// impl Describe for Shape {
///     fn kind(self: Self) -> String {
///         match self {
///             Shape::Circle(_) => "circle",
///             Shape::Square(_) => "square",
///             Shape::Rect(_, _) => "rectangle",
///         }
///     }
/// }
/// ```
///
/// # Recursion
///
/// An enum cannot have a data variant that carries the enum itself by value, as that would make
/// it infinitely sized. Use arrays or other container types for recursive data structures.
pub mod enums { }

/// User-defined traits: shared behavior for structs and enums.
///
/// A trait declares a set of methods that any type can implement. Once a type implements a trait,
/// it can be used anywhere that trait is expected — no `dyn` or `impl` keywords are needed.
///
/// # Defining a trait
///
/// A trait declares one or more required methods. It can also provide default implementations
/// that implementors inherit automatically:
///
/// ``` ignore
/// trait Speaker {
///     fn speak(self: Self) -> String;
///     fn shout(self: Self) -> String {
///         let words = self.speak();
///         words.to_uppercase() + "!"
///     }
/// }
/// ```
///
/// # Implementing a trait
///
/// Use `impl TraitName for TypeName { ... }` to provide the required methods. Default methods are
/// inherited but can be overridden:
///
/// ``` ignore
/// struct Dog { name: String }
///
/// impl Speaker for Dog {
///     fn speak(self: Self) -> String {
///         "woof, I'm " + self.name
///     }
/// }
///
/// let rex = Dog { name: "Rex" };
/// print(rex.speak());   // woof, I'm Rex
/// print(rex.shout());   // WOOF, I'M REX!
/// ```
///
/// Traits can be implemented on both structs and enums:
///
/// ``` ignore
/// enum Shape { Circle(i32), Square(i32) }
///
/// trait Describe {
///     fn kind(self: Self) -> String;
/// }
///
/// impl Describe for Shape {
///     fn kind(self: Self) -> String {
///         match self {
///             Shape::Circle(_) => "circle",
///             Shape::Square(_) => "square",
///         }
///     }
/// }
/// ```
///
/// # Trait objects
///
/// A trait name can be used as a type in function parameters, struct fields, and arrays. When a
/// concrete value is passed to a trait-typed slot, it is stored as a *trait object* — the concrete
/// type is preserved at runtime so the correct implementation is dispatched dynamically:
///
/// ``` ignore
/// struct Cat { lives: u8 }
///
/// impl Speaker for Cat {
///     fn speak(self: Self) -> String {
///         "meow x" + self.lives as String
///     }
/// }
///
/// fn make_sound(who: Speaker) {
///     print(who.speak());
/// }
///
/// make_sound(Dog { name: "Rex" });  // woof, I'm Rex
/// make_sound(Cat { lives: 9 });     // meow x9
/// ```
///
/// # Multiple trait bounds
///
/// A parameter can require multiple traits using `+`. The value must implement every listed trait:
///
/// ``` ignore
/// trait Named {
///     fn name(self: Self) -> String;
/// }
/// trait Aged {
///     fn age(self: Self) -> u8;
/// }
///
/// struct Person { n: String, a: u8 }
///
/// impl Named for Person {
///     fn name(self: Self) -> String { self.n }
/// }
/// impl Aged for Person {
///     fn age(self: Self) -> u8 { self.a }
/// }
///
/// fn describe(who: Named + Aged) {
///     print("{who.name()} is {who.age()} years old");
/// }
/// ```
///
/// # Returning concrete types from trait return slots
///
/// A function declaring a trait return type may return a concrete implementor. The returned value
/// is dispatched dynamically:
///
/// ``` ignore
/// fn make_pet(kind: String) -> Speaker {
///     if kind == "dog" {
///         Dog { name: "Spot" }
///     } else {
///         Cat { lives: 7 }
///     }
/// }
/// ```
///
/// # Intrinsic traits
///
/// In addition to user-defined traits, the compiler recognizes a set of [intrinsic traits](crate::documentation::intrinsic_traits)
/// that back language features like operators and casts. These are implemented with the same `impl`
/// syntax but are known to the compiler and in scope without a `use`.
pub mod traits { }

/// Intrinsic traits recognized by the compiler.
///
/// Unlike user-defined traits, these are known to the compiler: implementing one for a custom type
/// makes the language feature it backs work on that type. They are implemented exactly like any
/// other trait (`impl Add for MyType { ... }`) and are in scope in every module without a `use`.
///
/// Intrinsic traits only apply to custom types; built-in types always use compiler-native logic.
/// Implementing one never changes how the operator or cast behaves on built-in types.
///
/// # The arithmetic operator traits
///
/// [`Add`](crate::documentation::intrinsic_traits::Add), [`Sub`](crate::documentation::intrinsic_traits::Sub), [`Mul`](crate::documentation::intrinsic_traits::Mul), [`Div`](crate::documentation::intrinsic_traits::Div) and
/// [`Rem`](crate::documentation::intrinsic_traits::Rem) overload the binary arithmetic operators. They all
/// share one shape — `fn op(self: Self, rhs: Self) -> Self` — taking a right-hand operand of the same
/// type and producing a value of that type. Each one backs both its binary operator *and* the matching
/// compound-assignment form (`Add` backs `+` and `+=`, `Sub` backs `-` and `-=`, and so on); there is
/// no separate trait for the compound form. A type may implement any combination of them:
///
/// ``` ignore
/// struct Vec2 { x: i64, y: i64 }
///
/// impl Add for Vec2 {
///     fn add(self: Self, rhs: Self) -> Self {
///         Vec2 { x: self.x + rhs.x, y: self.y + rhs.y }
///     }
/// }
/// impl Mul for Vec2 {
///     fn mul(self: Self, rhs: Self) -> Self {
///         Vec2 { x: self.x * rhs.x, y: self.y * rhs.y }
///     }
/// }
///
/// let mut v = Vec2 { x: 1, y: 2 } + Vec2 { x: 3, y: 4 };  // Vec2 { x: 4, y: 6 }
/// v += Vec2 { x: 1, y: 1 };                               // Vec2 { x: 5, y: 7 }  (uses Add)
/// let scaled = v * Vec2 { x: 2, y: 2 };                   // Vec2 { x: 10, y: 14 }
/// ```
///
/// # The bitwise and shift operator traits
///
/// [`BitAnd`](crate::documentation::intrinsic_traits::BitAnd), [`BitOr`](crate::documentation::intrinsic_traits::BitOr) and
/// [`BitXor`](crate::documentation::intrinsic_traits::BitXor) overload the bitwise operators `&`, `|` and `^`.
/// Like the arithmetic traits they share the shape `fn op(self: Self, rhs: Self) -> Self` and each backs
/// both the binary operator and the compound-assignment form (`&=`, `|=`, `^=`).
///
/// [`Shl`](crate::documentation::intrinsic_traits::Shl) and [`Shr`](crate::documentation::intrinsic_traits::Shr) overload the
/// shift operators `<<` and `>>`. Their right-hand operand is `i64` (the shift amount), so their shape
/// is `fn op(self: Self, rhs: i64) -> Self`. Each backs both the binary and compound-assignment form
/// (`<<=`, `>>=`):
///
/// ``` ignore
/// struct BitField { bits: i64 }
///
/// impl BitAnd for BitField {
///     fn bitand(self: Self, rhs: Self) -> Self {
///         BitField { bits: self.bits & rhs.bits }
///     }
/// }
/// impl Shl for BitField {
///     fn shl(self: Self, rhs: i64) -> Self {
///         BitField { bits: self.bits << rhs }
///     }
/// }
///
/// let a = BitField { bits: 0b1111 };
/// let b = BitField { bits: 0b1010 };
/// let c = a & b;                                          // BitField { bits: 0b1010 }
/// let d = a << 2;                                         // BitField { bits: 0b111100 }
/// ```
///
/// # The equality trait
///
/// [`Eq`](crate::documentation::intrinsic_traits::Eq) overloads the `==` and `!=` operators. Its single method
/// `fn eq(self: Self, rhs: Self) -> bool` decides whether two values are equal; `!=` is the negation of
/// `eq` and needs no separate method. Implementing it overrides the built-in deep comparison, which is
/// what lets a type define its own notion of equality (e.g. ignoring a cached field):
///
/// ``` ignore
/// struct Money { cents: i64, note: String }
///
/// impl Eq for Money {
///     fn eq(self: Self, rhs: Self) -> bool {
///         self.cents == rhs.cents   // compare by value only, ignoring the note
///     }
/// }
///
/// let a = Money { cents: 500, note: "rent" };
/// let b = Money { cents: 500, note: "gift" };
/// let equal = a == b;   // true
/// let differ = a != b;  // false
/// ```
pub mod intrinsic_traits {

    /// Converts a value to a [`String`](crate::documentation::String).
    ///
    /// Backs the `as String` cast for custom types, and by extension string interpolation
    /// (which lowers to `as String`). Implementing it makes `value as String` and `"{value}"` work
    /// for the given type.
    ///
    /// # Examples
    ///
    /// ``` ignore
    /// struct Point { x: i32, y: i32 }
    /// impl ToString for Point {
    ///     fn to_string(self: Self) -> String {
    ///         "({self.x}, {self.y})"
    ///     }
    /// }
    ///
    /// let p = Point { x: 3, y: 7 };
    /// print(p as String);   // (3, 7)
    /// print("point: {p}");  // point: (3, 7)
    /// ```
    pub trait ToString {
        /// Returns the string representation of `self`.
        fn to_string(self: Self) -> String;
    }

    /// Overloads the `+` and `+=` operators. See [the operator traits](self#the-arithmetic-operator-traits)
    /// for an example.
    pub trait Add {
        /// Returns the result of `self + rhs`.
        fn add(self: Self, rhs: Self) -> Self;
    }

    /// Overloads the `-` and `-=` operators. See [the operator traits](self#the-arithmetic-operator-traits)
    /// for an example.
    pub trait Sub {
        /// Returns the result of `self - rhs`.
        fn sub(self: Self, rhs: Self) -> Self;
    }

    /// Overloads the `*` and `*=` operators. See [the operator traits](self#the-arithmetic-operator-traits)
    /// for an example.
    pub trait Mul {
        /// Returns the result of `self * rhs`.
        fn mul(self: Self, rhs: Self) -> Self;
    }

    /// Overloads the `/` and `/=` operators. See [the operator traits](self#the-arithmetic-operator-traits)
    /// for an example.
    pub trait Div {
        /// Returns the result of `self / rhs`.
        fn div(self: Self, rhs: Self) -> Self;
    }

    /// Overloads the `%` and `%=` operators. See [the operator traits](self#the-arithmetic-operator-traits)
    /// for an example.
    pub trait Rem {
        /// Returns the result of `self % rhs`.
        fn rem(self: Self, rhs: Self) -> Self;
    }

    /// Overloads the `&` and `&=` operators. See [the bitwise and shift operator traits](self#the-bitwise-and-shift-operator-traits)
    /// for an example.
    pub trait BitAnd {
        /// Returns the result of `self & rhs`.
        fn bitand(self: Self, rhs: Self) -> Self;
    }

    /// Overloads the `|` and `|=` operators. See [the bitwise and shift operator traits](self#the-bitwise-and-shift-operator-traits)
    /// for an example.
    pub trait BitOr {
        /// Returns the result of `self | rhs`.
        fn bitor(self: Self, rhs: Self) -> Self;
    }

    /// Overloads the `^` and `^=` operators. See [the bitwise and shift operator traits](self#the-bitwise-and-shift-operator-traits)
    /// for an example.
    pub trait BitXor {
        /// Returns the result of `self ^ rhs`.
        fn bitxor(self: Self, rhs: Self) -> Self;
    }

    /// Overloads the `<<` and `<<=` operators. See [the bitwise and shift operator traits](self#the-bitwise-and-shift-operator-traits)
    /// for an example.
    pub trait Shl {
        /// Returns the result of `self << rhs`.
        fn shl(self: Self, rhs: i64) -> Self;
    }

    /// Overloads the `>>` and `>>=` operators. See [the bitwise and shift operator traits](self#the-bitwise-and-shift-operator-traits)
    /// for an example.
    pub trait Shr {
        /// Returns the result of `self >> rhs`.
        fn shr(self: Self, rhs: i64) -> Self;
    }

    /// Overloads the `==` and `!=` operators. See [the equality trait](self#the-equality-trait)
    /// for an example.
    pub trait Eq {
        /// Returns `true` if `self` equals `rhs`. The `!=` operator returns the negation of this.
        fn eq(self: Self, rhs: Self) -> bool;
    }

    /// The error type of [`Result`](crate::documentation::Result).
    ///
    /// `Error` is the fixed error side of every `Result<T>`. Any type implementing `Error` is accepted
    /// as the `Err` payload of a `Result`, which is what lets the [`?`](crate::documentation::Result) operator
    /// propagate errors of different concrete types through a single `Result<T>` without conversion.
    ///
    /// # Examples
    ///
    /// ``` ignore
    /// struct NotFound { key: String }
    /// impl Error for NotFound {
    ///     fn description(self: Self) -> String {
    ///         "not found: {self.key}"
    ///     }
    /// }
    ///
    /// fn lookup(key: String) -> Result<i32> {
    ///     Err(NotFound { key: key })
    /// }
    ///
    /// fn use_it() -> Result<i32> {
    ///     let value = lookup("answer")?;  // returns early with the NotFound error
    ///     Ok(value + 1)
    /// }
    /// ```
    pub trait Error {
        /// Returns a human-readable description of the error.
        fn description(self: Self) -> String;
    }
}

/// Generator functions and the `yield` keyword.
///
/// A *generator* is a function that produces a sequence of values one at a time, suspending between
/// each. Any `fn` whose body contains a `yield` is a generator function; calling it returns a
/// [`Generator`] value rather than running the body.
/// The body only runs as the generator is driven, and it picks up exactly where it left off after
/// each `yield`. This lets a function describe a long — or unbounded — sequence lazily, computing
/// each element only when it is asked for.
///
/// # Writing a generator function
///
/// A generator function must declare its element type with a `Generator<V>` return type and produce
/// values with `yield`:
///
/// ``` ignore
/// fn squares(n: i32) -> Generator<i32> {
///     let mut i = 1;
///     while i <= n {
///         yield i * i;   // hand out one value, then suspend
///         i += 1;
///     }
/// }
/// ```
///
/// Each `yield expr` produces one value of type `V` and suspends the function until the next value is
/// requested. Execution resumes on the statement following the `yield`, with all local variables
/// intact. The function ends — and the generator becomes exhausted — when control falls off the end
/// of the body.
///
/// A bare `return;` stops the generator early. Because a generator yields its values rather than
/// returning one, a generator function may **not** `return` a value:
///
/// ``` ignore
/// fn until_zero(values: [i32]) -> Generator<i32> {
///     let mut i = 0;
///     while i < values.len() {
///         if values[i] == 0 {
///             return;        // stop the generator (no value)
///         }
///         yield values[i];
///         i += 1;
///     }
/// }
/// ```
///
/// `yield` may only appear directly in a generator function's own body — not inside a helper function
/// it calls.
///
/// # Key/value generators
///
/// A generator can produce a key alongside each value by declaring two type parameters,
/// `Generator<K, V>`, and yielding a pair with `yield key, value`:
///
/// ``` ignore
/// fn enumerate(items: [String]) -> Generator<i32, String> {
///     let mut i = 0;
///     while i < items.len() {
///         yield i, items[i];
///         i += 1;
///     }
/// }
/// ```
///
/// # Consuming a generator
///
/// The common case is a `for` loop, which drives the generator to exhaustion. The forms mirror map
/// iteration:
///
/// ``` ignore
/// for value in squares(4) {
///     print("{value}\n");                 // 1, 4, 9, 16
/// }
///
/// for index, name in enumerate(names) {
///     print("{index}: {name}\n");
/// }
///
/// for index, _ in enumerate(names) {
///     print("{index}\n");                 // keys only
/// }
/// ```
///
/// A generator can also be driven by hand with its methods (see
/// [`Generator`]): [`next`] advances to the following
/// `yield` and reports whether a value was produced, [`value`] returns that value, and [`key`]
/// returns the key of a `Generator<K, V>`. This is what makes consuming an unbounded generator
/// possible — you decide when to stop:
///
/// ``` ignore
/// fn fib() -> Generator<u64> {
///     let mut a = 0u64;
///     let mut b = 1u64;
///     while true {
///         yield a;
///         let next = a + b;
///         a = b;
///         b = next;
///     }
/// }
///
/// let g = fib();
/// let mut n = 0;
/// while n < 10 && g.next() {
///     print("{g.value()} ");              // 0 1 1 2 3 5 8 13 21 34
///     n += 1;
/// }
/// ```
///
/// Calling [`value`] or [`key`] before the first [`next`], or after [`next`] has returned `false`, is
/// a runtime error.
///
/// A generator is **single-shot**: once it has been exhausted it stays finished, and there is no way
/// to rewind or restart it. To iterate a sequence again, call the generator function again to obtain
/// a fresh generator.
///
/// [`next`]: crate::documentation::Generator::next
/// [`value`]: crate::documentation::Generator::value
/// [`key`]: crate::documentation::Generator::key
pub mod generators { }
