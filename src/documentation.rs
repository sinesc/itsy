
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
//! Structs, product of multiple types: `struct { field1: Type1, ... }`.\
//! Enums, disjoint union (one of n variants, variants can carry data): `enum { A, B(p1, p2, ...), ... }`.
//!
//! # Traits
//!
//! [Intrinsic traits](crate::documentation::traits) are recognized by the compiler. Implementing one for
//! a custom type makes the language functionality it backs work on that type (e.g. `Add` backs `+`, `ToString` backs `as String`).
//!
//! # Generators
//!
//! [Generators](crate::documentation::generators) are functions that lazily produce a sequence of values with
//! the `yield` keyword, returning a [`Generator<V>`](crate::documentation::Generator) that can be iterated with a `for` loop.

#[doc(inline)]
pub use crate::bytecode::builtins::builtin_type_documentation::*;

// Placeholder types referenced by `Map`'s method signatures. Kept private (not rendered as
// standalone types) to mirror how the macro treats `Element`/`UnsignedSelf`.
struct Key { }
struct Value { }

/// Dynamically sized key/value map type.
///
/// Maps associate `Key`s with `Value`s. Like arrays, maps are a reference type: binding a map
/// already bound to one variable to another makes both variables point at the same data.
/// Iteration, indexing (`map[key]`) and the literal syntax (`[ key => value ]`) are part of the
/// language; the methods below are the named operations. The iteration forms mirror arrays:
///
/// - `for value in map` — iterate values.
/// - `for key, value in map` — iterate keys and values.
/// - `for key, _ in map` — iterate keys alone.
///
/// Iteration walks a snapshot taken in insertion order, so the body is free to insert into or remove
/// from the map without disturbing the iteration.
///
/// # Examples
///
/// ``` ignore
/// let ages = [ "Anna" => 30, "Bert" => 42 ];
/// ages.insert("Cleo", 19);
///
/// for name, age in ages {
///     print("{name} is {age}\n");
/// }
/// ```
pub struct Map { }

impl Map {
    /// Inserts a key/value pair into the map, replacing and dropping any previous value stored
    /// under an equal key.
    pub fn insert(self: Self, key: Key, value: Value) { }

    /// Returns the value stored under the given key.
    ///
    /// # Error
    ///
    /// Halts the VM if the key is not present in the map. The VM is resumable.
    pub fn get(self: Self, key: Key) -> Value { }

    /// Returns `true` if the map contains a value for the given key.
    pub fn contains_key(self: Self, key: Key) -> bool { }

    /// Removes the entry stored under the given key, doing nothing if the key is absent.
    pub fn remove(self: Self, key: Key) { }

    /// Removes all entries from the map.
    pub fn clear(self: Self) { }

    /// Returns an array of the map's keys in insertion order.
    pub fn keys(self: Self) -> Array { }

    /// Returns an array of the map's values in insertion order.
    pub fn values(self: Self) -> Array { }

    /// Returns the number of entries in the map.
    pub fn len(self: Self) -> u64 { }

    /// Returns a shallow copy of the map. Keys and values are shared with the original (for reference
    /// types this means both maps observe mutations through the shared values), but the two maps'
    /// sets of entries are independent: inserting into or removing from one does not affect the other.
    pub fn clone(self: Self) -> Map { }
}

/// Success-or-error type, written `Result<T>`.
///
/// A `Result<T>` is either `Ok(value)` carrying a success value of type `T`, or `Err(error)`
/// carrying any value implementing the [`Error`](self::traits::Error) trait. Unlike Rust's
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

/// A lazily-produced sequence of values, written `Generator<V>` or `Generator<K, V>`.
///
/// A generator is what you get by calling a [generator function](self::generators) — any `fn` whose
/// body uses `yield`. Calling such a function does **not** run its body; it returns a `Generator`
/// whose execution is advanced on demand. Each `yield` in the body hands a value (form
/// `Generator<V>`) or a key/value pair (form `Generator<K, V>`) back to whoever is driving the
/// generator, then suspends until the next request. This makes it possible to describe long or even
/// unbounded sequences without ever materializing them as an array.
///
/// Like arrays and maps a generator is a reference type. It is also **single-shot**: once it runs to
/// completion it stays exhausted and cannot be restarted.
///
/// The usual way to consume one is a `for` loop, which drives it to exhaustion for you:
///
/// ``` ignore
/// fn count_up(n: i32) -> Generator<i32> {
///     let mut i = 0;
///     while i < n {
///         yield i;
///         i += 1;
///     }
/// }
///
/// for v in count_up(3) {
///     print("{v}\n");   // 0, 1, 2
/// }
/// ```
///
/// The key/value form is iterated like a map, with `for key, value in g` (or `for key, _ in g` for
/// the keys alone). See the [generators] reference for writing generator functions
/// and for driving a generator by hand with the methods below.
pub struct Generator { }

impl Generator {
    /// Resumes the generator until its next `yield` and returns `true`, or returns `false` once the
    /// generator has finished. After `false` is returned the generator is exhausted and
    /// [`value`](Self::value)/[`key`](Self::key) must no longer be called.
    pub fn next(self: Self) -> bool { }

    /// Returns the value produced by the most recent [`next`](Self::next).
    ///
    /// # Error
    ///
    /// Halts the VM if called before the first `next`, or after `next` has returned `false`. The VM
    /// is resumable.
    pub fn value(self: Self) -> Value { }

    /// Returns the key produced by the most recent [`next`](Self::next). Only available on the
    /// key/value form `Generator<K, V>`.
    ///
    /// # Error
    ///
    /// Halts the VM if called before the first `next`, or after `next` has returned `false`. The VM
    /// is resumable.
    pub fn key(self: Self) -> Key { }
}

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
/// [`Add`](traits::Add), [`Sub`](traits::Sub), [`Mul`](traits::Mul), [`Div`](traits::Div) and
/// [`Rem`](traits::Rem) overload the binary arithmetic operators. They all
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
/// # The equality trait
///
/// [`Eq`](traits::Eq) overloads the `==` and `!=` operators. Its single method
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
pub mod traits {

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