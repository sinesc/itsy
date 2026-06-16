use crate::util::*;

#[test]
fn dynamic_constructor() {
    let result = run(stringify!(
        struct Test {
            a: u32,
            b: String,
        }
        fn main() {
            let x = Test { a: 1100101, b: "Hello" + "World" };
            ret_string(x.b);
            ret_u32(x.a);
        }
    ));
    assert(&result[0], "HelloWorld".to_string());
    assert(&result[1], 1100101u32);
}

#[test]
fn trait_resolution() {
    let result = run(stringify!(
        struct StructA {
            a: u8,
        }
        struct StructB {
            b: u64,
        }
        trait TestTrait {
            fn required(self: Self, value: u8) -> String;
            fn provided(self: Self, value: u8) -> String {
                self.required(value) + " provided"
            }
        }
        impl TestTrait for StructA {
            fn required(self: Self, value: u8) -> String {
                "A S:" + self.a as String + " P:" + value as String
            }
        }
        impl TestTrait for StructB {
            fn required(self: Self, value: u8) -> String {
                "B S:" + self.b as String + " P:" + value as String
            }
        }
        fn indirect(stuff: TestTrait, first: u8, second: u8) {
            ret_string(stuff.provided(first));
            ret_string(stuff.required(second));
        }
        fn main() {
            let a = StructA { a: 111 };
            let b = StructB { b: 999 };
            indirect(a, 1, 2);
            indirect(b, 3, 4);
            ret_string(a.provided(5));
            ret_string(a.required(6));
            ret_string(b.provided(7));
            ret_string(b.required(8));
        }
    ));
    assert_all(&result, &[
        "A S:111 P:1 provided".to_string(),
        "A S:111 P:2".to_string(),
        "B S:999 P:3 provided".to_string(),
        "B S:999 P:4".to_string(),
        "A S:111 P:5 provided".to_string(),
        "A S:111 P:6".to_string(),
        "B S:999 P:7 provided".to_string(),
        "B S:999 P:8".to_string(),
    ]);
}

#[test]
fn temporary_traitobject() {
    let result = run(stringify!(
        struct Inner {
            x: u8,
        }
        struct Outer {
            inner: Inner,
        }
        trait Trait {
            fn get(self: Self) -> u8;
            fn double(self: Self) -> u8 {
                self.get() * 2
            }
        }
        impl Trait for Outer {
            fn get(self: Self) -> u8 {
                self.inner.x
            }
        }
        fn erase(obj: Trait) -> Trait {
            obj
        }
        fn get(obj: Trait) -> u8 {
            obj.get()
        }
        fn main() {
            let o = erase(Outer { inner: Inner { x: 123 } });
            get(o); // Test discard drop
            ret_u8(erase(o).get());
            ret_u8(get(o));
            ret_u8(erase(o).double());
        }
    ));
    assert_all(&result, &[ 123u8, 123, 246 ]);
}

#[test]
fn trait_self_return() {
    // a trait method returning Self, called on a concrete receiver, through a
    // trait-typed receiver (dynamic dispatch), and from a provided method that
    // itself returns Self
    let result = run(stringify!(
        trait Incrementable {
            fn incremented(self: Self) -> Self;
            fn twice(self: Self) -> Self {
                self.incremented().incremented()
            }
            fn value(self: Self) -> u8;
        }
        struct Test {
            val: u8,
        }
        impl Incrementable for Test {
            fn incremented(self: Self) -> Self {
                Test { val: self.val + 1 }
            }
            fn value(self: Self) -> u8 {
                self.val
            }
        }
        fn bump(x: Incrementable) -> u8 {
            // Self return through a trait-typed (dynamically dispatched) receiver
            x.incremented().value()
        }
        fn main() {
            let base = Test { val: 3 };
            ret_u8(base.incremented().value());            // 4 (concrete chain + field via method)
            ret_u8(base.incremented().incremented().val);  // 5 (concrete Self return, direct field access)
            ret_u8(base.twice().value());                  // 5 (provided method returning Self)
            ret_u8(bump(base));                            // 4 (dynamic dispatch)
        }
    ));
    assert_all(&result, &[ 4u8, 5, 5, 4 ]);
}

#[test]
fn trait_self_composite_return() {
    // a trait method returning a composite over Self ([ Self ]). The nested trait element is
    // serialized as a virtual constructor and resolved per-element at runtime.
    let result = run(stringify!(
        trait Listable {
            fn pair(self: Self) -> [ Self ];
            fn value(self: Self) -> u8;
        }
        struct Test {
            val: u8,
        }
        impl Listable for Test {
            fn pair(self: Self) -> [ Self ] {
                [ Test { val: self.val }, Test { val: self.val + 1 } ]
            }
            fn value(self: Self) -> u8 {
                self.val
            }
        }
        fn main() {
            let base = Test { val: 3 };
            let list = base.pair();
            ret_u8(list[0].value()); // 3
            ret_u8(list[1].value()); // 4
        }
    ));
    assert_all(&result, &[ 3u8, 4 ]);
}

#[test]
fn trait_object_nested_in_composites() {
    // trait objects nested inside a struct field, inside an array, and inside an array-in-struct,
    // including a heterogeneous array of implementors and dynamic dispatch through all of them.
    // The `run` helper additionally asserts the heap fully drains, so refcounting through the
    // virtual (trait-object) constructor markers is exercised here too.
    let result = run(stringify!(
        trait Animal {
            fn speak(self: Self) -> String;
        }
        struct Dog { name: String }
        struct Cat { lives: u8 }
        impl Animal for Dog {
            fn speak(self: Self) -> String { "woof " + self.name }
        }
        impl Animal for Cat {
            fn speak(self: Self) -> String { "meow x" + self.lives as String }
        }
        struct Holder { a: Animal }
        struct Zoo { animals: [ Animal ] }
        fn main() {
            // trait object as a struct field
            let h = Holder { a: Dog { name: "Rex" } };
            ret_string(h.a.speak());

            // heterogeneous array of trait objects nested in a struct
            let zoo = Zoo { animals: [ Dog { name: "A" }, Cat { lives: 9 } ] };
            ret_string(zoo.animals[0].speak());
            ret_string(zoo.animals[1].speak());

            // bare heterogeneous trait-object array, grown via push
            let mut zs: [ Animal ] = [ Dog { name: "B" } ];
            zs.push(Cat { lives: 7 });
            ret_string(zs[0].speak());
            ret_string(zs[1].speak());
        }
    ));
    assert_all(&result, &[
        "woof Rex".to_string(),
        "woof A".to_string(),
        "meow x9".to_string(),
        "woof B".to_string(),
        "meow x7".to_string(),
    ]);
}

#[test]
fn trait_object_field_equality() {
    // equality of structs containing a trait-object field exercises the virtual constructor in the
    // comparison path: equal when concrete type and contents match, unequal on differing contents
    // or differing concrete types.
    let result = run(stringify!(
        trait Animal {
            fn speak(self: Self) -> String;
        }
        struct Dog { name: String }
        struct Cat { name: String }
        impl Animal for Dog {
            fn speak(self: Self) -> String { "woof" }
        }
        impl Animal for Cat {
            fn speak(self: Self) -> String { "meow" }
        }
        struct Holder { a: Animal }
        fn main() {
            let same_a = Holder { a: Dog { name: "Rex" } };
            let same_b = Holder { a: Dog { name: "Rex" } };
            let diff_content = Holder { a: Dog { name: "Fae" } };
            let diff_type = Holder { a: Cat { name: "Rex" } };
            ret_bool(same_a == same_b);      // true:  same type and contents
            ret_bool(same_a == diff_content);// false: same type, different contents
            ret_bool(same_a == diff_type);   // false: different concrete type
        }
    ));
    assert_all(&result, &[ true, false, false ]);
}

#[test]
fn enum_trait_static_dispatch() {
    // a trait implemented on an enum, called directly on a concrete enum value
    let result = run(stringify!(
        trait Describe {
            fn kind(self: Self) -> String;
            fn describe(self: Self) -> String {
                "a " + self.kind()
            }
        }
        enum Shape { Circle(i32), Square(i32) }
        impl Describe for Shape {
            fn kind(self: Self) -> String {
                if self == Shape::Circle(3) { "small circle" } else { "other" }
            }
        }
        fn main() {
            let c = Shape::Circle(3);
            ret_string(c.kind());      // small circle (provided uses required)
            ret_string(c.describe());  // a small circle
            ret_string(Shape::Square(5).describe()); // a other
        }
    ));
    assert_all(&result, &[
        "small circle".to_string(),
        "a small circle".to_string(),
        "a other".to_string(),
    ]);
}

#[test]
fn enum_trait_dynamic_dispatch() {
    // enum and struct implementors dispatched through the same trait-object vtable,
    // including passing a temporary (variant constructor / struct literal) as a trait object
    let result = run(stringify!(
        trait Describe {
            fn kind(self: Self) -> String;
            fn describe(self: Self) -> String {
                "a " + self.kind()
            }
        }
        enum Shape { Circle(i32), Square(i32) }
        struct Dog { name: String }
        impl Describe for Shape {
            fn kind(self: Self) -> String {
                if self == Shape::Circle(3) { "circle" } else { "square" }
            }
        }
        impl Describe for Dog {
            fn kind(self: Self) -> String {
                "dog " + self.name
            }
        }
        fn announce(thing: Describe) {
            ret_string(thing.describe());
        }
        fn main() {
            let c = Shape::Circle(3);
            let d = Dog { name: "Rex" };
            announce(c);                  // a circle  (enum variable)
            announce(Shape::Square(5));   // a square  (enum temporary)
            announce(d);                  // a dog Rex (struct variable)
            announce(Dog { name: "Fae" });// a dog Fae (struct temporary)
        }
    ));
    assert_all(&result, &[
        "a circle".to_string(),
        "a square".to_string(),
        "a dog Rex".to_string(),
        "a dog Fae".to_string(),
    ]);
}
