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