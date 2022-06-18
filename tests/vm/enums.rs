use crate::util::*;

#[test]
fn primitive_enum() {
    let result = run("
        enum Simple {
            A, B, C
        }
        fn accept(ty: Simple) {
            if ty == Simple::A {
                ret_u8(1);
            } else if ty == Simple::B {
                ret_u8(2);
            } else if ty == Simple::C {
                ret_u8(3);
            }
        }
        fn main() {
            accept(Simple::A);
            accept(Simple::B);
            accept(Simple::C);
            let b = Simple::B;
            if b != Simple::B {
                ret_u8(101);
            } else {
                ret_u8(4);
            }
        }
    ");
    assert_all(&result, &[ 1u8, 2, 3, 4 ]);
}

#[test]
fn primitive_enum_cast() {
    let result = run("
        enum Simple {
            A = 3, B, C
        }
        fn main() {
            let a = Simple::A;
            ret_u8(a as u8);
            ret_u8(Simple::B as u8);
            if a as u8 == 3 {
                ret_u8(1)
            } else {
                ret_u8(0);
            }
        }
    ");
    assert_all(&result, &[ 3u8, 4, 1 ]);
}

