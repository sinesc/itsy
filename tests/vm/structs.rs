use crate::util::*;

#[test]
fn struct_access() {
    let result = run("
        struct Test {
            ba: bool,
            bb: bool,
            ia: i8,
            ib: i16,
            ic: i32,
            id: i64,
            ua: u8,
            ub: u16,
            uc: u32,
            ud: u64,
            fa: f32,
            fb: f64,
        }
        fn main() {
            let x: Test = Test {
                ba: true, bb: false,
                ia: -1, ib: 2, ic: -3, id: 4,
                ua: 1, ub: 2, uc: 3, ud: 4,
                fa: 0.01, fb: 0.001
            };
            ret_i8(x.ia);
            ret_i16(x.ib);
            ret_i32(x.ic);
            ret_i64(x.id);
            ret_u8(x.ua);
            ret_u16(x.ub);
            ret_u32(x.uc);
            ret_u64(x.ud);
            ret_f32(x.fa);
            ret_f64(x.fb);
            ret_bool(x.ba);
            ret_bool(x.bb);
        }
    ");
    assert(&result[0], -1i8);
    assert(&result[1], 2i16);
    assert(&result[2], -3i32);
    assert(&result[3], 4i64);
    assert(&result[4], 1u8);
    assert(&result[5], 2u16);
    assert(&result[6], 3u32);
    assert(&result[7], 4u64);
    assert(&result[8], 0.01f32);
    assert(&result[9], 0.001f64);
    assert(&result[10], true);
    assert(&result[11], false);
}

#[test]
fn struct_nesting() {
    let result = run("
        struct MoreInner {
            one: i8,
            two: i16,
            three: i32,
        }
        struct Inner {
            a: f32,
            b: i64,
            c: MoreInner,
        }
        struct Outer {
            first: u8,
            second: u16,
            third: Inner,
        }
        fn main() {
            let x: Outer = Outer { first: 3, second: 345, third: Inner { a: 3.1415, b: -23, c: MoreInner { one: -1, two: -378, three: -123 } } };
            ret_u8(x.first);
            ret_u16(x.second);
            ret_f32(x.third.a);
            ret_i64(x.third.b);
            ret_i8(x.third.c.one);
            ret_i16(x.third.c.two);
            ret_i32(x.third.c.three);
        }
    ");
    assert(&result[0], 3u8);
    assert(&result[1], 345u16);
    assert(&result[2], 3.1415f32);
    assert(&result[3], -23i64);
    assert(&result[4], -1i8);
    assert(&result[5], -378i16);
    assert(&result[6], -123i32);
}

#[test]
fn struct_array() {
    let result = run("
        struct Inner {
            ia: u8,
            ib: [ u64 ],
        }
        struct Outer {
            oa: i16,
            ob: Inner,
        }
        fn main() {
            let x: Outer = Outer {
                oa: -369,
                ob: Inner {
                    ia: 8,
                    ib: [ 3, 333, 333333 ],
                }
            };
            ret_i16(x.oa);
            ret_u8(x.ob.ia);
            ret_u64(x.ob.ib[0]);
            ret_u64(x.ob.ib[1]);
            ret_u64(x.ob.ib[2]);
        }
    ");
    assert(&result[0], -369i16);
    assert(&result[1], 8u8);
    assert(&result[2], 3u64);
    assert(&result[3], 333u64);
    assert(&result[4], 333333u64);
}

#[test]
fn member_assign_primitive() {
    let result = run("
        struct Inner {
            ia: u8,
            ib: [ u64 ],
        }
        struct Outer {
            oa: i16,
            ob: Inner,
        }
        fn main() {
            let mut x: Outer = Outer {
                oa: 0,
                ob: Inner {
                    ia: 0,
                    ib: [ 0, 0, 0 ],
                }
            };
            x.oa = -369;
            x.ob.ia = 8;
            ret_i16(x.oa);
            ret_u8(x.ob.ia);
        }
    ");
    assert(&result[0], -369i16);
    assert(&result[1], 8u8);
}

#[test]
fn index_assign_primitive() {
    let result = run("
        struct Inner {
            ia: u8,
            ib: [ u64 ],
        }
        struct Outer {
            oa: [ u8 ],
            ob: Inner,
        }
        fn main() {
            let x: Outer = Outer {
                oa: [ 0, 0, 0 ],
                ob: Inner {
                    ia: 0,
                    ib: [ 0, 0, 0 ],
                }
            };
            x.oa[0] = 13;
            x.oa[1] = 133;
            x.oa[2] = 255;
            x.ob.ib[0] = 12345678901;
            x.ob.ib[1] = 12345678902;
            x.ob.ib[2] = 12345678903;
            ret_u8(x.oa[0]);
            ret_u8(x.oa[1]);
            ret_u8(x.oa[2]);
            ret_u64(x.ob.ib[0]);
            ret_u64(x.ob.ib[1]);
            ret_u64(x.ob.ib[2]);
        }
    ");
    assert(&result[0], 13u8);
    assert(&result[1], 133u8);
    assert(&result[2], 255u8);
    assert(&result[3], 12345678901u64);
    assert(&result[4], 12345678902u64);
    assert(&result[5], 12345678903u64);
}
