use itsy::extern_rust;

extern_rust!(MyFns, {
    fn printi8(vm: &mut VM, value: i8) {
        println!("print:{}", value);
    }
    fn printi32(vm: &mut VM, value: i32) {
        println!("print:{}", value);
    }
    fn printi64(vm: &mut VM, value: i64) {
        println!("print:{}", value);
    }
    fn printf32(vm: &mut VM, value: f32) {
        println!("print:{}", value);
    }
    fn printf64(vm: &mut VM, value: f64) {
        println!("print:{}", value);
    }
    fn printb(vm: &mut VM, value: bool) {
        println!("print:{}", value);
    }
});

fn main() {
    /*let source = "
        fn main() {
            let x: i32 = 17 + 5 * 4;
            printi32(x);
        }
    ";*/
    /*let source = "
        fn main() {
            let a: f64 = 2;
            let b: f64 = 1;
            let c = a > b; printb(c);
            if a > b { printi(1); } else { printi(0); }
        }
    ";*/
    /*let source = "
        fn fibf(n: i32) -> i32 {
            if n < 2 {
                n
            } else {
                fibf(n - 1) + fibf(n - 2)
            }
        }
        fn main() {
            printi32(fibf(27));
        }
    ";*/
    /*
    let source = "
        fn main() {
            let x: bool = false;
            let y: bool = false;
            let a = 1;
            let b = 1;
            printb(x && !y && (a == b));
        }
    ";
    */
    let source = "
        fn main() {
            let a: i32 = 0;
            printi32(--a);
            printi32(--a);
            printi32(--a);
            printi32(--a);

            let b: i32 = 0;
            printi32(b--);
            printi32(b--);
            printi32(b--);
            printi32(b--);

            let c: i64 = 0;
            printi64(--c);
            printi64(--c);
            printi64(--c);
            printi64(--c);

            let d: i64 = 0;
            printi64(d--);
            printi64(d--);
            printi64(d--);
            printi64(d--);
        }
    ";

    let mut vm = itsy::vm::<MyFns>(source);
    println!("{:}", vm.dump_program());
    let vm_start = ::std::time::Instant::now();
    vm.run();
    let vm_runtime = ::std::time::Instant::now() - vm_start;
    let vm_runtime = vm_runtime.as_secs() as f64 + vm_runtime.subsec_nanos() as f64 * 1e-9;
    println!("vm time: {:.4}s", vm_runtime);
}
