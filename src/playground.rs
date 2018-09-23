use itsy::extern_rust;

extern_rust!(MyFns, {
    fn printi(vm: &mut VM, value: i32) {
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
            let a: f64 = 2;
            let b: f64 = 1;
            let c = a > b; printb(c);
            if a > b { printi(1); } else { printi(0); }
        }
    ";*/
    let source = "
        fn fibf(n: f64) -> f64 {
            if n < 2.0 {
                n
            } else {
                fibf(n - 1.0) + fibf(n - 2.0)
            }
        }
        fn main() {
            printf64(fibf(7.0));
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
