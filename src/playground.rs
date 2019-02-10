use itsy::extern_rust;

extern_rust!(MyFns, {
    fn printi8(vm: &mut VM, value: i8) {
        println!("print:{}", value);
    }
    fn printi32(vm: &mut VM, value: i32) {
        println!("print:{}", value);
    }
    fn printu32(vm: &mut VM, value: u32) {
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
    fn prints(vm: &mut VM, value: &str) {
        println!("print:{}", value);
    }
    fn strlen(vm: &mut VM, value: &str) -> u32 {
        value.len() as u32
    }
});

fn main() {
    /*let source = "
        let m = 2;
        let g = 3;
        let x = [ [ 1, 2, 3, 4, 5, 6, 7 ] ];
        let y = m + x[0][m*g] * g;
    ";*/
    /*let source = "
        let x = [ [ 1, 2 ] ];
        let y: u32 = x[7][8];
    ";*/
    let source = "
        fn main() {
            let x = [ 1, 2 ];
            let y: u32 = x[0];
            printu32(y);
        }
    ";
    println!("{}", source);
    let mut vm = itsy::vm::<MyFns>(source);
    println!("{:}", vm.dump_program());
    let vm_start = ::std::time::Instant::now();
    vm.run();
    let vm_runtime = ::std::time::Instant::now() - vm_start;
    let vm_runtime = vm_runtime.as_secs() as f64 + vm_runtime.subsec_nanos() as f64 * 1e-9;
    println!("vm time: {:.4}s", vm_runtime);
}
