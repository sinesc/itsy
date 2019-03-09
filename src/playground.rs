use itsy::extern_rust;

extern_rust!(MyFns, {
    fn printi8(vm: &mut VM, value: i8) {
        println!("printi8:{}", value);
    }
    fn printi16(vm: &mut VM, value: i16) {
        println!("printi16:{}", value);
    }
    fn printi32(vm: &mut VM, value: i32) {
        println!("printi32:{}", value);
    }
    fn printi64(vm: &mut VM, value: i64) {
        println!("printi64:{}", value);
    }
    fn printu8(vm: &mut VM, value: u8) {
        println!("printu8:{}", value);
    }
    fn printu16(vm: &mut VM, value: u16) {
        println!("printu16:{}", value);
    }
    fn printu32(vm: &mut VM, value: u32) {
        println!("printu32:{}", value);
    }
    fn printu64(vm: &mut VM, value: u64) {
        println!("printu64:{}", value);
    }
    fn printf32(vm: &mut VM, value: f32) {
        println!("printf32:{}", value);
    }
    fn printf64(vm: &mut VM, value: f64) {
        println!("printf64:{}", value);
    }
    fn printb(vm: &mut VM, value: bool) {
        println!("printb:{}", value);
    }
    fn strlen(vm: &mut VM, value: &str) -> u32 {
        value.len() as u32
    }
    fn prints_ref(vm: &mut VM, value: &str) {
        println!("prints:{}", value);
    }
    fn prints_val(vm: &mut VM, value: String) {
        println!("prints2:{}", &value);
    }
    fn hello(vm: &mut VM) -> String {
        "Hello World, from Rust, with love".to_string()
    }
});

#[allow(unused_variables)]
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
            let x = [ [ 1 ] ];
            let y = x[0];
            let z = y[0];
            printu32(z);
        }
    ";
    let source = "
        fn main() {
            let x = [ [ 1, 2 ], [ 3, 4 ] ];
            printi32(x[0][0]);
            let x = -13;
            printi8(x);
            let y = -299;
            printi16(y);
            let z = -299555;
            printi32(z);
            let s = \"Hello World\";
            prints(s);
        }
    ";
    let source = "
        fn main() {
            let x = 3;
            let y = 3;
            if x < y {
                printi32(x);
            } else if x > y {
                printi32(y);
            } else {
                printi32(x + y);
            }
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
