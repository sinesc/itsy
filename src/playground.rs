use itsy::extern_rust;

extern_rust!(MyFns, u32, {
    fn printi8(&mut context, value: i8) {
        println!("printi8:{}", value);
    }
    fn printi16(&mut context, value: i16) {
        println!("printi16:{}", value);
    }
    fn printi32(&mut context, value: i32) {
        println!("printi32:{}", value);
    }
    fn printi64(&mut context, value: i64) {
        println!("printi64:{}", value);
    }
    fn printu8(&mut context, value: u8) {
        println!("printu8:{}", value);
    }
    fn printu16(&mut context, value: u16) {
        println!("printu16:{}", value);
    }
    fn printu32(&mut context, value: u32) {
        println!("printu32:{}", value);
    }
    fn printu64(&mut context, value: u64) {
        println!("printu64:{}", value);
    }
    fn printf32(&mut context, value: f32) {
        println!("printf32:{}", value);
    }
    fn printf64(&mut context, value: f64) {
        println!("printf64:{}", value);
    }
    fn printb(&mut context, value: bool) {
        println!("printb:{}", value);
    }
    fn strlen(&mut context, value: &str) -> u32 {
        value.len() as u32
    }
    fn prints_ref(&mut context, value: &str) {
        println!("prints:{}", value);
    }
    fn prints_val(&mut context, value: String) {
        println!("prints2:{}", &value);
    }
    fn hello(&mut context) -> String {
        "Hello World, from Rust, with love".to_string()
    }
    fn ret(&mut context, val: u32) {
        *context = val;
    }
});

#[allow(unused_variables)]
fn main() {
    /*
            let x = [ [ 1, 2 ], [ 3, 4 ] ];
            printi32(x[0][0]);
    */
    let source = "
        fn main() {
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
            ret(255);
        }
    ";
    println!("{}", source);
    let mut vm = itsy::vm::<MyFns, u32>(source);
    println!("{:}", vm.dump_program());
    let vm_start = ::std::time::Instant::now();
    let mut result = 0;
    vm.run(&mut result);
    println!("result: {:}", result);
    let vm_runtime = ::std::time::Instant::now() - vm_start;
    let vm_runtime = vm_runtime.as_secs() as f64 + vm_runtime.subsec_nanos() as f64 * 1e-9;
    println!("vm time: {:.4}s", vm_runtime);
}
