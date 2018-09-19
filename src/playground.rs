#[macro_use]
extern crate itsy;

extern_rust!(MyFns, {
    /// prints given i32 value
    fn print(vm: &mut VM, value: i32) {
        println!("print:{}", value);
    }
    fn hello_world(vm: &mut VM) {
        println!("hello world!");
    }
});

fn main() {

    let source = "
        fn fib(n: i32) -> i32 {
            if n < 2 {
                n
            } else {
                fib(n - 1) + fib(n - 2)
            }
        }
        fn main() -> i32 {
            print(fib(37));
        }
    ";

    let source2 = "
        fn fib(n: i32){
            if n > 2 {
                print(2);
            } else {
                print(0)
            }
        }
        fn main() {
            fib(2);
        }
    ";

    let mut vm = itsy::exec::<MyFns>(source);
    println!("{:}", vm.dump_program());
    let vm_start = ::std::time::Instant::now();
    vm.run();
    let vm_runtime = ::std::time::Instant::now() - vm_start;
    let vm_runtime = vm_runtime.as_secs() as f64 + vm_runtime.subsec_nanos() as f64 * 1e-9;
    println!("vm time: {:.4}s", vm_runtime);
}
