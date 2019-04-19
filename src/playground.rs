use itsy::*;

extern_rust!(MyFns, (), {
    fn printi8(&mut context, value: i8) {
        println!("{}i8", value);
    }
    fn printi16(&mut context, value: i16) {
        println!("{}i16", value);
    }
    fn printi32(&mut context, value: i32) {
        println!("{}i32", value);
    }
    fn printi64(&mut context, value: i64) {
        println!("{}i64", value);
    }
    fn printu8(&mut context, value: u8) {
        println!("{}u8", value);
    }
    fn printu16(&mut context, value: u16) {
        println!("{}u16", value);
    }
    fn printu32(&mut context, value: u32) {
        println!("{}u32", value);
    }
    fn printu64(&mut context, value: u64) {
        println!("{}u64", value);
    }
    fn printf32(&mut context, value: f32) {
        println!("{}f32", value);
    }
    fn printf64(&mut context, value: f64) {
        println!("{}f64", value);
    }
    fn printb(&mut context, value: bool) {
        println!("{}", value);
    }
    fn prints(&mut context, value: &str) {
        println!("&\"{}\"", value);
    }
});

#[allow(unused_variables)]
fn main() {
    let source = "
        fn main() {
            prints(\"Hello World\");
        }
    ";
    //println!("source:\n{}", source);
    //println!("ast:\n{:#?}", parse(source));
    //println!("highest opcode id: {}", bytecode::OpCode::comment as u8);
    let mut vm = vm::<MyFns, ()>(source);
    println!("bytecode:\n{:}", vm.format_program());
    let vm_start = std::time::Instant::now();
    vm.run(&mut ());
    //println!("vm time: {:.4}s", (std::time::Instant::now() - vm_start).as_millis() as f32 / 1000.);
}
