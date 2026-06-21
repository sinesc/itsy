use crate::util::*;
use itsy::Program;
use itsy::runtime::{VM, VMState, CallError};

const PRELUDE: &str = "use TestFns::{ ret_u8, ret_u16, ret_u32, ret_u64, ret_i8, ret_i16, ret_i32, ret_i64, ret_f32, ret_f64, ret_bool, ret_string, ret_str };";

/// Builds `code` (which must define its own functions, including `main`) into a ready-to-drive VM.
fn build_vm(code: &str) -> VM<TestFns, Context> {
    let input = format!("{} {}", PRELUDE, code);
    let program = match build_str::<TestFns>(&input) {
        Ok(program) => program,
        Err(err) => {
            let loc = err.loc(&input);
            panic!("{} in line {}, column {}.", err, loc.0, loc.1);
        }
    };
    VM::new(program)
}

#[test]
fn call_primitive_args_and_return() {
    let mut vm = build_vm("
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn main() { }
    ");
    let mut context = Vec::new();
    let result = vm.call_function(&mut context, "add", &[ &2i32, &40i32 ]).unwrap();
    assert_eq!(*result.downcast::<i32>().unwrap(), 42);
}

#[test]
fn call_various_primitive_returns() {
    let mut vm = build_vm("
        fn ret_byte() -> u8 { 200 }
        fn ret_float(x: f64) -> f64 { x * 2.0 }
        fn is_big(x: i32) -> bool { x > 100 }
        fn main() { }
    ");
    let mut context = Vec::new();
    assert_eq!(*vm.call_function(&mut context, "ret_byte", &[]).unwrap().downcast::<u8>().unwrap(), 200u8);
    assert_eq!(*vm.call_function(&mut context, "ret_float", &[ &21.0f64 ]).unwrap().downcast::<f64>().unwrap(), 42.0f64);
    assert_eq!(*vm.call_function(&mut context, "is_big", &[ &5i32 ]).unwrap().downcast::<bool>().unwrap(), false);
    assert_eq!(*vm.call_function(&mut context, "is_big", &[ &500i32 ]).unwrap().downcast::<bool>().unwrap(), true);
}

#[test]
fn call_void_function() {
    let mut vm = build_vm("
        fn noop() { }
        fn main() { }
    ");
    let mut context = Vec::new();
    let result = vm.call_function(&mut context, "noop", &[]).unwrap();
    assert!(result.downcast::<()>().is_ok());
}

#[test]
fn call_string_roundtrip_leaves_heap_clean() {
    let mut vm = build_vm("
        fn greet(name: String) -> String { \"Hello, {name}!\" }
        fn main() { }
    ");
    let mut context = Vec::new();
    let world = "World".to_string();
    let result = vm.call_function(&mut context, "greet", &[ &world ]).unwrap();
    assert_eq!(*result.downcast::<String>().unwrap(), "Hello, World!".to_string());
    // argument string (freed by the callee epilogue) and result string (freed on read) must not leak
    assert_eq!(vm.heap.len(), 1, "heap leaked after string call");
}

#[test]
fn call_function_table_survives_serialization() {
    let input = format!("{} {}", PRELUDE, "
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn greet(name: String) -> String { \"Hi {name}\" }
        fn main() { }
    ");
    let program = build_str::<TestFns>(&input).unwrap();
    // round-trip the program through its serialized form
    let bytes = program.to_bytes();
    let restored = Program::<TestFns>::from_bytes(&bytes).expect("failed to deserialize program");
    let mut vm = VM::new(restored);
    let mut context = Vec::new();
    assert_eq!(*vm.call_function(&mut context, "add", &[ &19i32, &23i32 ]).unwrap().downcast::<i32>().unwrap(), 42);
    let name = "there".to_string();
    assert_eq!(*vm.call_function(&mut context, "greet", &[ &name ]).unwrap().downcast::<String>().unwrap(), "Hi there".to_string());
}

#[test]
fn call_function_not_found() {
    let mut vm = build_vm("fn main() { }");
    let mut context = Vec::new();
    match vm.call_function(&mut context, "nope", &[]) {
        Err(CallError::FunctionNotFound(name)) => assert_eq!(name, "nope"),
        other => panic!("expected FunctionNotFound, got {:?}", other),
    }
}

#[test]
fn call_argument_count_mismatch() {
    let mut vm = build_vm("
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn main() { }
    ");
    let mut context = Vec::new();
    match vm.call_function(&mut context, "add", &[ &1i32 ]) {
        Err(CallError::ArgumentCountMismatch { expected: 2, got: 1 }) => { },
        other => panic!("expected ArgumentCountMismatch, got {:?}", other),
    }
}

#[test]
fn call_argument_type_mismatch() {
    let mut vm = build_vm("
        fn add(a: i32, b: i32) -> i32 { a + b }
        fn main() { }
    ");
    let mut context = Vec::new();
    // pass a u8 where an i32 is expected: exact-type downcast must fail
    match vm.call_function(&mut context, "add", &[ &1u8, &2i32 ]) {
        Err(CallError::ArgumentTypeMismatch { index: 0, expected: "i32" }) => { },
        other => panic!("expected ArgumentTypeMismatch, got {:?}", other),
    }
}

#[test]
fn call_runtime_error_propagates() {
    let mut vm = build_vm("
        fn div(a: i32, b: i32) -> i32 { a / b }
        fn main() { }
    ");
    let mut context = Vec::new();
    match vm.call_function(&mut context, "div", &[ &1i32, &0i32 ]) {
        Err(CallError::Runtime(_)) => { },
        other => panic!("expected Runtime error, got {:?}", other),
    }
}

#[test]
fn call_across_suspend_retains_state() {
    let mut vm = build_vm("
        fn helper(x: i32) -> i32 { x * 2 }
        fn main() {
            let arr = [ 5i32 ];
            ret_i32(arr[0]);
            suspend;
            ret_i32(arr[0]);
        }
    ");
    let mut context: Context = Vec::new();

    // run until suspend
    assert_eq!(vm.run(&mut context).unwrap(), VMState::Suspended);
    assert_all(&context, &[ 5i32 ]);

    // call a helper against the suspended VM; state must be restored to Suspended afterwards
    let result = vm.call_function(&mut context, "helper", &[ &21i32 ]).unwrap();
    assert_eq!(*result.downcast::<i32>().unwrap(), 42);
    assert_eq!(vm.state(), VMState::Suspended);

    // resume: main's retained array is still intact
    assert_eq!(vm.run(&mut context).unwrap(), VMState::Terminated);
    assert_all(&context, &[ 5i32, 5 ]);
}
