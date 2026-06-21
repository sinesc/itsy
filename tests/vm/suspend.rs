use crate::util::*;
use itsy::runtime::{VM, VMState};

/// Builds `code` (wrapped in main() like the `run` helper) and returns a VM ready to be driven
/// manually so tests can inspect state across `suspend` boundaries.
fn build_vm(code: &str) -> VM<TestFns, Context> {
    let input = if code.find("main()").is_some() {
        format!("use TestFns::{{ ret_u8, ret_u16, ret_u32, ret_u64, ret_i8, ret_i16, ret_i32, ret_i64, ret_f32, ret_f64, ret_bool, ret_string, ret_str }}; {}", code)
    } else {
        format!("use TestFns::{{ ret_u8, ret_u16, ret_u32, ret_u64, ret_i8, ret_i16, ret_i32, ret_i64, ret_f32, ret_f64, ret_bool, ret_string, ret_str }}; fn main() {{ {} }}", code)
    };
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
fn suspend_then_resume_retains_stack_and_heap() {
    let mut vm = build_vm("
        let arr = [ 10u8, 20, 30 ];
        ret_u8(arr[0]);
        suspend;
        // arr must still be alive and unchanged after resuming
        ret_u8(arr[1]);
        arr.push(40u8);
        ret_u8(arr[3]);
    ");
    let mut context = Vec::new();

    // first run: stops at suspend, with the heap-allocated array still live
    let state = vm.run(&mut context).unwrap();
    assert_eq!(state, VMState::Suspended);
    assert_eq!(vm.state(), VMState::Suspended);
    assert_all(&context, &[ 10u8 ]);
    // the array (plus heap slot 0) is retained while suspended
    assert!(vm.heap.len() > 1, "heap data was not retained across suspend");

    // resume via run(): array reads/mutates correctly, proving stack+heap survived
    let state = vm.run(&mut context).unwrap();
    assert_eq!(state, VMState::Terminated);
    assert_all(&context, &[ 10u8, 20, 40 ]);
    // reaching Terminated through run() means the heap-corruption guard passed: the array
    // retained across the suspension was still cleaned up correctly on exit.
}

#[test]
fn suspend_multiple_times() {
    let mut vm = build_vm("
        let mut i = 0u8;
        while i < 3 {
            ret_u8(i);
            suspend;
            i += 1;
        }
        ret_u8(100);
    ");
    let mut context = Vec::new();

    for expect in 0u8..3 {
        let state = vm.run(&mut context).unwrap();
        assert_eq!(state, VMState::Suspended);
        assert_eq!(*context.last().unwrap().downcast_ref::<u8>().unwrap(), expect);
    }

    let state = vm.run(&mut context).unwrap();
    assert_eq!(state, VMState::Terminated);
    assert_all(&context, &[ 0u8, 1, 2, 100 ]);
}

#[test]
fn suspend_resumes_at_following_instruction() {
    // suspend must not interrupt control flow: execution continues right after it.
    let mut vm = build_vm("
        ret_u8(1);
        suspend;
        ret_u8(2);
    ");
    let mut context = Vec::new();

    assert_eq!(vm.run(&mut context).unwrap(), VMState::Suspended);
    assert_all(&context, &[ 1u8 ]);
    assert_eq!(vm.run(&mut context).unwrap(), VMState::Terminated);
    assert_all(&context, &[ 1u8, 2 ]);
}
