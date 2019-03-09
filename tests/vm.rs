use itsy::*;

extern_rust!(TestFns, {
    /// Pushes an i32 to the custom field on the vm.
    fn push_result(vm: &mut VM, value: i32) {
        vm.custom.push(value);
    }
});

/// Run a bit of itsy code and return the vm's custom field (populated by the code).
fn run(code: &str) -> Vec<i32> {
    let mut vm = vm::<TestFns>(&format!("fn main() {{ {} }}", code));
    vm.run();
    vm.custom
}

#[test]
fn comparisons() {
    let result = run("
        let x = 1;
        let y = 2;
        while x <= 3 {
            if x < y {
                push_result(x);
            } else if x > y {
                push_result(y);
            } else {
                push_result(x + y);
            }
            x = x + 1;
        }
    ");

    assert_eq!(result[..], [ 1, 4, 2 ]);
}
