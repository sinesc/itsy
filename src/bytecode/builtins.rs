//! Built-in functions. These will be implemented on the Builtin enum and exec'd via vm::builtincall()

use crate::prelude::*;
use crate::{StackAddress};
use crate::bytecode::{HeapRef, runtime::heap::HeapOp};

impl_builtins! {

    fn <
        array_push8<T: u8>(this: HeapRef, value: u8),
        array_push16<T: u16>(this: HeapRef, value: u16),
        array_push32<T: u32>(this: HeapRef, value: u32),
        array_push64<T: u64>(this: HeapRef, value: u64),
    >(&mut vm) {
        vm.heap.extend_from(this.index(), &value.to_ne_bytes());
    }

    fn array_pushx(&mut vm, this: HeapRef, value: HeapRef) {
        vm.heap.extend_from(this.index(), &value.to_ne_bytes());
    }

    fn <
        array_pop8<T: u8>(this: HeapRef) -> u8,
        array_pop16<T: u16>(this: HeapRef) -> u16,
        array_pop32<T: u32>(this: HeapRef) -> u32,
        array_pop64<T: u64>(this: HeapRef) -> u64,
    >(&mut vm) {
        let index = this.index();
        let offset = vm.heap.size_of_item(index) as usize - size_of::<T>();
        let result = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
        vm.heap.truncate(index, offset);
        result
    }

    fn array_popx(&mut vm, this: HeapRef) -> HeapRef {
        let index = this.index();
        let offset = vm.heap.size_of_item(index) as usize - size_of::<HeapRef>();
        let result = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
        vm.heap.truncate(index, offset);
        result
    }

    fn <
        array_truncate8<T: u8>(this: HeapRef, size: StackAddress),
        array_truncate16<T: u16>(this: HeapRef, size: StackAddress),
        array_truncate32<T: u32>(this: HeapRef, size: StackAddress),
        array_truncate64<T: u64>(this: HeapRef, size: StackAddress),
    >(&mut vm) {
        let index = this.index();
        let current_size = vm.heap.size_of_item(index) as usize;
        let new_size = size_of::<T>() * size as usize;
        if new_size < current_size {
            vm.heap.truncate(index, new_size);
        }
    }

    fn array_truncatex(&mut vm, this: HeapRef, size: StackAddress) {
        let index = this.index();
        let current_size = vm.heap.size_of_item(index) as usize;
        let new_size = size_of::<HeapRef>() * size as usize;
        if new_size < current_size {
            vm.heap.truncate(index, new_size);
        }
    }
}