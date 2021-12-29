//! Built-in functions. These will be implemented on the Builtin enum and exec'd via vm::builtincall()

use crate::prelude::*;
use crate::{StackAddress, StackOffset, STACK_ADDRESS_TYPE, ItemIndex};
use crate::bytecode::{HeapRef, runtime::{stack::{StackOp, StackRelativeOp}, heap::{HeapOp, HeapCmp, HeapRefOp}, vm::{VMState, CopyTarget}}};

impl_builtins! {

    fn <
        array_push8<T: u8>(this: HeapRef, value: u8),
        array_push16<T: u16>(this: HeapRef, value: u16),
        array_push32<T: u32>(this: HeapRef, value: u32),
        array_push64<T: u64>(this: HeapRef, value: u64),
        array_pushx<T: HeapRef>(this: HeapRef, value: HeapRef)
    >(&mut vm) {
        vm.heap.extend_from(this.index(), &value.to_ne_bytes());
    }

    fn <
        array_pop8<T: u8>(this: HeapRef) -> u8,
        array_pop16<T: u16>(this: HeapRef) -> u16,
        array_pop32<T: u32>(this: HeapRef) -> u32,
        array_pop64<T: u64>(this: HeapRef) -> u64,
        array_popx<T: HeapRef>(this: HeapRef) -> HeapRef
    >(&mut vm) {
        let index = this.index();
        let offset = vm.heap.size_of(index) as usize - size_of::<T>();
        let result = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
        vm.heap.truncate(index, offset);
        result
    }
}