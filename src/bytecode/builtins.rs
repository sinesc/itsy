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
        vm.heap.item_mut(this.index()).data.extend_from_slice(&value.to_ne_bytes());
    }

    fn array_pushx(&mut vm, this: HeapRef, value: HeapRef) {
        vm.heap.item_mut(this.index()).data.extend_from_slice(&value.to_ne_bytes());
        // TODO refcount
    }

    fn <
        array_pop8<T: u8>(this: HeapRef) -> u8,
        array_pop16<T: u16>(this: HeapRef) -> u16,
        array_pop32<T: u32>(this: HeapRef) -> u32,
        array_pop64<T: u64>(this: HeapRef) -> u64,
    >(&mut vm) {
        let index = this.index();
        let offset = vm.heap.item(index).data.len() - size_of::<T>();
        let result = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
        vm.heap.item_mut(index).data.truncate(offset);
        result
    }

    fn array_popx(&mut vm, this: HeapRef) -> HeapRef {
        let index = this.index();
        let offset = vm.heap.item(index).data.len() - size_of::<HeapRef>();
        let result = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
        vm.heap.item_mut(index).data.truncate(offset);
        result
        // TODO refcount
    }

    fn <
        array_truncate8<T: u8>(this: HeapRef, size: StackAddress),
        array_truncate16<T: u16>(this: HeapRef, size: StackAddress),
        array_truncate32<T: u32>(this: HeapRef, size: StackAddress),
        array_truncate64<T: u64>(this: HeapRef, size: StackAddress),
    >(&mut vm) {
        let index = this.index();
        let current_size = vm.heap.item(index).data.len();
        let new_size = size_of::<T>() * size as usize;
        if new_size < current_size {
            vm.heap.item_mut(index).data.truncate(new_size);
        }
    }

    fn array_truncatex(&mut vm, this: HeapRef, size: StackAddress) {
        let index = this.index();
        let current_size = vm.heap.item(index).data.len();
        let new_size = size_of::<HeapRef>() * size as usize;
        if new_size < current_size {
            vm.heap.item_mut(index).data.truncate(new_size);
        }
        // TODO refcount
    }

    fn <
        array_remove8<T: u8>(this: HeapRef, element: StackAddress) -> u8,
        array_remove16<T: u16>(this: HeapRef, element: StackAddress) -> u16,
        array_remove32<T: u32>(this: HeapRef, element: StackAddress) -> u32,
        array_remove64<T: u64>(this: HeapRef, element: StackAddress) -> u64,
    >(&mut vm) {
        let element_size = size_of::<T>();
        let offset = element as usize * element_size;
        let index = this.index();
        let result: T = vm.heap.read(HeapRef::new(index, offset as StackAddress));
        let data = &mut vm.heap.item_mut(index).data;
        data.copy_within(offset + element_size .., offset);
        data.truncate(data.len() - element_size);
        result
    }

    fn array_removex(&mut vm, this: HeapRef, element: StackAddress) -> HeapRef {
        let element_size = size_of::<HeapRef>();
        let offset = element as usize * element_size;
        let index = this.index();
        let result: HeapRef = vm.heap.read(HeapRef::new(index, offset as StackAddress));
        let data = &mut vm.heap.item_mut(index).data;
        data.copy_within(offset + element_size .., offset);
        data.truncate(data.len() - element_size);
        result
        // TODO refcount
    }
}