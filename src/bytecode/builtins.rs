//! Built-in functions. These will be implemented on the Builtin enum and exec'd via vm::call_builtin()

use crate::prelude::*;
#[cfg(feature="runtime")]
use crate::{StackAddress, ItemIndex, bytecode::{HeapRef, HeapRefOp, Constructor, runtime::{vm::VMState, error::RuntimeErrorKind, map::MAP_EMPTY}}};
use crate::bytecode::macros::impl_builtins;
#[cfg(feature="runtime")]
use std::str::Chars;

/// Appends len chars from source starting at start to target. If len is None the entire remaining source will be copied.
#[cfg(feature="runtime")]
fn append<'a>(start: usize, len: Option<usize>, source: &'a str, target: &mut String) -> Chars<'a> {

    let mut iter = source.chars();
    let mut index = 0;
    // skip over start
    while index < start {
        if let Some(_) = iter.next() {
            index += 1;
        } else {
            return iter;
        }
    }
    // copy given number of chars
    if let Some(len) = len {
        let end = start + len;
        while index < end {
            if let Some(c) = iter.next() {
                target.push(c);
                index += 1;
            } else {
                return iter;
            }
        }
    } else {
        loop {
            if let Some(c) = iter.next() {
                target.push(c);
            } else {
                return iter;
            }
        }
    }
    iter
}

impl_builtins! {

    /// Dynamically sized array type.
    ///
    /// Arrays store a variable amount of `Element`s of the same type. Arrays are a reference type,
    /// meaning that when an array already bound to one variable is bound to another, both of the
    /// variables will point at the same data.
    ///
    /// # Construction
    ///
    /// Create an empty array with an empty literal. Element type will be inferred from push:
    ///
    /// ``` ignore
    /// # fn main() {
    /// let mut names = [ ];
    /// names.push("Alice");
    /// names.push("Bob");
    /// for name in names {
    ///     println("{name}");
    /// }
    /// # }
    /// ```
    ///
    /// Create an array from a literal:
    ///
    /// ``` ignore
    /// # fn main() {
    /// let greetings = [ "hello", "world" ];
    /// for g in greetings {
    ///     println("{g}");
    /// }
    /// # }
    /// ```
    ///
    /// Declare an array:
    ///
    /// ``` ignore
    /// # fn main() {
    /// let mut items: [ String ];
    /// items = [ "first", "second" ];
    /// for item in items {
    ///     println("{item}");
    /// }
    /// # }
    /// ```
    ///
    /// # Iteration
    ///
    /// Iteration mirrors maps:
    ///
    /// - `for value in array` - iterate values.
    /// - `for index, value in array` - iterate indices and values.
    /// - `for index, _ in array` - iterate indices alone.
    ///
    /// The index range is snapshotted at loop entry, so the body may grow the array without extending
    /// the iteration.
    Array {
        /// Returns the length of the array.
        len(self: Self) -> u64 {
            fn <
                array_len8<T: u8>(this: Array) -> StackAddress,
                array_len16<T: u16>(this: Array) -> StackAddress,
                array_len32<T: u32>(this: Array) -> StackAddress,
                array_len64<T: u64>(this: Array) -> StackAddress,
            >(&mut vm) {
                (vm.heap.item(this.index()).data.len() / size_of::<T>()) as StackAddress
            }

            fn array_lenx(&mut vm + constructor, this: Array) -> StackAddress {
                (vm.heap.item(this.index()).data.len() / size_of::<HeapRef>()) as StackAddress
            }
        }

        /// Appends an element to the back of the array.
        push(self: Self, value: Element) {
            fn <
                array_push8<T: u8>(this: Array, value: u8),
                array_push16<T: u16>(this: Array, value: u16),
                array_push32<T: u32>(this: Array, value: u32),
                array_push64<T: u64>(this: Array, value: u64),
            >(&mut vm) {
                vm.heap.item_mut(this.index()).data.extend_from_slice(&value.to_ne_bytes());
            }

            fn array_pushx(&mut vm + constructor, this: Array, value: Element) {
                vm.heap.item_mut(this.index()).data.extend_from_slice(&value.to_ne_bytes());
                vm.refcount_value(value, constructor, HeapRefOp::Inc);
            }
        }

        /// Removes the last element from an array and returns it, or `None` if the array is empty.
        pop(self: Self) -> OptionalElement {
            fn <
                array_pop8<T: u8>(this: Array) -> OptionalElement,
                array_pop16<T: u16>(this: Array) -> OptionalElement,
                array_pop32<T: u32>(this: Array) -> OptionalElement,
                array_pop64<T: u64>(this: Array) -> OptionalElement,
            >(&mut vm) {
                let index = this.index();
                let len = vm.heap.item(index).data.len();
                if len == 0 { return vm.option_none(); }
                let offset = len - size_of::<T>();
                let value: T = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
                vm.heap.item_mut(index).data.truncate(offset);
                vm.option_some_bytes(&value.to_ne_bytes())
            }

            fn array_popx(&mut vm + constructor, this: Array) -> OptionalElement {
                let index = this.index();
                let len = vm.heap.item(index).data.len();
                if len == 0 {
                    vm.option_none()
                } else {
                    let offset = len - size_of::<HeapRef>();
                    let payload: HeapRef = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
                    vm.heap.item_mut(index).data.truncate(offset);
                    vm.refcount_value(payload, constructor, HeapRefOp::DecNoFree);
                    vm.option_some_ref(payload)
                }
            }
        }

        /// Shortens the array, keeping the first len elements and dropping the rest.
        truncate(self: Self, size: u64) {
            fn <
                array_truncate8<T: u8>(this: Array, size: StackAddress),
                array_truncate16<T: u16>(this: Array, size: StackAddress),
                array_truncate32<T: u32>(this: Array, size: StackAddress),
                array_truncate64<T: u64>(this: Array, size: StackAddress),
            >(&mut vm) {
                let index = this.index();
                let current_size = vm.heap.item(index).data.len();
                let new_size = size_of::<T>() * size as usize;
                if new_size < current_size {
                    vm.heap.item_mut(index).data.truncate(new_size);
                }
            }

            fn array_truncatex(&mut vm + constructor, this: Array, size: StackAddress) {
                let index = this.index();
                let current_size = vm.heap.item(index).data.len();
                let new_size = size_of::<HeapRef>() * size as usize;
                if new_size < current_size {
                    let mut cursor = HeapRef::new(index, new_size as StackAddress);
                    let end = HeapRef::new(index, current_size as StackAddress);
                    while cursor < end {
                        let item: HeapRef = vm.heap.read_seq(&mut cursor);
                        vm.refcount_value(item, constructor, HeapRefOp::Dec);
                    }
                    vm.heap.item_mut(index).data.truncate(new_size);
                }
            }
        }

        /// Inserts an element at position index within the array, shifting all elements after it to the right.
        insert(self: Self, index: u64, value: Element) {
            fn <
                array_insert8<T: u8>(this: Array, index: StackAddress, value: u8),
                array_insert16<T: u16>(this: Array, index: StackAddress, value: u16),
                array_insert32<T: u32>(this: Array, index: StackAddress, value: u32),
                array_insert64<T: u64>(this: Array, index: StackAddress, value: u64),
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let heap_index = this.index();
                let data = &mut vm.heap.item_mut(heap_index).data;
                let data_len = data.len();
                data.resize(data_len + ELEMENT_SIZE, 0);
                let index = index as usize;
                data.copy_within(index * ELEMENT_SIZE .. data_len, (index + 1) * ELEMENT_SIZE);
                vm.heap.store(heap_index, (index * ELEMENT_SIZE) as StackAddress, value);
            }

            fn array_insertx(&mut vm + constructor, this: Array, index: u64, value: HeapRef) {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let heap_index = this.index();
                let data = &mut vm.heap.item_mut(heap_index).data;
                let data_len = data.len();
                data.resize(data_len + ELEMENT_SIZE, 0);
                let index = index as usize;
                data.copy_within(index * ELEMENT_SIZE .. data_len, (index + 1) * ELEMENT_SIZE);
                vm.heap.store(heap_index, (index * ELEMENT_SIZE) as StackAddress, value);
                vm.refcount_value(value, constructor, HeapRefOp::Inc);
            }
        }

        /// Returns the element at the given index, or `None` if the index is out of range.
        get(self: Self, element: u64) -> OptionalElement {
            fn <
                array_get8<T: u8>(this: Array, element: StackAddress) -> OptionalElement,
                array_get16<T: u16>(this: Array, element: StackAddress) -> OptionalElement,
                array_get32<T: u32>(this: Array, element: StackAddress) -> OptionalElement,
                array_get64<T: u64>(this: Array, element: StackAddress) -> OptionalElement,
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let offset = element as usize * ELEMENT_SIZE;
                let index = this.index();
                let data = &vm.heap.item(index).data;
                if offset + ELEMENT_SIZE > data.len() {
                    vm.option_none()
                } else {
                    let result: T = vm.heap.read(HeapRef::new(index, offset as StackAddress));
                    vm.option_some_bytes(&result.to_ne_bytes())
                }
            }

            fn array_getx(&mut vm + constructor, this: Array, element: StackAddress) -> OptionalElement {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let offset = element as usize * ELEMENT_SIZE;
                let index = this.index();
                let data = &vm.heap.item(index).data;
                if offset + ELEMENT_SIZE > data.len() {
                    vm.option_none()
                } else {
                    let payload: HeapRef = vm.heap.read(HeapRef::new(index, offset as StackAddress));
                    vm.option_some_ref(payload)
                }
            }
        }

        /// Removes and returns the element at position index within the array, shifting all elements after it to the left.
        /// Returns `None` if the index is out of range.
        remove(self: Self, element: u64 ) -> OptionalElement {
            fn <
                array_remove8<T: u8>(this: Array, element: StackAddress) -> OptionalElement,
                array_remove16<T: u16>(this: Array, element: StackAddress) -> OptionalElement,
                array_remove32<T: u32>(this: Array, element: StackAddress) -> OptionalElement,
                array_remove64<T: u64>(this: Array, element: StackAddress) -> OptionalElement,
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let offset = element as usize * ELEMENT_SIZE;
                let index = this.index();
                let data = &vm.heap.item(index).data;
                if offset + ELEMENT_SIZE > data.len() {
                    vm.option_none()
                } else {
                    let result: T = vm.heap.read(HeapRef::new(index, offset as StackAddress));
                    let data = &mut vm.heap.item_mut(index).data;
                    data.copy_within(offset + ELEMENT_SIZE .., offset);
                    data.truncate(data.len() - ELEMENT_SIZE);
                    vm.option_some_bytes(&result.to_ne_bytes())
                }
            }

            fn array_removex(&mut vm + constructor, this: Array, element: StackAddress) -> OptionalElement {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let offset = element as usize * ELEMENT_SIZE;
                let index = this.index();
                let data = &vm.heap.item(index).data;
                if offset + ELEMENT_SIZE > data.len() {
                    vm.option_none()
                } else {
                    let payload: HeapRef = vm.heap.read(HeapRef::new(index, offset as StackAddress));
                    let data = &mut vm.heap.item_mut(index).data;
                    data.copy_within(offset + ELEMENT_SIZE .., offset);
                    data.truncate(data.len() - ELEMENT_SIZE);
                    vm.refcount_value(payload, constructor, HeapRefOp::DecNoFree);
                    vm.option_some_ref(payload)
                }
            }
        }

        /// Returns a shallow copy of the array.
        clone(self: Self) -> Self {
            fn <
                array_clone8<T: u8>(this: Array) -> Array,
                array_clone16<T: u16>(this: Array) -> Array,
                array_clone32<T: u32>(this: Array) -> Array,
                array_clone64<T: u64>(this: Array) -> Array,
            >(&mut vm) {
                let index = this.index();
                let implementor = vm.heap.item_implementor_index(index);
                let data = vm.heap.item(index).data.clone();
                let new_index = vm.heap.alloc_copy(&data, implementor);
                HeapRef::new(new_index, 0)
            }

            fn array_clonex(&mut vm, this: Array) -> Array {
                let index = this.index();
                let implementor = vm.heap.item_implementor_index(index);
                let data = vm.heap.item(index).data.clone();
                let new_index = vm.heap.alloc_copy(&data, implementor);
                HeapRef::new(new_index, 0)
            }
        }

        /// Reverses the order of elements in the array.
        reverse(self: Self) {
            fn <
                array_reverse8<T: u8>(this: Array),
                array_reverse16<T: u16>(this: Array),
                array_reverse32<T: u32>(this: Array),
                array_reverse64<T: u64>(this: Array),
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let index = this.index();
                let num_elements = vm.heap.item(index).data.len() / ELEMENT_SIZE;
                for i in 0..num_elements/2 {
                    let left_offset = (i * ELEMENT_SIZE) as StackAddress;
                    let right_offset = ((num_elements - i - 1) * ELEMENT_SIZE) as StackAddress;
                    let left: T = vm.heap.load(index, left_offset);
                    let right: T = vm.heap.load(index, right_offset);
                    vm.heap.store(index, left_offset, right);
                    vm.heap.store(index, right_offset, left);
                }
            }

            fn array_reversex(&mut vm + constructor, this: Array) {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let index = this.index();
                let num_elements = vm.heap.item(index).data.len() / ELEMENT_SIZE;
                for i in 0..num_elements/2 {
                    let left_offset = (i * ELEMENT_SIZE) as StackAddress;
                    let right_offset = ((num_elements - i - 1) * ELEMENT_SIZE) as StackAddress;
                    let left: HeapRef = vm.heap.load(index, left_offset);
                    let right: HeapRef = vm.heap.load(index, right_offset);
                    vm.heap.store(index, left_offset, right);
                    vm.heap.store(index, right_offset, left);
                }
            }
        }

        /// Returns true if the array has no elements.
        is_empty(self: Self) -> bool {
            fn <
                array_is_empty8<T: u8>(this: Array) -> bool,
                array_is_empty16<T: u16>(this: Array) -> bool,
                array_is_empty32<T: u32>(this: Array) -> bool,
                array_is_empty64<T: u64>(this: Array) -> bool,
            >(&mut vm) {
                vm.heap.item(this.index()).data.is_empty()
            }

            fn array_is_emptyx(&mut vm + constructor, this: Array) -> bool {
                vm.heap.item(this.index()).data.is_empty()
            }
        }

        /// Removes all elements from the array.
        clear(self: Self) {
            fn <
                array_clear8<T: u8>(this: Array),
                array_clear16<T: u16>(this: Array),
                array_clear32<T: u32>(this: Array),
                array_clear64<T: u64>(this: Array),
            >(&mut vm) {
                vm.heap.item_mut(this.index()).data.clear();
            }

            fn array_clearx(&mut vm + constructor, this: Array) {
                let index = this.index();
                let current_size = vm.heap.item(index).data.len();
                if current_size > 0 {
                    let mut cursor = HeapRef::new(index, 0);
                    let end = HeapRef::new(index, current_size as StackAddress);
                    while cursor < end {
                        let item: HeapRef = vm.heap.read_seq(&mut cursor);
                        vm.refcount_value(item, constructor, HeapRefOp::Dec);
                    }
                }
                vm.heap.item_mut(index).data.clear();
            }
        }

        /// Returns true if the array contains the given element.
        contains(self: Self, value: Element) -> bool {
            fn <
                array_contains8<T: u8>(this: Array, value: u8) -> bool,
                array_contains16<T: u16>(this: Array, value: u16) -> bool,
                array_contains32<T: u32>(this: Array, value: u32) -> bool,
                array_contains64<T: u64>(this: Array, value: u64) -> bool,
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let index = this.index();
                let data = &vm.heap.item(index).data;
                let num_elements = data.len() / ELEMENT_SIZE;
                for i in 0..num_elements {
                    let offset = (i * ELEMENT_SIZE) as StackAddress;
                    let elem: T = vm.heap.read(HeapRef::new(index, offset));
                    if elem == value {
                        return true;
                    }
                }
                false
            }

            fn array_containsx(&mut vm + constructor, this: Array, value: Element) -> bool {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let index = this.index();
                let num_elements = vm.heap.item(index).data.len() / ELEMENT_SIZE;
                let mut found = false;
                for i in 0..num_elements {
                    let offset = (i * ELEMENT_SIZE) as StackAddress;
                    let elem: HeapRef = vm.heap.read(HeapRef::new(index, offset));
                    // Deep equality, consistent with the primitive variants and the `==` operator.
                    if vm.compare_value(elem, value, constructor) {
                        found = true;
                        break;
                    }
                }
                found
            }
        }

        /// Returns the first element of the array, or `None` if the array is empty.
        first(self: Self) -> OptionalElement {
            fn <
                array_first8<T: u8>(this: Array) -> OptionalElement,
                array_first16<T: u16>(this: Array) -> OptionalElement,
                array_first32<T: u32>(this: Array) -> OptionalElement,
                array_first64<T: u64>(this: Array) -> OptionalElement,
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let index = this.index();
                let data = &vm.heap.item(index).data;
                if data.len() < ELEMENT_SIZE {
                    vm.option_none()
                } else {
                    let result: T = vm.heap.read(HeapRef::new(index, 0));
                    vm.option_some_bytes(&result.to_ne_bytes())
                }
            }

            fn array_firstx(&mut vm + constructor, this: Array) -> OptionalElement {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let index = this.index();
                let data = &vm.heap.item(index).data;
                if data.len() < ELEMENT_SIZE {
                    vm.option_none()
                } else {
                    let payload: HeapRef = vm.heap.read(HeapRef::new(index, 0));
                    vm.option_some_ref(payload)
                }
            }
        }

        /// Returns the last element of the array, or `None` if the array is empty.
        last(self: Self) -> OptionalElement {
            fn <
                array_last8<T: u8>(this: Array) -> OptionalElement,
                array_last16<T: u16>(this: Array) -> OptionalElement,
                array_last32<T: u32>(this: Array) -> OptionalElement,
                array_last64<T: u64>(this: Array) -> OptionalElement,
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let index = this.index();
                let data = &vm.heap.item(index).data;
                if data.len() < ELEMENT_SIZE {
                    vm.option_none()
                } else {
                    let offset = data.len() - ELEMENT_SIZE;
                    let result: T = vm.heap.read(HeapRef::new(index, offset as StackAddress));
                    vm.option_some_bytes(&result.to_ne_bytes())
                }
            }

            fn array_lastx(&mut vm + constructor, this: Array) -> OptionalElement {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let index = this.index();
                let data = &vm.heap.item(index).data;
                if data.len() < ELEMENT_SIZE {
                    vm.option_none()
                } else {
                    let offset = data.len() - ELEMENT_SIZE;
                    let payload: HeapRef = vm.heap.read(HeapRef::new(index, offset as StackAddress));
                    vm.option_some_ref(payload)
                }
            }
        }

        /// Appends all elements from another array to this one.
        extend(self: Self, other: Self) {
            fn <
                array_extend8<T: u8>(this: Array, other: Array),
                array_extend16<T: u16>(this: Array, other: Array),
                array_extend32<T: u32>(this: Array, other: Array),
                array_extend64<T: u64>(this: Array, other: Array),
            >(&mut vm) {
                let this_index = this.index();
                let other_index = other.index();
                let other_data = vm.heap.item(other_index).data.clone();
                vm.heap.item_mut(this_index).data.extend_from_slice(&other_data);
            }

            fn array_extendx(&mut vm + constructor, this: Array, other: Array) {
                let this_index = this.index();
                let other_index = other.index();
                let other_data = &vm.heap.item(other_index).data;
                let other_len = other_data.len();
                if other_len > 0 {
                    let mut cursor = HeapRef::new(other_index, 0);
                    let end = HeapRef::new(other_index, other_len as StackAddress);
                    while cursor < end {
                        let item: HeapRef = vm.heap.read_seq(&mut cursor);
                        vm.heap.item_mut(this_index).data.extend_from_slice(&item.to_ne_bytes());
                        vm.refcount_value(item, constructor, HeapRefOp::Inc);
                    }
                }
            }
        }

        /// Swaps the elements at index_a and index_b in place.
        swap(self: Self, index_a: u64, index_b: u64) {
            fn <
                array_swap8<T: u8>(this: Array, index_a: StackAddress, index_b: StackAddress),
                array_swap16<T: u16>(this: Array, index_a: StackAddress, index_b: StackAddress),
                array_swap32<T: u32>(this: Array, index_a: StackAddress, index_b: StackAddress),
                array_swap64<T: u64>(this: Array, index_a: StackAddress, index_b: StackAddress),
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let index = this.index();
                let offset_a = (index_a as usize * ELEMENT_SIZE) as StackAddress;
                let offset_b = (index_b as usize * ELEMENT_SIZE) as StackAddress;
                let a: T = vm.heap.load(index, offset_a);
                let b: T = vm.heap.load(index, offset_b);
                vm.heap.store(index, offset_a, b);
                vm.heap.store(index, offset_b, a);
            }

            fn array_swapx(&mut vm + constructor, this: Array, index_a: StackAddress, index_b: StackAddress) {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let index = this.index();
                let offset_a = (index_a as usize * ELEMENT_SIZE) as StackAddress;
                let offset_b = (index_b as usize * ELEMENT_SIZE) as StackAddress;
                let a: HeapRef = vm.heap.load(index, offset_a);
                let b: HeapRef = vm.heap.load(index, offset_b);
                vm.heap.store(index, offset_a, b);
                vm.heap.store(index, offset_b, a);
            }
        }

        /// Resizes the array to the given size. If larger, new elements are filled with the given value.
        /// If smaller, elements beyond the new size are dropped.
        resize(self: Self, new_size: u64, fill: Element) {
            fn <
                array_resize8<T: u8>(this: Array, new_size: StackAddress, fill: u8),
                array_resize16<T: u16>(this: Array, new_size: StackAddress, fill: u16),
                array_resize32<T: u32>(this: Array, new_size: StackAddress, fill: u32),
                array_resize64<T: u64>(this: Array, new_size: StackAddress, fill: u64),
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let index = this.index();
                let current_size = vm.heap.item(index).data.len();
                let new_data_size = ELEMENT_SIZE * new_size as usize;
                if new_data_size < current_size {
                    vm.heap.item_mut(index).data.truncate(new_data_size);
                } else if new_data_size > current_size {
                    let fill_bytes = fill.to_ne_bytes();
                    let data = &mut vm.heap.item_mut(index).data;
                    data.resize(new_data_size, 0);
                    for i in current_size..new_data_size {
                        data[i] = fill_bytes[i % ELEMENT_SIZE];
                    }
                }
            }

            fn array_resizex(&mut vm + constructor, this: Array, new_size: StackAddress, fill: Element) {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let index = this.index();
                let current_size = vm.heap.item(index).data.len();
                let new_data_size = ELEMENT_SIZE * new_size as usize;
                if new_data_size < current_size {
                    let mut cursor = HeapRef::new(index, new_data_size as StackAddress);
                    let end = HeapRef::new(index, current_size as StackAddress);
                    while cursor < end {
                        let item: HeapRef = vm.heap.read_seq(&mut cursor);
                        vm.refcount_value(item, constructor, HeapRefOp::Dec);
                    }
                    vm.heap.item_mut(index).data.truncate(new_data_size);
                } else if new_data_size > current_size {
                    let extra_elements = (new_data_size - current_size) / ELEMENT_SIZE;
                    let fill_bytes = fill.to_ne_bytes();
                    for _ in 0..extra_elements {
                        vm.heap.item_mut(index).data.extend_from_slice(&fill_bytes);
                    }
                    for _ in 0..extra_elements {
                        vm.refcount_value(fill, constructor, HeapRefOp::Inc);
                    }
                }
            }
        }

        /// Removes the element at the given index by swapping it with the last element.
        /// Returns the removed element, or `None` if the index is out of range.
        /// Order of elements is not preserved.
        swap_remove(self: Self, index: u64) -> OptionalElement {
            fn <
                array_swap_remove8<T: u8>(this: Array, index: StackAddress) -> OptionalElement,
                array_swap_remove16<T: u16>(this: Array, index: StackAddress) -> OptionalElement,
                array_swap_remove32<T: u32>(this: Array, index: StackAddress) -> OptionalElement,
                array_swap_remove64<T: u64>(this: Array, index: StackAddress) -> OptionalElement,
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let heap_index = this.index();
                let data = &vm.heap.item(heap_index).data;
                let num_elements = data.len() / ELEMENT_SIZE;
                let idx = index as usize;
                if idx >= num_elements {
                    vm.option_none()
                } else {
                    let offset = idx * ELEMENT_SIZE;
                    let last_offset = (num_elements - 1) * ELEMENT_SIZE;
                    let result: T = vm.heap.read(HeapRef::new(heap_index, offset as StackAddress));
                    let data = &mut vm.heap.item_mut(heap_index).data;
                    if idx != num_elements - 1 {
                        data.copy_within(last_offset .. last_offset + ELEMENT_SIZE, offset);
                    }
                    data.truncate(data.len() - ELEMENT_SIZE);
                    vm.option_some_bytes(&result.to_ne_bytes())
                }
            }

            fn array_swap_removex(&mut vm + constructor, this: Array, index: StackAddress) -> OptionalElement {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let heap_index = this.index();
                let data = &vm.heap.item(heap_index).data;
                let num_elements = data.len() / ELEMENT_SIZE;
                let idx = index as usize;
                if idx >= num_elements {
                    vm.option_none()
                } else {
                    let offset = idx * ELEMENT_SIZE;
                    let last_offset = (num_elements - 1) * ELEMENT_SIZE;
                    let payload: HeapRef = vm.heap.read(HeapRef::new(heap_index, offset as StackAddress));
                    let data = &mut vm.heap.item_mut(heap_index).data;
                    if idx != num_elements - 1 {
                        data.copy_within(last_offset .. last_offset + ELEMENT_SIZE, offset);
                    }
                    data.truncate(data.len() - ELEMENT_SIZE);
                    // DecNoFree the payload: the array slot's +1 reference is released here.
                    // The payload is then embedded into Some at refcount 0.
                    vm.refcount_value(payload, constructor, HeapRefOp::DecNoFree);
                    vm.option_some_ref(payload)
                }
            }
        }
    }

    /// The signed `i8` to `i64` and unsigned `u8` to `u64` integer types.
    ///
    /// Note: Some methods listed here are only available on signed or only on unsigned types. Due to limitations in the documentation
    /// generation signed and unsigned types cannot currently be listed separately.
    Integer {

        /// The smallest value that can be represented by this integer type.
        MIN() -> Self {
            fn <
                int_MINi8<T: i8>() -> i8,
                int_MINi16<T: i16>() -> i16,
                int_MINi32<T: i32>() -> i32,
                int_MINi64<T: i64>() -> i64,
                int_MINu8<T: u8>() -> u8,
                int_MINu16<T: u16>() -> u16,
                int_MINu32<T: u32>() -> u32,
                int_MINu64<T: u64>() -> u64,
            >(&mut vm) {
                T::MIN
            }
        }

        /// The largest value that can be represented by this integer type.
        MAX() -> Self {
            fn <
                int_MAXi8<T: i8>() -> i8,
                int_MAXi16<T: i16>() -> i16,
                int_MAXi32<T: i32>() -> i32,
                int_MAXi64<T: i64>() -> i64,
                int_MAXu8<T: u8>() -> u8,
                int_MAXu16<T: u16>() -> u16,
                int_MAXu32<T: u32>() -> u32,
                int_MAXu64<T: u64>() -> u64,
            >(&mut vm) {
                T::MAX
            }
        }

        /// The size of this integer type in bits.
        BITS() -> u32 {
            fn <
                int_BITSi8<T: i8>() -> u32,
                int_BITSi16<T: i16>() -> u32,
                int_BITSi32<T: i32>() -> u32,
                int_BITSi64<T: i64>() -> u32,
                int_BITSu8<T: u8>() -> u32,
                int_BITSu16<T: u16>() -> u32,
                int_BITSu32<T: u32>() -> u32,
                int_BITSu64<T: u64>() -> u32,
            >(&mut vm) {
                T::BITS
            }
        }

        /// Computes the absolute value of self.
        ///
        /// # Error
        ///
        /// Returns Integer::MAX() and halts the VM if the absolute value is not representable (as is the case for Integer::MIN).
        /// The VM is resumable.
        abs(self: Self) -> Self {
            fn <
                int_absi8<T: i8>(this: i8) -> i8,
                int_absi16<T: i16>(this: i16) -> i16,
                int_absi32<T: i32>(this: i32) -> i32,
                int_absi64<T: i64>(this: i64) -> i64,
            >(&mut vm) {
                if this == T::MIN {
                    vm.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
                    T::MAX
                } else {
                    this.abs()
                }
            }
        }

        /// Computes the absolute difference between self and other.
        abs_diff(self: Self, other: Self) -> UnsignedSelf {
            fn <
                int_abs_diffi8<T: i8>(this: i8, other: i8) -> u8,
                int_abs_diffi16<T: i16>(this: i16, other: i16) -> u16,
                int_abs_diffi32<T: i32>(this: i32, other: i32) -> u32,
                int_abs_diffi64<T: i64>(this: i64, other: i64) -> u64,
                int_abs_diffu8<T: u8>(this: u8, other: u8) -> u8,
                int_abs_diffu16<T: u16>(this: u16, other: u16) -> u16,
                int_abs_diffu32<T: u32>(this: u32, other: u32) -> u32,
                int_abs_diffu64<T: u64>(this: u64, other: u64) -> u64,
            >(&mut vm) {
                this.abs_diff(other)
            }
        }

        /// Calculates the quotient of Euclidean division of self by rhs.
        ///
        /// # Error
        ///
        /// Returns 0 and halts the VM if `other` is 0. The VM is resumable.\
        /// Returns `self` and halts the VM when an overflow occurs. The VM is resumable.
        div_euclid(self: Self, other: Self) -> Self {
            fn <
                int_div_euclidi8<T: i8>(this: i8, other: i8) -> i8,
                int_div_euclidi16<T: i16>(this: i16, other: i16) -> i16,
                int_div_euclidi32<T: i32>(this: i32, other: i32) -> i32,
                int_div_euclidi64<T: i64>(this: i64, other: i64) -> i64,
                int_div_euclidu8<T: u8>(this: u8, other: u8) -> u8,
                int_div_euclidu16<T: u16>(this: u16, other: u16) -> u16,
                int_div_euclidu32<T: u32>(this: u32, other: u32) -> u32,
                int_div_euclidu64<T: u64>(this: u64, other: u64) -> u64,
            >(&mut vm) {
                if other == 0 {
                    vm.state = VMState::Error(RuntimeErrorKind::DivisionByZero);
                    0
                } else {
                    let result = T::overflowing_div_euclid(this, other);
                    if result.1 {
                        vm.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
                    }
                    result.0
                }
            }
        }

        /// Calculates the least nonnegative remainder of self (mod rhs).
        ///
        /// # Error
        ///
        /// Returns 0 and halts the VM if `other` is 0. The VM is resumable.\
        /// Returns 0 and halts the VM when an overflow occurs. The VM is resumable.
        rem_euclid(self: Self, other: Self) -> Self {
            fn <
                int_rem_euclidi8<T: i8>(this: i8, other: i8) -> i8,
                int_rem_euclidi16<T: i16>(this: i16, other: i16) -> i16,
                int_rem_euclidi32<T: i32>(this: i32, other: i32) -> i32,
                int_rem_euclidi64<T: i64>(this: i64, other: i64) -> i64,
                int_rem_euclidu8<T: u8>(this: u8, other: u8) -> u8,
                int_rem_euclidu16<T: u16>(this: u16, other: u16) -> u16,
                int_rem_euclidu32<T: u32>(this: u32, other: u32) -> u32,
                int_rem_euclidu64<T: u64>(this: u64, other: u64) -> u64,
            >(&mut vm) {
                if other == 0 {
                    vm.state = VMState::Error(RuntimeErrorKind::DivisionByZero);
                    0
                } else {
                    let result = T::overflowing_rem_euclid(this, other);
                    if result.1 {
                        vm.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
                    }
                    result.0
                }
            }
        }

        /// Raises self to the power of exp.
        ///
        /// # Error
        ///
        /// Returns 0 and halts the VM when an overflow occurs. The VM is resumable.
        pow(self: Self, exp: u32) -> Self {
            fn <
                int_powi8<T: i8>(this: i8, exp: u32) -> i8,
                int_powi16<T: i16>(this: i16, exp: u32) -> i16,
                int_powi32<T: i32>(this: i32, exp: u32) -> i32,
                int_powi64<T: i64>(this: i64, exp: u32) -> i64,
                int_powu8<T: u8>(this: u8, exp: u32) -> u8,
                int_powu16<T: u16>(this: u16, exp: u32) -> u16,
                int_powu32<T: u32>(this: u32, exp: u32) -> u32,
                int_powu64<T: u64>(this: u64, exp: u32) -> u64,
            >(&mut vm) {
                let result = T::overflowing_pow(this, exp);
                if result.1 {
                    vm.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
                    0
                } else {
                    result.0
                }
            }
        }

        /// Returns the square root of the number, rounded down.
        ///
        /// # Error
        ///
        /// Returns 0 and halts the VM if self is negative. The VM is resumable.
        isqrt(self: Self) -> Self {
            fn <
                int_isqrti8<T: i8>(this: i8) -> i8,
                int_isqrti16<T: i16>(this: i16) -> i16,
                int_isqrti32<T: i32>(this: i32) -> i32,
                int_isqrti64<T: i64>(this: i64) -> i64,
            >(&mut vm) {
                if this < 0 {
                    vm.state = VMState::Error(RuntimeErrorKind::InvalidArgument);
                    0
                } else {
                    this.isqrt()
                }
            }
            fn <
                int_isqrtu8<T: u8>(this: u8) -> u8,
                int_isqrtu16<T: u16>(this: u16) -> u16,
                int_isqrtu32<T: u32>(this: u32) -> u32,
                int_isqrtu64<T: u64>(this: u64) -> u64,
            >(&mut vm) {
                this.isqrt()
            }
        }

        /// Returns the logarithm of the number with respect to an arbitrary base, rounded down.
        ///
        /// # Error
        ///
        /// Returns 0 and halts the VM if self is less than or equal to zero or base is less than 2. The VM is resumable.
        ilog(self: Self, base: Self) -> u32 {
            fn <
                int_ilogi8<T: i8>(this: i8, base: i8) -> u32,
                int_ilogi16<T: i16>(this: i16, base: i16) -> u32,
                int_ilogi32<T: i32>(this: i32, base: i32) -> u32,
                int_ilogi64<T: i64>(this: i64, base: i64) -> u32,
            >(&mut vm) {
                if this <= 0 || base < 2 {
                    vm.state = VMState::Error(RuntimeErrorKind::InvalidArgument);
                    0
                } else {
                    this.ilog(base)
                }
            }
            fn <
                int_ilogu8<T: u8>(this: u8, base: u8) -> u32,
                int_ilogu16<T: u16>(this: u16, base: u16) -> u32,
                int_ilogu32<T: u32>(this: u32, base: u32) -> u32,
                int_ilogu64<T: u64>(this: u64, base: u64) -> u32,
            >(&mut vm) {
                if this == 0 || base < 2 {
                    vm.state = VMState::Error(RuntimeErrorKind::InvalidArgument);
                    0
                } else {
                    this.ilog(base)
                }
            }
        }

        /// Returns the base 2 logarithm of the number, rounded down.
        ///
        /// # Error
        ///
        /// Returns 0 and halts the VM if self is less than or equal to zero. The VM is resumable.
        ilog2(self: Self) -> u32 {
            fn <
                int_ilog2i8<T: i8>(this: i8) -> u32,
                int_ilog2i16<T: i16>(this: i16) -> u32,
                int_ilog2i32<T: i32>(this: i32) -> u32,
                int_ilog2i64<T: i64>(this: i64) -> u32,
            >(&mut vm) {
                if this <= 0 {
                    vm.state = VMState::Error(RuntimeErrorKind::InvalidArgument);
                    0
                } else {
                    this.ilog2()
                }
            }
            fn <
                int_ilog2u8<T: u8>(this: u8) -> u32,
                int_ilog2u16<T: u16>(this: u16) -> u32,
                int_ilog2u32<T: u32>(this: u32) -> u32,
                int_ilog2u64<T: u64>(this: u64) -> u32,
            >(&mut vm) {
                if this == 0 {
                    vm.state = VMState::Error(RuntimeErrorKind::InvalidArgument);
                    0
                } else {
                    this.ilog2()
                }
            }
        }

        /// Returns the base 10 logarithm of the number, rounded down.
        ///
        /// # Error
        ///
        /// Returns 0 and halts the VM if self is less than or equal to zero. The VM is resumable.
        ilog10(self: Self) -> u32 {
            fn <
                int_ilog10i8<T: i8>(this: i8) -> u32,
                int_ilog10i16<T: i16>(this: i16) -> u32,
                int_ilog10i32<T: i32>(this: i32) -> u32,
                int_ilog10i64<T: i64>(this: i64) -> u32,
            >(&mut vm) {
                if this <= 0 {
                    vm.state = VMState::Error(RuntimeErrorKind::InvalidArgument);
                    0
                } else {
                    this.ilog10()
                }
            }
            fn <
                int_ilog10u8<T: u8>(this: u8) -> u32,
                int_ilog10u16<T: u16>(this: u16) -> u32,
                int_ilog10u32<T: u32>(this: u32) -> u32,
                int_ilog10u64<T: u64>(this: u64) -> u32,
            >(&mut vm) {
                if this == 0 {
                    vm.state = VMState::Error(RuntimeErrorKind::InvalidArgument);
                    0
                } else {
                    this.ilog10()
                }
            }
        }

        /// Returns a number representing sign of self.
        /// - `0` if the number is zero
        /// - `1` if the number is positive
        /// - `-1` if the number is negative
        signum(self: Self) -> Self {
            fn <
                int_signumi8<T: i8>(this: i8) -> i8,
                int_signumi16<T: i16>(this: i16) -> i16,
                int_signumi32<T: i32>(this: i32) -> i32,
                int_signumi64<T: i64>(this: i64) -> i64,
            >(&mut vm) {
                this.signum()
            }
        }

        /// Returns the minimum of the two numbers.
        min(self: Self, other: Self) -> Self {
            fn <
                int_mini8<T: i8>(this: i8, other: i8) -> i8,
                int_mini16<T: i16>(this: i16, other: i16) -> i16,
                int_mini32<T: i32>(this: i32, other: i32) -> i32,
                int_mini64<T: i64>(this: i64, other: i64) -> i64,
                int_minu8<T: u8>(this: u8, other: u8) -> u8,
                int_minu16<T: u16>(this: u16, other: u16) -> u16,
                int_minu32<T: u32>(this: u32, other: u32) -> u32,
                int_minu64<T: u64>(this: u64, other: u64) -> u64,
            >(&mut vm) {
                this.min(other)
            }
        }

        /// Returns the maximum of the two numbers.
        max(self: Self, other: Self) -> Self {
            fn <
                int_maxi8<T: i8>(this: i8, other: i8) -> i8,
                int_maxi16<T: i16>(this: i16, other: i16) -> i16,
                int_maxi32<T: i32>(this: i32, other: i32) -> i32,
                int_maxi64<T: i64>(this: i64, other: i64) -> i64,
                int_maxu8<T: u8>(this: u8, other: u8) -> u8,
                int_maxu16<T: u16>(this: u16, other: u16) -> u16,
                int_maxu32<T: u32>(this: u32, other: u32) -> u32,
                int_maxu64<T: u64>(this: u64, other: u64) -> u64,
            >(&mut vm) {
                this.max(other)
            }
        }

        /// Returns the number of zeros in the binary representation of self.
        count_zeros(self: Self) -> u32 {
            fn <
                int_count_zerosi8<T: i8>(this: i8) -> u32,
                int_count_zerosi16<T: i16>(this: i16) -> u32,
                int_count_zerosi32<T: i32>(this: i32) -> u32,
                int_count_zerosi64<T: i64>(this: i64) -> u32,
                int_count_zerosu8<T: u8>(this: u8) -> u32,
                int_count_zerosu16<T: u16>(this: u16) -> u32,
                int_count_zerosu32<T: u32>(this: u32) -> u32,
                int_count_zerosu64<T: u64>(this: u64) -> u32,
            >(&mut vm) {
                this.count_zeros()
            }
        }

        /// Returns the number of ones in the binary representation of self.
        count_ones(self: Self) -> u32 {
            fn <
                int_count_onesi8<T: i8>(this: i8) -> u32,
                int_count_onesi16<T: i16>(this: i16) -> u32,
                int_count_onesi32<T: i32>(this: i32) -> u32,
                int_count_onesi64<T: i64>(this: i64) -> u32,
                int_count_onesu8<T: u8>(this: u8) -> u32,
                int_count_onesu16<T: u16>(this: u16) -> u32,
                int_count_onesu32<T: u32>(this: u32) -> u32,
                int_count_onesu64<T: u64>(this: u64) -> u32,
            >(&mut vm) {
                this.count_ones()
            }
        }

        /// Returns the number of leading zeros in the binary representation of self.
        leading_zeros(self: Self) -> u32 {
            fn <
                int_leading_zerosi8<T: i8>(this: i8) -> u32,
                int_leading_zerosi16<T: i16>(this: i16) -> u32,
                int_leading_zerosi32<T: i32>(this: i32) -> u32,
                int_leading_zerosi64<T: i64>(this: i64) -> u32,
                int_leading_zerosu8<T: u8>(this: u8) -> u32,
                int_leading_zerosu16<T: u16>(this: u16) -> u32,
                int_leading_zerosu32<T: u32>(this: u32) -> u32,
                int_leading_zerosu64<T: u64>(this: u64) -> u32,
            >(&mut vm) {
                this.leading_zeros()
            }
        }

        /// Returns the number of leading ones in the binary representation of self.
        leading_ones(self: Self) -> u32 {
            fn <
                int_leading_onesi8<T: i8>(this: i8) -> u32,
                int_leading_onesi16<T: i16>(this: i16) -> u32,
                int_leading_onesi32<T: i32>(this: i32) -> u32,
                int_leading_onesi64<T: i64>(this: i64) -> u32,
                int_leading_onesu8<T: u8>(this: u8) -> u32,
                int_leading_onesu16<T: u16>(this: u16) -> u32,
                int_leading_onesu32<T: u32>(this: u32) -> u32,
                int_leading_onesu64<T: u64>(this: u64) -> u32,
            >(&mut vm) {
                this.leading_ones()
            }
        }

        /// Returns the number of trailing zeros in the binary representation of self.
        trailing_zeros(self: Self) -> u32 {
            fn <
                int_trailing_zerosi8<T: i8>(this: i8) -> u32,
                int_trailing_zerosi16<T: i16>(this: i16) -> u32,
                int_trailing_zerosi32<T: i32>(this: i32) -> u32,
                int_trailing_zerosi64<T: i64>(this: i64) -> u32,
                int_trailing_zerosu8<T: u8>(this: u8) -> u32,
                int_trailing_zerosu16<T: u16>(this: u16) -> u32,
                int_trailing_zerosu32<T: u32>(this: u32) -> u32,
                int_trailing_zerosu64<T: u64>(this: u64) -> u32,
            >(&mut vm) {
                this.trailing_zeros()
            }
        }

        /// Returns the number of trailing ones in the binary representation of self.
        trailing_ones(self: Self) -> u32 {
            fn <
                int_trailing_onesi8<T: i8>(this: i8) -> u32,
                int_trailing_onesi16<T: i16>(this: i16) -> u32,
                int_trailing_onesi32<T: i32>(this: i32) -> u32,
                int_trailing_onesi64<T: i64>(this: i64) -> u32,
                int_trailing_onesu8<T: u8>(this: u8) -> u32,
                int_trailing_onesu16<T: u16>(this: u16) -> u32,
                int_trailing_onesu32<T: u32>(this: u32) -> u32,
                int_trailing_onesu64<T: u64>(this: u64) -> u32,
            >(&mut vm) {
                this.trailing_ones()
            }
        }

        /// Shifts the bits to the left by a specified amount, n, wrapping the truncated bits to the end of the resulting integer.
        rotate_left(self: Self, n: u32) -> Self {
            fn <
                int_rotate_lefti8<T: i8>(this: i8, n: u32) -> i8,
                int_rotate_lefti16<T: i16>(this: i16, n: u32) -> i16,
                int_rotate_lefti32<T: i32>(this: i32, n: u32) -> i32,
                int_rotate_lefti64<T: i64>(this: i64, n: u32) -> i64,
                int_rotate_leftu8<T: u8>(this: u8, n: u32) -> u8,
                int_rotate_leftu16<T: u16>(this: u16, n: u32) -> u16,
                int_rotate_leftu32<T: u32>(this: u32, n: u32) -> u32,
                int_rotate_leftu64<T: u64>(this: u64, n: u32) -> u64,
            >(&mut vm) {
                this.rotate_left(n)
            }
        }

        /// Shifts the bits to the right by a specified amount, n, wrapping the truncated bits to the beginning of the resulting integer.
        rotate_right(self: Self, n: u32) -> Self {
            fn <
                int_rotate_righti8<T: i8>(this: i8, n: u32) -> i8,
                int_rotate_righti16<T: i16>(this: i16, n: u32) -> i16,
                int_rotate_righti32<T: i32>(this: i32, n: u32) -> i32,
                int_rotate_righti64<T: i64>(this: i64, n: u32) -> i64,
                int_rotate_rightu8<T: u8>(this: u8, n: u32) -> u8,
                int_rotate_rightu16<T: u16>(this: u16, n: u32) -> u16,
                int_rotate_rightu32<T: u32>(this: u32, n: u32) -> u32,
                int_rotate_rightu64<T: u64>(this: u64, n: u32) -> u64,
            >(&mut vm) {
                this.rotate_right(n)
            }
        }
    }

    /// The `f32` and `f64` floating point types.
    ///
    /// Value types that can represent a wide range of decimal numbers. `f64` has increased precision
    /// over `f32`.
    ///
    /// # Examples
    ///
    /// ``` ignore
    /// let approx: f64 = 355.0; // also try this with `f32`
    /// let imation = 113.0;
    /// let pi_approx = (approx / imation).fmt(7);
    ///
    /// println("π is approximately {pi_approx}.");
    /// ```
    ///
    /// This prints `π is approximately 3.1415929.`.
    ///
    /// Note: Much of this documentation is taken from the standard library documentation as these methods thinly wrap over their Rust counterparts.
    Float {

        /// Machine epsilon value for float type.
        EPSILON() -> Self {
            fn <
                float_EPSILON32<T: f32>() -> f32,
                float_EPSILON64<T: f64>() -> f64,
            >(&mut vm) {
                T::EPSILON
            }
        }

        /// Smallest finite float value.
        MIN() -> Self {
            fn <
                float_MIN32<T: f32>() -> f32,
                float_MIN64<T: f64>() -> f64,
            >(&mut vm) {
                T::MIN
            }
        }

        /// Smallest positive normal float value.
        MIN_POSITIVE() -> Self {
            fn <
                float_MIN_POSITIVE32<T: f32>() -> f32,
                float_MIN_POSITIVE64<T: f64>() -> f64,
            >(&mut vm) {
                T::MIN_POSITIVE
            }
        }

        /// Largest finite float value.
        MAX() -> Self {
            fn <
                float_MAX32<T: f32>() -> f32,
                float_MAX64<T: f64>() -> f64,
            >(&mut vm) {
                T::MAX
            }
        }

        /// Not a Number (NaN).
        NAN() -> Self {
            fn <
                float_NAN32<T: f32>() -> f32,
                float_NAN64<T: f64>() -> f64,
            >(&mut vm) {
                T::NAN
            }
        }

        /// Infinity (∞).
        INFINITY() -> Self {
            fn <
                float_INFINITY32<T: f32>() -> f32,
                float_INFINITY64<T: f64>() -> f64,
            >(&mut vm) {
                T::INFINITY
            }
        }

        /// Negative infinity (−∞).
        NEG_INFINITY() -> Self {
            fn <
                float_NEG_INFINITY32<T: f32>() -> f32,
                float_NEG_INFINITY64<T: f64>() -> f64,
            >(&mut vm) {
                T::NEG_INFINITY
            }
        }

        /// Returns the largest integer less than or equal to a number.
        floor(self: Self) -> Self {
            fn <
                float_floor32<T: f32>(this: f32) -> f32,
                float_floor64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.floor()
            }
        }

        /// Returns the smallest integer greater than or equal to a number.
        ceil(self: Self) -> Self {
            fn <
                float_ceil32<T: f32>(this: f32) -> f32,
                float_ceil64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.ceil()
            }
        }

        /// Returns the nearest integer to a number. Round half-way cases away from 0.0.
        round(self: Self) -> Self {
            fn <
                float_round32<T: f32>(this: f32) -> f32,
                float_round64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.round()
            }
        }

        /// Returns a string of a number formatted to the given number of decimals.
        fmt(self: Self, decimals: u32) -> String {
            fn <
                float_format32<T: f32>(this: f32, decimals: u32) -> String,
                float_format64<T: f64>(this: f64, decimals: u32) -> String,
            >(&mut vm) {
                format!("{:.1$}", this, decimals as usize)
            }
        }

        /// Returns the integer part of a number.
        trunc(self: Self) -> Self {
            fn <
                float_trunc32<T: f32>(this: f32) -> f32,
                float_trunc64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.trunc()
            }
        }

        /// Returns the fractional part of a number.
        fract(self: Self) -> Self {
            fn <
                float_fract32<T: f32>(this: f32) -> f32,
                float_fract64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.fract()
            }
        }

        /// Computes the absolute value of self. Returns NAN if the number is NAN.
        abs(self: Self) -> Self {
            fn <
                float_abs32<T: f32>(this: f32) -> f32,
                float_abs64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.abs()
            }
        }

        /// Returns a number that represents the sign of self.
        signum(self: Self) -> Self {
            fn <
                float_signum32<T: f32>(this: f32) -> f32,
                float_signum64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.signum()
            }
        }

        /// Calculates Euclidean division, the matching method for rem_euclid.
        div_euclid(self: Self, rhs: Self) -> Self {
            fn <
                float_div_euclid32<T: f32>(this: f32, rhs: f32) -> f32,
                float_div_euclid64<T: f64>(this: f64, rhs: f64) -> f64,
            >(&mut vm) {
                this.div_euclid(rhs)
            }
        }

        /// Calculates the least nonnegative remainder of self (mod rhs).
        rem_euclid(self: Self, rhs: Self) -> Self {
            fn <
                float_rem_euclid32<T: f32>(this: f32, rhs: f32) -> f32,
                float_rem_euclid64<T: f64>(this: f64, rhs: f64) -> f64,
            >(&mut vm) {
                this.rem_euclid(rhs)
            }
        }

        /// Raises a number to an integer power.
        powi(self: Self, n: i32) -> Self {
            fn <
                float_powi32<T: f32>(this: f32, n: i32) -> f32,
                float_powi64<T: f64>(this: f64, n: i32) -> f64,
            >(&mut vm) {
                this.powi(n)
            }
        }

        /// Raises a number to a floating point power.
        powf(self: Self, n: Self) -> Self {
            fn <
                float_powf32<T: f32>(this: f32, n: f32) -> f32,
                float_powf64<T: f64>(this: f64, n: f64) -> f64,
            >(&mut vm) {
                this.powf(n)
            }
        }

        /// Returns the square root of a number.
        sqrt(self: Self) -> Self {
            fn <
                float_sqrt32<T: f32>(this: f32) -> f32,
                float_sqrt64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.sqrt()
            }
        }

        /// Returns e^(self), (the exponential function).
        exp(self: Self) -> Self {
            fn <
                float_exp32<T: f32>(this: f32) -> f32,
                float_exp64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.exp()
            }
        }

        /// Returns 2^(self).
        exp2(self: Self) -> Self {
            fn <
                float_exp2_32<T: f32>(this: f32) -> f32,
                float_exp2_64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.exp2()
            }
        }

        /// Returns the natural logarithm of the number.
        ln(self: Self) -> Self {
            fn <
                float_ln32<T: f32>(this: f32) -> f32,
                float_ln64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.ln()
            }
        }

        /// Returns the logarithm of the number with respect to an arbitrary base.
        log(self: Self, base: Self) -> Self {
            fn <
                float_log32<T: f32>(this: f32, base: f32) -> f32,
                float_log64<T: f64>(this: f64, base: f64) -> f64,
            >(&mut vm) {
                this.log(base)
            }
        }

        /// Returns the base 2 logarithm of the number.
        log2(self: Self) -> Self {
            fn <
                float_log2_32<T: f32>(this: f32) -> f32,
                float_log2_64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.log2()
            }
        }

        /// Returns the base 10 logarithm of the number.
        log10(self: Self) -> Self {
            fn <
                float_log10_32<T: f32>(this: f32) -> f32,
                float_log10_64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.log10()
            }
        }

        /// Returns the cube root of a number.
        cbrt(self: Self) -> Self {
            fn <
                float_cbrt32<T: f32>(this: f32) -> f32,
                float_cbrt64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.cbrt()
            }
        }

        /// Calculates the length of the hypotenuse of a right-angle triangle given legs of length x and y.
        hypot(self: Self, other: Self) -> Self {
            fn <
                float_hypot32<T: f32>(this: f32, other: f32) -> f32,
                float_hypot64<T: f64>(this: f64, other: f64) -> f64,
            >(&mut vm) {
                this.hypot(other)
            }
        }

        /// Computes the sine of a number (in radians).
        sin(self: Self) -> Self {
            fn <
                float_sin32<T: f32>(this: f32) -> f32,
                float_sin64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.sin()
            }
        }

        /// Computes the cosine of a number (in radians).
        cos(self: Self) -> Self {
            fn <
                float_cos32<T: f32>(this: f32) -> f32,
                float_cos64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.cos()
            }
        }

        /// Computes the tangent of a number (in radians).
        tan(self: Self) -> Self {
            fn <
                float_tan32<T: f32>(this: f32) -> f32,
                float_tan64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.tan()
            }
        }

        /// Computes the arcsine of a number. Return value is in radians in the range [-pi/2, pi/2] or NaN if the number is outside the range [-1, 1].
        asin(self: Self) -> Self {
            fn <
                float_asin32<T: f32>(this: f32) -> f32,
                float_asin64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.asin()
            }
        }

        /// Computes the arccosine of a number. Return value is in radians in the range [0, pi] or NaN if the number is outside the range [-1, 1].
        acos(self: Self) -> Self {
            fn <
                float_acos32<T: f32>(this: f32) -> f32,
                float_acos64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.acos()
            }
        }

        /// Computes the arctangent of a number. Return value is in radians in the range [-pi/2, pi/2];
        atan(self: Self) -> Self {
            fn <
                float_atan32<T: f32>(this: f32) -> f32,
                float_atan64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.atan()
            }
        }

        /// Computes the four quadrant arctangent of self (y) and other (x) in radians.
        atan2(self: Self, other: Self) -> Self {
            fn <
                float_atan2_32<T: f32>(this: f32, other: f32) -> f32,
                float_atan2_64<T: f64>(this: f64, other: f64) -> f64,
            >(&mut vm) {
                this.atan2(other)
            }
        }

        /// Returns true if this value is NaN.
        is_nan(self: Self) -> bool {
            fn <
                float_is_nan32<T: f32>(this: f32) -> bool,
                float_is_nan64<T: f64>(this: f64) -> bool,
            >(&mut vm) {
                this.is_nan()
            }
        }

        /// Returns true if this value is positive infinity or negative infinity, and false otherwise.
        is_infinite(self: Self) -> bool {
            fn <
                float_is_infinite32<T: f32>(this: f32) -> bool,
                float_is_infinite64<T: f64>(this: f64) -> bool,
            >(&mut vm) {
                this.is_infinite()
            }
        }

        /// Returns true if this number is neither infinite nor NaN.
        is_finite(self: Self) -> bool {
            fn <
                float_is_finite32<T: f32>(this: f32) -> bool,
                float_is_finite64<T: f64>(this: f64) -> bool,
            >(&mut vm) {
                this.is_finite()
            }
        }

        /// Returns true if the number is subnormal.
        is_subnormal(self: Self) -> bool {
            fn <
                float_is_subnormal32<T: f32>(this: f32) -> bool,
                float_is_subnormal64<T: f64>(this: f64) -> bool,
            >(&mut vm) {
                this.is_subnormal()
            }
        }

        /// Returns true if the number is neither zero, infinite, subnormal, or NaN.
        is_normal(self: Self) -> bool {
            fn <
                float_is_normal32<T: f32>(this: f32) -> bool,
                float_is_normal64<T: f64>(this: f64) -> bool,
            >(&mut vm) {
                this.is_normal()
            }
        }

        /// Takes the reciprocal (inverse) of a number, 1/x.
        recip(self: Self) -> Self {
            fn <
                float_recip32<T: f32>(this: f32) -> f32,
                float_recip64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.recip()
            }
        }

        /// Converts radians to degrees.
        to_degrees(self: Self) -> Self {
            fn <
                float_to_degrees32<T: f32>(this: f32) -> f32,
                float_to_degrees64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.to_degrees()
            }
        }

        /// Converts degrees to radians.
        to_radians(self: Self) -> Self {
            fn <
                float_to_radians32<T: f32>(this: f32) -> f32,
                float_to_radians64<T: f64>(this: f64) -> f64,
            >(&mut vm) {
                this.to_radians()
            }
        }

        /// Returns the minimum of the two numbers.
        min(self: Self, other: Self) -> Self {
            fn <
                float_min32<T: f32>(this: f32, other: f32) -> f32,
                float_min64<T: f64>(this: f64, other: f64) -> f64,
            >(&mut vm) {
                this.min(other)
            }
        }

        /// Returns the maximum of the two numbers.
        max(self: Self, other: Self) -> Self {
            fn <
                float_max32<T: f32>(this: f32, other: f32) -> f32,
                float_max64<T: f64>(this: f64, other: f64) -> f64,
            >(&mut vm) {
                this.max(other)
            }
        }

        /// Restrict a value to a certain interval unless it is NaN.
        ///
        /// # Error
        ///
        /// Returns 0.0 and halts the VM if `min > max`, `min` is NaN, or `max` is NaN. The VM is resumable.
        clamp(self: Self, min: Self, max: Self) -> Self {
            fn <
                float_clamp32<T: f32>(this: f32, min: f32, max: f32) -> f32,
                float_clamp64<T: f64>(this: f64, min: f64, max: f64) -> f64,
            >(&mut vm) {
                if min.is_nan() || max.is_nan() || min > max {
                    vm.state = VMState::Error(RuntimeErrorKind::InvalidArgument);
                    0.0
                } else {
                    this.clamp(min, max)
                }
            }
        }
    }

    /// A string of characters.
    ///
    /// Strings are always UTF-8 encoded. Positions within the string refer to UTF-8 character
    /// offsets, not byte offsets. Internally strings are an immutable reference type that behaves like
    /// a mutable value type.
    ///
    /// String literals interpolate expressions encased in curly braces. The expression result needs to
    /// support `as`-casting to String.
    ///
    /// # Examples
    ///
    /// ``` ignore
    /// let hello = "Добридень";
    /// let original_hello = hello;
    /// hello += "!";
    ///
    /// println("{hello} <-> {original_hello}");
    /// println("There are {hello.len()} letters in {hello}");
    /// ```
    ///
    /// This prints `Добридень! <-> Добридень`.
    String {
        /// Returns the length of the string.
        len(self: Self) -> u64 {
            fn string_len(this: &str) -> StackAddress {
                this.chars().count() as StackAddress
            }
        }

        /// Inserts another string into this String at the given UTF-8 character position.
        insert(self: Self, position: u64, other: Self) -> Self {
            fn string_insert(this: &str, position: StackAddress, other: &str) -> String {
                let mut result = String::with_capacity(this.len() + other.len());
                let mut remainder = append(0, Some(position as usize), &this, &mut result);
                let _ = append(0, None, &other, &mut result);
                loop {
                    if let Some(c) = remainder.next() {
                        result.push(c);
                    } else {
                        break;
                    }
                }
                result
            }
        }

        /// Returns a substring of the String, starting at given UTF-8 character position and the given length.
        slice(self: Self, position: u64, len: u64) -> Self {
            fn string_slice(this: &str, position: StackAddress, len: StackAddress) -> String {
                let mut result = String::with_capacity(len as usize);
                let _ = append(position as usize, if len > 0 { Some(len as usize) } else { None }, &this, &mut result);
                result
            }
        }

        /// Returns true if the string starts with the given prefix string.
        starts_with(self: Self, other: Self) -> bool {
            fn string_starts_with(this: &str, other: &str) -> bool {
                this.starts_with(other)
            }
        }

        /// Returns true if the string ends with the given suffix string.
        ends_with(self: Self, other: Self) -> bool {
            fn string_ends_with(this: &str, other: &str) -> bool {
                this.ends_with(other)
            }
        }

        /// Returns a string with leading and trailing whitespace removed.
        trim(self: Self) -> Self {
            fn string_trim(this: &str) -> String {
                this.trim().to_string()
            }
        }

        /// Returns a string slice with leading whitespace removed.
        trim_start(self: Self) -> Self {
            fn string_trim_start(this: &str) -> String {
                this.trim_start().to_string()
            }
        }

        /// Returns a string slice with trailing whitespace removed.
        trim_end(self: Self) -> Self {
            fn string_trim_end(this: &str) -> String {
                this.trim_end().to_string()
            }
        }

        /// Returns true if the string contains a given substring.
        contains(self: Self) -> bool {
            fn string_contains(this: &str, other: &str) -> bool {
                this.contains(other)
            }
        }

        /// Replaces occurences of the given substring with another string.
        replace(self: Self, from: Self, to: Self) -> Self {
            fn string_replace(this: &str, from: &str, to: &str) -> String {
                this.replace(from, to)
            }
        }

        /// Returns the lowercase equivalent of this string.
        to_lowercase(self: Self) -> Self {
            fn string_to_lowercase(this: &str) -> String {
                this.to_lowercase()
            }
        }

        /// Returns the uppercase equivalent of this string.
        to_uppercase(self: Self) -> Self {
            fn string_to_uppercase(this: &str) -> String {
                this.to_uppercase()
            }
        }

        /// Creates a new String by repeating this string n times.
        ///
        /// # Error
        ///
        /// Returns an empty string and halts the VM if the resulting length would overflow. The VM is resumable.
        repeat(self: Self, n: u64) -> Self {
            fn string_repeat(&mut vm, this: String, n: StackAddress) -> String {
                if this.len().checked_mul(n as usize).is_some() {
                    this.repeat(n as usize)
                } else {
                    vm.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
                    "".to_string()
                }
            }
        }

        /// Returns the character index of the first character of this string that matches the given string, or `None` if there is no match.
        find(self: Self, other: String) -> OptionalIndex {
            fn string_find(&mut vm, this: String, other: String) -> OptionalIndex {
                let mut found = None;
                let mut index: StackAddress = 0;
                for (i, _c) in this.char_indices() {
                    if this[i..].starts_with(other.as_str()) {
                        found = Some(index);
                        break;
                    }
                    index += 1;
                }
                match found {
                    Some(index) => vm.option_some_bytes(&index.to_ne_bytes()),
                    None => vm.option_none(),
                }
            }
        }

        /// Pads the string on the left with the given padding string to reach the target length.
        /// The padding string is repeated as needed and truncated to fit exactly.
        /// If the string is already at or longer than the target length, it is returned unchanged.
        pad_start(self: Self, padding: Self, length: u64) -> Self {
            fn string_pad_start(this: &str, padding: &str, length: StackAddress) -> String {
                let this_len = this.chars().count();
                let target = length as usize;
                if this_len >= target {
                    this.to_string()
                } else {
                    let chars_needed = target - this_len;
                    let mut result = String::with_capacity(this.len() + padding.len());
                    let pad_chars: Vec<char> = padding.chars().collect();
                    let pad_len = pad_chars.len();
                    for i in 0..chars_needed {
                        result.push(pad_chars[i % pad_len]);
                    }
                    result.push_str(this);
                    result
                }
            }
        }

        /// Pads the string on the right with the given padding string to reach the target length.
        /// The padding string is repeated as needed and truncated to fit exactly.
        /// If the string is already at or longer than the target length, it is returned unchanged.
        pad_end(self: Self, padding: Self, length: u64) -> Self {
            fn string_pad_end(this: &str, padding: &str, length: StackAddress) -> String {
                let this_len = this.chars().count();
                let target = length as usize;
                if this_len >= target {
                    this.to_string()
                } else {
                    let chars_needed = target - this_len;
                    let mut result = String::with_capacity(this.len() + padding.len());
                    result.push_str(this);
                    let pad_chars: Vec<char> = padding.chars().collect();
                    let pad_len = pad_chars.len();
                    for i in 0..chars_needed {
                        result.push(pad_chars[i % pad_len]);
                    }
                    result
                }
            }
        }

        /// Returns the string representation of given ASCII code.
        from_ascii(char_code: u8) -> Self {
            fn string_from_ascii(char_code: u8) -> String {
                (char_code as char).to_string()
            }
        }
    }

    /// A map (hash table) from keys to values.
    ///
    /// Maps store a variable number of key-value pairs. Keys must be unique within a map.
    /// Maps are a reference type. Insertion order is preserved.
    ///
    /// # Construction
    ///
    /// Create an empty map with an empty literal. Key and value types will be inferred from inserts:
    ///
    /// ``` ignore
    /// # fn main() {
    /// let mut scores = [ => ];
    /// scores.insert("Alice", 100);
    /// scores.insert("Bob", 200);
    /// for name, score in scores {
    ///     println("{name}: {score}");
    /// }
    /// # }
    /// ```
    ///
    /// Create a map from a literal:
    ///
    /// ``` ignore
    /// # fn main() {
    /// let ages = [
    ///     "Alice" => 30,
    ///     "Bob" => 25,
    /// ];
    /// for name, age in ages {
    ///     println("{name} is {age}");
    /// }
    /// # }
    /// ```
    ///
    /// Declare a map:
    ///
    /// ``` ignore
    /// # fn main() {
    /// let mut data: [ String => u64 ];
    /// data = [ "x" => 10, "y" => 20 ];
    /// for key, value in data {
    ///     println("{key}={value}");
    /// }
    /// # }
    /// ```
    ///
    /// # Iteration
    ///
    /// Iteration mirrors arrays:
    ///
    /// - `for value in map` - iterate values.
    /// - `for key, value in map` - iterate keys and values.
    /// - `for key, _ in map` - iterate keys alone.
    Map {
        /// Inserts a key-value pair into the map. If the key already exists, the value is updated.
        insert(self: Self, key: Key, value: Value) {
            fn map_insert(&mut vm + constructor, this: Map, key: Key, value: Value) {
                let (key_ctor, value_ctor) = Constructor::map_sub_constructors(&vm.stack, constructor);
                vm.map_put(this.index(), key, value, key_ctor, value_ctor, true);
                vm.refcount_value(this, constructor, HeapRefOp::Free);
            }
        }

        /// Merges all entries from `other` into this map, overwriting values on key collision.
        extend(self: Self, other: Map) {
            fn map_extend(&mut vm + constructor, this: Map, other: Map) {
                let (key_ctor, value_ctor) = Constructor::map_sub_constructors(&vm.stack, constructor);
                let this_idx = this.index();
                let other_idx = other.index();
                // Self-extend is a no-op (every entry would collide with itself); skipping it also
                // avoids freeing a value box that is still referenced by the entry being rewritten.
                if this_idx != other_idx {
                    // The boxed key/value references are shared into this map; map_put increments them
                    // (retain), so both maps co-own each box and refcounts stay balanced on drop.
                    for (key, value) in vm.map_live_entries(other_idx) {
                        vm.map_put(this_idx, key, value, key_ctor, value_ctor, true);
                    }
                }
                vm.refcount_value(other, constructor, HeapRefOp::Free);
                vm.refcount_value(this, constructor, HeapRefOp::Free);
            }
        }

        /// Looks up a key and returns its value, or `None` if absent.
        get(self: Self, key: Key) -> OptionalValue {
            fn map_get(&mut vm + constructor, this: Map, key: Key) -> OptionalValue {
                let (key_ctor, value_ctor) = Constructor::map_sub_constructors(&vm.stack, constructor);
                let idx = this.index();
                let hr = size_of::<HeapRef>() as StackAddress;
                let found = vm.map_find(idx, key, key_ctor);
                vm.refcount_box_top(key, key_ctor, HeapRefOp::Free);
                vm.refcount_value(this, constructor, HeapRefOp::Free);
                match found {
                    Some(e) => {
                        let value_off = vm.map_entries_offset(idx) + e * 2 * hr + hr;
                        let value: HeapRef = vm.heap.read(HeapRef::new(idx, value_off));
                        if Constructor::is_primitive(&vm.stack, value_ctor) {
                            // Primitive values are boxed on the heap; unbox into inline bytes for Option<primitive>.
                            let n = Constructor::primitive_size(&vm.stack, value_ctor);
                            let bytes = vm.heap.item(value.index()).data[0..n].to_vec();
                            vm.option_some_bytes(&bytes)
                        } else {
                            vm.option_some_ref(value)
                        }
                    },
                    None => vm.option_none(),
                }
            }
        }

        /// Returns true if the map contains the given key.
        contains_key(self: Self, key: Key) -> bool {
            fn map_contains_key(&mut vm + constructor, this: Map, key: Key) -> bool {
                let (key_ctor, _value_ctor) = Constructor::map_sub_constructors(&vm.stack, constructor);
                let found = vm.map_find(this.index(), key, key_ctor).is_some();
                vm.refcount_box_top(key, key_ctor, HeapRefOp::Free);
                vm.refcount_value(this, constructor, HeapRefOp::Free);
                found
            }
        }

        /// Removes an entry by key, doing nothing if absent.
        remove(self: Self, key: Key) {
            fn map_remove(&mut vm + constructor, this: Map, key: Key) {
                let (key_ctor, value_ctor) = Constructor::map_sub_constructors(&vm.stack, constructor);
                let idx = this.index();
                let hr = size_of::<HeapRef>() as StackAddress;
                if let Some(e) = vm.map_find(idx, key, key_ctor) {
                    let entry_off = vm.map_entries_offset(idx) + e * 2 * hr;
                    let stored_key: HeapRef = vm.heap.read(HeapRef::new(idx, entry_off));
                    let stored_value: HeapRef = vm.heap.read(HeapRef::new(idx, entry_off + hr));
                    vm.refcount_box_top(stored_key, key_ctor, HeapRefOp::Dec);
                    vm.refcount_box_top(stored_value, value_ctor, HeapRefOp::Dec);
                    vm.heap.write(HeapRef::new(idx, entry_off), HeapRef::new(MAP_EMPTY, 0));
                    let sa = size_of::<StackAddress>() as StackAddress;
                    let len: StackAddress = vm.heap.load(idx, 0);
                    let len = len - 1;
                    vm.heap.store(idx, 0, len);
                    let n_entries: StackAddress = vm.heap.load(idx, sa);
                    if n_entries >= len.saturating_mul(2) {
                        vm.map_compact(idx);
                    }
                    vm.map_refill_buckets(idx, key_ctor);
                }
                vm.refcount_box_top(key, key_ctor, HeapRefOp::Free);
                vm.refcount_value(this, constructor, HeapRefOp::Free);
            }
        }

        /// Removes all entries from the map.
        clear(self: Self) {
            fn map_clear(&mut vm + constructor, this: Map) {
                let (key_ctor, value_ctor) = Constructor::map_sub_constructors(&vm.stack, constructor);
                let idx = this.index();
                for (key, value) in vm.map_live_entries(idx) {
                    vm.refcount_box_top(key, key_ctor, HeapRefOp::Dec);
                    vm.refcount_box_top(value, value_ctor, HeapRefOp::Dec);
                }
                let sa = size_of::<StackAddress>() as StackAddress;
                let entries_off = vm.map_entries_offset(idx);
                vm.heap.item_mut(idx).data.truncate(entries_off as usize);
                vm.heap.store(idx, 0, 0 as StackAddress);
                vm.heap.store(idx, sa, 0 as StackAddress);
                let n_buckets = vm.map_n_buckets(idx);
                for b in 0..n_buckets {
                    vm.map_bucket_set(idx, b, 0);
                }
                vm.refcount_value(this, constructor, HeapRefOp::Free);
            }
        }

        /// Returns a new array of the map's keys in insertion order.
        keys(self: Self) -> KeyArray {
            fn map_keys(&mut vm + constructor, this: Map) -> KeyArray {
                let (key_ctor, _value_ctor) = Constructor::map_sub_constructors(&vm.stack, constructor);
                let elem_ctor = key_ctor;
                let entries = vm.map_live_entries(this.index());
                let array_index = vm.heap.alloc_copy(&[], 0);
                if Constructor::is_primitive(&vm.stack, elem_ctor) {
                    let n = Constructor::primitive_size(&vm.stack, elem_ctor);
                    for (key, _value) in &entries {
                        let boxed = *key;
                        let bytes = vm.heap.item(boxed.index()).data[0..n].to_vec();
                        vm.heap.item_mut(array_index).data.extend_from_slice(&bytes);
                    }
                } else {
                    for (key, _value) in &entries {
                        let boxed = *key;
                        vm.heap.item_mut(array_index).data.extend_from_slice(&boxed.to_ne_bytes());
                    }
                }
                vm.refcount_value(this, constructor, HeapRefOp::Free);
                HeapRef::new(array_index, 0)
            }
        }

        /// Returns a new array of the map's values in insertion order.
        values(self: Self) -> ValueArray {
            fn map_values(&mut vm + constructor, this: Map) -> ValueArray {
                let (_key_ctor, value_ctor) = Constructor::map_sub_constructors(&vm.stack, constructor);
                let elem_ctor = value_ctor;
                let entries = vm.map_live_entries(this.index());
                let array_index = vm.heap.alloc_copy(&[], 0);
                if Constructor::is_primitive(&vm.stack, elem_ctor) {
                    let n = Constructor::primitive_size(&vm.stack, elem_ctor);
                    for (_key, value) in &entries {
                        let boxed = *value;
                        let bytes = vm.heap.item(boxed.index()).data[0..n].to_vec();
                        vm.heap.item_mut(array_index).data.extend_from_slice(&bytes);
                    }
                } else {
                    for (_key, value) in &entries {
                        let boxed = *value;
                        vm.heap.item_mut(array_index).data.extend_from_slice(&boxed.to_ne_bytes());
                    }
                }
                vm.refcount_value(this, constructor, HeapRefOp::Free);
                HeapRef::new(array_index, 0)
            }
        }

        /// Returns the number of entries in the map.
        len(self: Self) -> u64 {
            fn map_len(&mut vm + constructor, this: Map) -> StackAddress {
                let len = vm.heap.load(this.index(), 0);
                vm.refcount_value(this, constructor, HeapRefOp::Free);
                len
            }
        }

        /// Returns a shallow copy of the map.
        clone(self: Self) -> Map {
            fn map_clone(&mut vm + constructor, this: Map) -> Map {
                let data = vm.heap.item(this.index()).data.clone();
                let clone_index = vm.heap.alloc_copy(&data, ItemIndex::MAX);
                vm.refcount_value(this, constructor, HeapRefOp::Free);
                HeapRef::new(clone_index, 0)
            }
        }
    }

    /// A lazily-produced sequence of values, written `Generator<V>` or `Generator<K, V>`.
    ///
    /// A generator is what you get by calling a generator function - any `fn` whose body uses
    /// `yield`. Calling such a function does **not** run its body; it returns a `Generator` whose
    /// execution is advanced on demand. Each `yield` in the body hands a value (form
    /// `Generator<V>`) or a key/value pair (form `Generator<K, V>`) back to whoever is driving the
    /// generator, then suspends until the next request.
    ///
    /// Like arrays and maps a generator is a reference type. It is also **single-shot**: once it
    /// runs to completion it stays exhausted and cannot be restarted.
    ///
    /// The usual way to consume one is a `for` loop, which drives it to exhaustion for you.
    ///
    /// # Examples
    ///
    /// ``` ignore
    /// fn count_up(n: i32) -> Generator<i32> {
    ///     let mut i = 0;
    ///     while i < n {
    ///         yield i;
    ///         i += 1;
    ///     }
    /// }
    ///
    /// # fn main() {
    /// for v in count_up(3) {
    ///     print("{v}\n");   // 0, 1, 2
    /// }
    /// # }
    /// ```
    Generator {
        /// Resumes the generator until its next `yield` and returns `true`, or returns `false` once
        /// the generator has finished. After `false` is returned the generator is exhausted and
        /// `value`/`key` must no longer be called.
        next(self: Self) -> bool {
            fn gen_next(this: HeapRef) -> bool {
                // Placeholder; actual execution is via the gen_next opcode.
                // Search for the 'Generator types use dedicated gen_* opcodes' comment in impl_builtins.rs for explanation.
                true
            }
        }

        /// Returns the value produced by the most recent `next`.
        ///
        /// # Error
        ///
        /// Halts the VM if called before the first `next`, or after `next` has returned `false`. The VM
        /// is resumable.
        value(self: Self) -> GenValue {
            fn gen_value(this: HeapRef) -> GenValue {
                HeapRef::new(0, 0) // see gen_next
            }
        }

        /// Returns the key produced by the most recent `next`. Only available on the
        /// key/value form `Generator<K, V>`.
        ///
        /// # Error
        ///
        /// Halts the VM if called before the first `next`, or after `next` has returned `false`. The VM
        /// is resumable.
        key(self: Self) -> GenKey {
            fn gen_key(this: HeapRef) -> GenKey {
                HeapRef::new(0, 0) // see gen_next
            }
        }
    }
}
