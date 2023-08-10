//! Built-in functions. These will be implemented on the Builtin enum and exec'd via vm::call_builtin()

use crate::prelude::*;
#[cfg(feature="runtime")]
use crate::{StackAddress, StackOffset, bytecode::{HeapRef, HeapRefOp, runtime::{vm::VMState, error::RuntimeErrorKind}}};
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

    [
        /// Itsy builtin types.
        ///
        /// # Basic types
        ///
        /// Unsigned numbers: `u8`, `u16`, `u32`, `u64`.\
        /// Signed numbers: `i8`, `i16`, `i32`, `i64`.\
        /// Floating point numbers: [`f32`](crate::internals::documentation::Float), [`f64`](crate::internals::documentation::Float).\
        /// Character strings: [`String`](crate::internals::documentation::String)
        ///
        /// # Compound types
        ///
        /// Arrays, variable-length lists of values of the same type: [`[ .. ]`](crate::internals::documentation::Array).\
        /// Structs, fixed grouping of multiple types: `struct { .. }`.
    ]

    /// Dynamically sized array type.
    ///
    /// Arrays store a variable amount of `Element`s of the same type. Arrays are a reference type,
    /// meaning that when an array already bound to one variable is bound to another, both of the
    /// variables will point at the same data.
    ///
    /// # Examples
    ///
    /// ``` ignore
    /// let greetings = [
    ///     "Guten Tag!",
    ///     "Bonjour!",
    ///     "Добридень!",
    ///     "¡Hola!",
    ///     "今日は",
    /// ];
    ///
    /// greetings.push("Hello!");
    ///
    /// for greeting in greetings {
    ///     print("- {greeting} -");
    /// }
    /// ```
    ///
    /// This prints `- Guten Tag! -- Bonjour! -- Добридень! -- ¡Hola! -- 今日は -- Hello! -`.
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

        /// Removes the last element from an array and returns it.
        pop(self: Self) -> Element {
            fn <
                array_pop8<T: u8>(this: Array) -> u8,
                array_pop16<T: u16>(this: Array) -> u16,
                array_pop32<T: u32>(this: Array) -> u32,
                array_pop64<T: u64>(this: Array) -> u64,
            >(&mut vm) {
                let index = this.index();
                let offset = vm.heap.item(index).data.len() - size_of::<T>();
                let result = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
                vm.heap.item_mut(index).data.truncate(offset);
                result
            }

            fn array_popx(&mut vm + constructor, this: Array) -> Element { // FIXME: once data enums are usable this needs to return an Option
                let index = this.index();
                let offset = vm.heap.item(index).data.len() - size_of::<HeapRef>();
                let result = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
                vm.heap.item_mut(index).data.truncate(offset);
                vm.refcount_value(result, constructor, HeapRefOp::DecNoFree);
                result
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

        /// Removes and returns the element at position index within the array, shifting all elements after it to the left.
        remove(self: Self, element: u64 ) -> Element {
            fn <
                array_remove8<T: u8>(this: Array, element: StackAddress) -> u8,
                array_remove16<T: u16>(this: Array, element: StackAddress) -> u16,
                array_remove32<T: u32>(this: Array, element: StackAddress) -> u32,
                array_remove64<T: u64>(this: Array, element: StackAddress) -> u64,
            >(&mut vm) {
                const ELEMENT_SIZE: usize = size_of::<T>();
                let offset = element as usize * ELEMENT_SIZE;
                let index = this.index();
                let result: T = vm.heap.read(HeapRef::new(index, offset as StackAddress));
                let data = &mut vm.heap.item_mut(index).data;
                data.copy_within(offset + ELEMENT_SIZE .., offset);
                data.truncate(data.len() - ELEMENT_SIZE);
                result
            }

            fn array_removex(&mut vm + constructor, this: Array, element: StackAddress) -> Element {
                const ELEMENT_SIZE: usize = size_of::<HeapRef>();
                let offset = element as usize * ELEMENT_SIZE;
                let index = this.index();
                let result: HeapRef = vm.heap.read(HeapRef::new(index, offset as StackAddress));
                let data = &mut vm.heap.item_mut(index).data;
                data.copy_within(offset + ELEMENT_SIZE .., offset);
                data.truncate(data.len() - ELEMENT_SIZE);
                vm.refcount_value(result, constructor, HeapRefOp::DecNoFree);
                result
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
                    // todo: split signed/unsigned, use div_euclid for unsigned (can't overflow)
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
                    // todo: split signed/unsigned, use div_euclid for unsigned (can't overflow)
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
                let mut result = String::with_capacity(len as usize); // todo: probably want to over-allocate here since our len is in chars an capacity in bytes
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
        repeat(self: Self, n: u64) -> Self {
            fn string_repeat(this: &str, n: StackAddress) -> String {
                if this.len().checked_mul(n as usize).is_some() {
                    this.repeat(n as usize)
                } else {
                    "".to_string() // FIXME: need error reporting mechanism
                }
            }
        }

        /// Returns the character index of the first character of this string that matches the given string. TODO: Currently returns -1 if not found, need optional support.
        find(self: Self, other: String) -> i64 {
            fn string_find(this: &str, other: &str) -> StackOffset { // Todo: something like Option<StackAddress>
                let mut this_iter = this.char_indices();
                let mut index = 0;
                loop {
                    if let Some((i, c)) = this_iter.next() {
                        if this[i..].starts_with(other) {
                            break;
                        } else {
                            index += 1;
                        }
                    } else {
                        index = -1;
                        break;
                    }
                }
                index
            }
        }

        /// Returns the string representation of given ASCII code.
        from_ascii(char_code: u8) -> Self {
            fn string_from_ascii(char_code: u8) -> String {
                (char_code as char).to_string()
            }
        }
    }
}