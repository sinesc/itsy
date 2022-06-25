//! Built-in functions. These will be implemented on the Builtin enum and exec'd via vm::builtincall()

use std::str::Chars;
use crate::prelude::*;
use crate::{StackAddress, StackOffset};
use crate::bytecode::{HeapRef, runtime::heap::{HeapOp, HeapRefOp}};

/// Appends len chars from source starting at start to target. If len is None the entire remaining source will be copied.
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

    Array {
        /// Appends an element to the back of the array.
        # push(self: Self, value: Any)
        fn <
            array_push8<T: u8>(this: HeapRef, value: u8),
            array_push16<T: u16>(this: HeapRef, value: u16),
            array_push32<T: u32>(this: HeapRef, value: u32),
            array_push64<T: u64>(this: HeapRef, value: u64),
        >(&mut vm) {
            vm.heap.item_mut(this.index()).data.extend_from_slice(&value.to_ne_bytes());
        }

        fn array_pushx(&mut vm + constructor, this: HeapRef, value: HeapRef) {
            vm.heap.item_mut(this.index()).data.extend_from_slice(&value.to_ne_bytes());
            vm.refcount_value(value, constructor, HeapRefOp::Inc);
        }

        /// Removes the last element from an array and returns it.
        # pop(self: Self) -> Any
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

        fn array_popx(&mut vm + constructor, this: HeapRef) -> HeapRef { // FIXME: once data enums are usable this needs to return an Option
            let index = this.index();
            let offset = vm.heap.item(index).data.len() - size_of::<HeapRef>();
            let result = vm.heap.read(HeapRef::new(index as StackAddress, offset as StackAddress));
            vm.heap.item_mut(index).data.truncate(offset);
            vm.refcount_value(result, constructor, HeapRefOp::DecNoFree);
            result
        }

        /// Shortens the array, keeping the first len elements and dropping the rest.
        # truncate(self: Self, size: u64)
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

        fn array_truncatex(&mut vm + constructor, this: HeapRef, size: StackAddress) {
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

        /// Removes and returns the element at position index within the array, shifting all elements after it to the left.
        # remove(self: Self, element: u64 ) -> Any
        fn <
            array_remove8<T: u8>(this: HeapRef, element: StackAddress) -> u8,
            array_remove16<T: u16>(this: HeapRef, element: StackAddress) -> u16,
            array_remove32<T: u32>(this: HeapRef, element: StackAddress) -> u32,
            array_remove64<T: u64>(this: HeapRef, element: StackAddress) -> u64,
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

        fn array_removex(&mut vm + constructor, this: HeapRef, element: StackAddress) -> HeapRef {
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

    Float {
        /// Returns the largest integer less than or equal to a number.
        # floor(self: Self) -> Self
        fn <
            float_floor32<T: u32>(this: f32) -> f32,
            float_floor64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.floor()
        }

        /// Returns the smallest integer greater than or equal to a number.
        # ceil(self: Self) -> Self
        fn <
            float_ceil32<T: u32>(this: f32) -> f32,
            float_ceil64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.ceil()
        }

        /// Returns the nearest integer to a number. Round half-way cases away from 0.0.
        # round(self: Self) -> Self
        fn <
            float_round32<T: u32>(this: f32) -> f32,
            float_round64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.round()
        }

        /// Returns the integer part of a number.
        # trunc(self: Self) -> Self
        fn <
            float_trunc32<T: u32>(this: f32) -> f32,
            float_trunc64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.trunc()
        }

        /// Returns the fractional part of a number.
        # fract(self: Self) -> Self
        fn <
            float_fract32<T: u32>(this: f32) -> f32,
            float_fract64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.fract()
        }

        /// Computes the absolute value of self. Returns NAN if the number is NAN.
        # abs(self: Self) -> Self
        fn <
            float_abs32<T: u32>(this: f32) -> f32,
            float_abs64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.abs()
        }

        /// Returns a number that represents the sign of self.
        # signum(self: Self) -> Self
        fn <
            float_signum32<T: u32>(this: f32) -> f32,
            float_signum64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.signum()
        }

        /// Raises a number to an integer power.
        # powi(self: Self, n: i32) -> Self
        fn <
            float_powi32<T: u32>(this: f32, n: i32) -> f32,
            float_powi64<T: u64>(this: f64, n: i32) -> f64,
        >(&mut vm) {
            this.powi(n)
        }

        /// Raises a number to a floating point power.
        # powf(self: Self, n: Self) -> Self
        fn <
            float_powf32<T: u32>(this: f32, n: f32) -> f32,
            float_powf64<T: u64>(this: f64, n: f64) -> f64,
        >(&mut vm) {
            this.powf(n)
        }

        /// Returns the square root of a number.
        # sqrt(self: Self) -> Self
        fn <
            float_sqrt32<T: u32>(this: f32) -> f32,
            float_sqrt64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.sqrt()
        }

        /// Returns e^(self), (the exponential function).
        # exp(self: Self) -> Self
        fn <
            float_exp32<T: u32>(this: f32) -> f32,
            float_exp64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.exp()
        }

        /// Returns 2^(self).
        # exp2(self: Self) -> Self
        fn <
            float_exp2_32<T: u32>(this: f32) -> f32,
            float_exp2_64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.exp2()
        }

        /// Returns the natural logarithm of the number.
        # ln(self: Self) -> Self
        fn <
            float_ln32<T: u32>(this: f32) -> f32,
            float_ln64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.ln()
        }

        /// Returns the logarithm of the number with respect to an arbitrary base.
        # log(self: Self, base: Self) -> Self
        fn <
            float_log32<T: u32>(this: f32, base: f32) -> f32,
            float_log64<T: u64>(this: f64, base: f64) -> f64,
        >(&mut vm) {
            this.log(base)
        }

        /// Returns the base 2 logarithm of the number.
        # log2(self: Self) -> Self
        fn <
            float_log2_32<T: u32>(this: f32) -> f32,
            float_log2_64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.log2()
        }

        /// Returns the base 10 logarithm of the number.
        # log10(self: Self) -> Self
        fn <
            float_log10_32<T: u32>(this: f32) -> f32,
            float_log10_64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.log10()
        }

        /// Returns the cube root of a number.
        # cbrt(self: Self) -> Self
        fn <
            float_cbrt32<T: u32>(this: f32) -> f32,
            float_cbrt64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.cbrt()
        }

        /// Calculates the length of the hypotenuse of a right-angle triangle given legs of length x and y.
        # hypot(self: Self, other: Self) -> Self
        fn <
            float_hypot32<T: u32>(this: f32, other: f32) -> f32,
            float_hypot64<T: u64>(this: f64, other: f64) -> f64,
        >(&mut vm) {
            this.hypot(other)
        }

        /// Computes the sine of a number (in radians).
        # sin(self: Self) -> Self
        fn <
            float_sin32<T: u32>(this: f32) -> f32,
            float_sin64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.sin()
        }

        /// Computes the cosine of a number (in radians).
        # cos(self: Self) -> Self
        fn <
            float_cos32<T: u32>(this: f32) -> f32,
            float_cos64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.cos()
        }

        /// Computes the tangent of a number (in radians).
        # tan(self: Self) -> Self
        fn <
            float_tan32<T: u32>(this: f32) -> f32,
            float_tan64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.tan()
        }

        /// Computes the arcsine of a number. Return value is in radians in the range [-pi/2, pi/2] or NaN if the number is outside the range [-1, 1].
        # asin(self: Self) -> Self
        fn <
            float_asin32<T: u32>(this: f32) -> f32,
            float_asin64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.asin()
        }

        /// Computes the arccosine of a number. Return value is in radians in the range [0, pi] or NaN if the number is outside the range [-1, 1].
        # acos(self: Self) -> Self
        fn <
            float_acos32<T: u32>(this: f32) -> f32,
            float_acos64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.acos()
        }

        /// Computes the arctangent of a number. Return value is in radians in the range [-pi/2, pi/2];
        # atan(self: Self) -> Self
        fn <
            float_atan32<T: u32>(this: f32) -> f32,
            float_atan64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.atan()
        }

        /// Computes the four quadrant arctangent of self (y) and other (x) in radians.
        # atan2(self: Self, other: Self) -> Self
        fn <
            float_atan2_32<T: u32>(this: f32, other: f32) -> f32,
            float_atan2_64<T: u64>(this: f64, other: f64) -> f64,
        >(&mut vm) {
            this.atan2(other)
        }

        /// Returns true if this value is NaN.
        # is_nan(self: Self) -> bool
        fn <
            float_is_nan_32<T: u32>(this: f32) -> bool,
            float_is_nan_64<T: u64>(this: f64) -> bool,
        >(&mut vm) {
            this.is_nan()
        }

        /// Returns true if this value is positive infinity or negative infinity, and false otherwise.
        # is_infinite(self: Self) -> bool
        fn <
            float_is_infinite_32<T: u32>(this: f32) -> bool,
            float_is_infinite_64<T: u64>(this: f64) -> bool,
        >(&mut vm) {
            this.is_infinite()
        }

        /// Returns true if this number is neither infinite nor NaN.
        # is_finite(self: Self) -> bool
        fn <
            float_is_finite_32<T: u32>(this: f32) -> bool,
            float_is_finite_64<T: u64>(this: f64) -> bool,
        >(&mut vm) {
            this.is_finite()
        }

        /// Returns true if the number is subnormal.
        # is_subnormal(self: Self) -> bool
        fn <
            float_is_subnormal_32<T: u32>(this: f32) -> bool,
            float_is_subnormal_64<T: u64>(this: f64) -> bool,
        >(&mut vm) {
            this.is_subnormal()
        }

        /// Returns true if the number is neither zero, infinite, subnormal, or NaN.
        # is_normal(self: Self) -> bool
        fn <
            float_is_normal_32<T: u32>(this: f32) -> bool,
            float_is_normal_64<T: u64>(this: f64) -> bool,
        >(&mut vm) {
            this.is_normal()
        }

        /// Takes the reciprocal (inverse) of a number, 1/x.
        # recip(self: Self) -> Self
        fn <
            float_recip_32<T: u32>(this: f32) -> f32,
            float_recip_64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.recip()
        }

        /// Converts radians to degrees.
        # to_degrees(self: Self) -> Self
        fn <
            float_to_degrees_32<T: u32>(this: f32) -> f32,
            float_to_degrees_64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.to_degrees()
        }

        /// Converts degrees to radians.
        # to_radians(self: Self) -> Self
        fn <
            float_to_radians_32<T: u32>(this: f32) -> f32,
            float_to_radians_64<T: u64>(this: f64) -> f64,
        >(&mut vm) {
            this.to_radians()
        }

        /// Returns the minimum of the two numbers.
        # min(self: Self, other: Self) -> Self
        fn <
            float_min_32<T: u32>(this: f32, other: f32) -> f32,
            float_min_64<T: u64>(this: f64, other: f64) -> f64,
        >(&mut vm) {
            this.min(other)
        }

        /// Returns the maximum of the two numbers.
        # max(self: Self, other: Self) -> Self
        fn <
            float_max_32<T: u32>(this: f32, other: f32) -> f32,
            float_max_64<T: u64>(this: f64, other: f64) -> f64,
        >(&mut vm) {
            this.max(other)
        }

        /// Restrict a value to a certain interval unless it is NaN.
        # clamp(self: Self, min: Self, max: Self) -> Self
        fn <
            float_clamp_32<T: u32>(this: f32, min: f32, max: f32) -> f32,
            float_clamp_64<T: u64>(this: f64, min: f64, max: f64) -> f64,
        >(&mut vm) {
            this.clamp(min, max)
        }
    }

    String {

        /// Inserts another string into this String at the given UTF-8 character position.
        # insert(self: Self, position: u64, other: Self)
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

        /// Returns a substring of the String, starting at given UTF-8 character position and the given length.
        # slice(self: Self, position: u64, len: u64)
        fn string_slice(this: &str, position: StackAddress, len: StackAddress) -> String {
            let mut result = String::with_capacity(len as usize); // todo: probably want to over-allocate here since our len is in chars an capacity in bytes
            let _ = append(position as usize, if len > 0 { Some(len as usize) } else { None }, &this, &mut result);
            result
        }

        /// Returns true if the string starts with the given prefix string.
        # starts_with(self: Self, other: Self) -> bool
        fn string_starts_with(this: &str, other: &str) -> bool {
            this.starts_with(other)
        }

        /// Returns true if the string ends with the given suffix string.
        # ends_with(self: Self, other: Self) -> bool
        fn string_ends_with(this: &str, other: &str) -> bool {
            this.ends_with(other)
        }

        /// Returns a string with leading and trailing whitespace removed.
        # trim(self: Self) -> Self
        fn string_trim(this: &str) -> String {
            this.trim().to_string()
        }

        /// Returns a string slice with leading whitespace removed.
        # trim_start(self: Self) -> Self
        fn string_trim_start(this: &str) -> String {
            this.trim_start().to_string()
        }

        /// Returns a string slice with trailing whitespace removed.
        # trim_end(self: Self) -> Self
        fn string_trim_end(this: &str) -> String {
            this.trim_end().to_string()
        }

        /// Returns true if the string contains a given substring.
        # contains(self: Self) -> bool
        fn string_contains(this: &str, other: &str) -> bool {
            this.contains(other)
        }

        /// Replaces occurences of the given substring with another string.
        # replace(self: Self, from: Self, to: Self) -> String
        fn string_replace(this: &str, from: &str, to: &str) -> String {
            this.replace(from, to)
        }

        /// Returns the lowercase equivalent of this string.
        # to_lowercase(self: Self) -> String
        fn string_to_lowercase(this: &str) -> String {
            this.to_lowercase()
        }

        /// Returns the uppercase equivalent of this string.
        # to_uppercase(self: Self) -> String
        fn string_to_uppercase(this: &str) -> String {
            this.to_uppercase()
        }

        /// Creates a new String by repeating this string n times.
        # repeat(self: Self, n: u64) -> String
        fn string_repeat(this: &str, n: StackAddress) -> String {
            if this.len().checked_mul(n as usize).is_some() {
                this.repeat(n as usize)
            } else {
                "".to_string() // FIXME: need error reporting mechanism
            }
        }

        /// Returns the character index of the first character of this string that matches the given string. TODO: Currently returns -1 if not found, need optional support.
        # find(self: Self, other: String) -> i64
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
}