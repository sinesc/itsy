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

    // array builtins

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

    // float builtins

    fn <
        float_floor32<T: u32>(this: f32) -> f32,
        float_floor64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.floor()
    }

    fn <
        float_ceil32<T: u32>(this: f32) -> f32,
        float_ceil64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.ceil()
    }

    fn <
        float_round32<T: u32>(this: f32) -> f32,
        float_round64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.round()
    }

    fn <
        float_trunc32<T: u32>(this: f32) -> f32,
        float_trunc64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.trunc()
    }

    fn <
        float_fract32<T: u32>(this: f32) -> f32,
        float_fract64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.fract()
    }

    fn <
        float_abs32<T: u32>(this: f32) -> f32,
        float_abs64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.abs()
    }

    fn <
        float_signum32<T: u32>(this: f32) -> f32,
        float_signum64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.signum()
    }

    fn <
        float_powi32<T: u32>(this: f32, n: i32) -> f32,
        float_powi64<T: u64>(this: f64, n: i32) -> f64,
    >(&mut vm) {
        this.powi(n)
    }

    fn <
        float_powf32<T: u32>(this: f32, n: f32) -> f32,
        float_powf64<T: u64>(this: f64, n: f64) -> f64,
    >(&mut vm) {
        this.powf(n)
    }

    fn <
        float_sqrt32<T: u32>(this: f32) -> f32,
        float_sqrt64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.sqrt()
    }

    fn <
        float_exp32<T: u32>(this: f32) -> f32,
        float_exp64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.exp()
    }

    fn <
        float_exp2_32<T: u32>(this: f32) -> f32,
        float_exp2_64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.exp2()
    }

    fn <
        float_ln32<T: u32>(this: f32) -> f32,
        float_ln64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.ln()
    }

    fn <
        float_log32<T: u32>(this: f32, base: f32) -> f32,
        float_log64<T: u64>(this: f64, base: f64) -> f64,
    >(&mut vm) {
        this.log(base)
    }

    fn <
        float_log2_32<T: u32>(this: f32) -> f32,
        float_log2_64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.log2()
    }

    fn <
        float_log10_32<T: u32>(this: f32) -> f32,
        float_log10_64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.log10()
    }

    fn <
        float_cbrt32<T: u32>(this: f32) -> f32,
        float_cbrt64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.cbrt()
    }

    fn <
        float_hypot32<T: u32>(this: f32, other: f32) -> f32,
        float_hypot64<T: u64>(this: f64, other: f64) -> f64,
    >(&mut vm) {
        this.hypot(other)
    }

    fn <
        float_sin32<T: u32>(this: f32) -> f32,
        float_sin64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.sin()
    }

    fn <
        float_cos32<T: u32>(this: f32) -> f32,
        float_cos64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.cos()
    }

    fn <
        float_tan32<T: u32>(this: f32) -> f32,
        float_tan64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.tan()
    }

    fn <
        float_asin32<T: u32>(this: f32) -> f32,
        float_asin64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.asin()
    }

    fn <
        float_acos32<T: u32>(this: f32) -> f32,
        float_acos64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.acos()
    }

    fn <
        float_atan32<T: u32>(this: f32) -> f32,
        float_atan64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.atan()
    }

    fn <
        float_atan2_32<T: u32>(this: f32, other: f32) -> f32,
        float_atan2_64<T: u64>(this: f64, other: f64) -> f64,
    >(&mut vm) {
        this.atan2(other)
    }

    fn <
        float_is_nan_32<T: u32>(this: f32) -> bool,
        float_is_nan_64<T: u64>(this: f64) -> bool,
    >(&mut vm) {
        this.is_nan()
    }

    fn <
        float_is_infinite_32<T: u32>(this: f32) -> bool,
        float_is_infinite_64<T: u64>(this: f64) -> bool,
    >(&mut vm) {
        this.is_infinite()
    }

    fn <
        float_is_finite_32<T: u32>(this: f32) -> bool,
        float_is_finite_64<T: u64>(this: f64) -> bool,
    >(&mut vm) {
        this.is_finite()
    }

    fn <
        float_is_subnormal_32<T: u32>(this: f32) -> bool,
        float_is_subnormal_64<T: u64>(this: f64) -> bool,
    >(&mut vm) {
        this.is_subnormal()
    }

    fn <
        float_is_normal_32<T: u32>(this: f32) -> bool,
        float_is_normal_64<T: u64>(this: f64) -> bool,
    >(&mut vm) {
        this.is_normal()
    }

    fn <
        float_recip_32<T: u32>(this: f32) -> f32,
        float_recip_64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.recip()
    }

    fn <
        float_to_degrees_32<T: u32>(this: f32) -> f32,
        float_to_degrees_64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.to_degrees()
    }

    fn <
        float_to_radians_32<T: u32>(this: f32) -> f32,
        float_to_radians_64<T: u64>(this: f64) -> f64,
    >(&mut vm) {
        this.to_radians()
    }

    fn <
        float_min_32<T: u32>(this: f32, other: f32) -> f32,
        float_min_64<T: u64>(this: f64, other: f64) -> f64,
    >(&mut vm) {
        this.min(other)
    }

    fn <
        float_max_32<T: u32>(this: f32, other: f32) -> f32,
        float_max_64<T: u64>(this: f64, other: f64) -> f64,
    >(&mut vm) {
        this.max(other)
    }

    fn <
        float_clamp_32<T: u32>(this: f32, min: f32, max: f32) -> f32,
        float_clamp_64<T: u64>(this: f64, min: f64, max: f64) -> f64,
    >(&mut vm) {
        this.clamp(min, max)
    }

    // string builtins

    fn string_insert(this: &str, position: StackAddress, other: &str) -> String {
        let mut result = String::with_capacity(this.len() + other.len());
        let mut remainder = append(0, Some(position), &this, &mut result);
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

    fn string_slice(this: &str, position: StackAddress, len: StackAddress) -> String {
        let mut result = String::with_capacity(len as usize); // todo: probably want to over-allocate here since our len is in chars an capacity in bytes
        let _ = append(position, if len > 0 { Some(len) } else { None }, &this, &mut result);
        result
    }

    fn string_starts_with(this: &str, other: &str) -> bool {
        this.starts_with(other)
    }

    fn string_ends_with(this: &str, other: &str) -> bool {
        this.ends_with(other)
    }

    fn string_trim(this: &str) -> String {
        this.trim().to_string()
    }

    fn string_trim_start(this: &str) -> String {
        this.trim_start().to_string()
    }

    fn string_trim_end(this: &str) -> String {
        this.trim_end().to_string()
    }

    fn string_contains(this: &str, other: &str) -> bool {
        this.contains(other)
    }

    fn string_replace(this: &str, from: &str, to: &str) -> String {
        this.replace(from, to)
    }

    fn string_to_lowercase(this: &str) -> String {
        this.to_lowercase()
    }

    fn string_to_uppercase(this: &str) -> String {
        this.to_uppercase()
    }

    fn string_repeat(this: &str, n: StackAddress) -> String {
        if this.len().checked_mul(n as usize).is_some() {
            this.repeat(n as usize)
        } else {
            "".to_string() // FIXME: need error reporting mechanism
        }
    }

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