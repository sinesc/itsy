//! Opcode definitions. Implemented on Writer/VM.

use crate::prelude::*;
use crate::{StackAddress, StackOffset, STACK_ADDRESS_TYPE, RustFnIndex, BuiltinIndex, ItemIndex};
use crate::bytecode::{ARG1, ARG2, ARG3, HeapRef, builtins::Builtin, runtime::{stack::{StackOp, StackRelativeOp}, heap::{HeapOp, HeapCmp, HeapRefOp}, vm::{VMState, CopyTarget}}};

type Data8 = u8;
type Data16 = u16;
type Data32 = u32;
type Data64 = u64;

impl_opcodes!{

    /*/// Does nothing.
    fn noop(&mut self) { }
    */
    /// Moves the stack pointer by given number of bytes to make room for local variables.
    fn reserve(&mut self, num_bytes: u8) { // FIXME u8 is not enough
        self.stack.extend_zero(num_bytes as StackAddress);
    }

    /// Discards the top stack value.
    fn <
        discard8<T: Data8>(),
        discard16<T: Data16>(),
        discard32<T: Data32>(),
        discard64<T: Data64>(),
    >(&mut self) {
        self.stack.truncate(self.stack.sp() - size_of::<T>() as StackAddress);
    }

    /// Pushes 0 onto stack.
    fn <
        zero8<T: Data8>(),
        zero16<T: Data16>(),
        zero32<T: Data32>(),
        zero64<T: Data64>(),
    >(&mut self) {
        self.stack.push(0 as T);
    }

    /// Pushes 1 onto stack.
    fn <
        one8<T: Data8>(),
        one16<T: Data16>(),
        one32<T: Data32>(),
        one64<T: Data64>(),
    >(&mut self) {
        self.stack.push(1 as T);
    }

    /// Pushes -1 onto stack.
    fn <
        fill8<T: i8>(),
        fill16<T: i16>(),
        fill32<T: i32>(),
        fill64<T: i64>(),
    >(&mut self) {
        self.stack.push(-1 as T);
    }

    /// Pushes 8 bit value as an 8 or 32 bit value onto stack.
    fn <
        literali8(value: u8),
        literalu32(value: u8 as u32),
        literals32(value: i8 as i32),
    >(&mut self) {
        self.stack.push(value);
    }

    /// Loads 32 bit function argument 1 and pushes it onto the stack. Equals load32(ARG1).
    fn load_arg1(&mut self) {
        let local: Data32 = self.stack.load_fp(ARG1);
        self.stack.push(local);
    }
    /// Loads 32 bit function argument 2 and pushes it onto the stack. Equals load32(ARG2).
    fn load_arg2(&mut self) {
        let local: Data32 = self.stack.load_fp(ARG2);
        self.stack.push(local);
    }
    /// Loads 32 bit function argument 3 and pushes it onto the stack. Equals load32(ARG3).
    fn load_arg3(&mut self) {
        let local: Data32 = self.stack.load_fp(ARG3);
        self.stack.push(local);
    }

    /// Loads data from constpool at given offset and pushes it onto the stack.
    fn <
        const16_8<T: Data16>(offset: u8 as StackAddress),
        const16_16<T: Data16>(offset: u16 as StackAddress),
        const16_sa<T: Data16>(offset: StackAddress),
        const32_8<T: Data32>(offset: u8 as StackAddress),
        const32_16<T: Data32>(offset: u16 as StackAddress),
        const32_sa<T: Data32>(offset: StackAddress),
        const64_8<T: Data64>(offset: u8 as StackAddress),
        const64_16<T: Data64>(offset: u16 as StackAddress),
        const64_sa<T: Data64>(offset: StackAddress),
    >(&mut self) {
        let local: T = self.stack.load(offset);
        self.stack.push(local);
    }

    /// Loads data from stack at given offset (relative to the stack frame) and pushes it onto the stack.
    fn <
        load8_8<T: Data8>(offset: i8 as StackOffset),
        load8_16<T: Data8>(offset: i16 as StackOffset),
        load8_sa<T: Data8>(offset: StackOffset),
        load16_8<T: Data16>(offset: i8 as StackOffset),
        load16_16<T: Data16>(offset: i16 as StackOffset),
        load16_sa<T: Data16>(offset: StackOffset),
        load32_8<T: Data32>(offset: i8 as StackOffset),
        load32_16<T: Data32>(offset: i16 as StackOffset),
        load32_sa<T: Data32>(offset: StackOffset),
        load64_8<T: Data64>(offset: i8 as StackOffset),
        load64_16<T: Data64>(offset: i16 as StackOffset),
        load64_sa<T: Data64>(offset: StackOffset),
    >(&mut self) {
        let abs = (offset + if offset >= 0 { self.stack.fp as StackOffset } else { self.stack.sp() as StackOffset }) as StackAddress; // fp+offset or sp-offset
        let local: T = self.stack.load(abs);
        self.stack.push(local);
    }

    /// Pops data off the stack and stores it at the given offset (relative to the stack frame).
    fn <
        store8_8<T: Data8>(offset: i8 as StackOffset),
        store8_16<T: Data8>(offset: i16 as StackOffset),
        store8_sa<T: Data8>(offset: StackOffset),
        store16_8<T: Data16>(offset: i8 as StackOffset),
        store16_16<T: Data16>(offset: i16 as StackOffset),
        store16_sa<T: Data16>(offset: StackOffset),
        store32_8<T: Data32>(offset: i8 as StackOffset),
        store32_16<T: Data32>(offset: i16 as StackOffset),
        store32_sa<T: Data32>(offset: StackOffset),
        store64_8<T: Data64>(offset: i8 as StackOffset),
        store64_16<T: Data64>(offset: i16 as StackOffset),
        store64_sa<T: Data64>(offset: StackOffset),
    >(&mut self) {
        let abs = (offset + if offset >= 0 { self.stack.fp as StackOffset } else { self.stack.sp() as StackOffset }) as StackAddress; // fp+offset or sp-offset
        let local: T = self.stack.pop();
        self.stack.store(abs, local);
    }

    /// Pops HeapRef off the stack and stores it at the given offset (relative to the stack frame).
    /// Increases refcount of the new value.
    fn storex_new(&mut self, index: StackOffset, constructor: StackAddress) {
        let value: HeapRef = self.stack.pop();
        self.stack.store_fp(index, value);
        self.refcount_value(value, constructor, HeapRefOp::Inc);
    }

    /// Pops HeapRef off the stack and stores it at the given offset (relative to the stack frame).
    /// Decreses refcount of the previous contents and increases refcount of the new value.
    fn storex_replace(&mut self, index: StackOffset, constructor: StackAddress) {
        let prev: HeapRef = self.stack.load_fp(index);
        let next: HeapRef = self.stack.pop();
        self.stack.store_fp(index, next);
        if next != prev {
            self.refcount_value(next, constructor, HeapRefOp::Inc);
            self.refcount_value(prev, constructor, HeapRefOp::Dec);
        }
    }

    /// Clones the topmost stack value.
    fn <
        clone8<T: Data8>(),
        clone16<T: Data16>(),
        clone32<T: Data32>(),
        clone64<T: Data64>(),
    >(&mut self) {
        let data: T = self.stack.load_sp(- (size_of::<T>() as StackOffset));
        self.stack.push(data);
    }

    /// Swap the 2 topmost stack values.
    fn <
        swap8<T: Data8>(),
        swap16<T: Data16>(),
        swap32<T: Data32>(),
        swap64<T: Data64>(),
    >(&mut self) {
        let pos_a = self.stack.sp() - size_of::<T>() as StackAddress;
        let pos_b = pos_a - size_of::<T>() as StackAddress;
        let a: T = self.stack.load(pos_a);
        let b: T = self.stack.load(pos_b);
        self.stack.store(pos_a, b);
        self.stack.store(pos_b, a);
    }

    /// Decrements the value at the top of the stack.
    fn <
        deci8<T: i8>(decr: i8),
        deci16<T: i16>(decr: i8),
        deci32<T: i32>(decr: i8),
        deci64<T: i64>(decr: i8)
    >(&mut self) {
        let a: T = self.stack.pop();
        self.stack.push(T::wrapping_sub(a, decr as T));
    }

    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the result onto the stack.
    fn <
        predeci8<T: i8>(offset: StackOffset, decr: i8),
        predeci16<T: i16>(offset: StackOffset, decr: i8),
        predeci32<T: i32>(offset: StackOffset, decr: i8),
        predeci64<T: i64>(offset: StackOffset, decr: i8)
    >(&mut self) {
        let mut value: T = self.stack.load_fp(offset);
        value = T::wrapping_sub(value, decr as T);
        self.stack.store_fp(offset, value);
        self.stack.push(value);
    }

    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the previous value onto the stack.
    fn <
        postdeci8<T: i8>(offset: StackOffset, decr: i8),
        postdeci16<T: i16>(offset: StackOffset, decr: i8),
        postdeci32<T: i32>(offset: StackOffset, decr: i8),
        postdeci64<T: i64>(offset: StackOffset, decr: i8)
    >(&mut self) {
        let value: T = self.stack.load_fp(offset);
        self.stack.store_fp(offset, T::wrapping_sub(value, decr as T));
        self.stack.push(value);
    }

    /// Pop StackAddress sized "index" and heap reference and push the resulting heap reference with offset += index * element_size onto the stack.
    fn index(&mut self, element_size: u8) {
        let element_index: StackAddress = self.stack.pop();
        let mut item: HeapRef = self.stack.pop();
        item.add_offset(element_index as StackOffset * element_size as StackOffset);
        self.stack.push(item);
    }

    /// Offsets the heap address at the top of the stack by given value.
    fn <
        offsetx_8(offset: i8 as StackOffset),
        offsetx_16(offset: i16 as StackOffset),
        offsetx_sa(offset: StackOffset),
    >(&mut self) {
        let mut item: HeapRef = self.stack.pop();
        item.add_offset(offset);
        self.stack.push(item);
    }

    /// Pops value off the stack, pushes 0 for values < 0 and the original value for values >= 0.
    fn <
        zclampf32<T: f32>(),
        zclampf64<T: f64>(),
        zclampi8<T: i8>(),
        zclampi16<T: i16>(),
        zclampi32<T: i32>(),
        zclampi64<T: i64>(),
    >(&mut self) {
        let value: T = self.stack.pop();
        self.stack.push(if value >= 0 as T { value } else { 0 as T });
    }

    /// Pops a value and pushes its converted values.
    fn <
        i64_to_string<T: i64>(),
        u64_to_string<T: u64>(),
        f32_to_string<T: f32>(),
        f64_to_string<T: f64>(),
    >(&mut self) {
        let value: T = self.stack.pop();
        let string = format!("{}", value);
        let index: StackAddress = self.heap.alloc(string.into_bytes(), ItemIndex::MAX);
        self.stack.push(HeapRef::new(index, 0));
    }

    /// Pops a value and pushes its converted values.
    fn <
        i64_to_f32<F: i64, T: f32>(),
        u64_to_f32<F: u64, T: f32>(),
        f64_to_f32<F: f64, T: f32>(),
        i64_to_f64<F: i64, T: f64>(),
        u64_to_f64<F: u64, T: f64>(),
        f32_to_f64<F: f32, T: f64>(),
        f32_to_i64<F: f32, T: i64>(),
        f64_to_i64<F: f64, T: i64>(),
        f32_to_u64<F: f32, T: u64>(),
        f64_to_u64<F: f64, T: u64>(),
    >(&mut self) {
        let value: F = self.stack.pop();
        self.stack.push(value as T);
    }

    /// Pops value off the stack, saturates it to the given number of bits pushes the result.
    fn <
        trimu16<T: u16>(size: u8),
        trimu32<T: u32>(size: u8),
        trimu64<T: u64>(size: u8),
    >(&mut self) {
        let value: T = self.stack.pop();
        match size {
            64 => self.stack.push(if value > u64::MAX as T { u64::MAX } else if value < u64::MIN as T { u64::MIN } else { value as u64 }),
            32 => self.stack.push(if value > u32::MAX as T { u32::MAX } else if value < u32::MIN as T { u32::MIN } else { value as u32 }),
            16 => self.stack.push(if value > u16::MAX as T { u16::MAX } else if value < u16::MIN as T { u16::MIN } else { value as u16 }),
            8 => self.stack.push(if value > u8::MAX as T { u8::MAX } else if value < u8::MIN as T { u8::MIN } else { value as u8 }),
            _ => self.state = VMState::RuntimeError,
        };
    }

    /// Pops value off the stack, saturates it to the given number of bits pushes the result.
    fn <
        trims16<T: i16>(size: u8),
        trims32<T: i32>(size: u8),
        trims64<T: i64>(size: u8),
    >(&mut self) {
        let value: T = self.stack.pop();
        match size {
            64 => self.stack.push(if value > i64::MAX as T { i64::MAX } else if value < i64::MIN as T { i64::MIN } else { value as i64 }),
            32 => self.stack.push(if value > i32::MAX as T { i32::MAX } else if value < i32::MIN as T { i32::MIN } else { value as i32 }),
            16 => self.stack.push(if value > i16::MAX as T { i16::MAX } else if value < i16::MIN as T { i16::MIN } else { value as i16 }),
            8 => self.stack.push(if value > i8::MAX as T { i8::MAX } else if value < i8::MIN as T { i8::MIN } else { value as i8 }),
            _ => self.state = VMState::RuntimeError,
        };
    }

    /// Pops value off the stack, extends it to the given number of bits pushes the result.
    fn <
        extendu8<T: u8>(size: u8),
        extendu16<T: u16>(size: u8),
        extendu32<T: u32>(size: u8),
        extends8<T: i8>(size: u8),
        extends16<T: i16>(size: u8),
        extends32<T: i32>(size: u8),
    >(&mut self) {
        let value: T = self.stack.pop();
        match size {
            64 => self.stack.push(value as u64),
            32 => self.stack.push(value as u32),
            16 => self.stack.push(value as u16),
            _ => self.state = VMState::RuntimeError,
        };
    }

/*
    /// Pops value off the stack, truncates it to the given number of bits pushes the result.
    fn <
        truncate16<T: Data16>(size: u8),
        truncate32<T: Data32>(size: u8),
        truncate64<T: Data64>(size: u8),
    >(&mut self) {
        let value: T = self.stack.pop();
        match size {
            64 => self.stack.push((value & ((1 << size as T) - 1)) as u64),
            32 => self.stack.push((value & ((1 << size as T) - 1)) as u32),
            16 => self.stack.push((value & ((1 << size as T) - 1)) as u16),
            8 => self.stack.push((value & ((1 << size as T) - 1)) as u8),
            _ => self.state = VMState::RuntimeError,
        };
    }
*/

    /// Pops 2 values from the stack and pushes their logical conjunction.
    fn and(&mut self) {
        let b: Data8 = self.stack.pop();
        let a: Data8 = self.stack.pop();
        self.stack.push((a != 0 && b != 0) as Data8);
    }
    /// Pops 2 values from the stack and pushes their logical disjunction.
    fn or(&mut self) {
        let b: Data8 = self.stack.pop();
        let a: Data8 = self.stack.pop();
        self.stack.push((a != 0 || b != 0) as Data8);
    }
    /// Pops a values from the stack and pushes its logical negation.
    fn not(&mut self) {
        let a: Data8 = self.stack.pop();
        self.stack.push((a == 0) as Data8);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn <
        addi8<T: Data8>(),
        addi16<T: Data16>(),
        addi32<T: Data32>(),
        addi64<T: Data64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(T::wrapping_add(a, b));
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn <
        addf32<T: f32>(),
        addf64<T: f64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(a + b);
    }

    /// Pops 2 values from the stack and pushes their difference.
    fn <
        subi8<T: Data8>(),
        subi16<T: Data16>(),
        subi32<T: Data32>(),
        subi64<T: Data64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(T::wrapping_sub(a, b));
    }

    /// Pops 2 values from the stack and pushes their difference.
    fn <
        subf32<T: f32>(),
        subf64<T: f64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(a - b);
    }

    /// Pops 2 values from the stack and pushes their product.
    fn <
        muli8<T: Data8>(),
        muli16<T: Data16>(),
        muli32<T: Data32>(),
        muli64<T: Data64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(T::wrapping_mul(a, b));
    }

    /// Pops 2 values from the stack and pushes their product.
    fn <
        mulf32<T: f32>(),
        mulf64<T: f64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(a * b);
    }

    /// Pops 2 values from the stack and pushes their quotient.
    fn <
        divs8<T: i8>(),
        divs16<T: i16>(),
        divs32<T: i32>(),
        divs64<T: i64>(),
        divu8<T: u8>(),
        divu16<T: u16>(),
        divu32<T: u32>(),
        divu64<T: u64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(T::wrapping_div(a, b));
    }

    /// Pops 2 values from the stack and pushes their quotient.
    fn <
        divf32<T: f32>(),
        divf64<T: f64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(a / b);
    }

    /// Pops 2 values from the stack and pushes their remainder.
    fn <
        rems8<T: i8>(),
        rems16<T: i16>(),
        rems32<T: i32>(),
        rems64<T: i64>(),
        remu8<T: u8>(),
        remu16<T: u16>(),
        remu32<T: u32>(),
        remu64<T: u64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(T::wrapping_rem(a, b));
    }

    /// Pops 2 values from the stack and pushes their remainder.
    fn <
        remf32<T: f32>(),
        remf64<T: f64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(a % b);
    }

    /// Pops stack address sized value, shifts it by given number of bits and pushes it.
    fn shrsa(&mut self, num: u8) {
        let value: StackAddress = self.stack.pop();
        self.stack.push(value >> num);
    }

    /// Pops two values and pushes a 1 if the first value equals the second, otherwise a 0.
    fn <
        ceq8<T: Data8>(),
        ceq16<T: Data16>(),
        ceq32<T: Data32>(),
        ceq64<T: Data64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push((a == b) as Data8);
    }

    /// Pops two values and pushes a 1 if the first value does not equal the second, otherwise a 0.
    fn <
        cneq8<T: Data8>(),
        cneq16<T: Data16>(),
        cneq32<T: Data32>(),
        cneq64<T: Data64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push((a != b) as Data8);
    }

    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn <
        clts8<T: i8>(),
        cltu8<T: u8>(),
        clts16<T: i16>(),
        cltu16<T: u16>(),
        clts32<T: i32>(),
        cltu32<T: u32>(),
        clts64<T: i64>(),
        cltu64<T: u64>(),
        cltf32<T: f32>(),
        cltf64<T: f64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push((a < b) as Data8);
    }

    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn <
        cltes8<T: i8>(),
        clteu8<T: u8>(),
        cltes16<T: i16>(),
        clteu16<T: u16>(),
        cltes32<T: i32>(),
        clteu32<T: u32>(),
        cltes64<T: i64>(),
        clteu64<T: u64>(),
        cltef32<T: f32>(),
        cltef64<T: f64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push((a <= b) as Data8);
    }

    /// Jumps unconditionally to the given address.
    fn jmp(&mut self, addr: StackAddress) {
        self.pc = addr;
    }

    /// Pops an 8 bit value and jumps to given address if the value is 0.
    fn j0(&mut self, addr: StackAddress) {
        let a: Data8 = self.stack.pop();
        if a == 0 {
            self.pc = addr;
        }
    }

    /// Pops an 8 bit value and jumps to given address if the value is not 0.
    fn jn0(&mut self, addr: StackAddress) {
        let a: Data8 = self.stack.pop();
        if a != 0 {
            self.pc = addr;
        }
    }

    /// Jumps to given address if the 8 bit stack-top is 0.
    fn j0_nc(&mut self, addr: StackAddress) {
        let a: Data8 = self.stack.top();
        if a == 0 {
            self.pc = addr;
        }
    }

    /// Jumps to given address if the 8 bit stack-top is not 0.
    fn jn0_nc(&mut self, addr: StackAddress) {
        let a: Data8 = self.stack.top();
        if a != 0 {
            self.pc = addr;
        }
    }

    /// Jumps to given address if the StackAddress sized stack-top equals 0.
    fn j0_sa_nc(&mut self, addr: StackAddress) {
        let a: StackAddress = self.stack.top();
        if a == 0 {
            self.pc = addr;
        }
    }

    /// Jumps to given address if the StackAddress sized stack-top does not equal 0.
    fn jn0_sa_nc(&mut self, addr: StackAddress) {
        let a: StackAddress = self.stack.top();
        if a != 0 {
            self.pc = addr;
        }
    }

    /// Constructs an instance of a non-primitive type.
    fn construct(&mut self, constructor: StackAddress, prototype: StackAddress) {
        let mut prototype = prototype; // impl_opcodes macro does not allow mut arguments
        self.construct_value(constructor, &mut prototype, CopyTarget::Stack, false);
    }

    /// Moves an instance that was constructed on the stack to the heap.
    fn upload(&mut self, size: StackAddress, implementor_index: ItemIndex) {
        let mut data = Vec::with_capacity(size as usize);
        let data_start = self.stack.sp() as usize - size as usize;
        data.extend_from_slice(&self.stack.data()[data_start..]);
        self.stack.truncate(data_start as StackAddress);
        self.stack.push(HeapRef::new(self.heap.alloc(data, implementor_index), 0));
    }

    /// Pops a heap reference off the stack and performs a reference count operation.
    fn <
        cnt_8(constructor: u8 as StackAddress, op: HeapRefOp),
        cnt_16(constructor: u16 as StackAddress, op: HeapRefOp),
        cnt_sa(constructor: StackAddress, op: HeapRefOp),
    >(&mut self) {
        let item: HeapRef = self.stack.pop();
        self.refcount_value(item, constructor, op);
    }

    /// Performs a non-consuming reference count operation for the top heap reference on the stack.
    fn <
        cnt_8_nc(constructor: u8 as StackAddress, op: HeapRefOp),
        cnt_16_nc(constructor: u16 as StackAddress, op: HeapRefOp),
        cnt_sa_nc(constructor: StackAddress, op: HeapRefOp),
    >(&mut self) {
        let item: HeapRef = self.stack.top();
        self.refcount_value(item, constructor, op);
    }

    /// Calls the given Rust function.
    fn rustcall(&mut self, &mut context, rustfn: RustFn) {
        rustfn.exec(self, context);
    }

    /// Calls the given builtin function.
    fn builtincall(&mut self, builtin: Builtin) {
        builtin.exec(self);
    }

    /// Function call. Creates a new stack frame at SP - arg_size and sets programm counter to given addr.
    fn call(&mut self, addr: StackAddress, arg_size: StackAddress) {
        // stack: ... | ARGS
        self.stack.push(self.stack.fp);
        self.stack.fp = self.stack.sp() - arg_size - (size_of_val(&self.stack.fp) as StackAddress);
        self.stack.push(self.pc);
        self.pc = addr;
        // stack: ARGS | previous FP | previous PC | (local vars and dynamic stack follow here)
    }

    /// Virtual function call. Resolves concrete call address from vtable and invokes call().
    fn vcall(&mut self, function_base_address: StackAddress, arg_size: StackAddress) {
        let item: HeapRef = self.stack.load_sp(-(arg_size as StackOffset));
        let implementor_index = self.heap.item_implementor_index(item.index());
        let address: StackAddress = self.stack.load(function_base_address + ((implementor_index as usize) * size_of::<StackAddress>()) as StackAddress);
        self.call(address, arg_size);
    }

    /// Function return. Restores state, removes arguments left on stack by caller.
    fn ret0(&mut self, arg_size: StackAddress) {
        // stack: ARGS | previous FP | previous PC | local vars
        let prev_fp = self.stack.load_fp(arg_size as StackOffset);
        let prev_pc = self.stack.load_fp(arg_size as StackOffset + size_of_val(&prev_fp) as StackOffset);
        self.stack.truncate(self.stack.fp);
        self.pc = prev_pc;
        self.stack.fp = prev_fp;
    }

    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn <
        ret8<T: Data8>(arg_size: StackAddress),
        ret16<T: Data16>(arg_size: StackAddress),
        ret32<T: Data32>(arg_size: StackAddress),
        ret64<T: Data64>(arg_size: StackAddress),
    >(&mut self) {
        // stack: ARGS | previous FP | previous PC | local vars | RESULT
        let prev_fp = self.stack.load_fp(arg_size as StackOffset);
        let prev_pc = self.stack.load_fp(arg_size as StackOffset + size_of_val(&prev_fp) as StackOffset);
        let ret: T = self.stack.top();
        self.stack.store_fp(0, ret);
        self.stack.truncate(self.stack.fp + size_of::<T>() as StackAddress);
        self.pc = prev_pc;
        self.stack.fp = prev_fp;
    }

    /// Pops 2 heap references to strings, compares the strings for equality and pushes the result. Drops temporary references.
    fn string_ceq(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Eq);
        self.stack.push(equals as Data8);
        self.heap.ref_item(a.index(), HeapRefOp::Free);
        self.heap.ref_item(b.index(), HeapRefOp::Free);
    }

    /// Pops 2 heap references to strings, compares the strings for inequality and pushes the result. Drops temporary references.
    fn string_cneq(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Neq);
        self.stack.push(equals as Data8);
        self.heap.ref_item(a.index(), HeapRefOp::Free);
        self.heap.ref_item(b.index(), HeapRefOp::Free);
    }

    /// Pops 2 heap references to strings, compares the strings lexicographically and pushes the result. Drops temporary references.
    fn string_clt(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Lt);
        self.stack.push(equals as Data8);
        self.heap.ref_item(a.index(), HeapRefOp::Free);
        self.heap.ref_item(b.index(), HeapRefOp::Free);
    }

    /// Pops 2 heap references to strings, compares the strings lexicographically and pushes the result. Drops temporary references.
    fn string_clte(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Lte);
        self.stack.push(equals as Data8);
        self.heap.ref_item(a.index(), HeapRefOp::Free);
        self.heap.ref_item(b.index(), HeapRefOp::Free);
    }

    /// Pops two heap references to strings, concatenates the referenced strings into a new object and pushes its heap reference. Drops temporary references.
    fn string_concatx(&mut self) {
        let b: HeapRef = self.stack.pop();
        let b_len = self.heap.item(b.index()).data.len() as StackAddress;
        let a: HeapRef = self.stack.pop();
        let a_len = self.heap.item(a.index()).data.len() as StackAddress;
        let dest_index: StackAddress = self.heap.alloc(Vec::new(), ItemIndex::MAX);
        self.heap.copy(HeapRef::new(dest_index, 0), a, a_len);
        self.heap.copy(HeapRef::new(dest_index, a_len), b, b_len);
        self.stack.push(HeapRef::new(dest_index, 0));
        self.heap.ref_item(a.index(), HeapRefOp::Free);
        self.heap.ref_item(b.index(), HeapRefOp::Free);
    }

    /// Pops a heap reference and pushes the size of the referenced heap object. Drops temporary references.
    fn heap_size(&mut self, constructor: StackAddress) {
        let item: HeapRef = self.stack.pop();
        let size = self.heap.item(item.index()).data.len();
        self.stack.push(size as StackAddress);
        self.refcount_value(item, constructor, HeapRefOp::Free);
    }

    /*/// Pops 2 heap references dest and src and copies num_bytes bytes from src to dest.
    fn heap_copy(&mut self, constructor: StackAddress, num_bytes: StackAddress) {
        let dest: HeapRef = self.stack.pop();
        let src: HeapRef = self.stack.pop();
        self.heap.copy(dest, src, num_bytes);
        self.refcount_value(src, constructor, HeapRefOp::Free);
    }

    /// Pops 2 heap references and compares num_bytes bytes. Drops temporary references.
    fn heap_ceq(&mut self, constructor: StackAddress, num_bytes: StackAddress) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let data_a = self.heap.slice(a, num_bytes);
        let data_b = self.heap.slice(b, num_bytes);
        self.stack.push((data_a == data_b) as Data8);
        self.refcount_value(a, constructor, HeapRefOp::Free);
        self.refcount_value(b, constructor, HeapRefOp::Free);
    }

    /// Pops 2 heap references and compares num_bytes bytes. Drops temporary references.
    fn heap_cneq(&mut self, constructor: StackAddress, num_bytes: StackAddress) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let data_a = self.heap.slice(a, num_bytes);
        let data_b = self.heap.slice(b, num_bytes);
        self.stack.push((data_a != data_b) as Data8);
        self.refcount_value(a, constructor, HeapRefOp::Free);
        self.refcount_value(b, constructor, HeapRefOp::Free);
    }*/

    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the result onto the stack.
    fn <
        heap_predeci8<T: i8>(decr: i8),
        heap_predeci16<T: i16>(decr: i8),
        heap_predeci32<T: i32>(decr: i8),
        heap_predeci64<T: i64>(decr: i8)
    >(&mut self) {
        let item: HeapRef = self.stack.pop();
        let mut value: T = self.heap.read(item);
        value = T::wrapping_sub(value, decr as T);
        self.heap.write(item, value);
        self.stack.push(value); // push after inc/dec
    }

    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the result onto the stack.
    fn <
        heap_postdeci8<T: i8>(decr: i8),
        heap_postdeci16<T: i16>(decr: i8),
        heap_postdeci32<T: i32>(decr: i8),
        heap_postdeci64<T: i64>(decr: i8)
    >(&mut self) {
        let item: HeapRef = self.stack.pop();
        let mut value: T = self.heap.read(item);
        self.stack.push(value); // push before inc/dec
        value = T::wrapping_sub(value, decr as T);
        self.heap.write(item, value);
    }

    /// Pop a heap reference and push the heap value at its current offset onto the stack.
    fn <
        heap_fetch8<T: Data8>(),
        heap_fetch16<T: Data16>(),
        heap_fetch32<T: Data32>(),
        heap_fetch64<T: Data64>(),
    >(&mut self) {
        let item: HeapRef = self.stack.pop();
        let data: T = self.heap.read(item);
        self.stack.push(data);
    }

    /// Pop a value and a heap reference and store the value at current offset in the heap.
    fn <
        heap_put8<T: Data8>(),
        heap_put16<T: Data16>(),
        heap_put32<T: Data32>(),
        heap_put64<T: Data64>(),
    >(&mut self) {
        let value: T = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        self.heap.write(item, value);
    }

    /// Pop a value and a heap reference and store the value at current offset in the heap.
    /// Increases refcount of the new value.
    fn heap_putx_new(&mut self, constructor: StackAddress) {
        let value: HeapRef = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        self.heap.write(item, value);
        self.refcount_value(value, constructor, HeapRefOp::Inc);
    }

    /// Pop a value and a heap reference and store the value at current offset in the heap.
    /// Decreses refcount of the previous contents and increases refcount of the new value.
    fn heap_putx_replace(&mut self, constructor: StackAddress) {
        let next: HeapRef = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        let prev: HeapRef = self.heap.read(item);
        self.heap.write(item, next);
        if next != prev {
            self.refcount_value(next, constructor, HeapRefOp::Inc);
            self.refcount_value(prev, constructor, HeapRefOp::Dec);
        }
    }

    /// Pop a heap reference and push the heap value at its current offset + given offset onto the stack. Drops temporary references.
    fn <
        heap_fetch_member8<T: Data8>(offset: StackAddress, constructor: StackAddress),
        heap_fetch_member16<T: Data16>(offset: StackAddress, constructor: StackAddress),
        heap_fetch_member32<T: Data32>(offset: StackAddress, constructor: StackAddress),
        heap_fetch_member64<T: Data64>(offset: StackAddress, constructor: StackAddress),
    >(&mut self) {
        let item: HeapRef = self.stack.pop();
        let data: T = self.heap.read(item.with_offset(offset as StackOffset));
        self.refcount_value(item, constructor, HeapRefOp::Free);
        self.stack.push(data);
    }

    /// Pop an element index and heap reference and push the heap value at element index onto the stack. Drops temporary references.
    fn <
        heap_fetch_element8<T: Data8>(constructor: StackAddress),
        heap_fetch_element16<T: Data16>(constructor: StackAddress),
        heap_fetch_element32<T: Data32>(constructor: StackAddress),
        heap_fetch_element64<T: Data64>(constructor: StackAddress),
    >(&mut self) {
        let element_index: StackAddress = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        let data: T = self.heap.read(item.with_offset((size_of::<T>() as StackAddress * element_index) as StackOffset));
        self.refcount_value(item, constructor, HeapRefOp::Free);
        self.stack.push(data);
    }

    /// Read an element index (at top) and heap reference (just below) and push the heap value at element index from the end of the heap object onto the stack. Drops temporary references.
    fn <
        heap_tail_element8_nc<T: Data8>(constructor: StackAddress),
        heap_tail_element16_nc<T: Data16>(constructor: StackAddress),
        heap_tail_element32_nc<T: Data32>(constructor: StackAddress),
        heap_tail_element64_nc<T: Data64>(constructor: StackAddress),
    >(&mut self) {
        let element_index: StackAddress = self.stack.top();
        let item: HeapRef = self.stack.load_sp(-((STACK_ADDRESS_TYPE.primitive_size() + HeapRef::primitive_size()) as StackOffset));
        let offset = self.heap.item(item.index()).data.len() as StackAddress - size_of::<T>() as StackAddress * (element_index + 1);
        let data: T = self.heap.read(item.with_offset(offset as StackOffset));
        self.refcount_value(item, constructor, HeapRefOp::Free);
        self.stack.push(data);
    }

    /* /// Yield program execution.
    fn yld(&mut self) return {
        self.state = VMState::Yielded;
    }*/

    /// Terminate program execution.
    fn exit(&mut self) return {
        self.state = VMState::Terminated;
    }

    /// Does nothing. Written as comment into the opcode stream.
    #[allow(unused_variables)]
    fn comment(&mut self, text: String) {
    }
}
