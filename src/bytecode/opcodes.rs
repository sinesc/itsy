//! Opcode definitions. Implemented on Writer/VM.

#[macro_use]
mod macros;

use std::mem::size_of;
use crate::bytecode::{ARG1, ARG2, ARG3};
use crate::runtime::{StackOp, StackOffsetOp, HeapIO, HeapCmp, VMState, CopyTarget, HeapRefOp};
use crate::util::{HeapRef, StackAddress, StackOffset};

type Data8 = u8;
type Data16 = u16;
type Data32 = u32;
type Data64 = u64;

impl_vm!{

    /// Does nothing.
    fn noop(&mut self) { }

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

    /// Pushes 8 bit 0 onto stack.
    fn zero8(&mut self) {
        self.stack.push(0u8);
    }
    /// Pushes 32 bit 0 onto stack.
    fn zero32(&mut self) {
        self.stack.push(0u32);
    }
    /// Pushes 64 bit 0 onto stack.
    fn zero64(&mut self) {
        self.stack.push(0u64);
    }

    /// Pushes 8 bit 1 onto stack.
    fn one8(&mut self) {
        self.stack.push(1u8);
    }
    /// Pushes 32 bit 1 onto stack.
    fn one32(&mut self) {
        self.stack.push(1u32);
    }
    /// Pushes 64 bit 1 onto stack.
    fn one64(&mut self) {
        self.stack.push(1u64);
    }

    /// Pushes 8 bit -1 onto stack.
    fn fill8(&mut self) {
        self.stack.push(-1i8);
    }
    /// Pushes 32 bit -1 onto stack.
    fn fill32(&mut self) {
        self.stack.push(-1i32);
    }
    /// Pushes 64 bit -1 onto stack.
    fn fill64(&mut self) {
        self.stack.push(-1i64);
    }

    /// Pushes 8 bit value onto stack.
    fn literali8(&mut self, value: u8) {
        self.stack.push(value);
    }
    /// Pushes 32 bit unsigned value onto stack.
    fn literalu32(&mut self, value: u8) {
        self.stack.push(value as u32);
    }
    /// Pushes 32 bit signed value onto stack (sign extended).
    fn literals32(&mut self, value: i8) {
        self.stack.push(value as i32);
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
        const8_8<T: Data8>(offset: u8 as StackAddress),
        const8_16<T: Data8>(offset: u16 as StackAddress),
        const8_32<T: Data8>(offset: StackAddress),
        const16_8<T: Data16>(offset: u8 as StackAddress),
        const16_16<T: Data16>(offset: u16 as StackAddress),
        const16_32<T: Data16>(offset: StackAddress),
        const32_8<T: Data32>(offset: u8 as StackAddress),
        const32_16<T: Data32>(offset: u16 as StackAddress),
        const32_32<T: Data32>(offset: StackAddress),
        const64_8<T: Data64>(offset: u8 as StackAddress),
        const64_16<T: Data64>(offset: u16 as StackAddress),
        const64_32<T: Data64>(offset: StackAddress),
    >(&mut self) {
        let local: T = self.stack.load(offset);
        self.stack.push(local);
    }

    /// Loads data from stack at given offset (relative to the stack frame) and pushes it onto the stack.
    fn <
        load8_8<T: Data8>(offset: i8 as StackOffset),
        load8_16<T: Data8>(offset: i16 as StackOffset),
        load8_32<T: Data8>(offset: StackOffset),
        load16_8<T: Data16>(offset: i8 as StackOffset),
        load16_16<T: Data16>(offset: i16 as StackOffset),
        load16_32<T: Data16>(offset: StackOffset),
        load32_8<T: Data32>(offset: i8 as StackOffset),
        load32_16<T: Data32>(offset: i16 as StackOffset),
        load32_32<T: Data32>(offset: StackOffset),
        load64_8<T: Data64>(offset: i8 as StackOffset),
        load64_16<T: Data64>(offset: i16 as StackOffset),
        load64_32<T: Data64>(offset: StackOffset),
    >(&mut self) {
        let abs = (offset + if offset >= 0 { self.stack.fp as StackOffset } else { self.stack.sp() as StackOffset }) as StackAddress; // fp+offset or sp-offset
        let local: T = self.stack.load(abs);
        self.stack.push(local);
    }

    /// Pops data off the stack and stores it at the given offset (relative to the stack frame).
    fn <
        store8_8<T: Data8>(offset: i8 as StackOffset),
        store8_16<T: Data8>(offset: i16 as StackOffset),
        store8_32<T: Data8>(offset: StackOffset),
        store16_8<T: Data16>(offset: i8 as StackOffset),
        store16_16<T: Data16>(offset: i16 as StackOffset),
        store16_32<T: Data16>(offset: StackOffset),
        store32_8<T: Data32>(offset: i8 as StackOffset),
        store32_16<T: Data32>(offset: i16 as StackOffset),
        store32_32<T: Data32>(offset: StackOffset),
        store64_8<T: Data64>(offset: i8 as StackOffset),
        store64_16<T: Data64>(offset: i16 as StackOffset),
        store64_32<T: Data64>(offset: StackOffset),
    >(&mut self) {
        let abs = (offset + if offset >= 0 { self.stack.fp as StackOffset } else { self.stack.sp() as StackOffset }) as StackAddress; // fp+offset or sp-offset
        let local: T = self.stack.pop();
        self.stack.store(abs, local);
    }

    /// Reads value from the n-th stack element relative to the top of the stack and pushes it.
    /// n=0 is the topmost stack value, n=sizeof(value) the previous value.
    fn <
        clone8<T: Data8>(n: u8),
        clone16<T: Data16>(n: u8),
        clone32<T: Data32>(n: u8),
        clone64<T: Data64>(n: u8),
    >(&mut self) {
        let data: T = self.stack.load_sp(- (size_of::<T>() as StackOffset) - n as StackOffset);
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

    /// Pop u32 "index" and heap reference and push the resulting heap reference with offset += index * element_size onto the stack.
    fn index(&mut self, element_size: u8) {
        let element_index: StackAddress = self.stack.pop();
        let mut item: HeapRef = self.stack.pop();
        item.add_offset(element_index as StackOffset * element_size as StackOffset);
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
        let index: StackAddress = self.heap.alloc(string.into_bytes());
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

    /// Increments the value at the top of the stack.
    fn <
        inci8<T: Data8>(),
        inci16<T: Data16>(),
        inci32<T: Data32>(),
        inci64<T: Data64>()
    >(&mut self) {
        let a: T = self.stack.pop();
        self.stack.push(T::wrapping_add(a, 1));
    }

    /// Decrements the value at the top of the stack.
    fn <
        deci8<T: Data8>(),
        deci16<T: Data16>(),
        deci32<T: Data32>(),
        deci64<T: Data64>()
    >(&mut self) {
        let a: T = self.stack.pop();
        self.stack.push(T::wrapping_sub(a, 1));
    }

    /// Increments the stackvalue at given offset (relative to the stack frame) and pushes the result onto the stack.
    fn <
        preinci8<T: Data8>(offset: StackOffset),
        preinci16<T: Data16>(offset: StackOffset),
        preinci32<T: Data32>(offset: StackOffset),
        preinci64<T: Data64>(offset: StackOffset)
    >(&mut self) {
        let mut local: T = self.stack.load_fp(offset);
        local = T::wrapping_add(local, 1);
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }

    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the result onto the stack.
    fn <
        predeci8<T: Data8>(offset: StackOffset),
        predeci16<T: Data16>(offset: StackOffset),
        predeci32<T: Data32>(offset: StackOffset),
        predeci64<T: Data64>(offset: StackOffset)
    >(&mut self) {
        let mut local: T = self.stack.load_fp(offset);
        local = T::wrapping_sub(local, 1);
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }

    /// Increments the stackvalue at given offset (relative to the stack frame) and pushes the previous value onto the stack.
    fn <
        postinci8<T: Data8>(offset: StackOffset),
        postinci16<T: Data16>(offset: StackOffset),
        postinci32<T: Data32>(offset: StackOffset),
        postinci64<T: Data64>(offset: StackOffset)
    >(&mut self) {
        let local: T = self.stack.load_fp(offset);
        self.stack.store_fp(offset, T::wrapping_add(local, 1));
        self.stack.push(local);
    }

    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the previous value onto the stack.
    fn <
        postdeci8<T: Data8>(offset: StackOffset),
        postdeci16<T: Data16>(offset: StackOffset),
        postdeci32<T: Data32>(offset: StackOffset),
        postdeci64<T: Data64>(offset: StackOffset)
    >(&mut self) {
        let local: T = self.stack.load_fp(offset);
        self.stack.store_fp(offset, T::wrapping_sub(local, 1));
        self.stack.push(local);
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
    fn j0_top(&mut self, addr: StackAddress) {
        let a: Data8 = self.stack.top();
        if a == 0 {
            self.pc = addr;
        }
    }
    /// Jumps to given address if the 8 bit stack-top is not 0.
    fn jn0_top(&mut self, addr: StackAddress) {
        let a: Data8 = self.stack.top();
        if a != 0 {
            self.pc = addr;
        }
    }

    /// Constructs an instance of a non-primitive type.
    fn construct(&mut self, constructor: StackAddress, prototype: StackAddress) {
        let mut constructor = constructor; // impl_vm macro does not allow mut arguments
        let mut prototype = prototype; //if prototype < 0 { self.stack.sp() as i32 + prototype } else { prototype } as u32;
        self.construct_value(&mut constructor, &mut prototype, CopyTarget::Stack, false);
    }

    /// Constructs an instance of a non-primitive type from a prototype that was dynamically constructed on the stack.
    /// This opcode expects any contained strings to already be constructed on the heap and be stored as references on the stack.
    fn construct_dyn(&mut self, constructor: StackAddress, relative_prototype: StackAddress) {
        let mut constructor = constructor; // impl_vm macro does not allow mut arguments
        let mut prototype = (self.stack.sp() as i32 - relative_prototype as i32) as StackAddress;
        self.construct_value(&mut constructor, &mut prototype, CopyTarget::Stack, true);
    }

    /// Increase reference count for the top heap object on the stack. Does not pop the object off the stack.
    fn <
        cntinc_8(constructor: u8 as StackAddress),
        cntinc_16(constructor: u16 as StackAddress),
        cntinc_32(constructor: StackAddress),
    >(&mut self) {
        let item: HeapRef = self.stack.top();
        self.refcount_value(item, constructor, HeapRefOp::Inc);
    }
    /// Pops a heap object off the stack and decreases its reference count by 1, freeing it on 0.
    fn <
        cntdec_8(constructor: u8 as StackAddress),
        cntdec_16(constructor: u16 as StackAddress),
        cntdec_32(constructor: StackAddress),
    >(&mut self) {
        let item: HeapRef = self.stack.pop();
        self.refcount_value(item, constructor, HeapRefOp::Dec);
    }
    /// Pops a heap object off the stack and decreases its reference count by 1, freeing it on 0.
    fn <
        cntzero_8(constructor: u8 as StackAddress),
        cntzero_16(constructor: u16 as StackAddress),
        cntzero_32(constructor: StackAddress),
    >(&mut self) {
        let item: HeapRef = self.stack.top();
        self.refcount_value(item, constructor, HeapRefOp::Zero);
    }

    /// Reads the top value off the stack and pushes it onto the tmp stack.
    fn cntstore(&mut self) {
        let value: HeapRef = self.stack.top();
        self.cnt.push(value);
    }

    /// Pops the top value off the tmp stack and pushes it onto the stack.
    fn cntpop(&mut self) {
        let value: HeapRef = self.cnt.pop();
        self.stack.push(value);
    }

    /// Calls the given Rust function.
    fn rustcall(&mut self, &mut context, func: RustFn) {
        T::from_u16(func).exec(self, context);
    }

    /// Function call. Saves state and sets programm counter to given addr. Expects
    /// callee arguments on the stack.
    fn call(&mut self, addr: StackAddress, arg_size: StackAddress) {
        self.stack.push_frame(self.pc);
        self.stack.fp = self.stack.sp() - arg_size; // new frame starts with arguments
        self.pc = addr;                             // set new program counter
    }

    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn ret(&mut self, ret_size: u8) {
        let ret_size = ret_size as StackAddress;
        // move function result to the beginning of this stack frame
        if ret_size == 4 {
            let arg: u32 = self.stack.load(self.stack.sp() - 4);
            self.stack.store(self.stack.fp, arg);
        } else if ret_size > 0 {
            self.stack.copy(self.stack.sp() - ret_size, self.stack.fp, ret_size);
        }
        // truncate stack down, so that the result is the last item on the stack
        self.stack.truncate(self.stack.fp + ret_size);
        // remove stack frame, restore program counter
        self.pc = self.stack.pop_frame();
    }

    /// Pops 2 heap objects dest and src and copies num_bytes bytes from src to dest.
    fn heap_copy(&mut self, num_bytes: StackAddress) {
        let dest: HeapRef = self.stack.pop();
        let src: HeapRef = self.stack.pop();
        self.heap.copy(dest, src, num_bytes);
    }

    /// Pops 2 heap objects and compares num_bytes bytes.
    fn heap_ceq(&mut self, num_bytes: StackAddress) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare(a, b, num_bytes, HeapCmp::Eq);
        self.stack.push(equals as Data8);
    }
    /// Pops 2 heap objects and compares num_bytes bytes.
    fn heap_cneq(&mut self, num_bytes: StackAddress) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare(a, b, num_bytes, HeapCmp::Neq);
        self.stack.push(equals as Data8);
    }

    /// Pops 2 heap string slices, compares them for equality and pushes the result.
    fn string_ceq(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Eq);
        self.stack.push(equals as Data8);
    }
    /// Pops 2 heap string slices, compares them for inequality and pushes the result.
    fn string_cneq(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Neq);
        self.stack.push(equals as Data8);
    }
    /// Pops 2 heap string slices, compares them lexicographically and pushes the result.
    fn string_clt(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Lt);
        self.stack.push(equals as Data8);
    }
    /// Pops 2 heap string slices, compares them lexicographically and pushes the result.
    fn string_clte(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Lte);
        self.stack.push(equals as Data8);
    }
    /// Pops two heap slices, concatenates the heap objects into a new object and pushes its heap slice.
    fn string_concat(&mut self) {
        let src_b: HeapRef = self.stack.pop();
        let src_b_len = self.heap.size_of(src_b.index());
        let src_a: HeapRef = self.stack.pop();
        let src_a_len = self.heap.size_of(src_a.index());
        let dest_index: StackAddress = self.heap.alloc(Vec::new());
        self.heap.copy(HeapRef::new(dest_index, 0), src_a, src_a_len);
        self.heap.copy(HeapRef::new(dest_index, src_a_len), src_b, src_b_len);
        self.stack.push(HeapRef::new(dest_index, 0));
    }

    /// Pop a heap object and push the heap value at its current offset onto the stack.
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

    /// Pop a heap object and a value and store the value at current offset in the heap.
    fn <
        heap_put8<T: Data8>(),
        heap_put16<T: Data16>(),
        heap_put32<T: Data32>(),
        heap_put64<T: Data64>(),
    >(&mut self) {
        let item: HeapRef = self.stack.pop();
        let value: T = self.stack.pop();
        self.heap.write(item, value);
    }

    /// Pop a heap object and push the heap value at its current offset + given offset onto the stack.
    fn <
        heap_fetch_member8<T: Data8>(offset: StackAddress),
        heap_fetch_member16<T: Data16>(offset: StackAddress),
        heap_fetch_member32<T: Data32>(offset: StackAddress),
        heap_fetch_member64<T: Data64>(offset: StackAddress),
    >(&mut self) {
        let item: HeapRef = self.stack.pop();
        let data: T = self.heap.read(item.with_offset(offset as StackOffset));
        self.stack.push(data);
    }

    /// Pop an element index and heap object and push the heap value at element index onto the stack.
    fn <
        heap_fetch_element8<T: Data8>(),
        heap_fetch_element16<T: Data16>(),
        heap_fetch_element32<T: Data32>(),
        heap_fetch_element64<T: Data64>(),
    >(&mut self) {
        let element_index: StackAddress = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        let data: T = self.heap.read(item.with_offset((size_of::<T>() as StackAddress * element_index) as StackOffset));
        self.stack.push(data);
    }

    /// Yield program execution.
    fn yld(&mut self) return {
        self.state = VMState::Yield;
    }
    /// Terminate program execution.
    fn exit(&mut self) return {
        self.state = VMState::Terminate;
    }

    /// Does nothing. Written as comment into the opcode stream.
    #[allow(unused_variables)]
    fn comment(&mut self, text: String) {
    }
}
