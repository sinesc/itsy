//! Opcode definitions. Implemented on Writer/VM.

use crate::prelude::*;
use crate::{FrameAddress, StackAddress, ItemIndex};
use crate::bytecode::{HeapRefOp, builtins::Builtin, macros::impl_opcodes};
#[cfg(feature="runtime")]
use crate::{
    StackOffset,
    bytecode::{
        HeapRef,
        runtime::{error::RuntimeErrorKind, stack::{StackOp, StackRelativeOp}, heap::{HeapOp, HeapCmp}, vm::VMState}
    }
};

type Data8 = u8;
type Data16 = u16;
type Data32 = u32;
type Data64 = u64;

impl_opcodes!{

    [
        /// Bytecode instructions. Generated from bytecode method signatures defined via the `impl_opcodes!` macro.
        /// # Naming
        ///
        /// Instructions with multiple variants use the following naming convention:
        ///
        /// `<op>[<infix>][<suffix1>][_<suffix2>[_nc]]`
        ///
        /// `infix` refers to specialized variants that take instruction arguments to avoid stack operations\
        /// `suffix1` refers to stack inputs/outputs\
        /// `suffix2` refers to bytecode (compiletime) arguments
        ///
        /// Suffixes:
        ///
        /// `f`/`s`/`u`/`i`/`x`     float/signed/unsigned/any integer/heap reference (instruction performs refcounting)\
        /// `8`/`16`/`32`/`64`/`sa` size in bits, sa=StackAddress sized\
        /// `nc`                    operation is non-consuming, meaning stack arguments will be read but not removed
        ///
        /// Infixes:
        ///
        /// `vv` load two values from the two frame-relative address(fra) arguments\
        /// `xc` pop one value off the stack and use the argument as immediate\
        /// `xv` pop one value off the stack and load one value from the fra argument\
        /// `vc` load one value from the fra argument, use the other argument as immediate and write the result back to the fra argument
        ///
        /// # Examples
        /// `addi32`    add two 32 bit integer\
        /// `addf64`    add two 64 bit floats\
        /// `const64_8` load 64 bit from constpool at given 8 bit address
    ]

    /// Does nothing.
    fn noop(&mut self) { }

    /// Moves the stack pointer by given number of bytes to make room for local variables.
    fn reserve(&mut self, num_bytes: FrameAddress) {
        self.stack.extend_zero(num_bytes as StackAddress);
    }

    /// Discards the top num_bytes bytes on the stack.
    fn discard(&mut self, num_bytes: FrameAddress) {
        self.stack.truncate(self.stack.sp() - num_bytes as StackAddress);
    }

    /// Clones the top num_bytes bytes on the stack.
    fn clone(&mut self, num_bytes: FrameAddress) {
        self.stack.extend(self.stack.sp() - num_bytes as StackAddress, num_bytes as StackAddress);
    }

    /// Pushes given value onto the stack.
    fn <
        immediate8(value: Data8),
        immediate16(value: Data16),
        immediate32(value: Data32),
        immediate64(value: Data64),
    >(&mut self) {
        self.stack.push(value);
    }

    /// Loads data from stack at given stackframe-offset and pushes it onto the stack.
    fn <
        load8<T: Data8>(loc: FrameAddress),
        load16<T: Data16>(loc: FrameAddress),
        load32<T: Data32>(loc: FrameAddress),
        load64<T: Data64>(loc: FrameAddress),
    >(&mut self) {
        let local: T = self.stack.load_fp(loc);
        self.stack.push(local);
    }

    /// Pops data off the stack and stores it at the given stackframe-offset.
    fn <
        store8<T: Data8>(loc: FrameAddress),
        store16<T: Data16>(loc: FrameAddress),
        store32<T: Data32>(loc: FrameAddress),
        store64<T: Data64>(loc: FrameAddress),
    >(&mut self) {
        let local: T = self.stack.pop();
        self.stack.store_fp(loc, local);
    }

    /// Pops HeapRef off the stack and stores it at the given stackframe-offset.
    /// Increases refcount of the new value.
    fn storex_new(&mut self, loc: FrameAddress, constructor: StackAddress) {
        let value: HeapRef = self.stack.pop();
        self.stack.store_fp(loc, value);
        self.refcount_value(value, constructor, HeapRefOp::Inc);
    }

    /// Pops HeapRef off the stack and stores it at the given stackframe-offset.
    /// Decreses refcount of the previous contents if it was initialized and increases refcount of the new value.
    fn storex_replace(&mut self, loc: FrameAddress, constructor: StackAddress) {
        let prev: HeapRef = self.stack.load_fp(loc);
        let next: HeapRef = self.stack.pop();
        self.stack.store_fp(loc, next);
        if next != prev {
            self.refcount_value(next, constructor, HeapRefOp::Inc);
            // prev might be 0 if it was statically impossible to determine whether a variable is initialized (e.g. 'let x; if y { x = 0 } x' cannot be known)
            if prev.address != 0 {
                self.refcount_value(prev, constructor, HeapRefOp::Dec);
            }
        }
    }

    /// Sets program counter to start if value in variable iter is less than or equal to the top
    /// value on the stack. Does not consume top stack value.
    fn <
        loops8<T: i8>(iter: FrameAddress, start: StackAddress),
        loops16<T: i16>(iter: FrameAddress, start: StackAddress),
        loops32<T: i32>(iter: FrameAddress, start: StackAddress),
        loops64<T: i64>(iter: FrameAddress, start: StackAddress),
        loopu8<T: u8>(iter: FrameAddress, start: StackAddress),
        loopu16<T: u16>(iter: FrameAddress, start: StackAddress),
        loopu32<T: u32>(iter: FrameAddress, start: StackAddress),
        loopu64<T: u64>(iter: FrameAddress, start: StackAddress)
    >(&mut self) {
        use crate::bytecode::runtime::stack::StackOffsetOp;
        let iter_loc = self.stack.offset_fp(iter as StackAddress);
        let upper_loc = self.stack.offset_sp(size_of::<T>());
        let upper: T = self.stack.load(upper_loc);
        let current: T = self.stack.load(iter_loc);
        if current != upper {
            self.pc = start;
        }
        self.stack.store(iter_loc, T::wrapping_add(current, 1 as T));
    }

    /// Pops heap reference. Sets program counter to exit and returns if the reference
    /// offset points at the end of the referenced heap object.
    /// Otherwise fetches element at reference offset, moves offset to the next element,
    /// pushes the modified reference and stores the data in element.
    fn <
        arrayiter8<T: Data8>(element: FrameAddress, exit: StackAddress),
        arrayiter16<T: Data16>(element: FrameAddress, exit: StackAddress),
        arrayiter32<T: Data32>(element: FrameAddress, exit: StackAddress),
        arrayiter64<T: Data64>(element: FrameAddress, exit: StackAddress)
    >(&mut self) {
        use crate::bytecode::runtime::stack::StackOffsetOp;
        let top_loc = self.stack.offset_sp(size_of::<HeapRef>() as StackAddress);
        let mut item: HeapRef = self.stack.load(top_loc);
        if item.offset() >= self.heap.item(item.index()).data.len() {
            self.pc = exit;
            return;
        }
        let data: T = self.heap.read(item);
        item.add_offset(size_of::<T>() as StackOffset);
        self.stack.store(top_loc, item);
        self.stack.store_fp(element, data);
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
        let index: StackAddress = self.heap.alloc_place(string.into_bytes(), ItemIndex::MAX);
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
            64 => self.stack.push(if value > u64::MAX as T { u64::MAX } else if value < u64::MIN as T { u64::MIN } else { value as u64 }), // todo: nonsense when src=target size
            32 => self.stack.push(if value > u32::MAX as T { u32::MAX } else if value < u32::MIN as T { u32::MIN } else { value as u32 }),
            16 => self.stack.push(if value > u16::MAX as T { u16::MAX } else if value < u16::MIN as T { u16::MIN } else { value as u16 }),
            8 => self.stack.push(if value > u8::MAX as T { u8::MAX } else if value < u8::MIN as T { u8::MIN } else { value as u8 }),
            _ => panic!("Invalid size argument for trimu* opcode."),
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
            64 => self.stack.push(if value > i64::MAX as T { i64::MAX } else if value < i64::MIN as T { i64::MIN } else { value as i64 }), // todo: nonsense when src=target size
            32 => self.stack.push(if value > i32::MAX as T { i32::MAX } else if value < i32::MIN as T { i32::MIN } else { value as i32 }),
            16 => self.stack.push(if value > i16::MAX as T { i16::MAX } else if value < i16::MIN as T { i16::MIN } else { value as i16 }),
            8 => self.stack.push(if value > i8::MAX as T { i8::MAX } else if value < i8::MIN as T { i8::MIN } else { value as i8 }),
            _ => panic!("Invalid size argument for trims* opcode."),
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
            _ => panic!("Invalid size argument for extend* opcode."),
        };
    }

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
        adds8<T: i8>() [ check ],
        adds16<T: i16>() [ check ],
        adds32<T: i32>() [ check ],
        adds64<T: i64>() [ check ],
        addu8<T: u8>() [ check ],
        addu16<T: u16>() [ check ],
        addu32<T: u32>() [ check ],
        addu64<T: u64>() [ check ],
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        let result = T::overflowing_add(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
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

    /// Loads 2 values from the stack and pushes their sum.
    fn <
        addvvs8<T: i8>(left: FrameAddress, right: FrameAddress) [ check ],
        addvvs16<T: i16>(left: FrameAddress, right: FrameAddress) [ check ],
        addvvs32<T: i32>(left: FrameAddress, right: FrameAddress) [ check ],
        addvvs64<T: i64>(left: FrameAddress, right: FrameAddress) [ check ],
        addvvu8<T: u8>(left: FrameAddress, right: FrameAddress) [ check ],
        addvvu16<T: u16>(left: FrameAddress, right: FrameAddress) [ check ],
        addvvu32<T: u32>(left: FrameAddress, right: FrameAddress) [ check ],
        addvvu64<T: u64>(left: FrameAddress, right: FrameAddress) [ check ],
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.load_fp(left);
        let result = T::overflowing_add(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Loads 2 values from the stack and pushes their sum.
    fn <
        addvvf32<T: f32>(left: FrameAddress, right: FrameAddress),
        addvvf64<T: f64>(left: FrameAddress, right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.load_fp(left);
        self.stack.push(a + b);
    }

    /// Pops left from the stack and takes immediate argument right and pushes their sum.
    fn <
        addxcs8<T: i8>(right: i8) [ check ],
        addxcs16<T: i16>(right: i16) [ check ],
        addxcs32<T: i32>(right: i32) [ check ],
        addxcs64<T: i64>(right: i64) [ check ],
        addxcu8<T: u8>(right: u8) [ check ],
        addxcu16<T: u16>(right: u16) [ check ],
        addxcu32<T: u32>(right: u32) [ check ],
        addxcu64<T: u64>(right: u64) [ check ],
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.pop();
        let result = T::overflowing_add(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Pops left from the stack and takes immediate argument right and pushes their sum.
    fn <
        addxcf32<T: f32>(right: f32),
        addxcf64<T: f64>(right: f64)
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.pop();
        self.stack.push(a + b);
    }

    /// Pops left from the stack and loads argument right and pushes their sum.
    fn <
        addxvs8<T: i8>(right: FrameAddress) [ check ],
        addxvs16<T: i16>(right: FrameAddress) [ check ],
        addxvs32<T: i32>(right: FrameAddress) [ check ],
        addxvs64<T: i64>(right: FrameAddress) [ check ],
        addxvu8<T: u8>(right: FrameAddress) [ check ],
        addxvu16<T: u16>(right: FrameAddress) [ check ],
        addxvu32<T: u32>(right: FrameAddress) [ check ],
        addxvu64<T: u64>(right: FrameAddress) [ check ],
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        let result = T::overflowing_add(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Pops left from the stack and loads argument right and pushes their sum.
    fn <
        addxvf32<T: f32>(right: FrameAddress),
        addxvf64<T: f64>(right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        self.stack.push(a + b);
    }

    /// Loads left variable and takes immediate argument right and pushes their sum.
    fn <
        addvcs8<T: i8>(left: FrameAddress, right: i8) [ check ],
        addvcs16<T: i16>(left: FrameAddress, right: i16) [ check ],
        addvcs32<T: i32>(left: FrameAddress, right: i32) [ check ],
        addvcs64<T: i64>(left: FrameAddress, right: i64) [ check ],
        addvcu8<T: u8>(left: FrameAddress, right: u8) [ check ],
        addvcu16<T: u16>(left: FrameAddress, right: u16) [ check ],
        addvcu32<T: u32>(left: FrameAddress, right: u32) [ check ],
        addvcu64<T: u64>(left: FrameAddress, right: u64) [ check ],
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.load_fp(left);
        let result = T::overflowing_add(a, b);
        self.stack.store_fp(left, result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Loads left variable and takes immediate argument right and pushes their sum.
    fn <
        addvcf32<T: f32>(left: FrameAddress, right: f32),
        addvcf64<T: f64>(left: FrameAddress, right: f64)
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.load_fp(left);
        self.stack.store_fp(left, a + b);
    }

    /// Pops 2 values from the stack and pushes their difference.
    fn <
        subs8<T: i8>() [ check ],
        subs16<T: i16>() [ check ],
        subs32<T: i32>() [ check ],
        subs64<T: i64>() [ check ],
        subu8<T: u8>() [ check ],
        subu16<T: u16>() [ check ],
        subu32<T: u32>() [ check ],
        subu64<T: u64>() [ check ],
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        let result = T::overflowing_sub(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
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

    /// Loads 2 values from the stack and pushes their product.
    fn <
        subvvs8<T: i8>(left: FrameAddress, right: FrameAddress) [ check ],
        subvvs16<T: i16>(left: FrameAddress, right: FrameAddress) [ check ],
        subvvs32<T: i32>(left: FrameAddress, right: FrameAddress) [ check ],
        subvvs64<T: i64>(left: FrameAddress, right: FrameAddress) [ check ],
        subvvu8<T: u8>(left: FrameAddress, right: FrameAddress) [ check ],
        subvvu16<T: u16>(left: FrameAddress, right: FrameAddress) [ check ],
        subvvu32<T: u32>(left: FrameAddress, right: FrameAddress) [ check ],
        subvvu64<T: u64>(left: FrameAddress, right: FrameAddress) [ check ],
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.load_fp(left);
        let result = T::overflowing_sub(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Loads 2 values from the stack and pushes their product.
    fn <
        subvvf32<T: f32>(left: FrameAddress, right: FrameAddress),
        subvvf64<T: f64>(left: FrameAddress, right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.load_fp(left);
        self.stack.push(a - b);
    }

    /// Pops left from the stack and takes immediate argument right and pushes their sum.
    fn <
        subxcs8<T: i8>(right: i8) [ check ],
        subxcs16<T: i16>(right: i16) [ check ],
        subxcs32<T: i32>(right: i32) [ check ],
        subxcs64<T: i64>(right: i64) [ check ],
        subxcu8<T: u8>(right: u8) [ check ],
        subxcu16<T: u16>(right: u16) [ check ],
        subxcu32<T: u32>(right: u32) [ check ],
        subxcu64<T: u64>(right: u64) [ check ],
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.pop();
        let result = T::overflowing_sub(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Pops left from the stack and takes immediate argument right and pushes their sum.
    fn <
        subxcf32<T: f32>(right: f32),
        subxcf64<T: f64>(right: f64)
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.pop();
        self.stack.push(a - b);
    }

    /// Pops left from the stack and loads argument right and pushes their sum.
    fn <
        subxvs8<T: i8>(right: FrameAddress) [ check ],
        subxvs16<T: i16>(right: FrameAddress) [ check ],
        subxvs32<T: i32>(right: FrameAddress) [ check ],
        subxvs64<T: i64>(right: FrameAddress) [ check ],
        subxvu8<T: u8>(right: FrameAddress) [ check ],
        subxvu16<T: u16>(right: FrameAddress) [ check ],
        subxvu32<T: u32>(right: FrameAddress) [ check ],
        subxvu64<T: u64>(right: FrameAddress) [ check ],
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        let result = T::overflowing_sub(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Pops left from the stack and loads argument right and pushes their sum.
    fn <
        subxvf32<T: f32>(right: FrameAddress),
        subxvf64<T: f64>(right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        self.stack.push(a - b);
    }

    /// Loads left variable and takes immediate argument right and pushes their sum.
    fn <
        subvcs8<T: i8>(left: FrameAddress, right: i8) [ check ],
        subvcs16<T: i16>(left: FrameAddress, right: i16) [ check ],
        subvcs32<T: i32>(left: FrameAddress, right: i32) [ check ],
        subvcs64<T: i64>(left: FrameAddress, right: i64) [ check ],
        subvcu8<T: u8>(left: FrameAddress, right: u8) [ check ],
        subvcu16<T: u16>(left: FrameAddress, right: u16) [ check ],
        subvcu32<T: u32>(left: FrameAddress, right: u32) [ check ],
        subvcu64<T: u64>(left: FrameAddress, right: u64) [ check ],
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.load_fp(left);
        let result = T::overflowing_sub(a, b);
        self.stack.store_fp(left, result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Loads left variable and takes immediate argument right and pushes their sum.
    fn <
        subvcf32<T: f32>(left: FrameAddress, right: f32),
        subvcf64<T: f64>(left: FrameAddress, right: f64)
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.load_fp(left);
        self.stack.store_fp(left, a - b);
    }

    /// Pops 2 values from the stack and pushes their product.
    fn <
        muls8<T: i8>() [ check ],
        muls16<T: i16>() [ check ],
        muls32<T: i32>() [ check ],
        muls64<T: i64>() [ check ],
        mulu8<T: u8>() [ check ],
        mulu16<T: u16>() [ check ],
        mulu32<T: u32>() [ check ],
        mulu64<T: u64>() [ check ],
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        let result = T::overflowing_mul(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
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

    /// Loads 2 values from the stack and pushes their product.
    fn <
        mulvvs8<T: i8>(left: FrameAddress, right: FrameAddress) [ check ],
        mulvvs16<T: i16>(left: FrameAddress, right: FrameAddress) [ check ],
        mulvvs32<T: i32>(left: FrameAddress, right: FrameAddress) [ check ],
        mulvvs64<T: i64>(left: FrameAddress, right: FrameAddress) [ check ],
        mulvvu8<T: u8>(left: FrameAddress, right: FrameAddress) [ check ],
        mulvvu16<T: u16>(left: FrameAddress, right: FrameAddress) [ check ],
        mulvvu32<T: u32>(left: FrameAddress, right: FrameAddress) [ check ],
        mulvvu64<T: u64>(left: FrameAddress, right: FrameAddress) [ check ],
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.load_fp(left);
        let result = T::overflowing_mul(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Loads 2 values from the stack and pushes their product.
    fn <
        mulvvf32<T: f32>(left: FrameAddress, right: FrameAddress),
        mulvvf64<T: f64>(left: FrameAddress, right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.load_fp(left);
        self.stack.push(a * b);
    }

    /// Pops left from the stack and takes immediate argument right and pushes their product.
    fn <
        mulxcs8<T: i8>(right: i8) [ check ],
        mulxcs16<T: i16>(right: i16) [ check ],
        mulxcs32<T: i32>(right: i32) [ check ],
        mulxcs64<T: i64>(right: i64) [ check ],
        mulxcu8<T: u8>(right: u8) [ check ],
        mulxcu16<T: u16>(right: u16) [ check ],
        mulxcu32<T: u32>(right: u32) [ check ],
        mulxcu64<T: u64>(right: u64) [ check ],
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.pop();
        let result = T::overflowing_mul(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Pops left from the stack and takes immediate argument right and pushes their product.
    fn <
        mulxcf32<T: f32>(right: f32),
        mulxcf64<T: f64>(right: f64)
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.pop();
        self.stack.push(a * b);
    }

    /// Pops left from the stack and loads argument right and pushes their product.
    fn <
        mulxvs8<T: i8>(right: FrameAddress) [ check ],
        mulxvs16<T: i16>(right: FrameAddress) [ check ],
        mulxvs32<T: i32>(right: FrameAddress) [ check ],
        mulxvs64<T: i64>(right: FrameAddress) [ check ],
        mulxvu8<T: u8>(right: FrameAddress) [ check ],
        mulxvu16<T: u16>(right: FrameAddress) [ check ],
        mulxvu32<T: u32>(right: FrameAddress) [ check ],
        mulxvu64<T: u64>(right: FrameAddress) [ check ],
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        let result = T::overflowing_mul(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Pops left from the stack and loads argument right and pushes their product.
    fn <
        mulxvf32<T: f32>(right: FrameAddress),
        mulxvf64<T: f64>(right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        self.stack.push(a * b);
    }

    /// Pops 2 values from the stack and pushes their quotient.
    fn <
        divs8<T: i8>() [ check ],
        divs16<T: i16>() [ check ],
        divs32<T: i32>() [ check ],
        divs64<T: i64>() [ check ],
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(if b == 0 {
            self.state = VMState::Error(RuntimeErrorKind::DivisionByZero);
            0
        } else {
            let result = T::overflowing_div(a, b);
            if result.1 {
                self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
            }
            result.0
        });
    }

    /// Pops 2 values from the stack and pushes their quotient.
    fn <
        divu8<T: u8>() [ check ],
        divu16<T: u16>() [ check ],
        divu32<T: u32>() [ check ],
        divu64<T: u64>() [ check ],
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(if b == 0 {
            self.state = VMState::Error(RuntimeErrorKind::DivisionByZero);
            0
        } else {
            a / b
        });
    }

    /// Pops 2 values from the stack and pushes their quotient.
    fn <
        divf32<T: f32>(),
        divf64<T: f64>(),
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(a / b);
    }

    /// Pops a value off the stack and pushes its negative.
    fn <
        negs8<T: i8>() [ check ],
        negs16<T: i16>() [ check ],
        negs32<T: i32>() [ check ],
        negs64<T: i64>() [ check ],
    >(&mut self) {
        let v: T = self.stack.pop();
        let result = T::overflowing_neg(v);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Pops a value off the stack and pushes its negative.
    fn <
        negf32<T: f32>(),
        negf64<T: f64>(),
    >(&mut self) {
        let v: T = self.stack.pop();
        self.stack.push(-v);
    }

    /// Pops 2 values from the stack and pushes their remainder.
    fn <
        rems8<T: i8>() [ check ],
        rems16<T: i16>() [ check ],
        rems32<T: i32>() [ check ],
        rems64<T: i64>() [ check ],
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        let result = T::overflowing_rem(a, b);
        self.stack.push(result.0);
        if result.1 {
            self.state = VMState::Error(RuntimeErrorKind::IntegerOverflow);
        }
    }

    /// Pops 2 values from the stack and pushes their remainder.
    fn <
        remu8<T: u8>(),
        remu16<T: u16>(),
        remu32<T: u32>(),
        remu64<T: u64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push(a % b);
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

    /// Loads two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn <
        cltvvs8<T: i8>(left: FrameAddress, right: FrameAddress),
        cltvvu8<T: u8>(left: FrameAddress, right: FrameAddress),
        cltvvs16<T: i16>(left: FrameAddress, right: FrameAddress),
        cltvvu16<T: u16>(left: FrameAddress, right: FrameAddress),
        cltvvs32<T: i32>(left: FrameAddress, right: FrameAddress),
        cltvvu32<T: u32>(left: FrameAddress, right: FrameAddress),
        cltvvs64<T: i64>(left: FrameAddress, right: FrameAddress),
        cltvvu64<T: u64>(left: FrameAddress, right: FrameAddress),
        cltvvf32<T: f32>(left: FrameAddress, right: FrameAddress),
        cltvvf64<T: f64>(left: FrameAddress, right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.load_fp(left);
        self.stack.push((a < b) as Data8);
    }

    /// Pops left from the stack and loads argument right and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn <
        cltxvs8<T: i8>(right: FrameAddress),
        cltxvs16<T: i16>(right: FrameAddress),
        cltxvs32<T: i32>(right: FrameAddress),
        cltxvs64<T: i64>(right: FrameAddress),
        cltxvu8<T: u8>(right: FrameAddress),
        cltxvu16<T: u16>(right: FrameAddress),
        cltxvu32<T: u32>(right: FrameAddress),
        cltxvu64<T: u64>(right: FrameAddress),
        cltxvf32<T: f32>(right: FrameAddress),
        cltxvf64<T: f64>(right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        self.stack.push((a < b) as Data8);
    }

    /// Pops left from the stack and takes immediate argument right and pushes a 1 if the first value is lesser or equal than the second, otherwise a 0.
    fn <
        cltxcs8<T: i8>(right: i8),
        cltxcs16<T: i16>(right: i16),
        cltxcs32<T: i32>(right: i32),
        cltxcs64<T: i64>(right: i64),
        cltxcu8<T: u8>(right: u8),
        cltxcu16<T: u16>(right: u16),
        cltxcu32<T: u32>(right: u32),
        cltxcu64<T: u64>(right: u64),
        cltxcf32<T: f32>(right: f32),
        cltxcf64<T: f64>(right: f64)
    >(&mut self) {
        let b: T = right;
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

    /// Loads two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn <
        cltevvs8<T: i8>(left: FrameAddress, right: FrameAddress),
        cltevvu8<T: u8>(left: FrameAddress, right: FrameAddress),
        cltevvs16<T: i16>(left: FrameAddress, right: FrameAddress),
        cltevvu16<T: u16>(left: FrameAddress, right: FrameAddress),
        cltevvs32<T: i32>(left: FrameAddress, right: FrameAddress),
        cltevvu32<T: u32>(left: FrameAddress, right: FrameAddress),
        cltevvs64<T: i64>(left: FrameAddress, right: FrameAddress),
        cltevvu64<T: u64>(left: FrameAddress, right: FrameAddress),
        cltevvf32<T: f32>(left: FrameAddress, right: FrameAddress),
        cltevvf64<T: f64>(left: FrameAddress, right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.load_fp(left);
        self.stack.push((a <= b) as Data8);
    }

    /// Pops left from the stack and loads argument right and pushes a 1 if the first value is lesser or equal than the second, otherwise a 0.
    fn <
        cltexvs8<T: i8>(right: FrameAddress),
        cltexvs16<T: i16>(right: FrameAddress),
        cltexvs32<T: i32>(right: FrameAddress),
        cltexvs64<T: i64>(right: FrameAddress),
        cltexvu8<T: u8>(right: FrameAddress),
        cltexvu16<T: u16>(right: FrameAddress),
        cltexvu32<T: u32>(right: FrameAddress),
        cltexvu64<T: u64>(right: FrameAddress),
        cltexvf32<T: f32>(right: FrameAddress),
        cltexvf64<T: f64>(right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        self.stack.push((a <= b) as Data8);
    }

    /// Pops left from the stack and takes immediate argument right and pushes a 1 if the first value is lesser or equal than the second, otherwise a 0.
    fn <
        cltexcs8<T: i8>(right: i8),
        cltexcs16<T: i16>(right: i16),
        cltexcs32<T: i32>(right: i32),
        cltexcs64<T: i64>(right: i64),
        cltexcu8<T: u8>(right: u8),
        cltexcu16<T: u16>(right: u16),
        cltexcu32<T: u32>(right: u32),
        cltexcu64<T: u64>(right: u64),
        cltexcf32<T: f32>(right: f32),
        cltexcf64<T: f64>(right: f64)
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.pop();
        self.stack.push((a <= b) as Data8);
    }

    /// Pops two values and pushes a 1 if the first value is greater than the second, otherwise a 0.
    fn <
        cgts8<T: i8>(),
        cgtu8<T: u8>(),
        cgts16<T: i16>(),
        cgtu16<T: u16>(),
        cgts32<T: i32>(),
        cgtu32<T: u32>(),
        cgts64<T: i64>(),
        cgtu64<T: u64>(),
        cgtf32<T: f32>(),
        cgtf64<T: f64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push((a > b) as Data8);
    }

    /// Pops left from the stack and loads argument right and pushes a 1 if the first value is greater than the second, otherwise a 0.
    fn <
        cgtxvs8<T: i8>(right: FrameAddress),
        cgtxvs16<T: i16>(right: FrameAddress),
        cgtxvs32<T: i32>(right: FrameAddress),
        cgtxvs64<T: i64>(right: FrameAddress),
        cgtxvu8<T: u8>(right: FrameAddress),
        cgtxvu16<T: u16>(right: FrameAddress),
        cgtxvu32<T: u32>(right: FrameAddress),
        cgtxvu64<T: u64>(right: FrameAddress),
        cgtxvf32<T: f32>(right: FrameAddress),
        cgtxvf64<T: f64>(right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        self.stack.push((a > b) as Data8);
    }

    /// Pops left from the stack and takes immediate argument right and pushes a 1 if the first value is greater than the second, otherwise a 0.
    fn <
        cgtxcs8<T: i8>(right: i8),
        cgtxcs16<T: i16>(right: i16),
        cgtxcs32<T: i32>(right: i32),
        cgtxcs64<T: i64>(right: i64),
        cgtxcu8<T: u8>(right: u8),
        cgtxcu16<T: u16>(right: u16),
        cgtxcu32<T: u32>(right: u32),
        cgtxcu64<T: u64>(right: u64),
        cgtxcf32<T: f32>(right: f32),
        cgtxcf64<T: f64>(right: f64)
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.pop();
        self.stack.push((a > b) as Data8);
    }

    /// Pops two values and pushes a 1 if the first value is greater or equal the second, otherwise a 0.
    fn <
        cgtes8<T: i8>(),
        cgteu8<T: u8>(),
        cgtes16<T: i16>(),
        cgteu16<T: u16>(),
        cgtes32<T: i32>(),
        cgteu32<T: u32>(),
        cgtes64<T: i64>(),
        cgteu64<T: u64>(),
        cgtef32<T: f32>(),
        cgtef64<T: f64>()
    >(&mut self) {
        let b: T = self.stack.pop();
        let a: T = self.stack.pop();
        self.stack.push((a >= b) as Data8);
    }

    /// Pops left from the stack and loads argument right and pushes a 1 if the first value is greater or equal than the second, otherwise a 0.
    fn <
        cgtexvs8<T: i8>(right: FrameAddress),
        cgtexvs16<T: i16>(right: FrameAddress),
        cgtexvs32<T: i32>(right: FrameAddress),
        cgtexvs64<T: i64>(right: FrameAddress),
        cgtexvu8<T: u8>(right: FrameAddress),
        cgtexvu16<T: u16>(right: FrameAddress),
        cgtexvu32<T: u32>(right: FrameAddress),
        cgtexvu64<T: u64>(right: FrameAddress),
        cgtexvf32<T: f32>(right: FrameAddress),
        cgtexvf64<T: f64>(right: FrameAddress)
    >(&mut self) {
        let b: T = self.stack.load_fp(right);
        let a: T = self.stack.pop();
        self.stack.push((a >= b) as Data8);
    }

    /// Pops left from the stack and takes immediate argument right and pushes a 1 if the first value is greater or equal than the second, otherwise a 0.
    fn <
        cgtexcs8<T: i8>(right: i8),
        cgtexcs16<T: i16>(right: i16),
        cgtexcs32<T: i32>(right: i32),
        cgtexcs64<T: i64>(right: i64),
        cgtexcu8<T: u8>(right: u8),
        cgtexcu16<T: u16>(right: u16),
        cgtexcu32<T: u32>(right: u32),
        cgtexcu64<T: u64>(right: u64),
        cgtexcf32<T: f32>(right: f32),
        cgtexcf64<T: f64>(right: f64)
    >(&mut self) {
        let b: T = right;
        let a: T = self.stack.pop();
        self.stack.push((a >= b) as Data8);
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
    fn j0sa_nc(&mut self, addr: StackAddress) {
        let a: StackAddress = self.stack.top();
        if a == 0 {
            self.pc = addr;
        }
    }

    /// Jumps to given address if the StackAddress sized stack-top does not equal 0.
    fn jn0sa_nc(&mut self, addr: StackAddress) {
        let a: StackAddress = self.stack.top();
        if a != 0 {
            self.pc = addr;
        }
    }

    /// Function call. Creates a new stack frame at SP - arg_size and sets programm counter to given addr.
    fn call(&mut self, addr: StackAddress, arg_size: FrameAddress) {
        // stack: ... | ARGS
        self.stack.push(self.stack.fp);
        self.stack.fp = self.stack.sp() - arg_size as StackAddress - (size_of_val(&self.stack.fp) as StackAddress);
        self.stack.push(self.pc);
        self.pc = addr;
        // stack: ARGS | previous FP | previous PC | (local vars and dynamic stack follow here)
    }

    /// Calls the given Rust function.
    fn call_rust(&mut self, &mut context, rustfn: RustFn) {
        rustfn.exec(self, context);
    }

    /// Calls the given builtin function.
    fn call_builtin(&mut self, builtin: Builtin) [ check ] {
        builtin.exec(self, 0, 0);
    }

    /// Calls the given builtin function.
    fn call_builtinx(&mut self, builtin: Builtin, constructor: StackAddress, element_constructor: StackAddress) [ check ] {
        builtin.exec(self, constructor, element_constructor);
    }

    /// Dynamic function call. Pops function address of stack, creates a new stack frame at SP - arg_size and sets programm counter to given addr.
    fn call_dynamic(&mut self, arg_size: FrameAddress) {
        let heap_ref: HeapRef = self.stack.pop();
        let data = &self.heap.item(heap_ref.index()).data;
        let data_size = data.len() - (2 * size_of::<StackAddress>()); // trailing function address AND constructor offset
        self.stack.extend_from(&data[0..data_size]);
        let addr = StackAddress::from_ne_bytes(data[data_size..data_size + size_of::<StackAddress>()].try_into().unwrap());
        self.call(addr, arg_size + data_size as FrameAddress);
    }

    /// Virtual function call. Resolves concrete call address from vtable and invokes call().
    fn call_virtual(&mut self, function_base_address: StackAddress, arg_size: FrameAddress) {
        let item: HeapRef = self.stack.load_sp(arg_size as StackAddress);
        let implementor_index = self.heap.item_implementor_index(item.index());
        let addr: StackAddress = self.stack.load(function_base_address + ((implementor_index as usize) * size_of::<StackAddress>()) as StackAddress);
        self.call(addr, arg_size);
    }

    /// Function return. Restores state, removes arguments left on stack by caller.
    fn ret0(&mut self, arg_size: FrameAddress) {
        // stack: ARGS | previous FP | previous PC | local vars
        let prev_fp = self.stack.load_fp(arg_size);
        let prev_pc = self.stack.load_fp(arg_size + size_of_val(&prev_fp) as FrameAddress);
        self.stack.truncate(self.stack.fp);
        self.pc = prev_pc;
        self.stack.fp = prev_fp;
    }

    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn <
        ret8<T: Data8>(arg_size: FrameAddress),
        ret16<T: Data16>(arg_size: FrameAddress),
        ret32<T: Data32>(arg_size: FrameAddress),
        ret64<T: Data64>(arg_size: FrameAddress),
    >(&mut self) {
        // stack: ARGS | previous FP | previous PC | local vars | RESULT
        let prev_fp = self.stack.load_fp(arg_size);
        let prev_pc = self.stack.load_fp(arg_size + size_of_val(&prev_fp) as FrameAddress);
        let ret: T = self.stack.top();
        self.stack.store_fp(0, ret);
        self.stack.truncate(self.stack.fp + size_of::<T>() as StackAddress);
        self.pc = prev_pc;
        self.stack.fp = prev_fp;
    }

    /// Pops a heap reference off the stack and performs a reference count operation.
    fn <
        cnt_16(constructor: u16 as StackAddress, op: HeapRefOp),
        cnt_sa(constructor: StackAddress, op: HeapRefOp),
    >(&mut self) {
        let item: HeapRef = self.stack.pop();
        self.refcount_value(item, constructor, op);
    }

    /// Performs a non-consuming reference count operation for the top heap reference on the stack.
    fn <
        cnt_16_nc(constructor: u16 as StackAddress, op: HeapRefOp),
        cnt_sa_nc(constructor: StackAddress, op: HeapRefOp),
    >(&mut self) {
        let item: HeapRef = self.stack.top();
        self.refcount_value(item, constructor, op);
    }

    /// Moves an instance that was constructed on the stack to the heap.
    fn upload(&mut self, size: StackAddress, implementor_index: ItemIndex) {
        let data_start = self.stack.sp() as usize - size as usize;
        let heap_ref = self.heap.alloc_copy(&self.stack.data()[data_start..], implementor_index);
        self.stack.truncate(data_start as StackAddress);
        self.stack.push(HeapRef::new(heap_ref, 0));
    }

    /// Loads primitive data from the const pool directly onto the heap, pushing the new heap reference to the stack.
    fn upload_const(&mut self, const_offset: StackAddress) {
        let num_bytes: StackAddress = self.stack.load(const_offset);
        let heap_ref = HeapRef::new(self.heap.alloc(num_bytes, ItemIndex::MAX), 0);
        self.stack.push(heap_ref);
        let data_offset = const_offset as usize + size_of_val(&num_bytes);
        let src = self.stack.data();
        self.heap.item_mut(heap_ref.index()).data.extend_from_slice(&src[data_offset..data_offset + num_bytes as usize]);
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

    /// Pops 2 heap references to strings, compares the strings lexicographically and pushes the result. Drops temporary references.
    fn string_cgt(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Gt);
        self.stack.push(equals as Data8);
        self.heap.ref_item(a.index(), HeapRefOp::Free);
        self.heap.ref_item(b.index(), HeapRefOp::Free);
    }

    /// Pops 2 heap references to strings, compares the strings lexicographically and pushes the result. Drops temporary references.
    fn string_cgte(&mut self) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a, b, HeapCmp::Gte);
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
        let dest_index = self.heap.alloc((a_len + b_len) * 3 / 2, ItemIndex::MAX);
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

    /// Pop StackAddress sized "index" and heap reference and push the resulting heap reference with offset += index * element_size onto the stack.
    fn index(&mut self, element_size: u16) {
        let element_index: StackAddress = self.stack.pop();
        let mut item: HeapRef = self.stack.pop();
        item.add_offset(element_index as StackOffset * element_size as StackOffset);
        self.stack.push(item);
    }

    /// Offsets the heap address at the top of the stack by given value.
    fn offsetx_16(&mut self, offset: FrameAddress) {
        let mut item: HeapRef = self.stack.pop();
        item.add_offset(offset as StackOffset);
        self.stack.push(item);
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

    /// Pop a heap reference and push the heap value at its current offset + given offset onto the stack. Drops temporary references.
    fn heap_fetch_memberx(&mut self, offset: StackAddress, constructor: StackAddress) {
        let item: HeapRef = self.stack.pop();
        let data: HeapRef = self.heap.read(item.with_offset(offset as StackOffset));
        self.heap.ref_item(data.index(), HeapRefOp::Inc); // non recursive is fine since we only want to prevent it from being dropped and will reverse the change immediately
        self.refcount_value(item, constructor, HeapRefOp::Free);
        self.heap.ref_item(data.index(), HeapRefOp::DecNoFree);
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

    /// Pop an element index and heap reference and push the heap value at element index onto the stack. Drops temporary references.
    fn heap_fetch_elementx(&mut self, constructor: StackAddress) {
        let element_index: StackAddress = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        let data: HeapRef = self.heap.read(item.with_offset((size_of::<HeapRef>() as StackAddress * element_index) as StackOffset));
        self.heap.ref_item(data.index(), HeapRefOp::Inc); // non recursive is fine since we only want to prevent it from being dropped and will reverse the change immediately
        self.refcount_value(item, constructor, HeapRefOp::Free);
        self.heap.ref_item(data.index(), HeapRefOp::DecNoFree);
        self.stack.push(data);
    }

    /* /// Yield program execution.
    fn yld(&mut self) return {
        self.state = VMState::Yielded;
    }*/

    /// Terminate program execution.
    fn exit(&mut self) [ return ] {
        self.state = VMState::Terminated;
    }

    /// Does nothing. Written as comment into the opcode stream.
    #[allow(unused_variables)]
    #[cfg(all(feature="symbols", feature="comments"))]
    fn comment(&mut self, text: String) {
    }
}
