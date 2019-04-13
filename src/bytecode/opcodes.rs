//! Opcode definitions. Implemented on Writer/VM.

#[macro_use]
mod helper;
use crate::{util::{array1, array2, array4, array8}, runtime::{Value, Value64, StackOp, StackOpFp, HeapOp}};

impl_vm!{

    /// Does nothing.
    fn noop(self: &mut Self) { }

    /// Moves the stack pointer by given number of items (32bit words) to make room for local variables.
    fn reserve(self: &mut Self, num_items: u8) {
        for _ in 0..num_items {
            self.stack.push(0);
        }
    }

    /// Load 2 byte from constant pool onto stack.
    fn const_fetch16(self: &mut Self, const_id: u8) {
        self.const_fetch16_16(const_id as u16);
    }
    /// Load 2 byte from constant pool onto stack.
    fn const_fetch16_16(self: &mut Self, const_id: u16) {
        let const_id = const_id as usize;
        let tmp: u16 = u16::from_le_bytes(array2(&self.program.consts[const_id..const_id +2]));
        self.stack.push(tmp);
    }

    /// Load 4 byte from constant pool onto stack.
    fn const_fetch(self: &mut Self, const_id: u8) {
        self.const_fetch_16(const_id as u16);
    }
    /// Load 4 byte from constant pool onto stack.
    fn const_fetch_16(self: &mut Self, const_id: u16) {
        let const_id = const_id as usize;
        let tmp: u32 = u32::from_le_bytes(array4(&self.program.consts[const_id..const_id +4]));
        self.stack.push(tmp);
    }

    /// Load 8 byte from constant pool onto stack.
    fn const_fetch64(self: &mut Self, const_id: u8) {
        self.const_fetch64_16(const_id as u16);
    }
    /// Load 8 byte from constant pool onto stack.
    fn const_fetch64_16(self: &mut Self, const_id: u16) {
        let const_id = const_id as usize;
        let tmp: u64 = u64::from_le_bytes(array8(&self.program.consts[const_id..const_id +8]));
        self.stack.push(tmp);
    }

    /// Load object from constant pool onto stack+heap.
    fn const_fetch_object(self: &mut Self, const_id: u8) {
        self.const_fetch_object_16(const_id as u16);
    }
    /// Load object from constant pool onto stack+heap.
    fn const_fetch_object_16(self: &mut Self, const_id: u16) {
        let pos = const_id as usize;
        let len = u32::from_le_bytes(array4(&self.program.consts[pos .. pos + 4])) as usize;
        let data = self.program.consts[pos + 4 .. pos + 4 + len].to_vec();
        let heap_index: u32 = self.heap.store(data);
        self.stack.push(heap_index);
    }

    /// Push value onto stack.
    fn litu(self: &mut Self, value: u8) {
        self.stack.push(value);
    }
    /// Push value onto stack.
    fn lits(self: &mut Self, value: i8) {
        self.stack.push(value); // note to future self: litu/lits are required because the i8 needs to be sign extended on the (32 bit) stack
    }
    /// Push 0 onto stack.
    fn lit0(self: &mut Self) {
        self.stack.push(0u32);
    }
    /// Push 1 onto stack.
    fn lit1(self: &mut Self) {
        self.stack.push(1u32);
    }
    /// Push 2 onto stack.
    fn lit2(self: &mut Self) {
        self.stack.push(2u32);
    }
    /// Push -1 onto stack.
    fn litm1(self: &mut Self) {
        self.stack.push(-1i32);
    }

    /// Load function argument 1 and push it onto the stack. Equivalent to load -4.
    fn load_arg1(self: &mut Self) {
        let local: Value = self.stack.load_fp(-4);
        self.stack.push(local);
    }
    /// Load function argument 2 and push it onto the stack. Equivalent to load -5.
    fn load_arg2(self: &mut Self) {
        let local: Value = self.stack.load_fp(-5);
        self.stack.push(local);
    }
    /// Load function argument 3 and push it onto the stack. Equivalent to load -6.
    fn load_arg3(self: &mut Self) {
        let local: Value = self.stack.load_fp(-6);
        self.stack.push(local);
    }

    /// Load stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn loadr_s8(self: &mut Self, offset: i8) {
        self.loadr_s32(offset as i32);
    }
    /// Load stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn loadr_s16(self: &mut Self, offset: i16) {
        self.loadr_s32(offset as i32);
    }
    /// Load stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn loadr_s32(self: &mut Self, offset: i32) {
        let local: Value = self.stack.load_fp(offset);
        self.stack.push(local);
    }
    /// Pop stackvalue and store it at the given offset (relative to the stackframe).
    fn storer_s8(self: &mut Self, offset: i8) {
        self.storer_s32(offset as i32);
    }
    /// Pop stackvalue and store it at the given offset (relative to the stackframe).
    fn storer_s16(self: &mut Self, offset: i16) {
        self.storer_s32(offset as i32);
    }
    /// Pop stackvalue and store it at the given offset (relative to the stackframe).
    fn storer_s32(self: &mut Self, offset: i32) {
        let local: Value = self.stack.pop();
        self.stack.store_fp(offset, local);
    }

    /// Load 64 bit stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn loadr64_s8(self: &mut Self, offset: i8) {
        self.loadr64_s32(offset as i32);
    }
    /// Load 64 bit stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn loadr64_s16(self: &mut Self, offset: i16) {
        self.loadr64_s32(offset as i32);
    }
    /// Load 64 bit stackvalue from offset (relative to the stackframe) and push onto the stack.
    fn loadr64_s32(self: &mut Self, offset: i32) {
        let local: Value64 = self.stack.load_fp(offset);
        self.stack.push(local);
    }
    /// Pop 64 bit stackvalue and store it at the given offset (relative to the stackframe).
    fn storer64_s8(self: &mut Self, offset: i8) {
        self.storer64_s32(offset as i32);
    }
    /// Pop 64 bit stackvalue and store it at the given offset (relative to the stackframe).
    fn storer64_s16(self: &mut Self, offset: i16) {
        self.storer64_s32(offset as i32);
    }
    /// Pop 64 bit stackvalue and store it at the given offset (relative to the stackframe).
    fn storer64_s32(self: &mut Self, offset: i32) {
        let local: Value64 = self.stack.pop();
        self.stack.store_fp(offset, local);
    }

    /// Pop two u32 "index" and "offset" and push the result of offset + index * element_size onto the stack.
    fn index(self: &mut Self, element_size: u8) {
        self.index_32(element_size as u32);
    }
    /// Pop two u32 "index" and "offset" and push the result of offset + index * element_size onto the stack.
    fn index_16(self: &mut Self, element_size: u16) {
        self.index_32(element_size as u32);
    }
    /// Pop two u32 "index" and "offset" and push the result of offset + index * element_size onto the stack.
    fn index_32(self: &mut Self, element_size: u32) {
        let element_index: u32 = self.stack.pop();
        let current_offset: u32 = self.stack.pop();
        self.stack.push(current_offset + (element_index as u32 * element_size));
    }

    /// Pop a heap object and push the heap value at its current index+offset onto the stack.
    fn heap_fetch_member8(self: &mut Self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let data = self.heap.read8(heap_index, heap_offset + offset);
        self.stack.push(data);
    }
    /// Pop a heap object and push the heap value at its current index+offset onto the stack.
    fn heap_fetch_member16(self: &mut Self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let data = self.heap.read16(heap_index, heap_offset + offset);
        self.stack.push(data);
    }
    /// Pop a heap object and push the heap value at its current index+offset onto the stack.
    fn heap_fetch_member32(self: &mut Self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let data = self.heap.read32(heap_index, heap_offset + offset);
        self.stack.push(data);
    }
    /// Pop a heap object and push the heap value at its current index+offset onto the stack.
    fn heap_fetch_member64(self: &mut Self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let data = self.heap.read64(heap_index, heap_offset + offset);
        self.stack.push(data);
    }

    /// Pop a value and a heap object and store the value at current heap index+offset in the heap.
    fn heap_put_member8(self: &mut Self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let value: u8 = self.stack.pop();
        self.heap.write8(heap_index, heap_offset + offset, value);
    }
    /// Pop a value and a heap object and store the value at current heap index+offset in the heap.
    fn heap_put_member16(self: &mut Self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let value: u16 = self.stack.pop();
        self.heap.write16(heap_index, heap_offset + offset, value);
    }
    /// Pop a value and a heap object and store the value at current heap index+offset in the heap.
    fn heap_put_member32(self: &mut Self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let value: u32 = self.stack.pop();
        self.heap.write32(heap_index, heap_offset + offset, value);
    }
    /// Pop a value and a heap object and store the value at current heap index+offset in the heap.
    fn heap_put_member64(self: &mut Self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let value: u64 = self.stack.pop();
        self.heap.write64(heap_index, heap_offset + offset, value);
    }

    /// Pop an element index and heap object and push the heap value at element index onto the stack.
    fn heap_fetch_element8(self: &mut Self) {
        let element_index: u32 = self.stack.pop();
        let current_heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let heap_offset = current_heap_offset + (1 * element_index as u32);
        let data = self.heap.read8(heap_index, heap_offset);
        self.stack.push(data);
    }
    /// Pop an element index and heap object and push the heap value at element index * 2 onto the stack.
    fn heap_fetch_element16(self: &mut Self) {
        let element_index: u32 = self.stack.pop();
        let current_heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let heap_offset = current_heap_offset + (2 * element_index as u32);
        let data = self.heap.read16(heap_index, heap_offset);
        self.stack.push(data);
    }
    /// Pop an element index and heap object and push the heap value at element index * 4 onto the stack.
    fn heap_fetch_element32(self: &mut Self) {
        let element_index: u32 = self.stack.pop();
        let current_heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let heap_offset = current_heap_offset + (4 * element_index as u32);
        let data = self.heap.read32(heap_index, heap_offset);
        self.stack.push(data);
    }
    /// Pop an element index and heap object and push the heap value at element index * 8 onto the stack.
    fn heap_fetch_element64(self: &mut Self) {
        let element_index: u32 = self.stack.pop();
        let current_heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let heap_offset = current_heap_offset + (8 * element_index as u32);
        let data = self.heap.read64(heap_index, heap_offset);
        self.stack.push(data);
    }

    /// Pop an element index and heap object and push the heap value at element index * 8 onto the stack.
    fn heap_put_element8(self: &mut Self) {
        let element_index: u32 = self.stack.pop();
        let current_heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let heap_offset = current_heap_offset + (1 * element_index as u32);
        let value: u8 = self.stack.pop();
        self.heap.write8(heap_index, heap_offset, value);
    }
    /// Pop an element index and heap object and push the heap value at element index * 8 onto the stack.
    fn heap_put_element16(self: &mut Self) {
        let element_index: u32 = self.stack.pop();
        let current_heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let heap_offset = current_heap_offset + (2 * element_index as u32);
        let value: u16 = self.stack.pop();
        self.heap.write16(heap_index, heap_offset, value);
    }
    /// Pop an element index and heap object and push the heap value at element index * 8 onto the stack.
    fn heap_put_element32(self: &mut Self) {
        let element_index: u32 = self.stack.pop();
        let current_heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let heap_offset = current_heap_offset + (4 * element_index as u32);
        let value: u32 = self.stack.pop();
        self.heap.write32(heap_index, heap_offset, value);
    }
    /// Pop an element index and heap object and push the heap value at element index * 8 onto the stack.
    fn heap_put_element64(self: &mut Self) {
        let element_index: u32 = self.stack.pop();
        let current_heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let heap_offset = current_heap_offset + (8 * element_index as u32);
        let value: u64 = self.stack.pop();
        self.heap.write64(heap_index, heap_offset, value);
    }

    /// Clones the top stack value and pushes it.
    fn clone32(self: &mut Self) {
        let data: Value = self.stack.top();
        self.stack.push(data);
    }
    /// Clones the top stack value and pushes it.
    fn clone64(self: &mut Self) {
        let data: Value64 = self.stack.top();
        self.stack.push(data);
    }
/*
    /// Calls the given Rust function.
    fn rustcall(self: &mut Self, func: RustFn) {
        T::from_u16(func).exec(self);
    }
*/
    /// Function call. Saves state and sets programm counter to given addr. Expects
    /// callee arguments on the stack and number of arguments (in 32 bit words) as num_args.
    fn call(self: &mut Self, addr: u32, num_args: u8) { // todo: move num_args to ret? avoids one push
        let next_pc = self.pc;
        let fp = self.stack.fp;
        self.stack.push(num_args);          // save number of arguments
        self.stack.push(fp);                // save frame pointer
        self.stack.push(next_pc);           // save program counter as it would be after this instruction
        self.stack.fp = self.stack.sp();    // set new frame pointer
        self.pc = addr;                     // set new program counter
    }
    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn ret(self: &mut Self, num_ret: u8) {

        let num_ret = num_ret as u32;

        // get previous state
        let prev_pc = self.stack.load_fp(-1);           // load program counter from before the call
        let prev_fp = self.stack.load_fp(-2);           // load old frame pointer
        let prev_num_args: u8 = self.stack.load_fp(-3); // load number of arguments that were on the stack prior to call

        // truncate stack back down to the start of the callframe minus 3 (the above three states) minus the number
        // of arguments pushed by the caller prior to call (so that the caller doesn't have to clean them up).
        let ret_pos = self.stack.fp - 3 - prev_num_args as u32;

        for i in 0..num_ret {   // todo: this could be avoided if caller were to clean up the stack (prev_pc/fp stuff)
            self.stack[ret_pos + i] = self.stack[self.stack.sp() - num_ret + i];
        }

        self.stack.truncate(ret_pos + num_ret);

        // restore previous program counter and frame pointer
        self.stack.fp = prev_fp;
        self.pc = prev_pc;
    }

    /// Pops value off the stack, truncates it to the given number of bits and pushes the result.
    /// Allowed values for size are 8 and 16.
    fn trunc(self: &mut Self, size: u8) { // todo: might as well make 2 opcodes
        let value: Value = self.stack.pop();
        self.stack.push(value & ((1 << size as u32) - 1));
    }
    /// Pops a 64 bit value off the stack, truncates it to the given number of bits and pushes the 32 bit result.
    /// Allowed values for size are 8, 16 and 32.
    fn trunc64(self: &mut Self, size: u8) {
        let value: Value64 = self.stack.pop();
        self.stack.push((value & ((1 << size as u64) - 1)) as u32);
    }

    /// Pops a value off the stack, sign-extends it *by* the given number of bits up to a maximum of 32 and pushes the result.
    fn extends(self: &mut Self, num_bits: u8) {
        let value: Value = self.stack.pop();
        self.stack.push(value.wrapping_shl(num_bits as u32).wrapping_shr(num_bits as u32));
    }
    /// Pops a max 32 bit value off the stack, sign-extends it *by* the given number of bits up to a maximum of 64 and pushes the 64 bit result.
    fn extends64(self: &mut Self, num_bits: u8) { // todo: naming shouldn't be 64 since its popping 32 bits. we don't have a convention for output size though
        let value: Value = self.stack.pop();
        let value = value as i64;
        self.stack.push(value.wrapping_shl(num_bits as u32).wrapping_shr(num_bits as u32));
    }

    /// Pops an i64 and pushes its f32 representation.
    fn i64tof32(self: &mut Self) {
        let value: Value64 = self.stack.pop();
        self.stack.push(value as f32);
    }
    /// Pops an i64 and pushes its f64 representation.
    fn i64tof64(self: &mut Self) {
        let value: Value64 = self.stack.pop();
        self.stack.push(value as f64);
    }

    /// Pops an f32 and pushes its i64 representation.
    fn f32toi64(self: &mut Self) {
        let value: f32 = self.stack.pop();
        self.stack.push(value as i64);
    }
    /// Pops an f64 and pushes its i64 representation.
    fn f64toi64(self: &mut Self) {
        let value: f64 = self.stack.pop();
        self.stack.push(value as i64);
    }

    /// Pops an f32 and pushes its f64 representation.
    fn f32tof64(self: &mut Self) {
        let value: f32 = self.stack.pop();
        self.stack.push(value as f64);
    }
    /// Pops an f64 and pushes its f32 representation.
    fn f64tof32(self: &mut Self) {
        let value: f64 = self.stack.pop();
        self.stack.push(value as f32);
    }

    /// Pops 2 values from the stack and pushes their logical conjunction.
    fn and(self: &mut Self) {
        let a: bool = self.stack.pop();
        let b: bool = self.stack.pop();
        self.stack.push(a && b);
    }
    /// Pops 2 values from the stack and pushes their logical disjunction.
    fn or(self: &mut Self) {
        let a: bool = self.stack.pop();
        let b: bool = self.stack.pop();
        self.stack.push(a || b);
    }
    /// Pops a values from the stack and pushes its logical negation.
    fn not(self: &mut Self) {
        let a: bool = self.stack.pop();
        self.stack.push(!a);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addi(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their difference.
    fn subi(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn muli(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push(a * b);
    }
    /// Pops 2 values from the stack and pushes their quotient.
    fn divi(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push(a / b);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addf(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their sum.
    fn subf(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn mulf(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push(a * b);
    }
    /// Pops 2 values from the stack and pushes their quotient.
    fn divf(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push(a / b);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addi64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their difference.
    fn subi64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn muli64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push(a * b);
    }
    /// Pops 2 values from the stack and pushes their quotient.
    fn divi64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push(a / b);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addf64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their sum.
    fn subf64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn mulf64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push(a * b);
    }
    /// Pops 2 values from the stack and pushes their quotient.
    fn divf64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push(a / b);
    }

    /// Jumps unconditionally to the given address.
    fn jmp(self: &mut Self, addr: u32) {
        self.pc = addr;
    }
    /// Pops one values and jumps to given address if the value is 0.
    fn j0(self: &mut Self, addr: u32) {
        let a: Value = self.stack.pop();
        if a == 0 {
            self.pc = addr;
        }
    }
    /// Pops one values and jumps to given address if the value is not 0.
    fn jn0(self: &mut Self, addr: u32) {
        let a: Value = self.stack.pop();
        if a != 0 {
            self.pc = addr;
        }
    }

    // todo: comparisons need to be generated somehow, this is bound to contain copy&paste errors

    /// Pops two values and jumps to given address it they equal.
    fn jeqr32(self: &mut Self, addr: u32) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        if a == b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address it they equal.
    fn jeqr64(self: &mut Self, addr: u32) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        if a == b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address it they are not equal.
    fn jneqr32(self: &mut Self, addr: u32) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        if a != b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address it they are not equal.
    fn jneqr64(self: &mut Self, addr: u32) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        if a != b {
            self.pc = addr;
        }
    }

    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jlts32(self: &mut Self, addr: u32) {
        let a: i32 = self.stack.pop();
        let b: i32 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltu32(self: &mut Self, addr: u32) {
        let a: u32 = self.stack.pop();
        let b: u32 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltf32(self: &mut Self, addr: u32) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jlts64(self: &mut Self, addr: u32) {
        let a: i64 = self.stack.pop();
        let b: i64 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltu64(self: &mut Self, addr: u32) {
        let a: u64 = self.stack.pop();
        let b: u64 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltf64(self: &mut Self, addr: u32) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }

    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltes32(self: &mut Self, addr: u32) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jlteu32(self: &mut Self, addr: u32) {
        let a: u32 = self.stack.pop();
        let b: u32 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltef32(self: &mut Self, addr: u32) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltes64(self: &mut Self, addr: u32) {
        let a: i64 = self.stack.pop();
        let b: i64 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jlteu64(self: &mut Self, addr: u32) {
        let a: u64 = self.stack.pop();
        let b: u64 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltef64(self: &mut Self, addr: u32) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }

    /// Pops two values and pushes a 1 if the first value equals the second, otherwise a 0.
    fn ceqr32(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push((a == b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value equals the second, otherwise a 0.
    fn ceqr64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push((a == b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value does not equal the second, otherwise a 0.
    fn cneqr32(self: &mut Self) {
        let a: Value = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push((a != b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value does not equal the second, otherwise a 0.
    fn cneqr64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push((a != b) as Value);
    }

    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn clts32(self: &mut Self) {
        let a: i32 = self.stack.pop();
        let b: i32 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltu32(self: &mut Self) {
        let a: u32 = self.stack.pop();
        let b: u32 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltf32(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn clts64(self: &mut Self) {
        let a: i64 = self.stack.pop();
        let b: i64 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltu64(self: &mut Self) {
        let a: u64 = self.stack.pop();
        let b: u64 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltf64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }

    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltes32(self: &mut Self) {
        let a: i32 = self.stack.pop();
        let b: i32 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn clteu32(self: &mut Self) {
        let a: u32 = self.stack.pop();
        let b: u32 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltef32(self: &mut Self) {
        let a: f32 = self.stack.pop();
        let b: f32 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltes64(self: &mut Self) {
        let a: i64 = self.stack.pop();
        let b: i64 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn clteu64(self: &mut Self) {
        let a: u64 = self.stack.pop();
        let b: u64 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltef64(self: &mut Self) {
        let a: f64 = self.stack.pop();
        let b: f64 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }

    /// Increments the value at the top of the stack.
    fn inci(self: &mut Self) {
        let a: Value = self.stack.pop();
        self.stack.push(a + 1);
    }
    /// Decrements the value at the top of the stack.
    fn deci(self: &mut Self) {
        let a: Value = self.stack.pop();
        self.stack.push(a - 1);
    }
    /// Increments the value at the top of the stack.
    fn inci64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        self.stack.push(a + 1);
    }
    /// Decrements the value at the top of the stack.
    fn deci64(self: &mut Self) {
        let a: Value64 = self.stack.pop();
        self.stack.push(a - 1);
    }

    /// Increments the stackvalue at given offset (relative to the stackframe) and pushes the result onto the stack.
    fn preinci(self: &mut Self, offset: i32) {
        let mut local: Value = self.stack.load_fp(offset);
        local += 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stackframe) and pushes the result onto the stack.
    fn predeci(self: &mut Self, offset: i32) {
        let mut local: Value = self.stack.load_fp(offset);
        local -= 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }
    /// Increments the stackvalue at given offset (relative to the stackframe) and pushes the result onto the stack.
    fn preinci64(self: &mut Self, offset: i32) {
        let mut local: Value64 = self.stack.load_fp(offset);
        local += 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stackframe) and pushes the result onto the stack.
    fn predeci64(self: &mut Self, offset: i32) {
        let mut local: Value64 = self.stack.load_fp(offset);
        local -= 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }

    /// Increments the stackvalue at given offset (relative to the stackframe) and pushes the previous value onto the stack.
    fn postinci(self: &mut Self, offset: i32) {
        let local: Value = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local + 1);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stackframe) and pushes the previous value onto the stack.
    fn postdeci(self: &mut Self, offset: i32) {
        let local: Value = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local - 1);
        self.stack.push(local);
    }
    /// Increments the stackvalue at given offset (relative to the stackframe) and pushes the previous value onto the stack.
    fn postinci64(self: &mut Self, offset: i32) {
        let local: Value64 = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local + 1);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stackframe) and pushes the previous value onto the stack.
    fn postdeci64(self: &mut Self, offset: i32) {
        let local: Value64 = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local - 1);
        self.stack.push(local);
    }

    /// Yield program execution.
    fn yld(self: &mut Self) {
        self.state = crate::runtime::VMState::Yield;
    }
    /// Terminate program execution.
    fn exit(self: &mut Self) {
        self.state = crate::runtime::VMState::Terminate;
    }

    /// Does nothing. Written as comment into the opcode stream.
    #[allow(unused_variables)]
    fn comment(self: &mut Self, text: String) { }
}
