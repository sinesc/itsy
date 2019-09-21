//! Opcode definitions. Implemented on Writer/VM.

#[macro_use]
mod helper;
use crate::bytecode::{ARG1, ARG2, ARG3};
use crate::util::{array1, array2, array4, array8};
use crate::runtime::{Value, Value64, StackOp, StackOpRel, HeapOp, HeapCmp, HeapRef};

impl_vm!{

    /// Does nothing.
    fn noop(&mut self) { }

    /// Moves the stack pointer by given number of items (32bit words) to make room for local variables.
    fn reserve(&mut self, num_items: u8) {
        for _ in 0..num_items {
            self.stack.push(0);
        }
    }

    /// Discards the top 32 bit stack value.
    fn discard32(&mut self) {
        let _value: Value = self.stack.pop();
    }

    /// Load 2 byte from constant pool onto stack.
    fn const_fetch16(&mut self, const_id: u8) {
        self.const_fetch16_32(const_id as u32);
    }
    /// Load 2 byte from constant pool onto stack.
    fn const_fetch16_16(&mut self, const_id: u16) {
        self.const_fetch16_32(const_id as u32);
    }
    /// Load 2 byte from constant pool onto stack.
    fn const_fetch16_32(&mut self, const_id: u32) {
        let const_id = const_id as usize;
        let tmp: u16 = u16::from_le_bytes(array2(&self.program.consts[const_id..const_id +2]));
        self.stack.push(tmp);
    }

    /// Load 4 byte from constant pool onto stack.
    fn const_fetch32(&mut self, const_id: u8) {
        self.const_fetch32_32(const_id as u32);
    }
    /// Load 4 byte from constant pool onto stack.
    fn const_fetch32_16(&mut self, const_id: u16) {
        self.const_fetch32_32(const_id as u32);
    }
    /// Load 4 byte from constant pool onto stack.
    fn const_fetch32_32(&mut self, const_id: u32) {
        let const_id = const_id as usize;
        let tmp: u32 = u32::from_le_bytes(array4(&self.program.consts[const_id..const_id +4]));
        self.stack.push(tmp);
    }

    /// Load 8 byte from constant pool onto stack.
    fn const_fetch64(&mut self, const_id: u8) {
        self.const_fetch64_32(const_id as u32);
    }
    /// Load 8 byte from constant pool onto stack.
    fn const_fetch64_16(&mut self, const_id: u16) {
        self.const_fetch64_32(const_id as u32);
    }
    /// Load 8 byte from constant pool onto stack.
    fn const_fetch64_32(&mut self, const_id: u32) {
        let const_id = const_id as usize;
        let tmp: u64 = u64::from_le_bytes(array8(&self.program.consts[const_id..const_id +8]));
        self.stack.push(tmp);
    }

    /// Load object from constant pool onto stack+heap.
    fn const_fetch_object(&mut self, const_id: u8) {
        self.const_fetch_object_32(const_id as u32);
    }
    /// Load object from constant pool onto stack+heap.
    fn const_fetch_object_16(&mut self, const_id: u16) {
        self.const_fetch_object_32(const_id as u32);
    }
    /// Load object from constant pool onto stack+heap.
    fn const_fetch_object_32(&mut self, const_id: u32) {
        let pos = const_id as usize;
        let len = u32::from_le_bytes(array4(&self.program.consts[pos .. pos + 4])) as usize;
        let data = self.program.consts[pos + 4 .. pos + 4 + len].to_vec();
        let heap_index: u32 = self.heap.store(data);
        self.stack.push(heap_index);
    }

    /// Push value onto stack.
    fn litu(&mut self, value: u8) {
        self.stack.push(value);
    }
    /// Push value onto stack.
    fn lits(&mut self, value: i8) {
        self.stack.push(value); // note to future self: litu/lits are required because the i8 needs to be sign extended on the (32 bit) stack
    }
    /// Push 0 onto stack.
    fn lit0(&mut self) {
        self.stack.push(0u32);
    }
    /// Push 1 onto stack.
    fn lit1(&mut self) {
        self.stack.push(1u32);
    }
    /// Push 2 onto stack.
    fn lit2(&mut self) {
        self.stack.push(2u32);
    }
    /// Push -1 onto stack.
    fn litm1(&mut self) {
        self.stack.push(-1i32);
    }

    /// Load function argument 1 and push it onto the stack.
    fn load_arg1(&mut self) {
        let local: Value = self.stack.load_fp(ARG1);
        self.stack.push(local);
    }
    /// Load function argument 2 and push it onto the stack.
    fn load_arg2(&mut self) {
        let local: Value = self.stack.load_fp(ARG2);
        self.stack.push(local);
    }
    /// Load function argument 3 and push it onto the stack.
    fn load_arg3(&mut self) {
        let local: Value = self.stack.load_fp(ARG3);
        self.stack.push(local);
    }

    /// Load stackvalue from offset (relative to the stack frame) and push onto the stack.
    fn loadr32_s8(&mut self, offset: i8) {
        self.loadr32_s32(offset as i32);
    }
    /// Load stackvalue from offset (relative to the stack frame) and push onto the stack.
    fn loadr32_s16(&mut self, offset: i16) {
        self.loadr32_s32(offset as i32);
    }
    /// Load stackvalue from offset (relative to the stack frame) and push onto the stack.
    fn loadr32_s32(&mut self, offset: i32) {
        let local: Value = self.stack.load_fp(offset);
        self.stack.push(local);
    }
    /// Pop stackvalue and store it at the given offset (relative to the stack frame).
    fn storer32_s8(&mut self, offset: i8) {
        self.storer32_s32(offset as i32);
    }
    /// Pop stackvalue and store it at the given offset (relative to the stack frame).
    fn storer32_s16(&mut self, offset: i16) {
        self.storer32_s32(offset as i32);
    }
    /// Pop stackvalue and store it at the given offset (relative to the stack frame).
    fn storer32_s32(&mut self, offset: i32) {
        let local: Value = self.stack.pop();
        self.stack.store_fp(offset, local);
    }

    /// Load 64 bit stackvalue from offset (relative to the stack frame) and push onto the stack.
    fn loadr64_s8(&mut self, offset: i8) {
        self.loadr64_s32(offset as i32);
    }
    /// Load 64 bit stackvalue from offset (relative to the stack frame) and push onto the stack.
    fn loadr64_s16(&mut self, offset: i16) {
        self.loadr64_s32(offset as i32);
    }
    /// Load 64 bit stackvalue from offset (relative to the stack frame) and push onto the stack.
    fn loadr64_s32(&mut self, offset: i32) {
        let local: Value64 = self.stack.load_fp(offset);
        self.stack.push(local);
    }
    /// Pop 64 bit stackvalue and store it at the given offset (relative to the stack frame).
    fn storer64_s8(&mut self, offset: i8) {
        self.storer64_s32(offset as i32);
    }
    /// Pop 64 bit stackvalue and store it at the given offset (relative to the stack frame).
    fn storer64_s16(&mut self, offset: i16) {
        self.storer64_s32(offset as i32);
    }
    /// Pop 64 bit stackvalue and store it at the given offset (relative to the stack frame).
    fn storer64_s32(&mut self, offset: i32) {
        let local: Value64 = self.stack.pop();
        self.stack.store_fp(offset, local);
    }

    /// Pop two u32 "index" and "offset" and push the result of offset + index * element_size onto the stack.
    fn index(&mut self, element_size: u8) {
        self.index_32(element_size as u32);
    }
    /// Pop two u32 "index" and "offset" and push the result of offset + index * element_size onto the stack.
    fn index_16(&mut self, element_size: u16) {
        self.index_32(element_size as u32);
    }
    /// Pop two u32 "index" and "offset" and push the result of offset + index * element_size onto the stack.
    fn index_32(&mut self, element_size: u32) {
        let element_index: u32 = self.stack.pop();
        let current_offset: u32 = self.stack.pop();
        self.stack.push(current_offset + (element_index as u32 * element_size));
    }

    // Reads the top 32 bit value off the stack and pushes it onto the tmp stack.
    fn store_tmp32(&mut self) {
        let value: Value = self.stack.top();
        self.tmp.push(value);
    }
    // Reads the top 64 bit value off the stack and pushes it onto the tmp stack.
    fn store_tmp64(&mut self) {
        let value: Value64 = self.stack.top();
        self.tmp.push(value);
    }
    // Reads the top 32 bit value off the tmp stack and pushes it onto the stack.
    fn load_tmp32(&mut self) {
        let value: Value = self.tmp.top();
        self.stack.push(value);
    }
    // Reads the top 64 bit value off the tmp stack and pushes it onto the stack.
    fn load_tmp64(&mut self) {
        let value: Value64 = self.tmp.top();
        self.stack.push(value);
    }
    // Pops the 32 bit value off the stack and pushes it onto the tmp stack.
    fn push_tmp32(&mut self) {
        let value: Value = self.stack.pop();
        self.tmp.push(value);
    }
    // Pops the 64 bit value off the stack and pushes it onto the tmp stack.
    fn push_tmp64(&mut self) {
        let value: Value64 = self.stack.pop();
        self.tmp.push(value);
    }
    // Pops the top 32 bit value off the tmp stack and pushes it onto the stack.
    fn pop_tmp32(&mut self) {
        let value: Value = self.tmp.pop();
        self.stack.push(value);
    }
    // Pops the top 64 bit value off the tmp stack and pushes it onto the stack.
    fn pop_tmp64(&mut self) {
        let value: Value64 = self.tmp.pop();
        self.stack.push(value);
    }

    /// Increase reference count for the top heap object on the stack. Does not pop the object off the stack.
    fn heap_ref(&mut self) {
        let item: HeapRef = self.stack.load_sp(-2);
        self.heap.inc_ref(item.index);
    }
    /// Pops a heap object off the stack and decreases its reference count by 1, freeing it on 0.
    fn heap_unref(&mut self) {
        let item: HeapRef = self.stack.pop();
        self.heap.dec_ref_or_free(item.index);
    }
    /// Pops a heap object off the stack and decreases its reference count by 1, freeing it on 0 unless it matches the heap object index immediately above on the stack.
    fn heap_unref_result(&mut self) {
        let item: HeapRef = self.stack.pop();
        let prior_item: HeapRef = self.stack.load_sp(-2);
        //println!("prior: {} current: {}", prior_heap_index, heap_index);
        if item.index == prior_item.index {
            self.heap.dec_ref(item.index);
        } else {
            self.heap.dec_ref_or_free(item.index);
        }
    }

    /// Pops 2 heap objects dest and src and copies num_bytes bytes from src to dest.
    fn heap_copy_32(&mut self, num_bytes: u32) {
        let dest: HeapRef = self.stack.pop();
        let src: HeapRef = self.stack.pop();
        self.heap.copy(dest.index, dest.offset, src.index, src.offset, num_bytes);
    }

    /// Pops 2 heap objects and compares num_bytes bytes.
    fn heap_ceq(&mut self, num_bytes: u32) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare(a.index, a.offset, b.index, b.offset, num_bytes, HeapCmp::Eq);
        self.stack.push(equals as Value);
    }
    /// Pops 2 heap objects and compares num_bytes bytes.
    fn heap_cneq(&mut self, num_bytes: u32) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare(a.index, a.offset, b.index, b.offset, num_bytes, HeapCmp::Neq);
        self.stack.push(equals as Value);
    }

    /// Pops 2 heap objects as strings and compares up to num_bytes bytes or the entire string if num_bytes is 0.
    fn string_ceq(&mut self, num_bytes: u32) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a.index, a.offset, b.index, b.offset, num_bytes, HeapCmp::Eq);
        self.stack.push(equals as Value);
    }
    /// Pops 2 heap objects as strings and compares up to num_bytes bytes or the entire string if num_bytes is 0.
    fn string_cneq(&mut self, num_bytes: u32) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a.index, a.offset, b.index, b.offset, num_bytes, HeapCmp::Neq);
        self.stack.push(equals as Value);
    }
    /// Pops 2 heap objects as strings and compares up to num_bytes bytes or the entire string if num_bytes is 0.
    fn string_clt(&mut self, num_bytes: u32) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a.index, a.offset, b.index, b.offset, num_bytes, HeapCmp::Lt);
        self.stack.push(equals as Value);
    }
    /// Pops 2 heap objects as strings and compares up to num_bytes bytes or the entire string if num_bytes is 0.
    fn string_clte(&mut self, num_bytes: u32) {
        let b: HeapRef = self.stack.pop();
        let a: HeapRef = self.stack.pop();
        let equals = self.heap.compare_string(a.index, a.offset, b.index, b.offset, num_bytes, HeapCmp::Lte);
        self.stack.push(equals as Value);
    }
    /// Concatenates two heap objects into a new object.
    fn string_concat(&mut self) {
        let src_b: HeapRef = self.stack.pop();
        let src_a: HeapRef = self.stack.pop();

        let heap_index_dest: u32 = self.heap.alloc();

        let num_len_a = self.heap.size_of(src_a.index);
        self.heap.copy(heap_index_dest, 0, src_a.index, src_a.offset, num_len_a);

        let num_len_b = self.heap.size_of(src_b.index);
        self.heap.copy(heap_index_dest, num_len_a, src_b.index, src_b.offset, num_len_b);

        self.stack.push(HeapRef { index: heap_index_dest, offset: 0 });
    }

    /// Pop a heap object and push the heap value at its current offset onto the stack.
    fn heap_fetch8(&mut self) {
        let item: HeapRef = self.stack.pop();
        let data = self.heap.read8(item.index, item.offset);
        self.stack.push(data);
    }
    /// Pop a heap object and push the heap value at its current offset onto the stack.
    fn heap_fetch16(&mut self) {
        let item: HeapRef = self.stack.pop();
        let data = self.heap.read16(item.index, item.offset);
        self.stack.push(data);
    }
    /// Pop a heap object and push the heap value at its current offset onto the stack.
    fn heap_fetch32(&mut self) {
        let item: HeapRef = self.stack.pop();
        let data = self.heap.read32(item.index, item.offset);
        self.stack.push(data);
    }
    /// Pop a heap object and push the heap value at its current offset onto the stack.
    fn heap_fetch64(&mut self) {
        let item: HeapRef = self.stack.pop();
        let data = self.heap.read64(item.index, item.offset);
        self.stack.push(data);
    }

    /// Pop a heap object and a value and store the value at current offset in the heap.
    fn heap_put8(&mut self) {
        let item: HeapRef = self.stack.pop();
        let value = self.stack.pop();
        self.heap.write8(item.index, item.offset, value);
    }
    /// Pop a heap object and a value and store the value at current offset in the heap.
    fn heap_put16(&mut self) {
        let item: HeapRef = self.stack.pop();
        let value = self.stack.pop();
        self.heap.write16(item.index, item.offset, value);
    }
    /// Pop a heap object and a value and store the value at current offset in the heap.
    fn heap_put32(&mut self) {
        let item: HeapRef = self.stack.pop();
        let value = self.stack.pop();
        self.heap.write32(item.index, item.offset, value);
    }
    /// Pop a heap object and a value and store the value at current offset in the heap.
    fn heap_put64(&mut self) {
        let item: HeapRef = self.stack.pop();
        let value = self.stack.pop();
        self.heap.write64(item.index, item.offset, value);
    }

    /// Pop a heap object and push the heap value at its current offset + given offset onto the stack.
    fn heap_fetch_member8(&mut self, offset: u32) {
        let item: HeapRef = self.stack.pop();
        let data = self.heap.read8(item.index, item.offset + offset);
        self.stack.push(data);
    }
    /// Pop a heap object and push the heap value at its current offset + given offset onto the stack.
    fn heap_fetch_member16(&mut self, offset: u32) {
        let item: HeapRef = self.stack.pop();
        let data = self.heap.read16(item.index, item.offset + offset);
        self.stack.push(data);
    }
    /// Pop a heap object and push the heap value at its current offset + given offset onto the stack.
    fn heap_fetch_member32(&mut self, offset: u32) {
        let item: HeapRef = self.stack.pop();
        let data = self.heap.read32(item.index, item.offset + offset);
        self.stack.push(data);
    }
    /// Pop a heap object and push the heap value at its current offset + given offset onto the stack.
    fn heap_fetch_member64(&mut self, offset: u32) {
        let item: HeapRef = self.stack.pop();
        let data = self.heap.read64(item.index, item.offset + offset);
        self.stack.push(data);
    }
/*
    /// Pop a heap object and a value and store the value at current offset + given offset in the heap.
    fn heap_put_member8(&mut self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let value = self.stack.pop();
        self.heap.write8(heap_index, heap_offset + offset, value);
    }
    /// Pop a heap object and a value and store the value at current offset + given offset in the heap.
    fn heap_put_member16(&mut self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let value = self.stack.pop();
        self.heap.write16(heap_index, heap_offset + offset, value);
    }
    /// Pop a heap object and a value and store the value at current offset + given offset in the heap.
    fn heap_put_member32(&mut self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let value = self.stack.pop();
        self.heap.write32(heap_index, heap_offset + offset, value);
    }
    /// Pop a heap object and a value and store the value at current offset + given offset in the heap.
    fn heap_put_member64(&mut self, offset: u32) {
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let value = self.stack.pop();
        self.heap.write64(heap_index, heap_offset + offset, value);
    }
*/
    /// Pop an element index and heap object and push the heap value at element index onto the stack.
    fn heap_fetch_element8(&mut self) {
        let element_index: u32 = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        let source_heap_offset = item.offset + (1 * element_index as u32);
        let data = self.heap.read8(item.index, source_heap_offset);
        self.stack.push(data);
    }
    /// Pop an element index and heap object and push the heap value at element index * 2 onto the stack.
    fn heap_fetch_element16(&mut self) {
        let element_index: u32 = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        let source_heap_offset = item.offset + (2 * element_index as u32);
        let data = self.heap.read16(item.index, source_heap_offset);
        self.stack.push(data);
    }
    /// Pop an element index and heap object and push the heap value at element index * 4 onto the stack.
    fn heap_fetch_element32(&mut self) {
        let element_index: u32 = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        let source_heap_offset = item.offset + (4 * element_index as u32);
        let data = self.heap.read32(item.index, source_heap_offset);
        self.stack.push(data);
    }
    /// Pop an element index and heap object and push the heap value at element index * 8 onto the stack.
    fn heap_fetch_element64(&mut self) {
        let element_index: u32 = self.stack.pop();
        let item: HeapRef = self.stack.pop();
        let source_heap_offset = item.offset + (8 * element_index as u32);
        let data = self.heap.read64(item.index, source_heap_offset);
        self.stack.push(data);
    }
/*
    /// Pop an element index, a heap object and a value off the stack and store the value at current offset + element index in the heap.
    fn heap_put_element8(&mut self) {
        let element_index: u32 = self.stack.pop();
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let target_heap_offset = heap_offset + (1 * element_index as u32);
        let value = self.stack.pop();
        self.heap.write8(heap_index, target_heap_offset, value);
    }
    /// Pop an element index, a heap object and a value off the stack and store the value at current offset + element index * 2 in the heap.
    fn heap_put_element16(&mut self) {
        let element_index: u32 = self.stack.pop();
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let target_heap_offset = heap_offset + (2 * element_index as u32);
        let value = self.stack.pop();
        self.heap.write16(heap_index, target_heap_offset, value);
    }
    /// Pop an element index, a heap object and a value off the stack and store the value at current offset + element index * 4 in the heap.
    fn heap_put_element32(&mut self) {
        let element_index: u32 = self.stack.pop();
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let target_heap_offset = heap_offset + (4 * element_index as u32);
        let value = self.stack.pop();
        self.heap.write32(heap_index, target_heap_offset, value);
    }
    /// Pop an element index, a heap object and a value off the stack and store the value at current offset + element index * 8 in the heap.
    fn heap_put_element64(&mut self) {
        let element_index: u32 = self.stack.pop();
        let heap_offset: u32 = self.stack.pop();
        let heap_index: u32 = self.stack.pop();
        let target_heap_offset = heap_offset + (8 * element_index as u32);
        let value = self.stack.pop();
        self.heap.write64(heap_index, target_heap_offset, value);
    }
*/
    /// Reads a 32 bit value from the n-th stack element relative to the top of the (32 bit) stack and pushes it.
    /// n=0 is the topmost 32 bit stack value, n=1 the previous value.
    fn clone32(&mut self, n: u8) {
        let data: Value = self.stack.load(self.stack.sp() - 1 - n as u32);
        self.stack.push(data);
    }
    /// Reads a 64 bit value from the n-th stack element relative to the top of the (32 bit) stack and pushes it.
    /// n=0 is the topmost 64 bit stack value, n=2 the previous value.
    fn clone64(&mut self, n: u8) {
        let data: Value64 = self.stack.load(self.stack.sp() - 2 - n as u32);
        self.stack.push(data);
    }

    /// Swap the 2 topmost 32 bit stack values.
    fn swap32(&mut self) {
        let pos_a = self.stack.sp() - 1;
        let pos_b = pos_a - 1;
        let a: Value = self.stack.load(pos_a);
        let b: Value = self.stack.load(pos_b);
        self.stack.store(pos_a, b);
        self.stack.store(pos_b, a);
    }
    /// Swap the 2 topmost 64 bit stack values.
    fn swap64(&mut self) {
        let pos_a = self.stack.sp() - 2;
        let pos_b = pos_a - 2;
        let a: Value64 = self.stack.load(pos_a);
        let b: Value64 = self.stack.load(pos_b);
        self.stack.store(pos_a, b);
        self.stack.store(pos_b, a);
    }
    /// Swap the two topmost 64 and 32 bit stack values.
    fn swap64_32(&mut self) {
        let a: Value64 = self.stack.pop();
        let b: Value = self.stack.pop();
        self.stack.push(a);
        self.stack.push(b);
    }
    /// Swap the two topmost 32 and 64 bit stack values.
    fn swap32_64(&mut self) {
        let a: Value = self.stack.pop();
        let b: Value64 = self.stack.pop();
        self.stack.push(a);
        self.stack.push(b);
    }

    /// Calls the given Rust function.
    fn rustcall(&mut self, &mut context, func: RustFn) {
        T::from_u16(func).exec(self, context);
    }

    /// Function call. Saves state and sets programm counter to given addr. Expects
    /// callee arguments on the stack.
    fn call(&mut self, addr: u32) {
        let next_pc = self.pc;
        let fp = self.stack.fp;
        self.stack.push(fp);                // save frame pointer
        self.stack.push(next_pc);           // save program counter as it would be after this instruction
        self.stack.fp = self.stack.sp();    // set new frame pointer
        self.pc = addr;                     // set new program counter
    }
    /// Function return. Restores state, removes arguments left on stack by caller and
    /// leaves call result on the stack.
    fn ret(&mut self, num_ret: u8, num_args: u8) {

        let num_ret = num_ret as u32;

        // get previous state
        let prev_pc = self.stack.load_fp(-1);           // load program counter from before the call
        let prev_fp = self.stack.load_fp(-2);           // load old frame pointer

        // truncate stack back down to the start of the callframe minus 2 (the above states) minus the number
        // of arguments pushed by the caller prior to call (so that the caller doesn't have to clean them up).
        let ret_pos = self.stack.fp - 2 - num_args as u32;

        // appears to be slightly faster than always looping, even for non-empty functions
        if num_ret == 1 {
            self.stack[ret_pos] = self.stack[self.stack.sp() - num_ret];
        } else if num_ret == 2 {
            self.stack[ret_pos] = self.stack[self.stack.sp() - num_ret];
            self.stack[ret_pos + 1] = self.stack[self.stack.sp() - num_ret + 1];
        } else if num_ret > 2 {
            for i in 0..num_ret {
                self.stack[ret_pos + i] = self.stack[self.stack.sp() - num_ret + i];
            }
        }

        self.stack.truncate(ret_pos + num_ret);

        // restore previous program counter and frame pointer
        self.stack.fp = prev_fp;
        self.pc = prev_pc;
    }

    /// Pops value off the stack, truncates it to the given number of bits and pushes the result.
    /// Allowed values for size are 8 and 16.
    fn trunc(&mut self, size: u8) { // todo: might as well make 2 opcodes
        let value: Value = self.stack.pop();
        self.stack.push(value & ((1 << size as u32) - 1));
    }
    /// Pops a 64 bit value off the stack, truncates it to the given number of bits and pushes the 32 bit result.
    /// Allowed values for size are 8, 16 and 32.
    fn trunc64(&mut self, size: u8) {
        let value: Value64 = self.stack.pop();
        self.stack.push((value & ((1 << size as u64) - 1)) as u32);
    }

    /// Pops a value off the stack, sign-extends it *by* the given number of bits up to a maximum of 32 and pushes the result.
    fn extends(&mut self, num_bits: u8) {
        let value: Value = self.stack.pop();
        self.stack.push(value.wrapping_shl(num_bits as u32).wrapping_shr(num_bits as u32));
    }
    /// Pops a max 32 bit value off the stack, sign-extends it *by* the given number of bits up to a maximum of 64 and pushes the 64 bit result.
    fn extends64(&mut self, num_bits: u8) { // todo: naming shouldn't be 64 since its popping 32 bits. we don't have a convention for output size though
        let value: Value = self.stack.pop();
        let value = value as i64;
        self.stack.push(value.wrapping_shl(num_bits as u32).wrapping_shr(num_bits as u32));
    }

    /// Pops an i64 and pushes its f32 representation.
    fn i64tof32(&mut self) {
        let value: Value64 = self.stack.pop();
        self.stack.push(value as f32);
    }
    /// Pops an i64 and pushes its f64 representation.
    fn i64tof64(&mut self) {
        let value: Value64 = self.stack.pop();
        self.stack.push(value as f64);
    }

    /// Pops an f32 and pushes its i64 representation.
    fn f32toi64(&mut self) {
        let value: f32 = self.stack.pop();
        self.stack.push(value as i64);
    }
    /// Pops an f64 and pushes its i64 representation.
    fn f64toi64(&mut self) {
        let value: f64 = self.stack.pop();
        self.stack.push(value as i64);
    }

    /// Pops an f32 and pushes its f64 representation.
    fn f32tof64(&mut self) {
        let value: f32 = self.stack.pop();
        self.stack.push(value as f64);
    }
    /// Pops an f64 and pushes its f32 representation.
    fn f64tof32(&mut self) {
        let value: f64 = self.stack.pop();
        self.stack.push(value as f32);
    }

    /// Pops 2 values from the stack and pushes their logical conjunction.
    fn and(&mut self) {
        let b: bool = self.stack.pop();
        let a: bool = self.stack.pop();
        self.stack.push(a && b);
    }
    /// Pops 2 values from the stack and pushes their logical disjunction.
    fn or(&mut self) {
        let b: bool = self.stack.pop();
        let a: bool = self.stack.pop();
        self.stack.push(a || b);
    }
    /// Pops a values from the stack and pushes its logical negation.
    fn not(&mut self) {
        let a: bool = self.stack.pop();
        self.stack.push(!a);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addi(&mut self) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their difference.
    fn subi(&mut self) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn muli(&mut self) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        self.stack.push(a * b);
    }
    /// Pops 2 values from the stack and pushes their quotient.
    fn divi(&mut self) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        self.stack.push(a / b);
    }
    /// Pops 2 values from the stack and pushes their remainder.
    fn remi(&mut self) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        self.stack.push(a % b);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addf(&mut self) {
        let b: f32 = self.stack.pop();
        let a: f32 = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their sum.
    fn subf(&mut self) {
        let b: f32 = self.stack.pop();
        let a: f32 = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn mulf(&mut self) {
        let b: f32 = self.stack.pop();
        let a: f32 = self.stack.pop();
        self.stack.push(a * b);
    }
    /// Pops 2 values from the stack and pushes their quotient.
    fn divf(&mut self) {
        let b: f32 = self.stack.pop();
        let a: f32 = self.stack.pop();
        self.stack.push(a / b);
    }
    /// Pops 2 values from the stack and pushes their remainder.
    fn remf(&mut self) {
        let b: f32 = self.stack.pop();
        let a: f32 = self.stack.pop();
        self.stack.push(a % b);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addi64(&mut self) {
        let b: Value64 = self.stack.pop();
        let a: Value64 = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their difference.
    fn subi64(&mut self) {
        let b: Value64 = self.stack.pop();
        let a: Value64 = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn muli64(&mut self) {
        let b: Value64 = self.stack.pop();
        let a: Value64 = self.stack.pop();
        self.stack.push(a * b);
    }
    /// Pops 2 values from the stack and pushes their quotient.
    fn divi64(&mut self) {
        let b: Value64 = self.stack.pop();
        let a: Value64 = self.stack.pop();
        self.stack.push(a / b);
    }
    /// Pops 2 values from the stack and pushes their remainder.
    fn remi64(&mut self) {
        let b: Value64 = self.stack.pop();
        let a: Value64 = self.stack.pop();
        self.stack.push(a % b);
    }

    /// Pops 2 values from the stack and pushes their sum.
    fn addf64(&mut self) {
        let b: f64 = self.stack.pop();
        let a: f64 = self.stack.pop();
        self.stack.push(a + b);
    }
    /// Pops 2 values from the stack and pushes their sum.
    fn subf64(&mut self) {
        let b: f64 = self.stack.pop();
        let a: f64 = self.stack.pop();
        self.stack.push(a - b);
    }
    /// Pops 2 values from the stack and pushes their product.
    fn mulf64(&mut self) {
        let b: f64 = self.stack.pop();
        let a: f64 = self.stack.pop();
        self.stack.push(a * b);
    }
    /// Pops 2 values from the stack and pushes their quotient.
    fn divf64(&mut self) {
        let b: f64 = self.stack.pop();
        let a: f64 = self.stack.pop();
        self.stack.push(a / b);
    }
    /// Pops 2 values from the stack and pushes their remainder.
    fn remf64(&mut self) {
        let b: f64 = self.stack.pop();
        let a: f64 = self.stack.pop();
        self.stack.push(a % b);
    }

    /// Jumps unconditionally to the given address.
    fn jmp(&mut self, addr: u32) {
        self.pc = addr;
    }
    /// Pops one values and jumps to given address if the value is 0.
    fn j0(&mut self, addr: u32) {
        let a: Value = self.stack.pop();
        if a == 0 {
            self.pc = addr;
        }
    }
    /// Pops one values and jumps to given address if the value is not 0.
    fn jn0(&mut self, addr: u32) {
        let a: Value = self.stack.pop();
        if a != 0 {
            self.pc = addr;
        }
    }
    /// Jumps to given address if the 32bit stack-top is 0.
    fn j0_top(&mut self, addr: u32) {
        let a: Value = self.stack.top();
        if a == 0 {
            self.pc = addr;
        }
    }
    /// Jumps to given address if the 32bit stack-top is not 0.
    fn jn0_top(&mut self, addr: u32) {
        let a: Value = self.stack.top();
        if a != 0 {
            self.pc = addr;
        }
    }

    // todo: comparisons need to be generated somehow, this is bound to contain copy&paste errors

    /// Pops two values and jumps to given address it they equal.
    fn jeqr32(&mut self, addr: u32) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        if a == b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address it they equal.
    fn jeqr64(&mut self, addr: u32) {
        let b: Value64 = self.stack.pop();
        let a: Value64 = self.stack.pop();
        if a == b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address it they are not equal.
    fn jneqr32(&mut self, addr: u32) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        if a != b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address it they are not equal.
    fn jneqr64(&mut self, addr: u32) {
        let b: Value64 = self.stack.pop();
        let a: Value64 = self.stack.pop();
        if a != b {
            self.pc = addr;
        }
    }

    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jlts32(&mut self, addr: u32) {
        let b: i32 = self.stack.pop();
        let a: i32 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltu32(&mut self, addr: u32) {
        let b: u32 = self.stack.pop();
        let a: u32 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltf32(&mut self, addr: u32) {
        let b: f32 = self.stack.pop();
        let a: f32 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jlts64(&mut self, addr: u32) {
        let b: i64 = self.stack.pop();
        let a: i64 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltu64(&mut self, addr: u32) {
        let b: u64 = self.stack.pop();
        let a: u64 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less than the second.
    fn jltf64(&mut self, addr: u32) {
        let b: f64 = self.stack.pop();
        let a: f64 = self.stack.pop();
        if a < b {
            self.pc = addr;
        }
    }

    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltes32(&mut self, addr: u32) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jlteu32(&mut self, addr: u32) {
        let b: u32 = self.stack.pop();
        let a: u32 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltef32(&mut self, addr: u32) {
        let b: f32 = self.stack.pop();
        let a: f32 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltes64(&mut self, addr: u32) {
        let b: i64 = self.stack.pop();
        let a: i64 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jlteu64(&mut self, addr: u32) {
        let b: u64 = self.stack.pop();
        let a: u64 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }
    /// Pops two values and jumps to given address if the first value is less/equal the second.
    fn jltef64(&mut self, addr: u32) {
        let b: f64 = self.stack.pop();
        let a: f64 = self.stack.pop();
        if a <= b {
            self.pc = addr;
        }
    }

    /// Pops two values and pushes a 1 if the first value equals the second, otherwise a 0.
    fn ceqr32(&mut self) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        self.stack.push((a == b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value equals the second, otherwise a 0.
    fn ceqr64(&mut self) {
        let b: Value64 = self.stack.pop();
        let a: Value64 = self.stack.pop();
        self.stack.push((a == b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value does not equal the second, otherwise a 0.
    fn cneqr32(&mut self) {
        let b: Value = self.stack.pop();
        let a: Value = self.stack.pop();
        self.stack.push((a != b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value does not equal the second, otherwise a 0.
    fn cneqr64(&mut self) {
        let b: Value64 = self.stack.pop();
        let a: Value64 = self.stack.pop();
        self.stack.push((a != b) as Value);
    }

    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn clts32(&mut self) {
        let b: i32 = self.stack.pop();
        let a: i32 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltu32(&mut self) {
        let b: u32 = self.stack.pop();
        let a: u32 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltf32(&mut self) {
        let b: f32 = self.stack.pop();
        let a: f32 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn clts64(&mut self) {
        let b: i64 = self.stack.pop();
        let a: i64 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltu64(&mut self) {
        let b: u64 = self.stack.pop();
        let a: u64 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser than the second, otherwise a 0.
    fn cltf64(&mut self) {
        let b: f64 = self.stack.pop();
        let a: f64 = self.stack.pop();
        self.stack.push((a < b) as Value);
    }

    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltes32(&mut self) {
        let b: i32 = self.stack.pop();
        let a: i32 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn clteu32(&mut self) {
        let b: u32 = self.stack.pop();
        let a: u32 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltef32(&mut self) {
        let b: f32 = self.stack.pop();
        let a: f32 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltes64(&mut self) {
        let b: i64 = self.stack.pop();
        let a: i64 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn clteu64(&mut self) {
        let b: u64 = self.stack.pop();
        let a: u64 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }
    /// Pops two values and pushes a 1 if the first value is lesser or equal the second, otherwise a 0.
    fn cltef64(&mut self) {
        let b: f64 = self.stack.pop();
        let a: f64 = self.stack.pop();
        self.stack.push((a <= b) as Value);
    }

    /// Increments the value at the top of the stack.
    fn inci(&mut self) {
        let a: Value = self.stack.pop();
        self.stack.push(a + 1);
    }
    /// Decrements the value at the top of the stack.
    fn deci(&mut self) {
        let a: Value = self.stack.pop();
        self.stack.push(a - 1);
    }
    /// Increments the value at the top of the stack.
    fn inci64(&mut self) {
        let a: Value64 = self.stack.pop();
        self.stack.push(a + 1);
    }
    /// Decrements the value at the top of the stack.
    fn deci64(&mut self) {
        let a: Value64 = self.stack.pop();
        self.stack.push(a - 1);
    }

    /// Increments the stackvalue at given offset (relative to the stack frame) and pushes the result onto the stack.
    fn preinci(&mut self, offset: i32) {
        let mut local: Value = self.stack.load_fp(offset);
        local += 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the result onto the stack.
    fn predeci(&mut self, offset: i32) {
        let mut local: Value = self.stack.load_fp(offset);
        local -= 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }
    /// Increments the stackvalue at given offset (relative to the stack frame) and pushes the result onto the stack.
    fn preinci64(&mut self, offset: i32) {
        let mut local: Value64 = self.stack.load_fp(offset);
        local += 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the result onto the stack.
    fn predeci64(&mut self, offset: i32) {
        let mut local: Value64 = self.stack.load_fp(offset);
        local -= 1;
        self.stack.store_fp(offset, local);
        self.stack.push(local);
    }

    /// Increments the stackvalue at given offset (relative to the stack frame) and pushes the previous value onto the stack.
    fn postinci(&mut self, offset: i32) {
        let local: Value = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local + 1);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the previous value onto the stack.
    fn postdeci(&mut self, offset: i32) {
        let local: Value = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local - 1);
        self.stack.push(local);
    }
    /// Increments the stackvalue at given offset (relative to the stack frame) and pushes the previous value onto the stack.
    fn postinci64(&mut self, offset: i32) {
        let local: Value64 = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local + 1);
        self.stack.push(local);
    }
    /// Decrements the stackvalue at given offset (relative to the stack frame) and pushes the previous value onto the stack.
    fn postdeci64(&mut self, offset: i32) {
        let local: Value64 = self.stack.load_fp(offset);
        self.stack.store_fp(offset, local - 1);
        self.stack.push(local);
    }

    /// Yield program execution.
    fn yld(&mut self) {
        self.state = crate::runtime::VMState::Yield;
    }
    /// Terminate program execution.
    fn exit(&mut self) {
        self.state = crate::runtime::VMState::Terminate;
    }

    /// Does nothing. Written as comment into the opcode stream.
    #[allow(unused_variables)]
    fn comment(&mut self, text: String) { }
}
