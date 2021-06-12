//! A virtual machine for running Itsy bytecode.

use std::convert::TryInto;
use std::mem::size_of;
use crate::bytecode::{Program, ConstDescriptor, ConstEndianness};
use crate::util::Constructor;
use crate::runtime::*;

/// Current state of the vm, checked after each instruction.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VMState {
    /// Yield after current instruction. The program can be resumed after a yield.
    Yield,
    /// Terminate after current instruction. The program state will be reset.
    Terminate,
    /// A runtime error was encountered.
    RuntimeError,
}

#[derive(Copy, Clone, Debug)]
pub enum CopyTarget {
    Stack,
    Heap(HeapRef),
}

/// A virtual machine for running Itsy bytecode.
#[derive(Debug)]
pub struct VM<T, U> where T: crate::runtime::VMFunc<T> {
    context_type            : std::marker::PhantomData<U>,
    func_type               : std::marker::PhantomData<T>,
    pub(crate) instructions : Vec<u8>,
    pub(crate) pc           : u32,
    pub(crate) state        : VMState,
    pub stack               : Stack,
    pub heap                : Heap,
    pub(crate) tmp          : Stack,
    pub(crate) tmp_fp       : u32,
}

/// Public VM methods.
impl<T, U> VM<T, U> where T: crate::runtime::VMFunc<T>+crate::runtime::VMData<T, U> {
    /// Create a new VM instance with the given Program.
    pub fn new(program: Program<T>) -> Self {
        let Program { instructions, consts, const_descriptors, .. } = program;
        let stack = Self::init_consts(consts, const_descriptors);
        VM {
            context_type: std::marker::PhantomData,
            func_type   : std::marker::PhantomData,
            instructions: instructions,
            pc          : 0,
            state       : VMState::Terminate,
            stack       : stack,
            heap        : Heap::new(),
            tmp         : Stack::new(),
            tmp_fp      : 0,
        }
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self, context: &mut U) -> &mut Self {
        self.exec(context);
        if self.state == VMState::Terminate {
            if self.heap.len() > 0 {
                panic!("{} Heap elements remaining after program termination: {:?}", self.heap.len(), self.heap.data());
            }
            self.reset();
        }
        self
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.reset();
        self.heap.reset();
        self.tmp.reset();
        self.pc = 0;
        self.state = VMState::Terminate;
        self.tmp_fp = 0;
    }

    /// Disassembles the bytecode and returns it as a string.
    pub fn format_program(self: &Self) -> String {
        let mut position = 0;
        let mut result = "".to_string();
        while let Some((instruction, next_position)) = self.describe_instruction(position) {
            result.push_str(&instruction);
            result.push_str("\n");
            position = next_position;
        }
        result
    }

    /// Disassembles the current bytecode instruction and returns it as a string.
    pub fn format_instruction(self: &Self) -> Option<String> {
        self.describe_instruction(self.pc).map(|result| result.0)
    }

    /// Returns the current stack as a string.
    pub fn format_stack(self: &Self) -> String {
        format!("{:?}", self.stack)
    }

    /// Returns the current stack-frame as a string.
    pub fn format_frame(self: &Self) -> String {
        format!("{:?}", &self.stack.frame())
    }

    /// Pushes program const pool onto the stack, converting them from Little Endian to native endianness.
    fn init_consts(consts: Vec<u8>, const_descriptors: Vec<ConstDescriptor>) -> Stack {
        use ConstEndianness as CE;
        let mut stack = Stack::new();
        for descriptor in const_descriptors {
            let start = descriptor.position as usize;
            let end = start + descriptor.size as usize;
            match (descriptor.endianness, descriptor.size) {
                (_, 1)              => stack.push(u8::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Integer, 2)    => stack.push(u16::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Integer, 4)    => stack.push(u32::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Integer, 8)    => stack.push(u64::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Integer, 16)   => stack.push(u128::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Float, 4)      => stack.push(f32::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::Float, 8)      => stack.push(f64::from_le_bytes(consts[start..end].try_into().unwrap())),
                (CE::None, _)       => stack.extend_from(&consts[start..end]),
                _ => panic!("Unexpected ConstDescriptor {:?}", &descriptor),
            }
        }
        stack.begin();
        stack
    }

    /// Reads a constructor opcode.
    #[inline(always)]
    fn read_op(self: &Self, constructor_offset: &mut u32) -> Constructor {
        let op = Constructor::from_u8(self.stack.load(*constructor_offset));
        *constructor_offset += size_of::<u8>() as u32;
        op
    }

    /// Reads a constructor argument.
    #[inline(always)]
    fn read_arg(self: &Self, constructor_offset: &mut u32) -> u32 {
        let arg: u32 = self.stack.load(*constructor_offset);
        *constructor_offset += size_of::<u32>() as u32;
        arg
    }

    /// Writes prototype copy to given target (stack or heap).
    fn write_proto(self: &mut Self, target: CopyTarget, prototype_offset: u32, num_bytes: u32) {
        match target {
            CopyTarget::Heap(target_heap_ref) => {
                let src = self.stack.consts();
                self.heap.extend_from(target_heap_ref.index, &src[prototype_offset as usize .. prototype_offset as usize + num_bytes as usize]);
            },
            CopyTarget::Stack => self.stack.extend(prototype_offset, num_bytes),
        }
    }

    /// Writes a heap reference to the given target (stack or heap).
    fn write_ref(self: &mut Self, target: CopyTarget, heap_ref: HeapRef) {
        match target {
            CopyTarget::Heap(target_heap_ref) => {
                self.heap.extend_from(target_heap_ref.index, &heap_ref.index.to_ne_bytes());
                self.heap.extend_from(target_heap_ref.index, &heap_ref.offset.to_ne_bytes());
            }
            CopyTarget::Stack => self.stack.push(heap_ref),
        }
    }

    /// Constructs instance from given constructor and prototype. Modifies input for internal purposes.
    pub(crate) fn construct_value(self: &mut Self, constructor_offset: &mut u32, prototype_offset: &mut u32, target: CopyTarget) {
        match self.read_op(constructor_offset) {
            Constructor::Primitive => {
                let num_bytes = self.read_arg(constructor_offset);
                self.write_proto(target, *prototype_offset, num_bytes);
                *prototype_offset += num_bytes;
            }
            Constructor::Array => {
                let heap_ref = HeapRef { index: self.heap.alloc(Vec::new()), offset: 0 }; // TODO: use with_capacity() with correct final size. probably best to store final array size with constructor so we don't need to look ahead at runtime
                self.write_ref(target, heap_ref);
                let num_elements = self.read_arg(constructor_offset);
                let original_constructor_offset = *constructor_offset;
                for _ in 0..num_elements {
                    // reset offset each iteration to keep constructing the same type for each element but make sure we have advanced once at the end of the loop
                    *constructor_offset = original_constructor_offset;
                    self.construct_value(constructor_offset, prototype_offset, CopyTarget::Heap(heap_ref));
                }
            }
            Constructor::Struct => {
                let heap_ref = HeapRef { index: self.heap.alloc(Vec::new()), offset: 0 };
                self.write_ref(target, heap_ref);
                let num_fields = self.read_arg(constructor_offset);
                for _ in 0..num_fields {
                    self.construct_value(constructor_offset, prototype_offset, CopyTarget::Heap(heap_ref));
                }
            }
            Constructor::String => {
                let heap_ref = HeapRef { index: self.heap.alloc(Vec::new()), offset: 0 };
                self.write_ref(target, heap_ref);
                let num_bytes: u32 = self.stack.load(*prototype_offset); // fetch num bytes from prototype instead of constructor. strings have variable length
                *prototype_offset += 4;
                self.write_proto(CopyTarget::Heap(heap_ref), *prototype_offset, num_bytes);
            }
        };
    }

    /// Updates the refcounts for given heap reference and any nested heap references.
    pub(crate) fn refcount_value(self: &mut Self, item: HeapRef, mut constructor_offset: u32, op: HeapRefOp, free: bool) {
        let constructor = self.read_op(&mut constructor_offset);
        self.refcount_recurse(constructor, item, &mut constructor_offset, op, free);
    }

    /// Support method usd by refcount_value() to allow for reading the type before recursing into the type-constructor.
    fn refcount_recurse(self: &mut Self, constructor: Constructor, mut item: HeapRef, mut constructor_offset: &mut u32, op: HeapRefOp, free: bool) {
        match constructor {
            Constructor::Array => {
                let num_elements = self.read_arg(&mut constructor_offset);
                let element_constructor = self.read_op(&mut constructor_offset);
                if element_constructor != Constructor::Primitive {
                    let original_constructor_offset = *constructor_offset;
                    for _ in 0..num_elements {
                        // reset offset each iteration to keep constructing the same type for each element but make sure we have advanced once at the end of the loop
                        *constructor_offset = original_constructor_offset;
                        let element: HeapRef = self.heap.read_seq(&mut item);
                        self.refcount_recurse(element_constructor, element, &mut constructor_offset, op, free);
                    }
                } else {
                    // skip primitive num_bytes
                    self.read_arg(&mut constructor_offset);
                }
                self.heap.ref_item(item.index, op, free);
            }
            Constructor::Struct => {
                let num_fields = self.read_arg(&mut constructor_offset);
                for _ in 0..num_fields {
                    let field_constructor = self.read_op(&mut constructor_offset);
                    if field_constructor != Constructor::Primitive {
                        let field: HeapRef = self.heap.read_seq(&mut item);
                        self.refcount_recurse(field_constructor, field, &mut constructor_offset, op, free);
                    } else {
                        let num_bytes = self.read_arg(&mut constructor_offset);
                        item.offset += num_bytes;
                    }
                }
                self.heap.ref_item(item.index, op, free);
            }
            Constructor::String => {
                self.heap.ref_item(item.index, op, free);
            }
            Constructor::Primitive => {
                panic!("Unexpected primitive constructor");
            }
        };
    }
}