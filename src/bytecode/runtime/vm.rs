//! A virtual machine for running Itsy bytecode.

use std::convert::TryInto;
use std::mem::size_of;
use crate::{StackAddress, StackOffset, ItemCount};
use crate::bytecode::{HeapRef, Constructor, Program, ConstDescriptor, ConstEndianness, VMFunc, VMData, runtime::{stack::{Stack, StackOp}, heap::{Heap, HeapOp, HeapRefOp}}};

/// Current state of the vm, checked after each instruction.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VMState {
    /// The program is ready to run.
    Ready,
    /// The program has yielded. It can be resumed.
    Yielded,
    /// The program has terminated and must be reset before it can be run again.
    Terminated,
    /// The program encountered a runtime error and must be reset before it can be run again.
    RuntimeError,
}

#[derive(Copy, Clone, Debug)]
pub enum CopyTarget {
    Stack,
    Heap(HeapRef),
}

/// A virtual machine for running Itsy bytecode.
#[derive(Debug)]
pub struct VM<T, U> where T: VMFunc<T> {
    context_type            : std::marker::PhantomData<U>,
    func_type               : std::marker::PhantomData<T>,
    pub(crate) instructions : Vec<u8>,
    pub(crate) pc           : StackAddress,
    pub(crate) state        : VMState,
    pub stack               : Stack,
    pub heap                : Heap,
    pub cnt                 : Stack,
}

/// Public VM methods.
impl<T, U> VM<T, U> where T: VMFunc<T> + VMData<T, U> {
    /// Create a new VM instance with the given Program.
    pub fn new(program: &Program<T>) -> Self {
        let Program { instructions, consts, const_descriptors, .. } = program;
        let stack = Self::init_consts(consts, const_descriptors);
        VM {
            context_type: std::marker::PhantomData,
            func_type   : std::marker::PhantomData,
            instructions: instructions.clone(),
            pc          : 0,
            state       : VMState::Ready,
            stack       : stack,
            heap        : Heap::new(),
            cnt         : Stack::new(),
        }
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self, context: &mut U) -> VMState {
        if self.state != VMState::Ready && self.state != VMState::Yielded {
            panic!("Attempted to run in non-ready state");
        }
        self.exec(context);
        if self.state == VMState::Terminated && self.heap.len() > 0 {
            panic!("{} Heap elements remaining after program termination: {:?}", self.heap.len(), self.heap.data());
        }
        self.state
    }

    /// Executes single bytecode instruction.
    pub fn step(self: &mut Self, context: &mut U) -> VMState {
        if self.state != VMState::Ready && self.state != VMState::Yielded {
            panic!("Attempted to run in non-ready state");
        }
        self.exec_step(context);
        if self.state == VMState::Terminated && self.heap.len() > 0 {
            panic!("{} Heap elements remaining after program termination: {:?}", self.heap.len(), self.heap.data());
        }
        self.state
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.reset();
        self.heap.reset();
        self.cnt.reset();
        self.pc = 0;
        self.state = VMState::Ready;
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
    fn init_consts(consts: &Vec<u8>, const_descriptors: &Vec<ConstDescriptor>) -> Stack {
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
    fn read_op(self: &Self, constructor_offset: &mut StackAddress) -> Constructor {
        let op = Constructor::from_u8(self.stack.load(*constructor_offset));
        *constructor_offset += size_of::<u8>() as StackAddress;
        op
    }

    /// Reads a constructor argument.
    #[inline(always)]
    fn read_arg(self: &Self, constructor_offset: &mut StackAddress) -> StackAddress {
        let arg: ItemCount = self.stack.load(*constructor_offset);
        *constructor_offset += size_of::<ItemCount>() as StackAddress;
        arg as StackAddress
    }

    /// Writes prototype copy to given target (stack or heap).
    fn write_proto(self: &mut Self, target: CopyTarget, prototype_offset: StackAddress, num_bytes: StackAddress) {
        match target {
            CopyTarget::Heap(target_heap_ref) => {
                let src = self.stack.data();
                self.heap.extend_from(target_heap_ref.index(), &src[prototype_offset as usize .. prototype_offset as usize + num_bytes as usize]);
            },
            CopyTarget::Stack => self.stack.extend(prototype_offset, num_bytes),
        }
    }

    /// Writes a heap reference to the given target (stack or heap).
    fn write_ref(self: &mut Self, target: CopyTarget, heap_ref: HeapRef) {
        match target {
            CopyTarget::Heap(target_heap_ref) => {
                self.heap.extend_from(target_heap_ref.index(), &heap_ref.to_ne_bytes());
            }
            CopyTarget::Stack => self.stack.push(heap_ref),
        }
    }

    /// Constructs instance from given constructor and prototype. Modifies input for internal purposes.
    pub(crate) fn construct_value(self: &mut Self, constructor_offset: &mut StackAddress, prototype_offset: &mut StackAddress, target: CopyTarget, existing_strings: bool) {
        match self.read_op(constructor_offset) {
            Constructor::Primitive => {
                let num_bytes = self.read_arg(constructor_offset);
                self.write_proto(target, *prototype_offset, num_bytes);
                *prototype_offset += num_bytes;
            }
            Constructor::Array => {
                let heap_ref = HeapRef::new(self.heap.alloc(Vec::new()), 0); // TODO: use with_capacity() with correct final size. probably best to store final array size with constructor so we don't need to look ahead at runtime
                self.write_ref(target, heap_ref);
                let num_elements = self.read_arg(constructor_offset);
                let original_constructor_offset = *constructor_offset;
                for _ in 0..num_elements {
                    // reset offset each iteration to keep constructing the same type for each element but make sure we have advanced once at the end of the loop
                    *constructor_offset = original_constructor_offset;
                    self.construct_value(constructor_offset, prototype_offset, CopyTarget::Heap(heap_ref), existing_strings);
                }
            }
            Constructor::Struct => {
                let heap_ref = HeapRef::new(self.heap.alloc(Vec::new()), 0);
                self.write_ref(target, heap_ref);
                let num_fields = self.read_arg(constructor_offset);
                for _ in 0..num_fields {
                    self.construct_value(constructor_offset, prototype_offset, CopyTarget::Heap(heap_ref), existing_strings);
                }
            }
            Constructor::String => {
                if existing_strings {
                    let num_bytes = HeapRef::primitive_size() as StackAddress;
                    self.write_proto(target, *prototype_offset, num_bytes);
                    *prototype_offset += num_bytes;
                } else {
                    let heap_ref = HeapRef::new(self.heap.alloc(Vec::new()), 0);
                    self.write_ref(target, heap_ref);
                    let num_bytes: StackAddress = self.stack.load(*prototype_offset); // fetch num bytes from prototype instead of constructor. strings have variable length
                    *prototype_offset += ::std::mem::size_of_val(&num_bytes) as StackAddress;
                    self.write_proto(CopyTarget::Heap(heap_ref), *prototype_offset, num_bytes);
                    *prototype_offset += num_bytes;
                }
            }
        };
    }

    /// Updates the refcounts for given heap reference and any nested heap references.
    pub(crate) fn refcount_value(self: &mut Self, item: HeapRef, mut constructor_offset: StackAddress, op: HeapRefOp) {
        let constructor = self.read_op(&mut constructor_offset);
        let epoch = self.heap.new_epoch();
        self.refcount_recurse(constructor, item, &mut constructor_offset, op, epoch);
    }

    /// Support method usd by refcount_value() to allow for reading the type before recursing into the type-constructor.
    fn refcount_recurse(self: &mut Self, constructor: Constructor, mut item: HeapRef, mut constructor_offset: &mut StackAddress, op: HeapRefOp, epoch: u8) {
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
                        if epoch != self.heap.item_epoch(element.index()) {
                            self.refcount_recurse(element_constructor, element, &mut constructor_offset, op, epoch);
                        }
                    }
                } else {
                    // skip primitive num_bytes
                    self.read_arg(&mut constructor_offset);
                }
                self.heap.ref_item(item.index(), op);
            }
            Constructor::Struct => {
                let num_fields = self.read_arg(&mut constructor_offset);
                for _ in 0..num_fields {
                    let field_constructor = self.read_op(&mut constructor_offset);
                    if field_constructor != Constructor::Primitive {
                        let field: HeapRef = self.heap.read_seq(&mut item);
                        if epoch != self.heap.item_epoch(field.index()) {
                            self.refcount_recurse(field_constructor, field, &mut constructor_offset, op, epoch);
                        }
                    } else {
                        let num_bytes = self.read_arg(&mut constructor_offset);
                        item.add_offset(num_bytes as StackOffset);
                    }
                }
                self.heap.ref_item(item.index(), op);
            }
            Constructor::String => {
                self.heap.ref_item(item.index(), op);
            }
            Constructor::Primitive => {
                panic!("Unexpected primitive constructor");
            }
        };
    }
}