//! A virtual machine for running Itsy bytecode.

use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex, VariantIndex};
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
pub struct VM<T, U> {
    context_type            : PhantomData<U>,
    func_type               : PhantomData<T>,
    pub(crate) instructions : Vec<u8>,
    pub(crate) pc           : StackAddress,
    pub(crate) state        : VMState,
    pub stack               : Stack,
    pub heap                : Heap,
}

/// Public VM methods.
impl<T, U> VM<T, U> {
    /// Create a new VM instance with the given Program.
    pub fn new(program: &Program<T>) -> Self where T: VMFunc<T> + VMData<T, U> {
        let Program { instructions, consts, const_descriptors, .. } = program;
        let stack = Self::init_consts(consts, const_descriptors);
        VM {
            context_type: PhantomData,
            func_type   : PhantomData,
            instructions: instructions.clone(),
            pc          : 0,
            state       : VMState::Ready,
            stack       : stack,
            heap        : Heap::new(),
        }
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self, context: &mut U) -> VMState where T: VMFunc<T> + VMData<T, U> {
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
    #[cfg(feature="debugging")]
    pub fn step(self: &mut Self, context: &mut U) -> VMState where T: VMFunc<T> + VMData<T, U> {
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
        self.pc = 0;
        self.state = VMState::Ready;
    }

    /// Disassembles the bytecode and returns it as a string.
    #[cfg(feature="debugging")]
    pub fn format_program(self: &Self) -> String where T: VMFunc<T> + VMData<T, U> {
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
    #[cfg(feature="debugging")]
    pub fn format_instruction(self: &Self) -> Option<String> where T: VMFunc<T> + VMData<T, U> {
        self.describe_instruction(self.pc).map(|result| result.0)
    }

    /// Returns the current stack as a string.
    #[cfg(feature="debugging")]
    pub fn format_stack(self: &Self) -> String {
        format!("{:?}", self.stack)
    }

    /// Returns the current stack-frame as a string.
    #[cfg(feature="debugging")]
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
    fn construct_read_op(self: &Self, constructor_offset: &mut StackAddress) -> Constructor {
        let op = Constructor::from_u8(self.stack.load(*constructor_offset));
        *constructor_offset += size_of_val(&op) as StackAddress;
        op
    }

    /// Reads an ItemIndex sized constructor argument.
    #[inline(always)]
    fn construct_read_index(self: &Self, constructor_offset: &mut StackAddress) -> ItemIndex {
        let arg: ItemIndex = self.stack.load(*constructor_offset);
        *constructor_offset += size_of_val(&arg) as StackAddress;
        arg
    }

    /// Reads a StackAddress sized constructor argument.
    #[inline(always)]
    fn construct_read_address(self: &Self, constructor_offset: &mut StackAddress) -> StackAddress {
        let arg: StackAddress = self.stack.load(*constructor_offset);
        *constructor_offset += size_of_val(&arg) as StackAddress;
        arg
    }

    /// Writes prototype copy to given target (stack or heap).
    fn construct_copy_value(self: &mut Self, target: CopyTarget, prototype_offset: StackAddress, num_bytes: StackAddress) {
        match target {
            CopyTarget::Heap(target_heap_ref) => {
                let src = self.stack.data();
                self.heap.item_mut(target_heap_ref.index()).data.extend_from_slice(&src[prototype_offset as usize .. prototype_offset as usize + num_bytes as usize]);
            },
            CopyTarget::Stack => self.stack.extend(prototype_offset, num_bytes),
        }
    }

    /// Writes a heap reference to the given target (stack or heap).
    fn construct_write_ref(self: &mut Self, target: CopyTarget, heap_ref: HeapRef) {
        match target {
            CopyTarget::Heap(target_heap_ref) => {
                self.heap.item_mut(target_heap_ref.index()).data.extend_from_slice(&heap_ref.to_ne_bytes());
            },
            CopyTarget::Stack => self.stack.push(heap_ref),
        }
    }

    /// Constructs instance from given constructor and prototype. Modifies input for internal purposes.
    pub(crate) fn construct_value(self: &mut Self, constructor_offset: &mut StackAddress, prototype_offset: &mut StackAddress, target: CopyTarget, existing_strings: bool) {
        match self.construct_read_op(constructor_offset) {
            Constructor::Primitive => {
                let num_bytes = self.construct_read_index(constructor_offset) as StackAddress;
                self.construct_copy_value(target, *prototype_offset, num_bytes);
                *prototype_offset += num_bytes;
            },
            Constructor::ArrayDyn => {
                let heap_ref = HeapRef::new(self.heap.alloc(Vec::new(), ItemIndex::MAX), 0); // TODO: use with_capacity() with correct final size. probably best to store final array size with constructor so we don't need to look ahead at runtime
                self.construct_write_ref(target, heap_ref);
                // read size from prototype instead of constructor
                let num_elements: ItemIndex = self.stack.load(*prototype_offset);
                *prototype_offset += size_of_val(&num_elements) as StackAddress;
                let original_constructor_offset = *constructor_offset;
                for _ in 0..num_elements {
                    // reset offset each iteration to keep constructing the same type for each element but make sure we have advanced once at the end of the loop
                    *constructor_offset = original_constructor_offset;
                    self.construct_value(constructor_offset, prototype_offset, CopyTarget::Heap(heap_ref), existing_strings);
                }
            },
            Constructor::Struct => {
                let implementor_index = self.construct_read_index(constructor_offset);
                let num_fields = self.construct_read_index(constructor_offset);
                let heap_ref = HeapRef::new(self.heap.alloc(Vec::new(), implementor_index), 0);
                self.construct_write_ref(target, heap_ref);
                for _ in 0..num_fields {
                    self.construct_value(constructor_offset, prototype_offset, CopyTarget::Heap(heap_ref), existing_strings);
                }
            },
            Constructor::Enum => {
                let implementor_index = self.construct_read_index(constructor_offset);
                let num_variants = self.construct_read_index(constructor_offset);
                // read variant index from prototype
                let variant_index: ItemIndex = self.stack.load(*prototype_offset);
                *prototype_offset += size_of_val(&variant_index) as StackAddress;
                assert!(variant_index < num_variants, "Prototype specifies invalid enum variant");
                // seek to variant offset, read it from constructor and seek to the offset
                *constructor_offset += (size_of::<StackAddress>() * variant_index as usize) as StackAddress;
                *constructor_offset = self.construct_read_address(constructor_offset);
                // read and construct fields for the variant
                let num_fields = self.construct_read_index(constructor_offset);
                let heap_ref = HeapRef::new(self.heap.alloc(Vec::new(), implementor_index), 0);
                self.construct_write_ref(target, heap_ref);
                for _ in 0..num_fields {
                    self.construct_value(constructor_offset, prototype_offset, CopyTarget::Heap(heap_ref), existing_strings);
                }
            },
            Constructor::String => {
                if existing_strings {
                    let num_bytes = HeapRef::primitive_size() as StackAddress;
                    self.construct_copy_value(target, *prototype_offset, num_bytes);
                    *prototype_offset += num_bytes;
                } else {
                    let heap_ref = HeapRef::new(self.heap.alloc(Vec::new(), ItemIndex::MAX), 0);
                    self.construct_write_ref(target, heap_ref);
                    let num_bytes: StackAddress = self.stack.load(*prototype_offset); // fetch num bytes from prototype instead of constructor. strings have variable length
                    *prototype_offset += size_of_val(&num_bytes) as StackAddress;
                    self.construct_copy_value(CopyTarget::Heap(heap_ref), *prototype_offset, num_bytes);
                    *prototype_offset += num_bytes;
                }
            },
        };
    }

    /// Updates the refcounts for given heap reference and any nested heap references.
    pub(crate) fn refcount_value(self: &mut Self, item: HeapRef, mut constructor_offset: StackAddress, op: HeapRefOp) {
        let constructor = self.construct_read_op(&mut constructor_offset);
        let epoch = self.heap.new_epoch();
        self.refcount_recurse(constructor, item, &mut constructor_offset, op, epoch);
    }

    /// Support method usd by refcount_value() to allow for reading the type before recursing into the type-constructor.
    fn refcount_recurse(self: &mut Self, constructor: Constructor, mut item: HeapRef, mut constructor_offset: &mut StackAddress, op: HeapRefOp, epoch: usize) {
        match constructor {
            Constructor::ArrayDyn => {
                let element_constructor = self.construct_read_op(&mut constructor_offset);
                if element_constructor != Constructor::Primitive {
                    let original_constructor_offset = *constructor_offset;
                    // compute number of elements from heap size
                    let num_elements = self.heap.item(item.index()).data.len() / HeapRef::primitive_size() as usize;
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
                    self.construct_read_index(&mut constructor_offset);
                }
                self.heap.ref_item(item.index(), op);
            },
            Constructor::Struct => {
                let _implementor_index = self.construct_read_index(constructor_offset);
                let num_fields = self.construct_read_index(&mut constructor_offset);
                for _ in 0..num_fields {
                    let field_constructor = self.construct_read_op(&mut constructor_offset);
                    if field_constructor != Constructor::Primitive {
                        let field: HeapRef = self.heap.read_seq(&mut item);
                        if epoch != self.heap.item_epoch(field.index()) {
                            self.refcount_recurse(field_constructor, field, &mut constructor_offset, op, epoch);
                        }
                    } else {
                        let num_bytes = self.construct_read_index(&mut constructor_offset) as StackOffset;
                        item.add_offset(num_bytes);
                    }
                }
                self.heap.ref_item(item.index(), op);
            },
            Constructor::Enum => {
                let variant_index: VariantIndex = self.heap.read_seq(&mut item);
                let _implementor_index = self.construct_read_index(constructor_offset);
                let num_variants = self.construct_read_index(constructor_offset);
                assert!(variant_index < num_variants, "Enum object specifies invalid enum variant");
                // seek to variant offset, read it from constructor and seek to the offset
                *constructor_offset += (size_of::<StackAddress>() * variant_index as usize) as StackAddress;
                *constructor_offset = self.construct_read_address(constructor_offset);
                // handle fields
                let num_fields = self.construct_read_index(&mut constructor_offset);
                for _ in 0..num_fields {
                    let field_constructor = self.construct_read_op(&mut constructor_offset);
                    if field_constructor != Constructor::Primitive {
                        let field: HeapRef = self.heap.read_seq(&mut item);
                        if epoch != self.heap.item_epoch(field.index()) {
                            self.refcount_recurse(field_constructor, field, &mut constructor_offset, op, epoch);
                        }
                    } else {
                        let num_bytes = self.construct_read_index(&mut constructor_offset) as StackOffset;
                        item.add_offset(num_bytes);
                    }
                }
                self.heap.ref_item(item.index(), op);
            }
            Constructor::String => {
                self.heap.ref_item(item.index(), op);
            },
            Constructor::Primitive => {
                panic!("Unexpected primitive constructor");
            },
        };
    }
}