//! A virtual machine for running Itsy bytecode.

use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex, VariantIndex};
use crate::bytecode::{HeapRef, HeapRefOp, Constructor, Program, ConstDescriptor, ConstEndianness, VMFunc, VMData, runtime::{stack::{Stack, StackOp}, heap::{Heap, HeapOp}}};
#[cfg(feature="debugging")]
use crate::bytecode::opcodes::OpCode;

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
///
/// See [itsy_api](crate::itsy_api) for an example that runs an Itsy program using [VM::run].
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
        // occupy heap element 0 so we can identify intialized heap objects their address != 0
        let mut heap = Heap::new();
        heap.alloc(0, 0);
        VM {
            context_type: PhantomData,
            func_type   : PhantomData,
            instructions: instructions.clone(),
            pc          : 0,
            state       : VMState::Ready,
            stack       : stack,
            heap        : heap,
        }
    }

    /// Executes bytecode until it terminates.
    pub fn run(self: &mut Self, context: &mut U) -> VMState where T: VMFunc<T> + VMData<T, U> {
        if self.state != VMState::Ready && self.state != VMState::Yielded {
            panic!("Attempted to run in non-ready state");
        }
        self.exec(context);
        if self.state == VMState::Terminated && self.heap.len() > 1 {
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
        if self.state == VMState::Terminated && self.heap.len() > 1 {
            panic!("{} Heap elements remaining after program termination: {:?}", self.heap.len(), self.heap.data());
        }
        self.state
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.reset();
        self.heap.reset();
        self.heap.alloc(0, 0);
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

    /// Returns the current instruction
    #[cfg(feature="debugging")]
    pub fn get_instruction(self: &Self) -> Option<OpCode> where T: VMFunc<T> + VMData<T, U> {
        self.read_instruction(self.pc)
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
                (_, 1)              => stack.push(consts[start]),
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
    fn construct_read_op(self: &Self, constructor_offset: &mut StackAddress) -> Constructor {
        let op = Constructor::from_u8(self.stack.load(*constructor_offset));
        *constructor_offset += size_of_val(&op) as StackAddress;
        op
    }

    /// Reads an ItemIndex sized constructor argument.
    fn construct_read_index(self: &Self, constructor_offset: &mut StackAddress) -> ItemIndex {
        let arg: ItemIndex = self.stack.load(*constructor_offset);
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
            CopyTarget::Stack => unreachable!(), //self.stack.extend(prototype_offset, num_bytes),
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
    pub(crate) fn construct_value(self: &mut Self, constructor_offset: StackAddress, prototype_offset: &mut StackAddress, target: CopyTarget, existing_strings: bool) -> StackAddress {
        let parsed = Constructor::parse(&self.stack, constructor_offset);
        match parsed.op {
            Constructor::Primitive => {
                let primitive_size = Constructor::parse_primitive(&self.stack, parsed.offset) as StackAddress;
                self.construct_copy_value(target, *prototype_offset, primitive_size);
                *prototype_offset += primitive_size;
            },
            Constructor::Array => {
                let heap_ref = HeapRef::new(self.heap.alloc(0, ItemIndex::MAX), 0); // TODO: use with_capacity() with correct final size. probably best to store final array size with constructor so we don't need to look ahead at runtime
                self.construct_write_ref(target, heap_ref);
                // read size from prototype instead of constructor
                let num_elements: ItemIndex = self.stack.load(*prototype_offset);
                *prototype_offset += size_of_val(&num_elements) as StackAddress;
                for _ in 0..num_elements {
                    // reuse offset for each array element
                    self.construct_value(parsed.offset, prototype_offset, CopyTarget::Heap(heap_ref), existing_strings);
                }
            },
            Constructor::Struct => {
                let (implementor_index, num_fields, mut field_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                let heap_ref = HeapRef::new(self.heap.alloc(0, implementor_index), 0);
                self.construct_write_ref(target, heap_ref);
                for _ in 0..num_fields {
                    field_offset = self.construct_value(field_offset, prototype_offset, CopyTarget::Heap(heap_ref), existing_strings);
                }
            },
            Constructor::Enum => {
                // TODO: this is never used, enum literals are compiled to use upload instruction
                let (implementor_index, num_variants, variant_table_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                // read variant index from prototype
                let variant_index: ItemIndex = self.stack.load(*prototype_offset);
                *prototype_offset += size_of_val(&variant_index) as StackAddress;
                assert!(variant_index < num_variants, "Prototype specifies invalid enum variant");
                // seek to variant offset, read it from constructor and seek to the offset
                let (num_fields, mut variant_field_offset) = Constructor::parse_variant_table(&self.stack, variant_table_offset, variant_index);
                // read and construct fields for the variant
                let heap_ref = HeapRef::new(self.heap.alloc(0, implementor_index), 0);
                self.construct_write_ref(target, heap_ref);
                self.heap.item_mut(heap_ref.index()).data.extend_from_slice(&variant_index.to_ne_bytes());
                for _ in 0..num_fields {
                    variant_field_offset = self.construct_value(variant_field_offset, prototype_offset, CopyTarget::Heap(heap_ref), existing_strings);
                }
            },
            Constructor::String => {
                if existing_strings {
                    let num_bytes = HeapRef::primitive_size() as StackAddress;
                    self.construct_copy_value(target, *prototype_offset, num_bytes);
                    *prototype_offset += num_bytes;
                } else {
                    let heap_ref = HeapRef::new(self.heap.alloc(0, ItemIndex::MAX), 0);
                    self.construct_write_ref(target, heap_ref);
                    let num_bytes: StackAddress = self.stack.load(*prototype_offset); // fetch num bytes from prototype instead of constructor. strings have variable length
                    *prototype_offset += size_of_val(&num_bytes) as StackAddress;
                    self.construct_copy_value(CopyTarget::Heap(heap_ref), *prototype_offset, num_bytes);
                    *prototype_offset += num_bytes;
                }
            },
        };
        parsed.next
    }

    /// Updates the refcounts for given heap reference and any nested heap references. Looks up virtual constructor if offset is 0.
    pub(crate) fn refcount_value(self: &mut Self, item: HeapRef, mut constructor_offset: StackAddress, op: HeapRefOp) {
        if constructor_offset == 0 {
            let implementor_index = self.heap.item_implementor_index(item.index()) as usize;
            constructor_offset = self.stack.load((implementor_index * size_of::<StackAddress>()) as StackAddress);
        }
        let constructor = self.construct_read_op(&mut constructor_offset);
        let epoch = self.heap.new_epoch();
        self.refcount_recurse(constructor, item, &mut constructor_offset, op, epoch);
    }

    /// Support method usd by refcount_value() to allow for reading the type before recursing into the type-constructor.
    fn refcount_recurse(self: &mut Self, constructor: Constructor, mut item: HeapRef, constructor_offset: &mut StackAddress, op: HeapRefOp, epoch: usize) {
        let item_index = item.index();
        let refs = self.heap.item_refs(item_index);
        let recurse = (refs == 1 && (op == HeapRefOp::Dec || op == HeapRefOp::DecNoFree)) || (refs == 0 && (op == HeapRefOp::Inc || op == HeapRefOp::Free));
        let parsed = Constructor::parse_with(&self.stack, *constructor_offset, constructor);
        if !recurse {
            // No recursion is required if only the refcount changes, but we need to skip the constructor
            self.heap.ref_item(item_index, op);
            *constructor_offset = parsed.next;
        } else {
            // Value will either be be..
            //         dropped: recursively decrease reference count for referenced values
            // or materialized: recursively increase reference count for referenced values
            *constructor_offset = parsed.offset;
            match constructor {
                Constructor::Array => {
                    let element_constructor = self.construct_read_op(constructor_offset);
                    if element_constructor != Constructor::Primitive {
                        let original_constructor_offset = *constructor_offset;
                        // compute number of elements from heap size
                        let num_elements = self.heap.item(item_index).data.len() / HeapRef::primitive_size() as usize;
                        for _ in 0..num_elements {
                            // reset offset each iteration to keep referencing the same type for each element but make sure we have advanced once at the end of the loop
                            *constructor_offset = original_constructor_offset;
                            let element: HeapRef = self.heap.read_seq(&mut item);
                            let element_index = element.index();
                            if epoch != self.heap.item_epoch(element_index) {
                                self.refcount_recurse(element_constructor, element, constructor_offset, op, epoch);
                            }
                        }
                    } else {
                        // skip primitive num_bytes
                        self.construct_read_index(constructor_offset);
                    }
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Struct => {
                    let (_implementor_index, num_fields, mut field_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                    // handle fields // TODO dedup with enum code
                    for _ in 0..num_fields {
                        let field_constructor = self.construct_read_op(&mut field_offset);
                        if field_constructor != Constructor::Primitive {
                            let field: HeapRef = self.heap.read_seq(&mut item);
                            let field_index = field.index();
                            if epoch != self.heap.item_epoch(field_index) {
                                self.refcount_recurse(field_constructor, field, &mut field_offset, op, epoch);
                                // fixme: skip when epoch matches, constructor_offset will be wrong otherwise
                            }
                        } else {
                            let num_bytes = self.construct_read_index(&mut field_offset) as StackOffset;
                            item.add_offset(num_bytes);
                        }
                    }
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Enum => {
                    let variant_index: VariantIndex = self.heap.read_seq(&mut item);
                    let (_implementor_index, num_variants, variant_table_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                    assert!(variant_index < num_variants, "Enum object specifies invalid enum variant");
                    // seek to variant offset, read it from constructor and seek to the offset
                    let (num_fields, mut field_offset) = Constructor::parse_variant_table(&self.stack, variant_table_offset, variant_index);
                    // handle fields // TODO dedup with struct code
                    for _ in 0..num_fields {
                        let field_constructor = self.construct_read_op(&mut field_offset);
                        if field_constructor != Constructor::Primitive {
                            let field: HeapRef = self.heap.read_seq(&mut item);
                            let field_index = field.index();
                            if epoch != self.heap.item_epoch(field_index) {
                                self.refcount_recurse(field_constructor, field, &mut field_offset, op, epoch);
                                // fixme: skip when epoch matches, constructor_offset will be wrong otherwise
                            }
                        } else {
                            let num_bytes = self.construct_read_index(&mut field_offset) as StackOffset;
                            item.add_offset(num_bytes);
                        }
                    }
                    self.heap.ref_item(item_index, op);
                },
                Constructor::String => {
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Primitive => {
                    panic!("Unexpected primitive constructor");
                },
            };
            *constructor_offset = parsed.next;
        }
    }
}