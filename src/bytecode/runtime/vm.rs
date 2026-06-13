//! A virtual machine for running Itsy bytecode.

use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex, VariantIndex};
use crate::bytecode::{HeapRef, HeapRefOp, Constructor, Program, ConstDescriptor, ConstEndianness, VMFunc, VMData, runtime::{stack::{Stack, StackOp}, heap::{Heap, HeapOp, HeapCmp}, error::*}};
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
    Error(RuntimeErrorKind),
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
    pub(crate) error_pc     : StackAddress,
    pub(crate) state        : VMState,
    pub stack               : Stack,
    pub heap                : Heap,
}

// todo check where bounds here, some seam pointless

/// Public VM methods.
impl<T, U> VM<T, U> {
    /// Create a new VM instance with the given Program.
    pub fn new(program: Program<T>) -> Self where T: VMFunc<T> + VMData<T, U> {
        let Program { instructions, consts, const_descriptors, .. } = program;
        let stack = Self::init_consts(&consts, &const_descriptors);
        // occupy heap element 0 so we can identify intialized heap objects their address != 0
        let mut heap = Heap::new();
        heap.alloc(0, 0);
        VM {
            context_type: PhantomData,
            func_type   : PhantomData,
            instructions: instructions,
            pc          : 0,
            error_pc    : 0,
            state       : VMState::Ready,
            stack       : stack,
            heap        : heap,
        }
    }

    /// Executes the current program until it yields or terminates.
    pub fn run(self: &mut Self, context: &mut U) -> RuntimeResult<VMState> where T: VMFunc<T> + VMData<T, U> {
        if self.state != VMState::Ready && self.state != VMState::Yielded {
            return Err(RuntimeError::new(0, RuntimeErrorKind::NotReady, None));
        }
        self.exec(context);
        match self.state {
            VMState::Terminated if self.heap.len() > 1 => Err(RuntimeError::new(0, RuntimeErrorKind::HeapCorruption, None)),
            VMState::Error(kind) => Err(RuntimeError::new(self.error_pc, kind, None)),
            VMState::Ready => {
                let kind = RuntimeErrorKind::UnexpectedReady;
                self.state = VMState::Error(kind);
                Err(RuntimeError::new(self.pc, kind, None))
            },
            VMState::Terminated | VMState::Yielded => Ok(self.state),
        }
    }

    /// Clears runtime error, allowing the VM to resume via run(). This is a no-op if the VM is
    /// in Ready or Yielded state and an error in Terminated state.
    pub fn clear_error(self: &mut Self) -> RuntimeResult where T: VMFunc<T> + VMData<T, U> {
        match self.state {
            VMState::Error(_) => {
                self.error_pc = 0;
                self.state = VMState::Ready;
                Ok(())
            },
            VMState::Ready | VMState::Yielded => Ok(()),
            _ => Err(RuntimeError::new(0, RuntimeErrorKind::CannotClear, None)),
        }
    }

    /// Returns the current VM state
    pub fn state(self: &Self) -> VMState {
        self.state
    }

    /// Resets the VM, keeping only code and constants.
    pub fn reset(self: &mut Self) {
        self.stack.reset();
        self.heap.reset();
        self.heap.alloc(0, 0);
        self.pc = 0;
        self.error_pc = 0;
        self.state = VMState::Ready;
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
                _ => panic!("Unexpected ConstDescriptor {:?}.", &descriptor),
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

    /// Resolves a value's constructor offset, following the indirection used for trait implementors (offset 0,
    /// looked up via the object's implementor index) and dynamic constructors (offset 1, stored at the end of the
    /// object). Returns `None` if the object has no constructor, i.e. contains no nested heap references.
    fn resolve_constructor_offset(self: &Self, item: HeapRef, mut constructor_offset: StackAddress) -> Option<StackAddress> {
        if constructor_offset == 0 {
            // trait implementor specific constructor
            let implementor_index = self.heap.item_implementor_index(item.index()) as usize;
            constructor_offset = self.stack.load((implementor_index * size_of::<StackAddress>()) as StackAddress);
        } else if constructor_offset == 1 {
            // dynamic constructor, index stored at the end of the heap-object
            let data = &self.heap.item(item.index()).data;
            let pos = data.len() - size_of::<StackAddress>();
            constructor_offset = StackAddress::from_ne_bytes(data[pos..].try_into().unwrap());
            if constructor_offset == 0 {
                return None;
            }
        }
        Some(constructor_offset)
    }

    /// Updates the refcounts for given heap reference and any nested heap references. Looks up virtual constructor if offset is 0.
    pub(crate) fn refcount_value(self: &mut Self, item: HeapRef, constructor_offset: StackAddress, op: HeapRefOp) {
        match self.resolve_constructor_offset(item, constructor_offset) {
            None => self.heap.ref_item(item.index(), op),
            Some(mut constructor_offset) => {
                let constructor = self.construct_read_op(&mut constructor_offset);
                let epoch = self.heap.new_epoch();
                self.refcount_recurse(constructor, HeapRef::new(item.index(), 0), &mut constructor_offset, op, epoch);
            }
        }
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
                    self.refcount_fields(num_fields, &mut field_offset, &mut item, op, epoch);
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Enum => {
                    let variant_index: VariantIndex = self.heap.read_seq(&mut item);
                    let (_implementor_index, num_variants, variant_table_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                    assert!(variant_index < num_variants, "Enum object specifies invalid enum variant");
                    // seek to variant offset, read it from constructor and seek to the offset
                    let (num_fields, mut field_offset) = Constructor::parse_variant_table(&self.stack, variant_table_offset, variant_index);
                    self.refcount_fields(num_fields, &mut field_offset, &mut item, op, epoch);
                    self.heap.ref_item(item_index, op);
                },
                Constructor::String => {
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Closure => {
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Primitive => {
                    panic!("Unexpected primitive constructor.");
                },
            };
            *constructor_offset = parsed.next;
        }
    }

    /// Updates the refcounts for a sequence of struct fields or enum-variant fields described by the constructor at field_offset.
    fn refcount_fields(self: &mut Self, num_fields: ItemIndex, field_offset: &mut StackAddress, item: &mut HeapRef, op: HeapRefOp, epoch: usize) {
        for _ in 0..num_fields {
            let field_constructor = self.construct_read_op(field_offset);
            if field_constructor != Constructor::Primitive {
                let field: HeapRef = self.heap.read_seq(item);
                let field_index = field.index();
                if epoch != self.heap.item_epoch(field_index) {
                    self.refcount_recurse(field_constructor, field, field_offset, op, epoch);
                    // fixme: skip when epoch matches, constructor_offset will be wrong otherwise
                }
            } else {
                let num_bytes = self.construct_read_index(field_offset) as StackOffset;
                item.add_offset(num_bytes);
            }
        }
    }

    /// Compares the targets of two heap references of identical reference type for deep equality, guided by the
    /// type's serialized constructor. Used to implement equality for non-primitive enums.
    pub(crate) fn compare_value(self: &Self, a: HeapRef, b: HeapRef, constructor_offset: StackAddress) -> bool {
        // both operands share the static type, so the constructor is resolved via a
        match self.resolve_constructor_offset(a, constructor_offset) {
            // no nested constructor: equal iff the same object is referenced
            None => a.index() == b.index(),
            Some(mut constructor_offset) => {
                let constructor = self.construct_read_op(&mut constructor_offset);
                self.compare_recurse(constructor, HeapRef::new(a.index(), 0), HeapRef::new(b.index(), 0), &mut constructor_offset)
            }
        }
    }

    /// Recursively compares two heap reference targets for equality, guided by the serialized type constructor.
    fn compare_recurse(self: &Self, constructor: Constructor, mut a: HeapRef, mut b: HeapRef, constructor_offset: &mut StackAddress) -> bool {
        let parsed = Constructor::parse_with(&self.stack, *constructor_offset, constructor);
        *constructor_offset = parsed.offset;
        let result = match constructor {
            Constructor::Array => {
                let element_constructor = self.construct_read_op(constructor_offset);
                if element_constructor != Constructor::Primitive {
                    let a_len = self.heap.item(a.index()).data.len();
                    let b_len = self.heap.item(b.index()).data.len();
                    if a_len != b_len {
                        false
                    } else {
                        let original_constructor_offset = *constructor_offset;
                        let num_elements = a_len / HeapRef::primitive_size() as usize;
                        let mut equal = true;
                        for _ in 0..num_elements {
                            // reset offset each iteration to re-read the same element type, advancing once at the end
                            *constructor_offset = original_constructor_offset;
                            let a_element: HeapRef = self.heap.read_seq(&mut a);
                            let b_element: HeapRef = self.heap.read_seq(&mut b);
                            if !self.compare_recurse(element_constructor, a_element, b_element, constructor_offset) {
                                equal = false;
                                break;
                            }
                        }
                        equal
                    }
                } else {
                    // primitive elements: comparing the raw object data covers both length and contents
                    self.construct_read_index(constructor_offset); // skip primitive num_bytes
                    self.heap.item(a.index()).data == self.heap.item(b.index()).data
                }
            },
            Constructor::Struct => {
                let (_implementor_index, num_fields, mut field_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                self.compare_fields(num_fields, &mut field_offset, &mut a, &mut b)
            },
            Constructor::Enum => {
                let a_variant: VariantIndex = self.heap.read_seq(&mut a);
                let b_variant: VariantIndex = self.heap.read_seq(&mut b);
                if a_variant != b_variant {
                    false
                } else {
                    let (_implementor_index, _num_variants, variant_table_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                    let (num_fields, mut field_offset) = Constructor::parse_variant_table(&self.stack, variant_table_offset, a_variant);
                    self.compare_fields(num_fields, &mut field_offset, &mut a, &mut b)
                }
            },
            Constructor::String => {
                self.heap.compare_string(a, b, HeapCmp::Eq)
            },
            Constructor::Closure => {
                // closures are opaque: equal only if the same heap object is referenced
                a.index() == b.index()
            },
            Constructor::Primitive => {
                panic!("Unexpected primitive constructor.");
            },
        };
        *constructor_offset = parsed.next;
        result
    }

    /// Compares a sequence of struct fields or enum-variant fields, described by the constructor at field_offset.
    fn compare_fields(self: &Self, num_fields: ItemIndex, field_offset: &mut StackAddress, a: &mut HeapRef, b: &mut HeapRef) -> bool {
        for _ in 0..num_fields {
            let field_constructor = self.construct_read_op(field_offset);
            if field_constructor != Constructor::Primitive {
                let a_field: HeapRef = self.heap.read_seq(a);
                let b_field: HeapRef = self.heap.read_seq(b);
                if !self.compare_recurse(field_constructor, a_field, b_field, field_offset) {
                    return false;
                }
            } else {
                let num_bytes = self.construct_read_index(field_offset) as StackOffset;
                if !self.heap.compare_bytes(*a, *b, num_bytes as usize) {
                    return false;
                }
                a.add_offset(num_bytes);
                b.add_offset(num_bytes);
            }
        }
        true
    }
}

#[cfg(feature="debugging")]
impl<T, U> VM<T, U> {
    /// Executes single bytecode instruction.
    pub fn step(self: &mut Self, context: &mut U) -> RuntimeResult<VMState> where T: VMFunc<T> + VMData<T, U> {
        if self.state != VMState::Ready && self.state != VMState::Yielded {
            return Err(RuntimeError::new(0, RuntimeErrorKind::NotReady, None));
        }
        self.exec_step(context);
        match self.state {
            VMState::Terminated if self.heap.len() > 1 => Err(RuntimeError::new(0, RuntimeErrorKind::HeapCorruption, None)),
            VMState::Error(kind) => {
                #[cfg(feature="symbols")]
                let opcode = self.describe_instruction(self.error_pc).map(|result| result.0);
                #[cfg(not(feature="symbols"))]
                let opcode = None;
                Err(RuntimeError::new(self.pc, kind, opcode))
            },
            VMState::Terminated | VMState::Yielded | VMState::Ready => Ok(self.state),
        }
    }

    /// Disassembles the bytecode and returns it as a string.
    #[cfg(feature="symbols")]
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
    #[cfg(feature="symbols")]
    pub fn format_instruction(self: &Self) -> Option<String> where T: VMFunc<T> + VMData<T, U> {
        self.describe_instruction(self.pc).map(|result| result.0)
    }

    /// Returns the opcode for the current instruction.
    pub fn get_opcode(self: &Self) -> Option<OpCode> where T: VMFunc<T> + VMData<T, U> {
        self.read_opcode(self.pc)
    }

    /// Returns the current stack as a string.
    pub fn format_stack(self: &Self) -> String {
        format!("{:?}", self.stack)
    }

    /// Returns the current stack-frame as a string.
    pub fn format_frame(self: &Self) -> String {
        format!("{:?}", &self.stack.frame())
    }
}