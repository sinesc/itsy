//! A virtual machine for running Itsy bytecode.

use crate::prelude::*;
use crate::{StackAddress, StackOffset, ItemIndex, VariantIndex};
use crate::bytecode::{HeapRef, HeapRefOp, Constructor, Program, ConstDescriptor, VMFunc, VMData, runtime::{stack::{Stack, StackOp}, heap::{Heap, HeapCmp}, error::*}};
use crate::bytecode::marshal::FunctionMeta;
#[cfg(feature="debugging")]
use crate::bytecode::opcodes::OpCode;
use super::vm_generator::GenControl;

/// Current state of the vm, checked after each instruction.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VMState {
    /// The program is ready to run.
    Ready,
    /// The program has suspended via a `suspend` statement, retaining its stack and heap. It can be
    /// resumed by calling [`VM::run`] again.
    Suspended,
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
    /// Stack of in-flight generator resumptions (see [`GenControl`]).
    pub(crate) gen_control  : Vec<GenControl>,
    /// Host-callable top-level functions, keyed by name (see [`VM::call_typed`]).
    pub(crate) functions    : Map<String, FunctionMeta>,
    /// Return target for host-initiated function calls (a halt instruction).
    pub(crate) host_return_addr: StackAddress,
}

// todo check where bounds here, some seam pointless

/// Public VM methods.
impl<T, U> VM<T, U> {
    /// Create a new VM instance with the given Program.
    pub fn new(program: Program<T>) -> Self where T: VMFunc<T> + VMData<T, U> {
        let Program { instructions, consts, const_descriptors, functions, host_return_addr, .. } = program;
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
            gen_control : Vec::new(),
            functions   : functions,
            host_return_addr: host_return_addr,
        }
    }

    /// Executes the current program until it yields or terminates.
    pub fn run(self: &mut Self, context: &mut U) -> RuntimeResult<VMState> where T: VMFunc<T> + VMData<T, U> {
        if self.state != VMState::Ready && self.state != VMState::Suspended {
            return Err(RuntimeError::new(0, RuntimeErrorKind::NotReady, None));
        }
        self.exec(context);
        match self.state {
            VMState::Terminated if self.heap.len() > 1 => Err(RuntimeError::new(0, RuntimeErrorKind::HeapCorruption, None)),
            VMState::Error(kind) => {
                #[cfg(all(feature="debugging", feature="symbols"))]
                let opcode = self.describe_instruction(self.error_pc).map(|result| result.0);
                #[cfg(not(all(feature="debugging", feature="symbols")))]
                let opcode = None;
                Err(RuntimeError::new(self.error_pc, kind, opcode))
            },
            VMState::Ready => {
                let kind = RuntimeErrorKind::UnexpectedReady;
                self.state = VMState::Error(kind);
                Err(RuntimeError::new(self.pc, kind, None))
            },
            VMState::Terminated | VMState::Suspended => Ok(self.state),
        }
    }

    /// Invokes a top-level Itsy function by name with caller-supplied marshalling, used by the typed
    /// call wrappers generated from an `itsy_api!` `callables { ... }` block. This drives the
    /// monomorphised [`VMValue`](crate::internals::marshal::VMValue) path: `push_args` pushes the
    /// already-typed arguments onto the stack and `read_ret` reads the typed result off the stack top.
    ///
    /// This is the host-call equivalent of running the function: arguments must already be marshalled to
    /// their on-stack footprint by `push_args` (total size matching the function's `arg_size`), and
    /// `read_ret` must consume exactly the function's return value. `arg_count` is checked against the
    /// function's declared parameter count. The VM's prior state is restored on success; on a runtime
    /// error the VM is left in its error state.
    ///
    /// Not intended to be called directly — use the wrappers generated by `itsy_api!`.
    #[doc(hidden)]
    pub fn call_typed<Ret>(
        self: &mut Self,
        context: &mut U,
        name: &str,
        arg_count: usize,
        push_args: impl FnOnce(&mut Stack, &mut Heap),
        read_ret: impl FnOnce(&Stack, &mut Heap) -> Ret,
    ) -> Result<Ret, CallError> where T: VMFunc<T> + VMData<T, U> {
        let meta = match self.functions.get(name) {
            Some(meta) => meta.clone(),
            None => return Err(CallError::FunctionNotFound(name.to_string())),
        };
        if arg_count != meta.args_count {
            return Err(CallError::ArgumentCountMismatch { expected: meta.args_count, got: arg_count });
        }
        // save state to restore once the call completes; restore_sp marks the pre-call stack top
        let saved_pc = self.pc;
        let saved_state = self.state;
        let restore_sp = self.stack.sp();
        // marshal arguments onto the stack (disjoint borrows of stack and heap)
        push_args(&mut self.stack, &mut self.heap);
        // set up the call: returning from the function lands on the host halt instruction, which breaks
        // the exec loop back to us. Setting pc first makes call() record it as the return address.
        self.pc = self.host_return_addr;
        self.call(meta.addr, meta.arg_size);
        self.exec(context);
        // only a clean return to the host halt (Terminated) is a successful call
        match self.state {
            VMState::Terminated => { },
            VMState::Error(kind) => {
                #[cfg(all(feature="debugging", feature="symbols"))]
                let opcode = self.describe_instruction(self.error_pc).map(|result| result.0);
                #[cfg(not(all(feature="debugging", feature="symbols")))]
                let opcode = None;
                return Err(CallError::Runtime(RuntimeError::new(self.error_pc, kind, opcode)))
            }
            _ => return Err(CallError::Runtime(RuntimeError::new(self.pc, RuntimeErrorKind::UnexpectedReady, None))),
        }
        // read the result off the stack top, then truncate back to the pre-call stack and restore state
        let result = read_ret(&self.stack, &mut self.heap);
        self.stack.truncate(restore_sp);
        self.pc = saved_pc;
        self.state = saved_state;
        Ok(result)
    }

    /// Clears runtime error, allowing the VM to resume via run(). This is a no-op if the VM is
    /// in Ready or Suspended state and an error in Terminated state.
    pub fn clear_error(self: &mut Self) -> RuntimeResult where T: VMFunc<T> + VMData<T, U> {
        match self.state {
            VMState::Error(_) => {
                self.error_pc = 0;
                self.state = VMState::Ready;
                Ok(())
            },
            VMState::Ready | VMState::Suspended => Ok(()),
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
        self.gen_control.clear();
    }
}

/// Internal VM methods.
impl<T, U> VM<T, U> {
    /// Pushes program const pool onto the stack.
    fn init_consts(consts: &Vec<u8>, const_descriptors: &Vec<ConstDescriptor>) -> Stack {
        let mut stack = Stack::new();
        // descriptors describe consecutive blocks of the const pool, so the start of each block is
        // the running sum of all preceding block sizes
        let mut start = 0usize;
        for descriptor in const_descriptors {
            let end = start + descriptor.size as usize;
            match descriptor.size {
                1 => stack.push(consts[start]),
                2 => stack.push(u16::from_ne_bytes(consts[start..end].try_into().unwrap())),
                4 => stack.push(u32::from_ne_bytes(consts[start..end].try_into().unwrap())),
                8 => stack.push(u64::from_ne_bytes(consts[start..end].try_into().unwrap())),
                _ => stack.extend_from(&consts[start..end]),
            }
            start = end;
        }
        stack.begin();
        stack
    }

    /// Resolves a value's constructor offset, following the indirection used for trait implementors (offset 0,
    /// looked up via the object's implementor index) and dynamic constructors (offset 1, stored at the end of the
    /// object). Returns `None` if the object has no constructor, i.e. contains no nested heap references.
    pub(crate) fn resolve_constructor_offset(self: &Self, item: HeapRef, mut constructor_offset: StackAddress) -> Option<StackAddress> {
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

    /// Compute the inlined data size (view packed size) for a constructor at the given offset.
    /// Mirrors the compile-time `compute_packed_size` logic.
    fn constructor_data_size(self: &Self, mut offset: StackAddress) -> usize {
        let constructor = Constructor::read_op(&self.stack, &mut offset);
        let parsed = Constructor::parse_with(&self.stack, offset, constructor);
        offset = parsed.offset;

        match constructor {
            Constructor::Primitive => {
                Constructor::read_index(&self.stack, &mut offset) as usize
            }
            Constructor::Struct => {
                let (_implementor_index, num_fields, field_offset) =
                    Constructor::parse_struct(&self.stack, offset);
                let mut total = 0;
                let mut foff = field_offset;
                for _ in 0..num_fields {
                    let field_size = self.constructor_data_size(foff);
                    foff = Constructor::skip(&self.stack, foff);
                    total += field_size;
                }
                total
            }
            Constructor::Enum => {
                // Data size = discriminant (2) + max variant payload size
                let (_implementor_index, num_variants, variant_table_offset) =
                    Constructor::parse_struct(&self.stack, offset);
                let mut max_payload = 0usize;
                for vi in 0..num_variants {
                    let (num_fields, field_offset) =
                        Constructor::parse_variant_table(&self.stack, variant_table_offset, vi as ItemIndex);
                    let mut variant_size = 0usize;
                    let mut foff = field_offset;
                    for _ in 0..num_fields {
                        variant_size += self.constructor_data_size(foff);
                        foff = Constructor::skip(&self.stack, foff);
                    }
                    if variant_size > max_payload {
                        max_payload = variant_size;
                    }
                }
                2 + max_payload
            }
            _ => 0,
        }
    }

    /// Compute the total heap data size for a sequence of fields described by constructors starting at field_offset.
    /// For primitives: sum of byte sizes. For reference types: 8 bytes per field (HeapRef size).
    fn constructor_fields_data_size(self: &Self, num_fields: ItemIndex, field_offset: StackAddress) -> usize {
        let mut total = 0usize;
        let mut foff = field_offset;
        for _ in 0..num_fields {
            let field_start = foff;
            let field_constructor = Constructor::read_op(&self.stack, &mut foff);
            if field_constructor != Constructor::Primitive {
                total += 8; // HeapRef
                foff = Constructor::skip(&self.stack, field_start);
            } else {
                total += Constructor::read_index(&self.stack, &mut foff) as usize;
            }
        }
        total
    }

    /// Compute the heap data size for a constructor (heap layout, not view-packed layout).
    /// For primitives: byte size. For structs: sum of field heap sizes. For enums: disc (2) + max variant.
    fn constructor_heap_size(self: &Self, mut offset: StackAddress) -> usize {
        let constructor = Constructor::read_op(&self.stack, &mut offset);
        let parsed = Constructor::parse_with(&self.stack, offset, constructor);
        offset = parsed.offset;

        match constructor {
            Constructor::Primitive => {
                Constructor::read_index(&self.stack, &mut offset) as usize
            }
            Constructor::Struct => {
                let (_implementor_index, num_fields, field_offset) =
                    Constructor::parse_struct(&self.stack, offset);
                self.constructor_fields_data_size(num_fields, field_offset)
            }
            Constructor::Enum => {
                let (_implementor_index, num_variants, variant_table_offset) =
                    Constructor::parse_struct(&self.stack, offset);
                let mut max_payload = 0usize;
                for vi in 0..num_variants {
                    let (num_fields, field_offset) =
                        Constructor::parse_variant_table(&self.stack, variant_table_offset, vi as ItemIndex);
                    let size = self.constructor_fields_data_size(num_fields, field_offset);
                    if size > max_payload { max_payload = size; }
                }
                2 + max_payload
            }
            _ => 0,
        }
    }

    /// Bounds-checks an array element access: returns `true` if reading or writing `element_size`
    /// bytes at `element_index` stays within the heap object referenced by `item`. Otherwise sets the
    /// VM into an `IndexOutOfBounds` error state and returns `false`. Callers must honor the result;
    /// the element opcodes carry the `check` flag so the exec loop halts once the error is set.
    #[cfg_attr(not(debug_assertions), inline(always))]
    pub(crate) fn check_element_bounds(self: &mut Self, item: HeapRef, element_index: StackAddress, element_size: StackAddress) -> bool {
        let len = self.heap.item(item.index()).data.len() as StackAddress;
        let end = item.offset()
            .saturating_add(element_index.saturating_mul(element_size))
            .saturating_add(element_size);
        end <= len
    }

    /// Builds a `Some(primitive)` heap object: `[variant_tag: u16][payload_bytes]`.
    /// `payload_bytes` are the raw bytes of the primitive value.
    /// Returns the `HeapRef` pointing to the new heap object (refcount 0).
    pub(crate) fn option_some_bytes(self: &mut Self, payload_bytes: &[u8]) -> HeapRef {
        let mut bytes = Vec::with_capacity(2 + payload_bytes.len());
        bytes.extend_from_slice(&(0u16).to_ne_bytes()); // variant 0 = Some
        bytes.extend_from_slice(payload_bytes);
        let index = self.heap.alloc_copy(&bytes, 0);
        HeapRef::new(index, 0)
    }

    /// Builds a `Some(ref)` heap object: `[variant_tag: u16][HeapRef_bytes]`.
    /// The payload `HeapRef` is embedded into the object.
    /// Returns the `HeapRef` pointing to the new heap object (refcount 0).
    pub(crate) fn option_some_ref(self: &mut Self, payload: HeapRef) -> HeapRef {
        let mut bytes = Vec::with_capacity(2 + size_of::<HeapRef>());
        bytes.extend_from_slice(&(0u16).to_ne_bytes()); // variant 0 = Some
        bytes.extend_from_slice(&payload.to_ne_bytes());
        let index = self.heap.alloc_copy(&bytes, 0);
        HeapRef::new(index, 0)
    }

    /// Builds a `None` heap object: `[variant_tag: u16]` (variant 1 = None).
    /// Returns the `HeapRef` pointing to the new heap object (refcount 0).
    pub(crate) fn option_none(self: &mut Self) -> HeapRef {
        let bytes = (1u16).to_ne_bytes(); // variant 1 = None
        let index = self.heap.alloc_copy(&bytes, 0);
        HeapRef::new(index, 0)
    }

    /// Updates the refcounts for given heap reference and any nested heap references. Looks up virtual constructor if offset is 0.
    #[inline(never)]
    pub(crate) fn refcount_value(self: &mut Self, item: HeapRef, constructor_offset: StackAddress, op: HeapRefOp) {
        match self.resolve_constructor_offset(item, constructor_offset) {
            None => self.heap.ref_item(item.index(), op),
            Some(mut constructor_offset) => {
                let constructor = Constructor::read_op(&self.stack, &mut constructor_offset);
                self.refcount_recurse(constructor, HeapRef::new(item.index(), 0), &mut constructor_offset, op);
            }
        }
    }

    /// Support method used by refcount_value() to allow for reading the type before recursing into the type-constructor.
    fn refcount_recurse(self: &mut Self, constructor: Constructor, mut item: HeapRef, constructor_offset: &mut StackAddress, op: HeapRefOp) {
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
                    let element_constructor = Constructor::read_op(&self.stack, constructor_offset);
                    if element_constructor != Constructor::Primitive {
                        let original_constructor_offset = *constructor_offset;
                        // compute number of elements from heap size
                        let num_elements = self.heap.item(item_index).data.len() / HeapRef::primitive_size() as usize;
                        for _ in 0..num_elements {
                            // reset offset each iteration to keep referencing the same type for each element but make sure we have advanced once at the end of the loop
                            *constructor_offset = original_constructor_offset;
                            let element: HeapRef = self.heap.read_seq(&mut item);
                            self.refcount_recurse(element_constructor, element, constructor_offset, op);
                        }
                    } else {
                        // skip primitive num_bytes
                        Constructor::read_index(&self.stack, constructor_offset);
                    }
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Struct => {
                    let (_implementor_index, num_fields, mut field_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                    self.refcount_fields(num_fields, &mut field_offset, &mut item, op);
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Enum => {
                    let variant_index: VariantIndex = self.heap.read_seq(&mut item);
                    let (_implementor_index, num_variants, variant_table_offset) = Constructor::parse_struct(&self.stack, parsed.offset);
                    assert!(variant_index < num_variants, "Enum object specifies invalid enum variant");
                    // seek to variant offset, read it from constructor and seek to the offset
                    let (num_fields, mut field_offset) = Constructor::parse_variant_table(&self.stack, variant_table_offset, variant_index);
                    self.refcount_fields(num_fields, &mut field_offset, &mut item, op);
                    self.heap.ref_item(item_index, op);
                },
                Constructor::String => {
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Closure => {
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Generator => {
                    // A generator carrier's frozen frame (ref-typed args/locals) and its yielded value/key
                    // slots hold heap references owned exclusively by the carrier, so they are released
                    // exactly once, when the carrier itself is freed - never touched as its refcount
                    // otherwise rises and falls (so Inc/DecNoFree skip them). The Generator constructor
                    // carries the value/key constructor offsets; the frame refs come from the live-ref-map
                    // recorded in the carrier's header.
                    if op == HeapRefOp::Dec || op == HeapRefOp::Free {
                        let value_ctor: StackAddress = self.stack.load(parsed.offset);
                        let key_ctor: StackAddress = self.stack.load(parsed.offset + size_of::<StackAddress>() as StackAddress);
                        self.refcount_generator_frame(item_index, value_ctor, key_ctor, op);
                    }
                    self.heap.ref_item(item_index, op);
                },
                Constructor::Virtual => {
                    // trait-object reference: resolve the concrete type's constructor via the object's
                    // implementor index and descend into it. The recursive call performs the single
                    // ref_item for this item; the virtual marker itself carries no inline layout.
                    self.refcount_value(HeapRef::new(item_index, 0), 0, op);
                },
                Constructor::Map => {
                    // Keys and values are boxed (stored inline as HeapRefs). Refcount each live entry's
                    // boxed key and value via their respective sub-constructors. parsed.offset points at
                    // the key constructor; the value constructor follows it.
                    let key_ctor_offset = *constructor_offset;
                    let value_ctor_offset = Constructor::skip(&self.stack, key_ctor_offset);
                    for (key, value) in self.map_live_entries(item_index) {
                        self.refcount_box(key, key_ctor_offset, op);
                        self.refcount_box(value, value_ctor_offset, op);
                    }
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
    fn refcount_fields(self: &mut Self, num_fields: ItemIndex, field_offset: &mut StackAddress, item: &mut HeapRef, op: HeapRefOp) {
        for _ in 0..num_fields {
            let field_constructor = Constructor::read_op(&self.stack, field_offset);
            if field_constructor != Constructor::Primitive {
                let field: HeapRef = self.heap.read_seq(item);
                self.refcount_recurse(field_constructor, field, field_offset, op);
            } else {
                let num_bytes = Constructor::read_index(&self.stack, field_offset) as StackOffset;
                item.add_offset(num_bytes);
            }
        }
    }

    /// Recursively flatten a heap object into a view's backing array using the type's serialized
    /// constructor. Reads data from the heap and writes it to the view at `view_base_offset`.
    #[inline(never)]
    pub(crate) fn heap_to_view_recurse(self: &mut Self, mut constructor_offset: StackAddress, heap_ref: HeapRef, view_index: StackAddress, view_base_offset: StackAddress) {
        // Work item: (constructor_offset, heap_item_index, view_byte_offset)
        let mut work: Vec<(StackAddress, StackAddress, StackAddress)> = Vec::with_capacity(8);
        work.push((constructor_offset, heap_ref.index(), view_base_offset));

        while let Some((ctor_offset, item_index, view_offset)) = work.pop() {
            constructor_offset = ctor_offset;
            let constructor = Constructor::read_op(&self.stack, &mut constructor_offset);
            let parsed = Constructor::parse_with(&self.stack, constructor_offset, constructor);
            constructor_offset = parsed.offset;
            // Bounds check: ensure the data we're about to write fits in the view's backing array
            let data_size = self.constructor_data_size(ctor_offset);
            let view_data_len = self.heap.item(view_index).data.len();
            if view_offset as usize + data_size > view_data_len {
                self.state = VMState::Error(RuntimeErrorKind::IndexOutOfBounds);
                return;
            }
            match constructor {
                Constructor::Primitive => {
                    let num_bytes = Constructor::read_index(&self.stack, &mut constructor_offset) as usize;
                    let src = self.heap.item(item_index).data[..num_bytes].to_vec();
                    let dst_data = &mut self.heap.item_mut(view_index).data;
                    dst_data[view_offset as usize..view_offset as usize + num_bytes]
                        .copy_from_slice(&src);
                }
                Constructor::Struct => {
                    let (_implementor_index, num_fields, field_offset) =
                        Constructor::parse_struct(&self.stack, constructor_offset);
                    self.heap_to_view_fields(
                        num_fields, field_offset, item_index, 0,
                        view_index, view_offset, &mut work,
                    );
                }
                Constructor::Enum => {
                    // Read discriminant from heap, write to view
                    let disc_bytes = self.heap.item(item_index).data[0..2].to_vec();
                    let dst_data = &mut self.heap.item_mut(view_index).data;
                    dst_data[view_offset as usize..view_offset as usize + 2]
                        .copy_from_slice(&disc_bytes);

                    let (_implementor_index, _num_variants, variant_table_offset) =
                        Constructor::parse_struct(&self.stack, constructor_offset);
                    let variant_index: VariantIndex = u16::from_le_bytes(
                        disc_bytes.try_into().unwrap()
                    ) as VariantIndex;
                    let (num_fields, field_offset) =
                        Constructor::parse_variant_table(&self.stack, variant_table_offset, variant_index as ItemIndex);
                    // Enum variant fields start at heap offset 2 (after discriminant)
                    self.heap_to_view_fields(
                        num_fields, field_offset, item_index, 2,
                        view_index, view_offset + 2, &mut work,
                    );
                }
                _ => {
                    panic!("unexpected constructor {:?} in heap_to_view", constructor);
                }
            }
        }
    }

    /// Flatten enum-variant or struct fields from a heap item into a view slot.
    fn heap_to_view_fields(
        self: &mut Self,
        num_fields: ItemIndex,
        mut field_offset: StackAddress,
        heap_item_index: StackAddress,
        initial_heap_offset: usize,
        view_index: StackAddress,
        mut view_byte_offset: StackAddress,
        work: &mut Vec<(StackAddress, StackAddress, StackAddress)>,
    ) {
        let heap_data = self.heap.item(heap_item_index).data.clone();
        let mut heap_read_offset: usize = initial_heap_offset;

        for _ in 0..num_fields {
            let field_ctor_offset = field_offset;
            let field_constructor = Constructor::read_op(&self.stack, &mut field_offset);
            if field_constructor != Constructor::Primitive {
                // Reference type: read heap_ref, schedule recursive flatten
                let field_ref = HeapRef::from_ne_bytes(
                    heap_data[heap_read_offset..heap_read_offset + 8].try_into().unwrap(),
                );
                heap_read_offset += 8;
                let field_data_size = self.constructor_data_size(field_ctor_offset);
                work.push((field_ctor_offset, field_ref.index(), view_byte_offset));
                view_byte_offset += field_data_size as StackAddress;
                // Advance past this field's constructor (including nested data)
                field_offset = Constructor::skip(&self.stack, field_ctor_offset);
            } else {
                // Primitive: copy bytes directly
                let num_bytes = Constructor::read_index(&self.stack, &mut field_offset) as usize;
                let src = heap_data[heap_read_offset..heap_read_offset + num_bytes].to_vec();
                let dst_data = &mut self.heap.item_mut(view_index).data;
                dst_data[view_byte_offset as usize..view_byte_offset as usize + num_bytes]
                    .copy_from_slice(&src);
                heap_read_offset += num_bytes;
                view_byte_offset += num_bytes as StackAddress;
            }
        }
    }

    /// Materialize inlined view data into heap objects using the type's serialized constructor.
    /// Returns the heap_ref of the top-level heap object. Uses a work stack to avoid Rust recursion.
    #[inline(never)]
    pub(crate) fn view_to_heap_recurse(
        self: &mut Self,
        mut constructor_offset: StackAddress,
        view_index: StackAddress,
        view_base_offset: StackAddress,
    ) -> HeapRef {
        // Work item: (constructor_offset, view_byte_offset, parent_heap_item_index, parent_heap_offset)
        // parent fields are None for the root call.
        type WorkItem = (StackAddress, StackAddress, Option<(StackAddress, usize)>);
        let mut work: Vec<WorkItem> = Vec::with_capacity(8);

        // Allocate the root heap object (heap layout size, not view-packed size)
        let root_size = self.constructor_heap_size(constructor_offset);
        let root_index = self.heap.alloc_place(vec![0u8; root_size], 0);

        work.push((constructor_offset, view_base_offset, None));

        while let Some((ctor_offset, view_offset, parent)) = work.pop() {
            constructor_offset = ctor_offset;
            let constructor = Constructor::read_op(&self.stack, &mut constructor_offset);
            let parsed = Constructor::parse_with(&self.stack, constructor_offset, constructor);
            constructor_offset = parsed.offset;

            match constructor {
                Constructor::Primitive => {
                    let num_bytes = Constructor::read_index(&self.stack, &mut constructor_offset) as usize;
                    let (parent_index, parent_offset) = parent.unwrap();
                    let src = self.heap.item(view_index).data[view_offset as usize..view_offset as usize + num_bytes].to_vec();
                    let dst_data = &mut self.heap.item_mut(parent_index).data;
                    dst_data[parent_offset..parent_offset + num_bytes].copy_from_slice(&src);
                }
                Constructor::Struct => {
                    let (_implementor_index, num_fields, field_offset) =
                        Constructor::parse_struct(&self.stack, constructor_offset);
                    let struct_size = self.constructor_fields_data_size(num_fields, field_offset);
                    // For root: use the pre-allocated root object. For nested: allocate new.
                    let struct_index = if parent.is_none() {
                        root_index
                    } else {
                        self.heap.alloc_place(vec![0u8; struct_size], 0)
                    };
                    // Store HeapRef in parent
                    if let Some((parent_index, parent_offset)) = parent {
                        let ref_bytes = HeapRef::new(struct_index, 0).to_ne_bytes();
                        let dst_data = &mut self.heap.item_mut(parent_index).data;
                        dst_data[parent_offset..parent_offset + 8].copy_from_slice(&ref_bytes);
                    }
                    // Schedule field materialization
                    self.view_to_heap_fields(
                        num_fields, field_offset, view_index, view_offset,
                        struct_index, 0, &mut work,
                    );
                }
                Constructor::Enum => {
                    // Read discriminant from view
                    let disc_bytes = self.heap.item(view_index).data[view_offset as usize..view_offset as usize + 2].to_vec();
                    let variant_index: VariantIndex = u16::from_le_bytes(
                        disc_bytes.clone().try_into().unwrap()
                    ) as VariantIndex;

                    let (_implementor_index, _num_variants, variant_table_offset) =
                        Constructor::parse_struct(&self.stack, constructor_offset);
                    let (num_fields, field_offset) =
                        Constructor::parse_variant_table(&self.stack, variant_table_offset, variant_index as ItemIndex);

                    // Allocate enum heap object: discriminant (2) + variant payload
                    let payload_size = if num_fields > 0 {
                        self.constructor_fields_data_size(num_fields, field_offset)
                    } else {
                        0
                    };
                    // For root: use the pre-allocated root object. For nested: allocate new.
                    let enum_index = if parent.is_none() {
                        root_index
                    } else {
                        self.heap.alloc_place(vec![0u8; 2 + payload_size], 0)
                    };

                    // Write discriminant
                    let dst_data = &mut self.heap.item_mut(enum_index).data;
                    dst_data[0..2].copy_from_slice(&disc_bytes);

                    // Store HeapRef in parent
                    if let Some((parent_index, parent_offset)) = parent {
                        let ref_bytes = HeapRef::new(enum_index, 0).to_ne_bytes();
                        let dst_data = &mut self.heap.item_mut(parent_index).data;
                        dst_data[parent_offset..parent_offset + 8].copy_from_slice(&ref_bytes);
                    }

                    // Schedule field materialization (fields start at heap offset 2)
                    self.view_to_heap_fields(
                        num_fields, field_offset, view_index, view_offset + 2,
                        enum_index, 2, &mut work,
                    );
                }
                _ => {
                    panic!("unexpected constructor {:?} in view_to_heap", constructor);
                }
            }
        }

        HeapRef::new(root_index, 0)
    }

    /// Materialize enum-variant or struct fields from a view slot into a heap object.
    fn view_to_heap_fields(
        self: &mut Self,
        num_fields: ItemIndex,
        mut field_offset: StackAddress,
        view_index: StackAddress,
        mut view_byte_offset: StackAddress,
        heap_item_index: StackAddress,
        initial_heap_offset: usize,
        work: &mut Vec<(StackAddress, StackAddress, Option<(StackAddress, usize)>)>,
    ) {
        let mut heap_write_offset: usize = initial_heap_offset;

        for _ in 0..num_fields {
            let field_ctor_offset = field_offset;
            let field_constructor = Constructor::read_op(&self.stack, &mut field_offset);
            if field_constructor != Constructor::Primitive {
                // Reference type: schedule recursive materialization
                work.push((field_ctor_offset, view_byte_offset, Some((heap_item_index, heap_write_offset))));
                let field_data_size = self.constructor_data_size(field_ctor_offset);
                view_byte_offset += field_data_size as StackAddress;
                heap_write_offset += 8; // HeapRef size
                field_offset = Constructor::skip(&self.stack, field_ctor_offset);
            } else {
                // Primitive: copy bytes directly
                let num_bytes = Constructor::read_index(&self.stack, &mut field_offset) as usize;
                let src = self.heap.item(view_index).data[view_byte_offset as usize..view_byte_offset as usize + num_bytes].to_vec();
                let dst_data = &mut self.heap.item_mut(heap_item_index).data;
                dst_data[heap_write_offset..heap_write_offset + num_bytes].copy_from_slice(&src);
                view_byte_offset += num_bytes as StackAddress;
                heap_write_offset += num_bytes;
            }
        }
    }

    /// Compares the targets of two heap references of identical reference type for deep equality, guided by the
    /// type's serialized constructor. Used to implement equality for non-primitive enums.
    #[inline(never)]
    pub(crate) fn compare_value(self: &Self, a: HeapRef, b: HeapRef, constructor_offset: StackAddress) -> bool {
        // both operands share the static type, so the constructor is resolved via a
        match self.resolve_constructor_offset(a, constructor_offset) {
            // no nested constructor: equal iff the same object is referenced
            None => a.index() == b.index(),
            Some(mut constructor_offset) => {
                let constructor = Constructor::read_op(&self.stack, &mut constructor_offset);
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
                let element_constructor = Constructor::read_op(&self.stack, constructor_offset);
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
                    Constructor::read_index(&self.stack, constructor_offset); // skip primitive num_bytes
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
            Constructor::Closure | Constructor::Generator => {
                // closures and generators are opaque: equal only if the same heap object is referenced
                a.index() == b.index()
            },
            Constructor::Map => {
                a.index() == b.index() // FIXME: this compares reference only
            },
            Constructor::Virtual => {
                // trait-object references: resolve each operand's concrete constructor via its implementor
                // index. Differing concrete types are never equal; otherwise compare guided by that constructor.
                match (self.resolve_constructor_offset(a, 0), self.resolve_constructor_offset(b, 0)) {
                    (None, None) => a.index() == b.index(),
                    (Some(a_offset), Some(b_offset)) if a_offset == b_offset => {
                        let mut a_offset = a_offset;
                        let constructor = Constructor::read_op(&self.stack, &mut a_offset);
                        self.compare_recurse(constructor, HeapRef::new(a.index(), 0), HeapRef::new(b.index(), 0), &mut a_offset)
                    },
                    _ => false,
                }
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
            let field_constructor = Constructor::read_op(&self.stack, field_offset);
            if field_constructor != Constructor::Primitive {
                let a_field: HeapRef = self.heap.read_seq(a);
                let b_field: HeapRef = self.heap.read_seq(b);
                if !self.compare_recurse(field_constructor, a_field, b_field, field_offset) {
                    return false;
                }
            } else {
                let num_bytes = Constructor::read_index(&self.stack, field_offset) as StackOffset;
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
        if self.state != VMState::Ready && self.state != VMState::Suspended {
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
            VMState::Terminated | VMState::Suspended | VMState::Ready => Ok(self.state),
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