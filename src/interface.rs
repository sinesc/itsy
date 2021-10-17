
pub use crate::shared::error::Error;
#[cfg(feature="compiler")]
pub use crate::shared::error::BuildError;

#[cfg(feature="compiler")]
pub mod parser {
    //! Sourcecode parsing.
    pub use crate::frontend::parser::{parse, parse_module, module_filename, types::ParsedModule, types::ParsedProgram, error::{ParseError, ParseErrorKind}};
}

#[cfg(feature="compiler")]
pub mod ast {
    //! Abstract syntax tree representation.
    pub use crate::frontend::ast::*;
}

#[cfg(feature="compiler")]
pub mod resolver {
    //! Type resolution.
    pub use crate::frontend::resolver::{resolve, resolved::ResolvedProgram, error::ResolveError, error::ResolveErrorKind};
    pub mod resolved {
        //! Resolved information to be used by the bytecode generator.
        pub use crate::frontend::resolver::resolved::{ResolvedProgram, IdMappings};
        pub mod ids {
            //! Typed ids used to refer to specific program elements.
            pub use crate::shared::typed_ids::{BindingId, FunctionId, TypeId, ScopeId};
        }
        pub mod meta {
            //! Resolved program type information.
            pub use crate::shared::types::{Type, Struct, Enum, Array};
            pub use crate::shared::numeric::Numeric;
            pub use crate::shared::infos::{FunctionInfo, BindingInfo, FunctionKind};
        }
    }
}

#[cfg(feature="compiler")]
pub mod compiler {
    //! Bytecode generation.
    pub use crate::bytecode::Program;
    pub use crate::bytecode::compiler::{compile, error::{CompileError, CompileErrorKind}};
    pub use crate::bytecode::writer::{Writer, StoreConst};
    pub use crate::bytecode::opcodes::OpCode;
}

pub mod runtime {
    //! Bytecode execution.
    pub use crate::bytecode::{runtime::vm::{VM, VMState}, VMFunc, VMData};
    pub mod stack {
        //! Virtual machine stack.
        pub use crate::bytecode::runtime::stack::{Stack, StackOp, StackOffsetOp, StackRelativeOp};
    }
    pub mod heap {
        //! Virtual machine heap.
        pub use crate::bytecode::runtime::heap::{Heap, HeapCmp, HeapRefOp, HeapOp};
        pub use crate::bytecode::{HeapRef, HeapSlice};
    }
}