
#[cfg(feature="compiler")]
pub use crate::shared::error::{Error, BuildError};
pub use crate::bytecode::Program;

#[cfg(feature="compiler")]
pub mod parser {
    //! Sourcecode parsing.
    //!
    //! See [parse] for an example that loads and parses multiple files into a [ParsedProgram], ready for type resolution by [resolve](crate::resolver::resolve).
    pub use crate::frontend::parser::{parse, parse_module, module_filename, types::ParsedModule, types::ParsedProgram, error::{ParseError, ParseErrorKind}};
}

pub mod internals {
    //! Advanced functionality like AST and bytecode manipulation.
    #[cfg(feature="compiler")]
    pub mod ast {
        //! Abstract syntax tree representation.
        pub use crate::frontend::ast::*;
    }
    #[cfg(feature="compiler")]
    pub mod resolved {
        //! Resolved program type information.
        pub use crate::shared::meta::{Type, Struct, Enum, Array, Trait, ImplTrait, Function, FunctionKind, Binding, EnumVariant};
        pub use crate::shared::numeric::Numeric;
        pub mod ids {
            //! Typed ids used to refer to specific program elements.
            pub use crate::shared::typed_ids::{BindingId, FunctionId, TypeId, ScopeId};
            pub use crate::frontend::resolver::resolved::Resolved;
        }
    }
    pub mod binary {
        //! Support for generating and executing compiled code.
        #[cfg(feature="compiler")]
        pub use crate::bytecode::writer::{Writer, StoreConst};
        #[cfg(feature="runtime")]
        pub mod stack {
            //! Virtual machine stack.
            pub use crate::bytecode::runtime::stack::{Stack, StackOp, StackOffsetOp, StackRelativeOp};
        }
        #[cfg(feature="runtime")]
        pub mod heap {
            //! Virtual machine heap.
            pub use crate::bytecode::runtime::heap::{Heap, HeapCmp, HeapOp};
            pub use crate::bytecode::{HeapRef, HeapRefOp};
        }
        pub mod sizes {
            //! Defines the sizes of fundamental VM types.
            pub use crate::config::*;
            pub use crate::config_derived::*;
        }
        pub use crate::bytecode::{VMFunc, VMData};
        pub use crate::bytecode::{opcodes::OpCode, builtins::Builtin};
    }
    #[cfg(doc)]
    pub use crate::bytecode::builtins::documentation;
}

#[cfg(feature="compiler")]
pub mod resolver {
    //! Type resolution.
    //!
    //! See [resolve] for an example that resolves a [ParsedProgram](crate::parser::ParsedProgram) into a [ResolvedProgram], ready for compilation by [compile](crate::compiler::compile).
    pub use crate::frontend::resolver::{resolve, resolved::ResolvedProgram, error::{ResolveError, ResolveErrorKind}};
}

#[cfg(feature="compiler")]
pub mod compiler {
    //! Bytecode generation.
    //!
    //! See [compile] for an example that compiles a [ResolvedProgram](crate::resolver::ResolvedProgram) into a [Program](crate::Program), ready to be run by [run](crate::run) or [VM::run](crate::runtime::VM::run).
    pub use crate::bytecode::compiler::{compile, error::{CompileError, CompileErrorKind}};
}

#[cfg(feature="runtime")]
pub mod runtime {
    //! Bytecode execution.
    pub use crate::bytecode::runtime::{vm::{VM, VMState}, error::{RuntimeError, RuntimeErrorKind}};
}