#[macro_use]
mod util;
#[path="vm/bitwise.rs"]
mod bitwise;
#[path="vm/operators.rs"]
mod operators;
#[path="vm/casts.rs"]
mod casts;
#[path="vm/expressions.rs"]
mod expressions;
#[path="vm/literals.rs"]
mod literals;
#[path="vm/arrays.rs"]
mod arrays;
#[path="vm/maps.rs"]
mod maps;
#[path="vm/structs.rs"]
mod structs;
#[path="vm/for_loop.rs"]
mod for_loop;
#[path="vm/refs.rs"]
mod refs;
#[path="vm/strings.rs"]
mod strings;
#[path="vm/traits.rs"]
mod traits;
#[path="vm/controlflow.rs"]
mod controlflow;
#[path="vm/enums.rs"]
mod enums;
#[path="vm/result.rs"]
mod result;
#[path="vm/option.rs"]
mod option;
#[path="vm/generators.rs"]
mod generators;
#[path="vm/closure.rs"]
mod closure;
#[path="vm/suspend.rs"]
mod suspend;
#[cfg(feature="call_function")]
#[path="vm/call_function.rs"]
mod call_function;
#[cfg(feature="derive")]
#[path="vm/api_types.rs"]
mod api_types;
#[cfg(all(feature="derive", feature="call_function"))]
#[path="vm/call_typed.rs"]
mod call_typed;
#[path="vm/index_ops.rs"]
mod index_ops;
#[path="vm/int_methods.rs"]
mod int_methods;
#[path="vm/consts.rs"]
mod consts;
