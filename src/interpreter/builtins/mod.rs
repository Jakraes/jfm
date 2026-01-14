//! Built-in functions for the JFM interpreter.
//!
//! Functions are organized into categories:
//! - **Array**: Operations on arrays (sum, avg, min, max, sort, filter, etc.)
//! - **String**: String manipulation (split, join, trim, upper, lower, etc.)
//! - **Math**: Mathematical functions (floor, ceil, sqrt, sin, cos, etc.)
//! - **Object**: Object operations (keys, values, entries, merge, etc.)
//! - **Type**: Type checking and conversion (typeof, is_*, to_*)
//! - **Generators**: Functions that generate values (range, replicate, cross)
//! - **IO**: Input/output functions (print, input)

mod array;
mod generators;
mod misc;
mod object;
mod string;

// Re-export all builtins
pub use array::*;
pub use generators::*;
pub use misc::*;
pub use object::*;
pub use string::*;

// Types used by the exported macros - must be in scope here
#[allow(unused_imports)]
use crate::value::Value;
#[allow(unused_imports)]
use super::error::InterpreterError;

macro_rules! require_args {
    ($args:expr, $n:expr, $name:expr) => {
        if $args.len() < $n {
            return Err(InterpreterError::invalid_operation(
                format!("{} requires {} argument(s)", $name, $n)
            ));
        }
    };
}

macro_rules! with_array {
    ($args:expr, $name:expr, $body:expr) => {
        match &$args[0] {
            Value::Array(arr) => $body(arr),
            _ => Err(InterpreterError::type_error(format!("{} requires array", $name))),
        }
    };
}

macro_rules! with_string {
    ($args:expr, $name:expr, $body:expr) => {
        match &$args[0] {
            Value::String(s) => $body(s),
            _ => Err(InterpreterError::type_error(format!("{} requires string", $name))),
        }
    };
}

macro_rules! with_number {
    ($args:expr, $name:expr, $body:expr) => {
        match &$args[0] {
            Value::Number(n, _) => $body(*n),
            _ => Err(InterpreterError::type_error(format!("{} requires number", $name))),
        }
    };
}

pub(crate) use require_args;
pub(crate) use with_array;
pub(crate) use with_number;
pub(crate) use with_string;
