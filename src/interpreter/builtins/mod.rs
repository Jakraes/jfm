mod array;
mod generators;
mod misc;
mod object;
mod string;

pub use array::*;
pub use generators::*;
pub use misc::*;
pub use object::*;
pub use string::*;

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
