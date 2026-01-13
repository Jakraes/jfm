//! Mathematical built-in functions.

use crate::value::Value;
use super::super::error::InterpreterError;
use super::{require_args, with_number};

macro_rules! unary_math {
    ($name:ident, $op:ident) => {
        pub fn $name(args: &[Value]) -> Result<Value, InterpreterError> {
            require_args!(args, 1, stringify!($op));
            with_number!(args, stringify!($op), |n: f64| Ok(Value::Number(
                n.$op(),
                true
            )))
        }
    };
}

unary_math!(builtin_floor, floor);
unary_math!(builtin_ceil, ceil);
unary_math!(builtin_round, round);
unary_math!(builtin_abs, abs);
unary_math!(builtin_sin, sin);
unary_math!(builtin_cos, cos);
unary_math!(builtin_tan, tan);

pub fn builtin_sqrt(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "sqrt");
    with_number!(args, "sqrt", |n: f64| {
        if n < 0.0 {
            Err(InterpreterError::invalid_operation("sqrt: negative number"))
        } else {
            Ok(Value::Number(n.sqrt(), true))
        }
    })
}

pub fn builtin_pow(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "pow");
    if let (Value::Number(base, _), Value::Number(exp, _)) = (&args[0], &args[1]) {
        Ok(Value::Number(base.powf(*exp), true))
    } else {
        Err(InterpreterError::type_error("pow requires two numbers"))
    }
}

pub fn builtin_random(_args: &[Value]) -> Result<Value, InterpreterError> {
    use rand::Rng;
    Ok(Value::Number(rand::thread_rng().r#gen::<f64>(), true))
}
