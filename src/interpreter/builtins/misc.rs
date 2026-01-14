//! Miscellaneous built-in functions (IO, Math, Types).

use crate::value::Value;
use super::super::error::InterpreterError;
use super::{require_args, with_number, with_string};
use std::cell::RefCell;
use std::rc::Rc;

// IO functions

pub fn builtin_print(
    args: &[Value],
    value_to_string: impl Fn(&Value) -> String,
) -> Result<Value, InterpreterError> {
    use std::io::Write;
    if args.is_empty() {
        println!();
    } else {
        let output: Vec<String> = args.iter().map(&value_to_string).collect();
        println!("{}", output.join(" "));
    }
    std::io::stdout().flush().ok();
    Ok(Value::Null)
}

pub fn builtin_input(args: &[Value]) -> Result<Value, InterpreterError> {
    use std::io::{self, Write};
    if let Some(Value::String(prompt)) = args.first() {
        print!("{}", prompt);
        io::stdout().flush().ok();
    }
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .map_err(|e| InterpreterError::invalid_operation(format!("Failed to read input: {}", e)))?;
    Ok(Value::String(Rc::from(input.trim_end_matches(['\n', '\r']))))
}

// Math functions

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

// Type functions

macro_rules! type_check {
    ($name:ident, $pattern:pat) => {
        pub fn $name(args: &[Value]) -> Result<Value, InterpreterError> {
            require_args!(args, 1, stringify!($name));
            Ok(Value::Bool(matches!(args[0], $pattern)))
        }
    };
}

pub fn builtin_typeof(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "typeof");
    Ok(Value::String(Rc::from(match &args[0] {
        Value::Null => "null",
        Value::Bool(_) => "bool",
        Value::Number(_, _) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
        Value::Function(_) => "function",
        Value::Module(_) => "module",
    })))
}

type_check!(builtin_is_null, Value::Null);
type_check!(builtin_is_array, Value::Array(_));
type_check!(builtin_is_object, Value::Object(_));
type_check!(builtin_is_string, Value::String(_));
type_check!(builtin_is_number, Value::Number(_, _));
type_check!(builtin_is_bool, Value::Bool(_));

pub fn builtin_to_string(
    args: &[Value],
    value_to_string: impl Fn(&Value) -> String,
) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "to_string");
    Ok(Value::String(Rc::from(value_to_string(&args[0]))))
}

pub fn builtin_to_number(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "to_number");
    match &args[0] {
        Value::Number(n, is_float) => Ok(Value::Number(*n, *is_float)),
        Value::String(s) => s
            .parse::<f64>()
            .map(|n| Value::Number(n, s.contains('.')))
            .map_err(|_| InterpreterError::invalid_operation("to_number: invalid string")),
        _ => Err(InterpreterError::type_error(
            "to_number requires number or string",
        )),
    }
}

pub fn builtin_to_int(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "to_int");
    match &args[0] {
        Value::Number(n, _) => Ok(Value::Number(n.trunc(), false)),
        Value::String(s) => s
            .parse::<f64>()
            .map(|n| Value::Number(n.trunc(), false))
            .map_err(|_| InterpreterError::invalid_operation("to_int: invalid string")),
        Value::Bool(b) => Ok(Value::Number(if *b { 1.0 } else { 0.0 }, false)),
        _ => Err(InterpreterError::type_error(
            "to_int requires number, string, or bool",
        )),
    }
}

pub fn builtin_to_float(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "to_float");
    match &args[0] {
        Value::Number(n, _) => Ok(Value::Number(*n, true)),
        Value::String(s) => s
            .parse::<f64>()
            .map(|n| Value::Number(n, true))
            .map_err(|_| InterpreterError::invalid_operation("to_float: invalid string")),
        Value::Bool(b) => Ok(Value::Number(if *b { 1.0 } else { 0.0 }, true)),
        _ => Err(InterpreterError::type_error(
            "to_float requires number, string, or bool",
        )),
    }
}

pub fn builtin_to_bool(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "to_bool");
    Ok(Value::Bool(match &args[0] {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Number(n, _) => *n != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Array(a) => !a.borrow().is_empty(),
        Value::Object(o) => !o.borrow().is_empty(),
        Value::Function(_) => true,
        Value::Module(_) => true,
    }))
}

pub fn builtin_parse_json(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "parse_json");
    with_string!(args, "parse_json", |s: &Rc<str>| {
        use crate::format;
        fn convert(v: &serde_json::Value) -> Value {
            match v {
                serde_json::Value::Null => Value::Null,
                serde_json::Value::Bool(b) => Value::Bool(*b),
                serde_json::Value::Number(n) => Value::Number(n.as_f64().unwrap_or(0.0), n.is_f64()),
                serde_json::Value::String(s) => Value::String(Rc::from(s.as_str())),
                serde_json::Value::Array(a) => {
                    Value::Array(Rc::new(RefCell::new(a.iter().map(convert).collect())))
                }
                serde_json::Value::Object(o) => Value::Object(Rc::new(RefCell::new(
                    o.iter().map(|(k, v)| (k.clone(), convert(v))).collect(),
                ))),
            }
        }
        format::parse_json(s.as_ref())
            .map(|v| convert(&v))
            .map_err(|e| InterpreterError::invalid_operation(format!("parse_json: {}", e)))
    })
}
