use crate::value::Value;
use super::super::error::InterpreterError;
use super::{require_args, with_string};
use std::cell::RefCell;
use std::rc::Rc;

pub fn builtin_split(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "split");
    if let (Value::String(s), Value::String(delim)) = (&args[0], &args[1]) {
        let parts: Vec<Value> = s
            .split(delim.as_ref())
            .map(|p| Value::String(Rc::from(p)))
            .collect();
        Ok(Value::Array(Rc::new(RefCell::new(parts))))
    } else {
        Err(InterpreterError::type_error(
            "split requires string and delimiter",
        ))
    }
}

pub fn builtin_join(
    args: &[Value],
    value_to_string: impl Fn(&Value) -> String,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "join");
    if let (Value::Array(arr), Value::String(delim)) = (&args[0], &args[1]) {
        let parts: Vec<String> = arr.borrow().iter().map(value_to_string).collect();
        Ok(Value::String(Rc::from(parts.join(delim.as_ref()))))
    } else {
        Err(InterpreterError::type_error(
            "join requires array and delimiter",
        ))
    }
}

pub fn builtin_trim(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "trim");
    with_string!(args, "trim", |s: &Rc<str>| Ok(Value::String(Rc::from(
        s.trim()
    ))))
}

pub fn builtin_upper(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "upper");
    with_string!(args, "upper", |s: &Rc<str>| Ok(Value::String(Rc::from(
        s.to_uppercase()
    ))))
}

pub fn builtin_lower(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "lower");
    with_string!(args, "lower", |s: &Rc<str>| Ok(Value::String(Rc::from(
        s.to_lowercase()
    ))))
}

pub fn builtin_contains(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "contains");
    if let (Value::String(s), Value::String(sub)) = (&args[0], &args[1]) {
        Ok(Value::Bool(s.contains(sub.as_ref())))
    } else {
        Err(InterpreterError::type_error("contains requires two strings"))
    }
}

pub fn builtin_starts_with(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "starts_with");
    if let (Value::String(s), Value::String(prefix)) = (&args[0], &args[1]) {
        Ok(Value::Bool(s.starts_with(prefix.as_ref())))
    } else {
        Err(InterpreterError::type_error(
            "starts_with requires two strings",
        ))
    }
}

pub fn builtin_ends_with(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "ends_with");
    if let (Value::String(s), Value::String(suffix)) = (&args[0], &args[1]) {
        Ok(Value::Bool(s.ends_with(suffix.as_ref())))
    } else {
        Err(InterpreterError::type_error(
            "ends_with requires two strings",
        ))
    }
}

pub fn builtin_replace(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 3, "replace");
    if let (Value::String(s), Value::String(from), Value::String(to)) =
        (&args[0], &args[1], &args[2])
    {
        Ok(Value::String(Rc::from(
            s.replace(from.as_ref(), to.as_ref()),
        )))
    } else {
        Err(InterpreterError::type_error(
            "replace requires three strings",
        ))
    }
}

pub fn builtin_len(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "len");
    with_string!(args, "len", |s: &Rc<str>| Ok(Value::Number(
        s.len() as f64,
        false
    )))
}
