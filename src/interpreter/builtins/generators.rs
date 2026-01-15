use crate::value::Value;
use super::super::error::InterpreterError;
use super::require_args;
use std::cell::RefCell;
use std::rc::Rc;

pub fn builtin_replicate(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "replicate");
    if let (Value::Number(count, _), Value::Function(_)) = (&args[0], &args[1]) {
        let n = *count as usize;
        let mut result = Vec::with_capacity(n);
        for i in 0..n {
            result.push(call_fn(&args[1], &[Value::Number(i as f64, false)])?);
        }
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::type_error(
            "replicate requires count and function",
        ))
    }
}

pub fn builtin_range_fn(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "range");
    let start = match &args[0] {
        Value::Number(n, _) => *n,
        _ => return Err(InterpreterError::type_error("range requires numbers")),
    };
    let end = match &args[1] {
        Value::Number(n, _) => *n,
        _ => return Err(InterpreterError::type_error("range requires numbers")),
    };
    let step = if args.len() > 2 {
        match &args[2] {
            Value::Number(n, _) => *n,
            _ => return Err(InterpreterError::type_error("range step must be a number")),
        }
    } else if start <= end {
        1.0
    } else {
        -1.0
    };

    if step == 0.0 {
        return Err(InterpreterError::invalid_operation(
            "range step cannot be zero",
        ));
    }

    let mut result = Vec::new();
    let mut current = start;

    if step > 0.0 {
        while current <= end + f64::EPSILON {
            result.push(Value::Number(current, current.fract() != 0.0));
            current += step;
        }
    } else {
        while current >= end - f64::EPSILON {
            result.push(Value::Number(current, current.fract() != 0.0));
            current += step;
        }
    }

    Ok(Value::Array(Rc::new(RefCell::new(result))))
}

pub fn builtin_cross(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::invalid_operation(
            "cross requires at least one array",
        ));
    }

    let arrays: Vec<Vec<Value>> = args
        .iter()
        .map(|arg| match arg {
            Value::Array(arr) => Ok(arr.borrow().clone()),
            _ => Err(InterpreterError::type_error("cross requires arrays")),
        })
        .collect::<Result<Vec<_>, _>>()?;

    if arrays.iter().any(|a| a.is_empty()) {
        return Ok(Value::Array(Rc::new(RefCell::new(vec![]))));
    }

    let mut result: Vec<Vec<Value>> = vec![vec![]];

    for array in &arrays {
        let mut new_result = Vec::new();
        for combo in &result {
            for item in array {
                let mut new_combo = combo.clone();
                new_combo.push(item.clone());
                new_result.push(new_combo);
            }
        }
        result = new_result;
    }

    let final_result: Vec<Value> = result
        .into_iter()
        .map(|combo| Value::Array(Rc::new(RefCell::new(combo))))
        .collect();

    Ok(Value::Array(Rc::new(RefCell::new(final_result))))
}
