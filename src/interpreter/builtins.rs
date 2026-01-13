use crate::lexer::Value;
use super::error::InterpreterError;
use indexmap::{IndexMap, IndexSet};
use std::cell::RefCell;
use std::rc::Rc;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
enum HashableValue {
    Null,
    Bool(bool),
    Number(u64),
    String(Rc<str>),
}

impl PartialEq for HashableValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for HashableValue {}

impl Hash for HashableValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::Null => {}
            Self::Bool(b) => b.hash(state),
            Self::Number(n) => n.hash(state),
            Self::String(s) => s.hash(state),
        }
    }
}

fn to_hashable(value: &Value) -> Option<HashableValue> {
    match value {
        Value::Null => Some(HashableValue::Null),
        Value::Bool(b) => Some(HashableValue::Bool(*b)),
        Value::Number(n) => Some(HashableValue::Number(n.to_bits())),
        Value::String(s) => Some(HashableValue::String(s.clone())),
        Value::Array(_) | Value::Object(_) | Value::Function(_) => None,
    }
}

pub fn builtin_count(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Ok(Value::Number(0.0));
    }
    if let Value::Array(arr) = &args[0] {
        Ok(Value::Number(arr.borrow().len() as f64))
    } else {
        Err(InterpreterError::TypeError("count requires array".to_string()))
    }
}

pub fn builtin_sum(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Ok(Value::Number(0.0));
    }
    if let Value::Array(arr) = &args[0] {
        let total = arr.borrow().iter().filter_map(|v| {
            if let Value::Number(n) = v { Some(*n) } else { None }
        }).sum();
        Ok(Value::Number(total))
    } else {
        Err(InterpreterError::TypeError("sum requires array".to_string()))
    }
}

pub fn builtin_avg(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Ok(Value::Number(0.0));
    }
    if let Value::Array(arr_rc) = &args[0] {
        let arr = arr_rc.borrow();
        if arr.is_empty() {
            return Ok(Value::Number(0.0));
        }
        let total: f64 = arr.iter().filter_map(|v| {
            if let Value::Number(n) = v { Some(*n) } else { None }
        }).sum();
        Ok(Value::Number(total / arr.len() as f64))
    } else {
        Err(InterpreterError::TypeError("avg requires array".to_string()))
    }
}

pub fn builtin_min(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Ok(Value::Number(0.0));
    }
    if let Value::Array(arr) = &args[0] {
        let min = arr.borrow().iter().filter_map(|v| {
            if let Value::Number(n) = v { Some(*n) } else { None }
        }).fold(f64::MAX, |acc, n| acc.min(n));
        Ok(Value::Number(min))
    } else {
        Err(InterpreterError::TypeError("min requires array".to_string()))
    }
}

pub fn builtin_max(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Ok(Value::Number(0.0));
    }
    if let Value::Array(arr) = &args[0] {
        let max = arr.borrow().iter().filter_map(|v| {
            if let Value::Number(n) = v { Some(*n) } else { None }
        }).fold(f64::MIN, |acc, n| acc.max(n));
        Ok(Value::Number(max))
    } else {
        Err(InterpreterError::TypeError("max requires array".to_string()))
    }
}

pub fn builtin_take(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("take requires array and count".to_string()));
    }
    if let (Value::Array(arr), Value::Number(n)) = (&args[0], &args[1]) {
        let count = *n as usize;
        let borrowed = arr.borrow();
        let result: Vec<Value> = borrowed.iter().take(count).cloned().collect();
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::TypeError("take requires array and number".to_string()))
    }
}

pub fn builtin_push(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("push requires array and value".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        arr.borrow_mut().push(args[1].clone());
        Ok(args[0].clone())
    } else {
        Err(InterpreterError::TypeError("push requires array as first argument".to_string()))
    }
}

pub fn builtin_print(args: &[Value], value_to_string: impl Fn(&Value) -> String) -> Result<Value, InterpreterError> {
    use std::io::Write;
    
    if args.is_empty() {
        println!();
        return Ok(Value::Null);
    }
    
    let output: Vec<String> = args.iter().map(|arg| value_to_string(arg)).collect();
    println!("{}", output.join(" "));
    std::io::stdout().flush().ok();
    
    Ok(Value::Null)
}

pub fn builtin_input(args: &[Value]) -> Result<Value, InterpreterError> {
    use std::io::{self, Write};
    
    if !args.is_empty() {
        if let Value::String(prompt) = &args[0] {
            print!("{}", prompt);
            io::stdout().flush().ok();
        }
    }
    
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .map_err(|e| InterpreterError::InvalidOperation(format!("Failed to read input: {}", e)))?;
    
    let input = input.trim_end_matches('\n').trim_end_matches('\r');
    
    Ok(Value::String(Rc::from(input)))
}

pub fn builtin_unique(
    args: &[Value],
    deep_equals: impl Fn(&Value, &Value) -> bool,
) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("unique requires argument".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        let borrowed = arr.borrow();
        let has_complex = borrowed.iter().any(|v| matches!(v, Value::Array(_) | Value::Object(_)));
        
        if has_complex {
            let mut result = Vec::new();
            for v in borrowed.iter() {
                if !result.iter().any(|existing| deep_equals(existing, v)) {
                    result.push(v.clone());
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        } else {
            let mut seen: IndexSet<HashableValue> = IndexSet::new();
            let mut result = Vec::new();
            for v in borrowed.iter() {
                if let Some(hashable) = to_hashable(v) {
                    if seen.insert(hashable) {
                        result.push(v.clone());
                    }
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
    } else {
        Err(InterpreterError::TypeError("unique requires array".to_string()))
    }
}

pub fn builtin_sort_by(
    args: &[Value],
    get_field: impl Fn(&Value, &str) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("sort_by requires array and field name".to_string()));
    }
    if let (Value::Array(arr), Value::String(field)) = (&args[0], &args[1]) {
        arr.borrow_mut().sort_by(|a, b| {
            let val_a = get_field(a, field).unwrap_or(Value::Null);
            let val_b = get_field(b, field).unwrap_or(Value::Null);
            match (val_a, val_b) {
                (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(&n2).unwrap_or(std::cmp::Ordering::Equal),
                (Value::String(s1), Value::String(s2)) => s1.cmp(&s2),
                _ => std::cmp::Ordering::Equal,
            }
        });
        Ok(args[0].clone())
    } else {
        Err(InterpreterError::TypeError("sort_by requires array and field name string".to_string()))
    }
}

pub fn builtin_group_by(
    args: &[Value],
    get_field: impl Fn(&Value, &str) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("group_by requires array and field name".to_string()));
    }
    if let (Value::Array(arr_rc), Value::String(field)) = (&args[0], &args[1]) {
        let mut groups: IndexMap<String, Vec<Value>> = IndexMap::new();
        let arr = arr_rc.borrow();
        for item in arr.iter() {
            let key_val = get_field(item, field).unwrap_or(Value::Null);
            let key_str = match key_val {
                Value::String(s) => s.to_string(),
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                _ => "null".to_string(),
            };
            groups.entry(key_str).or_default().push(item.clone());
        }
        let mut result_obj = IndexMap::new();
        for (k, v) in groups {
            result_obj.insert(k, Value::Array(Rc::new(RefCell::new(v))));
        }
        Ok(Value::Object(Rc::new(RefCell::new(result_obj))))
    } else {
        Err(InterpreterError::TypeError("group_by requires array and field name string".to_string()))
    }
}

