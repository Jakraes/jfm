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

pub fn builtin_reverse(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("reverse requires array argument".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        let mut result: Vec<Value> = arr.borrow().iter().cloned().collect();
        result.reverse();
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::TypeError("reverse requires array".to_string()))
    }
}

pub fn builtin_sort(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("sort requires array argument".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        let mut result: Vec<Value> = arr.borrow().iter().cloned().collect();
        result.sort_by(|a, b| {
            match (a, b) {
                (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(n2).unwrap_or(std::cmp::Ordering::Equal),
                (Value::String(s1), Value::String(s2)) => s1.cmp(s2),
                (Value::Bool(b1), Value::Bool(b2)) => b1.cmp(b2),
                (Value::Null, Value::Null) => std::cmp::Ordering::Equal,
                _ => std::cmp::Ordering::Equal,
            }
        });
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::TypeError("sort requires array".to_string()))
    }
}

pub fn builtin_slice(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("slice requires array argument".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        let arr_ref = arr.borrow();
        let len = arr_ref.len();
        
        let start = if args.len() > 1 {
            if let Value::Number(n) = &args[1] {
                let idx = *n as i64;
                if idx < 0 {
                    (len as i64 + idx).max(0) as usize
                } else {
                    (idx as usize).min(len)
                }
            } else {
                return Err(InterpreterError::TypeError("slice start index must be number".to_string()));
            }
        } else {
            0
        };
        
        let end = if args.len() > 2 {
            if let Value::Number(n) = &args[2] {
                let idx = *n as i64;
                if idx < 0 {
                    (len as i64 + idx).max(0) as usize
                } else {
                    (idx as usize).min(len)
                }
            } else {
                return Err(InterpreterError::TypeError("slice end index must be number".to_string()));
            }
        } else {
            len
        };
        
        let result: Vec<Value> = arr_ref.iter().skip(start).take(end.saturating_sub(start)).cloned().collect();
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::TypeError("slice requires array".to_string()))
    }
}

pub fn builtin_pop(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("pop requires array argument".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        let mut arr_ref = arr.borrow_mut();
        if arr_ref.is_empty() {
            Ok(Value::Null)
        } else {
            Ok(arr_ref.pop().unwrap())
        }
    } else {
        Err(InterpreterError::TypeError("pop requires array".to_string()))
    }
}

pub fn builtin_shift(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("shift requires array argument".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        let mut arr_ref = arr.borrow_mut();
        if arr_ref.is_empty() {
            Ok(Value::Null)
        } else {
            Ok(arr_ref.remove(0))
        }
    } else {
        Err(InterpreterError::TypeError("shift requires array".to_string()))
    }
}

fn flatten_recursive(arr: &[Value], depth: usize, current_depth: usize) -> Vec<Value> {
    if current_depth >= depth {
        return arr.to_vec();
    }
    let mut result = Vec::new();
    for item in arr {
        match item {
            Value::Array(nested) => {
                let nested_ref = nested.borrow();
                result.extend(flatten_recursive(&nested_ref, depth, current_depth + 1));
            }
            _ => result.push(item.clone()),
        }
    }
    result
}

pub fn builtin_flat(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("flat requires array argument".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        let arr_ref = arr.borrow();
        let depth = if args.len() > 1 {
            if let Value::Number(n) = &args[1] {
                *n as usize
            } else {
                return Err(InterpreterError::TypeError("flat depth must be number".to_string()));
            }
        } else {
            1
        };
        let result = flatten_recursive(&arr_ref, depth, 0);
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::TypeError("flat requires array".to_string()))
    }
}

pub fn builtin_flatten(args: &[Value]) -> Result<Value, InterpreterError> {
    builtin_flat(args)
}

pub fn builtin_find(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("find requires array and predicate function".to_string()));
    }
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        let arr_ref = arr.borrow();
        for item in arr_ref.iter() {
            let result = call_fn(&args[1], &[item.clone()])?;
            if let Value::Bool(true) = result {
                return Ok(item.clone());
            }
        }
        Ok(Value::Null)
    } else {
        Err(InterpreterError::TypeError("find requires array and function".to_string()))
    }
}

pub fn builtin_find_index(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("find_index requires array and predicate function".to_string()));
    }
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        let arr_ref = arr.borrow();
        for (index, item) in arr_ref.iter().enumerate() {
            let result = call_fn(&args[1], &[item.clone()])?;
            if let Value::Bool(true) = result {
                return Ok(Value::Number(index as f64));
            }
        }
        Ok(Value::Number(-1.0))
    } else {
        Err(InterpreterError::TypeError("find_index requires array and function".to_string()))
    }
}

pub fn builtin_reduce(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    if args.len() < 3 {
        return Err(InterpreterError::InvalidOperation("reduce requires array, function, and initial value".to_string()));
    }
    if let (Value::Array(arr), Value::Function(_), initial) = (&args[0], &args[1], &args[2]) {
        let arr_ref = arr.borrow();
        let mut accumulator = initial.clone();
        for item in arr_ref.iter() {
            accumulator = call_fn(&args[1], &[accumulator, item.clone()])?;
        }
        Ok(accumulator)
    } else {
        Err(InterpreterError::TypeError("reduce requires array, function, and initial value".to_string()))
    }
}

pub fn builtin_every(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("every requires array and predicate function".to_string()));
    }
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        let arr_ref = arr.borrow();
        if arr_ref.is_empty() {
            return Ok(Value::Bool(true));
        }
        for item in arr_ref.iter() {
            let result = call_fn(&args[1], &[item.clone()])?;
            if let Value::Bool(false) = result {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    } else {
        Err(InterpreterError::TypeError("every requires array and function".to_string()))
    }
}

pub fn builtin_some(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("some requires array and predicate function".to_string()));
    }
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        let arr_ref = arr.borrow();
        for item in arr_ref.iter() {
            let result = call_fn(&args[1], &[item.clone()])?;
            if let Value::Bool(true) = result {
                return Ok(Value::Bool(true));
            }
        }
        Ok(Value::Bool(false))
    } else {
        Err(InterpreterError::TypeError("some requires array and function".to_string()))
    }
}

pub fn builtin_zip(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("zip requires two arrays".to_string()));
    }
    if let (Value::Array(arr1), Value::Array(arr2)) = (&args[0], &args[1]) {
        let arr1_ref = arr1.borrow();
        let arr2_ref = arr2.borrow();
        let min_len = arr1_ref.len().min(arr2_ref.len());
        let mut result = Vec::new();
        for i in 0..min_len {
            let pair = Value::Array(Rc::new(RefCell::new(vec![arr1_ref[i].clone(), arr2_ref[i].clone()])));
            result.push(pair);
        }
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::TypeError("zip requires two arrays".to_string()))
    }
}

pub fn builtin_first(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("first requires array argument".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        let arr_ref = arr.borrow();
        if arr_ref.is_empty() {
            Ok(Value::Null)
        } else {
            Ok(arr_ref[0].clone())
        }
    } else {
        Err(InterpreterError::TypeError("first requires array".to_string()))
    }
}

pub fn builtin_last(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("last requires array argument".to_string()));
    }
    if let Value::Array(arr) = &args[0] {
        let arr_ref = arr.borrow();
        if arr_ref.is_empty() {
            Ok(Value::Null)
        } else {
            Ok(arr_ref[arr_ref.len() - 1].clone())
        }
    } else {
        Err(InterpreterError::TypeError("last requires array".to_string()))
    }
}

pub fn builtin_split(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("split requires string and delimiter".to_string()));
    }
    if let (Value::String(s), Value::String(delimiter)) = (&args[0], &args[1]) {
        let parts: Vec<Value> = s.split(delimiter.as_ref())
            .map(|part| Value::String(Rc::from(part)))
            .collect();
        Ok(Value::Array(Rc::new(RefCell::new(parts))))
    } else {
        Err(InterpreterError::TypeError("split requires string and delimiter string".to_string()))
    }
}

pub fn builtin_join(
    args: &[Value],
    value_to_string: impl Fn(&Value) -> String,
) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("join requires array and delimiter".to_string()));
    }
    if let (Value::Array(arr), Value::String(delimiter)) = (&args[0], &args[1]) {
        let arr_ref = arr.borrow();
        let parts: Vec<String> = arr_ref.iter().map(|v| value_to_string(v)).collect();
        let result = parts.join(delimiter.as_ref());
        Ok(Value::String(Rc::from(result)))
    } else {
        Err(InterpreterError::TypeError("join requires array and delimiter string".to_string()))
    }
}

pub fn builtin_trim(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("trim requires string argument".to_string()));
    }
    if let Value::String(s) = &args[0] {
        Ok(Value::String(Rc::from(s.trim())))
    } else {
        Err(InterpreterError::TypeError("trim requires string".to_string()))
    }
}

pub fn builtin_upper(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("upper requires string argument".to_string()));
    }
    if let Value::String(s) = &args[0] {
        Ok(Value::String(Rc::from(s.to_uppercase())))
    } else {
        Err(InterpreterError::TypeError("upper requires string".to_string()))
    }
}

pub fn builtin_lower(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("lower requires string argument".to_string()));
    }
    if let Value::String(s) = &args[0] {
        Ok(Value::String(Rc::from(s.to_lowercase())))
    } else {
        Err(InterpreterError::TypeError("lower requires string".to_string()))
    }
}

pub fn builtin_contains(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("contains requires string and substring".to_string()));
    }
    if let (Value::String(s), Value::String(substr)) = (&args[0], &args[1]) {
        Ok(Value::Bool(s.contains(substr.as_ref())))
    } else {
        Err(InterpreterError::TypeError("contains requires string and substring string".to_string()))
    }
}

pub fn builtin_starts_with(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("starts_with requires string and prefix".to_string()));
    }
    if let (Value::String(s), Value::String(prefix)) = (&args[0], &args[1]) {
        Ok(Value::Bool(s.starts_with(prefix.as_ref())))
    } else {
        Err(InterpreterError::TypeError("starts_with requires string and prefix string".to_string()))
    }
}

pub fn builtin_ends_with(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("ends_with requires string and suffix".to_string()));
    }
    if let (Value::String(s), Value::String(suffix)) = (&args[0], &args[1]) {
        Ok(Value::Bool(s.ends_with(suffix.as_ref())))
    } else {
        Err(InterpreterError::TypeError("ends_with requires string and suffix string".to_string()))
    }
}

pub fn builtin_replace(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 3 {
        return Err(InterpreterError::InvalidOperation("replace requires string, from, and to".to_string()));
    }
    if let (Value::String(s), Value::String(from), Value::String(to)) = (&args[0], &args[1], &args[2]) {
        let result = s.replace(from.as_ref(), to.as_ref());
        Ok(Value::String(Rc::from(result)))
    } else {
        Err(InterpreterError::TypeError("replace requires string, from string, and to string".to_string()))
    }
}

pub fn builtin_len(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("len requires string argument".to_string()));
    }
    if let Value::String(s) = &args[0] {
        Ok(Value::Number(s.len() as f64))
    } else {
        Err(InterpreterError::TypeError("len requires string".to_string()))
    }
}

pub fn builtin_keys(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("keys requires object argument".to_string()));
    }
    if let Value::Object(obj) = &args[0] {
        let obj_ref = obj.borrow();
        let keys: Vec<Value> = obj_ref.keys().map(|k| Value::String(Rc::from(k.clone()))).collect();
        Ok(Value::Array(Rc::new(RefCell::new(keys))))
    } else {
        Err(InterpreterError::TypeError("keys requires object".to_string()))
    }
}

pub fn builtin_values(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("values requires object argument".to_string()));
    }
    if let Value::Object(obj) = &args[0] {
        let obj_ref = obj.borrow();
        let values: Vec<Value> = obj_ref.values().cloned().collect();
        Ok(Value::Array(Rc::new(RefCell::new(values))))
    } else {
        Err(InterpreterError::TypeError("values requires object".to_string()))
    }
}

pub fn builtin_entries(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("entries requires object argument".to_string()));
    }
    if let Value::Object(obj) = &args[0] {
        let obj_ref = obj.borrow();
        let entries: Vec<Value> = obj_ref.iter()
            .map(|(k, v)| {
                Value::Array(Rc::new(RefCell::new(vec![
                    Value::String(Rc::from(k.clone())),
                    v.clone(),
                ])))
            })
            .collect();
        Ok(Value::Array(Rc::new(RefCell::new(entries))))
    } else {
        Err(InterpreterError::TypeError("entries requires object".to_string()))
    }
}

pub fn builtin_has(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("has requires object and key".to_string()));
    }
    if let (Value::Object(obj), Value::String(key)) = (&args[0], &args[1]) {
        let obj_ref = obj.borrow();
        Ok(Value::Bool(obj_ref.contains_key(key.as_ref())))
    } else {
        Err(InterpreterError::TypeError("has requires object and key string".to_string()))
    }
}

pub fn builtin_merge(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("merge requires two objects".to_string()));
    }
    if let (Value::Object(obj1), Value::Object(obj2)) = (&args[0], &args[1]) {
        let mut result = IndexMap::new();
        let obj1_ref = obj1.borrow();
        let obj2_ref = obj2.borrow();
        for (k, v) in obj1_ref.iter() {
            result.insert(k.clone(), v.clone());
        }
        for (k, v) in obj2_ref.iter() {
            result.insert(k.clone(), v.clone());
        }
        Ok(Value::Object(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::TypeError("merge requires two objects".to_string()))
    }
}

pub fn builtin_typeof(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("typeof requires argument".to_string()));
    }
    let type_str = match &args[0] {
        Value::Null => "null",
        Value::Bool(_) => "bool",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
        Value::Function(_) => "function",
    };
    Ok(Value::String(Rc::from(type_str)))
}

pub fn builtin_is_null(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("is_null requires argument".to_string()));
    }
    Ok(Value::Bool(matches!(args[0], Value::Null)))
}

pub fn builtin_is_array(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("is_array requires argument".to_string()));
    }
    Ok(Value::Bool(matches!(args[0], Value::Array(_))))
}

pub fn builtin_is_object(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("is_object requires argument".to_string()));
    }
    Ok(Value::Bool(matches!(args[0], Value::Object(_))))
}

pub fn builtin_is_string(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("is_string requires argument".to_string()));
    }
    Ok(Value::Bool(matches!(args[0], Value::String(_))))
}

pub fn builtin_is_number(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("is_number requires argument".to_string()));
    }
    Ok(Value::Bool(matches!(args[0], Value::Number(_))))
}

pub fn builtin_is_bool(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("is_bool requires argument".to_string()));
    }
    Ok(Value::Bool(matches!(args[0], Value::Bool(_))))
}

pub fn builtin_to_string(
    args: &[Value],
    value_to_string: impl Fn(&Value) -> String,
) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("to_string requires argument".to_string()));
    }
    Ok(Value::String(Rc::from(value_to_string(&args[0]))))
}

pub fn builtin_to_number(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("to_number requires argument".to_string()));
    }
    match &args[0] {
        Value::Number(n) => Ok(Value::Number(*n)),
        Value::String(s) => {
            s.parse::<f64>()
                .map(Value::Number)
                .map_err(|_| InterpreterError::InvalidOperation("to_number: cannot parse string as number".to_string()))
        }
        _ => Err(InterpreterError::TypeError("to_number requires number or string".to_string())),
    }
}

pub fn builtin_parse_json(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("parse_json requires string argument".to_string()));
    }
    if let Value::String(json_str) = &args[0] {
        use crate::json;
        let json_value = json::parse_json(json_str.as_ref())
            .map_err(|e| InterpreterError::InvalidOperation(format!("parse_json error: {}", e)))?;
        
        fn convert_json_value(v: &serde_json::Value) -> Value {
            match v {
                serde_json::Value::Null => Value::Null,
                serde_json::Value::Bool(b) => Value::Bool(*b),
                serde_json::Value::Number(n) => Value::Number(n.as_f64().unwrap_or(0.0)),
                serde_json::Value::String(s) => Value::String(Rc::from(s.as_str())),
                serde_json::Value::Array(arr) => {
                    let converted: Vec<Value> = arr.iter().map(convert_json_value).collect();
                    Value::Array(Rc::new(RefCell::new(converted)))
                }
                serde_json::Value::Object(obj) => {
                    let mut map = IndexMap::new();
                    for (k, v) in obj.iter() {
                        map.insert(k.clone(), convert_json_value(v));
                    }
                    Value::Object(Rc::new(RefCell::new(map)))
                }
            }
        }
        
        Ok(convert_json_value(&json_value))
    } else {
        Err(InterpreterError::TypeError("parse_json requires string".to_string()))
    }
}

pub fn builtin_floor(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("floor requires number argument".to_string()));
    }
    if let Value::Number(n) = &args[0] {
        Ok(Value::Number(n.floor()))
    } else {
        Err(InterpreterError::TypeError("floor requires number".to_string()))
    }
}

pub fn builtin_ceil(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("ceil requires number argument".to_string()));
    }
    if let Value::Number(n) = &args[0] {
        Ok(Value::Number(n.ceil()))
    } else {
        Err(InterpreterError::TypeError("ceil requires number".to_string()))
    }
}

pub fn builtin_round(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("round requires number argument".to_string()));
    }
    if let Value::Number(n) = &args[0] {
        Ok(Value::Number(n.round()))
    } else {
        Err(InterpreterError::TypeError("round requires number".to_string()))
    }
}

pub fn builtin_abs(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("abs requires number argument".to_string()));
    }
    if let Value::Number(n) = &args[0] {
        Ok(Value::Number(n.abs()))
    } else {
        Err(InterpreterError::TypeError("abs requires number".to_string()))
    }
}

pub fn builtin_sqrt(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("sqrt requires number argument".to_string()));
    }
    if let Value::Number(n) = &args[0] {
        if *n < 0.0 {
            return Err(InterpreterError::InvalidOperation("sqrt: cannot take square root of negative number".to_string()));
        }
        Ok(Value::Number(n.sqrt()))
    } else {
        Err(InterpreterError::TypeError("sqrt requires number".to_string()))
    }
}

pub fn builtin_pow(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.len() < 2 {
        return Err(InterpreterError::InvalidOperation("pow requires base and exponent".to_string()));
    }
    if let (Value::Number(base), Value::Number(exp)) = (&args[0], &args[1]) {
        Ok(Value::Number(base.powf(*exp)))
    } else {
        Err(InterpreterError::TypeError("pow requires two numbers".to_string()))
    }
}

pub fn builtin_sin(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("sin requires number argument".to_string()));
    }
    if let Value::Number(n) = &args[0] {
        Ok(Value::Number(n.sin()))
    } else {
        Err(InterpreterError::TypeError("sin requires number".to_string()))
    }
}

pub fn builtin_cos(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("cos requires number argument".to_string()));
    }
    if let Value::Number(n) = &args[0] {
        Ok(Value::Number(n.cos()))
    } else {
        Err(InterpreterError::TypeError("cos requires number".to_string()))
    }
}

pub fn builtin_tan(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Err(InterpreterError::InvalidOperation("tan requires number argument".to_string()));
    }
    if let Value::Number(n) = &args[0] {
        Ok(Value::Number(n.tan()))
    } else {
        Err(InterpreterError::TypeError("tan requires number".to_string()))
    }
}

pub fn builtin_random(_args: &[Value]) -> Result<Value, InterpreterError> {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    Ok(Value::Number(rng.r#gen::<f64>()))
}

