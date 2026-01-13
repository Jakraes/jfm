//! Array manipulation built-in functions.

use crate::value::Value;
use super::super::error::InterpreterError;
use super::{require_args, with_array};
use indexmap::IndexSet;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

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
        Value::Number(n, _) => Some(HashableValue::Number(n.to_bits())),
        Value::String(s) => Some(HashableValue::String(s.clone())),
        _ => None,
    }
}

pub fn builtin_sum(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Ok(Value::Number(0.0, false));
    }
    with_array!(args, "sum", |arr: &Rc<RefCell<Vec<Value>>>| {
        let mut has_float = false;
        let total: f64 = arr
            .borrow()
            .iter()
            .filter_map(|v| {
                if let Value::Number(n, is_float) = v {
                    has_float |= *is_float;
                    Some(*n)
                } else {
                    None
                }
            })
            .sum();
        Ok(Value::Number(total, has_float || total.fract() != 0.0))
    })
}

pub fn builtin_avg(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Ok(Value::Number(0.0, true));
    }
    with_array!(args, "avg", |arr: &Rc<RefCell<Vec<Value>>>| {
        let borrowed = arr.borrow();
        if borrowed.is_empty() {
            return Ok(Value::Number(0.0, true));
        }
        let total: f64 = borrowed
            .iter()
            .filter_map(|v| {
                if let Value::Number(n, _) = v {
                    Some(*n)
                } else {
                    None
                }
            })
            .sum();
        Ok(Value::Number(total / borrowed.len() as f64, true))
    })
}

pub fn builtin_min(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Ok(Value::Number(0.0, false));
    }
    with_array!(args, "min", |arr: &Rc<RefCell<Vec<Value>>>| {
        let mut has_float = false;
        let min = arr
            .borrow()
            .iter()
            .filter_map(|v| {
                if let Value::Number(n, is_float) = v {
                    has_float |= *is_float;
                    Some(*n)
                } else {
                    None
                }
            })
            .fold(f64::MAX, |acc, n| acc.min(n));
        Ok(Value::Number(min, has_float || min.fract() != 0.0))
    })
}

pub fn builtin_max(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() {
        return Ok(Value::Number(0.0, false));
    }
    with_array!(args, "max", |arr: &Rc<RefCell<Vec<Value>>>| {
        let mut has_float = false;
        let max = arr
            .borrow()
            .iter()
            .filter_map(|v| {
                if let Value::Number(n, is_float) = v {
                    has_float |= *is_float;
                    Some(*n)
                } else {
                    None
                }
            })
            .fold(f64::MIN, |acc, n| acc.max(n));
        Ok(Value::Number(max, has_float || max.fract() != 0.0))
    })
}

pub fn builtin_push(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "push");
    with_array!(args, "push", |arr: &Rc<RefCell<Vec<Value>>>| {
        arr.borrow_mut().push(args[1].clone());
        Ok(args[0].clone())
    })
}

pub fn builtin_unique(
    args: &[Value],
    deep_equals: impl Fn(&Value, &Value) -> bool,
) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "unique");
    with_array!(args, "unique", |arr: &Rc<RefCell<Vec<Value>>>| {
        let borrowed = arr.borrow();
        let has_complex = borrowed
            .iter()
            .any(|v| matches!(v, Value::Array(_) | Value::Object(_)));

        let result = if has_complex {
            let mut res = Vec::new();
            for v in borrowed.iter() {
                if !res.iter().any(|existing| deep_equals(existing, v)) {
                    res.push(v.clone());
                }
            }
            res
        } else {
            let mut seen: IndexSet<HashableValue> = IndexSet::new();
            borrowed
                .iter()
                .filter_map(|v| {
                    to_hashable(v).and_then(|h| {
                        if seen.insert(h) {
                            Some(v.clone())
                        } else {
                            None
                        }
                    })
                })
                .collect()
        };
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    })
}

pub fn builtin_sort_by(
    args: &[Value],
    get_field: impl Fn(&Value, &str) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "sort_by");
    if let (Value::Array(arr), Value::String(field)) = (&args[0], &args[1]) {
        arr.borrow_mut().sort_by(|a, b| {
            let (val_a, val_b) = (
                get_field(a, field).unwrap_or(Value::Null),
                get_field(b, field).unwrap_or(Value::Null),
            );
            match (val_a, val_b) {
                (Value::Number(n1, _), Value::Number(n2, _)) => {
                    n1.partial_cmp(&n2).unwrap_or(std::cmp::Ordering::Equal)
                }
                (Value::String(s1), Value::String(s2)) => s1.cmp(&s2),
                _ => std::cmp::Ordering::Equal,
            }
        });
        Ok(args[0].clone())
    } else {
        Err(InterpreterError::type_error(
            "sort_by requires array and field name string",
        ))
    }
}

pub fn builtin_group_by(
    args: &[Value],
    get_field: impl Fn(&Value, &str) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "group_by");
    if let (Value::Array(arr_rc), Value::String(field)) = (&args[0], &args[1]) {
        let mut groups: indexmap::IndexMap<String, Vec<Value>> = indexmap::IndexMap::new();
        for item in arr_rc.borrow().iter() {
            let key = match get_field(item, field).unwrap_or(Value::Null) {
                Value::String(s) => s.to_string(),
                Value::Number(n, is_float) => {
                    if is_float {
                        n.to_string()
                    } else {
                        format!("{:.0}", n)
                    }
                }
                Value::Bool(b) => b.to_string(),
                _ => "null".to_string(),
            };
            groups.entry(key).or_default().push(item.clone());
        }
        let result: indexmap::IndexMap<String, Value> = groups
            .into_iter()
            .map(|(k, v)| (k, Value::Array(Rc::new(RefCell::new(v)))))
            .collect();
        Ok(Value::Object(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::type_error(
            "group_by requires array and field name string",
        ))
    }
}

pub fn builtin_reverse(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "reverse");
    with_array!(args, "reverse", |arr: &Rc<RefCell<Vec<Value>>>| {
        let mut result: Vec<Value> = arr.borrow().iter().cloned().collect();
        result.reverse();
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    })
}

pub fn builtin_sort(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "sort");
    with_array!(args, "sort", |arr: &Rc<RefCell<Vec<Value>>>| {
        let mut result: Vec<Value> = arr.borrow().iter().cloned().collect();
        result.sort_by(|a, b| match (a, b) {
            (Value::Number(n1, _), Value::Number(n2, _)) => {
                n1.partial_cmp(n2).unwrap_or(std::cmp::Ordering::Equal)
            }
            (Value::String(s1), Value::String(s2)) => s1.cmp(s2),
            (Value::Bool(b1), Value::Bool(b2)) => b1.cmp(b2),
            _ => std::cmp::Ordering::Equal,
        });
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    })
}

pub fn builtin_slice(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "slice");
    with_array!(args, "slice", |arr: &Rc<RefCell<Vec<Value>>>| {
        let arr_ref = arr.borrow();
        let len = arr_ref.len();

        let parse_index = |idx: f64| -> usize {
            let i = idx as i64;
            if i < 0 {
                (len as i64 + i).max(0) as usize
            } else {
                (i as usize).min(len)
            }
        };

        let start = args
            .get(1)
            .map(|v| match v {
                Value::Number(n, _) => Ok(parse_index(*n)),
                _ => Err(InterpreterError::type_error("slice index must be number")),
            })
            .transpose()?
            .unwrap_or(0);

        let end = args
            .get(2)
            .map(|v| match v {
                Value::Number(n, _) => Ok(parse_index(*n)),
                _ => Err(InterpreterError::type_error("slice index must be number")),
            })
            .transpose()?
            .unwrap_or(len);

        let result: Vec<Value> = arr_ref
            .iter()
            .skip(start)
            .take(end.saturating_sub(start))
            .cloned()
            .collect();
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    })
}

pub fn builtin_pop(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "pop");
    with_array!(args, "pop", |arr: &Rc<RefCell<Vec<Value>>>| {
        Ok(arr.borrow_mut().pop().unwrap_or(Value::Null))
    })
}

pub fn builtin_shift(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "shift");
    with_array!(args, "shift", |arr: &Rc<RefCell<Vec<Value>>>| {
        let mut arr_ref = arr.borrow_mut();
        Ok(if arr_ref.is_empty() {
            Value::Null
        } else {
            arr_ref.remove(0)
        })
    })
}

fn flatten_recursive(arr: &[Value], depth: usize, current: usize) -> Vec<Value> {
    if current >= depth {
        return arr.to_vec();
    }
    arr.iter()
        .flat_map(|item| match item {
            Value::Array(nested) => flatten_recursive(&nested.borrow(), depth, current + 1),
            _ => vec![item.clone()],
        })
        .collect()
}

pub fn builtin_flat(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "flat");
    with_array!(args, "flat", |arr: &Rc<RefCell<Vec<Value>>>| {
        let depth = args
            .get(1)
            .map(|v| match v {
                Value::Number(n, _) => Ok(*n as usize),
                _ => Err(InterpreterError::type_error("flat depth must be number")),
            })
            .transpose()?
            .unwrap_or(1);
        Ok(Value::Array(Rc::new(RefCell::new(flatten_recursive(
            &arr.borrow(),
            depth,
            0,
        )))))
    })
}

pub fn builtin_find(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "find");
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        for item in arr.borrow().iter() {
            if let Value::Bool(true) = call_fn(&args[1], &[item.clone()])? {
                return Ok(item.clone());
            }
        }
        Ok(Value::Null)
    } else {
        Err(InterpreterError::type_error(
            "find requires array and function",
        ))
    }
}

pub fn builtin_find_index(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "find_index");
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        for (i, item) in arr.borrow().iter().enumerate() {
            if let Value::Bool(true) = call_fn(&args[1], &[item.clone()])? {
                return Ok(Value::Number(i as f64, false));
            }
        }
        Ok(Value::Number(-1.0, false))
    } else {
        Err(InterpreterError::type_error(
            "find_index requires array and function",
        ))
    }
}

pub fn builtin_reduce(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 3, "reduce");
    if let (Value::Array(arr), Value::Function(_), initial) = (&args[0], &args[1], &args[2]) {
        let mut acc = initial.clone();
        for item in arr.borrow().iter() {
            acc = call_fn(&args[1], &[acc, item.clone()])?;
        }
        Ok(acc)
    } else {
        Err(InterpreterError::type_error(
            "reduce requires array, function, and initial value",
        ))
    }
}

pub fn builtin_every(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "every");
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        for item in arr.borrow().iter() {
            if let Value::Bool(false) = call_fn(&args[1], &[item.clone()])? {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    } else {
        Err(InterpreterError::type_error(
            "every requires array and function",
        ))
    }
}

pub fn builtin_some(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "some");
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        for item in arr.borrow().iter() {
            if let Value::Bool(true) = call_fn(&args[1], &[item.clone()])? {
                return Ok(Value::Bool(true));
            }
        }
        Ok(Value::Bool(false))
    } else {
        Err(InterpreterError::type_error(
            "some requires array and function",
        ))
    }
}

pub fn builtin_zip(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "zip");
    if let (Value::Array(a1), Value::Array(a2)) = (&args[0], &args[1]) {
        let (r1, r2) = (a1.borrow(), a2.borrow());
        let result: Vec<Value> = r1
            .iter()
            .zip(r2.iter())
            .map(|(x, y)| Value::Array(Rc::new(RefCell::new(vec![x.clone(), y.clone()]))))
            .collect();
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::type_error("zip requires two arrays"))
    }
}

pub fn builtin_first(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "first");
    with_array!(args, "first", |arr: &Rc<RefCell<Vec<Value>>>| {
        Ok(arr.borrow().first().cloned().unwrap_or(Value::Null))
    })
}

pub fn builtin_last(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "last");
    with_array!(args, "last", |arr: &Rc<RefCell<Vec<Value>>>| {
        Ok(arr.borrow().last().cloned().unwrap_or(Value::Null))
    })
}

pub fn builtin_enumerate(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "enumerate");
    with_array!(args, "enumerate", |arr: &Rc<RefCell<Vec<Value>>>| {
        let items = arr.borrow();
        let result: Vec<Value> = items
            .iter()
            .enumerate()
            .map(|(i, v)| {
                Value::Array(Rc::new(RefCell::new(vec![
                    Value::Number(i as f64, false),
                    v.clone(),
                ])))
            })
            .collect();
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    })
}
