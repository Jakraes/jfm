use crate::lexer::Value;
use super::error::InterpreterError;
use indexmap::{IndexMap, IndexSet};
use std::cell::RefCell;
use std::rc::Rc;
use std::hash::{Hash, Hasher};

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

macro_rules! unary_math {
    ($name:ident, $op:ident) => {
        pub fn $name(args: &[Value]) -> Result<Value, InterpreterError> {
            require_args!(args, 1, stringify!($op));
            with_number!(args, stringify!($op), |n: f64| Ok(Value::Number(n.$op(), true)))
        }
    };
}

macro_rules! type_check {
    ($name:ident, $pattern:pat) => {
        pub fn $name(args: &[Value]) -> Result<Value, InterpreterError> {
            require_args!(args, 1, stringify!($name));
            Ok(Value::Bool(matches!(args[0], $pattern)))
        }
    };
}

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

pub fn builtin_count(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() { return Ok(Value::Number(0.0, false)); }
    with_array!(args, "count", |arr: &Rc<RefCell<Vec<Value>>>| {
        Ok(Value::Number(arr.borrow().len() as f64, false))
    })
}

pub fn builtin_sum(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() { return Ok(Value::Number(0.0, false)); }
    with_array!(args, "sum", |arr: &Rc<RefCell<Vec<Value>>>| {
        let mut has_float = false;
        let total: f64 = arr.borrow().iter()
            .filter_map(|v| if let Value::Number(n, is_float) = v { has_float |= *is_float; Some(*n) } else { None })
            .sum();
        Ok(Value::Number(total, has_float || total.fract() != 0.0))
    })
}

pub fn builtin_avg(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() { return Ok(Value::Number(0.0, true)); }
    with_array!(args, "avg", |arr: &Rc<RefCell<Vec<Value>>>| {
        let borrowed = arr.borrow();
        if borrowed.is_empty() { return Ok(Value::Number(0.0, true)); }
        let total: f64 = borrowed.iter()
            .filter_map(|v| if let Value::Number(n, _) = v { Some(*n) } else { None })
            .sum();
        Ok(Value::Number(total / borrowed.len() as f64, true))
    })
}

pub fn builtin_min(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() { return Ok(Value::Number(0.0, false)); }
    with_array!(args, "min", |arr: &Rc<RefCell<Vec<Value>>>| {
        let mut has_float = false;
        let min = arr.borrow().iter()
            .filter_map(|v| if let Value::Number(n, is_float) = v { has_float |= *is_float; Some(*n) } else { None })
            .fold(f64::MAX, |acc, n| acc.min(n));
        Ok(Value::Number(min, has_float || min.fract() != 0.0))
    })
}

pub fn builtin_max(args: &[Value]) -> Result<Value, InterpreterError> {
    if args.is_empty() { return Ok(Value::Number(0.0, false)); }
    with_array!(args, "max", |arr: &Rc<RefCell<Vec<Value>>>| {
        let mut has_float = false;
        let max = arr.borrow().iter()
            .filter_map(|v| if let Value::Number(n, is_float) = v { has_float |= *is_float; Some(*n) } else { None })
            .fold(f64::MIN, |acc, n| acc.max(n));
        Ok(Value::Number(max, has_float || max.fract() != 0.0))
    })
}

pub fn builtin_take(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "take");
    if let (Value::Array(arr), Value::Number(n, _)) = (&args[0], &args[1]) {
        let result: Vec<Value> = arr.borrow().iter().take(*n as usize).cloned().collect();
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::type_error("take requires array and number"))
    }
}

pub fn builtin_push(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "push");
    with_array!(args, "push", |arr: &Rc<RefCell<Vec<Value>>>| {
        arr.borrow_mut().push(args[1].clone());
        Ok(args[0].clone())
    })
}

pub fn builtin_print(args: &[Value], value_to_string: impl Fn(&Value) -> String) -> Result<Value, InterpreterError> {
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
    io::stdin().read_line(&mut input)
        .map_err(|e| InterpreterError::invalid_operation(format!("Failed to read input: {}", e)))?;
    Ok(Value::String(Rc::from(input.trim_end_matches(['\n', '\r']))))
}

pub fn builtin_unique(
    args: &[Value],
    deep_equals: impl Fn(&Value, &Value) -> bool,
) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "unique");
    with_array!(args, "unique", |arr: &Rc<RefCell<Vec<Value>>>| {
        let borrowed = arr.borrow();
        let has_complex = borrowed.iter().any(|v| matches!(v, Value::Array(_) | Value::Object(_)));
        
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
            borrowed.iter().filter_map(|v| {
                to_hashable(v).and_then(|h| if seen.insert(h) { Some(v.clone()) } else { None })
            }).collect()
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
            let (val_a, val_b) = (get_field(a, field).unwrap_or(Value::Null), get_field(b, field).unwrap_or(Value::Null));
            match (val_a, val_b) {
                (Value::Number(n1, _), Value::Number(n2, _)) => n1.partial_cmp(&n2).unwrap_or(std::cmp::Ordering::Equal),
                (Value::String(s1), Value::String(s2)) => s1.cmp(&s2),
                _ => std::cmp::Ordering::Equal,
            }
        });
        Ok(args[0].clone())
    } else {
        Err(InterpreterError::type_error("sort_by requires array and field name string"))
    }
}

pub fn builtin_group_by(
    args: &[Value],
    get_field: impl Fn(&Value, &str) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "group_by");
    if let (Value::Array(arr_rc), Value::String(field)) = (&args[0], &args[1]) {
        let mut groups: IndexMap<String, Vec<Value>> = IndexMap::new();
        for item in arr_rc.borrow().iter() {
            let key = match get_field(item, field).unwrap_or(Value::Null) {
                Value::String(s) => s.to_string(),
                Value::Number(n, is_float) => if is_float { n.to_string() } else { format!("{:.0}", n) },
                Value::Bool(b) => b.to_string(),
                _ => "null".to_string(),
            };
            groups.entry(key).or_default().push(item.clone());
        }
        let result: IndexMap<String, Value> = groups.into_iter()
            .map(|(k, v)| (k, Value::Array(Rc::new(RefCell::new(v)))))
            .collect();
        Ok(Value::Object(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::type_error("group_by requires array and field name string"))
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
            (Value::Number(n1, _), Value::Number(n2, _)) => n1.partial_cmp(n2).unwrap_or(std::cmp::Ordering::Equal),
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
            if i < 0 { (len as i64 + i).max(0) as usize } else { (i as usize).min(len) }
        };
        
        let start = args.get(1).map(|v| match v {
            Value::Number(n, _) => Ok(parse_index(*n)),
            _ => Err(InterpreterError::type_error("slice index must be number")),
        }).transpose()?.unwrap_or(0);
        
        let end = args.get(2).map(|v| match v {
            Value::Number(n, _) => Ok(parse_index(*n)),
            _ => Err(InterpreterError::type_error("slice index must be number")),
        }).transpose()?.unwrap_or(len);
        
        let result: Vec<Value> = arr_ref.iter().skip(start).take(end.saturating_sub(start)).cloned().collect();
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
        Ok(if arr_ref.is_empty() { Value::Null } else { arr_ref.remove(0) })
    })
}

fn flatten_recursive(arr: &[Value], depth: usize, current: usize) -> Vec<Value> {
    if current >= depth { return arr.to_vec(); }
    arr.iter().flat_map(|item| match item {
        Value::Array(nested) => flatten_recursive(&nested.borrow(), depth, current + 1),
        _ => vec![item.clone()],
    }).collect()
}

pub fn builtin_flat(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "flat");
    with_array!(args, "flat", |arr: &Rc<RefCell<Vec<Value>>>| {
        let depth = args.get(1).map(|v| match v {
            Value::Number(n, _) => Ok(*n as usize),
            _ => Err(InterpreterError::type_error("flat depth must be number")),
        }).transpose()?.unwrap_or(1);
        Ok(Value::Array(Rc::new(RefCell::new(flatten_recursive(&arr.borrow(), depth, 0)))))
    })
}

pub fn builtin_flatten(args: &[Value]) -> Result<Value, InterpreterError> { builtin_flat(args) }

pub fn builtin_map(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "map");
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        let mut result = Vec::new();
        for item in arr.borrow().iter() {
            result.push(call_fn(&args[1], &[item.clone()])?);
        }
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::type_error("map requires array and function"))
    }
}

pub fn builtin_filter(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "filter");
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        let mut result = Vec::new();
        for item in arr.borrow().iter() {
            if let Value::Bool(true) = call_fn(&args[1], &[item.clone()])? {
                result.push(item.clone());
            }
        }
        Ok(Value::Array(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::type_error("filter requires array and function"))
    }
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
        Err(InterpreterError::type_error("find requires array and function"))
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
        Err(InterpreterError::type_error("find_index requires array and function"))
    }
}

pub fn builtin_reduce(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 3, "reduce");
    if let (Value::Array(arr), Value::Function(_), initial) = (&args[0], &args[1], &args[2]) {
        let mut acc = initial.clone();
        for item in arr.borrow().iter() { acc = call_fn(&args[1], &[acc, item.clone()])?; }
        Ok(acc)
    } else {
        Err(InterpreterError::type_error("reduce requires array, function, and initial value"))
    }
}

pub fn builtin_every(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "every");
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        for item in arr.borrow().iter() {
            if let Value::Bool(false) = call_fn(&args[1], &[item.clone()])? { return Ok(Value::Bool(false)); }
        }
        Ok(Value::Bool(true))
    } else {
        Err(InterpreterError::type_error("every requires array and function"))
    }
}

pub fn builtin_some(
    args: &[Value],
    mut call_fn: impl FnMut(&Value, &[Value]) -> Result<Value, InterpreterError>,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "some");
    if let (Value::Array(arr), Value::Function(_)) = (&args[0], &args[1]) {
        for item in arr.borrow().iter() {
            if let Value::Bool(true) = call_fn(&args[1], &[item.clone()])? { return Ok(Value::Bool(true)); }
        }
        Ok(Value::Bool(false))
    } else {
        Err(InterpreterError::type_error("some requires array and function"))
    }
}

pub fn builtin_zip(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "zip");
    if let (Value::Array(a1), Value::Array(a2)) = (&args[0], &args[1]) {
        let (r1, r2) = (a1.borrow(), a2.borrow());
        let result: Vec<Value> = r1.iter().zip(r2.iter())
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

pub fn builtin_split(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "split");
    if let (Value::String(s), Value::String(delim)) = (&args[0], &args[1]) {
        let parts: Vec<Value> = s.split(delim.as_ref()).map(|p| Value::String(Rc::from(p))).collect();
        Ok(Value::Array(Rc::new(RefCell::new(parts))))
    } else {
        Err(InterpreterError::type_error("split requires string and delimiter"))
    }
}

pub fn builtin_join(
    args: &[Value],
    value_to_string: impl Fn(&Value) -> String,
) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "join");
    if let (Value::Array(arr), Value::String(delim)) = (&args[0], &args[1]) {
        let parts: Vec<String> = arr.borrow().iter().map(|v| value_to_string(v)).collect();
        Ok(Value::String(Rc::from(parts.join(delim.as_ref()))))
    } else {
        Err(InterpreterError::type_error("join requires array and delimiter"))
    }
}

pub fn builtin_trim(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "trim");
    with_string!(args, "trim", |s: &Rc<str>| Ok(Value::String(Rc::from(s.trim()))))
}

pub fn builtin_upper(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "upper");
    with_string!(args, "upper", |s: &Rc<str>| Ok(Value::String(Rc::from(s.to_uppercase()))))
}

pub fn builtin_lower(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "lower");
    with_string!(args, "lower", |s: &Rc<str>| Ok(Value::String(Rc::from(s.to_lowercase()))))
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
        Err(InterpreterError::type_error("starts_with requires two strings"))
    }
}

pub fn builtin_ends_with(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "ends_with");
    if let (Value::String(s), Value::String(suffix)) = (&args[0], &args[1]) {
        Ok(Value::Bool(s.ends_with(suffix.as_ref())))
    } else {
        Err(InterpreterError::type_error("ends_with requires two strings"))
    }
}

pub fn builtin_replace(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 3, "replace");
    if let (Value::String(s), Value::String(from), Value::String(to)) = (&args[0], &args[1], &args[2]) {
        Ok(Value::String(Rc::from(s.replace(from.as_ref(), to.as_ref()))))
    } else {
        Err(InterpreterError::type_error("replace requires three strings"))
    }
}

pub fn builtin_len(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "len");
    with_string!(args, "len", |s: &Rc<str>| Ok(Value::Number(s.len() as f64, false)))
}

pub fn builtin_keys(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "keys");
    if let Value::Object(obj) = &args[0] {
        let keys: Vec<Value> = obj.borrow().keys().map(|k| Value::String(Rc::from(k.clone()))).collect();
        Ok(Value::Array(Rc::new(RefCell::new(keys))))
    } else {
        Err(InterpreterError::type_error("keys requires object"))
    }
}

pub fn builtin_values(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "values");
    if let Value::Object(obj) = &args[0] {
        let values: Vec<Value> = obj.borrow().values().cloned().collect();
        Ok(Value::Array(Rc::new(RefCell::new(values))))
    } else {
        Err(InterpreterError::type_error("values requires object"))
    }
}

pub fn builtin_entries(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "entries");
    if let Value::Object(obj) = &args[0] {
        let entries: Vec<Value> = obj.borrow().iter()
            .map(|(k, v)| Value::Array(Rc::new(RefCell::new(vec![Value::String(Rc::from(k.clone())), v.clone()]))))
            .collect();
        Ok(Value::Array(Rc::new(RefCell::new(entries))))
    } else {
        Err(InterpreterError::type_error("entries requires object"))
    }
}

pub fn builtin_has(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "has");
    if let (Value::Object(obj), Value::String(key)) = (&args[0], &args[1]) {
        Ok(Value::Bool(obj.borrow().contains_key(key.as_ref())))
    } else {
        Err(InterpreterError::type_error("has requires object and key"))
    }
}

pub fn builtin_merge(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "merge");
    if let (Value::Object(o1), Value::Object(o2)) = (&args[0], &args[1]) {
        let mut result = IndexMap::new();
        for (k, v) in o1.borrow().iter() { result.insert(k.clone(), v.clone()); }
        for (k, v) in o2.borrow().iter() { result.insert(k.clone(), v.clone()); }
        Ok(Value::Object(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::type_error("merge requires two objects"))
    }
}

pub fn builtin_typeof(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "typeof");
    Ok(Value::String(Rc::from(match &args[0] {
        Value::Null => "null", Value::Bool(_) => "bool", Value::Number(_, _) => "number",
        Value::String(_) => "string", Value::Array(_) => "array", Value::Object(_) => "object",
        Value::Function(_) => "function",
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
        Value::String(s) => s.parse::<f64>().map(|n| Value::Number(n, s.contains('.')))
            .map_err(|_| InterpreterError::invalid_operation("to_number: invalid string")),
        _ => Err(InterpreterError::type_error("to_number requires number or string")),
    }
}

pub fn builtin_to_int(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "to_int");
    match &args[0] {
        Value::Number(n, _) => Ok(Value::Number(n.trunc(), false)),
        Value::String(s) => s.parse::<f64>()
            .map(|n| Value::Number(n.trunc(), false))
            .map_err(|_| InterpreterError::invalid_operation("to_int: invalid string")),
        Value::Bool(b) => Ok(Value::Number(if *b { 1.0 } else { 0.0 }, false)),
        _ => Err(InterpreterError::type_error("to_int requires number, string, or bool")),
    }
}

pub fn builtin_to_float(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "to_float");
    match &args[0] {
        Value::Number(n, _) => Ok(Value::Number(*n, true)),
        Value::String(s) => s.parse::<f64>()
            .map(|n| Value::Number(n, true))
            .map_err(|_| InterpreterError::invalid_operation("to_float: invalid string")),
        Value::Bool(b) => Ok(Value::Number(if *b { 1.0 } else { 0.0 }, true)),
        _ => Err(InterpreterError::type_error("to_float requires number, string, or bool")),
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
    }))
}

pub fn builtin_parse_json(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "parse_json");
    with_string!(args, "parse_json", |s: &Rc<str>| {
        use crate::json;
        fn convert(v: &serde_json::Value) -> Value {
            match v {
                serde_json::Value::Null => Value::Null,
                serde_json::Value::Bool(b) => Value::Bool(*b),
                serde_json::Value::Number(n) => Value::Number(n.as_f64().unwrap_or(0.0), n.is_f64()),
                serde_json::Value::String(s) => Value::String(Rc::from(s.as_str())),
                serde_json::Value::Array(a) => Value::Array(Rc::new(RefCell::new(a.iter().map(convert).collect()))),
                serde_json::Value::Object(o) => Value::Object(Rc::new(RefCell::new(o.iter().map(|(k, v)| (k.clone(), convert(v))).collect()))),
            }
        }
        json::parse_json(s.as_ref()).map(|v| convert(&v))
            .map_err(|e| InterpreterError::invalid_operation(format!("parse_json: {}", e)))
    })
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
        if n < 0.0 { Err(InterpreterError::invalid_operation("sqrt: negative number")) }
        else { Ok(Value::Number(n.sqrt(), true)) }
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

