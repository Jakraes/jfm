use crate::value::Value;
use super::super::error::InterpreterError;
use super::require_args;
use indexmap::IndexMap;
use std::cell::RefCell;
use std::rc::Rc;

pub fn builtin_keys(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "keys");
    if let Value::Object(obj) = &args[0] {
        let keys: Vec<Value> = obj
            .borrow()
            .keys()
            .map(|k| Value::String(Rc::from(k.clone())))
            .collect();
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
        let entries: Vec<Value> = obj
            .borrow()
            .iter()
            .map(|(k, v)| {
                Value::Array(Rc::new(RefCell::new(vec![
                    Value::String(Rc::from(k.clone())),
                    v.clone(),
                ])))
            })
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
        for (k, v) in o1.borrow().iter() {
            result.insert(k.clone(), v.clone());
        }
        for (k, v) in o2.borrow().iter() {
            result.insert(k.clone(), v.clone());
        }
        Ok(Value::Object(Rc::new(RefCell::new(result))))
    } else {
        Err(InterpreterError::type_error("merge requires two objects"))
    }
}

pub fn builtin_deep_merge(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 2, "deep_merge");

    fn deep_merge_impl(base: &Value, overlay: &Value) -> Value {
        match (base, overlay) {
            (Value::Object(base_obj), Value::Object(overlay_obj)) => {
                let mut result = base_obj.borrow().clone();
                for (key, overlay_val) in overlay_obj.borrow().iter() {
                    if let Some(base_val) = result.get(key).cloned() {
                        result.insert(key.clone(), deep_merge_impl(&base_val, overlay_val));
                    } else {
                        result.insert(key.clone(), overlay_val.clone());
                    }
                }
                Value::Object(Rc::new(RefCell::new(result)))
            }
            (_, overlay) => overlay.clone(),
        }
    }

    Ok(deep_merge_impl(&args[0], &args[1]))
}

pub fn builtin_set_path(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 3, "set_path");

    let path = match &args[1] {
        Value::String(s) => s.as_ref(),
        _ => return Err(InterpreterError::type_error("set_path requires string path")),
    };

    let parts: Vec<&str> = path.split('.').collect();

    fn set_recursive(obj: &Value, parts: &[&str], value: &Value) -> Value {
        if parts.is_empty() {
            return value.clone();
        }

        let key = parts[0];
        let remaining = &parts[1..];

        match obj {
            Value::Object(map_rc) => {
                let mut map = map_rc.borrow().clone();
                let inner = map
                    .get(key)
                    .cloned()
                    .unwrap_or(Value::Object(Rc::new(RefCell::new(IndexMap::new()))));
                map.insert(key.to_string(), set_recursive(&inner, remaining, value));
                Value::Object(Rc::new(RefCell::new(map)))
            }
            _ => {
                let mut map = IndexMap::new();
                let inner = Value::Object(Rc::new(RefCell::new(IndexMap::new())));
                map.insert(key.to_string(), set_recursive(&inner, remaining, value));
                Value::Object(Rc::new(RefCell::new(map)))
            }
        }
    }

    Ok(set_recursive(&args[0], &parts, &args[2]))
}

/// Deep clone a Value, recursively cloning nested arrays and objects
pub fn deep_clone(value: &Value) -> Value {
    match value {
        Value::Array(arr) => {
            let cloned: Vec<Value> = arr.borrow().iter().map(deep_clone).collect();
            Value::Array(Rc::new(RefCell::new(cloned)))
        }
        Value::Object(obj) => {
            let cloned: IndexMap<String, Value> = obj
                .borrow()
                .iter()
                .map(|(k, v)| (k.clone(), deep_clone(v)))
                .collect();
            Value::Object(Rc::new(RefCell::new(cloned)))
        }
        other => other.clone(),
    }
}

pub fn builtin_clone(args: &[Value]) -> Result<Value, InterpreterError> {
    require_args!(args, 1, "clone");
    Ok(deep_clone(&args[0]))
}
