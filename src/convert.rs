use crate::Value;
use std::cell::RefCell;
use std::rc::Rc;

pub fn json_to_value(json_val: serde_json::Value) -> Value {
    match json_val {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Bool(b),
        serde_json::Value::Number(json_number) => {
            let numeric_value = json_number.as_f64().unwrap_or(0.0);
            let number_string = json_number.to_string();
            let is_float = number_string.contains('.')
                || number_string.contains('e')
                || number_string.contains('E');
            Value::Number(numeric_value, is_float)
        }
        serde_json::Value::String(s) => Value::String(Rc::from(s.as_str())),
        serde_json::Value::Array(array) => {
            let items: Vec<Value> = array.into_iter().map(json_to_value).collect();
            Value::Array(Rc::new(RefCell::new(items)))
        }
        serde_json::Value::Object(object) => {
            let mut map = indexmap::IndexMap::new();
            for (k, v) in object {
                map.insert(k, json_to_value(v));
            }
            Value::Object(Rc::new(RefCell::new(map)))
        }
    }
}
