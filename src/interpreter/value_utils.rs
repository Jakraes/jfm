use crate::value::Value;

pub fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Number(a, _), Value::Number(b, _)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        _ => false,
    }
}

pub fn deep_equals(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Number(a, _), Value::Number(b, _)) => a.to_bits() == b.to_bits(),
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Array(arr_a), Value::Array(arr_b)) => {
            let arr_a_ref = arr_a.borrow();
            let arr_b_ref = arr_b.borrow();
            if arr_a_ref.len() != arr_b_ref.len() {
                return false;
            }
            arr_a_ref
                .iter()
                .zip(arr_b_ref.iter())
                .all(|(va, vb)| deep_equals(va, vb))
        }
        (Value::Object(obj_a), Value::Object(obj_b)) => {
            let obj_a_ref = obj_a.borrow();
            let obj_b_ref = obj_b.borrow();
            if obj_a_ref.len() != obj_b_ref.len() {
                return false;
            }
            obj_a_ref.iter().all(|(k, va)| {
                if let Some(vb) = obj_b_ref.get(k) {
                    deep_equals(va, vb)
                } else {
                    false
                }
            })
        }
        _ => false,
    }
}

pub fn value_to_string(val: &Value) -> String {
    match val {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Number(n, is_float) => {
            if *is_float || n.fract() != 0.0 {
                n.to_string()
            } else {
                format!("{:.0}", n)
            }
        }
        Value::String(s) => s.to_string(),
        Value::Array(arr) => {
            let items: Vec<String> = arr.borrow().iter().map(|v| value_to_display(v)).collect();
            format!("[{}]", items.join(", "))
        }
        Value::Object(obj) => {
            let fields: Vec<String> = obj.borrow().iter()
                .map(|(k, v)| format!("\"{}\": {}", k, value_to_display(v)))
                .collect();
            format!("{{{}}}", fields.join(", "))
        }
        Value::Function(_) => "<function>".to_string(),
        Value::Module(m) => format!("<module:{}>", m.name),
    }
}

pub fn value_to_display(val: &Value) -> String {
    match val {
        Value::String(s) => format!("\"{}\"", s),
        _ => value_to_string(val),
    }
}
