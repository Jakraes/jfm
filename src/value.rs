use indexmap::IndexMap;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

use crate::ast::Stmt;

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Number(f64, bool),
    String(Rc<str>),
    Array(Rc<RefCell<Vec<Value>>>),
    Object(Rc<RefCell<IndexMap<String, Value>>>),
    Function(Rc<Function>),
    Module(Rc<Module>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(left_bool), Value::Bool(right_bool)) => left_bool == right_bool,
            (Value::Number(left_num, _), Value::Number(right_num, _)) => left_num == right_num,
            (Value::String(left_str), Value::String(right_str)) => left_str == right_str,
            (Value::Array(left_arr), Value::Array(right_arr)) => left_arr == right_arr,
            (Value::Object(left_obj), Value::Object(right_obj)) => left_obj == right_obj,
            (Value::Function(left_fn), Value::Function(right_fn)) => Rc::ptr_eq(left_fn, right_fn),
            (Value::Module(left_mod), Value::Module(right_mod)) => Rc::ptr_eq(left_mod, right_mod),
            _ => false,
        }
    }
}

impl Value {
    pub fn as_object(&self) -> Option<Ref<'_, IndexMap<String, Value>>> {
        if let Value::Object(object) = self {
            Some(object.borrow())
        } else {
            None
        }
    }

    pub fn as_array(&self) -> Option<Ref<'_, Vec<Value>>> {
        if let Value::Array(array) = self {
            Some(array.borrow())
        } else {
            None
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        if let Value::Number(numeric_value, _) = self {
            Some(*numeric_value)
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        if let Value::String(string_ref) = self {
            Some(string_ref.as_ref())
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let Value::Bool(bool_value) = self {
            Some(*bool_value)
        } else {
            None
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Null => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<crate::ast::Param>,
    pub body_expr: Option<Box<crate::ast::Expr>>,
    pub body_stmts: Option<Vec<Stmt>>,
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub exports: IndexMap<String, Value>,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            exports: IndexMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.exports.get(name)
    }
}

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
            if obj_b_ref.len() != obj_a_ref.len() {
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
