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
    pub params: Vec<Rc<str>>,
    pub body_expr: Option<Box<crate::ast::Expr>>,
    pub body_stmts: Option<Vec<Stmt>>,
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
