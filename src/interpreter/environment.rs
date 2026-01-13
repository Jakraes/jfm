use crate::lexer::Value;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct Environment {
    variables: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    pub fn with_parent(parent: Rc<Environment>) -> Self {
        Self {
            variables: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(parent),
        }
    }

    pub fn set(&self, name: String, value: Value) {
        self.variables.borrow_mut().insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.variables.borrow().get(name) {
            Some(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn update(&self, name: &str, value: Value) -> bool {
        if self.variables.borrow().contains_key(name) {
            self.variables.borrow_mut().insert(name.to_string(), value);
            true
        } else if let Some(parent) = &self.parent {
            parent.update(name, value)
        } else {
            false
        }
    }
}

