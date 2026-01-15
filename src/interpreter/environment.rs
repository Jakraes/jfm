use crate::value::Value;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type Scope = HashMap<String, Value>;

#[derive(Debug, Clone)]
pub struct Environment {
    scopes: Rc<RefCell<Vec<Scope>>>,
    consts: Rc<RefCell<HashSet<String>>>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut scopes = Vec::with_capacity(8);
        scopes.push(HashMap::new());
        Self {
            scopes: Rc::new(RefCell::new(scopes)),
            consts: Rc::new(RefCell::new(HashSet::new())),
            parent: None,
        }
    }

    pub fn push_scope(&self) {
        self.scopes.borrow_mut().push(HashMap::new());
    }

    pub fn pop_scope(&self) {
        let mut scopes = self.scopes.borrow_mut();
        if scopes.len() > 1 {
            scopes.pop();
        }
    }

    pub fn set(&self, name: String, value: Value) {
        let mut scopes = self.scopes.borrow_mut();
        if let Some(current_scope) = scopes.last_mut() {
            current_scope.insert(name, value);
        }
    }

    pub fn set_const(&self, name: String, value: Value) {
        self.consts.borrow_mut().insert(name.clone());
        self.set(name, value);
    }

    pub fn is_const(&self, name: &str) -> bool {
        if self.consts.borrow().contains(name) {
            return true;
        }
        if let Some(parent) = &self.parent {
            parent.is_const(name)
        } else {
            false
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let scopes = self.scopes.borrow();
        
        for scope in scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        
        if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    /// Updates an existing variable. Returns:
    /// - Ok(true) if updated successfully
    /// - Ok(false) if variable not found
    /// - Err(()) if variable is const and cannot be reassigned
    pub fn update(&self, name: &str, value: Value) -> Result<bool, ()> {
        // Check if variable is const before updating
        if self.is_const(name) {
            return Err(());
        }
        
        let mut scopes = self.scopes.borrow_mut();
        
        for scope in scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return Ok(true);
            }
        }
        
        if let Some(parent) = &self.parent {
            parent.update(name, value)
        } else {
            Ok(false)
        }
    }

    pub fn get_all_bindings(&self) -> Vec<(String, Value)> {
        let scopes = self.scopes.borrow();
        let mut result = HashMap::new();
        
        for scope in scopes.iter() {
            for (name, value) in scope.iter() {
                result.insert(name.clone(), value.clone());
            }
        }
        
        result.into_iter().collect()
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_set_get() {
        let env = Environment::new();
        env.set("x".to_string(), Value::Number(42.0, false));
        assert_eq!(env.get("x"), Some(Value::Number(42.0, false)));
    }

    #[test]
    fn test_scope_push_pop() {
        let env = Environment::new();
        env.set("x".to_string(), Value::Number(1.0, false));
        
        env.push_scope();
        env.set("x".to_string(), Value::Number(2.0, false));
        assert_eq!(env.get("x"), Some(Value::Number(2.0, false)));
        
        env.pop_scope();
        assert_eq!(env.get("x"), Some(Value::Number(1.0, false)));
    }

    #[test]
    fn test_update_in_outer_scope() {
        let env = Environment::new();
        env.set("x".to_string(), Value::Number(1.0, false));
        
        env.push_scope();
        assert!(env.update("x", Value::Number(2.0, false)).unwrap());
        
        env.pop_scope();
        assert_eq!(env.get("x"), Some(Value::Number(2.0, false)));
    }

    #[test]
    fn test_const_cannot_be_reassigned() {
        let env = Environment::new();
        env.set_const("x".to_string(), Value::Number(1.0, false));
        
        // Attempting to update a const should return Err
        assert!(env.update("x", Value::Number(2.0, false)).is_err());
        
        // Value should remain unchanged
        assert_eq!(env.get("x"), Some(Value::Number(1.0, false)));
    }

    #[test]
    fn test_is_const() {
        let env = Environment::new();
        env.set("x".to_string(), Value::Number(1.0, false));
        env.set_const("y".to_string(), Value::Number(2.0, false));
        
        assert!(!env.is_const("x"));
        assert!(env.is_const("y"));
    }
}
