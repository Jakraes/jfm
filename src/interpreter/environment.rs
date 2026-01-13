use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Represents a single scope level in the environment.
type Scope = HashMap<String, Value>;

/// Environment manages variable scopes with efficient push/pop semantics.
/// 
/// Uses a stack of scopes rather than creating new Environment objects,
/// reducing allocations during loop iterations and function calls.
#[derive(Debug, Clone)]
pub struct Environment {
    scopes: Rc<RefCell<Vec<Scope>>>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut scopes = Vec::with_capacity(8);
        scopes.push(HashMap::new());
        Self {
            scopes: Rc::new(RefCell::new(scopes)),
            parent: None,
        }
    }

    pub fn with_parent(parent: Rc<Environment>) -> Self {
        let mut scopes = Vec::with_capacity(4);
        scopes.push(HashMap::new());
        Self {
            scopes: Rc::new(RefCell::new(scopes)),
            parent: Some(parent),
        }
    }

    /// Push a new scope onto the stack. Use this at the start of a loop iteration
    /// or block scope instead of creating a new Environment.
    pub fn push_scope(&self) {
        self.scopes.borrow_mut().push(HashMap::new());
    }

    /// Pop the current scope from the stack. Use this at the end of a loop iteration
    /// or block scope.
    pub fn pop_scope(&self) {
        let mut scopes = self.scopes.borrow_mut();
        if scopes.len() > 1 {
            scopes.pop();
        }
    }

    /// Set a variable in the current (topmost) scope.
    pub fn set(&self, name: String, value: Value) {
        let mut scopes = self.scopes.borrow_mut();
        if let Some(current_scope) = scopes.last_mut() {
            current_scope.insert(name, value);
        }
    }

    /// Get a variable, searching from innermost to outermost scope.
    pub fn get(&self, name: &str) -> Option<Value> {
        let scopes = self.scopes.borrow();
        
        // Search from innermost to outermost scope
        for scope in scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        
        // Check parent environment if not found
        if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    /// Update an existing variable in any scope.
    /// Returns true if the variable was found and updated.
    pub fn update(&self, name: &str, value: Value) -> bool {
        let mut scopes = self.scopes.borrow_mut();
        
        // Search from innermost to outermost scope
        for scope in scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return true;
            }
        }
        
        // Try parent environment
        if let Some(parent) = &self.parent {
            parent.update(name, value)
        } else {
            false
        }
    }

    /// Returns the number of scopes in this environment.
    #[allow(dead_code)]
    pub fn scope_depth(&self) -> usize {
        self.scopes.borrow().len()
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
        env.update("x", Value::Number(2.0, false));
        
        env.pop_scope();
        assert_eq!(env.get("x"), Some(Value::Number(2.0, false)));
    }
}
