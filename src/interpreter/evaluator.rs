use crate::lexer::{ArrayElement, BinaryOp, Expr, ExprKind, Function, ObjectEntry, Stmt, UnaryOp, Value};
use super::environment::Environment;
use super::error::InterpreterError;
use super::control_flow::ControlFlow;
use super::parser::TokenParser;
use super::builtins;
use chumsky::Parser;
use std::rc::Rc;
use std::cell::RefCell;

/// A segment in a path - either a field name or an array index
#[derive(Debug, Clone)]
enum PathSegment {
    Field(String),
    Index(usize),
}

pub struct Interpreter {
    env: Environment,
    pipe_step: Option<usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
            pipe_step: None,
        }
    }

    pub fn with_root(root: Value) -> Self {
        let env = Environment::new();
        env.set("root".to_string(), root);
        Self {
            env,
            pipe_step: None,
        }
    }

    pub fn run(&mut self, stmts: Vec<Stmt>) -> Result<Option<Value>, InterpreterError> {
        let mut last_val = None;
        for stmt in stmts {
            match self.execute_stmt(&stmt)? {
                ControlFlow::Return(val) => return Ok(Some(val)),
                ControlFlow::Value(val) => last_val = Some(val),
                ControlFlow::Next => last_val = None,
                ControlFlow::Break | ControlFlow::Continue => {
                    return Err(InterpreterError::invalid_operation("break/continue outside loop"));
                }
            }
        }
        Ok(last_val)
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<ControlFlow, InterpreterError> {
        match stmt {
            Stmt::Let { name, value } => {
                let val = self.eval_expr(value)?;
                self.env.set(name.to_string(), val);
                Ok(ControlFlow::Next)
            }
            Stmt::Expr(expr) => {
                let val = self.eval_expr(expr)?;
                Ok(ControlFlow::Value(val))
            }
            Stmt::Block(stmts) => {
                let old_env = Rc::new(self.env.clone());
                self.env = Environment::with_parent(old_env.clone());

                let mut result = ControlFlow::Next;
                for s in stmts {
                    match self.execute_stmt(s)? {
                        ControlFlow::Return(val) => {
                            result = ControlFlow::Return(val);
                            break;
                        }
                        ControlFlow::Break | ControlFlow::Continue => {
                            self.env = (*old_env).clone();
                            return Ok(self.execute_stmt(s)?);
                        }
                        ControlFlow::Value(_) | ControlFlow::Next => {}
                    }
                }

                self.env = (*old_env).clone();
                Ok(result)
            }
            Stmt::If { condition, then_branch, else_branch } => {
                let cond_val = self.eval_expr(condition)?;
                let truthy = match cond_val {
                    Value::Bool(b) => b,
                    Value::Null => false,
                    _ => true,
                };
                
                if truthy {
                    self.execute_stmt(&Stmt::Block(then_branch.clone()))
                } else if let Some(else_stmts) = else_branch {
                    self.execute_stmt(&Stmt::Block(else_stmts.clone()))
                } else {
                    Ok(ControlFlow::Next)
                }
            }
            Stmt::For { var, iterable, body } => {
                let iter_val = self.eval_expr(iterable)?;
                let items_rc = match iter_val {
                    Value::Array(arr) => arr,
                    _ => return Err(InterpreterError::type_error("Cannot iterate over non-array")),
                };
                
                let items = items_rc.borrow();
                for item in items.iter() {
                    let old_env = Rc::new(self.env.clone());
                    self.env = Environment::with_parent(old_env.clone());
                    self.env.set(var.to_string(), item.clone());

                    let mut result = ControlFlow::Next;
                    for s in body {
                        match self.execute_stmt(s)? {
                            ControlFlow::Return(val) => {
                                result = ControlFlow::Return(val);
                                break;
                            }
                            ControlFlow::Break => {
                                self.env = (*old_env).clone();
                                return Ok(ControlFlow::Next);
                            }
                            ControlFlow::Continue => {
                                result = ControlFlow::Continue;
                                break;
                            }
                            ControlFlow::Value(_) | ControlFlow::Next => {}
                        }
                    }

                    self.env = (*old_env).clone();
                    match result {
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                        ControlFlow::Continue => continue,
                        _ => {}
                    }
                }
                
                Ok(ControlFlow::Next)
            }
            Stmt::While { condition, body } => {
                loop {
                    let cond_val = self.eval_expr(condition)?;
                    let truthy = match cond_val {
                        Value::Bool(b) => b,
                        Value::Null => false,
                        _ => true,
                    };
                    
                    if !truthy {
                        break;
                    }
                    
                    let old_env = Rc::new(self.env.clone());
                    self.env = Environment::with_parent(old_env.clone());
                    
                    let mut result = ControlFlow::Next;
                    for s in body {
                        match self.execute_stmt(s)? {
                            ControlFlow::Return(val) => {
                                result = ControlFlow::Return(val);
                                break;
                            }
                            ControlFlow::Break => {
                                self.env = (*old_env).clone();
                                return Ok(ControlFlow::Next);
                            }
                            ControlFlow::Continue => {
                                result = ControlFlow::Continue;
                                break;
                            }
                            ControlFlow::Value(_) | ControlFlow::Next => {}
                        }
                    }
                    
                    self.env = (*old_env).clone();
                    match result {
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                        ControlFlow::Continue => continue,
                        _ => {}
                    }
                }
                
                Ok(ControlFlow::Next)
            }
            Stmt::Break => Ok(ControlFlow::Break),
            Stmt::Continue => Ok(ControlFlow::Continue),
            Stmt::Function { name, params, body } => {
                let func = Function {
                    params: params.clone(),
                    body_expr: None,
                    body_stmts: Some(body.clone()),
                };
                self.env.set(name.to_string(), Value::Function(Rc::new(func)));
                Ok(ControlFlow::Next)
            }
            Stmt::Return(expr) => {
                let val = if let Some(e) = expr {
                    self.eval_expr(e)?
                } else {
                    Value::Null
                };
                Ok(ControlFlow::Return(val))
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, InterpreterError> {
        match &expr.kind {
            ExprKind::Literal(val) => Ok(val.clone()),

            ExprKind::Identifier(name) => self
                .env
                .get(name.as_ref())
                .ok_or_else(|| InterpreterError::undefined_variable_at(name.to_string(), expr.span)),

            ExprKind::FieldAccess { object, field } => {
                let obj = self.eval_expr(object)?;
                self.get_field(&obj, field)
            }

            ExprKind::OptionalFieldAccess { object, field } => {
                let obj = self.eval_expr(object)?;
                if matches!(obj, Value::Null) {
                    Ok(Value::Null)
                } else {
                    self.get_field(&obj, field)
                }
            }

            ExprKind::ArrayIndex { array, index } => {
                let arr = self.eval_expr(array)?;
                let idx = self.eval_expr(index)?;
                self.get_index(&arr, &idx)
            }

            ExprKind::Binary { left, op, right } => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;
                self.eval_binary_op(&left_val, op, &right_val)
            }

            ExprKind::Unary { op, expr } => {
                let val = self.eval_expr(expr)?;
                self.eval_unary_op(op, &val)
            }

            ExprKind::Pipe { left, right } => {
                // Count how many pipe operators are in the left side
                let left_pipe_count = self.count_pipe_operators(left);
                // The current step is left_pipe_count + 1 (the right side we're about to evaluate)
                // For `a | b | c`: left is `a | b` (1 pipe), so current_step = 1 + 1 = 2
                // But we want step 3 for `c`, so we need: left_pipe_count + 2
                // Actually: if left has 1 pipe, that means it's `a | b`, so we've done steps 1 and 2
                // The right side `c` is step 3, so: left_pipe_count + 2? No.
                // Let's think: `a | b | c` = Pipe { left: Pipe { left: a, right: b }, right: c }
                // When evaluating outer pipe:
                //   - left is Pipe { left: a, right: b }, which has 1 pipe operator
                //   - So left_pipe_count = 1
                //   - Steps completed: a (step 1), b (step 2)
                //   - Current step should be: 1 + left_pipe_count + 1 = 3
                // Actually simpler: total steps = left_pipe_count + 2
                // But we want the step number for the right side, which is: left_pipe_count + 2
                let current_step = left_pipe_count + 2;
                
                // Evaluate the left side (which may itself be a pipe chain)
                let left_val = match self.eval_expr(left) {
                    Ok(val) => val,
                    Err(e) => {
                        // If error occurred in left side, we need to determine which step it was
                        // The left side could be a single expression or a pipe chain
                        // For now, we'll use a simpler approach: if left is a pipe, recurse
                        // Otherwise, it's step 1
                        let step_num = if matches!(left.kind, ExprKind::Pipe { .. }) {
                            // If left is a pipe, the error occurred somewhere in that chain
                            // We can't easily determine the exact step without more context
                            // For now, use left_pipe_count + 1 as an approximation
                            left_pipe_count + 1
                        } else {
                            1
                        };
                        let step_source = self.expr_to_string(left);
                        return Err(InterpreterError::pipe_chain_error_at(
                            step_num,
                            step_source,
                            e,
                            left.span,
                        ));
                    }
                };
                
                // Set pipe_step for the current step
                let old_pipe_step = self.pipe_step;
                self.pipe_step = Some(current_step);
                
                // Evaluate the right side with error wrapping
                let result = match self.eval_smart_pipe(left_val, right) {
                    Ok(val) => Ok(val),
                    Err(e) => {
                        let step_source = self.expr_to_string(right);
                        Err(InterpreterError::pipe_chain_error_at(
                            current_step,
                            step_source,
                            e,
                            right.span,
                        ))
                    }
                };
                
                // Restore pipe_step
                self.pipe_step = old_pipe_step;
                result
            }

            ExprKind::Grouped(expr) => self.eval_expr(expr),

            ExprKind::Array { elements } => {
                let mut vals = Vec::new();
                for e in elements {
                    vals.push(self.eval_expr(e)?);
                }
                Ok(Value::Array(Rc::new(RefCell::new(vals))))
            }

            ExprKind::ArrayWithSpread { elements } => {
                let mut vals = Vec::new();
                for elem in elements {
                    match elem {
                        ArrayElement::Single(e) => {
                            vals.push(self.eval_expr(e)?);
                        }
                        ArrayElement::Spread(e) => {
                            let spread_val = self.eval_expr(e)?;
                            match spread_val {
                                Value::Array(arr) => {
                                    vals.extend(arr.borrow().iter().cloned());
                                }
                                _ => return Err(InterpreterError::type_error("Spread requires an array")),
                            }
                        }
                    }
                }
                Ok(Value::Array(Rc::new(RefCell::new(vals))))
            }

            ExprKind::Object { fields } => {
                let mut map = indexmap::IndexMap::new();
                for (k, v) in fields {
                    map.insert(k.clone(), self.eval_expr(v)?);
                }
                Ok(Value::Object(Rc::new(RefCell::new(map))))
            }

            ExprKind::ObjectWithSpread { entries } => {
                let mut map = indexmap::IndexMap::new();
                for entry in entries {
                    match entry {
                        ObjectEntry::Field { key, value } => {
                            map.insert(key.clone(), self.eval_expr(value)?);
                        }
                        ObjectEntry::Spread(e) => {
                            let spread_val = self.eval_expr(e)?;
                            match spread_val {
                                Value::Object(obj) => {
                                    for (k, v) in obj.borrow().iter() {
                                        map.insert(k.clone(), v.clone());
                                    }
                                }
                                _ => return Err(InterpreterError::type_error("Spread in object requires an object")),
                            }
                        }
                    }
                }
                Ok(Value::Object(Rc::new(RefCell::new(map))))
            }

            ExprKind::Spread(_) => {
                Err(InterpreterError::invalid_operation("Spread operator can only be used inside arrays or objects"))
            }

            ExprKind::Lambda { params, body } => {
                let func = Function {
                    params: params.clone(),
                    body_expr: Some(body.clone()),
                    body_stmts: None,
                };
                Ok(Value::Function(Rc::new(func)))
            }

            ExprKind::Call { name, args } => {
                let mut arg_vals = Vec::new();
                for a in args {
                    arg_vals.push(self.eval_expr(a)?);
                }
                
                if let Some(func_val) = self.env.get(name.as_ref()) {
                    if let Value::Function(func) = func_val {
                        return self.call_user_function(&func, arg_vals);
                    }
                }
                
                self.call_function(name, arg_vals)
            }

            ExprKind::Assignment { target, value } => {
                let val = self.eval_expr(value)?;
                self.perform_assignment(target, val)
            }

            ExprKind::Range { start, end } => {
                let s = self.eval_expr(start)?;
                let e = self.eval_expr(end)?;
                match (s, e) {
                    (Value::Number(start_num, _), Value::Number(end_num, _)) => {
                        let mut values = Vec::new();
                        let mut current = start_num;
                        while current <= end_num {
                            values.push(Value::Number(current, false));
                            current += 1.0;
                        }
                        Ok(Value::Array(Rc::new(RefCell::new(values))))
                    }
                    _ => Err(InterpreterError::type_error_at("Range requires numbers", expr.span)),
                }
            }

            ExprKind::Ternary { condition, then_branch, else_branch } => {
                let cond_val = self.eval_expr(condition)?;
                let truthy = match cond_val {
                    Value::Bool(b) => b,
                    Value::Null => false,
                    _ => true,
                };
                if truthy {
                    self.eval_expr(then_branch)
                } else {
                    self.eval_expr(else_branch)
                }
            }

            ExprKind::NullCoalesce { left, right } => {
                let left_val = self.eval_expr(left)?;
                if matches!(left_val, Value::Null) {
                    self.eval_expr(right)
                } else {
                    Ok(left_val)
                }
            }

            ExprKind::TemplateLiteral { parts } => {
                use crate::lexer::TemplatePart;
                let mut result = String::new();
                for part in parts {
                    match part {
                        TemplatePart::Literal(s) => result.push_str(s),
                        TemplatePart::Interpolation(expr) => {
                            let val = self.eval_expr(expr)?;
                            result.push_str(&self.value_to_string(&val));
                        }
                    }
                }
                Ok(Value::String(Rc::from(result)))
            }

            ExprKind::Match { value, arms } => {
                use crate::lexer::MatchPattern;
                let val = self.eval_expr(value)?;
                
                for (pattern, result_expr) in arms {
                    match pattern {
                        MatchPattern::Wildcard => {
                            return self.eval_expr(result_expr);
                        }
                        MatchPattern::Literal(pattern_val) => {
                            if self.values_equal(&val, pattern_val) {
                                return self.eval_expr(result_expr);
                            }
                        }
                    }
                }
                
                Err(InterpreterError::invalid_operation_at(
                    "No matching pattern in match expression",
                    expr.span,
                ))
            }
        }
    }

    fn perform_assignment(&mut self, target: &Expr, value: Value) -> Result<Value, InterpreterError> {
        match &target.kind {
            ExprKind::Identifier(name) => {
                if !self.env.update(name.as_ref(), value.clone()) {
                    self.env.set(name.to_string(), value.clone());
                }
                Ok(value)
            }
            ExprKind::FieldAccess { object, field } => {
                // Check if this is a nested path starting from Null (short field/index access)
                if let Some(path) = self.extract_path_segments(target) {
                    // This is a short access chain like .profile.age or .items[0].name
                    if let Some(it_val) = self.env.get("@") {
                        self.set_nested_path(&it_val, &path, value.clone())?;
                        return Ok(value);
                    }
                    // Fall back to root
                    if let Some(root_val) = self.env.get("root") {
                        self.set_nested_path(&root_val, &path, value.clone())?;
                        return Ok(value);
                    }
                    return Err(InterpreterError::type_error("Cannot set field - no context object"));
                }
                
                // Not a short field access chain, evaluate the object
                let obj_val = self.eval_expr(object)?;
                match obj_val {
                    Value::Object(map_rc) => {
                        map_rc.borrow_mut().insert(field.clone(), value.clone());
                        Ok(value)
                    }
                    _ => Err(InterpreterError::type_error("Cannot set field on non-object")),
                }
            }
            ExprKind::ArrayIndex { array, index } => {
                // Check if this is a nested path starting from Null (short field/index access)
                if let Some(path) = self.extract_path_segments(target) {
                    // This is a short access chain like .items[0] or .data[0].value
                    if let Some(it_val) = self.env.get("@") {
                        self.set_nested_path(&it_val, &path, value.clone())?;
                        return Ok(value);
                    }
                    // Fall back to root
                    if let Some(root_val) = self.env.get("root") {
                        self.set_nested_path(&root_val, &path, value.clone())?;
                        return Ok(value);
                    }
                    return Err(InterpreterError::type_error("Cannot set index - no context object"));
                }

                let idx_val = self.eval_expr(index)?;
                let idx = match idx_val {
                    Value::Number(n, _) => n as usize,
                    _ => return Err(InterpreterError::type_error("Index must be a number")),
                };

                let arr_val = self.eval_expr(array)?;
                match arr_val {
                    Value::Array(items_rc) => {
                        let mut items = items_rc.borrow_mut();
                        if idx < items.len() {
                            items[idx] = value.clone();
                            Ok(value)
                        } else {
                            Err(InterpreterError::index_out_of_bounds_at(idx, items.len(), target.span))
                        }
                    }
                    _ => Err(InterpreterError::type_error("Cannot index non-array")),
                }
            }
            _ => Err(InterpreterError::invalid_operation("Invalid assignment target")),
        }
    }
    
    fn eval_smart_pipe(&mut self, left: Value, right: &Expr) -> Result<Value, InterpreterError> {
        // Check if the right side is a lambda - use it for map/filter
        if let ExprKind::Lambda { params, body } = &right.kind {
            return self.eval_pipe_with_lambda(left, params, body);
        }
        
        // Check if the right side is a function call - if so, prepend left as first argument
        if let ExprKind::Call { name, args } = &right.kind {
            let mut arg_vals = vec![left];
            for a in args {
                arg_vals.push(self.eval_expr(a)?);
            }
            
            if let Some(func_val) = self.env.get(name.as_ref()) {
                if let Value::Function(func) = func_val {
                    return self.call_user_function(&func, arg_vals);
                }
            }
            
            return self.call_function(name, arg_vals);
        }
        
        // Check if expression is a binary operation on a short field/index access (.field + 2, .profile.age + 5, or .items[0] + 1)
        // These should automatically mutate the field and return the modified object
        // This makes `.id + 2` equivalent to `.id += 2` in pipe context
        let mutation_info = self.get_pipe_mutation_info(right);
        
        // Check if expression is an assignment on a short field/index access (.field = value, .profile.age = value, or .items[0] = value)
        let assignment_path = self.get_pipe_assignment_path(right);
        
        match left {
            Value::Array(items_rc) => {
                let mut results = Vec::new();
                let items = items_rc.borrow();
                for item in items.iter() {
                    let old_env = Rc::new(self.env.clone());
                    self.env = Environment::with_parent(old_env.clone());
                    self.env.set("@".to_string(), item.clone());

                    if let Some((path, op, value_expr)) = &mutation_info {
                        // Evaluate the mutation: get current value, apply op, set back
                        let current = self.get_nested_path(item, path)?;
                        let right_val = self.eval_expr(value_expr)?;
                        let new_val = self.eval_binary_op(&current, op, &right_val)?;
                        
                        // Update the field on @ using nested path
                        if let Some(it_val) = self.env.get("@") {
                            self.set_nested_path(&it_val, path, new_val)?;
                        }
                        
                        // Return the modified @
                        if let Some(modified_item) = self.env.get("@") {
                            results.push(modified_item);
                        } else {
                            results.push(item.clone());
                        }
                    } else if let Some(path) = &assignment_path {
                        // Extract the value from the assignment expression
                        if let ExprKind::Assignment { value, .. } = &right.kind {
                            let new_val = self.eval_expr(value)?;
                            // Update using nested path
                            if let Some(it_val) = self.env.get("@") {
                                self.set_nested_path(&it_val, path, new_val)?;
                            }
                        }
                        if let Some(modified_item) = self.env.get("@") {
                            results.push(modified_item);
                        } else {
                            results.push(item.clone());
                        }
                    } else {
                        let res = self.eval_expr(right)?;
                        match res {
                            Value::Bool(b) => {
                                if b { results.push(item.clone()); }
                            }
                            other => results.push(other),
                        }
                    }

                    self.env = (*old_env).clone();
                }

                Ok(Value::Array(Rc::new(RefCell::new(results))))
            }
            other => {
                let old_env = Rc::new(self.env.clone());
                self.env = Environment::with_parent(old_env.clone());
                self.env.set("@".to_string(), other.clone());
                
                let result = if let Some((path, op, value_expr)) = &mutation_info {
                    // Evaluate the mutation using nested path
                    let current = self.get_nested_path(&other, path)?;
                    let right_val = self.eval_expr(value_expr)?;
                    let new_val = self.eval_binary_op(&current, op, &right_val)?;
                    
                    // Update using nested path
                    if let Some(it_val) = self.env.get("@") {
                        self.set_nested_path(&it_val, path, new_val)?;
                    }
                    
                    self.env.get("@").unwrap_or(other)
                } else if let Some(path) = &assignment_path {
                    // Extract the value from the assignment expression
                    if let ExprKind::Assignment { value, .. } = &right.kind {
                        let new_val = self.eval_expr(value)?;
                        // Update using nested path
                        if let Some(it_val) = self.env.get("@") {
                            self.set_nested_path(&it_val, path, new_val)?;
                        }
                    }
                    self.env.get("@").unwrap_or(other)
                } else {
                    self.eval_expr(right)?
                };
                
                self.env = (*old_env).clone();
                Ok(result)
            }
        }
    }
    
    /// Evaluate a pipe with a lambda - handles both map and filter based on return type
    fn eval_pipe_with_lambda(&mut self, left: Value, params: &[Rc<str>], body: &Expr) -> Result<Value, InterpreterError> {
        match left {
            Value::Array(items_rc) => {
                let mut results = Vec::new();
                let items = items_rc.borrow();
                
                for item in items.iter() {
                    let old_env = Rc::new(self.env.clone());
                    self.env = Environment::with_parent(old_env.clone());
                    
                    // Bind parameters
                    if !params.is_empty() {
                        self.env.set(params[0].to_string(), item.clone());
                    }
                    self.env.set("@".to_string(), item.clone());
                    
                    let res = self.eval_expr(body)?;
                    
                    match res {
                        Value::Bool(b) => {
                            // Filter: if true, keep the item
                            if b { results.push(item.clone()); }
                        }
                        other => {
                            // Map: push the transformed value
                            results.push(other);
                        }
                    }
                    
                    self.env = (*old_env).clone();
                }
                
                Ok(Value::Array(Rc::new(RefCell::new(results))))
            }
            other => {
                // Single value - just apply the lambda
                let old_env = Rc::new(self.env.clone());
                self.env = Environment::with_parent(old_env.clone());
                
                if !params.is_empty() {
                    self.env.set(params[0].to_string(), other.clone());
                }
                self.env.set("@".to_string(), other);
                
                let res = self.eval_expr(body)?;
                self.env = (*old_env).clone();
                Ok(res)
            }
        }
    }
    
    /// A segment in a path - either a field name or an array index
    /// e.g., `.items[0].active` would be [Field("items"), Index(0), Field("active")]
    
    /// Count the number of pipe operations in a pipe chain
    /// For `a | b | c`, this returns 2 (2 pipe operators, so 3 steps total)
    /// Returns the number of pipe operators, not the total number of steps
    fn count_pipe_operators(&self, expr: &Expr) -> usize {
        match &expr.kind {
            ExprKind::Pipe { left, .. } => {
                1 + self.count_pipe_operators(left)
            }
            _ => 0,
        }
    }

    /// Get a string representation of an expression for error messages
    /// This is a simplified version - in a real implementation, you might want to
    /// reconstruct the source from the span or store source text
    fn expr_to_string(&self, expr: &Expr) -> String {
        match &expr.kind {
            ExprKind::Identifier(name) => name.to_string(),
            ExprKind::Literal(val) => format!("{:?}", val),
            ExprKind::FieldAccess { object, field } => {
                format!("{}.{}", self.expr_to_string(object), field)
            }
            ExprKind::ArrayIndex { array, index } => {
                format!("{}[{}]", self.expr_to_string(array), self.expr_to_string(index))
            }
            ExprKind::Call { name, .. } => format!("{}()", name),
            ExprKind::Binary { left, op, right } => {
                let op_str = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                    BinaryOp::Mod => "%",
                    BinaryOp::Pow => "^",
                    BinaryOp::Eq => "==",
                    BinaryOp::NotEq => "!=",
                    BinaryOp::Less => "<",
                    BinaryOp::LessEq => "<=",
                    BinaryOp::Greater => ">",
                    BinaryOp::GreaterEq => ">=",
                    BinaryOp::And => "&&",
                    BinaryOp::Or => "||",
                    BinaryOp::Pipe => "|",
                };
                format!("{} {} {}", self.expr_to_string(left), op_str, self.expr_to_string(right))
            }
            ExprKind::Pipe { left, right } => {
                format!("{} | {}", self.expr_to_string(left), self.expr_to_string(right))
            }
            _ => "<expression>".to_string(),
        }
    }

    /// Extract the field path from a nested field access expression starting from Null
    /// e.g., `.profile.age` returns Some([Field("profile"), Field("age")])
    /// e.g., `.items[0].active` returns Some([Field("items"), Index(0), Field("active")])
    fn extract_path_segments(&self, expr: &Expr) -> Option<Vec<PathSegment>> {
        let mut path = Vec::new();
        let mut current = expr;
        
        loop {
            match &current.kind {
                ExprKind::FieldAccess { object, field } => {
                    path.push(PathSegment::Field(field.clone()));
                    current = object;
                }
                ExprKind::ArrayIndex { array, index } => {
                    // Try to evaluate index as a constant number
                    if let ExprKind::Literal(Value::Number(n, _)) = &index.kind {
                        path.push(PathSegment::Index(*n as usize));
                        current = array;
                    } else {
                        return None; // Non-constant index, can't handle statically
                    }
                }
                ExprKind::Literal(Value::Null) => {
                    // We've reached the root (short field access starting with `.`)
                    path.reverse();
                    return Some(path);
                }
                _ => return None, // Not a short field access chain
            }
        }
    }
    
    
    /// Check if the expression is a mutation pattern in pipe context
    /// Returns Some((path_segments, op, value_expr)) for patterns like `.field + 2`, `.profile.age + 5`, or `.items[0] + 1`
    /// For assignments (.field = value), returns None but is handled separately
    fn get_pipe_mutation_info(&self, expr: &Expr) -> Option<(Vec<PathSegment>, BinaryOp, Expr)> {
        // Check for binary ops like `.field + 2`, `.profile.age * 3`, `.items[0] + 1`, etc.
        if let ExprKind::Binary { left, op, right } = &expr.kind {
            // Only treat arithmetic/string ops as mutations, not comparisons
            if matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod | BinaryOp::Pow) {
                // Check if left is a short field/index access chain
                if let Some(path) = self.extract_path_segments(left) {
                    return Some((path, op.clone(), (**right).clone()));
                }
            }
        }
        
        None
    }
    
    /// Check if the expression is an assignment on a short field access (.field = value, .profile.age = value, or .items[0] = value)
    /// Returns the path segments if it's a pipe assignment
    fn get_pipe_assignment_path(&self, expr: &Expr) -> Option<Vec<PathSegment>> {
        if let ExprKind::Assignment { target, .. } = &expr.kind {
            return self.extract_path_segments(target);
        }
        None
    }
    
    /// Get a value at a nested path (fields and/or array indices) from an object
    fn get_nested_path(&self, obj: &Value, path: &[PathSegment]) -> Result<Value, InterpreterError> {
        if path.is_empty() {
            return Ok(obj.clone());
        }
        
        let mut current = obj.clone();
        for segment in path {
            current = match segment {
                PathSegment::Field(field) => {
                    self.get_field(&current, field)?
                }
                PathSegment::Index(idx) => {
                    match current {
                        Value::Array(items_rc) => {
                            let items = items_rc.borrow();
                            if *idx < items.len() {
                                items[*idx].clone()
                            } else {
                                return Err(InterpreterError::index_out_of_bounds(*idx, items.len()));
                            }
                        }
                        _ => return Err(InterpreterError::type_error("Cannot index non-array")),
                    }
                }
            };
        }
        Ok(current)
    }
    
    /// Set a value at a nested path (fields and/or array indices), modifying it in place
    fn set_nested_path(&self, obj: &Value, path: &[PathSegment], value: Value) -> Result<(), InterpreterError> {
        if path.is_empty() {
            return Err(InterpreterError::invalid_operation("Cannot set empty path"));
        }
        
        if path.len() == 1 {
            // Single segment - set directly
            match &path[0] {
                PathSegment::Field(field) => {
                    if let Value::Object(map_rc) = obj {
                        map_rc.borrow_mut().insert(field.clone(), value);
                        return Ok(());
                    }
                    return Err(InterpreterError::type_error("Cannot set field on non-object"));
                }
                PathSegment::Index(idx) => {
                    if let Value::Array(items_rc) = obj {
                        let mut items = items_rc.borrow_mut();
                        if *idx < items.len() {
                            items[*idx] = value;
                            return Ok(());
                        }
                        return Err(InterpreterError::index_out_of_bounds(*idx, items.len()));
                    }
                    return Err(InterpreterError::type_error("Cannot index non-array"));
                }
            }
        }
        
        // Navigate to the parent container, then set the final segment
        let parent_path = &path[..path.len() - 1];
        let current = self.get_nested_path(obj, parent_path)?;
        
        match &path[path.len() - 1] {
            PathSegment::Field(field) => {
                if let Value::Object(map_rc) = current {
                    map_rc.borrow_mut().insert(field.clone(), value);
                    Ok(())
                } else {
                    Err(InterpreterError::type_error("Cannot set field on non-object"))
                }
            }
            PathSegment::Index(idx) => {
                if let Value::Array(items_rc) = current {
                    let mut items = items_rc.borrow_mut();
                    if *idx < items.len() {
                        items[*idx] = value;
                        Ok(())
                    } else {
                        Err(InterpreterError::index_out_of_bounds(*idx, items.len()))
                    }
                } else {
                    Err(InterpreterError::type_error("Cannot index non-array"))
                }
            }
        }
    }
    

    fn call_function_value(&mut self, func_val: &Value, call_args: &[Value]) -> Result<Value, InterpreterError> {
        if let Value::Function(func) = func_val {
            self.call_user_function(func, call_args.to_vec())
        } else {
            Err(InterpreterError::type_error("Expected function value"))
        }
    }

    fn call_function(&mut self, name: &str, args: Vec<Value>) -> Result<Value, InterpreterError> {
        // Functions that need closures for callbacks
        if matches!(name, "find" | "find_index" | "reduce" | "every" | "some" | "replicate") {
            return match name {
                "find" => builtins::builtin_find(&args, |func, call_args| self.call_function_value(func, call_args)),
                "find_index" => builtins::builtin_find_index(&args, |func, call_args| self.call_function_value(func, call_args)),
                "reduce" => builtins::builtin_reduce(&args, |func, call_args| self.call_function_value(func, call_args)),
                "every" => builtins::builtin_every(&args, |func, call_args| self.call_function_value(func, call_args)),
                "some" => builtins::builtin_some(&args, |func, call_args| self.call_function_value(func, call_args)),
                "replicate" => builtins::builtin_replicate(&args, |func, call_args| self.call_function_value(func, call_args)),
                _ => unreachable!(),
            };
        }
        
        match name {
            // Note: "count" has been removed - use .length property instead
            "sum" => builtins::builtin_sum(&args),
            "avg" => builtins::builtin_avg(&args),
            "min" => builtins::builtin_min(&args),
            "max" => builtins::builtin_max(&args),
            // Note: "take" has been removed - use slice(arr, 0, n) instead
            "push" => builtins::builtin_push(&args),
            "input" => builtins::builtin_input(&args),
            "print" => builtins::builtin_print(&args, |v| self.value_to_string(v)),
            "unique" => builtins::builtin_unique(&args, |a, b| self.deep_equals(a, b)),
            "sort_by" => builtins::builtin_sort_by(&args, |v, f| self.get_field(v, f)),
            "group_by" => builtins::builtin_group_by(&args, |v, f| self.get_field(v, f)),
            "include" => self.builtin_include(&args),
            "reverse" => builtins::builtin_reverse(&args),
            "enumerate" => builtins::builtin_enumerate(&args),
            "range" => builtins::builtin_range_fn(&args),
            "deep_merge" => builtins::builtin_deep_merge(&args),
            "cross" => builtins::builtin_cross(&args),
            "set_path" => builtins::builtin_set_path(&args),
            "clone" => builtins::builtin_clone(&args),
            "sort" => builtins::builtin_sort(&args),
            "slice" => builtins::builtin_slice(&args),
            "pop" => builtins::builtin_pop(&args),
            "shift" => builtins::builtin_shift(&args),
            "flat" => builtins::builtin_flat(&args),
            // Note: "flatten" has been removed - use "flat" instead
            "zip" => builtins::builtin_zip(&args),
            "first" => builtins::builtin_first(&args),
            "last" => builtins::builtin_last(&args),
            "split" => builtins::builtin_split(&args),
            "join" => builtins::builtin_join(&args, |v| self.value_to_string(v)),
            "trim" => builtins::builtin_trim(&args),
            "upper" => builtins::builtin_upper(&args),
            "lower" => builtins::builtin_lower(&args),
            "contains" => builtins::builtin_contains(&args),
            "starts_with" => builtins::builtin_starts_with(&args),
            "ends_with" => builtins::builtin_ends_with(&args),
            "replace" => builtins::builtin_replace(&args),
            "len" => builtins::builtin_len(&args),
            "keys" => builtins::builtin_keys(&args),
            "values" => builtins::builtin_values(&args),
            "entries" => builtins::builtin_entries(&args),
            "has" => builtins::builtin_has(&args),
            "merge" => builtins::builtin_merge(&args),
            "typeof" => builtins::builtin_typeof(&args),
            "is_null" => builtins::builtin_is_null(&args),
            "is_array" => builtins::builtin_is_array(&args),
            "is_object" => builtins::builtin_is_object(&args),
            "is_string" => builtins::builtin_is_string(&args),
            "is_number" => builtins::builtin_is_number(&args),
            "is_bool" => builtins::builtin_is_bool(&args),
            "to_string" => builtins::builtin_to_string(&args, |v| self.value_to_string(v)),
            "to_number" => builtins::builtin_to_number(&args),
            "to_int" => builtins::builtin_to_int(&args),
            "to_float" => builtins::builtin_to_float(&args),
            "to_bool" => builtins::builtin_to_bool(&args),
            "parse_json" => builtins::builtin_parse_json(&args),
            "floor" => builtins::builtin_floor(&args),
            "ceil" => builtins::builtin_ceil(&args),
            "round" => builtins::builtin_round(&args),
            "abs" => builtins::builtin_abs(&args),
            "sqrt" => builtins::builtin_sqrt(&args),
            "pow" => builtins::builtin_pow(&args),
            "sin" => builtins::builtin_sin(&args),
            "cos" => builtins::builtin_cos(&args),
            "tan" => builtins::builtin_tan(&args),
            "random" => builtins::builtin_random(&args),
            _ => Err(InterpreterError::invalid_operation(format!("Unknown function: {}", name))),
        }
    }

    fn call_user_function(&mut self, func: &Function, args: Vec<Value>) -> Result<Value, InterpreterError> {
        if func.params.len() != args.len() {
            return Err(InterpreterError::invalid_operation(format!(
                "Function expects {} arguments, got {}",
                func.params.len(),
                args.len()
            )));
        }

        let old_env = Rc::new(self.env.clone());
        self.env = Environment::with_parent(old_env.clone());

        for (param, arg) in func.params.iter().zip(args.iter()) {
            self.env.set(param.to_string(), arg.clone());
        }

        let result = if let Some(ref body_expr) = func.body_expr {
            self.eval_expr(body_expr)?
        } else if let Some(ref body_stmts) = func.body_stmts {
            let mut last_val = Value::Null;
            for stmt in body_stmts {
                match self.execute_stmt(stmt)? {
                    ControlFlow::Return(val) => {
                        self.env = (*old_env).clone();
                        return Ok(val);
                    }
                    ControlFlow::Value(val) => last_val = val,
                    ControlFlow::Next => {}
                    ControlFlow::Break | ControlFlow::Continue => {
                        return Err(InterpreterError::invalid_operation("break/continue outside loop"));
                    }
                }
            }
            last_val
        } else {
            return Err(InterpreterError::invalid_operation("Function has no body"));
        };

        self.env = (*old_env).clone();
        Ok(result)
    }

    fn builtin_include(&mut self, args: &[Value]) -> Result<Value, InterpreterError> {
        if args.is_empty() {
            return Err(InterpreterError::invalid_operation("include requires a file path argument"));
        }
        if let Value::String(path) = &args[0] {
            let script_content = std::fs::read_to_string(path.as_ref())
                .map_err(|e| InterpreterError::invalid_operation(format!("Failed to read script file '{}': {}", path, e)))?;
            
            let lexer = crate::lexer::lexer();
            let tokens = lexer
                .parse(&script_content)
                .into_result()
                .map_err(|e| InterpreterError::invalid_operation(format!("Lexer error in included script: {:?}", e)))?;
            
            let mut parser = TokenParser::from_lexer_output(tokens, script_content.len());
            let stmts = parser
                .parse()
                .map_err(|e| InterpreterError::invalid_operation(format!("Parser error in included script: {}", e)))?;
            
            let result = self.run(stmts)?;
            Ok(result.unwrap_or(Value::Null))
        } else {
            Err(InterpreterError::type_error("include requires a string path argument"))
        }
    }

    pub fn get_field(&self, obj: &Value, field: &str) -> Result<Value, InterpreterError> {
        match obj {
            Value::Object(map) => Ok(map
                .borrow()
                .get(field)
                .cloned()
                .unwrap_or(Value::Null)),
            Value::Array(items) => {
                if field == "length" {
                    Ok(Value::Number(items.borrow().len() as f64, false))
                } else {
                    Ok(Value::Null)
                }
            }
            Value::Null => {
                // First try @ (for pipe context)
                if let Some(it) = self.env.get("@") {
                    if let Value::Object(map) = it {
                        return map.borrow().get(field).cloned()
                            .ok_or_else(|| InterpreterError::field_not_found(field));
                    }
                }
                // Fall back to root (for top-level .field access)
                if let Some(root) = self.env.get("root") {
                    if let Value::Object(map) = root {
                        return Ok(map.borrow().get(field).cloned().unwrap_or(Value::Null));
                    }
                }
                Ok(Value::Null)
            }
            _ => Err(InterpreterError::type_error(format!(
                "Cannot access field '{}' on non-object",
                field
            ))),
        }
    }

    fn get_index(&self, arr: &Value, idx: &Value) -> Result<Value, InterpreterError> {
        match (arr, idx) {
            (Value::Array(items), Value::Number(n, _)) => {
                let index = *n as usize;
                let len = items.borrow().len();
                items
                    .borrow()
                    .get(index)
                    .cloned()
                    .ok_or_else(|| InterpreterError::index_out_of_bounds(index, len))
            }
            _ => Err(InterpreterError::type_error(
                "Array indexing requires array and number",
            )),
        }
    }

    fn eval_binary_op(
        &self,
        left: &Value,
        op: &BinaryOp,
        right: &Value,
    ) -> Result<Value, InterpreterError> {
        match (left, op, right) {
            (Value::Number(left_num, left_float), BinaryOp::Add, Value::Number(right_num, right_float)) => Ok(Value::Number(left_num + right_num, *left_float || *right_float)),
            (Value::Number(left_num, left_float), BinaryOp::Sub, Value::Number(right_num, right_float)) => Ok(Value::Number(left_num - right_num, *left_float || *right_float)),
            (Value::Number(left_num, left_float), BinaryOp::Mul, Value::Number(right_num, right_float)) => Ok(Value::Number(left_num * right_num, *left_float || *right_float)),
            (Value::Number(left_num, _), BinaryOp::Div, Value::Number(right_num, _)) => {
                if *right_num == 0.0 {
                    Err(InterpreterError::division_by_zero())
                } else {
                    Ok(Value::Number(left_num / right_num, true))
                }
            }
            (Value::Number(left_num, left_float), BinaryOp::Mod, Value::Number(right_num, right_float)) => Ok(Value::Number(left_num % right_num, *left_float || *right_float)),
            (Value::Number(base, _), BinaryOp::Pow, Value::Number(exponent, _)) => Ok(Value::Number(base.powf(*exponent), true)),
            (Value::String(left_str), BinaryOp::Add, Value::String(right_str)) => {
                let mut combined = String::with_capacity(left_str.len() + right_str.len());
                combined.push_str(left_str);
                combined.push_str(right_str);
                Ok(Value::String(Rc::<str>::from(combined)))
            }
            (Value::Array(left_arr), BinaryOp::Add, Value::Array(right_arr)) => {
                let mut result = left_arr.borrow().clone();
                result.extend(right_arr.borrow().iter().cloned());
                Ok(Value::Array(Rc::new(RefCell::new(result))))
            }
            (Value::Array(array), BinaryOp::Add, value) => {
                let mut result = array.borrow().clone();
                result.push(value.clone());
                Ok(Value::Array(Rc::new(RefCell::new(result))))
            }
            (left_val, BinaryOp::Eq, right_val) => Ok(Value::Bool(self.values_equal(left_val, right_val))),
            (left_val, BinaryOp::NotEq, right_val) => Ok(Value::Bool(!self.values_equal(left_val, right_val))),
            (Value::Number(left_num, _), BinaryOp::Greater, Value::Number(right_num, _)) => Ok(Value::Bool(left_num > right_num)),
            (Value::Number(left_num, _), BinaryOp::Less, Value::Number(right_num, _)) => Ok(Value::Bool(left_num < right_num)),
            (Value::Number(left_num, _), BinaryOp::GreaterEq, Value::Number(right_num, _)) => Ok(Value::Bool(left_num >= right_num)),
            (Value::Number(left_num, _), BinaryOp::LessEq, Value::Number(right_num, _)) => Ok(Value::Bool(left_num <= right_num)),
            (Value::Bool(left_bool), BinaryOp::And, Value::Bool(right_bool)) => Ok(Value::Bool(*left_bool && *right_bool)),
            (Value::Bool(left_bool), BinaryOp::Or, Value::Bool(right_bool)) => Ok(Value::Bool(*left_bool || *right_bool)),
            _ => Err(InterpreterError::invalid_operation(format!(
                "Cannot apply {:?} to {:?} and {:?}",
                op, left, right
            ))),
        }
    }

    fn eval_unary_op(&mut self, op: &UnaryOp, val: &Value) -> Result<Value, InterpreterError> {
        match (op, val) {
            (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            (UnaryOp::Neg, Value::Number(n, is_float)) => Ok(Value::Number(-n, *is_float)),
            _ => Err(InterpreterError::invalid_operation(format!(
                "Cannot apply {:?} to {:?}",
                op, val
            ))),
        }
    }

    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a, _), Value::Number(b, _)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            _ => false,
        }
    }

    pub fn deep_equals(&self, a: &Value, b: &Value) -> bool {
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
                    .all(|(va, vb)| self.deep_equals(va, vb))
            }
            (Value::Object(obj_a), Value::Object(obj_b)) => {
                let obj_a_ref = obj_a.borrow();
                let obj_b_ref = obj_b.borrow();
                if obj_a_ref.len() != obj_b_ref.len() {
                    return false;
                }
                obj_a_ref.iter().all(|(k, va)| {
                    if let Some(vb) = obj_b_ref.get(k) {
                        self.deep_equals(va, vb)
                    } else {
                        false
                    }
                })
            }
            _ => false,
        }
    }

    pub fn value_to_string(&self, val: &Value) -> String {
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
                let items: Vec<String> = arr.borrow().iter().map(|v| self.value_to_display(v)).collect();
                format!("[{}]", items.join(", "))
            }
            Value::Object(obj) => {
                let fields: Vec<String> = obj.borrow().iter()
                    .map(|(k, v)| format!("\"{}\": {}", k, self.value_to_display(v)))
                    .collect();
                format!("{{{}}}", fields.join(", "))
            }
            Value::Function(_) => "<function>".to_string(),
        }
    }

    fn value_to_display(&self, val: &Value) -> String {
        match val {
            Value::String(s) => format!("\"{}\"", s),
            _ => self.value_to_string(val),
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<Value> {
        self.env.get(name)
    }
}

pub fn parse_and_run(source: &str, root: Value) -> Result<Option<Value>, String> {
    let tokens = match crate::lexer::lexer().parse(source).into_output() {
        Some(t) => t,
        None => return Err("Lexer failed".to_string()),
    };

    let mut parser = TokenParser::from_lexer_output(tokens, source.len());
    let stmts = parser.parse()?;

    let mut interpreter = Interpreter::with_root(root);
    interpreter
        .run(stmts)
        .map_err(|e| format!("Runtime error: {}", e))
}

pub fn parse_and_run_with_diagnostics(
    source: &str, 
    root: Value
) -> Result<Option<Value>, Vec<crate::diagnostic::Diagnostic>> {
    use crate::diagnostic::Diagnostic;

    let tokens = match crate::lexer::lexer().parse(source).into_output() {
        Some(t) => t,
        None => {
            return Err(vec![Diagnostic::error("Lexer failed to tokenize input")
                .with_code("E0001")]);
        }
    };

    let mut parser = TokenParser::from_lexer_output(tokens, source.len());
    let parse_result = parser.parse_with_errors();

    if !parse_result.errors.is_empty() {
        let diagnostics: Vec<Diagnostic> = parse_result
            .errors
            .iter()
            .map(|e| e.to_diagnostic())
            .collect();
        return Err(diagnostics);
    }

    let mut interpreter = Interpreter::with_root(root);
    match interpreter.run(parse_result.statements) {
        Ok(val) => Ok(val),
        Err(e) => Err(vec![e.to_diagnostic()]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;

    fn make_test_root() -> Value {
        let mut root_obj = IndexMap::new();

        let mut users = Vec::new();

        let mut user1 = IndexMap::new();
        user1.insert("name".to_string(), Value::String(Rc::from("Bob")));
        user1.insert("age".to_string(), Value::Number(30.0, false));
        users.push(Value::Object(Rc::new(RefCell::new(user1))));

        let mut user2 = IndexMap::new();
        user2.insert("name".to_string(), Value::String(Rc::from("Alice")));
        user2.insert("age".to_string(), Value::Number(25.0, false));
        users.push(Value::Object(Rc::new(RefCell::new(user2))));

        let mut user3 = IndexMap::new();
        user3.insert("name".to_string(), Value::String(Rc::from("Bob")));
        user3.insert("age".to_string(), Value::Number(35.0, false));
        users.push(Value::Object(Rc::new(RefCell::new(user3))));

        root_obj.insert("users".to_string(), Value::Array(Rc::new(RefCell::new(users))));

        Value::Object(Rc::new(RefCell::new(root_obj)))
    }

    #[test]
    fn test_simple_assignment() {
        let source = "let x = 5;";
        let root = Value::Null;
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_simple_assignment_returns_none() {
        let source = "let x = 5;";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_expression_returns_value() {
        let source = "5 + 3;";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert_eq!(result, Some(Value::Number(8.0, false)));
    }

    #[test]
    fn test_field_access() {
        let source = "let users = root.users;";
        let root = make_test_root();
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_arithmetic() {
        let source = "10 + 5 * 2;";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert_eq!(result, Some(Value::Number(20.0, false)));
    }

    #[test]
    fn test_comparison() {
        let source = "10 > 5;";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert_eq!(result, Some(Value::Bool(true)));
    }

    #[test]
    fn test_pipe_filter() {
        let source = r#"
            let users = root.users;
            users | .name == "Bob";
        "#;
        let root = make_test_root();
        let result = parse_and_run(source, root).unwrap();
        if let Some(Value::Array(arr)) = result {
            assert_eq!(arr.borrow().len(), 2);
        } else {
            panic!("Expected array result");
        }
    }

    #[test]
    fn test_array_indexing() {
        let source = "root.users[0];";
        let root = make_test_root();
        let result = parse_and_run(source, root).unwrap();
        assert!(result.is_some());
    }

    #[test]
    fn test_logical_operators() {
        let source = "true && false || true;";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert_eq!(result, Some(Value::Bool(true)));
    }

    #[test]
    fn test_nested_field_access() {
        let source = "root.users[0].name;";
        let root = make_test_root();
        let result = parse_and_run(source, root).unwrap();
        assert_eq!(result, Some(Value::String(Rc::from("Bob"))));
    }

    #[test]
    fn test_complex_filter() {
        let source = r#"root.users | .age > 25;"#;
        let root = make_test_root();
        let result = parse_and_run(source, root).unwrap();
        if let Some(Value::Array(arr)) = result {
            assert_eq!(arr.borrow().len(), 2);
        } else {
            panic!("Expected array result");
        }
    }

    #[test]
    fn test_combined_filter() {
        let source = r#"root.users | .name == "Bob" && .age > 30;"#;
        let root = make_test_root();
        let result = parse_and_run(source, root).unwrap();
        if let Some(Value::Array(arr)) = result {
            assert_eq!(arr.borrow().len(), 1);
        } else {
            panic!("Expected array result");
        }
    }

    #[test]
    fn test_array_length_property() {
        let source = "root.users.length;";
        let root = make_test_root();
        let result = parse_and_run(source, root).unwrap();
        assert_eq!(result, Some(Value::Number(3.0, false)));
    }

    #[test]
    fn test_sum_function() {
        let source = "sum([1, 2, 3, 4, 5]);";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert_eq!(result, Some(Value::Number(15.0, false)));
    }

    #[test]
    fn test_avg_function() {
        let source = "avg([10, 20, 30]);";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert_eq!(result, Some(Value::Number(20.0, true)));
    }
}

