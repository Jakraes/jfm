use crate::ast::{ArrayElement, BinaryOp, Expr, ExprKind, ObjectEntry, Stmt, UnaryOp, Param};
use crate::value::{deep_equals, values_equal, value_to_string, Function, Value};
use super::builtins;
use super::environment::Environment;
use super::error::InterpreterError;
use super::parser::TokenParser;
use chumsky::Parser;
use std::cell::RefCell;
use std::rc::Rc;

/// Variable name for the pipe context (the current item being processed)
pub const PIPE_CONTEXT: &str = "@";

/// Variable name for the root JSON value
pub const ROOT_CONTEXT: &str = "root";

/// Control flow for statement execution
#[derive(Debug, Clone)]
pub enum ControlFlow {
    Next,
    Value(Value),
    Return(Value),
    Break,
    Continue,
}

/// Represents a segment in an access path for nested field/index operations.
/// For example, `.items[0].active` would be represented as:
/// `[Field("items"), Index(0), Field("active")]`
#[derive(Debug, Clone)]
enum AccessPathSegment {
    Field(String),
    Index(usize),
    NegativeIndex(i64),
}

pub struct Interpreter {
    env: Environment,
    pipe_step: Option<usize>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
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
        env.set(ROOT_CONTEXT.to_string(), root);
        Self {
            env,
            pipe_step: None,
        }
    }

    pub fn run(&mut self, stmts: Vec<Stmt>) -> Result<Option<Value>, InterpreterError> {
        let mut last_val = None;
        for stmt in stmts {
            match self.execute_statement(&stmt)? {
                ControlFlow::Return(val) => return Ok(Some(val)),
                ControlFlow::Value(val) => last_val = Some(val),
                ControlFlow::Next => last_val = None,
                ControlFlow::Break | ControlFlow::Continue => {
                    return Err(InterpreterError::invalid_operation("break/continue outside loop"));
                }
            }
        }
        // If no explicit value was returned, return root as default
        if last_val.is_none() {
            last_val = self.env.get(ROOT_CONTEXT);
        }
        Ok(last_val)
    }

    /// Execute a loop body (for/while) and handle control flow.
    /// Returns:
    /// - Ok(Some(ControlFlow::Return(val))) if return was hit
    /// - Ok(Some(ControlFlow::Break)) if break was hit  
    /// - Ok(Some(ControlFlow::Continue)) if continue was hit
    /// - Ok(None) for normal completion
    fn execute_loop_body(&mut self, body: &[Stmt]) -> Result<Option<ControlFlow>, InterpreterError> {
        for stmt in body {
            match self.execute_statement(stmt)? {
                ControlFlow::Return(val) => return Ok(Some(ControlFlow::Return(val))),
                ControlFlow::Break => return Ok(Some(ControlFlow::Break)),
                ControlFlow::Continue => return Ok(Some(ControlFlow::Continue)),
                ControlFlow::Value(_) | ControlFlow::Next => {}
            }
        }
        Ok(None)
    }

    fn execute_statement(&mut self, statement: &Stmt) -> Result<ControlFlow, InterpreterError> {
        match statement {
            Stmt::Let { name, value } => {
                let val = self.evaluate(value)?;
                self.env.set(name.to_string(), val);
                Ok(ControlFlow::Next)
            }
            Stmt::Const { name, value } => {
                let val = self.evaluate(value)?;
                // Check if const already exists (immutable)
                if self.env.get(name.as_ref()).is_some() {
                    return Err(InterpreterError::invalid_operation(format!(
                        "Cannot redeclare const '{}'",
                        name
                    )));
                }
                self.env.set(name.to_string(), val);
                Ok(ControlFlow::Next)
            }
            Stmt::Expr(expr) => {
                let val = self.evaluate(expr)?;
                // Assignments are side-effect statements, don't treat as return values
                if matches!(expr.kind, ExprKind::Assignment { .. }) {
                    Ok(ControlFlow::Next)
                } else {
                    Ok(ControlFlow::Value(val))
                }
            }
            Stmt::Block(stmts) => {
                self.env.push_scope();

                let mut result = ControlFlow::Next;
                for s in stmts {
                    match self.execute_statement(s)? {
                        ControlFlow::Return(val) => {
                            result = ControlFlow::Return(val);
                            break;
                        }
                        ControlFlow::Break | ControlFlow::Continue => {
                            self.env.pop_scope();
                            return self.execute_statement(s);
                        }
                        ControlFlow::Value(_) | ControlFlow::Next => {}
                    }
                }

                self.env.pop_scope();
                Ok(result)
            }
            Stmt::If { condition, then_branch, else_branch } => {
                let cond_val = self.evaluate(condition)?;
                if cond_val.is_truthy() {
                    self.execute_statement(&Stmt::Block(then_branch.clone()))
                } else if let Some(else_stmts) = else_branch {
                    self.execute_statement(&Stmt::Block(else_stmts.clone()))
                } else {
                    Ok(ControlFlow::Next)
                }
            }
            Stmt::For { var, iterable, body } => {
                let iter_val = self.evaluate(iterable)?;
                let items_rc = match iter_val {
                    Value::Array(arr) => arr,
                    _ => return Err(InterpreterError::type_error("Cannot iterate over non-array")),
                };
                
                let items = items_rc.borrow();
                for item in items.iter() {
                    self.env.push_scope();
                    self.env.set(var.to_string(), item.clone());

                    let result = self.execute_loop_body(body)?;
                    self.env.pop_scope();
                    
                    match result {
                        Some(ControlFlow::Return(val)) => return Ok(ControlFlow::Return(val)),
                        Some(ControlFlow::Break) => return Ok(ControlFlow::Next),
                        Some(ControlFlow::Continue) => continue,
                        _ => {}
                    }
                }
                
                Ok(ControlFlow::Next)
            }
            Stmt::While { condition, body } => {
                loop {
                    let cond_val = self.evaluate(condition)?;
                    if !cond_val.is_truthy() {
                        break;
                    }
                    
                    self.env.push_scope();
                    let result = self.execute_loop_body(body)?;
                    self.env.pop_scope();
                    
                    match result {
                        Some(ControlFlow::Return(val)) => return Ok(ControlFlow::Return(val)),
                        Some(ControlFlow::Break) => return Ok(ControlFlow::Next),
                        Some(ControlFlow::Continue) => continue,
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
                    self.evaluate(e)?
                } else {
                    Value::Null
                };
                Ok(ControlFlow::Return(val))
            }
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value, InterpreterError> {
        match &expr.kind {
            ExprKind::Literal(val) => Ok(val.clone()),

            ExprKind::Identifier(name) => {
                // First try environment
                if let Some(val) = self.env.get(name.as_ref()) {
                    Ok(val)
                } else if let Some(pipe_val) = self.env.get(PIPE_CONTEXT) {
                    // If not in environment and we're in pipe context, try implicit @ lookup
                    // This handles expressions like `age >= 18` in `{ adult: age >= 18 }`
                    match self.get_field(&pipe_val, name.as_ref()) {
                        Ok(field_val) if !matches!(field_val, Value::Null) => Ok(field_val),
                        _ => Err(InterpreterError::undefined_variable_at(name.to_string(), expr.span)),
                    }
                } else {
                    Err(InterpreterError::undefined_variable_at(name.to_string(), expr.span))
                }
            }

            ExprKind::FieldAccess { object, field } => {
                let obj = self.evaluate(object)?;
                self.get_field(&obj, field)
            }

            ExprKind::OptionalFieldAccess { object, field } => {
                let obj = self.evaluate(object)?;
                if matches!(obj, Value::Null) {
                    Ok(Value::Null)
                } else {
                    self.get_field(&obj, field)
                }
            }

            ExprKind::ArrayIndex { array, index } => {
                let arr = self.evaluate(array)?;
                let idx = self.evaluate(index)?;
                self.get_index(&arr, &idx)
            }

            ExprKind::DeepFieldAccess { object, field } => {
                let obj = self.evaluate(object)?;
                let results = self.deep_find_all(&obj, field);
                if results.len() == 1 {
                    Ok(results[0].clone())
                } else {
                    Ok(Value::Array(Rc::new(RefCell::new(results))))
                }
            }

            ExprKind::Binary { left, op, right } => {
                // Special case: n * expr for replication
                // Applies when: left is a number AND right is NOT a simple expression that evaluates to a number
                // (i.e., right is an object, array, or uses @)
                if matches!(op, BinaryOp::Mul) {
                    let left_val = self.evaluate(left)?;
                    if let Value::Number(n, _) = &left_val {
                        // Check if right looks like a replication template (object, array, or contains @)
                        let is_replication_template = self.is_replication_template(right);
                        
                        if is_replication_template {
                            // It's replication: n * template - evaluate right for each iteration
                            let count = *n as usize;
                            let mut results = Vec::with_capacity(count);
                            for i in 0..count {
                                self.env.push_scope();
                                self.env.set(PIPE_CONTEXT.to_string(), Value::Number(i as f64, false));
                                let val = self.evaluate(right)?;
                                self.env.pop_scope();
                                results.push(val);
                            }
                            return Ok(Value::Array(Rc::new(RefCell::new(results))));
                        }
                        // Not a replication template, do normal evaluation
                    }
                }
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;
                self.eval_binary_op(&left_val, op, &right_val)
            }

            ExprKind::Unary { op, expr } => {
                let val = self.evaluate(expr)?;
                self.eval_unary_op(op, &val)
            }

            ExprKind::Pipe { left, right } => {
                // Special case: `arr | @ as name` followed by another pipe
                // Check if left is a Pipe ending with AsBinding
                // If so, we need to handle this as a combined operation
                if let ExprKind::Pipe { left: inner_left, right: inner_right } = &left.kind
                    && let ExprKind::AsBinding { name, .. } = &inner_right.kind {
                        // Pattern: arr | @ as name | expr
                        // Handle as a named lambda over the array
                        let arr_val = self.evaluate(inner_left)?;
                        return self.eval_pipe_with_named_binding(arr_val, name, right);
                    }
                
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
                let left_val = match self.evaluate(left) {
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
                let result = match self.apply_pipe_operation(left_val, right) {
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

            ExprKind::PipeUpdate { left, path, value } => {
                // Update pipe: left |~ path => value
                // Evaluate left to get the object/array to update
                let left_val = self.evaluate(left)?;
                
                // Extract path segments from the path expression (e.g., .field or [0])
                let path_segments = extract_path_segments(path)
                    .ok_or_else(|| InterpreterError::type_error("PipeUpdate path must be a field access or array index"))?;
                
                // Get the current value at the path (for @ context)
                let current_path_val = self.get_nested_path(&left_val, &path_segments)?;
                
                // Set @ to the current path value before evaluating the value expression
                self.env.push_scope();
                self.env.set(PIPE_CONTEXT.to_string(), current_path_val.clone());
                
                // Evaluate the new value (which can use @)
                let new_val = self.evaluate(value)?;
                
                // Restore scope
                self.env.pop_scope();
                
                // Clone the value to make it mutable, then update it
                let updated = left_val.clone();
                self.set_nested_path(&updated, &path_segments, new_val.clone())?;
                
                Ok(updated)
            }

            ExprKind::Grouped(expr) => self.evaluate(expr),

            ExprKind::Array { elements } => {
                let mut vals = Vec::new();
                for e in elements {
                    vals.push(self.evaluate(e)?);
                }
                Ok(Value::Array(Rc::new(RefCell::new(vals))))
            }

            ExprKind::ArrayWithSpread { elements } => {
                let mut vals = Vec::new();
                for elem in elements {
                    match elem {
                        ArrayElement::Single(e) => {
                            vals.push(self.evaluate(e)?);
                        }
                        ArrayElement::Spread(e) => {
                            let spread_val = self.evaluate(e)?;
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
                    map.insert(k.clone(), self.evaluate(v)?);
                }
                Ok(Value::Object(Rc::new(RefCell::new(map))))
            }

            ExprKind::ObjectWithSpread { entries } => {
                let mut result = Value::Object(Rc::new(RefCell::new(indexmap::IndexMap::new())));
                
                for entry in entries {
                    match entry {
                        ObjectEntry::Field { key, value } => {
                            let val = self.evaluate(value)?;
                            if let Value::Object(map_rc) = &result {
                                map_rc.borrow_mut().insert(key.clone(), val);
                            }
                        }
                        ObjectEntry::PathField { path, value } => {
                            // Handle nested path like `downlink.subcell_id: value`
                            let val = self.evaluate(value)?;
                            result = self.set_path_on_value(result, path, val)?;
                        }
                        ObjectEntry::Shorthand { name } => {
                            // Handle shorthand like { name } meaning { name: name }
                            // In pipe context, prefer @.name (implicit @) over variable lookup
                            let val = if let Some(pipe_val) = self.env.get(PIPE_CONTEXT) {
                                // Try to get field from pipe context first (implicit @ in projections)
                                match self.get_field(&pipe_val, &name) {
                                    Ok(field_val) if !matches!(field_val, Value::Null) => field_val,
                                    _ => {
                                        // Fall back to variable lookup (regular shorthand)
                                        self.env.get(name.as_str())
                                            .ok_or_else(|| InterpreterError::undefined_variable(name.clone()))?
                                            .clone()
                                    }
                                }
                            } else {
                                // No pipe context, use variable lookup
                                self.env.get(name.as_str())
                                    .ok_or_else(|| InterpreterError::undefined_variable(name.clone()))?
                                    .clone()
                            };
                            if let Value::Object(map_rc) = &result {
                                map_rc.borrow_mut().insert(name.clone(), val);
                            }
                        }
                        ObjectEntry::Spread(e) => {
                            let spread_val = self.evaluate(e)?;
                            match spread_val {
                                Value::Object(obj) => {
                                    if let Value::Object(map_rc) = &result {
                                        for (k, v) in obj.borrow().iter() {
                                            map_rc.borrow_mut().insert(k.clone(), v.clone());
                                        }
                                    }
                                }
                                _ => return Err(InterpreterError::type_error("Spread in object requires an object")),
                            }
                        }
                        ObjectEntry::Projection { field } => {
                             // Projection { .name } -> { name: @.name }
                             // Only works if PIPE_CONTEXT is available
                             if let Some(pipe_val) = self.env.get(PIPE_CONTEXT) {
                                 let val = match self.get_field(&pipe_val, field) {
                                     Ok(v) => v,
                                     Err(_) => Value::Null, // Or undefined variable error? Usually projection allows nulls.
                                 };
                                 if let Value::Object(map_rc) = &result {
                                     map_rc.borrow_mut().insert(field.clone(), val);
                                 }
                             } else {
                                 return Err(InterpreterError::undefined_variable(field.clone()));
                             }
                        }
                    }
                }
                Ok(result)
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
                    arg_vals.push(self.evaluate(a)?);
                }
                
                if let Some(func_val) = self.env.get(name.as_ref())
                    && let Value::Function(func) = func_val {
                        return self.call_user_function(&func, arg_vals);
                    }
                
                self.call_function(name, arg_vals)
            }

            ExprKind::Assignment { target, value } => {
                let val = self.evaluate(value)?;
                self.perform_assignment(target, val)
            }

            ExprKind::Range { start, end } => {
                let s = self.evaluate(start)?;
                let e = self.evaluate(end)?;
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
                let cond_val = self.evaluate(condition)?;
                if cond_val.is_truthy() {
                    self.evaluate(then_branch)
                } else {
                    self.evaluate(else_branch)
                }
            }

            ExprKind::NullCoalesce { left, right } => {
                let left_val = self.evaluate(left)?;
                if matches!(left_val, Value::Null) {
                    self.evaluate(right)
                } else {
                    Ok(left_val)
                }
            }

            ExprKind::TemplateLiteral { parts } => {
                use crate::ast::TemplatePart;
                let mut result = String::new();
                for part in parts {
                    match part {
                        TemplatePart::Literal(s) => result.push_str(s),
                        TemplatePart::Interpolation(expr) => {
                            let val = self.evaluate(expr)?;
                            result.push_str(&value_to_string(&val));
                        }
                    }
                }
                Ok(Value::String(Rc::from(result)))
            }

            ExprKind::Match { value, arms } => {
                use crate::ast::MatchPattern;
                let val = self.evaluate(value)?;
                
                for (pattern, result_expr) in arms {
                    match pattern {
                        MatchPattern::Wildcard => {
                            return self.evaluate(result_expr);
                        }
                        MatchPattern::Literal(pattern_val) => {
                            if values_equal(&val, pattern_val) {
                                return self.evaluate(result_expr);
                            }
                        }
                        MatchPattern::Range { start, end } => {
                            // Range pattern: check if value is within range (inclusive)
                            let start_val = self.evaluate(start)?;
                            let end_val = self.evaluate(end)?;
                            
                            if let (Value::Number(val_num, _), Value::Number(start_num, _), Value::Number(end_num, _)) = 
                                (&val, &start_val, &end_val) {
                                if *val_num >= *start_num && *val_num <= *end_num {
                                    return self.evaluate(result_expr);
                                }
                            }
                        }
                    }
                }
                
                Err(InterpreterError::invalid_operation_at(
                    "No matching pattern in match expression",
                    expr.span,
                ))
            }
            
            ExprKind::MethodCall { object, method, args } => {
                // Method call: obj.method(args) -> method(obj, args)
                let obj_val = self.evaluate(object)?;
                let mut all_args = vec![obj_val];
                for a in args {
                    all_args.push(self.evaluate(a)?);
                }
                
                // Try user-defined function first
                if let Some(func_val) = self.env.get(method.as_str())
                    && let Value::Function(func) = func_val {
                        return self.call_user_function(&func, all_args);
                    }
                
                // Call builtin
                self.call_function(method, all_args)
            }
            
            ExprKind::ModuleCall { module, function, args } => {
                // Evaluate the module expression
                let module_val = self.evaluate(module)?;
                
                // Get the function from the module
                if let Value::Module(mod_ref) = module_val {
                    let func_val = mod_ref.get(function).ok_or_else(|| {
                        InterpreterError::invalid_operation_at(
                            format!("Function '{}' not found in module '{}'", function, mod_ref.name),
                            expr.span,
                        )
                    })?;
                    
                    // Evaluate arguments
                    let mut arg_vals = Vec::new();
                    for a in args {
                        arg_vals.push(self.evaluate(a)?);
                    }
                    
                    // Call the function
                    if let Value::Function(func) = func_val {
                        self.call_user_function(func, arg_vals)
                    } else {
                        Err(InterpreterError::type_error_at(
                            format!("'{}' is not a function in module '{}'", function, mod_ref.name),
                            expr.span,
                        ))
                    }
                } else {
                    Err(InterpreterError::type_error_at(
                        "Expected a module value before '::'",
                        module.span,
                    ))
                }
            }
            
            ExprKind::AsBinding { expr: inner, name } => {
                // Evaluate and bind to name in current scope
                let val = self.evaluate(inner)?;
                self.env.set(name.to_string(), val.clone());
                Ok(val)
            }
            
            ExprKind::Replicate { count, template } => {
                // n * expr with @ as index
                let count_val = self.evaluate(count)?;
                let n = match count_val {
                    Value::Number(n, _) => n as usize,
                    _ => return Err(InterpreterError::type_error("Replicate count must be a number")),
                };
                
                let mut results = Vec::with_capacity(n);
                for i in 0..n {
                    self.env.push_scope();
                    self.env.set(PIPE_CONTEXT.to_string(), Value::Number(i as f64, false));
                    let val = self.evaluate(template)?;
                    self.env.pop_scope();
                    results.push(val);
                }
                Ok(Value::Array(Rc::new(RefCell::new(results))))
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
                if let Some(path) = extract_path_segments(target) {
                    // This is a short access chain like .profile.age or .items[0].name
                    if let Some(it_val) = self.env.get(PIPE_CONTEXT) {
                        self.set_nested_path(&it_val, &path, value.clone())?;
                        return Ok(value);
                    }
                    // Fall back to root
                    if let Some(root_val) = self.env.get(ROOT_CONTEXT) {
                        self.set_nested_path(&root_val, &path, value.clone())?;
                        return Ok(value);
                    }
                    return Err(InterpreterError::type_error("Cannot set field - no context object"));
                }
                
                // Not a short field access chain, evaluate the object
                let obj_val = self.evaluate(object)?;
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
                if let Some(path) = extract_path_segments(target) {
                    // This is a short access chain like .items[0] or .data[0].value
                    if let Some(it_val) = self.env.get(PIPE_CONTEXT) {
                        self.set_nested_path(&it_val, &path, value.clone())?;
                        return Ok(value);
                    }
                    // Fall back to root
                    if let Some(root_val) = self.env.get(ROOT_CONTEXT) {
                        self.set_nested_path(&root_val, &path, value.clone())?;
                        return Ok(value);
                    }
                    return Err(InterpreterError::type_error("Cannot set index - no context object"));
                }

                let idx_val = self.evaluate(index)?;
                let idx = match idx_val {
                    Value::Number(n, _) => n as usize,
                    _ => return Err(InterpreterError::type_error("Index must be a number")),
                };

                let arr_val = self.evaluate(array)?;
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
    
    fn apply_pipe_operation(&mut self, left: Value, right: &Expr) -> Result<Value, InterpreterError> {
        // Check if the right side is a short array index [n] - index the result directly
        if let ExprKind::ArrayIndex { array, index } = &right.kind
            && matches!(array.kind, ExprKind::Literal(Value::Null)) {
                let idx = self.evaluate(index)?;
                let val = self.get_index(&left, &idx)?;
                if matches!(left, Value::Array(_)) {
                    return Ok(Value::Array(Rc::new(RefCell::new(vec![val]))));
                }
                return Ok(val);
            }
        
        // Check if the right side is `@ as name | expr` - like a named lambda
        // This handles: arr | @ as x | expr where x is bound per element
        if let ExprKind::Pipe { left: binding_expr, right: body } = &right.kind
            && let ExprKind::AsBinding { expr: inner, name } = &binding_expr.kind {
                // Treat `arr | @ as x | expr` like a named lambda
                return self.eval_pipe_with_as_binding(left, inner, name, body);
            }
        
        // NOTE: `@ as name` bindings are handled in the general iteration logic below
        // This ensures per-element binding for arrays
        
        // Check if the right side is a lambda - use it for map/filter
        if let ExprKind::Lambda { params, body } = &right.kind {
            return self.eval_pipe_with_lambda(left, params, body);
        }
        
        // Check if the right side is a bare identifier - call as function with left as arg
        // This allows: arr | sum, arr | flat, str | upper
        if let ExprKind::Identifier(name) = &right.kind {
            // Don't treat @ or other special vars as functions
            if name.as_ref() != "@" && name.as_ref() != "root" {
                // Try user-defined function
                if let Some(func_val) = self.env.get(name.as_ref())
                    && let Value::Function(func) = func_val {
                        return self.call_user_function(&func, vec![left]);
                    }
                // Try builtin
                if let Ok(result) = self.call_function(name, vec![left.clone()]) {
                    return Ok(result);
                }
            }
        }
        
        // Check if the right side is a method call - call with left prepended
        if let ExprKind::MethodCall { object, method, args } = &right.kind {
            // Evaluate object in context of left
            self.env.push_scope();
            self.env.set(PIPE_CONTEXT.to_string(), left.clone());
            let obj_val = self.evaluate(object)?;
            let mut all_args = vec![obj_val];
            for a in args {
                all_args.push(self.evaluate(a)?);
            }
            self.env.pop_scope();
            
            if let Some(func_val) = self.env.get(method.as_str())
                && let Value::Function(func) = func_val {
                    return self.call_user_function(&func, all_args);
                }
            return self.call_function(method, all_args);
        }
        
        // Check if the right side is a function call - if so, prepend left as first argument
        if let ExprKind::Call { name, args } = &right.kind {
            let mut arg_vals = vec![left];
            for a in args {
                arg_vals.push(self.evaluate(a)?);
            }
            
            if let Some(func_val) = self.env.get(name.as_ref())
                && let Value::Function(func) = func_val {
                    return self.call_user_function(&func, arg_vals);
                }
            
            return self.call_function(name, arg_vals);
        }
        
        // Check if expression is a binary operation on a short field/index access (.field + 2, .profile.age + 5, or .items[0] + 1)
        // These should automatically mutate the field and return the modified object
        // This makes `.id + 2` equivalent to `.id += 2` in pipe context
        let mutation_info = get_pipe_mutation_info(right);

        // Check if expression is an assignment on a short field/index access (.field = value, .profile.age = value, or .items[0] = value)
        let assignment_path = get_pipe_assignment_path(right);
        
        match left {
            Value::Array(items_rc) => {
                let items = items_rc.borrow();
                let mut results = Vec::with_capacity(items.len());
                
                for item in items.iter() {
                    self.env.push_scope();
                    self.env.set(PIPE_CONTEXT.to_string(), item.clone());

                    if let Some((path, op, value_expr)) = &mutation_info {
                        let current = self.get_nested_path(item, path)?;
                        let right_val = self.evaluate(value_expr)?;
                        let new_val = self.eval_binary_op(&current, op, &right_val)?;
                        
                        if let Some(it_val) = self.env.get(PIPE_CONTEXT) {
                            self.set_nested_path(&it_val, path, new_val)?;
                        }
                        
                        if let Some(modified_item) = self.env.get(PIPE_CONTEXT) {
                            results.push(modified_item);
                        } else {
                            results.push(item.clone());
                        }
                    } else if let Some(path) = &assignment_path {
                        if let ExprKind::Assignment { value, .. } = &right.kind {
                            let new_val = self.evaluate(value)?;
                            if let Some(it_val) = self.env.get(PIPE_CONTEXT) {
                                self.set_nested_path(&it_val, path, new_val)?;
                            }
                        }
                        if let Some(modified_item) = self.env.get(PIPE_CONTEXT) {
                            results.push(modified_item);
                        } else {
                            results.push(item.clone());
                        }
                    } else {
                        let res = self.evaluate(right)?;
                        match res {
                            Value::Bool(b) => {
                                if b { results.push(item.clone()); }
                            }
                            other => results.push(other),
                        }
                    }

                    self.env.pop_scope();
                }

                Ok(Value::Array(Rc::new(RefCell::new(results))))
            }
            other => {
                self.env.push_scope();
                self.env.set(PIPE_CONTEXT.to_string(), other.clone());
                
                let result = if let Some((path, op, value_expr)) = &mutation_info {
                    let current = self.get_nested_path(&other, path)?;
                    let right_val = self.evaluate(value_expr)?;
                    let new_val = self.eval_binary_op(&current, op, &right_val)?;
                    
                    if let Some(it_val) = self.env.get(PIPE_CONTEXT) {
                        self.set_nested_path(&it_val, path, new_val)?;
                    }
                    
                    self.env.get(PIPE_CONTEXT).unwrap_or(other)
                } else if let Some(path) = &assignment_path {
                    if let ExprKind::Assignment { value, .. } = &right.kind {
                        let new_val = self.evaluate(value)?;
                        if let Some(it_val) = self.env.get(PIPE_CONTEXT) {
                            self.set_nested_path(&it_val, path, new_val)?;
                        }
                    }
                    self.env.get(PIPE_CONTEXT).unwrap_or(other)
                } else {
                    self.evaluate(right)?
                };
                
                self.env.pop_scope();
                Ok(result)
            }
        }
    }
    
    /// Evaluate a pipe chain with a named binding: `arr | @ as name | expr`
    /// For each element, binds name to the element and evaluates the body expression
    fn eval_pipe_with_named_binding(&mut self, left: Value, name: &Rc<str>, body: &Expr) -> Result<Value, InterpreterError> {
        match left {
            Value::Array(items_rc) => {
                let items = items_rc.borrow();
                let mut results = Vec::with_capacity(items.len());
                
                for item in items.iter() {
                    self.env.push_scope();
                    self.env.set(name.to_string(), item.clone());
                    self.env.set(PIPE_CONTEXT.to_string(), item.clone());
                    
                    // Evaluate the body (which could be any expression or another pipe)
                    let res = self.evaluate(body)?;
                    
                    match res {
                        Value::Bool(b) => {
                            if b { results.push(item.clone()); }
                        }
                        other => results.push(other),
                    }
                    
                    self.env.pop_scope();
                }
                
                Ok(Value::Array(Rc::new(RefCell::new(results))))
            }
            other => {
                self.env.push_scope();
                self.env.set(name.to_string(), other.clone());
                self.env.set(PIPE_CONTEXT.to_string(), other.clone());
                
                let res = self.evaluate(body)?;
                self.env.pop_scope();
                Ok(res)
            }
        }
    }
    
    /// Evaluate a pipe with an `as` binding: `arr | @ as name | expr`
    /// For each element, binds name to the element and continues piping
    fn eval_pipe_with_as_binding(&mut self, left: Value, _inner: &Expr, name: &Rc<str>, body: &Expr) -> Result<Value, InterpreterError> {
        match left {
            Value::Array(items_rc) => {
                let items = items_rc.borrow();
                let mut results = Vec::with_capacity(items.len());
                
                for item in items.iter() {
                    self.env.push_scope();
                    self.env.set(name.to_string(), item.clone());
                    self.env.set(PIPE_CONTEXT.to_string(), item.clone());
                    
                    // Continue piping - the body might be another pipe or expression
                    let res = self.evaluate(body)?;
                    
                    match res {
                        Value::Bool(b) => {
                            if b { results.push(item.clone()); }
                        }
                        other => results.push(other),
                    }
                    
                    self.env.pop_scope();
                }
                
                Ok(Value::Array(Rc::new(RefCell::new(results))))
            }
            other => {
                self.env.push_scope();
                self.env.set(name.to_string(), other.clone());
                self.env.set(PIPE_CONTEXT.to_string(), other.clone());
                
                let res = self.evaluate(body)?;
                self.env.pop_scope();
                Ok(res)
            }
        }
    }
    
    /// Evaluate a pipe with a lambda - handles both map and filter based on return type
    fn eval_pipe_with_lambda(&mut self, left: Value, params: &[Param], body: &Expr) -> Result<Value, InterpreterError> {
        match left {
            Value::Array(items_rc) => {
                let items = items_rc.borrow();
                let mut results = Vec::with_capacity(items.len());
                
                for item in items.iter() {
                    self.env.push_scope();
                    
                    if !params.is_empty() {
                        self.env.set(params[0].name.to_string(), item.clone());
                    }
                    self.env.set(PIPE_CONTEXT.to_string(), item.clone());
                    
                    let res = self.evaluate(body)?;
                    
                    match res {
                        Value::Bool(b) => {
                            if b { results.push(item.clone()); }
                        }
                        other => {
                            results.push(other);
                        }
                    }
                    
                    self.env.pop_scope();
                }
                
                Ok(Value::Array(Rc::new(RefCell::new(results))))
            }
            other => {
                self.env.push_scope();
                
                if !params.is_empty() {
                    self.env.set(params[0].name.to_string(), other.clone());
                }
                self.env.set(PIPE_CONTEXT.to_string(), other);
                
                let res = self.evaluate(body)?;
                self.env.pop_scope();
                Ok(res)
            }
        }
    }

    /// Count the number of pipe operations in a pipe chain.
    /// For `a | b | c`, this returns 2 (2 pipe operators, so 3 steps total).
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
}

/// Extract path segments from an expression (for short field/index access like .field or .items[0])
fn extract_path_segments(expr: &Expr) -> Option<Vec<AccessPathSegment>> {
    fn extract_recursive(expr: &Expr, segments: &mut Vec<AccessPathSegment>) -> bool {
        match &expr.kind {
            ExprKind::Literal(Value::Null) => true, // Base case for short access
            ExprKind::FieldAccess { object, field } => {
                if extract_recursive(object, segments) {
                    segments.push(AccessPathSegment::Field(field.clone()));
                    true
                } else {
                    false
                }
            }
            ExprKind::ArrayIndex { array, index } => {
                match &index.kind {
                    ExprKind::Literal(Value::Number(n, _)) => {
                        if extract_recursive(array, segments) {
                            segments.push(AccessPathSegment::Index(*n as usize));
                            true
                        } else {
                            false
                        }
                    }
                    ExprKind::Unary { op: UnaryOp::Neg, expr } => {
                        if let ExprKind::Literal(Value::Number(n, _)) = &expr.kind {
                             if extract_recursive(array, segments) {
                                segments.push(AccessPathSegment::NegativeIndex(- (*n as i64)));
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }
    
    let mut segments = Vec::new();
    if extract_recursive(expr, &mut segments) {
        Some(segments)
    } else {
        None
    }
}

/// Extract mutation info from a binary expression on a short path (e.g., .field + 2)
fn get_pipe_mutation_info(expr: &Expr) -> Option<(Vec<AccessPathSegment>, BinaryOp, Expr)> {
    if let ExprKind::Binary { left, op, right } = &expr.kind {
        // Only handle arithmetic operations
        if matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod) {
            if let Some(path) = extract_path_segments(left) {
                return Some((path, op.clone(), *right.clone()));
            }
        }
    }
    None
}

/// Extract assignment path from an assignment expression (e.g., .field = value)
fn get_pipe_assignment_path(expr: &Expr) -> Option<Vec<AccessPathSegment>> {
    if let ExprKind::Assignment { target, .. } = &expr.kind {
        extract_path_segments(target)
    } else {
        None
    }
}

impl Interpreter {
    /// Get a value at a nested path (fields and/or array indices) from an object
    fn get_nested_path(&self, obj: &Value, path: &[AccessPathSegment]) -> Result<Value, InterpreterError> {
        if path.is_empty() {
            return Ok(obj.clone());
        }
        
        let mut current = obj.clone();
        for segment in path {
            current = match segment {
                AccessPathSegment::Field(field) => {
                    self.get_field(&current, field)?
                }
                AccessPathSegment::Index(idx) => {
                    self.get_index(&current, &Value::Number(*idx as f64, false))?
                }
                AccessPathSegment::NegativeIndex(idx) => {
                    self.get_index(&current, &Value::Number(*idx as f64, false))?
                }
            };
        }
        Ok(current)
    }
    
    /// Set a value at a nested path (fields and/or array indices), modifying it in place
    fn set_nested_path(&self, obj: &Value, path: &[AccessPathSegment], value: Value) -> Result<(), InterpreterError> {
        if path.is_empty() {
            return Err(InterpreterError::invalid_operation("Cannot set empty path"));
        }
        
        if path.len() == 1 {
            // Single segment - set directly
            return match &path[0] {
                AccessPathSegment::Field(field) => {
                    if let Value::Object(map_rc) = obj {
                        map_rc.borrow_mut().insert(field.clone(), value);
                        Ok(())
                    } else {
                        Err(InterpreterError::type_error("Cannot set field on non-object"))
                    }
                }
                AccessPathSegment::Index(idx) => {
                    self.set_index_on_value(obj, *idx as i64, value)
                }
                AccessPathSegment::NegativeIndex(idx) => {
                    self.set_index_on_value(obj, *idx, value)
                }
            };
        }
        
        // Navigate to the parent container, then set the final segment
        let parent_path = &path[..path.len() - 1];
        let parent = self.get_nested_path(obj, parent_path)?;
        
        match &path[path.len() - 1] {
            AccessPathSegment::Field(field) => {
                if let Value::Object(map_rc) = parent {
                    map_rc.borrow_mut().insert(field.clone(), value);
                    Ok(())
                } else {
                    Err(InterpreterError::type_error("Cannot set field on non-object"))
                }
            }
            AccessPathSegment::Index(idx) => {
                self.set_index_on_value(&parent, *idx as i64, value)
            }
            AccessPathSegment::NegativeIndex(idx) => {
                self.set_index_on_value(&parent, *idx, value)
            }
        }
    }

    fn set_index_on_value(&self, obj: &Value, idx: i64, value: Value) -> Result<(), InterpreterError> {
        if let Value::Array(items_rc) = obj {
            let mut items = items_rc.borrow_mut();
            let real_idx = if idx < 0 {
                let r = items.len() as i64 + idx;
                if r < 0 {
                    return Err(InterpreterError::index_out_of_bounds(idx as usize, items.len()));
                }
                r as usize
            } else {
                idx as usize
            };

            if real_idx < items.len() {
                items[real_idx] = value;
                Ok(())
            } else {
                Err(InterpreterError::index_out_of_bounds(real_idx, items.len()))
            }
        } else {
            Err(InterpreterError::type_error("Cannot index non-array"))
        }
    }
    
    /// Set a value at a dot-separated path on an object, creating nested objects as needed.
    /// Used for object literal path fields like `{ downlink.subcell_id: value }`.
    fn set_path_on_value(&self, obj: Value, path: &[String], value: Value) -> Result<Value, InterpreterError> {
        if path.is_empty() {
            return Ok(value);
        }
        
        fn set_recursive(obj: &Value, parts: &[String], value: &Value) -> Value {
            if parts.is_empty() {
                return value.clone();
            }
            
            let key = &parts[0];
            let remaining = &parts[1..];
            
            match obj {
                Value::Object(map_rc) => {
                    let mut map = map_rc.borrow().clone();
                    let inner = map
                        .get(key)
                        .cloned()
                        .unwrap_or(Value::Object(Rc::new(RefCell::new(indexmap::IndexMap::new()))));
                    map.insert(key.clone(), set_recursive(&inner, remaining, value));
                    Value::Object(Rc::new(RefCell::new(map)))
                }
                _ => {
                    let mut map = indexmap::IndexMap::new();
                    let inner = Value::Object(Rc::new(RefCell::new(indexmap::IndexMap::new())));
                    map.insert(key.clone(), set_recursive(&inner, remaining, value));
                    Value::Object(Rc::new(RefCell::new(map)))
                }
            }
        }
        
        Ok(set_recursive(&obj, path, &value))
    }

    fn deep_find_all(&self, obj: &Value, field: &str) -> Vec<Value> {
        let mut results = Vec::new();
        self.deep_find_recursive(obj, field, &mut results);
        results
    }

    fn deep_find_recursive(&self, obj: &Value, field: &str, results: &mut Vec<Value>) {
        match obj {
            Value::Object(map_rc) => {
                let map = map_rc.borrow();
                // Check if current object has field
                if let Some(val) = map.get(field) {
                    results.push(val.clone());
                }
                // Recurse into children values
                for v in map.values() {
                    self.deep_find_recursive(v, field, results);
                }
            }
            Value::Array(items_rc) => {
                for v in items_rc.borrow().iter() {
                    self.deep_find_recursive(v, field, results);
                }
            }
            _ => {}
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
        if matches!(name, "find" | "find_index" | "reduce" | "every" | "some" | "replicate" | "flat_map") {
            return match name {
                "find" => builtins::builtin_find(&args, |func, call_args| self.call_function_value(func, call_args)),
                "find_index" => builtins::builtin_find_index(&args, |func, call_args| self.call_function_value(func, call_args)),
                "reduce" => builtins::builtin_reduce(&args, |func, call_args| self.call_function_value(func, call_args)),
                "every" => builtins::builtin_every(&args, |func, call_args| self.call_function_value(func, call_args)),
                "some" => builtins::builtin_some(&args, |func, call_args| self.call_function_value(func, call_args)),
                "replicate" => builtins::builtin_replicate(&args, |func, call_args| self.call_function_value(func, call_args)),
                "flat_map" => builtins::builtin_flat_map(&args, |func, call_args| self.call_function_value(func, call_args)),
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
            "print" => builtins::builtin_print(&args, |v| value_to_string(v)),
            "unique" => builtins::builtin_unique(&args, |a, b| deep_equals(a, b)),
            "sort_by" => builtins::builtin_sort_by(&args, |v, f| self.get_field(v, f)),
            "group_by" => builtins::builtin_group_by(&args, |v, f| self.get_field(v, f)),
            "include" => self.builtin_include(&args),
            "import" => self.builtin_import(&args),
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
            "join" => builtins::builtin_join(&args, |v| value_to_string(v)),
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
            "to_string" => builtins::builtin_to_string(&args, |v| value_to_string(v)),
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
        // Check if we have too many arguments
        if args.len() > func.params.len() {
             return Err(InterpreterError::invalid_operation(format!(
                "Function expects at most {} arguments, got {}",
                func.params.len(),
                args.len()
            )));
        }

        self.env.push_scope();

        let mut args_iter = args.into_iter();

        for param in &func.params {
            if let Some(arg) = args_iter.next() {
                self.env.set(param.name.to_string(), arg.clone());
            } else if let Some(default_expr) = &param.default {
                let val = self.evaluate(default_expr)?;
                self.env.set(param.name.to_string(), val);
            } else {
                 return Err(InterpreterError::invalid_operation(format!("Missing argument for parameter '{}'", param.name)));
            }
        }

        let result = if let Some(ref body_expr) = func.body_expr {
            self.evaluate(body_expr)?
        } else if let Some(ref body_stmts) = func.body_stmts {
            let mut last_val = Value::Null;
            for stmt in body_stmts {
                match self.execute_statement(stmt)? {
                    ControlFlow::Return(val) => {
                        self.env.pop_scope();
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

        self.env.pop_scope();
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

    fn builtin_import(&mut self, args: &[Value]) -> Result<Value, InterpreterError> {
        use crate::value::Module;
        use indexmap::IndexMap;
        
        if args.is_empty() {
            return Err(InterpreterError::invalid_operation("import requires a file path argument"));
        }
        if let Value::String(path) = &args[0] {
            let script_content = std::fs::read_to_string(path.as_ref())
                .map_err(|e| InterpreterError::invalid_operation(format!("Failed to read module file '{}': {}", path, e)))?;
            
            let lexer = crate::lexer::lexer();
            let tokens = lexer
                .parse(&script_content)
                .into_result()
                .map_err(|e| InterpreterError::invalid_operation(format!("Lexer error in imported module: {:?}", e)))?;
            
            let mut parser = TokenParser::from_lexer_output(tokens, script_content.len());
            let stmts = parser
                .parse()
                .map_err(|e| InterpreterError::invalid_operation(format!("Parser error in imported module: {}", e)))?;
            
            // Create a new interpreter to execute the module in isolation
            let mut module_interpreter = Interpreter::new();
            module_interpreter.run(stmts)?;
            
            // Extract all functions and variables from the module's environment
            let mut exports: IndexMap<String, Value> = IndexMap::new();
            for (name, value) in module_interpreter.env.get_all_bindings() {
                // Export functions and other values (but skip @ and root)
                if name != PIPE_CONTEXT && name != ROOT_CONTEXT {
                    exports.insert(name, value);
                }
            }
            
            // Extract module name from path
            let module_name = std::path::Path::new(path.as_ref())
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("module")
                .to_string();
            
            Ok(Value::Module(Rc::new(Module {
                name: module_name,
                exports,
            })))
        } else {
            Err(InterpreterError::type_error("import requires a string path argument"))
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
                if let Some(it) = self.env.get(PIPE_CONTEXT)
                    && let Value::Object(map) = it {
                        return map.borrow().get(field).cloned()
                            .ok_or_else(|| InterpreterError::field_not_found(field));
                    }
                // Fall back to root (for top-level .field access)
                if let Some(root) = self.env.get(ROOT_CONTEXT)
                    && let Value::Object(map) = root {
                        return Ok(map.borrow().get(field).cloned().unwrap_or(Value::Null));
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
                let len = items.borrow().len();
                // Handle negative indices: -1 is last element, -2 is second to last, etc.
                let index = if *n < 0.0 {
                    let neg_idx = (-*n) as usize;
                    if neg_idx > len {
                        return Err(InterpreterError::type_error(format!("Index -{} out of bounds for array of length {}", neg_idx, len)));
                    }
                    len - neg_idx
                } else {
                    *n as usize
                };
                items
                    .borrow()
                    .get(index)
                    .cloned()
                    .ok_or_else(|| InterpreterError::index_out_of_bounds(index, len))
            }
            (Value::Null, Value::Number(n, _)) => {
                // Try pipe context first (for [n] in pipe operations)
                if let Some(it) = self.env.get(PIPE_CONTEXT)
                    && let Value::Array(items) = it {
                        let len = items.borrow().len();
                        let index = if *n < 0.0 {
                            let neg_idx = (-*n) as usize;
                            if neg_idx > len {
                                return Err(InterpreterError::type_error(format!("Index -{} out of bounds for array of length {}", neg_idx, len)));
                            }
                            len - neg_idx
                        } else {
                            *n as usize
                        };
                        return items
                            .borrow()
                            .get(index)
                            .cloned()
                            .ok_or_else(|| InterpreterError::index_out_of_bounds(index, len));
                    }
                // Fall back to root
                if let Some(root) = self.env.get(ROOT_CONTEXT)
                    && let Value::Array(items) = root {
                        let len = items.borrow().len();
                        let index = if *n < 0.0 {
                            let neg_idx = (-*n) as usize;
                            if neg_idx > len {
                                return Err(InterpreterError::type_error(format!("Index -{} out of bounds for array of length {}", neg_idx, len)));
                            }
                            len - neg_idx
                        } else {
                            *n as usize
                        };
                        return items
                            .borrow()
                            .get(index)
                            .cloned()
                            .ok_or_else(|| InterpreterError::index_out_of_bounds(index, len));
                    }
                Err(InterpreterError::type_error("Cannot index null"))
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
            (left_val, BinaryOp::Eq, right_val) => Ok(Value::Bool(values_equal(left_val, right_val))),
            (left_val, BinaryOp::NotEq, right_val) => Ok(Value::Bool(!values_equal(left_val, right_val))),
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

    /// Check if an expression looks like a replication template (contains @, is an object, or is an array)
    fn is_replication_template(&self, expr: &Expr) -> bool {
        match &expr.kind {
            // Objects and arrays are replication templates
            ExprKind::Object { .. } | ExprKind::ObjectWithSpread { .. } 
            | ExprKind::Array { .. } | ExprKind::ArrayWithSpread { .. } => true,
            // @ by itself means we're using the index
            ExprKind::Identifier(name) if name.as_ref() == "@" => true,
            // Check for @ usage in nested expressions
            ExprKind::FieldAccess { object, .. } => self.is_replication_template(object),
            ExprKind::Binary { left, right, .. } => {
                self.is_replication_template(left) || self.is_replication_template(right)
            }
            ExprKind::Unary { expr, .. } => self.is_replication_template(expr),
            ExprKind::Grouped(inner) => self.is_replication_template(inner),
            ExprKind::Call { args, .. } => args.iter().any(|a| self.is_replication_template(a)),
            ExprKind::TemplateLiteral { parts } => {
                parts.iter().any(|p| {
                    if let crate::ast::TemplatePart::Interpolation(e) = p {
                        self.is_replication_template(e)
                    } else {
                        false
                    }
                })
            }
            // Lambdas with body containing @ - treat as template
            ExprKind::Lambda { body, .. } => self.is_replication_template(body),
            // Otherwise it's probably a regular value
            _ => false,
        }
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
    fn test_simple_assignment_returns_root() {
        // When no explicit return, returns root by default
        let source = "let x = 5;";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert_eq!(result, Some(Value::Null)); // Returns root (which is null)
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

