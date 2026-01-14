use crate::ast::{ArrayElement, BinaryOp, Expr, ExprKind, ObjectEntry, Stmt, UnaryOp, PipeOp, FunctionParam, FilterPattern, PatternMatcher, AggregateExpr};
use crate::value::{Function, Value};
use super::builtins;
use super::control_flow::ControlFlow;
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

/// Represents a segment in an access path for nested field/index operations.
/// For example, `.items[0].active` would be represented as:
/// `[Field("items"), Index(0), Field("active")]`
#[derive(Debug, Clone)]
enum AccessPathSegment {
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

    fn execute_statement(&mut self, statement: &Stmt) -> Result<ControlFlow, InterpreterError> {
        match statement {
            Stmt::Let { name, value } => {
                let val = self.evaluate(value)?;
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
                            return Ok(self.execute_statement(s)?);
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

                    let mut result = ControlFlow::Next;
                    for s in body {
                        match self.execute_statement(s)? {
                            ControlFlow::Return(val) => {
                                result = ControlFlow::Return(val);
                                break;
                            }
                            ControlFlow::Break => {
                                self.env.pop_scope();
                                return Ok(ControlFlow::Next);
                            }
                            ControlFlow::Continue => {
                                result = ControlFlow::Continue;
                                break;
                            }
                            ControlFlow::Value(_) | ControlFlow::Next => {}
                        }
                    }

                    self.env.pop_scope();
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
                    let cond_val = self.evaluate(condition)?;
                    if !cond_val.is_truthy() {
                        break;
                    }
                    
                    self.env.push_scope();
                    
                    let mut result = ControlFlow::Next;
                    for s in body {
                        match self.execute_statement(s)? {
                            ControlFlow::Return(val) => {
                                result = ControlFlow::Return(val);
                                break;
                            }
                            ControlFlow::Break => {
                                self.env.pop_scope();
                                return Ok(ControlFlow::Next);
                            }
                            ControlFlow::Continue => {
                                result = ControlFlow::Continue;
                                break;
                            }
                            ControlFlow::Value(_) | ControlFlow::Next => {}
                        }
                    }
                    
                    self.env.pop_scope();
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

            ExprKind::Identifier(name) => self
                .env
                .get(name.as_ref())
                .ok_or_else(|| InterpreterError::undefined_variable_at(name.to_string(), expr.span)),

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
                if let ExprKind::Pipe { left: inner_left, right: inner_right } = &left.kind {
                    if let ExprKind::AsBinding { name, .. } = &inner_right.kind {
                        // Pattern: arr | @ as name | expr
                        // Handle as a named lambda over the array
                        let arr_val = self.evaluate(inner_left)?;
                        return self.eval_pipe_with_named_binding(arr_val, name, right);
                    }
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
                            let val = self.env.get(name.as_str())
                                .ok_or_else(|| InterpreterError::undefined_variable(name.clone()))?;
                            if let Value::Object(map_rc) = &result {
                                map_rc.borrow_mut().insert(name.clone(), val);
                            }
                        }
                        ObjectEntry::ProjectionField { path } => {
                            // Handle projection shorthand: { .field } or { .nested.field }
                            // Gets value from current context (@) or root
                            let context = self.env.get(PIPE_CONTEXT)
                                .or_else(|| self.env.get(ROOT_CONTEXT))
                                .ok_or_else(|| InterpreterError::invalid_operation("No context for projection"))?;
                            
                            let val = self.get_nested_path(&context, &path.iter()
                                .map(|s| AccessPathSegment::Field(s.clone()))
                                .collect::<Vec<_>>())?;
                            
                            // Use the last part of the path as the key
                            let key = path.last().cloned().unwrap_or_default();
                            if let Value::Object(map_rc) = &result {
                                map_rc.borrow_mut().insert(key, val);
                            }
                        }
                        ObjectEntry::ProjectionRename { new_name, path } => {
                            // Handle projection with rename: { newName: .field }
                            let context = self.env.get(PIPE_CONTEXT)
                                .or_else(|| self.env.get(ROOT_CONTEXT))
                                .ok_or_else(|| InterpreterError::invalid_operation("No context for projection"))?;
                            
                            let val = self.get_nested_path(&context, &path.iter()
                                .map(|s| AccessPathSegment::Field(s.clone()))
                                .collect::<Vec<_>>())?;
                            
                            if let Value::Object(map_rc) = &result {
                                map_rc.borrow_mut().insert(new_name.clone(), val);
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
                    }
                }
                Ok(result)
            }

            ExprKind::Spread(_) => {
                Err(InterpreterError::invalid_operation("Spread operator can only be used inside arrays or objects"))
            }

            ExprKind::Lambda { params, body } => {
                // Convert simple params to FunctionParams
                let func_params: Vec<FunctionParam> = params.iter()
                    .map(|p| FunctionParam { name: p.clone(), default: None })
                    .collect();
                let func = Function {
                    params: func_params,
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
                
                if let Some(func_val) = self.env.get(name.as_ref()) {
                    if let Value::Function(func) = func_val {
                        return self.call_user_function(&func, arg_vals);
                    }
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
                            result.push_str(&self.value_to_string(&val));
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
                            if self.values_equal(&val, pattern_val) {
                                return self.evaluate(result_expr);
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
                if let Some(func_val) = self.env.get(method.as_str()) {
                    if let Value::Function(func) = func_val {
                        return self.call_user_function(&func, all_args);
                    }
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
            
            // New JFM v2 expression kinds
            
            ExprKind::PipeExpr { left, op, right } => {
                let left_val = self.evaluate(left)?;
                self.apply_typed_pipe_operation(left_val, op, right)
            }
            
            ExprKind::DeepAccess { object, field } => {
                let obj = self.evaluate(object)?;
                self.deep_extract(&obj, field)
            }
            
            ExprKind::ColonMethodCall { object, method, args } => {
                // Colon method call: obj:method(args)
                let obj_val = self.evaluate(object)?;
                
                // Methods that take expression predicates/transforms (don't evaluate args)
                match method.as_str() {
                    "where" => {
                        // :where(predicate) - filter using predicate expression
                        if args.len() != 1 {
                            return Err(InterpreterError::invalid_operation(":where requires exactly 1 argument"));
                        }
                        return self.apply_pipe_filter(obj_val, &args[0]);
                    }
                    "select" => {
                        // :select(transform) - map using transform expression
                        if args.len() != 1 {
                            return Err(InterpreterError::invalid_operation(":select requires exactly 1 argument"));
                        }
                        return self.apply_pipe_map(obj_val, &args[0]);
                    }
                    _ => {}
                }
                
                // For other methods, evaluate args normally
                let mut all_args = vec![obj_val];
                for a in args {
                    all_args.push(self.evaluate(a)?);
                }
                
                // Map method names to builtins
                let builtin_name = match method.as_str() {
                    "sortBy" => "sort_by",
                    "groupBy" => "group_by",
                    "take" => "slice",
                    "find" => "find",
                    "findAll" => "find_all",
                    "collect" => "collect",
                    other => other,
                };
                
                // Try user-defined function first
                if let Some(func_val) = self.env.get(builtin_name) {
                    if let Value::Function(func) = func_val {
                        return self.call_user_function(&func, all_args);
                    }
                }
                
                // Special handling for :take(n) which maps to slice(arr, 0, n)
                if method == "take" && all_args.len() == 2 {
                    let arr = all_args.remove(0);
                    let n = all_args.remove(0);
                    return self.call_function("slice", vec![arr, Value::Number(0.0, false), n]);
                }
                
                self.call_function(builtin_name, all_args)
            }
            
            ExprKind::DestructuringLambda { pattern: _pattern, body } => {
                // Create a function that destructures its argument
                // TODO: Implement actual destructuring by binding pattern fields from input
                let func_params = vec![FunctionParam { 
                    name: Rc::from("__destructure_arg__"), 
                    default: None 
                }];
                let func = Function {
                    params: func_params,
                    body_expr: Some(body.clone()),
                    body_stmts: None,
                };
                // Store the pattern for later use during function call
                // For now, we'll handle this specially in apply_typed_pipe_operation
                Ok(Value::Function(Rc::new(func)))
            }
            
            ExprKind::SliceRange { start, end } => {
                // This should be evaluated in the context of a pipe
                // Get the array from pipe context
                let arr = self.env.get(PIPE_CONTEXT)
                    .ok_or_else(|| InterpreterError::invalid_operation("Slice range requires pipe context"))?;
                
                if let Value::Array(items_rc) = arr {
                    let items = items_rc.borrow();
                    let len = items.len() as i64;
                    
                    let start_idx = match start {
                        Some(s) => {
                            let v = self.evaluate(s)?;
                            match v {
                                Value::Number(n, _) => {
                                    let idx = n as i64;
                                    if idx < 0 { (len + idx).max(0) as usize } else { idx as usize }
                                }
                                _ => return Err(InterpreterError::type_error("Slice index must be number")),
                            }
                        }
                        None => 0,
                    };
                    
                    let end_idx = match end {
                        Some(e) => {
                            let v = self.evaluate(e)?;
                            match v {
                                Value::Number(n, _) => {
                                    let idx = n as i64;
                                    if idx < 0 { (len + idx).max(0) as usize } else { (idx as usize).min(items.len()) }
                                }
                                _ => return Err(InterpreterError::type_error("Slice index must be number")),
                            }
                        }
                        None => items.len(),
                    };
                    
                    let sliced: Vec<Value> = items.iter()
                        .skip(start_idx)
                        .take(end_idx.saturating_sub(start_idx))
                        .cloned()
                        .collect();
                    
                    Ok(Value::Array(Rc::new(RefCell::new(sliced))))
                } else {
                    Err(InterpreterError::type_error("Slice requires array"))
                }
            }
            
            ExprKind::PatternMatch { pattern } => {
                // Pattern matching is evaluated in pipe context
                let item = self.env.get(PIPE_CONTEXT)
                    .ok_or_else(|| InterpreterError::invalid_operation("Pattern match requires pipe context"))?;
                
                let matches = self.match_pattern(&item, pattern)?;
                Ok(Value::Bool(matches))
            }
            
            ExprKind::AggregateObject { fields } => {
                // Aggregation object for |& operator
                // This expects the array to be in pipe context
                let arr = self.env.get(PIPE_CONTEXT)
                    .ok_or_else(|| InterpreterError::invalid_operation("Aggregation requires pipe context"))?;
                
                let mut result = indexmap::IndexMap::new();
                
                for (key, agg_expr) in fields {
                    let val = self.evaluate_aggregate(&arr, agg_expr)?;
                    result.insert(key.clone(), val);
                }
                
                Ok(Value::Object(Rc::new(RefCell::new(result))))
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
                if let Some(path) = self.extract_path_segments(target) {
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
        if let ExprKind::ArrayIndex { array, index } = &right.kind {
            if matches!(array.kind, ExprKind::Literal(Value::Null)) {
                let idx = self.evaluate(index)?;
                return self.get_index(&left, &idx);
            }
        }
        
        // Check if the right side is `@ as name | expr` - like a named lambda
        // This handles: arr | @ as x | expr where x is bound per element
        if let ExprKind::Pipe { left: binding_expr, right: body } = &right.kind {
            if let ExprKind::AsBinding { expr: inner, name } = &binding_expr.kind {
                // Treat `arr | @ as x | expr` like a named lambda
                return self.eval_pipe_with_as_binding(left, inner, name, body);
            }
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
                if let Some(func_val) = self.env.get(name.as_ref()) {
                    if let Value::Function(func) = func_val {
                        return self.call_user_function(&func, vec![left]);
                    }
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
            
            if let Some(func_val) = self.env.get(method.as_str()) {
                if let Value::Function(func) = func_val {
                    return self.call_user_function(&func, all_args);
                }
            }
            return self.call_function(method, all_args);
        }
        
        // Check if the right side is a function call - if so, prepend left as first argument
        if let ExprKind::Call { name, args } = &right.kind {
            let mut arg_vals = vec![left];
            for a in args {
                arg_vals.push(self.evaluate(a)?);
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
    fn eval_pipe_with_lambda(&mut self, left: Value, params: &[Rc<str>], body: &Expr) -> Result<Value, InterpreterError> {
        match left {
            Value::Array(items_rc) => {
                let items = items_rc.borrow();
                let mut results = Vec::with_capacity(items.len());
                
                for item in items.iter() {
                    self.env.push_scope();
                    
                    if !params.is_empty() {
                        self.env.set(params[0].to_string(), item.clone());
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
                    self.env.set(params[0].to_string(), other.clone());
                }
                self.env.set(PIPE_CONTEXT.to_string(), other);
                
                let res = self.evaluate(body)?;
                self.env.pop_scope();
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
    fn extract_path_segments(&self, expr: &Expr) -> Option<Vec<AccessPathSegment>> {
        let mut path = Vec::new();
        let mut current = expr;
        
        loop {
            match &current.kind {
                ExprKind::FieldAccess { object, field } => {
                    path.push(AccessPathSegment::Field(field.clone()));
                    current = object;
                }
                ExprKind::ArrayIndex { array, index } => {
                    // Try to evaluate index as a constant number
                    if let ExprKind::Literal(Value::Number(n, _)) = &index.kind {
                        path.push(AccessPathSegment::Index(*n as usize));
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
    fn get_pipe_mutation_info(&self, expr: &Expr) -> Option<(Vec<AccessPathSegment>, BinaryOp, Expr)> {
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
    fn get_pipe_assignment_path(&self, expr: &Expr) -> Option<Vec<AccessPathSegment>> {
        if let ExprKind::Assignment { target, .. } = &expr.kind {
            return self.extract_path_segments(target);
        }
        None
    }
    
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
    fn set_nested_path(&self, obj: &Value, path: &[AccessPathSegment], value: Value) -> Result<(), InterpreterError> {
        if path.is_empty() {
            return Err(InterpreterError::invalid_operation("Cannot set empty path"));
        }
        
        if path.len() == 1 {
            // Single segment - set directly
            match &path[0] {
                AccessPathSegment::Field(field) => {
                    if let Value::Object(map_rc) = obj {
                        map_rc.borrow_mut().insert(field.clone(), value);
                        return Ok(());
                    }
                    return Err(InterpreterError::type_error("Cannot set field on non-object"));
                }
                AccessPathSegment::Index(idx) => {
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
            AccessPathSegment::Field(field) => {
                if let Value::Object(map_rc) = current {
                    map_rc.borrow_mut().insert(field.clone(), value);
                    Ok(())
                } else {
                    Err(InterpreterError::type_error("Cannot set field on non-object"))
                }
            }
            AccessPathSegment::Index(idx) => {
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
            "print" => builtins::builtin_print(&args, |v| self.value_to_string(v)),
            "unique" => builtins::builtin_unique(&args, |a, b| self.deep_equals(a, b)),
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
        // Count required parameters (those without defaults)
        let required_params = func.params.iter().filter(|p| p.default.is_none()).count();
        
        if args.len() < required_params {
            return Err(InterpreterError::invalid_operation(format!(
                "Function expects at least {} arguments, got {}",
                required_params,
                args.len()
            )));
        }
        
        if args.len() > func.params.len() {
            return Err(InterpreterError::invalid_operation(format!(
                "Function expects at most {} arguments, got {}",
                func.params.len(),
                args.len()
            )));
        }

        self.env.push_scope();

        // Bind parameters, using defaults where args are missing
        for (i, param) in func.params.iter().enumerate() {
            let value = if i < args.len() {
                args[i].clone()
            } else if let Some(ref default_expr) = param.default {
                self.evaluate(default_expr)?
            } else {
                return Err(InterpreterError::invalid_operation(format!(
                    "Missing required argument: {}",
                    param.name
                )));
            };
            self.env.set(param.name.to_string(), value);
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
                if let Some(it) = self.env.get(PIPE_CONTEXT) {
                    if let Value::Object(map) = it {
                        return map.borrow().get(field).cloned()
                            .ok_or_else(|| InterpreterError::field_not_found(field));
                    }
                }
                // Fall back to root (for top-level .field access)
                if let Some(root) = self.env.get(ROOT_CONTEXT) {
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
            (Value::Null, Value::Number(n, _)) => {
                // Try pipe context first (for [n] in pipe operations)
                if let Some(it) = self.env.get(PIPE_CONTEXT) {
                    if let Value::Array(items) = it {
                        let index = *n as usize;
                        let len = items.borrow().len();
                        return items
                            .borrow()
                            .get(index)
                            .cloned()
                            .ok_or_else(|| InterpreterError::index_out_of_bounds(index, len));
                    }
                }
                // Fall back to root
                if let Some(root) = self.env.get(ROOT_CONTEXT) {
                    if let Value::Array(items) = root {
                        let index = *n as usize;
                        let len = items.borrow().len();
                        return items
                            .borrow()
                            .get(index)
                            .cloned()
                            .ok_or_else(|| InterpreterError::index_out_of_bounds(index, len));
                    }
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
            Value::Module(m) => format!("<module:{}>", m.name),
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

    // === JFM v2 Helper Methods ===

    /// Apply typed pipe operation based on PipeOp
    fn apply_typed_pipe_operation(
        &mut self,
        left: Value,
        op: &PipeOp,
        right: &Expr,
    ) -> Result<Value, InterpreterError> {
        match op {
            PipeOp::Filter => self.apply_pipe_filter(left, right),
            PipeOp::Map => self.apply_pipe_map(left, right),
            PipeOp::Mutate => self.apply_pipe_mutate(left, right),
            PipeOp::Aggregate => self.apply_pipe_aggregate(left, right),
            PipeOp::Tap => self.apply_pipe_tap(left, right),
            PipeOp::Legacy => {
                // Legacy pipe behavior - auto-detect filter vs map
                self.apply_legacy_pipe(left, right)
            }
        }
    }

    /// Legacy pipe behavior - auto-detect filter vs map based on expression
    fn apply_legacy_pipe(&mut self, left: Value, right: &Expr) -> Result<Value, InterpreterError> {
        // Check if it's an array to decide behavior
        if let Value::Array(items_rc) = left {
            let items = items_rc.borrow();
            
            // First, evaluate the expression for the first item to check if it returns a boolean
            if let Some(first_item) = items.first() {
                self.env.push_scope();
                self.env.set(PIPE_CONTEXT.to_string(), first_item.clone());
                let test_result = self.evaluate(right)?;
                self.env.pop_scope();
                
                // If it's a boolean, treat as filter
                if matches!(test_result, Value::Bool(_)) {
                    let mut results = Vec::new();
                    for item in items.iter() {
                        self.env.push_scope();
                        self.env.set(PIPE_CONTEXT.to_string(), item.clone());
                        let predicate_result = self.evaluate(right)?;
                        self.env.pop_scope();
                        
                        if let Value::Bool(true) = predicate_result {
                            results.push(item.clone());
                        }
                    }
                    return Ok(Value::Array(Rc::new(RefCell::new(results))));
                }
            }
            
            // Otherwise treat as map
            let mut results = Vec::with_capacity(items.len());
            for item in items.iter() {
                self.env.push_scope();
                self.env.set(PIPE_CONTEXT.to_string(), item.clone());
                let mapped = self.evaluate(right)?;
                self.env.pop_scope();
                results.push(mapped);
            }
            Ok(Value::Array(Rc::new(RefCell::new(results))))
        } else {
            // For non-arrays, just set context and evaluate
            self.env.push_scope();
            self.env.set(PIPE_CONTEXT.to_string(), left);
            let result = self.evaluate(right)?;
            self.env.pop_scope();
            Ok(result)
        }
    }

    /// |? - Filter operation: keeps items where predicate is true
    fn apply_pipe_filter(&mut self, left: Value, right: &Expr) -> Result<Value, InterpreterError> {
        if let Value::Array(items_rc) = left {
            let items = items_rc.borrow();
            let mut results = Vec::new();
            
            for item in items.iter() {
                self.env.push_scope();
                self.env.set(PIPE_CONTEXT.to_string(), item.clone());
                
                let predicate_result = self.evaluate(right)?;
                
                self.env.pop_scope();
                
                if let Value::Bool(true) = predicate_result {
                    results.push(item.clone());
                }
            }
            
            Ok(Value::Array(Rc::new(RefCell::new(results))))
        } else {
            Err(InterpreterError::type_error("Filter (|?) requires an array"))
        }
    }

    /// |> - Map operation: transforms each item
    fn apply_pipe_map(&mut self, left: Value, right: &Expr) -> Result<Value, InterpreterError> {
        if let Value::Array(items_rc) = left {
            let items = items_rc.borrow();
            let mut results = Vec::with_capacity(items.len());
            
            for item in items.iter() {
                self.env.push_scope();
                self.env.set(PIPE_CONTEXT.to_string(), item.clone());
                
                let mapped = self.evaluate(right)?;
                
                self.env.pop_scope();
                results.push(mapped);
            }
            
            Ok(Value::Array(Rc::new(RefCell::new(results))))
        } else {
            // For non-arrays, just evaluate with the value as context
            self.env.push_scope();
            self.env.set(PIPE_CONTEXT.to_string(), left);
            let result = self.evaluate(right)?;
            self.env.pop_scope();
            Ok(result)
        }
    }

    /// |= - Mutate operation: modifies items in place (returns new array)
    fn apply_pipe_mutate(&mut self, left: Value, right: &Expr) -> Result<Value, InterpreterError> {
        if let Value::Array(items_rc) = left {
            let items = items_rc.borrow();
            let mut results = Vec::with_capacity(items.len());
            
            for item in items.iter() {
                // Clone the item and merge with the mutation expression
                let mut new_item = item.clone();
                
                self.env.push_scope();
                self.env.set(PIPE_CONTEXT.to_string(), item.clone());
                
                let mutation = self.evaluate(right)?;
                
                self.env.pop_scope();
                
                // Merge mutation into item if both are objects
                if let (Value::Object(obj_rc), Value::Object(mutation_rc)) = (&mut new_item, &mutation) {
                    let mut obj = obj_rc.borrow_mut();
                    for (k, v) in mutation_rc.borrow().iter() {
                        obj.insert(k.clone(), v.clone());
                    }
                } else {
                    // If not both objects, replace entirely
                    new_item = mutation;
                }
                
                results.push(new_item);
            }
            
            Ok(Value::Array(Rc::new(RefCell::new(results))))
        } else if let Value::Object(obj_rc) = left {
            // For single object, apply mutation directly
            self.env.push_scope();
            self.env.set(PIPE_CONTEXT.to_string(), Value::Object(obj_rc.clone()));
            
            let mutation = self.evaluate(right)?;
            
            self.env.pop_scope();
            
            if let Value::Object(mutation_rc) = mutation {
                let mut result = obj_rc.borrow().clone();
                for (k, v) in mutation_rc.borrow().iter() {
                    result.insert(k.clone(), v.clone());
                }
                Ok(Value::Object(Rc::new(RefCell::new(result))))
            } else {
                Ok(mutation)
            }
        } else {
            Err(InterpreterError::type_error("Mutate (|=) requires an array or object"))
        }
    }

    /// |& - Aggregate operation: reduces array to single value
    fn apply_pipe_aggregate(&mut self, left: Value, right: &Expr) -> Result<Value, InterpreterError> {
        // Set the array as pipe context for aggregate expressions
        self.env.push_scope();
        self.env.set(PIPE_CONTEXT.to_string(), left);
        
        let result = self.evaluate(right)?;
        
        self.env.pop_scope();
        Ok(result)
    }

    /// |# - Tap operation: executes side effect, returns original value
    fn apply_pipe_tap(&mut self, left: Value, right: &Expr) -> Result<Value, InterpreterError> {
        self.env.push_scope();
        self.env.set(PIPE_CONTEXT.to_string(), left.clone());
        
        // Execute the expression for side effects
        let _ = self.evaluate(right)?;
        
        self.env.pop_scope();
        
        // Return the original value unchanged
        Ok(left)
    }

    /// Deep extraction - recursively finds field in nested objects/arrays
    fn deep_extract(&self, value: &Value, field: &str) -> Result<Value, InterpreterError> {
        let mut results = Vec::new();
        self.collect_deep_field(value, field, &mut results);
        
        // If only one result and the original was an object, return that result directly
        if results.len() == 1 {
            Ok(results.pop().unwrap())
        } else if results.is_empty() {
            Ok(Value::Null)
        } else {
            Ok(Value::Array(Rc::new(RefCell::new(results))))
        }
    }

    /// Helper to recursively collect field values from nested structures
    fn collect_deep_field(&self, value: &Value, field: &str, results: &mut Vec<Value>) {
        match value {
            Value::Object(obj_rc) => {
                let obj = obj_rc.borrow();
                // Check if this object has the field
                if let Some(val) = obj.get(field) {
                    results.push(val.clone());
                }
                // Also recurse into all values
                for (_key, val) in obj.iter() {
                    self.collect_deep_field(val, field, results);
                }
            }
            Value::Array(arr_rc) => {
                let arr = arr_rc.borrow();
                for item in arr.iter() {
                    self.collect_deep_field(item, field, results);
                }
            }
            _ => {}
        }
    }

    /// Evaluate a FilterPattern for pattern matching in |?
    fn match_pattern(&self, value: &Value, pattern: &FilterPattern) -> Result<bool, InterpreterError> {
        match pattern {
            FilterPattern::Object(fields) => {
                if let Value::Object(obj_rc) = value {
                    let obj = obj_rc.borrow();
                    
                    for field in fields {
                        let field_val = obj.get(&field.key);
                        
                        let matches = match &field.matcher {
                            PatternMatcher::Exact(expected) => {
                                if let Some(actual) = field_val {
                                    self.values_equal(actual, expected)
                                } else {
                                    false
                                }
                            }
                            PatternMatcher::Comparison(op, expected) => {
                                if let Some(actual) = field_val {
                                    self.compare_values(actual, op, expected)?
                                } else {
                                    false
                                }
                            }
                            PatternMatcher::Alternatives(alternatives) => {
                                if let Some(actual) = field_val {
                                    alternatives.iter().any(|alt| self.values_equal(actual, alt))
                                } else {
                                    false
                                }
                            }
                            PatternMatcher::Negated(inner) => {
                                !self.match_field_pattern(field_val, inner)?
                            }
                            PatternMatcher::Any => {
                                field_val.is_some()
                            }
                        };
                        
                        if !matches {
                            return Ok(false);
                        }
                    }
                    
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            FilterPattern::Negated(inner) => {
                Ok(!self.match_pattern(value, inner)?)
            }
        }
    }

    /// Helper to match a field value against a pattern matcher
    fn match_field_pattern(&self, field_val: Option<&Value>, matcher: &PatternMatcher) -> Result<bool, InterpreterError> {
        match matcher {
            PatternMatcher::Exact(expected) => {
                if let Some(actual) = field_val {
                    Ok(self.values_equal(actual, expected))
                } else {
                    Ok(false)
                }
            }
            PatternMatcher::Comparison(op, expected) => {
                if let Some(actual) = field_val {
                    self.compare_values(actual, op, expected)
                } else {
                    Ok(false)
                }
            }
            PatternMatcher::Alternatives(alternatives) => {
                if let Some(actual) = field_val {
                    Ok(alternatives.iter().any(|alt| self.values_equal(actual, alt)))
                } else {
                    Ok(false)
                }
            }
            PatternMatcher::Negated(inner) => {
                Ok(!self.match_field_pattern(field_val, inner)?)
            }
            PatternMatcher::Any => {
                Ok(field_val.is_some())
            }
        }
    }

    /// Compare values for pattern matching
    fn compare_values(&self, left: &Value, op: &BinaryOp, right: &Value) -> Result<bool, InterpreterError> {
        match (left, op, right) {
            (Value::Number(a, _), BinaryOp::Greater, Value::Number(b, _)) => Ok(a > b),
            (Value::Number(a, _), BinaryOp::GreaterEq, Value::Number(b, _)) => Ok(a >= b),
            (Value::Number(a, _), BinaryOp::Less, Value::Number(b, _)) => Ok(a < b),
            (Value::Number(a, _), BinaryOp::LessEq, Value::Number(b, _)) => Ok(a <= b),
            (a, BinaryOp::Eq, b) => Ok(self.values_equal(a, b)),
            (a, BinaryOp::NotEq, b) => Ok(!self.values_equal(a, b)),
            _ => Ok(false),
        }
    }

    /// Check if a value is truthy
    #[allow(dead_code)]
    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Number(n, _) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(a) => !a.borrow().is_empty(),
            Value::Object(o) => !o.borrow().is_empty(),
            Value::Function(_) => true,
            Value::Module(_) => true,
        }
    }

    /// Evaluate an aggregate expression on an array
    fn evaluate_aggregate(&mut self, arr: &Value, agg_expr: &AggregateExpr) -> Result<Value, InterpreterError> {
        if let Value::Array(items_rc) = arr {
            let items = items_rc.borrow();
            
            match agg_expr {
                AggregateExpr::Method(method) => {
                    match method.as_str() {
                        "length" | "count" => Ok(Value::Number(items.len() as f64, false)),
                        "first" => Ok(items.first().cloned().unwrap_or(Value::Null)),
                        "last" => Ok(items.last().cloned().unwrap_or(Value::Null)),
                        "sum" => {
                            let total: f64 = items.iter().filter_map(|v| {
                                if let Value::Number(n, _) = v { Some(*n) } else { None }
                            }).sum();
                            Ok(Value::Number(total, true))
                        }
                        "avg" => {
                            let nums: Vec<f64> = items.iter().filter_map(|v| {
                                if let Value::Number(n, _) = v { Some(*n) } else { None }
                            }).collect();
                            if nums.is_empty() {
                                Ok(Value::Number(0.0, true))
                            } else {
                                Ok(Value::Number(nums.iter().sum::<f64>() / nums.len() as f64, true))
                            }
                        }
                        "min" => {
                            let min = items.iter().filter_map(|v| {
                                if let Value::Number(n, _) = v { Some(*n) } else { None }
                            }).fold(f64::INFINITY, f64::min);
                            if min == f64::INFINITY {
                                Ok(Value::Null)
                            } else {
                                Ok(Value::Number(min, true))
                            }
                        }
                        "max" => {
                            let max = items.iter().filter_map(|v| {
                                if let Value::Number(n, _) = v { Some(*n) } else { None }
                            }).fold(f64::NEG_INFINITY, f64::max);
                            if max == f64::NEG_INFINITY {
                                Ok(Value::Null)
                            } else {
                                Ok(Value::Number(max, true))
                            }
                        }
                        _ => Err(InterpreterError::invalid_operation(format!("Unknown aggregate method: {}", method))),
                    }
                }
                AggregateExpr::MethodWithPath(method, path) => {
                    // Extract values at the path from each item
                    let values: Vec<f64> = items.iter().filter_map(|item| {
                        self.extract_path_value(item, path)
                            .and_then(|v| if let Value::Number(n, _) = v { Some(n) } else { None })
                    }).collect();
                    
                    match method.as_str() {
                        "sum" => Ok(Value::Number(values.iter().sum(), true)),
                        "avg" => {
                            if values.is_empty() {
                                Ok(Value::Number(0.0, true))
                            } else {
                                Ok(Value::Number(values.iter().sum::<f64>() / values.len() as f64, true))
                            }
                        }
                        "min" => {
                            values.iter().copied().fold(None, |min, v| {
                                Some(min.map_or(v, |m: f64| m.min(v)))
                            }).map_or(Ok(Value::Null), |n| Ok(Value::Number(n, true)))
                        }
                        "max" => {
                            values.iter().copied().fold(None, |max, v| {
                                Some(max.map_or(v, |m: f64| m.max(v)))
                            }).map_or(Ok(Value::Null), |n| Ok(Value::Number(n, true)))
                        }
                        "first" => {
                            for item in items.iter() {
                                if let Some(val) = self.extract_path_value(item, path) {
                                    return Ok(val);
                                }
                            }
                            Ok(Value::Null)
                        }
                        "last" => {
                            let mut last = Value::Null;
                            for item in items.iter() {
                                if let Some(val) = self.extract_path_value(item, path) {
                                    last = val;
                                }
                            }
                            Ok(last)
                        }
                        _ => Err(InterpreterError::invalid_operation(format!("Unknown aggregate method: {}", method))),
                    }
                }
                AggregateExpr::Collect(path) => {
                    let mut collected = Vec::new();
                    for item in items.iter() {
                        if let Some(val) = self.extract_path_value(item, path) {
                            collected.push(val);
                        }
                    }
                    Ok(Value::Array(Rc::new(RefCell::new(collected))))
                }
            }
        } else {
            Err(InterpreterError::type_error("Aggregate requires array"))
        }
    }

    /// Extract a value at a path from an object
    fn extract_path_value(&self, value: &Value, path: &[String]) -> Option<Value> {
        let mut current = value.clone();
        for segment in path {
            match current {
                Value::Object(obj_rc) => {
                    let borrowed = obj_rc.borrow();
                    if let Some(val) = borrowed.get(segment) {
                        current = val.clone();
                    } else {
                        return None;
                    }
                }
                _ => return None,
            }
        }
        Some(current)
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

