use crate::lexer::{BinaryOp, Expr, ExprKind, Stmt, Token, UnaryOp, Value};
use super::environment::Environment;
use super::error::InterpreterError;
use super::control_flow::ControlFlow;
use super::parser::TokenParser;
use super::builtins;
use chumsky::Parser;
use std::rc::Rc;
use std::cell::RefCell;

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    pub fn with_root(root: Value) -> Self {
        let env = Environment::new();
        env.set("root".to_string(), root);
        Self { env }
    }

    pub fn run(&mut self, stmts: Vec<Stmt>) -> Result<Option<Value>, InterpreterError> {
        let mut last_val = None;
        for stmt in stmts {
            match self.execute_stmt(&stmt)? {
                ControlFlow::Return(val) => return Ok(Some(val)),
                ControlFlow::Value(val) => last_val = Some(val),
                ControlFlow::Next => last_val = None,
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
                    _ => return Err(InterpreterError::TypeError("Cannot iterate over non-array".to_string())),
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
                            ControlFlow::Value(_) | ControlFlow::Next => {}
                        }
                    }

                    self.env = (*old_env).clone();
                    if let ControlFlow::Return(val) = result {
                        return Ok(ControlFlow::Return(val));
                    }
                }
                
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
                .ok_or_else(|| InterpreterError::UndefinedVariable(name.to_string())),

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
                let left_val = self.eval_expr(left)?;
                self.eval_smart_pipe(left_val, right)
            }

            ExprKind::Grouped(expr) => self.eval_expr(expr),

            ExprKind::Array { elements } => {
                let mut vals = Vec::new();
                for e in elements {
                    vals.push(self.eval_expr(e)?);
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

            ExprKind::Lambda { params: _, body: _ } => {
                Err(InterpreterError::InvalidOperation("Lambdas not fully implemented in evaluation yet".to_string()))
            }

            ExprKind::Call { name, args } => {
                let mut arg_vals = Vec::new();
                for a in args {
                    arg_vals.push(self.eval_expr(a)?);
                }
                self.call_function(name, arg_vals)
            }

            ExprKind::Assignment { target, value } => {
                let val = self.eval_expr(value)?;
                self.perform_assignment(target, val)
            }

            ExprKind::CompoundAssignment { target, op, value } => {
                let right_val = self.eval_expr(value)?;
                let current_val = self.eval_expr(target)?;
                let new_val = self.eval_binary_op(&current_val, op, &right_val)?;
                self.perform_assignment(target, new_val)
            }

            ExprKind::Range { start, end } => {
                let s = self.eval_expr(start)?;
                let e = self.eval_expr(end)?;
                match (s, e) {
                    (Value::Number(start_n), Value::Number(end_n)) => {
                        let mut vals = Vec::new();
                        let mut i = start_n;
                        while i <= end_n {
                            vals.push(Value::Number(i));
                            i += 1.0;
                        }
                        Ok(Value::Array(Rc::new(RefCell::new(vals))))
                    }
                    _ => Err(InterpreterError::TypeError("Range requires numbers".to_string())),
                }
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
                let obj_val = self.eval_expr(object)?;
                match obj_val {
                    Value::Object(map_rc) => {
                        map_rc.borrow_mut().insert(field.clone(), value.clone());
                        Ok(value)
                    }
                    _ => Err(InterpreterError::TypeError("Cannot set field on non-object".to_string())),
                }
            }
            ExprKind::ArrayIndex { array, index } => {
                let arr_val = self.eval_expr(array)?;
                let idx_val = self.eval_expr(index)?;
                let idx = match idx_val {
                    Value::Number(n) => n as usize,
                    _ => return Err(InterpreterError::TypeError("Index must be a number".to_string())),
                };

                match arr_val {
                    Value::Array(items_rc) => {
                        let mut items = items_rc.borrow_mut();
                        if idx < items.len() {
                            items[idx] = value.clone();
                            Ok(value)
                        } else {
                            Err(InterpreterError::IndexOutOfBounds(idx))
                        }
                    }
                    _ => Err(InterpreterError::TypeError("Cannot index non-array".to_string())),
                }
            }
            _ => Err(InterpreterError::InvalidOperation("Invalid assignment target".to_string())),
        }
    }
    
    fn eval_smart_pipe(&mut self, left: Value, right: &Expr) -> Result<Value, InterpreterError> {
        match left {
            Value::Array(items_rc) => {
                let mut results = Vec::new();
                let items = items_rc.borrow();
                for item in items.iter() {
                    let old_env = Rc::new(self.env.clone());
                    self.env = Environment::with_parent(old_env.clone());
                    self.env.set("_it".to_string(), item.clone());

                    let res = self.eval_expr(right)?;

                    match res {
                        Value::Bool(b) => {
                            if b { results.push(item.clone()); }
                        }
                        other => results.push(other),
                    }

                    self.env = (*old_env).clone();
                }

                Ok(Value::Array(Rc::new(RefCell::new(results))))
            }
            other => {
                let old_env = Rc::new(self.env.clone());
                self.env = Environment::with_parent(old_env.clone());
                self.env.set("_it".to_string(), other);
                let res = self.eval_expr(right)?;
                self.env = (*old_env).clone();
                Ok(res)
            }
        }
    }

    fn call_function(&mut self, name: &str, args: Vec<Value>) -> Result<Value, InterpreterError> {
        match name {
            "count" => builtins::builtin_count(&args),
            "sum" => builtins::builtin_sum(&args),
            "avg" => builtins::builtin_avg(&args),
            "min" => builtins::builtin_min(&args),
            "max" => builtins::builtin_max(&args),
            "take" => builtins::builtin_take(&args),
            "push" => builtins::builtin_push(&args),
            "input" => builtins::builtin_input(&args),
            "print" => builtins::builtin_print(&args, |v| self.value_to_string(v)),
            "unique" => builtins::builtin_unique(&args, |a, b| self.deep_equals(a, b)),
            "sort_by" => builtins::builtin_sort_by(&args, |v, f| self.get_field(v, f)),
            "group_by" => builtins::builtin_group_by(&args, |v, f| self.get_field(v, f)),
            "include" => self.builtin_include(&args),
            _ => Err(InterpreterError::InvalidOperation(format!("Unknown function: {}", name))),
        }
    }

    fn builtin_include(&mut self, args: &[Value]) -> Result<Value, InterpreterError> {
        if args.is_empty() {
            return Err(InterpreterError::InvalidOperation("include requires a file path argument".to_string()));
        }
        if let Value::String(path) = &args[0] {
            let script_content = std::fs::read_to_string(path.as_ref())
                .map_err(|e| InterpreterError::InvalidOperation(format!("Failed to read script file '{}': {}", path, e)))?;
            
            let lexer = crate::lexer::lexer();
            let tokens: Vec<Token> = lexer
                .parse(&script_content)
                .into_result()
                .map_err(|e| InterpreterError::InvalidOperation(format!("Lexer error in included script: {:?}", e)))?
                .into_iter()
                .map(|(tok, _)| tok)
                .collect();
            
            let mut parser = TokenParser::new(tokens);
            let stmts = parser
                .parse()
                .map_err(|e| InterpreterError::InvalidOperation(format!("Parser error in included script: {}", e)))?;
            
            let result = self.run(stmts)?;
            Ok(result.unwrap_or(Value::Null))
        } else {
            Err(InterpreterError::TypeError("include requires a string path argument".to_string()))
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
                    Ok(Value::Number(items.borrow().len() as f64))
                } else {
                    Ok(Value::Null)
                }
            }
            Value::Null => {
                if let Some(it) = self.env.get("_it") {
                    if let Value::Object(map) = it {
                        return map.borrow().get(field).cloned()
                            .ok_or_else(|| InterpreterError::FieldNotFound(field.to_string()));
                    }
                }
                Ok(Value::Null)
            }
            _ => Err(InterpreterError::TypeError(format!(
                "Cannot access field '{}' on non-object",
                field
            ))),
        }
    }

    fn get_index(&self, arr: &Value, idx: &Value) -> Result<Value, InterpreterError> {
        match (arr, idx) {
            (Value::Array(items), Value::Number(n)) => {
                let index = *n as usize;
                items
                    .borrow()
                    .get(index)
                    .cloned()
                    .ok_or_else(|| InterpreterError::IndexOutOfBounds(index))
            }
            _ => Err(InterpreterError::TypeError(
                "Array indexing requires array and number".to_string(),
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
            (Value::Number(a), BinaryOp::Add, Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::Number(a), BinaryOp::Sub, Value::Number(b)) => Ok(Value::Number(a - b)),
            (Value::Number(a), BinaryOp::Mul, Value::Number(b)) => Ok(Value::Number(a * b)),
            (Value::Number(a), BinaryOp::Div, Value::Number(b)) => {
                if *b == 0.0 {
                    Err(InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Number(a / b))
                }
            }
            (Value::Number(a), BinaryOp::Mod, Value::Number(b)) => Ok(Value::Number(a % b)),
            (Value::Number(a), BinaryOp::Pow, Value::Number(b)) => Ok(Value::Number(a.powf(*b))),
            (Value::String(a), BinaryOp::Add, Value::String(b)) => {
                let mut combined = String::with_capacity(a.len() + b.len());
                combined.push_str(a);
                combined.push_str(b);
                Ok(Value::String(Rc::<str>::from(combined)))
            }
            (Value::Array(a), BinaryOp::Add, Value::Array(b)) => {
                let mut result = a.borrow().clone();
                result.extend(b.borrow().iter().cloned());
                Ok(Value::Array(Rc::new(RefCell::new(result))))
            }
            (Value::Array(a), BinaryOp::Add, b) => {
                let mut result = a.borrow().clone();
                result.push(b.clone());
                Ok(Value::Array(Rc::new(RefCell::new(result))))
            }
            (a, BinaryOp::Eq, b) => Ok(Value::Bool(self.values_equal(a, b))),
            (a, BinaryOp::NotEq, b) => Ok(Value::Bool(!self.values_equal(a, b))),
            (Value::Number(a), BinaryOp::Greater, Value::Number(b)) => Ok(Value::Bool(a > b)),
            (Value::Number(a), BinaryOp::Less, Value::Number(b)) => Ok(Value::Bool(a < b)),
            (Value::Number(a), BinaryOp::GreaterEq, Value::Number(b)) => Ok(Value::Bool(a >= b)),
            (Value::Number(a), BinaryOp::LessEq, Value::Number(b)) => Ok(Value::Bool(a <= b)),
            (Value::Bool(a), BinaryOp::And, Value::Bool(b)) => Ok(Value::Bool(*a && *b)),
            (Value::Bool(a), BinaryOp::Or, Value::Bool(b)) => Ok(Value::Bool(*a || *b)),
            _ => Err(InterpreterError::InvalidOperation(format!(
                "Cannot apply {:?} to {:?} and {:?}",
                op, left, right
            ))),
        }
    }

    fn eval_unary_op(&mut self, op: &UnaryOp, val: &Value) -> Result<Value, InterpreterError> {
        match (op, val) {
            (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            (UnaryOp::Neg, Value::Number(n)) => Ok(Value::Number(-n)),
            _ => Err(InterpreterError::InvalidOperation(format!(
                "Cannot apply {:?} to {:?}",
                op, val
            ))),
        }
    }

    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            _ => false,
        }
    }

    pub fn deep_equals(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a.to_bits() == b.to_bits(),
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
            Value::Number(n) => {
                if n.fract() == 0.0 && n.abs() < 1e15 {
                    format!("{:.0}", n)
                } else {
                    n.to_string()
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
        Some(t) => t
            .into_iter()
            .map(|(tok, _)| tok)
            .collect::<Vec<Token>>(),
        None => return Err("Lexer failed".to_string()),
    };

    let mut parser = TokenParser::new(tokens);
    let stmts = parser.parse()?;

    let mut interpreter = Interpreter::with_root(root);
    interpreter
        .run(stmts)
        .map_err(|e| format!("Runtime error: {}", e))
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
        user1.insert("age".to_string(), Value::Number(30.0));
        users.push(Value::Object(Rc::new(RefCell::new(user1))));

        let mut user2 = IndexMap::new();
        user2.insert("name".to_string(), Value::String(Rc::from("Alice")));
        user2.insert("age".to_string(), Value::Number(25.0));
        users.push(Value::Object(Rc::new(RefCell::new(user2))));

        let mut user3 = IndexMap::new();
        user3.insert("name".to_string(), Value::String(Rc::from("Bob")));
        user3.insert("age".to_string(), Value::Number(35.0));
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
        assert_eq!(result, Some(Value::Number(8.0)));
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
        assert_eq!(result, Some(Value::Number(20.0)));
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
    fn test_count_function() {
        let source = "count(root.users);";
        let root = make_test_root();
        let result = parse_and_run(source, root).unwrap();
        assert_eq!(result, Some(Value::Number(3.0)));
    }

    #[test]
    fn test_sum_function() {
        let source = "sum([1, 2, 3, 4, 5]);";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert_eq!(result, Some(Value::Number(15.0)));
    }

    #[test]
    fn test_avg_function() {
        let source = "avg([10, 20, 30]);";
        let result = parse_and_run(source, Value::Null).unwrap();
        assert_eq!(result, Some(Value::Number(20.0)));
    }
}

