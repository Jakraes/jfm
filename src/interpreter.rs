use crate::lexer::{BinaryOp, Expr, ExprKind, Stmt, Token, UnaryOp, Value};
use chumsky::span::{SimpleSpan, Span};
use chumsky::Parser;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

pub struct TokenParser {
    tokens: Vec<Token>,
    current: usize,
}

impl TokenParser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.current < self.tokens.len() {
            let token = self.tokens[self.current].clone();
            self.current += 1;
            Some(token)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.current_token() {
            Some(token) if std::mem::discriminant(token) == std::mem::discriminant(&expected) => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(format!("Expected {:?}, found {:?}", expected, token)),
            None => Err(format!("Expected {:?}, found end of input", expected)),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();
        while self.current_token().is_some() {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt, String> {
        match self.current_token() {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::For) => self.parse_for_statement(),
            Some(Token::If) => self.parse_if_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::LBrace) => {
                let block = self.parse_block()?;
                Ok(Stmt::Block(block))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, String> {
        self.expect(Token::LBrace)?;
        let mut statements = Vec::new();
        while !matches!(self.current_token(), Some(Token::RBrace)) && self.current_token().is_some() {
            statements.push(self.parse_statement()?);
        }
        self.expect(Token::RBrace)?;
        Ok(statements)
    }

    fn parse_for_statement(&mut self) -> Result<Stmt, String> {
        self.expect(Token::For)?;
        let var = match self.advance() {
            Some(Token::Ident(n)) => Rc::from(n.as_str()),
            other => return Err(format!("Expected identifier in for loop, found {:?}", other)),
        };
        self.expect(Token::In)?;
        let iterable = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(Stmt::For { var, iterable, body })
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, String> {
        self.expect(Token::If)?;
        let condition = self.parse_expression()?;
        let then_branch = self.parse_block()?;
        let else_branch = if matches!(self.current_token(), Some(Token::Else)) {
            self.advance();
            if matches!(self.current_token(), Some(Token::If)) {
                Some(vec![self.parse_if_statement()?])
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Return)?;
        let value = if !matches!(self.current_token(), Some(Token::Semicolon)) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect(Token::Semicolon)?;
        Ok(Stmt::Return(value))
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Let)?;

        let name = match self.advance() {
            Some(Token::Ident(n)) => n,
            other => return Err(format!("Expected identifier, found {:?}", other)),
        };

        self.expect(Token::Assign)?;
        let value = self.parse_expression()?;
        self.expect(Token::Semicolon)?;

        Ok(Stmt::Let {
            name: Rc::from(name.as_str()),
            value,
        })
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, String> {
        let left = self.parse_lambda()?;

        if let Some(token) = self.current_token().cloned() {
            let op = match token {
                Token::Assign => Some(None),
                Token::PlusEq => Some(Some(BinaryOp::Add)),
                Token::MinusEq => Some(Some(BinaryOp::Sub)),
                Token::StarEq => Some(Some(BinaryOp::Mul)),
                Token::SlashEq => Some(Some(BinaryOp::Div)),
                Token::PipeEq => Some(Some(BinaryOp::Pipe)),
                _ => None,
            };

            if let Some(op_kind) = op {
                self.advance();
                let right = self.parse_assignment()?;
                let span = SimpleSpan::new((), 0..0);
                return match op_kind {
                    None => Ok(Expr {
                        kind: ExprKind::Assignment {
                            target: Box::new(left),
                            value: Box::new(right),
                        },
                        span,
                    }),
                    Some(binary_op) => Ok(Expr {
                        kind: ExprKind::CompoundAssignment {
                            target: Box::new(left),
                            op: binary_op,
                            value: Box::new(right),
                        },
                        span,
                    }),
                };
            }
        }

        Ok(left)
    }

    fn parse_lambda(&mut self) -> Result<Expr, String> {
        if matches!(self.current_token(), Some(Token::LParen)) {
        }

        let expr = self.parse_pipe()?;

        if matches!(self.current_token(), Some(Token::Arrow)) {
            self.advance();
            let body = self.parse_expression()?;
            let span = SimpleSpan::new((), 0..0);
            
            let params = match expr.kind {
                ExprKind::Identifier(name) => vec![name],
                ExprKind::Grouped(inner) => match inner.kind {
                     _ => return Err("Invalid lambda parameters".to_string()),
                },
                _ => return Err("Invalid lambda parameters".to_string()),
            };

            return Ok(Expr {
                kind: ExprKind::Lambda {
                    params,
                    body: Box::new(body),
                },
                span,
            });
        }

        Ok(expr)
    }

    fn parse_pipe(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_logical_or()?;

        while matches!(self.current_token(), Some(Token::Pipe)) {
            self.advance();
            let right = self.parse_logical_or()?;
            let span = SimpleSpan::new((), 0..0);
            left = Expr {
                kind: ExprKind::Pipe {
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn parse_logical_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_logical_and()?;

        while matches!(self.current_token(), Some(Token::Or)) {
            self.advance();
            let right = self.parse_logical_and()?;
            let span = SimpleSpan::new((), 0..0);
            left = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Or,
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_comparison()?;

        while matches!(self.current_token(), Some(Token::And)) {
            self.advance();
            let right = self.parse_comparison()?;
            let span = SimpleSpan::new((), 0..0);
            left = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op: BinaryOp::And,
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_range()?;

        while let Some(op) = self.current_token() {
            let binary_op = match op {
                Token::Eq => BinaryOp::Eq,
                Token::NotEq => BinaryOp::NotEq,
                Token::Greater => BinaryOp::Greater,
                Token::Less => BinaryOp::Less,
                Token::GreaterEq => BinaryOp::GreaterEq,
                Token::LessEq => BinaryOp::LessEq,
                _ => break,
            };
            self.advance();
            let right = self.parse_range()?;
            let span = SimpleSpan::new((), 0..0);
            left = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op: binary_op,
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn parse_range(&mut self) -> Result<Expr, String> {
        let left = self.parse_additive()?;

        if matches!(self.current_token(), Some(Token::DotDot)) {
            self.advance();
            let right = self.parse_additive()?;
            let span = SimpleSpan::new((), 0..0);
            return Ok(Expr {
                kind: ExprKind::Range {
                    start: Box::new(left),
                    end: Box::new(right),
                },
                span,
            });
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_multiplicative()?;

        while let Some(op) = self.current_token() {
            let binary_op = match op {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            let span = SimpleSpan::new((), 0..0);
            left = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op: binary_op,
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_power()?;

        while let Some(op) = self.current_token() {
            let binary_op = match op {
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                Token::Percent => BinaryOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_power()?;
            let span = SimpleSpan::new((), 0..0);
            left = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op: binary_op,
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn parse_power(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_unary()?;

        while matches!(self.current_token(), Some(Token::Caret)) {
            self.advance();
            let right = self.parse_unary()?;
            let span = SimpleSpan::new((), 0..0);
            left = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op: BinaryOp::Pow,
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        match self.current_token() {
            Some(Token::Bang) => {
                self.advance();
                let expr = self.parse_unary()?;
                let span = SimpleSpan::new((), 0..0);
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    },
                    span,
                })
            }
            Some(Token::Minus) => {
                self.advance();
                let expr = self.parse_unary()?;
                let span = SimpleSpan::new((), 0..0);
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Neg,
                        expr: Box::new(expr),
                    },
                    span,
                })
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.current_token() {
            Some(Token::Dot) => {
                    self.advance();
                    let field = match self.advance() {
                        Some(Token::Ident(f)) => f,
                        other => return Err(format!("Expected field name, found {:?}", other)),
                    };
                    let span = SimpleSpan::new((), 0..0);
                    expr = Expr {
                        kind: ExprKind::FieldAccess {
                            object: Box::new(expr),
                            field,
                        },
                        span,
                    };
                }
                Some(Token::QuestionDot) => {
                    self.advance();
                    let field = match self.advance() {
                        Some(Token::Ident(f)) => f,
                        other => return Err(format!("Expected field name, found {:?}", other)),
                    };
                    let span = SimpleSpan::new((), 0..0);
                    expr = Expr {
                        kind: ExprKind::OptionalFieldAccess {
                            object: Box::new(expr),
                            field,
                        },
                        span,
                    };
                }
                Some(Token::LBracket) => {
                    self.advance();
                    let index = self.parse_expression()?;
                    self.expect(Token::RBracket)?;
                    let span = SimpleSpan::new((), 0..0);
                    expr = Expr {
                        kind: ExprKind::ArrayIndex {
                            array: Box::new(expr),
                            index: Box::new(index),
                        },
                        span,
                    };
                }
                Some(Token::LParen) => {
                    self.advance();
                    let mut args = Vec::new();
                    if !matches!(self.current_token(), Some(Token::RParen)) {
                        loop {
                            args.push(self.parse_expression()?);
                            if matches!(self.current_token(), Some(Token::Comma)) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(Token::RParen)?;
                    let span = SimpleSpan::new((), 0..0);
                    
                    if let ExprKind::Identifier(name) = expr.kind {
                        expr = Expr {
                            kind: ExprKind::Call { name, args },
                            span,
                        };
                    } else {
                        return Err("Only identifiers can be called as functions".to_string());
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        let span = SimpleSpan::new((), 0..0);

        match self.advance() {
            Some(Token::Number(n)) => Ok(Expr {
                kind: ExprKind::Literal(Value::Number(n)),
                span,
            }),
            Some(Token::String(s)) => Ok(Expr {
                kind: ExprKind::Literal(Value::String(Rc::from(s.as_str()))),
                span,
            }),
            Some(Token::True) => Ok(Expr {
                kind: ExprKind::Literal(Value::Bool(true)),
                span,
            }),
            Some(Token::False) => Ok(Expr {
                kind: ExprKind::Literal(Value::Bool(false)),
                span,
            }),
            Some(Token::Null) => Ok(Expr {
                kind: ExprKind::Literal(Value::Null),
                span,
            }),
            Some(Token::Ident(name)) => Ok(Expr {
                kind: ExprKind::Identifier(Rc::from(name.as_str())),
                span,
            }),
            Some(Token::Filter) => Ok(Expr {
                kind: ExprKind::Identifier(Rc::from("filter")),
                span,
            }),
            Some(Token::Map) => Ok(Expr {
                kind: ExprKind::Identifier(Rc::from("map")),
                span,
            }),
            Some(Token::LParen) => {
                let expr = self.parse_expression()?;
                self.expect(Token::RParen)?;
                Ok(Expr {
                    kind: ExprKind::Grouped(Box::new(expr)),
                    span,
                })
            }
            Some(Token::LBracket) => {
                let mut elements = Vec::new();
                if !matches!(self.current_token(), Some(Token::RBracket)) {
                    loop {
                        elements.push(self.parse_expression()?);
                        if matches!(self.current_token(), Some(Token::Comma)) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                self.expect(Token::RBracket)?;
                Ok(Expr {
                    kind: ExprKind::Array { elements },
                    span,
                })
            }
            Some(Token::LBrace) => {
                let mut fields = Vec::new();
                if !matches!(self.current_token(), Some(Token::RBrace)) {
                    loop {
                        let key = match self.advance() {
                            Some(Token::Ident(k)) => k,
                            Some(Token::String(k)) => k,
                            other => return Err(format!("Expected identifier or string for object key, found {:?}", other)),
                        };
                        self.expect(Token::Colon)?;
                        let value = self.parse_expression()?;
                        fields.push((key, value));
                        if matches!(self.current_token(), Some(Token::Comma)) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                self.expect(Token::RBrace)?;
                Ok(Expr {
                    kind: ExprKind::Object { fields },
                    span,
                })
            }
            Some(Token::Dot) => {
                let field = match self.advance() {
                    Some(Token::Ident(f)) => f,
                    other => {
                        return Err(format!("Expected field name after dot, found {:?}", other));
                    }
                };
                Ok(Expr {
                    kind: ExprKind::FieldAccess {
                        object: Box::new(Expr {
                            kind: ExprKind::Literal(Value::Null),
                            span,
                        }),
                        field,
                    },
                    span,
                })
            }
            other => Err(format!(
                "Unexpected token in primary expression: {:?}",
                other
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    vars: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            vars: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    pub fn with_parent(parent: Rc<Environment>) -> Self {
        Self {
            vars: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(parent),
        }
    }

    pub fn set(&self, name: String, value: Value) {
        self.vars.borrow_mut().insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.vars.borrow().get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn update(&self, name: &str, value: Value) -> bool {
        if self.vars.borrow().contains_key(name) {
            self.vars.borrow_mut().insert(name.to_string(), value);
            true
        } else if let Some(parent) = &self.parent {
            parent.update(name, value)
        } else {
            false
        }
    }
}

pub enum ControlFlow {
    Next,
    Value(Value),
    Return(Value),
}

pub struct Interpreter {
    env: Environment,
}

#[derive(Debug, Clone)]
pub enum InterpreterError {
    UndefinedVariable(String),
    TypeError(String),
    IndexOutOfBounds(usize),
    FieldNotFound(String),
    DivisionByZero,
    InvalidOperation(String),
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::UndefinedVariable(name) => {
                write!(f, "Undefined variable: {}", name)
            }
            InterpreterError::TypeError(msg) => write!(f, "Type error: {}", msg),
            InterpreterError::IndexOutOfBounds(idx) => {
                write!(f, "Index out of bounds: {}", idx)
            }
            InterpreterError::FieldNotFound(field) => {
                write!(f, "Field not found: {}", field)
            }
            InterpreterError::DivisionByZero => write!(f, "Division by zero"),
            InterpreterError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
        }
    }
}

impl std::error::Error for InterpreterError {}

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
                ControlFlow::Value(val) => {
                    last_val = Some(val);
                }
                ControlFlow::Next => {
                    last_val = None;
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
        if let ExprKind::Call { name, .. } = &right.kind {
            match name.as_ref() {
                "filter" | "map" | "sort_by" | "group_by" | "take" | "unique" => {
                }
                _ => {}
            }
        }

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
            "count" => {
                if args.is_empty() { return Ok(Value::Number(0.0)); }
                if let Value::Array(arr) = &args[0] {
                    Ok(Value::Number(arr.borrow().len() as f64))
                } else {
                    Err(InterpreterError::TypeError("count requires array".to_string()))
                }
            }
            "sum" => {
                if args.is_empty() { return Ok(Value::Number(0.0)); }
                if let Value::Array(arr) = &args[0] {
                    let mut s = 0.0;
                    for v in arr.borrow().iter() {
                        if let Value::Number(n) = v { s += *n; }
                    }
                    Ok(Value::Number(s))
                } else {
                    Err(InterpreterError::TypeError("sum requires array".to_string()))
                }
            }
            "avg" => {
                if args.is_empty() { return Ok(Value::Number(0.0)); }
                if let Value::Array(arr_rc) = &args[0] {
                    let arr = arr_rc.borrow();
                    if arr.is_empty() { return Ok(Value::Number(0.0)); }
                    let mut s = 0.0;
                    for v in arr.iter() {
                        if let Value::Number(n) = v { s += *n; }
                    }
                    Ok(Value::Number(s / arr.len() as f64))
                } else {
                    Err(InterpreterError::TypeError("avg requires array".to_string()))
                }
            }
            "min" => {
                if args.is_empty() { return Ok(Value::Number(0.0)); }
                if let Value::Array(arr) = &args[0] {
                    let mut m = f64::MAX;
                    for v in arr.borrow().iter() {
                        if let Value::Number(n) = v { if *n < m { m = *n; } }
                    }
                    Ok(Value::Number(m))
                } else {
                    Err(InterpreterError::TypeError("min requires array".to_string()))
                }
            }
            "max" => {
                if args.is_empty() { return Ok(Value::Number(0.0)); }
                if let Value::Array(arr) = &args[0] {
                    let mut m = f64::MIN;
                    for v in arr.borrow().iter() {
                        if let Value::Number(n) = v { if *n > m { m = *n; } }
                    }
                    Ok(Value::Number(m))
                } else {
                    Err(InterpreterError::TypeError("max requires array".to_string()))
                }
            }
            "take" => {
                if args.len() < 2 { return Err(InterpreterError::InvalidOperation("take requires array and count".to_string())); }
                if let (Value::Array(arr), Value::Number(n)) = (&args[0], &args[1]) {
                    let count = *n as usize;
                    let borrowed = arr.borrow();
                    let result: Vec<Value> = borrowed.iter().take(count).cloned().collect();
                    Ok(Value::Array(Rc::new(RefCell::new(result))))
                } else {
                    Err(InterpreterError::TypeError("take requires array and number".to_string()))
                }
            }
            "unique" => {
                if args.is_empty() { return Err(InterpreterError::InvalidOperation("unique requires argument".to_string())); }
                if let Value::Array(arr) = &args[0] {
                    let mut result = Vec::new();
                    for v in arr.borrow().iter() {
                        if !result.iter().any(|existing| self.deep_equals(existing, v)) {
                            result.push(v.clone());
                        }
                    }
                    Ok(Value::Array(Rc::new(RefCell::new(result))))
                } else {
                    Err(InterpreterError::TypeError("unique requires array".to_string()))
                }
            }
            "push" => {
                if args.len() < 2 { return Err(InterpreterError::InvalidOperation("push requires array and value".to_string())); }
                if let Value::Array(arr) = &args[0] {
                    arr.borrow_mut().push(args[1].clone());
                    Ok(args[0].clone())
                } else {
                    Err(InterpreterError::TypeError("push requires array as first argument".to_string()))
                }
            }
            "sort_by" => {
                if args.len() < 2 { return Err(InterpreterError::InvalidOperation("sort_by requires array and field name".to_string())); }
                if let (Value::Array(arr), Value::String(field)) = (&args[0], &args[1]) {
                    arr.borrow_mut().sort_by(|a, b| {
                        let val_a = self.get_field(a, field).unwrap_or(Value::Null);
                        let val_b = self.get_field(b, field).unwrap_or(Value::Null);
                        match (val_a, val_b) {
                            (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(&n2).unwrap_or(std::cmp::Ordering::Equal),
                            (Value::String(s1), Value::String(s2)) => s1.cmp(&s2),
                            _ => std::cmp::Ordering::Equal,
                        }
                    });
                    Ok(args[0].clone())
                } else {
                    Err(InterpreterError::TypeError("sort_by requires array and field name string".to_string()))
                }
            }
            "group_by" => {
                if args.len() < 2 { return Err(InterpreterError::InvalidOperation("group_by requires array and field name".to_string())); }
                if let (Value::Array(arr_rc), Value::String(field)) = (&args[0], &args[1]) {
                    let mut groups: IndexMap<String, Vec<Value>> = IndexMap::new();
                    let arr = arr_rc.borrow();
                    for item in arr.iter() {
                        let key_val = self.get_field(item, field).unwrap_or(Value::Null);
                        let key_str = match key_val {
                            Value::String(s) => s.to_string(),
                            Value::Number(n) => n.to_string(),
                            Value::Bool(b) => b.to_string(),
                            _ => "null".to_string(),
                        };
                        groups.entry(key_str).or_insert_with(Vec::new).push(item.clone());
                    }
                    let mut result_obj = IndexMap::new();
                    for (k, v) in groups {
                        result_obj.insert(k, Value::Array(Rc::new(RefCell::new(v))));
                    }
                    Ok(Value::Object(Rc::new(RefCell::new(result_obj))))
                } else {
                    Err(InterpreterError::TypeError("group_by requires array and field name string".to_string()))
                }
            }
            _ => Err(InterpreterError::InvalidOperation(format!("Unknown function: {}", name))),
        }
    }

    fn get_field(&self, obj: &Value, field: &str) -> Result<Value, InterpreterError> {
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

    fn deep_equals(&self, a: &Value, b: &Value) -> bool {
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
    fn test_field_access() {
        let source = "let users = root.users;";
        let root = make_test_root();
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_arithmetic() {
        let source = "let result = 10 + 5 * 2;";
        let root = Value::Null;
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_comparison() {
        let source = "let check = 10 > 5;";
        let root = Value::Null;
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_pipe_filter() {
        let source = r#"
            let users = root.users;
            let bobs = users | .name == "Bob";
        "#;
        let root = make_test_root();
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_array_indexing() {
        let source = "let first = root.users[0];";
        let root = make_test_root();
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_logical_operators() {
        let source = "let result = true && false || true;";
        let root = Value::Null;
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_nested_field_access() {
        let source = "let first_name = root.users[0].name;";
        let root = make_test_root();
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_filter() {
        let source = r#"let result = root.users | .age > 25;"#;
        let root = make_test_root();
        let result = parse_and_run(source, root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_combined_filter() {
        let source = r#"let result = root.users | .name == "Bob" && .age > 30;"#;
        let root = make_test_root();
        let result = parse_and_run(source, root);
        if let Err(e) = &result {
            println!("Error: {}", e);
        }
        assert!(result.is_ok());
    }
}
