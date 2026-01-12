use crate::lexer::{BinaryOp, Expr, ExprKind, Stmt, Token, UnaryOp, Value};
use chumsky::span::{SimpleSpan, Span};
use std::rc::Rc;

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

