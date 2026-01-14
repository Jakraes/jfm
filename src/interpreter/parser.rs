use crate::ast::{ArrayElement, BinaryOp, Expr, ExprKind, MatchPattern, ObjectEntry, Stmt, UnaryOp};
use crate::diagnostic::{Diagnostic, Label, Span};
use crate::token::Token;
use crate::value::Value;
use chumsky::Parser as _;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub expected: Vec<String>,
    pub found: Option<String>,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            expected: Vec::new(),
            found: None,
        }
    }

    pub fn with_expected(mut self, expected: Vec<String>) -> Self {
        self.expected = expected;
        self
    }

    pub fn with_found(mut self, found: impl Into<String>) -> Self {
        self.found = Some(found.into());
        self
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        let mut msg = self.message.clone();
        if !self.expected.is_empty() {
            msg = format!("expected {}", self.expected.join(" or "));
            if let Some(found) = &self.found {
                msg.push_str(&format!(", found {}", found));
            }
        }

        let mut diag = Diagnostic::error(msg)
            .with_code("E0101")
            .with_label(Label::primary(self.span, ""));

        if !self.expected.is_empty() && self.expected.len() == 1 {
            diag = diag.with_help(format!("expected {} here", self.expected[0]));
        }

        diag
    }
}

pub struct ParseResult {
    pub statements: Vec<Stmt>,
    pub errors: Vec<ParseError>,
}

impl ParseResult {
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
}

pub struct TokenParser {
    tokens: Vec<SpannedToken>,
    current: usize,
    errors: Vec<ParseError>,
    source_len: usize,
}

impl TokenParser {
    pub fn new(tokens: Vec<SpannedToken>, source_len: usize) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
            source_len,
        }
    }

    pub fn from_lexer_output(tokens: Vec<(Token, chumsky::span::SimpleSpan)>, source_len: usize) -> Self {
        let spanned_tokens: Vec<SpannedToken> = tokens
            .into_iter()
            .map(|(token, span)| SpannedToken {
                token,
                span: Span::new(span.start, span.end),
            })
            .collect();
        Self::new(spanned_tokens, source_len)
    }

    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.current).map(|st| &st.token)
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.current)
            .map(|st| st.span)
            .unwrap_or_else(|| Span::new(self.source_len, self.source_len))
    }

    fn previous_span(&self) -> Span {
        if self.current > 0 {
            self.tokens[self.current - 1].span
        } else {
            Span::new(0, 0)
        }
    }

    fn advance(&mut self) -> Option<SpannedToken> {
        if self.current < self.tokens.len() {
            let st = self.tokens[self.current].clone();
            self.current += 1;
            Some(st)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: Token) -> Result<Span, ParseError> {
        match self.current_token() {
            Some(token) if std::mem::discriminant(token) == std::mem::discriminant(&expected) => {
                let span = self.current_span();
                self.advance();
                Ok(span)
            }
            Some(token) => {
                let err = ParseError::new("unexpected token", self.current_span())
                    .with_expected(vec![format!("{:?}", expected)])
                    .with_found(format!("{:?}", token));
                Err(err)
            }
            None => {
                let err = ParseError::new("unexpected end of input", self.current_span())
                    .with_expected(vec![format!("{:?}", expected)]);
                Err(err)
            }
        }
    }

    fn add_error(&mut self, error: ParseError) {
        self.errors.push(error);
    }

    fn synchronize(&mut self) {
        while let Some(token) = self.current_token() {
            match token {
                Token::Semicolon => {
                    self.advance();
                    return;
                }
                Token::Let | Token::For | Token::While | Token::If | Token::Return | Token::Fn => {
                    return;
                }
                Token::RBrace => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();
        while self.current_token().is_some() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.add_error(err.clone());
                    self.synchronize();
                    if self.errors.len() >= 10 {
                        break;
                    }
                }
            }
        }

        if self.errors.is_empty() {
            Ok(statements)
        } else {
            // Return first error for backward compatibility
            Err(self.errors[0].message.clone())
        }
    }

    pub fn parse_with_errors(&mut self) -> ParseResult {
        let mut statements = Vec::new();
        while self.current_token().is_some() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.add_error(err);
                    self.synchronize();
                    if self.errors.len() >= 10 {
                        break;
                    }
                }
            }
        }
        ParseResult {
            statements,
            errors: std::mem::take(&mut self.errors),
        }
    }

    pub fn get_errors(&self) -> &[ParseError] {
        &self.errors
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.current_token() {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::For) => self.parse_for_statement(),
            Some(Token::While) => self.parse_while_statement(),
            Some(Token::If) => self.parse_if_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::Break) => {
                self.advance();
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Break)
            }
            Some(Token::Continue) => {
                self.advance();
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Continue)
            }
            Some(Token::Fn) => self.parse_function_statement(),
            Some(Token::LBrace) => {
                let block = self.parse_block()?;
                Ok(Stmt::Block(block))
            }
            Some(Token::Ident(_)) => {
                // Try short function syntax: name(x, y) => expr
                if let Some(func) = self.try_parse_short_function()? {
                    return Ok(func);
                }
                // Otherwise parse as expression
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }
    
    /// Try to parse short function syntax: `name(x, y) => expr`
    /// Returns None if the pattern doesn't match (restores parser position)
    fn try_parse_short_function(&mut self) -> Result<Option<Stmt>, ParseError> {
        let saved = self.current;
        
        // Get function name
        let name = match self.advance() {
            Some(SpannedToken { token: Token::Ident(n), .. }) => Rc::from(n.as_str()),
            _ => {
                self.current = saved;
                return Ok(None);
            }
        };
        
        // Expect (
        if !matches!(self.current_token(), Some(Token::LParen)) {
            self.current = saved;
            return Ok(None);
        }
        self.advance();
        
        // Parse parameters
        let mut params = Vec::new();
        if !matches!(self.current_token(), Some(Token::RParen)) {
            loop {
                match self.advance() {
                    Some(SpannedToken { token: Token::Ident(p), .. }) => {
                        params.push(Rc::from(p.as_str()));
                    }
                    _ => {
                        self.current = saved;
                        return Ok(None);
                    }
                }
                if matches!(self.current_token(), Some(Token::Comma)) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        
        // Expect )
        if !matches!(self.current_token(), Some(Token::RParen)) {
            self.current = saved;
            return Ok(None);
        }
        self.advance();
        
        // Expect =>
        if !matches!(self.current_token(), Some(Token::Arrow)) {
            self.current = saved;
            return Ok(None);
        }
        self.advance();
        
        // Parse body expression
        let body_expr = self.parse_expression()?;
        
        // Convert to return statement
        let body = vec![Stmt::Return(Some(body_expr))];
        
        // Optionally consume trailing semicolon (short functions can end with or without it)
        if matches!(self.current_token(), Some(Token::Semicolon)) {
            self.advance();
        }
        
        Ok(Some(Stmt::Function { name, params, body }))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        self.expect(Token::LBrace)?;
        let mut statements = Vec::new();
        while !matches!(self.current_token(), Some(Token::RBrace)) && self.current_token().is_some()
        {
            statements.push(self.parse_statement()?);
        }
        self.expect(Token::RBrace)?;
        Ok(statements)
    }

    fn parse_for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::For)?;
        let var = match self.advance() {
            Some(SpannedToken { token: Token::Ident(identifier_name), .. }) => Rc::from(identifier_name.as_str()),
            other => {
                let span = other.as_ref().map(|st| st.span).unwrap_or(self.current_span());
                return Err(ParseError::new("expected identifier in for loop", span)
                    .with_expected(vec!["identifier".to_string()])
                    .with_found(format!("{:?}", other.map(|st| st.token))));
            }
        };
        self.expect(Token::In)?;
        let iterable = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(Stmt::For { var, iterable, body })
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::While)?;
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(Stmt::While { condition, body })
    }

    fn parse_function_statement(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Fn)?;
        let name = match self.advance() {
            Some(SpannedToken { token: Token::Ident(identifier_name), .. }) => Rc::from(identifier_name.as_str()),
            other => {
                let span = other.map(|st| st.span).unwrap_or(self.current_span());
                return Err(ParseError::new("expected function name", span)
                    .with_expected(vec!["identifier".to_string()]));
            }
        };
        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        if !matches!(self.current_token(), Some(Token::RParen)) {
            loop {
                let param = match self.advance() {
                    Some(SpannedToken { token: Token::Ident(param_name), .. }) => Rc::from(param_name.as_str()),
                    other => {
                        let span = other.map(|st| st.span).unwrap_or(self.current_span());
                        return Err(ParseError::new("expected parameter name", span));
                    }
                };
                params.push(param);
                if matches!(self.current_token(), Some(Token::Comma)) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RParen)?;
        let body = self.parse_block()?;
        Ok(Stmt::Function { name, params, body })
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, ParseError> {
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

    fn parse_return_statement(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Return)?;
        let value = if !matches!(self.current_token(), Some(Token::Semicolon)) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect(Token::Semicolon)?;
        Ok(Stmt::Return(value))
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Token::Let)?;

        let name = match self.advance() {
            Some(SpannedToken { token: Token::Ident(identifier_name), .. }) => identifier_name,
            other => {
                let span = other.map(|st| st.span).unwrap_or(self.current_span());
                return Err(ParseError::new("expected identifier after 'let'", span)
                    .with_expected(vec!["identifier".to_string()]));
            }
        };

        self.expect(Token::Assign)?;
        let value = self.parse_expression()?;
        self.expect(Token::Semicolon)?;

        Ok(Stmt::Let {
            name: Rc::from(name.as_str()),
            value,
        })
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let start_span = self.current_span();
        let left = self.parse_lambda()?;

        if matches!(self.current_token(), Some(Token::Assign)) {
            self.advance();
            let right = self.parse_assignment()?;
            let span = start_span.merge(right.span);
            return Ok(Expr {
                kind: ExprKind::Assignment {
                    target: Box::new(left),
                    value: Box::new(right),
                },
                span,
            });
        }

        Ok(left)
    }

    fn parse_lambda(&mut self) -> Result<Expr, ParseError> {
        let saved_current = self.current;
        let start_span = self.current_span();

        let params = if matches!(self.current_token(), Some(Token::LParen)) {
            self.advance();
            let mut parsed_params = Vec::new();
            if !matches!(self.current_token(), Some(Token::RParen)) {
                loop {
                    let param = match self.advance() {
                        Some(SpannedToken { token: Token::Ident(param_name), .. }) => Rc::from(param_name.as_str()),
                        _ => {
                            self.current = saved_current;
                            return self.parse_pipe();
                        }
                    };
                    parsed_params.push(param);
                    if matches!(self.current_token(), Some(Token::Comma)) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            if !matches!(self.current_token(), Some(Token::RParen)) {
                self.current = saved_current;
                return self.parse_pipe();
            }
            self.advance();

            if matches!(self.current_token(), Some(Token::Arrow)) {
                parsed_params
            } else {
                self.current = saved_current;
                return self.parse_pipe();
            }
        } else if let Some(Token::Ident(_)) = self.current_token() {
            let param_name = match self.advance() {
                Some(SpannedToken { token: Token::Ident(identifier_name), .. }) => Rc::from(identifier_name.as_str()),
                _ => {
                    self.current = saved_current;
                    return self.parse_pipe();
                }
            };

            if matches!(self.current_token(), Some(Token::Arrow)) {
                vec![param_name]
            } else {
                self.current = saved_current;
                return self.parse_pipe();
            }
        } else {
            return self.parse_pipe();
        };

        self.advance();
        let body = self.parse_expression()?;
        let span = start_span.merge(body.span);

        Ok(Expr {
            kind: ExprKind::Lambda {
                params,
                body: Box::new(body),
            },
            span,
        })
    }

    fn parse_pipe(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_ternary()?;

        while matches!(self.current_token(), Some(Token::Pipe)) {
            self.advance();
            // Parse right side allowing assignments (for pipe-first mutations like .id *= 2)
            let right = self.parse_pipe_right()?;
            let span = left.span.merge(right.span);
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

    // Parse the right side of a pipe, allowing lambdas for map/filter
    fn parse_pipe_right(&mut self) -> Result<Expr, ParseError> {
        let saved_current = self.current;
        let start_span = self.current_span();

        // Check for [n] indexing - shorthand for indexing the pipe result
        if matches!(self.current_token(), Some(Token::LBracket)) {
            self.advance();
            let index = self.parse_expression()?;
            let end_span = self.expect(Token::RBracket)?;
            let span = start_span.merge(end_span);
            // Create an array index on the pipe context (null placeholder, will be resolved at runtime)
            return Ok(Expr {
                kind: ExprKind::ArrayIndex {
                    array: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Null),
                        span: start_span,
                    }),
                    index: Box::new(index),
                },
                span,
            });
        }

        // Try to parse a lambda (x => expr or (x, y) => expr)
        if let Some(Token::Ident(_)) = self.current_token() {
            let param_name = match self.advance() {
                Some(SpannedToken { token: Token::Ident(name), .. }) => Rc::from(name.as_str()),
                _ => {
                    self.current = saved_current;
                    return self.parse_ternary();
                }
            };

            if matches!(self.current_token(), Some(Token::Arrow)) {
                self.advance();
                let body = self.parse_pipe_right()?;
                let span = start_span.merge(body.span);
                return Ok(Expr {
                    kind: ExprKind::Lambda {
                        params: vec![param_name],
                        body: Box::new(body),
                    },
                    span,
                });
            } else {
                self.current = saved_current;
            }
        } else if matches!(self.current_token(), Some(Token::LParen)) {
            self.advance();
            let mut params = Vec::new();
            if !matches!(self.current_token(), Some(Token::RParen)) {
                loop {
                    let param = match self.advance() {
                        Some(SpannedToken { token: Token::Ident(name), .. }) => Rc::from(name.as_str()),
                        _ => {
                            self.current = saved_current;
                            return self.parse_ternary();
                        }
                    };
                    params.push(param);
                    if matches!(self.current_token(), Some(Token::Comma)) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            if !matches!(self.current_token(), Some(Token::RParen)) {
                self.current = saved_current;
                return self.parse_ternary();
            }
            self.advance();

            if matches!(self.current_token(), Some(Token::Arrow)) {
                self.advance();
                let body = self.parse_pipe_right()?;
                let span = start_span.merge(body.span);
                return Ok(Expr {
                    kind: ExprKind::Lambda {
                        params,
                        body: Box::new(body),
                    },
                    span,
                });
            } else {
                self.current = saved_current;
            }
        }

        // Parse expression, then check for assignment
        let start_span = self.current_span();
        let left = self.parse_ternary()?;
        
        // Check for assignment (e.g., .id = 1)
        if matches!(self.current_token(), Some(Token::Assign)) {
            self.advance();
            let right = self.parse_pipe_right()?;
            let span = start_span.merge(right.span);
            return Ok(Expr {
                kind: ExprKind::Assignment {
                    target: Box::new(left),
                    value: Box::new(right),
                },
                span,
            });
        }
        
        Ok(left)
    }

    fn parse_ternary(&mut self) -> Result<Expr, ParseError> {
        let condition = self.parse_null_coalesce()?;

        if matches!(self.current_token(), Some(Token::Question)) {
            self.advance();
            let then_branch = self.parse_ternary()?;
            self.expect(Token::Colon)?;
            let else_branch = self.parse_ternary()?;
            let span = condition.span.merge(else_branch.span);
            return Ok(Expr {
                kind: ExprKind::Ternary {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
                span,
            });
        }

        Ok(condition)
    }

    fn parse_null_coalesce(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_binary_expr(0)?;

        while matches!(self.current_token(), Some(Token::NullCoalesce)) {
            self.advance();
            let right = self.parse_binary_expr(0)?;
            let span = left.span.merge(right.span);
            left = Expr {
                kind: ExprKind::NullCoalesce {
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn token_to_binary_operator(token: &Token) -> Option<(u8, BinaryOp)> {
        match token {
            Token::Or => Some((1, BinaryOp::Or)),
            Token::And => Some((2, BinaryOp::And)),
            Token::Eq => Some((3, BinaryOp::Eq)),
            Token::NotEq => Some((3, BinaryOp::NotEq)),
            Token::Greater => Some((3, BinaryOp::Greater)),
            Token::Less => Some((3, BinaryOp::Less)),
            Token::GreaterEq => Some((3, BinaryOp::GreaterEq)),
            Token::LessEq => Some((3, BinaryOp::LessEq)),
            Token::Plus => Some((5, BinaryOp::Add)),
            Token::Minus => Some((5, BinaryOp::Sub)),
            Token::Star => Some((6, BinaryOp::Mul)),
            Token::Slash => Some((6, BinaryOp::Div)),
            Token::Percent => Some((6, BinaryOp::Mod)),
            Token::Caret => Some((7, BinaryOp::Pow)),
            _ => None,
        }
    }

    fn parse_binary_expr(&mut self, min_precedence: u8) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;

        const RANGE_OPERATOR_PRECEDENCE: u8 = 4;
        if min_precedence <= RANGE_OPERATOR_PRECEDENCE && matches!(self.current_token(), Some(Token::DotDot)) {
            self.advance();
            let right = self.parse_binary_expr(RANGE_OPERATOR_PRECEDENCE + 1)?;
            let span = left.span.merge(right.span);
            left = Expr {
                kind: ExprKind::Range {
                    start: Box::new(left),
                    end: Box::new(right),
                },
                span,
            };
            return Ok(left);
        }

        while let Some(token) = self.current_token().cloned() {
            let (precedence, operator) = match Self::token_to_binary_operator(&token) {
                Some((prec, op)) if prec >= min_precedence => (prec, op),
                _ => break,
            };
            self.advance();
            let right = self.parse_binary_expr(precedence + 1)?;
            let span = left.span.merge(right.span);
            left = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    op: operator,
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        let start_span = self.current_span();
        match self.current_token() {
            Some(Token::Bang) => {
                self.advance();
                let expr = self.parse_unary()?;
                let span = start_span.merge(expr.span);
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
                let span = start_span.merge(expr.span);
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

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.current_token() {
                Some(Token::Dot) => {
                    self.advance();
                    let field_span = self.current_span();
                    let field = match self.advance() {
                        Some(SpannedToken { token: Token::Ident(field_name), .. }) => field_name,
                        other => {
                            return Err(ParseError::new(
                                "expected field name after '.'",
                                other.map(|st| st.span).unwrap_or(field_span),
                            )
                            .with_expected(vec!["identifier".to_string()]));
                        }
                    };
                    
                    // Check for method call: obj.method(args)
                    if matches!(self.current_token(), Some(Token::LParen)) {
                        self.advance(); // consume (
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
                        let end_span = self.expect(Token::RParen)?;
                        let span = expr.span.merge(end_span);
                        expr = Expr {
                            kind: ExprKind::MethodCall {
                                object: Box::new(expr),
                                method: field,
                                args,
                            },
                            span,
                        };
                    } else {
                        let span = expr.span.merge(self.previous_span());
                        expr = Expr {
                            kind: ExprKind::FieldAccess {
                                object: Box::new(expr),
                                field,
                            },
                            span,
                        };
                    }
                }
                Some(Token::QuestionDot) => {
                    self.advance();
                    let field_span = self.current_span();
                    let field = match self.advance() {
                        Some(SpannedToken { token: Token::Ident(field_name), .. }) => field_name,
                        other => {
                            return Err(ParseError::new(
                                "expected field name after '?.'",
                                other.map(|st| st.span).unwrap_or(field_span),
                            ));
                        }
                    };
                    let span = expr.span.merge(self.previous_span());
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
                    let end_span = self.expect(Token::RBracket)?;
                    let span = expr.span.merge(end_span);
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
                    let end_span = self.expect(Token::RParen)?;
                    let span = expr.span.merge(end_span);

                    if let ExprKind::Identifier(name) = expr.kind {
                        expr = Expr {
                            kind: ExprKind::Call { name, args },
                            span,
                        };
                    } else {
                        return Err(ParseError::new(
                            "only identifiers can be called as functions",
                            expr.span,
                        ));
                    }
                }
                Some(Token::As) => {
                    // Handle `expr as name` binding
                    self.advance();
                    let name = match self.advance() {
                        Some(SpannedToken { token: Token::Ident(n), .. }) => Rc::from(n.as_str()),
                        other => {
                            let err_span = other.map(|st| st.span).unwrap_or(self.current_span());
                            return Err(ParseError::new(
                                "expected identifier after 'as'",
                                err_span,
                            ));
                        }
                    };
                    let span = expr.span.merge(self.previous_span());
                    expr = Expr {
                        kind: ExprKind::AsBinding {
                            expr: Box::new(expr),
                            name,
                        },
                        span,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_template_literal(&mut self, encoded: String, span: Span) -> Result<Expr, ParseError> {
        use crate::ast::TemplatePart;
        
        // Check if it's a simple template (no interpolations)
        if !encoded.starts_with("TPL:") {
            // Simple template literal - just a string
            return Ok(Expr {
                kind: ExprKind::TemplateLiteral {
                    parts: vec![TemplatePart::Literal(encoded)],
                },
                span,
            });
        }

        // Parse encoded template: TPL:TEXT:...\x00EXPR:...\x00TEXT:...
        let content = &encoded[4..]; // Skip "TPL:"
        let segments: Vec<&str> = content.split('\x00').collect();
        
        let mut parts = Vec::new();
        
        for segment in segments {
            if let Some(text) = segment.strip_prefix("TEXT:") {
                // Text segment
                if !text.is_empty() {
                    parts.push(TemplatePart::Literal(text.to_string()));
                }
            } else if let Some(expr_source) = segment.strip_prefix("EXPR:") {
                // Expression segment - need to parse it
                if !expr_source.is_empty() {
                    // Tokenize and parse the expression
                    let tokens = crate::lexer::lexer()
                        .parse(expr_source)
                        .into_output()
                        .ok_or_else(|| ParseError::new(
                            format!("failed to tokenize interpolation: {}", expr_source),
                            span,
                        ))?;
                    
                    let mut sub_parser = TokenParser::from_lexer_output(tokens, expr_source.len());
                    let expr = sub_parser.parse_expression().map_err(|e| {
                        ParseError::new(
                            format!("error in interpolation '{}': {}", expr_source, e.message),
                            span,
                        )
                    })?;
                    
                    parts.push(TemplatePart::Interpolation(Box::new(expr)));
                }
            }
        }

        // If we end up with a single literal part, still wrap it
        Ok(Expr {
            kind: ExprKind::TemplateLiteral { parts },
            span,
        })
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let st = self.advance();
        let (token, span) = match st {
            Some(SpannedToken { token, span }) => (token, span),
            None => {
                return Err(ParseError::new(
                    "unexpected end of input",
                    Span::new(self.source_len, self.source_len),
                )
                .with_expected(vec!["expression".to_string()]));
            }
        };

        match token {
            Token::Number(numeric_value, is_float) => Ok(Expr {
                kind: ExprKind::Literal(Value::Number(numeric_value, is_float)),
                span,
            }),
            Token::String(string_value) => Ok(Expr {
                kind: ExprKind::Literal(Value::String(Rc::from(string_value.as_str()))),
                span,
            }),
            Token::TemplateFull(template_content) => {
                self.parse_template_literal(template_content, span)
            },
            Token::True => Ok(Expr {
                kind: ExprKind::Literal(Value::Bool(true)),
                span,
            }),
            Token::False => Ok(Expr {
                kind: ExprKind::Literal(Value::Bool(false)),
                span,
            }),
            Token::Null => Ok(Expr {
                kind: ExprKind::Literal(Value::Null),
                span,
            }),
            Token::Ident(name) => Ok(Expr {
                kind: ExprKind::Identifier(Rc::from(name.as_str())),
                span,
            }),
            Token::Filter => Ok(Expr {
                kind: ExprKind::Identifier(Rc::from("filter")),
                span,
            }),
            Token::Map => Ok(Expr {
                kind: ExprKind::Identifier(Rc::from("map")),
                span,
            }),
            Token::LParen => {
                let expr = self.parse_expression()?;
                let end_span = self.expect(Token::RParen)?;
                Ok(Expr {
                    kind: ExprKind::Grouped(Box::new(expr)),
                    span: span.merge(end_span),
                })
            }
            Token::LBracket => {
                let start_span = span;
                let mut elements = Vec::new();
                let mut has_spread = false;
                if !matches!(self.current_token(), Some(Token::RBracket)) {
                    loop {
                        // Check for spread operator
                        if matches!(self.current_token(), Some(Token::DotDot)) {
                            self.advance(); // consume first dot
                            // DotDot is ".." so we need one more "."
                            if !matches!(self.current_token(), Some(Token::Dot)) {
                                return Err(ParseError::new(
                                    "expected '...' for spread operator",
                                    self.current_span(),
                                ));
                            }
                            self.advance(); // consume the third dot
                            let spread_expr = self.parse_expression()?;
                            elements.push(ArrayElement::Spread(spread_expr));
                            has_spread = true;
                        } else {
                            elements.push(ArrayElement::Single(self.parse_expression()?));
                        }
                        if matches!(self.current_token(), Some(Token::Comma)) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                let end_span = self.expect(Token::RBracket)?;
                if has_spread {
                    Ok(Expr {
                        kind: ExprKind::ArrayWithSpread { elements },
                        span: start_span.merge(end_span),
                    })
                } else {
                    // Convert back to simple array
                    let simple_elements: Vec<Expr> = elements.into_iter().map(|e| match e {
                        ArrayElement::Single(expr) => expr,
                        ArrayElement::Spread(_) => unreachable!(),
                    }).collect();
                    Ok(Expr {
                        kind: ExprKind::Array { elements: simple_elements },
                        span: start_span.merge(end_span),
                    })
                }
            }
            Token::LBrace => {
                let start_span = span;
                let mut entries = Vec::new();
                let mut has_spread = false;
                if !matches!(self.current_token(), Some(Token::RBrace)) {
                    loop {
                        // Check for spread operator
                        if matches!(self.current_token(), Some(Token::DotDot)) {
                            self.advance(); // consume ".."
                            // DotDot is ".." so we need one more "."
                            if !matches!(self.current_token(), Some(Token::Dot)) {
                                return Err(ParseError::new(
                                    "expected '...' for spread operator",
                                    self.current_span(),
                                ));
                            }
                            self.advance(); // consume the third dot
                            let spread_expr = self.parse_expression()?;
                            entries.push(ObjectEntry::Spread(spread_expr));
                            has_spread = true;
                        } else {
                            let first_key = match self.advance() {
                                Some(SpannedToken { token: Token::Ident(key_name), .. }) => key_name,
                                Some(SpannedToken { token: Token::String(key_name), .. }) => key_name,
                                other => {
                                    let err_span =
                                        other.map(|st| st.span).unwrap_or(self.current_span());
                                    return Err(ParseError::new(
                                        "expected identifier or string for object key",
                                        err_span,
                                    ));
                                }
                            };
                            
                            // Check for nested path like `downlink.subcell_id: value`
                            if matches!(self.current_token(), Some(Token::Dot)) {
                                let mut path = vec![first_key];
                                while matches!(self.current_token(), Some(Token::Dot)) {
                                    self.advance(); // consume the dot
                                    let segment = match self.advance() {
                                        Some(SpannedToken { token: Token::Ident(name), .. }) => name,
                                        other => {
                                            let err_span = other.map(|st| st.span).unwrap_or(self.current_span());
                                            return Err(ParseError::new(
                                                "expected identifier after '.' in object key path",
                                                err_span,
                                            ));
                                        }
                                    };
                                    path.push(segment);
                                }
                                self.expect(Token::Colon)?;
                                let value = self.parse_expression()?;
                                entries.push(ObjectEntry::PathField { path, value });
                                has_spread = true; // PathField requires ObjectWithSpread evaluation
                            } else if matches!(self.current_token(), Some(Token::Colon)) {
                                // Regular field with colon
                                self.expect(Token::Colon)?;
                                let value = self.parse_expression()?;
                                entries.push(ObjectEntry::Field { key: first_key, value });
                            } else {
                                // Shorthand property: { name } means { name: name }
                                entries.push(ObjectEntry::Shorthand { name: first_key });
                                has_spread = true; // Shorthand requires ObjectWithSpread evaluation
                            }
                        }
                        if matches!(self.current_token(), Some(Token::Comma)) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                let end_span = self.expect(Token::RBrace)?;
                if has_spread {
                    Ok(Expr {
                        kind: ExprKind::ObjectWithSpread { entries },
                        span: start_span.merge(end_span),
                    })
                } else {
                    // Convert back to simple object (only contains Field entries at this point)
                    let fields: Vec<(String, Expr)> = entries.into_iter().map(|e| match e {
                        ObjectEntry::Field { key, value } => (key, value),
                        ObjectEntry::Spread(_) | ObjectEntry::PathField { .. } | ObjectEntry::Shorthand { .. } => unreachable!(),
                    }).collect();
                    Ok(Expr {
                        kind: ExprKind::Object { fields },
                        span: start_span.merge(end_span),
                    })
                }
            }
            Token::Dot => {
                let field_span = self.current_span();
                let field = match self.advance() {
                    Some(SpannedToken { token: Token::Ident(field_name), span: field_name_span }) => {
                        (field_name, field_name_span)
                    }
                    other => {
                        return Err(ParseError::new(
                            "expected field name after '.'",
                            other.map(|st| st.span).unwrap_or(field_span),
                        ));
                    }
                };
                let full_span = span.merge(field.1);
                Ok(Expr {
                    kind: ExprKind::FieldAccess {
                        object: Box::new(Expr {
                            kind: ExprKind::Literal(Value::Null),
                            span,
                        }),
                        field: field.0,
                    },
                    span: full_span,
                })
            }
            Token::At => {
                // @ is the pipe context variable, represented as identifier "@"
                Ok(Expr {
                    kind: ExprKind::Identifier(Rc::from("@")),
                    span,
                })
            }
            Token::Match => self.parse_match_expression(span),
            _ => Err(ParseError::new(
                format!("unexpected token: {:?}", token),
                span,
            )
            .with_expected(vec!["expression".to_string()])),
        }
    }

    fn parse_match_expression(&mut self, start_span: Span) -> Result<Expr, ParseError> {
        // Parse the value being matched
        let value = self.parse_expression()?;
        
        self.expect(Token::LBrace)?;
        
        let mut arms = Vec::new();
        
        while !matches!(self.current_token(), Some(Token::RBrace)) && self.current_token().is_some() {
            // Parse the pattern
            let pattern = self.parse_match_pattern()?;
            
            self.expect(Token::Arrow)?;
            
            // Parse the result expression
            let result = self.parse_expression()?;
            
            arms.push((pattern, result));
            
            // Optional comma between arms
            if matches!(self.current_token(), Some(Token::Comma)) {
                self.advance();
            }
        }
        
        let end_span = self.expect(Token::RBrace)?;
        
        Ok(Expr {
            kind: ExprKind::Match {
                value: Box::new(value),
                arms,
            },
            span: start_span.merge(end_span),
        })
    }

    fn parse_match_pattern(&mut self) -> Result<MatchPattern, ParseError> {
        let token = self.current_token().cloned();
        
        match token {
            // Wildcard pattern: _
            Some(Token::Ident(ref name)) if name == "_" => {
                self.advance();
                Ok(MatchPattern::Wildcard)
            }
            // Number literal
            Some(Token::Number(n, is_float)) => {
                self.advance();
                Ok(MatchPattern::Literal(Value::Number(n, is_float)))
            }
            // String literal
            Some(Token::String(s)) => {
                self.advance();
                Ok(MatchPattern::Literal(Value::String(Rc::from(s.as_str()))))
            }
            // Boolean literals
            Some(Token::True) => {
                self.advance();
                Ok(MatchPattern::Literal(Value::Bool(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(MatchPattern::Literal(Value::Bool(false)))
            }
            // Null literal
            Some(Token::Null) => {
                self.advance();
                Ok(MatchPattern::Literal(Value::Null))
            }
            // Negative numbers
            Some(Token::Minus) => {
                self.advance();
                match self.current_token().cloned() {
                    Some(Token::Number(n, is_float)) => {
                        self.advance();
                        Ok(MatchPattern::Literal(Value::Number(-n, is_float)))
                    }
                    _ => Err(ParseError::new(
                        "expected number after '-' in match pattern",
                        self.current_span(),
                    )),
                }
            }
            _ => Err(ParseError::new(
                "expected pattern (literal or _)",
                self.current_span(),
            )
            .with_expected(vec!["pattern".to_string()])),
        }
    }
}
