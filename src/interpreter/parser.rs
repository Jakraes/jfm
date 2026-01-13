use crate::diagnostic::{Diagnostic, Label, Span};
use crate::lexer::{BinaryOp, Expr, ExprKind, Stmt, Token, UnaryOp, Value};
use chumsky::Parser as _;
use std::rc::Rc;

/// A token with its source span
#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

/// Parse error with location information
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

/// Result of parsing with potential errors
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

    /// Create a TokenParser from the lexer output
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

    /// Synchronize parser state after an error (skip to recovery point)
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
                    // Limit errors to prevent cascade
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

    /// Parse and return full result with all errors
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

    /// Get collected errors
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
            _ => {
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
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
            Some(SpannedToken { token: Token::Ident(n), .. }) => Rc::from(n.as_str()),
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
            Some(SpannedToken { token: Token::Ident(n), .. }) => Rc::from(n.as_str()),
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
                    Some(SpannedToken { token: Token::Ident(n), .. }) => Rc::from(n.as_str()),
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
            Some(SpannedToken { token: Token::Ident(n), .. }) => n,
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
                let span = start_span.merge(right.span);
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

    fn parse_lambda(&mut self) -> Result<Expr, ParseError> {
        let saved_current = self.current;
        let start_span = self.current_span();

        let params = if matches!(self.current_token(), Some(Token::LParen)) {
            self.advance();
            let mut parsed_params = Vec::new();
            if !matches!(self.current_token(), Some(Token::RParen)) {
                loop {
                    let param = match self.advance() {
                        Some(SpannedToken { token: Token::Ident(n), .. }) => Rc::from(n.as_str()),
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
                Some(SpannedToken { token: Token::Ident(n), .. }) => Rc::from(n.as_str()),
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

        self.advance(); // consume =>
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
            let right = self.parse_ternary()?;
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
        let mut left = self.parse_logical_or()?;

        while matches!(self.current_token(), Some(Token::NullCoalesce)) {
            self.advance();
            let right = self.parse_logical_or()?;
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

    fn parse_logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_logical_and()?;

        while matches!(self.current_token(), Some(Token::Or)) {
            self.advance();
            let right = self.parse_logical_and()?;
            let span = left.span.merge(right.span);
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

    fn parse_logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison()?;

        while matches!(self.current_token(), Some(Token::And)) {
            self.advance();
            let right = self.parse_comparison()?;
            let span = left.span.merge(right.span);
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

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
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
            let span = left.span.merge(right.span);
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

    fn parse_range(&mut self) -> Result<Expr, ParseError> {
        let left = self.parse_additive()?;

        if matches!(self.current_token(), Some(Token::DotDot)) {
            self.advance();
            let right = self.parse_additive()?;
            let span = left.span.merge(right.span);
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

    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplicative()?;

        while let Some(op) = self.current_token() {
            let binary_op = match op {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            let span = left.span.merge(right.span);
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

    fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
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
            let span = left.span.merge(right.span);
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

    fn parse_power(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;

        while matches!(self.current_token(), Some(Token::Caret)) {
            self.advance();
            let right = self.parse_unary()?;
            let span = left.span.merge(right.span);
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
                        Some(SpannedToken { token: Token::Ident(f), .. }) => f,
                        other => {
                            return Err(ParseError::new(
                                "expected field name after '.'",
                                other.map(|st| st.span).unwrap_or(field_span),
                            )
                            .with_expected(vec!["identifier".to_string()]));
                        }
                    };
                    let span = expr.span.merge(self.previous_span());
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
                    let field_span = self.current_span();
                    let field = match self.advance() {
                        Some(SpannedToken { token: Token::Ident(f), .. }) => f,
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
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_template_literal(&mut self, encoded: String, span: Span) -> Result<Expr, ParseError> {
        use crate::lexer::TemplatePart;
        
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
            Token::Number(n, is_float) => Ok(Expr {
                kind: ExprKind::Literal(Value::Number(n, is_float)),
                span,
            }),
            Token::String(s) => Ok(Expr {
                kind: ExprKind::Literal(Value::String(Rc::from(s.as_str()))),
                span,
            }),
            Token::TemplateFull(s) => {
                self.parse_template_literal(s, span)
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
                let end_span = self.expect(Token::RBracket)?;
                Ok(Expr {
                    kind: ExprKind::Array { elements },
                    span: start_span.merge(end_span),
                })
            }
            Token::LBrace => {
                let start_span = span;
                let mut fields = Vec::new();
                if !matches!(self.current_token(), Some(Token::RBrace)) {
                    loop {
                        let key = match self.advance() {
                            Some(SpannedToken { token: Token::Ident(k), .. }) => k,
                            Some(SpannedToken { token: Token::String(k), .. }) => k,
                            other => {
                                let err_span =
                                    other.map(|st| st.span).unwrap_or(self.current_span());
                                return Err(ParseError::new(
                                    "expected identifier or string for object key",
                                    err_span,
                                ));
                            }
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
                let end_span = self.expect(Token::RBrace)?;
                Ok(Expr {
                    kind: ExprKind::Object { fields },
                    span: start_span.merge(end_span),
                })
            }
            Token::Dot => {
                let field_span = self.current_span();
                let field = match self.advance() {
                    Some(SpannedToken { token: Token::Ident(f), span: f_span }) => {
                        (f, f_span)
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
            _ => Err(ParseError::new(
                format!("unexpected token: {:?}", token),
                span,
            )
            .with_expected(vec!["expression".to_string()])),
        }
    }
}
