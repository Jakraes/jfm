use chumsky::prelude::*;
use indexmap::IndexMap;
use std::rc::Rc;
use std::cell::{RefCell, Ref};
use crate::diagnostic::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,
    Filter,
    Map,
    For,
    In,
    If,
    Else,
    Return,
    While,
    Break,
    Continue,
    Fn,

    Ident(String),
    Number(f64, bool),
    String(String),
    TemplateFull(String),
    True,
    False,
    Null,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Eq,
    NotEq,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    And,
    Or,
    Pipe,
    Bang,
    Assign,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PipeEq,
    DotDot,
    Spread,
    Arrow,
    Comma,
    Colon,
    QuestionDot,
    Question,
    NullCoalesce,

    Dot,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Number(f64, bool),
    String(Rc<str>),
    Array(Rc<RefCell<Vec<Value>>>),
    Object(Rc<RefCell<IndexMap<String, Value>>>),
    Function(Rc<Function>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(left_bool), Value::Bool(right_bool)) => left_bool == right_bool,
            (Value::Number(left_num, _), Value::Number(right_num, _)) => left_num == right_num,
            (Value::String(left_str), Value::String(right_str)) => left_str == right_str,
            (Value::Array(left_arr), Value::Array(right_arr)) => left_arr == right_arr,
            (Value::Object(left_obj), Value::Object(right_obj)) => left_obj == right_obj,
            (Value::Function(left_fn), Value::Function(right_fn)) => Rc::ptr_eq(left_fn, right_fn),
            _ => false,
        }
    }
}

impl Value {
    pub fn as_object(&self) -> Option<Ref<'_, IndexMap<String, Value>>> {
        if let Value::Object(obj) = self {
            Some(obj.borrow())
        } else {
            None
        }
    }

    pub fn as_array(&self) -> Option<Ref<'_, Vec<Value>>> {
        if let Value::Array(arr) = self {
            Some(arr.borrow())
        } else {
            None
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        if let Value::Number(numeric_value, _) = self {
            Some(*numeric_value)
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        if let Value::String(string_ref) = self {
            Some(string_ref.as_ref())
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let Value::Bool(bool_value) = self {
            Some(*bool_value)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    NotEq,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    And,
    Or,
    Pipe,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let { name: Rc<str>, value: Expr },
    Expr(Expr),
    For { var: Rc<str>, iterable: Expr, body: Vec<Stmt> },
    While { condition: Expr, body: Vec<Stmt> },
    If { condition: Expr, then_branch: Vec<Stmt>, else_branch: Option<Vec<Stmt>> },
    Return(Option<Expr>),
    Break,
    Continue,
    Function { name: Rc<str>, params: Vec<Rc<str>>, body: Vec<Stmt> },
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<Rc<str>>,
    pub body_expr: Option<Box<Expr>>,
    pub body_stmts: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Value),
    Identifier(Rc<str>),
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    OptionalFieldAccess {
        object: Box<Expr>,
        field: String,
    },
    ArrayIndex {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Pipe {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Call {
        name: Rc<str>,
        args: Vec<Expr>,
    },
    Object {
        fields: Vec<(String, Expr)>,
        spreads: Vec<(usize, Expr)>,
    },
    Array {
        elements: Vec<Expr>,
        spreads: Vec<(usize, Expr)>,
    },
    Lambda {
        params: Vec<Rc<str>>,
        body: Box<Expr>,
    },
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
    },
    Assignment {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    CompoundAssignment {
        target: Box<Expr>,
        op: BinaryOp,
        value: Box<Expr>,
    },
    Ternary {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    NullCoalesce {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    TemplateLiteral {
        parts: Vec<TemplatePart>,
    },
    Grouped(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplatePart {
    Literal(String),
    Interpolation(Box<Expr>),
}

pub fn lexer<'a>()
-> impl Parser<'a, &'a str, Vec<(Token, SimpleSpan)>, extra::Err<Simple<'a, char>>> {
    let number = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .map(|s: &str| Token::Number(s.parse().unwrap(), s.contains('.')));

    let escape = just('\\').ignore_then(choice((
        just('\\'),
        just('/'),
        just('"'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
    )));

    let string = just('"')
        .ignore_then(none_of("\\\"").or(escape).repeated().collect::<String>())
        .then_ignore(just('"'))
        .map(Token::String);

    let template_escape = just('\\').ignore_then(choice((
        just('\\'),
        just('`'),
        just('$'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
    )));

    let interpolation_expression = recursive(|nested| {
        choice((
            none_of("{}\"'`").map(|c: char| c.to_string()),
            just('{').ignore_then(nested.clone()).then_ignore(just('}')).map(|s| format!("{{{}}}", s)),
            just('"').ignore_then(
                choice((
                    just("\\\"").to('"'),
                    just("\\\\").to('\\'),
                    just("\\n").to('\n'),
                    just("\\t").to('\t'),
                    none_of("\\\""),
                )).repeated().collect::<String>()
            ).then_ignore(just('"')).map(|s| format!("\"{}\"", s)),
            just('\'').ignore_then(none_of("'").repeated().collect::<String>()).then_ignore(just('\'')).map(|s| format!("'{}'", s)),
        ))
        .repeated()
        .collect::<Vec<String>>()
        .map(|v| v.join(""))
    });

    #[derive(Clone)]
    enum TemplateChunk {
        Char(char),
        Interpolation(String),
    }

    let template_literal = just('`').ignore_then(
        choice((
            just("${").ignore_then(interpolation_expression).then_ignore(just('}')).map(TemplateChunk::Interpolation),
            template_escape.map(TemplateChunk::Char),
            none_of("\\`$").map(TemplateChunk::Char),
            just('$').then_ignore(none_of("{").rewind()).map(|_| TemplateChunk::Char('$')),
            just('$').then_ignore(just('`').rewind()).map(|_| TemplateChunk::Char('$')),
        ))
        .repeated()
        .collect::<Vec<TemplateChunk>>()
    ).then_ignore(just('`'))
    .map(|chunks: Vec<TemplateChunk>| {
        // Build encoded string from chunks
        let mut parts: Vec<(bool, String)> = Vec::new(); // (is_expr, content)
        let mut current_text = String::new();
        
        for chunk in chunks {
            match chunk {
                TemplateChunk::Char(character) => current_text.push(character),
                TemplateChunk::Interpolation(expression) => {
                    if !current_text.is_empty() {
                        parts.push((false, std::mem::take(&mut current_text)));
                    }
                    parts.push((true, expression));
                }
            }
        }
        if !current_text.is_empty() {
            parts.push((false, current_text));
        }
        
        let has_interpolations = parts.iter().any(|(is_interpolation, _)| *is_interpolation);
        
        if !has_interpolations {
            let text = parts.into_iter().map(|(_, content)| content).collect::<String>();
            Token::TemplateFull(text)
        } else {
            let mut encoded = String::from("TPL:");
            for (index, (is_interpolation, content)) in parts.iter().enumerate() {
                if index > 0 {
                    encoded.push('\x00');
                }
                if *is_interpolation {
                    encoded.push_str("EXPR:");
                } else {
                    encoded.push_str("TEXT:");
                }
                encoded.push_str(content);
            }
            Token::TemplateFull(encoded)
        }
    });

    let ident = text::ident().map(|s: &str| match s {
        "let" => Token::Let,
        "filter" => Token::Filter,
        "map" => Token::Map,
        "for" => Token::For,
        "in" => Token::In,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "while" => Token::While,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "fn" => Token::Fn,
        "true" => Token::True,
        "false" => Token::False,
        "null" => Token::Null,
        _ => Token::Ident(s.to_string()),
    });

    let op_binary = choice((
        just("==").to(Token::Eq),
        just("!=").to(Token::NotEq),
        just(">=").to(Token::GreaterEq),
        just("<=").to(Token::LessEq),
        just("&&").to(Token::And),
        just("||").to(Token::Or),
        just("+=").to(Token::PlusEq),
        just("-=").to(Token::MinusEq),
        just("*=").to(Token::StarEq),
        just("/=").to(Token::SlashEq),
        just("|=").to(Token::PipeEq),
        just("...").to(Token::Spread),
        just("..").to(Token::DotDot),
        just("=>").to(Token::Arrow),
        just("?.").to(Token::QuestionDot),
        just("??").to(Token::NullCoalesce),
        just('?').to(Token::Question),
    ));

    let op_single = choice((
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Star),
        just('/').to(Token::Slash),
        just('%').to(Token::Percent),
        just('^').to(Token::Caret),
        just('>').to(Token::Greater),
        just('<').to(Token::Less),
        just('|').to(Token::Pipe),
        just('!').to(Token::Bang),
        just('=').to(Token::Assign),
        just('.').to(Token::Dot),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just('[').to(Token::LBracket),
        just(']').to(Token::RBracket),
    ));

    let op = op_binary.or(op_single);

    let token = number
        .or(string)
        .or(template_literal)
        .or(ident)
        .or(op)
        .map_with(|tok, e| (tok, e.span()))
        .padded();

    token.repeated().collect().then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::Parser;

    fn lex(source: &str) -> Vec<Token> {
        lexer()
            .parse(source)
            .output()
            .expect("Lexer failed")
            .iter()
            .map(|(tok, _)| tok.clone())
            .collect()
    }

    #[test]
    fn test_keywords() {
        assert_eq!(lex("let"), vec![Token::Let]);
        assert_eq!(lex("true"), vec![Token::True]);
        assert_eq!(lex("false"), vec![Token::False]);
        assert_eq!(lex("null"), vec![Token::Null]);
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(lex("foo"), vec![Token::Ident("foo".to_string())]);
        assert_eq!(lex("bar123"), vec![Token::Ident("bar123".to_string())]);
        assert_eq!(lex("_test"), vec![Token::Ident("_test".to_string())]);
        assert_eq!(
            lex("camelCase"),
            vec![Token::Ident("camelCase".to_string())]
        );
    }

    #[test]
    fn test_numbers() {
        assert_eq!(lex("42"), vec![Token::Number(42.0, false)]);
        assert_eq!(lex("0"), vec![Token::Number(0.0, false)]);
        assert_eq!(lex("3.14"), vec![Token::Number(3.14, true)]);
        assert_eq!(lex("0.5"), vec![Token::Number(0.5, true)]);
        assert_eq!(lex("123.456"), vec![Token::Number(123.456, true)]);
    }

    #[test]
    fn test_strings() {
        assert_eq!(lex(r#""hello""#), vec![Token::String("hello".to_string())]);
        assert_eq!(lex(r#""world""#), vec![Token::String("world".to_string())]);
        assert_eq!(lex(r#""""#), vec![Token::String("".to_string())]);
    }

    #[test]
    fn test_string_escapes() {
        assert_eq!(
            lex(r#""hello\nworld""#),
            vec![Token::String("hello\nworld".to_string())]
        );
        assert_eq!(
            lex(r#""tab\there""#),
            vec![Token::String("tab\there".to_string())]
        );
        assert_eq!(
            lex(r#""quote\"here""#),
            vec![Token::String("quote\"here".to_string())]
        );
        assert_eq!(
            lex(r#""backslash\\here""#),
            vec![Token::String("backslash\\here".to_string())]
        );
        assert_eq!(
            lex(r#""slash\/here""#),
            vec![Token::String("slash/here".to_string())]
        );
        assert_eq!(
            lex(r#""return\rhere""#),
            vec![Token::String("return\rhere".to_string())]
        );
    }

    #[test]
    fn test_arithmetic_operators() {
        assert_eq!(lex("+"), vec![Token::Plus]);
        assert_eq!(lex("-"), vec![Token::Minus]);
        assert_eq!(lex("*"), vec![Token::Star]);
        assert_eq!(lex("/"), vec![Token::Slash]);
        assert_eq!(lex("%"), vec![Token::Percent]);
        assert_eq!(lex("^"), vec![Token::Caret]);
    }

    #[test]
    fn test_comparison_operators() {
        assert_eq!(lex("=="), vec![Token::Eq]);
        assert_eq!(lex("!="), vec![Token::NotEq]);
        assert_eq!(lex(">"), vec![Token::Greater]);
        assert_eq!(lex("<"), vec![Token::Less]);
        assert_eq!(lex(">="), vec![Token::GreaterEq]);
        assert_eq!(lex("<="), vec![Token::LessEq]);
    }

    #[test]
    fn test_logical_operators() {
        assert_eq!(lex("&&"), vec![Token::And]);
        assert_eq!(lex("||"), vec![Token::Or]);
        assert_eq!(lex("!"), vec![Token::Bang]);
    }

    #[test]
    fn test_other_operators() {
        assert_eq!(lex("|"), vec![Token::Pipe]);
        assert_eq!(lex("="), vec![Token::Assign]);
    }

    #[test]
    fn test_delimiters() {
        assert_eq!(lex("."), vec![Token::Dot]);
        assert_eq!(lex(";"), vec![Token::Semicolon]);
        assert_eq!(lex("("), vec![Token::LParen]);
        assert_eq!(lex(")"), vec![Token::RParen]);
        assert_eq!(lex("{"), vec![Token::LBrace]);
        assert_eq!(lex("}"), vec![Token::RBrace]);
        assert_eq!(lex("["), vec![Token::LBracket]);
        assert_eq!(lex("]"), vec![Token::RBracket]);
    }

    #[test]
    fn test_whitespace_handling() {
        assert_eq!(
            lex("let   users"),
            vec![Token::Let, Token::Ident("users".to_string())]
        );
        assert_eq!(
            lex("  let\n\tusers  "),
            vec![Token::Let, Token::Ident("users".to_string())]
        );
    }

    #[test]
    fn test_simple_assignment() {
        assert_eq!(
            lex("let x = 5;"),
            vec![
                Token::Let,
                Token::Ident("x".to_string()),
                Token::Assign,
                Token::Number(5.0, false),
                Token::Semicolon
            ]
        );
    }

    #[test]
    fn test_field_access() {
        assert_eq!(
            lex("root.users"),
            vec![
                Token::Ident("root".to_string()),
                Token::Dot,
                Token::Ident("users".to_string())
            ]
        );
        assert_eq!(
            lex("obj.field.nested"),
            vec![
                Token::Ident("obj".to_string()),
                Token::Dot,
                Token::Ident("field".to_string()),
                Token::Dot,
                Token::Ident("nested".to_string())
            ]
        );
    }

    #[test]
    fn test_pipe_expression() {
        assert_eq!(
            lex("users | .name"),
            vec![
                Token::Ident("users".to_string()),
                Token::Pipe,
                Token::Dot,
                Token::Ident("name".to_string())
            ]
        );
    }

    #[test]
    fn test_comparison_expression() {
        assert_eq!(
            lex(".name == \"Bob\""),
            vec![
                Token::Dot,
                Token::Ident("name".to_string()),
                Token::Eq,
                Token::String("Bob".to_string())
            ]
        );
    }

    #[test]
    fn test_complex_query() {
        let tokens = lex(r#"let users = root.users;"#);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Ident("users".to_string()),
                Token::Assign,
                Token::Ident("root".to_string()),
                Token::Dot,
                Token::Ident("users".to_string()),
                Token::Semicolon
            ]
        );
    }

    #[test]
    fn test_full_query_example() {
        let source = r#"let users = root.users;
let test_name = users | .name == "Bob";"#;
        let tokens = lex(source);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Ident("users".to_string()),
                Token::Assign,
                Token::Ident("root".to_string()),
                Token::Dot,
                Token::Ident("users".to_string()),
                Token::Semicolon,
                Token::Let,
                Token::Ident("test_name".to_string()),
                Token::Assign,
                Token::Ident("users".to_string()),
                Token::Pipe,
                Token::Dot,
                Token::Ident("name".to_string()),
                Token::Eq,
                Token::String("Bob".to_string()),
                Token::Semicolon
            ]
        );
    }

    #[test]
    fn test_arithmetic_expression() {
        assert_eq!(
            lex("a + b * c - d / e"),
            vec![
                Token::Ident("a".to_string()),
                Token::Plus,
                Token::Ident("b".to_string()),
                Token::Star,
                Token::Ident("c".to_string()),
                Token::Minus,
                Token::Ident("d".to_string()),
                Token::Slash,
                Token::Ident("e".to_string())
            ]
        );
    }

    #[test]
    fn test_logical_expression() {
        assert_eq!(
            lex("a && b || !c"),
            vec![
                Token::Ident("a".to_string()),
                Token::And,
                Token::Ident("b".to_string()),
                Token::Or,
                Token::Bang,
                Token::Ident("c".to_string())
            ]
        );
    }

    #[test]
    fn test_parenthesized_expression() {
        assert_eq!(
            lex("(a + b) * c"),
            vec![
                Token::LParen,
                Token::Ident("a".to_string()),
                Token::Plus,
                Token::Ident("b".to_string()),
                Token::RParen,
                Token::Star,
                Token::Ident("c".to_string())
            ]
        );
    }

    #[test]
    fn test_array_access() {
        assert_eq!(
            lex("arr[0]"),
            vec![
                Token::Ident("arr".to_string()),
                Token::LBracket,
                Token::Number(0.0, false),
                Token::RBracket
            ]
        );
    }

    #[test]
    fn test_multiple_statements() {
        let source = r#"
            let x = 10;
            let y = 20;
            let z = x + y;
        "#;
        let tokens = lex(source);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Ident("x".to_string()),
                Token::Assign,
                Token::Number(10.0, false),
                Token::Semicolon,
                Token::Let,
                Token::Ident("y".to_string()),
                Token::Assign,
                Token::Number(20.0, false),
                Token::Semicolon,
                Token::Let,
                Token::Ident("z".to_string()),
                Token::Assign,
                Token::Ident("x".to_string()),
                Token::Plus,
                Token::Ident("y".to_string()),
                Token::Semicolon
            ]
        );
    }

    #[test]
    fn test_complex_filter_query() {
        let source = r#"let filtered = data | .age > 18 && .status == "active";"#;
        let tokens = lex(source);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Ident("filtered".to_string()),
                Token::Assign,
                Token::Ident("data".to_string()),
                Token::Pipe,
                Token::Dot,
                Token::Ident("age".to_string()),
                Token::Greater,
                Token::Number(18.0, false),
                Token::And,
                Token::Dot,
                Token::Ident("status".to_string()),
                Token::Eq,
                Token::String("active".to_string()),
                Token::Semicolon
            ]
        );
    }

    #[test]
    fn test_nested_field_access() {
        assert_eq!(
            lex("root.user.profile.name"),
            vec![
                Token::Ident("root".to_string()),
                Token::Dot,
                Token::Ident("user".to_string()),
                Token::Dot,
                Token::Ident("profile".to_string()),
                Token::Dot,
                Token::Ident("name".to_string())
            ]
        );
    }

    #[test]
    fn test_boolean_literals() {
        assert_eq!(
            lex("true && false || true"),
            vec![
                Token::True,
                Token::And,
                Token::False,
                Token::Or,
                Token::True
            ]
        );
    }

    #[test]
    fn test_null_literal() {
        assert_eq!(
            lex("let x = null;"),
            vec![
                Token::Let,
                Token::Ident("x".to_string()),
                Token::Assign,
                Token::Null,
                Token::Semicolon
            ]
        );
    }

    #[test]
    fn test_modulo_and_power() {
        assert_eq!(
            lex("a % b ^ c"),
            vec![
                Token::Ident("a".to_string()),
                Token::Percent,
                Token::Ident("b".to_string()),
                Token::Caret,
                Token::Ident("c".to_string())
            ]
        );
    }

    #[test]
    fn test_all_comparison_operators() {
        assert_eq!(
            lex("a == b != c > d < e >= f <= g"),
            vec![
                Token::Ident("a".to_string()),
                Token::Eq,
                Token::Ident("b".to_string()),
                Token::NotEq,
                Token::Ident("c".to_string()),
                Token::Greater,
                Token::Ident("d".to_string()),
                Token::Less,
                Token::Ident("e".to_string()),
                Token::GreaterEq,
                Token::Ident("f".to_string()),
                Token::LessEq,
                Token::Ident("g".to_string())
            ]
        );
    }
}
