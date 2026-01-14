#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
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
    Match,
    As,

    // Literals and Identifiers
    Ident(String),
    Number(f64, bool),
    String(String),
    TemplateFull(String),
    True,
    False,
    Null,
    At,

    // Arithmetic Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,

    // Comparison Operators
    Eq,
    NotEq,
    Greater,
    Less,
    GreaterEq,
    LessEq,

    // Logical Operators
    And,
    Or,
    Bang,

    // Special Operators
    Pipe,
    Assign,
    DotDot,
    Arrow,
    Comma,
    Colon,
    QuestionDot,
    Question,
    NullCoalesce,

    // Delimiters
    Dot,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
}
