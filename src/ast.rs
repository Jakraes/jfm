use std::rc::Rc;

use crate::diagnostic::Span;
use crate::value::Value;

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
pub enum ArrayElement {
    Single(Expr),
    Spread(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectEntry {
    Field { key: String, value: Expr },
    PathField { path: Vec<String>, value: Expr },
    Shorthand { name: String },
    Projection { field: String },
    Spread(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Rc<str>,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let { name: Rc<str>, value: Expr },
    Const { name: Rc<str>, value: Expr },
    Expr(Expr),
    For { var: Rc<str>, iterable: Expr, body: Vec<Stmt> },
    While { condition: Expr, body: Vec<Stmt> },
    If { condition: Expr, then_branch: Vec<Stmt>, else_branch: Option<Vec<Stmt>> },
    Return(Option<Expr>),
    Break,
    Continue,
    Function { name: Rc<str>, params: Vec<Param>, body: Vec<Stmt> },
    Block(Vec<Stmt>),
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
    OptionalArrayIndex {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    DeepFieldAccess {
        object: Box<Expr>,
        field: String,
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
    PipeUpdate {
        left: Box<Expr>,
        path: Box<Expr>,
        value: Box<Expr>,
    },
    Call {
        name: Rc<str>,
        args: Vec<Expr>,
    },
    Object {
        fields: Vec<(String, Expr)>,
    },
    ObjectWithSpread {
        entries: Vec<ObjectEntry>,
    },
    Array {
        elements: Vec<Expr>,
    },
    ArrayWithSpread {
        elements: Vec<ArrayElement>,
    },
    Spread(Box<Expr>),
    Lambda {
        params: Vec<Param>,
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
    Match {
        value: Box<Expr>,
        arms: Vec<(MatchPattern, Expr)>,
    },
    Grouped(Box<Expr>),
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
    ModuleCall {
        module: Box<Expr>,
        function: String,
        args: Vec<Expr>,
    },
    AsBinding {
        expr: Box<Expr>,
        name: Rc<str>,
    },
    Replicate {
        count: Box<Expr>,
        template: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchPattern {
    Literal(Value),
    Range { start: Box<Expr>, end: Box<Expr> },
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplatePart {
    Literal(String),
    Interpolation(Box<Expr>),
}
