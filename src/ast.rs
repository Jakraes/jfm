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

/// Pipe operator type for JFM v2
#[derive(Debug, Clone, PartialEq)]
pub enum PipeOp {
    /// |? - Filter: keep items matching predicate
    Filter,
    /// |> - Map: transform each item
    Map,
    /// |= - Mutate: modify each item in place
    Mutate,
    /// |& - Aggregate: reduce to single value
    Aggregate,
    /// |# - Tap: debug/side-effect, passes data through
    Tap,
    /// Legacy | - infers operation from expression
    Legacy,
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
    /// A nested path field like `downlink.subcell_id: value`
    PathField { path: Vec<String>, value: Expr },
    /// Shorthand property like `{ name }` meaning `{ name: name }`
    Shorthand { name: String },
    /// Projection shorthand like `{ .field }` extracting from current context
    ProjectionField { path: Vec<String> },
    /// Projection shorthand with rename like `{ newName: .field }`
    ProjectionRename { new_name: String, path: Vec<String> },
    Spread(Expr),
}

/// Function parameter with optional default value
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam {
    pub name: Rc<str>,
    pub default: Option<Expr>,
}

/// Destructuring pattern for lambdas and pipes
#[derive(Debug, Clone, PartialEq)]
pub enum DestructurePattern {
    /// Simple identifier binding
    Ident(Rc<str>),
    /// Object destructuring { a, b, c: renamed }
    Object(Vec<DestructureField>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DestructureField {
    /// The field name to extract
    pub field: String,
    /// Optional rename (for { field: renamed } syntax)
    pub rename: Option<Rc<str>>,
}

/// Pattern for pattern matching in filters
#[derive(Debug, Clone, PartialEq)]
pub enum FilterPattern {
    /// Match object shape { type: "user", active: true }
    Object(Vec<PatternField>),
    /// Negated pattern !{ ... }
    Negated(Box<FilterPattern>),
}

/// A field in a pattern match
#[derive(Debug, Clone, PartialEq)]
pub struct PatternField {
    pub key: String,
    pub matcher: PatternMatcher,
}

/// How to match a field value
#[derive(Debug, Clone, PartialEq)]
pub enum PatternMatcher {
    /// Exact value match
    Exact(Value),
    /// Comparison operators like > 100
    Comparison(BinaryOp, Value),
    /// Alternative values like "click" | "tap"
    Alternatives(Vec<Value>),
    /// Negated value like !"deleted"
    Negated(Box<PatternMatcher>),
    /// Any value (field must exist) - for partial patterns with ...
    Any,
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
    Function { name: Rc<str>, params: Vec<FunctionParam>, body: Vec<Stmt> },
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
    /// Deep field extraction: obj..field (gets all 'field' at any depth)
    DeepAccess {
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
    /// Legacy pipe operator (infers filter/map from expression)
    Pipe {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// New explicit pipe operators |?, |>, |=, |&, |#
    PipeExpr {
        left: Box<Expr>,
        op: PipeOp,
        right: Box<Expr>,
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
        params: Vec<Rc<str>>,
        body: Box<Expr>,
    },
    /// Lambda with destructuring pattern
    DestructuringLambda {
        pattern: DestructurePattern,
        body: Box<Expr>,
    },
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
    },
    /// Slice range in pipes: [0..5], [-1..], etc.
    SliceRange {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
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
    /// Method call like `arr.sum()` or `obj.method(args)` 
    /// Stored as (object, method_name, args)
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
    /// Colon method call like `arr:where()` or `arr:select()`
    ColonMethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
    /// Module access like `math::add(1, 2)` for calling module functions
    ModuleCall {
        module: Box<Expr>,
        function: String,
        args: Vec<Expr>,
    },
    /// As binding like `@ as name` - binds expression result to name
    AsBinding {
        expr: Box<Expr>,
        name: Rc<str>,
    },
    /// Replication like `3 * { id: @ }` - creates array with @ as index
    Replicate {
        count: Box<Expr>,
        template: Box<Expr>,
    },
    /// Pattern matching in filter: |? { type: "user", active: true }
    PatternMatch {
        pattern: FilterPattern,
    },
    /// Aggregation expression for |& : { count: :length, avg: :avg(.age) }
    AggregateObject {
        fields: Vec<(String, AggregateExpr)>,
    },
}

/// Aggregate expression for use in |& { field: :aggregate(.path) }
#[derive(Debug, Clone, PartialEq)]
pub enum AggregateExpr {
    /// :length, :count, etc. on the array itself
    Method(String),
    /// :avg(.field), :sum(.field), etc. with a field path
    MethodWithPath(String, Vec<String>),
    /// :collect(.field) - gather all values of a field
    Collect(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchPattern {
    Literal(Value),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplatePart {
    Literal(String),
    Interpolation(Box<Expr>),
}
