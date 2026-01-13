use crate::lexer::Value;

pub enum ControlFlow {
    Next,
    Value(Value),
    Return(Value),
    Break,
    Continue,
}

