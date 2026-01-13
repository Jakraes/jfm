use crate::value::Value;

pub enum ControlFlow {
    Next,
    Value(Value),
    Return(Value),
    Break,
    Continue,
}

