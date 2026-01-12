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

