use crate::diagnostic::{Diagnostic, Label, Span};

#[derive(Debug, Clone)]
pub enum InterpreterError {
    UndefinedVariable { name: String, span: Span },
    TypeError { message: String, span: Span },
    IndexOutOfBounds { index: usize, length: usize, span: Span },
    FieldNotFound { field: String, span: Span },
    DivisionByZero { span: Span },
    InvalidOperation { message: String, span: Span },
    PipeChainError { step: usize, step_source: String, inner: Box<InterpreterError>, span: Span },
}

impl InterpreterError {
    pub fn undefined_variable(name: impl Into<String>) -> Self {
        Self::UndefinedVariable { name: name.into(), span: Span::dummy() }
    }

    pub fn type_error(message: impl Into<String>) -> Self {
        Self::TypeError { message: message.into(), span: Span::dummy() }
    }

    pub fn index_out_of_bounds(index: usize, length: usize) -> Self {
        Self::IndexOutOfBounds { index, length, span: Span::dummy() }
    }

    pub fn field_not_found(field: impl Into<String>) -> Self {
        Self::FieldNotFound { field: field.into(), span: Span::dummy() }
    }

    pub fn division_by_zero() -> Self {
        Self::DivisionByZero { span: Span::dummy() }
    }

    pub fn invalid_operation(message: impl Into<String>) -> Self {
        Self::InvalidOperation { message: message.into(), span: Span::dummy() }
    }

    pub fn undefined_variable_at(name: impl Into<String>, span: Span) -> Self {
        Self::UndefinedVariable { name: name.into(), span }
    }

    pub fn type_error_at(message: impl Into<String>, span: Span) -> Self {
        Self::TypeError { message: message.into(), span }
    }

    pub fn index_out_of_bounds_at(index: usize, length: usize, span: Span) -> Self {
        Self::IndexOutOfBounds { index, length, span }
    }

    pub fn field_not_found_at(field: impl Into<String>, span: Span) -> Self {
        Self::FieldNotFound { field: field.into(), span }
    }

    pub fn division_by_zero_at(span: Span) -> Self {
        Self::DivisionByZero { span }
    }

    pub fn invalid_operation_at(message: impl Into<String>, span: Span) -> Self {
        Self::InvalidOperation { message: message.into(), span }
    }

    pub fn pipe_chain_error_at(step: usize, step_source: impl Into<String>, inner: InterpreterError, span: Span) -> Self {
        Self::PipeChainError {
            step,
            step_source: step_source.into(),
            inner: Box::new(inner),
            span,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::UndefinedVariable { span, .. } => *span,
            Self::TypeError { span, .. } => *span,
            Self::IndexOutOfBounds { span, .. } => *span,
            Self::FieldNotFound { span, .. } => *span,
            Self::DivisionByZero { span } => *span,
            Self::InvalidOperation { span, .. } => *span,
            Self::PipeChainError { span, .. } => *span,
        }
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Self::UndefinedVariable { name, span } => {
                Diagnostic::error(format!("cannot find value `{}` in this scope", name))
                    .with_code("E0201")
                    .with_label(Label::primary(*span, format!("not found in this scope")))
                    .with_help(format!("the variable `{}` is not defined in the current scope", name))
            }
            Self::TypeError { message, span } => {
                Diagnostic::error(format!("{}", message))
                    .with_code("E0202")
                    .with_label(Label::primary(*span, format!("{}", message)))
            }
            Self::IndexOutOfBounds { index, length, span } => {
                Diagnostic::error(format!("index out of bounds: the length is {} but the index is {}", length, index))
                    .with_code("E0203")
                    .with_label(Label::primary(*span, format!("index {} out of range [0..{})", index, length)))
            }
            Self::FieldNotFound { field, span } => {
                Diagnostic::error(format!("no field named `{}` found", field))
                    .with_code("E0204")
                    .with_label(Label::primary(*span, format!("unknown field `{}`", field)))
            }
            Self::DivisionByZero { span } => {
                Diagnostic::error("division by zero")
                    .with_code("E0205")
                    .with_label(Label::primary(*span, "attempt to divide by zero"))
            }
            Self::InvalidOperation { message, span } => {
                Diagnostic::error(format!("{}", message))
                    .with_code("E0206")
                    .with_label(Label::primary(*span, format!("{}", message)))
            }
            Self::PipeChainError { step, step_source, inner, span } => {
                let mut diag = Diagnostic::error(format!("pipe chain failed at step {}", step))
                    .with_code("E0207")
                    .with_label(Label::primary(*span, format!("error occurred at step {}", step)));
                
                let inner_diag = inner.to_diagnostic();
                if let Some(first_label) = inner_diag.labels.first() {
                    if !first_label.span.is_dummy() {
                        diag = diag.with_label(Label::secondary(first_label.span, "original error occurred here"));
                    }
                }
                
                diag = diag.with_note(format!("failing expression: {}", step_source));
                if !inner_diag.message.is_empty() {
                    diag = diag.with_note(format!("caused by: {}", inner_diag.message));
                }
                diag
            }
        }
    }
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::UndefinedVariable { name, .. } => {
                write!(f, "Undefined variable: {}", name)
            }
            InterpreterError::TypeError { message, .. } => write!(f, "Type error: {}", message),
            InterpreterError::IndexOutOfBounds { index, length, .. } => {
                write!(f, "Index out of bounds: {} (length: {})", index, length)
            }
            InterpreterError::FieldNotFound { field, .. } => {
                write!(f, "Field not found: {}", field)
            }
            InterpreterError::DivisionByZero { .. } => write!(f, "Division by zero"),
            InterpreterError::InvalidOperation { message, .. } => write!(f, "Invalid operation: {}", message),
            InterpreterError::PipeChainError { step, step_source, inner, .. } => {
                write!(f, "Pipe chain failed at step {} (in `{}`): {}", step, step_source, inner)
            }
        }
    }
}

impl std::error::Error for InterpreterError {}
