//! Input/output built-in functions.

use crate::value::Value;
use super::super::error::InterpreterError;
use std::rc::Rc;

pub fn builtin_print(
    args: &[Value],
    value_to_string: impl Fn(&Value) -> String,
) -> Result<Value, InterpreterError> {
    use std::io::Write;
    if args.is_empty() {
        println!();
    } else {
        let output: Vec<String> = args.iter().map(&value_to_string).collect();
        println!("{}", output.join(" "));
    }
    std::io::stdout().flush().ok();
    Ok(Value::Null)
}

pub fn builtin_input(args: &[Value]) -> Result<Value, InterpreterError> {
    use std::io::{self, Write};
    if let Some(Value::String(prompt)) = args.first() {
        print!("{}", prompt);
        io::stdout().flush().ok();
    }
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .map_err(|e| InterpreterError::invalid_operation(format!("Failed to read input: {}", e)))?;
    Ok(Value::String(Rc::from(input.trim_end_matches(['\n', '\r']))))
}
