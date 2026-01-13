use jfm::lexer::Value;
use jfm::interpreter::parse_and_run;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

// ============================================================================
// Basic Template Literal Tests
// ============================================================================

#[test]
fn test_simple_template_literal() {
    let source = "`hello world`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "hello world");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_empty_template_literal() {
    let source = "``;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "");
    } else {
        panic!("Expected empty string");
    }
}

#[test]
fn test_template_with_single_interpolation() {
    let source = "let name = \"Alice\"; `Hello ${name}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "Hello Alice");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_interpolation_at_start() {
    let source = "let x = 42; `${x} is the answer`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "42 is the answer");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_interpolation_at_end() {
    let source = "let x = 42; `The answer is ${x}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "The answer is 42");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_multiple_interpolations() {
    let source = "let a = 1; let b = 2; `${a} and ${b}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "1 and 2");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_adjacent_interpolations() {
    let source = "let a = \"Hello\"; let b = \"World\"; `${a}${b}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "HelloWorld");
    } else {
        panic!("Expected string");
    }
}

// ============================================================================
// Expression Interpolation Tests
// ============================================================================

#[test]
fn test_template_with_arithmetic() {
    let source = "let a = 2; let b = 3; `${a} + ${b} = ${a + b}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "2 + 3 = 5");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_function_call() {
    let source = "let arr = [1, 2, 3]; `Length: ${arr.length}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "Length: 3");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_ternary() {
    let source = "let x = 10; `x is ${x > 5 ? \"big\" : \"small\"}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "x is big");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_field_access() {
    let mut obj = IndexMap::new();
    obj.insert("name".to_string(), Value::String(Rc::from("Bob")));
    obj.insert("age".to_string(), Value::Number(30.0, false));
    let root = Value::Object(Rc::new(RefCell::new(obj)));
    
    let source = "`${root.name} is ${root.age} years old`;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "Bob is 30 years old");
    } else {
        panic!("Expected string");
    }
}

// ============================================================================
// Escape Sequence Tests
// ============================================================================

#[test]
fn test_template_with_literal_dollar() {
    let source = "`$100`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "$100");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_escaped_backtick() {
    let source = r#"`hello \` world`;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "hello ` world");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_escaped_dollar() {
    let source = r#"`\${not interpolated}`;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "${not interpolated}");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_newline() {
    let source = r#"`line1\nline2`;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "line1\nline2");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_tab() {
    let source = r#"`col1\tcol2`;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "col1\tcol2");
    } else {
        panic!("Expected string");
    }
}

// ============================================================================
// Complex Expression Tests
// ============================================================================

#[test]
fn test_template_with_nested_braces_in_expr() {
    let source = "let obj = {a: 1}; `value: ${obj.a}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "value: 1");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_string_in_expr() {
    let source = r#"`greeting: ${"Hello"}`;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "greeting: Hello");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_in_variable() {
    let source = "let name = \"World\"; let greeting = `Hello ${name}!`; greeting;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "Hello World!");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_boolean_interpolation() {
    let source = "let flag = true; `The flag is ${flag}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "The flag is true");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_null_interpolation() {
    let source = "let x = null; `Value: ${x}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "Value: null");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_with_array_interpolation() {
    let source = "let arr = [1, 2, 3]; `Array: ${arr}`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        // Array should be converted to string representation
        assert!(s.contains("1") && s.contains("2") && s.contains("3"));
    } else {
        panic!("Expected string");
    }
}

// ============================================================================
// Integration Tests
// ============================================================================

#[test]
fn test_template_with_null_coalesce() {
    let source = "let name = null; `Hello ${name ?? \"Guest\"}!`;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "Hello Guest!");
    } else {
        panic!("Expected string");
    }
}

#[test]
fn test_template_in_array() {
    let source = "let x = 1; let y = 2; [`Value: ${x}`, `Value: ${y}`];";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Array(arr) = result {
        let borrowed = arr.borrow();
        assert_eq!(borrowed.len(), 2);
        if let Value::String(s) = &borrowed[0] {
            assert_eq!(s.as_ref(), "Value: 1");
        }
        if let Value::String(s) = &borrowed[1] {
            assert_eq!(s.as_ref(), "Value: 2");
        }
    } else {
        panic!("Expected array");
    }
}
