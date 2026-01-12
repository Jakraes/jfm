use jfm::lexer::Value;
use jfm::interpreter::parse_and_run;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

fn make_simple_data() -> Value {
    let mut obj = IndexMap::new();
    obj.insert("x".to_string(), Value::Number(10.0));
    obj.insert("y".to_string(), Value::Number(20.0));
    obj.insert("name".to_string(), Value::String(Rc::from("test")));
    Value::Object(Rc::new(RefCell::new(obj)))
}

#[test]
fn test_simple_variable_assignment() {
    let source = "let x = 5;";
    let root = Value::Null;
    let result = parse_and_run(source, root);
    assert!(result.is_ok(), "Should parse and execute without error");
}

#[test]
fn test_number_arithmetic() {
    let source = "let result = 10 + 5 * 2; result;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Number(n) = result {
        assert_eq!(n, 20.0, "Should compute 10 + (5 * 2) = 20");
    } else {
        panic!("Expected number result");
    }
}

#[test]
fn test_field_access() {
    let source = "let val = root.x; val;";
    let root = make_simple_data();
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Number(n) = result {
        assert_eq!(n, 10.0);
    } else {
        panic!("Expected number from field access");
    }
}

#[test]
fn test_chained_field_access() {
    let mut inner = IndexMap::new();
    inner.insert("value".to_string(), Value::Number(42.0));
    
    let mut outer = IndexMap::new();
    outer.insert("nested".to_string(), Value::Object(Rc::new(RefCell::new(inner))));
    
    let root = Value::Object(Rc::new(RefCell::new(outer)));
    
    let source = "let val = root.nested.value; val;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Number(n) = result {
        assert_eq!(n, 42.0);
    } else {
        panic!("Expected number from nested field access");
    }
}

#[test]
fn test_array_indexing() {
    let arr = vec![Value::Number(10.0), Value::Number(20.0), Value::Number(30.0)];
    let mut obj = IndexMap::new();
    obj.insert("numbers".to_string(), Value::Array(Rc::new(RefCell::new(arr))));
    let root = Value::Object(Rc::new(RefCell::new(obj)));
    
    let source = "let first = root.numbers[0]; first;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Number(n) = result {
        assert_eq!(n, 10.0);
    } else {
        panic!("Expected number from array index");
    }
}

#[test]
fn test_comparison_operators() {
    let source = "let result = 10 > 5; result;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Bool(b) = result {
        assert_eq!(b, true);
    } else {
        panic!("Expected boolean result");
    }
}

#[test]
fn test_equality_operators() {
    let source = "let result = 10 == 10; result;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Bool(b) = result {
        assert_eq!(b, true);
    } else {
        panic!("Expected boolean result");
    }
}

#[test]
fn test_logical_and() {
    let source = "let result = true && false; result;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Bool(b) = result {
        assert_eq!(b, false);
    } else {
        panic!("Expected boolean result");
    }
}

#[test]
fn test_logical_or() {
    let source = "let result = true || false; result;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Bool(b) = result {
        assert_eq!(b, true);
    } else {
        panic!("Expected boolean result");
    }
}

#[test]
fn test_unary_not() {
    let source = "let result = !false; result;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Bool(b) = result {
        assert_eq!(b, true);
    } else {
        panic!("Expected boolean result");
    }
}

#[test]
fn test_string_literals() {
    let source = r#"let name = "Alice"; name;"#;
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "Alice");
    } else {
        panic!("Expected string result");
    }
}

#[test]
fn test_null_literal() {
    let source = "let value = null; value;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert!(matches!(result, Value::Null), "Expected null value");
}

#[test]
fn test_zero_and_negative_numbers() {
    let source = "let result = -5 + 0; result;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result, Value::Number(-5.0));
}

#[test]
fn test_empty_array_length() {
    let source = "let arr = []; arr.length;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result, Value::Number(0.0));
}

#[test]
fn test_empty_object_literal() {
    let source = "let obj = {}; obj;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Object(obj) = result {
        assert_eq!(obj.borrow().len(), 0);
    } else {
        panic!("Expected empty object");
    }
}

#[test]
fn test_string_concat_with_empty() {
    let source = r#"let s = "" + "abc"; s;"#;
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result, Value::String(Rc::from("abc")));
}