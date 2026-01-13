use jfm::interpreter::parse_and_run;
use jfm::Value;
use std::rc::Rc;

#[test]
fn test_negative_range() {
    let source = "-3..0;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 4);
    assert_eq!(arr[0], Value::Number(-3.0, false));
    assert_eq!(arr[3], Value::Number(0.0, false));
}

#[test]
fn test_zero_length_range() {
    let source = "5..3;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert!(arr.is_empty());
}

#[test]
fn test_unicode_string_handling() {
    let source = r#"let s = "こんにちは"; s;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::String(Rc::from("こんにちは")));
}

#[test]
fn test_type_equality_across_types() {
    let source = r#"let res = 1 == "1"; res;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_array_with_null_elements() {
    let source = "let arr = [null, 1]; arr[0];";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

#[test]
fn test_optional_chaining_deep_null() {
    let source = "let v = root?.missing?.field; v;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Null);
}

#[test]
fn test_large_number_overflow_to_infinity() {
    let source = "let arr = 0..999; arr.length;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(1000.0, false));
}

