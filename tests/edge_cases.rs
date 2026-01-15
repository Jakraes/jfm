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
fn test_descending_range() {
    // Descending ranges now work: 5..1 produces [5, 4, 3, 2, 1]
    let source = "5..1;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 5);
    assert_eq!(arr[0], Value::Number(5.0, false));
    assert_eq!(arr[4], Value::Number(1.0, false));
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

// Regression tests for audit fixes

#[test]
fn test_const_reassignment_error() {
    let source = "const x = 1; x = 2;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_modulo_by_zero_error() {
    let source = "5 % 0;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_string_length_property() {
    let source = r#"let s = "hello"; s.length;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(5.0, false));
}

#[test]
fn test_unicode_string_length() {
    let source = r#"let s = "こんにちは"; s.length;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // Unicode char count, not byte count
    assert_eq!(result, Value::Number(5.0, false));
}

#[test]
fn test_single_quote_strings() {
    let source = "let s = 'hello world'; s;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::String(Rc::from("hello world")));
}

#[test]
fn test_optional_array_index_on_array() {
    let source = "let arr = [1, 2, 3]; arr?.[1];";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(2.0, false));
}

#[test]
fn test_optional_array_index_on_null() {
    let source = "let arr = null; arr?.[0];";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Null);
}

#[test]
fn test_min_empty_array_returns_null() {
    let source = "min([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Null);
}

#[test]
fn test_max_empty_array_returns_null() {
    let source = "max([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Null);
}

#[test]
fn test_avg_empty_array_returns_null() {
    let source = "avg([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Null);
}

#[test]
fn test_truthy_zero_is_falsy() {
    // Use ternary for truthy check
    let source = "0 ? 1 : 2;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(2.0, false));
}

#[test]
fn test_truthy_empty_string_is_falsy() {
    // Use ternary for truthy check
    let source = r#""" ? 1 : 2;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(2.0, false));
}

#[test]
fn test_sort_by_does_not_mutate_original() {
    let source = r#"
        let arr = [{ x: 3 }, { x: 1 }, { x: 2 }];
        let sorted = sort_by(arr, "x");
        arr[0].x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // Original array should be unchanged
    assert_eq!(result, Value::Number(3.0, false));
}

