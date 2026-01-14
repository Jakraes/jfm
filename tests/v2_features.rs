//! Tests for JFM v2 syntax features

use jfm::interpreter::evaluator::parse_and_run;
use jfm::value::Value;
use std::rc::Rc;

// =============================================================================
// NEW PIPE OPERATORS
// =============================================================================

#[test]
fn test_pipe_filter_operator() {
    let source = r#"
        let users = [
            { name: "Bob", age: 30 },
            { name: "Alice", age: 25 },
            { name: "Charlie", age: 35 }
        ];
        users |? .age > 28;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
}

#[test]
fn test_pipe_map_operator() {
    let source = r#"
        let numbers = [1, 2, 3];
        numbers |> @ * 2;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0], Value::Number(2.0, false));
    assert_eq!(arr[1], Value::Number(4.0, false));
    assert_eq!(arr[2], Value::Number(6.0, false));
}

#[test]
fn test_pipe_mutate_operator() {
    let source = r#"
        let users = [{ name: "Bob", age: 30 }];
        users |= { senior: .age > 28 };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    let obj = arr[0].as_object().unwrap();
    assert_eq!(obj.get("senior"), Some(&Value::Bool(true)));
    assert_eq!(obj.get("name"), Some(&Value::String(Rc::from("Bob"))));
}

#[test]
fn test_pipe_tap_operator() {
    let source = r#"
        let numbers = [1, 2, 3];
        numbers |# null |> @ * 2;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0], Value::Number(2.0, false));
}

#[test]
fn test_pipe_operator_chain() {
    let source = r#"
        let users = [
            { name: "Bob", age: 30, active: true },
            { name: "Alice", age: 25, active: false },
            { name: "Charlie", age: 35, active: true }
        ];
        users |? .active == true |> { name: .name, years: .age };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
}

// =============================================================================
// COLON METHOD CHAINING
// =============================================================================

#[test]
fn test_colon_where_method() {
    let source = r#"
        let users = [
            { name: "Bob", age: 30 },
            { name: "Alice", age: 25 }
        ];
        users:where(.age > 25);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1);
}

#[test]
fn test_colon_select_method() {
    let source = r#"
        let users = [
            { name: "Bob", age: 30 },
            { name: "Alice", age: 25 }
        ];
        users:select(.name);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
    assert_eq!(arr[0], Value::String(Rc::from("Bob")));
}

#[test]
fn test_colon_method_chain() {
    let source = r#"
        let users = [
            { name: "Bob", age: 30 },
            { name: "Alice", age: 25 },
            { name: "Charlie", age: 35 }
        ];
        users:where(.age > 25):select(.name);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
}

// =============================================================================
// DEEP EXTRACTION
// =============================================================================

#[test]
fn test_deep_extraction_simple() {
    // Use field access to make it clearly object-like
    let source = r#"
        let data = { outer: { inner: { value: 42 } } };
        data.outer..value;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(42.0, false));
}

#[test]
fn test_deep_extraction_multiple() {
    let source = r#"
        let data = { a: { value: 1 }, b: { value: 2 }, c: { value: 3 } };
        (data)..value;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn test_deep_extraction_nested() {
    let source = r#"
        let data = {
            outer: {
                inner: { value: 42 },
                value: 10
            },
            value: 5
        };
        (data)..value;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

// =============================================================================
// DEFAULT PARAMETERS
// =============================================================================

#[test]
fn test_default_parameter_used() {
    let source = r#"
        fn greet(name, greeting = "Hello") {
            return `${greeting}, ${name}!`;
        }
        greet("World");
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::String(Rc::from("Hello, World!")));
}

#[test]
fn test_default_parameter_overridden() {
    let source = r#"
        fn greet(name, greeting = "Hello") {
            return `${greeting}, ${name}!`;
        }
        greet("World", "Hi");
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::String(Rc::from("Hi, World!")));
}

#[test]
fn test_multiple_default_parameters() {
    let source = r#"
        fn combine(a, b = 10, c = 20) {
            return a + b + c;
        }
        combine(1);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(31.0, false));
}

#[test]
fn test_partial_default_override() {
    let source = r#"
        fn combine(a, b = 10, c = 20) {
            return a + b + c;
        }
        combine(1, 5);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(26.0, false));
}

// =============================================================================
// RANGE EXPRESSIONS (ensure they still work with new ..)
// =============================================================================

#[test]
fn test_range_still_works() {
    let source = "0..5;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 6);
}

#[test]
fn test_range_with_variables() {
    let source = r#"
        let start = 1;
        let end = 5;
        start..end;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 5);
}

// =============================================================================
// LEGACY PIPE COMPATIBILITY
// =============================================================================

#[test]
fn test_legacy_pipe_filter() {
    let source = r#"
        let users = [
            { name: "Bob", age: 30 },
            { name: "Alice", age: 25 }
        ];
        users | .age > 25;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1);
}

#[test]
fn test_legacy_pipe_map() {
    let source = r#"
        let users = [
            { name: "Bob", age: 30 },
            { name: "Alice", age: 25 }
        ];
        users | { user: .name };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
}

// =============================================================================
// PROJECTION SHORTHAND
// =============================================================================

#[test]
fn test_projection_shorthand() {
    let source = r#"
        let user = { name: "Bob", age: 30, city: "NYC" };
        user | { .name, .age };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("name"), Some(&Value::String(Rc::from("Bob"))));
    assert_eq!(obj.get("age"), Some(&Value::Number(30.0, false)));
    assert_eq!(obj.get("city"), None);
}

#[test]
fn test_projection_rename() {
    let source = r#"
        let user = { name: "Bob", age: 30 };
        user | { userName: .name };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("userName"), Some(&Value::String(Rc::from("Bob"))));
}
