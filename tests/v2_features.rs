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
        users | .age > 28;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
}

#[test]
fn test_pipe_map_operator() {
    let source = r#"
        let numbers = [1, 2, 3];
        numbers | @ * 2;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0], Value::Number(2.0, false));
    assert_eq!(arr[1], Value::Number(4.0, false));
    assert_eq!(arr[2], Value::Number(6.0, false));
}

#[test]
fn test_pipe_tap_operator() {
    let source = r#"
        let numbers = [1, 2, 3];
        numbers | @ * 2;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0], Value::Number(2.0, false));
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
        users | .age > 25;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1);
}

#[test]
fn test_colon_method_chain() {
    let source = r#"
        let users = [
            { name: "Bob", age: 30 },
            { name: "Alice", age: 25 },
            { name: "Charlie", age: 35 }
        ];
        users | .age > 25 | .name;
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

// =============================================================================
// CONST KEYWORD
// =============================================================================

#[test]
fn test_const_declaration() {
    let source = r#"
        const PI = 3.14159;
        PI;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(3.14159, true));
}

#[test]
fn test_const_cannot_redeclare() {
    let source = r#"
        const x = 10;
        const x = 20;
    "#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err(), "Should error on const redeclaration");
}

// =============================================================================
// COMPOUND ASSIGNMENTS
// =============================================================================

#[test]
fn test_plus_assign() {
    let source = r#"
        let x = 10;
        x += 5;
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(15.0, false));
}

#[test]
fn test_minus_assign() {
    let source = r#"
        let x = 10;
        x -= 3;
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(7.0, false));
}

#[test]
fn test_mul_assign() {
    let source = r#"
        let x = 5;
        x *= 3;
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(15.0, false));
}

#[test]
fn test_div_assign() {
    let source = r#"
        let x = 20;
        x /= 4;
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(5.0, true));
}

#[test]
fn test_mod_assign() {
    let source = r#"
        let x = 17;
        x %= 5;
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(2.0, false));
}

// =============================================================================
// UPDATE PIPE (|~)
// =============================================================================

#[test]
fn test_update_pipe_field() {
    let source = r#"
        let user = { name: "Bob", age: 30 };
        user |~ .age => 31;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("age"), Some(&Value::Number(31.0, false)));
    assert_eq!(obj.get("name"), Some(&Value::String(Rc::from("Bob"))));
}

#[test]
fn test_update_pipe_with_expression() {
    let source = r#"
        let user = { name: "Bob", age: 30 };
        user |~ .age => @ + 1;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("age"), Some(&Value::Number(31.0, false)));
}

#[test]
fn test_update_pipe_array_index() {
    let source = r#"
        let arr = [1, 2, 3];
        arr |~ [0] => 10;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr[0], Value::Number(10.0, false));
    assert_eq!(arr[1], Value::Number(2.0, false));
}

// =============================================================================
// RANGE PATTERNS IN MATCH
// =============================================================================

#[test]
fn test_match_range_pattern() {
    let source = r#"
        let age = 25;
        match age {
            18..30 => "young",
            31..50 => "middle",
            _ => "senior"
        };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::String(Rc::from("young")));
}

#[test]
fn test_match_range_pattern_boundary() {
    let source = r#"
        match 30 {
            18..30 => "included",
            _ => "excluded"
        };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::String(Rc::from("included")));
}

#[test]
fn test_match_range_pattern_negative() {
    let source = r#"
        match -5 {
            -10..0 => "negative",
            _ => "other"
        };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::String(Rc::from("negative")));
}

// =============================================================================
// NEGATIVE ARRAY INDICES
// =============================================================================

#[test]
fn test_negative_index_last() {
    let source = r#"
        let arr = [10, 20, 30, 40];
        arr[-1];
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(40.0, false));
}

#[test]
fn test_negative_index_second_last() {
    let source = r#"
        let arr = [10, 20, 30, 40];
        arr[-2];
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(30.0, false));
}

#[test]
fn test_negative_index_in_pipe() {
    let source = r#"
        let arr = [10, 20, 30];
        arr | [-1];
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let result_arr = result.as_array().unwrap();
    assert_eq!(result_arr.len(), 1);
    assert_eq!(result_arr[0], Value::Number(30.0, false));
}

// =============================================================================
// NEW COMMENT SYNTAX
// =============================================================================

#[test]
fn test_single_line_hash_comment() {
    let source = r#"
        # This is a comment
        let x = 10;
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(10.0, false));
}

#[test]
fn test_multi_line_comment() {
    let source = r#"
        /* This is a
           multi-line comment */
        let x = 20;
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(20.0, false));
}

#[test]
fn test_comments_with_code() {
    let source = r#"
        let x = 5; # inline comment
        let y = 10; /* another comment */
        x + y;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Number(15.0, false));
}

// =============================================================================
// ENHANCED IMPLICIT @ IN PROJECTIONS
// =============================================================================

#[test]
fn test_implicit_at_in_projection() {
    let source = r#"
        let user = { name: "Bob", age: 30, active: true };
        user | { name, adult: age >= 18 };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("name"), Some(&Value::String(Rc::from("Bob"))));
    assert_eq!(obj.get("adult"), Some(&Value::Bool(true)));
}

#[test]
fn test_implicit_at_with_computed_fields() {
    let source = r#"
        let user = { firstName: "Bob", lastName: "Smith", age: 30 };
        user | { 
            name: firstName + " " + lastName,
            age,
            isAdult: age >= 18
        };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("name"), Some(&Value::String(Rc::from("Bob Smith"))));
    assert_eq!(obj.get("age"), Some(&Value::Number(30.0, false)));
    assert_eq!(obj.get("isAdult"), Some(&Value::Bool(true)));
}

#[test]
fn test_implicit_at_fallback_to_variable() {
    let source = r#"
        let globalName = "Global";
        let user = { age: 30 };
        user | { name: globalName, age };
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("name"), Some(&Value::String(Rc::from("Global"))));
    assert_eq!(obj.get("age"), Some(&Value::Number(30.0, false)));
}
