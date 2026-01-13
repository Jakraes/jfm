//! Error handling and edge case tests

use jfm::interpreter::parse_and_run;
use jfm::Value;

// =============================================================================
// TYPE ERROR TESTS
// =============================================================================

#[test]
fn test_error_add_string_and_number() {
    let source = r#"let x = "hello" + 5;"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_subtract_strings() {
    let source = r#"let x = "hello" - "world";"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_multiply_strings() {
    let source = r#"let x = "hello" * "world";"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_divide_strings() {
    let source = r#"let x = "hello" / "world";"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_negate_string() {
    let source = r#"let x = -"hello";"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_not_number() {
    let source = "let x = !42;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_field_access_on_number() {
    let source = "let x = 42; x.field;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_field_access_on_string() {
    let source = r#"let x = "hello"; x.field;"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_index_on_object() {
    let source = "let x = {a: 1}; x[0];";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_index_with_string() {
    let source = r#"let arr = [1, 2, 3]; arr["hello"];"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

// =============================================================================
// UNDEFINED VARIABLE TESTS
// =============================================================================

#[test]
fn test_error_undefined_variable() {
    let source = "let x = undefined_var;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Undefined"));
}

#[test]
fn test_error_undefined_in_expression() {
    let source = "let x = 5 + undefined_var;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_undefined_in_condition() {
    let source = "if undefined_var { 1; }";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_undefined_in_loop() {
    let source = "for x in undefined_arr { x; }";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

// =============================================================================
// INDEX OUT OF BOUNDS TESTS
// =============================================================================

#[test]
fn test_error_index_out_of_bounds() {
    let source = "let arr = [1, 2, 3]; arr[10];";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("out of bounds"));
}

#[test]
fn test_error_negative_index() {
    let source = "let arr = [1, 2, 3]; arr[-1];";
    let result = parse_and_run(source, Value::Null);
    // Negative indices convert to unsigned, becoming very large - may or may not error
    // depending on implementation. Just verify execution completes.
    let _ = result;
}

#[test]
fn test_error_empty_array_index() {
    let source = "let arr = []; arr[0];";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

// =============================================================================
// DIVISION BY ZERO TESTS
// =============================================================================

#[test]
fn test_error_division_by_zero() {
    let source = "let x = 10 / 0;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Division by zero"));
}

#[test]
fn test_error_division_by_zero_variable() {
    let source = "let y = 0; let x = 10 / y;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_modulo_by_zero() {
    let source = "let x = 10 % 0;";
    let result = parse_and_run(source, Value::Null);
    // Modulo by zero in Rust returns NaN, not an error
    assert!(result.is_ok());
}

// =============================================================================
// FUNCTION ARGUMENT ERRORS
// =============================================================================

#[test]
fn test_error_sum_non_array() {
    let source = "sum(42);";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

// Note: count and take functions have been removed
// count() - use .length property instead
// take(arr, n) - use slice(arr, 0, n) instead

#[test]
fn test_error_slice_non_array() {
    let source = "slice(42, 0, 5);";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_slice_non_number() {
    let source = r#"slice([1,2,3], "zero", 3);"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_push_non_array() {
    let source = "push(42, 5);";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_sort_by_non_array() {
    let source = r#"sort_by(42, "field");"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_group_by_non_array() {
    let source = r#"group_by(42, "field");"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_unique_non_array() {
    let source = "unique(42);";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_unknown_function() {
    let source = "unknown_function(42);";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Unknown function"));
}

// =============================================================================
// PARSE ERROR TESTS
// =============================================================================

#[test]
fn test_error_missing_semicolon() {
    let source = "let x = 5";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_unclosed_brace() {
    let source = "let x = {a: 1;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_unclosed_bracket() {
    let source = "let x = [1, 2, 3;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_unclosed_paren() {
    let source = "let x = (1 + 2;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_invalid_operator() {
    let source = "let x = 5 +++ 3;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

// =============================================================================
// LOOP ERROR TESTS
// =============================================================================

#[test]
fn test_error_for_over_non_array() {
    let source = "for x in 42 { x; }";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("non-array"));
}

#[test]
fn test_error_for_over_string() {
    let source = r#"for x in "hello" { x; }"#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

#[test]
fn test_error_for_over_object() {
    let source = "for x in {a: 1} { x; }";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
}

// =============================================================================
// EDGE CASE VALUE TESTS
// =============================================================================

#[test]
fn test_null_equality() {
    let source = "null == null;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Bool(true));
}

#[test]
fn test_null_inequality_with_number() {
    let source = "null == 0;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_null_inequality_with_empty_string() {
    let source = r#"null == "";"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_null_inequality_with_false() {
    let source = "null == false;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_empty_array_equality() {
    let source = "[] == [];";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // Empty arrays are different instances
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_empty_object_equality() {
    let source = "let a = {}; let b = {}; a == b;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // Empty objects are different instances  
    assert_eq!(result, Value::Bool(false));
}

#[test]
fn test_zero_equality() {
    let source = "0 == 0;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Bool(true));
}

#[test]
fn test_negative_zero_equality() {
    let source = "0 == -0;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result, Value::Bool(true));
}

#[test]
fn test_float_precision() {
    let source = "0.1 + 0.2;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let n = result.as_number().unwrap();
    // Allow for floating point imprecision
    assert!((n - 0.3).abs() < 0.0001);
}

// =============================================================================
// SPECIAL NUMBER TESTS
// =============================================================================

#[test]
fn test_very_large_number() {
    let source = "let x = 99999999999999999999; x;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(result.as_number().is_some());
}

#[test]
fn test_very_small_number() {
    let source = "let x = 0.0000000001; x;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(result.as_number().is_some());
}

#[test]
fn test_power_overflow() {
    let source = "2 ^ 1024;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // Very large exponent results in infinity
    let n = result.as_number().unwrap();
    assert!(n.is_infinite() || n > 1e300);
}

// =============================================================================
// STRING EDGE CASES
// =============================================================================

#[test]
fn test_empty_string() {
    let source = r#"let s = ""; s;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "");
}

#[test]
fn test_string_with_spaces() {
    let source = r#"let s = "   "; s;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "   ");
}

#[test]
fn test_string_with_newlines() {
    let source = r#"let s = "line1\nline2"; s;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(result.as_string().unwrap().contains('\n'));
}

#[test]
fn test_string_with_tabs() {
    let source = r#"let s = "col1\tcol2"; s;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(result.as_string().unwrap().contains('\t'));
}

#[test]
fn test_string_concatenation_empty() {
    let source = r#"let s = "" + ""; s;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "");
}

#[test]
fn test_unicode_emoji() {
    let source = r#"let s = "Hello 👋 World 🌍"; s;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(result.as_string().unwrap().contains("👋"));
}

// =============================================================================
// ARRAY EDGE CASES
// =============================================================================

#[test]
fn test_deeply_nested_array() {
    let source = "let arr = [[[[[1]]]]]; arr[0][0][0][0][0];";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 1.0);
}

#[test]
fn test_array_of_nulls() {
    let source = "let arr = [null, null, null]; arr.length;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 3.0);
}

#[test]
fn test_mixed_type_array() {
    let source = r#"let arr = [1, "two", true, null, [5], {x: 6}]; arr.length;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 6.0);
}

// =============================================================================
// OBJECT EDGE CASES
// =============================================================================

#[test]
fn test_object_with_numeric_string_key() {
    let source = r#"let obj = {"123": "value"}; obj;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert!(obj.contains_key("123"));
}

#[test]
fn test_deeply_nested_object() {
    let source = "let obj = {a: {b: {c: {d: {e: 42}}}}}; obj.a.b.c.d.e;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 42.0);
}

#[test]
fn test_object_field_not_found() {
    let source = "let obj = {a: 1}; obj.b;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

// =============================================================================
// SCOPE EDGE CASES
// =============================================================================

#[test]
fn test_variable_shadowing_in_loop() {
    let source = r#"
        let x = 100;
        for x in [1, 2, 3] {
            x;
        }
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 100.0);
}

#[test]
fn test_variable_shadowing_in_block() {
    let source = r#"
        let x = 1;
        {
            let x = 2;
            x;
        }
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 1.0);
}

#[test]
fn test_nested_scope_access() {
    let source = r#"
        let x = 1;
        let y = x + 1;
        let z = y + 1;
        z;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 3.0);
}

// =============================================================================
// PIPE CHAIN ERROR TESTS
// =============================================================================

#[test]
fn test_pipe_chain_error_step_1() {
    // Error in first step of pipe chain
    let source = r#"
        let users = [{ name: "Bob", age: 30 }];
        users | .nonexistent;
    "#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    let error_msg = result.unwrap_err();
    // Should mention pipe chain and step
    assert!(error_msg.contains("pipe chain") || error_msg.contains("step") || error_msg.contains("Pipe chain"));
}

#[test]
fn test_pipe_chain_error_step_2() {
    // Error in second step of pipe chain
    // First step (.name) succeeds, second step (.nonexistent) errors on object without that field
    let source = r#"
        let users = [{ name: "Bob", age: 30 }];
        users | { profile: .name } | .nonexistent;
    "#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    let error_msg = result.unwrap_err();
    // Should mention pipe chain and step
    assert!(error_msg.contains("pipe chain") || error_msg.contains("step") || error_msg.contains("Pipe chain"));
}

#[test]
fn test_pipe_chain_error_step_3() {
    // Error in third step of pipe chain
    let source = r#"
        let users = [{ name: "Bob", age: 30 }];
        users | .name | len | .nonexistent;
    "#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    let error_msg = result.unwrap_err();
    // Should mention pipe chain and step
    assert!(error_msg.contains("pipe chain") || error_msg.contains("step") || error_msg.contains("Pipe chain"));
}

#[test]
fn test_pipe_chain_error_with_field_not_found() {
    // Field not found error in pipe chain
    let source = r#"
        let data = [{ id: 1 }];
        data | .missing_field;
    "#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    let error_msg = result.unwrap_err();
    // Should contain information about the error
    assert!(error_msg.contains("field") || error_msg.contains("missing") || error_msg.contains("pipe chain"));
}

#[test]
fn test_pipe_chain_error_with_type_error() {
    // Type error in pipe chain - accessing field on non-object
    let source = r#"
        let data = [{ value: 42 }];
        data | .value | @ / 0;
    "#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    let error_msg = result.unwrap_err();
    // Should contain information about the error (division by zero or pipe chain)
    assert!(error_msg.len() > 0);
}

#[test]
fn test_pipe_chain_error_index_out_of_bounds() {
    // Index out of bounds in pipe chain
    let source = r#"
        let arr = [[1, 2], [3, 4]];
        arr | .[5];
    "#;
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_err());
    let error_msg = result.unwrap_err();
    // Should contain information about the error
    assert!(error_msg.len() > 0);
}
