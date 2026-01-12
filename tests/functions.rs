//! Comprehensive function tests for all built-in functions

use jfm::interpreter::parse_and_run;
use jfm::lexer::Value;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

fn make_test_array() -> Value {
    let arr = vec![
        Value::Number(5.0),
        Value::Number(3.0),
        Value::Number(8.0),
        Value::Number(1.0),
        Value::Number(9.0),
        Value::Number(2.0),
    ];
    let mut root = IndexMap::new();
    root.insert("numbers".to_string(), Value::Array(Rc::new(RefCell::new(arr))));
    Value::Object(Rc::new(RefCell::new(root)))
}

fn make_user_array() -> Value {
    let mut root = IndexMap::new();
    let mut users = Vec::new();
    
    for (name, age, dept) in [
        ("Alice", 25, "Engineering"),
        ("Bob", 30, "Sales"),
        ("Charlie", 25, "Engineering"),
        ("Diana", 35, "Marketing"),
        ("Eve", 30, "Sales"),
    ] {
        let mut user = IndexMap::new();
        user.insert("name".to_string(), Value::String(Rc::from(name)));
        user.insert("age".to_string(), Value::Number(age as f64));
        user.insert("department".to_string(), Value::String(Rc::from(dept)));
        users.push(Value::Object(Rc::new(RefCell::new(user))));
    }
    
    root.insert("users".to_string(), Value::Array(Rc::new(RefCell::new(users))));
    Value::Object(Rc::new(RefCell::new(root)))
}

// =============================================================================
// COUNT FUNCTION TESTS
// =============================================================================

#[test]
fn test_count_basic() {
    let root = make_test_array();
    let result = parse_and_run("count(root.numbers);", root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 6.0);
}

#[test]
fn test_count_empty_array() {
    let source = "count([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 0.0);
}

#[test]
fn test_count_single_element() {
    let source = "count([42]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 1.0);
}

#[test]
fn test_count_with_filter() {
    let root = make_test_array();
    let source = "count(root.numbers | _it > 5);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 2.0); // 8 and 9
}

#[test]
fn test_count_no_args() {
    let source = "count();";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 0.0);
}

// =============================================================================
// SUM FUNCTION TESTS
// =============================================================================

#[test]
fn test_sum_basic() {
    let root = make_test_array();
    let result = parse_and_run("sum(root.numbers);", root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 28.0); // 5+3+8+1+9+2
}

#[test]
fn test_sum_empty_array() {
    let source = "sum([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 0.0);
}

#[test]
fn test_sum_single_element() {
    let source = "sum([42]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 42.0);
}

#[test]
fn test_sum_negative_numbers() {
    let source = "sum([-5, 10, -3, 8]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 10.0);
}

#[test]
fn test_sum_floats() {
    let source = "sum([1.5, 2.5, 3.0]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 7.0);
}

#[test]
fn test_sum_range() {
    let source = "sum(1..10);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 55.0); // 1+2+...+10
}

// =============================================================================
// AVG FUNCTION TESTS
// =============================================================================

#[test]
fn test_avg_basic() {
    let root = make_test_array();
    let result = parse_and_run("avg(root.numbers);", root).unwrap().unwrap();
    let expected = 28.0 / 6.0;
    assert!((result.as_number().unwrap() - expected).abs() < 0.001);
}

#[test]
fn test_avg_empty_array() {
    let source = "avg([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 0.0);
}

#[test]
fn test_avg_single_element() {
    let source = "avg([42]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 42.0);
}

#[test]
fn test_avg_integers() {
    let source = "avg([10, 20, 30]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 20.0);
}

// =============================================================================
// MIN FUNCTION TESTS
// =============================================================================

#[test]
fn test_min_basic() {
    let root = make_test_array();
    let result = parse_and_run("min(root.numbers);", root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 1.0);
}

#[test]
fn test_min_negative() {
    let source = "min([-5, 10, -3, 8]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), -5.0);
}

#[test]
fn test_min_single_element() {
    let source = "min([42]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 42.0);
}

#[test]
fn test_min_all_same() {
    let source = "min([5, 5, 5, 5]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 5.0);
}

// =============================================================================
// MAX FUNCTION TESTS
// =============================================================================

#[test]
fn test_max_basic() {
    let root = make_test_array();
    let result = parse_and_run("max(root.numbers);", root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 9.0);
}

#[test]
fn test_max_negative() {
    let source = "max([-5, -10, -3, -8]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), -3.0);
}

#[test]
fn test_max_single_element() {
    let source = "max([42]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 42.0);
}

#[test]
fn test_max_all_same() {
    let source = "max([5, 5, 5, 5]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 5.0);
}

// =============================================================================
// TAKE FUNCTION TESTS
// =============================================================================

#[test]
fn test_take_basic() {
    let root = make_test_array();
    let result = parse_and_run("take(root.numbers, 3);", root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0].as_number().unwrap(), 5.0);
    assert_eq!(arr[2].as_number().unwrap(), 8.0);
}

#[test]
fn test_take_zero() {
    let source = "take([1, 2, 3], 0);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 0);
}

#[test]
fn test_take_more_than_length() {
    let source = "take([1, 2, 3], 10);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn test_take_one() {
    let source = "take([1, 2, 3], 1);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1);
    assert_eq!(arr[0].as_number().unwrap(), 1.0);
}

// =============================================================================
// UNIQUE FUNCTION TESTS
// =============================================================================

#[test]
fn test_unique_basic() {
    let source = "unique([1, 2, 2, 3, 3, 3]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn test_unique_all_same() {
    let source = "unique([5, 5, 5, 5, 5]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1);
}

#[test]
fn test_unique_all_different() {
    let source = "unique([1, 2, 3, 4, 5]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 5);
}

#[test]
fn test_unique_strings() {
    let source = r#"unique(["a", "b", "a", "c", "b"]);"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn test_unique_empty() {
    let source = "unique([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 0);
}

// =============================================================================
// PUSH FUNCTION TESTS
// =============================================================================

#[test]
fn test_push_basic() {
    let source = "let arr = [1, 2]; push(arr, 3); arr;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[2].as_number().unwrap(), 3.0);
}

#[test]
fn test_push_to_empty() {
    let source = "let arr = []; push(arr, 1); arr;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1);
}

#[test]
fn test_push_multiple() {
    let source = r#"
        let arr = [];
        push(arr, 1);
        push(arr, 2);
        push(arr, 3);
        arr;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn test_push_object() {
    let source = r#"let arr = []; push(arr, {"x": 1}); arr[0].x;"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 1.0);
}

// =============================================================================
// SORT_BY FUNCTION TESTS
// =============================================================================

#[test]
fn test_sort_by_number_field() {
    let root = make_user_array();
    let source = r#"sort_by(root.users, "age");"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr[0].as_object().unwrap().get("age").unwrap().as_number().unwrap(), 25.0);
    assert_eq!(arr[4].as_object().unwrap().get("age").unwrap().as_number().unwrap(), 35.0);
}

#[test]
fn test_sort_by_string_field() {
    let root = make_user_array();
    let source = r#"sort_by(root.users, "name");"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr[0].as_object().unwrap().get("name").unwrap().as_string().unwrap(), "Alice");
    assert_eq!(arr[4].as_object().unwrap().get("name").unwrap().as_string().unwrap(), "Eve");
}

#[test]
fn test_sort_by_preserves_all() {
    let root = make_user_array();
    let source = r#"count(sort_by(root.users, "age"));"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 5.0);
}

// =============================================================================
// GROUP_BY FUNCTION TESTS
// =============================================================================

#[test]
fn test_group_by_basic() {
    let root = make_user_array();
    let source = r#"group_by(root.users, "department");"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.len(), 3); // Engineering, Sales, Marketing
}

#[test]
fn test_group_by_counts() {
    let root = make_user_array();
    let source = r#"
        let groups = group_by(root.users, "department");
        count(groups.Engineering);
    "#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 2.0); // Alice and Charlie
}

#[test]
fn test_group_by_number_field() {
    let root = make_user_array();
    let source = r#"group_by(root.users, "age");"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert!(obj.contains_key("25"));
    assert!(obj.contains_key("30"));
    assert!(obj.contains_key("35"));
}

// =============================================================================
// PRINT FUNCTION TESTS
// =============================================================================

#[test]
fn test_print_returns_null() {
    let source = r#"print("test");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

#[test]
fn test_print_multiple_values() {
    let source = r#"print("a", 1, true, null);"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

#[test]
fn test_print_no_args() {
    let source = "print();";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

#[test]
fn test_print_complex_types() {
    let source = r#"print([1, 2], {"x": 1});"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

// =============================================================================
// COMBINED FUNCTION TESTS
// =============================================================================

#[test]
fn test_sum_of_filtered() {
    let root = make_test_array();
    let source = "sum(root.numbers | _it > 5);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 17.0); // 8 + 9
}

#[test]
fn test_avg_of_sorted() {
    let root = make_test_array();
    let source = "avg(take(root.numbers, 3));";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let expected = (5.0 + 3.0 + 8.0) / 3.0;
    assert!((result.as_number().unwrap() - expected).abs() < 0.001);
}

#[test]
fn test_count_unique() {
    let source = "count(unique([1, 1, 2, 2, 3, 3, 4]));";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 4.0);
}

#[test]
fn test_max_of_mapped() {
    let root = make_user_array();
    let source = "max(root.users | .age);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 35.0);
}

#[test]
fn test_min_of_mapped() {
    let root = make_user_array();
    let source = "min(root.users | .age);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 25.0);
}
