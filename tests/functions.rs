//! Comprehensive function tests for all built-in functions

use jfm::interpreter::parse_and_run;
use jfm::Value;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

fn make_test_array() -> Value {
    let arr = vec![
        Value::Number(5.0, false),
        Value::Number(3.0, false),
        Value::Number(8.0, false),
        Value::Number(1.0, false),
        Value::Number(9.0, false),
        Value::Number(2.0, false),
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
        user.insert("age".to_string(), Value::Number(age as f64, false));
        user.insert("department".to_string(), Value::String(Rc::from(dept)));
        users.push(Value::Object(Rc::new(RefCell::new(user))));
    }
    
    root.insert("users".to_string(), Value::Array(Rc::new(RefCell::new(users))));
    Value::Object(Rc::new(RefCell::new(root)))
}

// =============================================================================
// ARRAY LENGTH TESTS (replaces count function)
// =============================================================================

#[test]
fn test_length_basic() {
    let root = make_test_array();
    let result = parse_and_run("root.numbers.length;", root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 6.0);
}

#[test]
fn test_length_empty_array() {
    let source = "[].length;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 0.0);
}

#[test]
fn test_length_single_element() {
    let source = "[42].length;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 1.0);
}

#[test]
fn test_length_with_filter() {
    let root = make_test_array();
    let source = "(root.numbers | @ > 5).length;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 2.0); // 8 and 9
}

#[test]
fn test_length_inline_array() {
    let source = "[1, 2, 3, 4, 5].length;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 5.0);
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
// SLICE FUNCTION TESTS (replaces take function - use slice(arr, 0, n))
// =============================================================================

#[test]
fn test_slice_take_basic() {
    let root = make_test_array();
    let result = parse_and_run("slice(root.numbers, 0, 3);", root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0].as_number().unwrap(), 5.0);
    assert_eq!(arr[2].as_number().unwrap(), 8.0);
}

#[test]
fn test_slice_take_zero() {
    let source = "slice([1, 2, 3], 0, 0);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 0);
}

#[test]
fn test_slice_take_more_than_length() {
    let source = "slice([1, 2, 3], 0, 10);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn test_slice_take_one() {
    let source = "slice([1, 2, 3], 0, 1);";
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
    let source = r#"sort_by(root.users, "age").length;"#;
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
        groups.Engineering.length;
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

// Note: print() tests are in io.rs

// =============================================================================
// COMBINED FUNCTION TESTS
// =============================================================================

#[test]
fn test_sum_of_filtered() {
    let root = make_test_array();
    let source = "sum(root.numbers | @ > 5);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 17.0); // 8 + 9
}

#[test]
fn test_avg_of_sliced() {
    let root = make_test_array();
    let source = "avg(slice(root.numbers, 0, 3));";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let expected = (5.0 + 3.0 + 8.0) / 3.0;
    assert!((result.as_number().unwrap() - expected).abs() < 0.001);
}

#[test]
fn test_unique_length() {
    let source = "unique([1, 1, 2, 2, 3, 3, 4]).length;";
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

// =============================================================================
// REVERSE FUNCTION TESTS
// =============================================================================

#[test]
fn test_reverse_basic() {
    let source = "reverse([1, 2, 3]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0].as_number().unwrap(), 3.0);
    assert_eq!(arr[2].as_number().unwrap(), 1.0);
}

#[test]
fn test_reverse_empty() {
    let source = "reverse([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 0);
}

#[test]
fn test_reverse_single() {
    let source = "reverse([42]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1);
    assert_eq!(arr[0].as_number().unwrap(), 42.0);
}

// =============================================================================
// SORT FUNCTION TESTS
// =============================================================================

#[test]
fn test_sort_numbers() {
    let source = "sort([3, 1, 4, 1, 5]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 5);
    assert_eq!(arr[0].as_number().unwrap(), 1.0);
    assert_eq!(arr[4].as_number().unwrap(), 5.0);
}

#[test]
fn test_sort_strings() {
    let source = r#"sort(["zebra", "apple", "banana"]);"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr[0].as_string().unwrap(), "apple");
    assert_eq!(arr[2].as_string().unwrap(), "zebra");
}

// =============================================================================
// SLICE FUNCTION TESTS
// =============================================================================

#[test]
fn test_slice_basic() {
    let source = "slice([1, 2, 3, 4, 5], 1, 4);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0].as_number().unwrap(), 2.0);
    assert_eq!(arr[2].as_number().unwrap(), 4.0);
}

#[test]
fn test_slice_no_end() {
    let source = "slice([1, 2, 3, 4, 5], 2);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn test_slice_negative() {
    let source = "slice([1, 2, 3, 4, 5], -2);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
}

// =============================================================================
// POP FUNCTION TESTS
// =============================================================================

#[test]
fn test_pop_basic() {
    let source = "let arr = [1, 2, 3]; pop(arr);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 3.0);
}

#[test]
fn test_pop_empty() {
    let source = "let arr = []; pop(arr);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

// =============================================================================
// SHIFT FUNCTION TESTS
// =============================================================================

#[test]
fn test_shift_basic() {
    let source = "let arr = [1, 2, 3]; shift(arr);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 1.0);
}

#[test]
fn test_shift_empty() {
    let source = "let arr = []; shift(arr);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

// =============================================================================
// FLAT FUNCTION TESTS
// =============================================================================

#[test]
fn test_flat_basic() {
    let source = "flat([[1, 2], [3, 4]]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 4);
}

#[test]
fn test_flat_nested() {
    let source = "flat([[[1]], [[2]]], 2);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
}

// =============================================================================
// FIND FUNCTION TESTS
// =============================================================================

#[test]
fn test_find_basic() {
    let source = "find([1, 2, 3, 4, 5], x => x > 3);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 4.0);
}

#[test]
fn test_find_not_found() {
    let source = "find([1, 2, 3], x => x > 10);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

// =============================================================================
// FIND_INDEX FUNCTION TESTS
// =============================================================================

#[test]
fn test_find_index_basic() {
    let source = "find_index([1, 2, 3, 4, 5], x => x > 3);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 3.0);
}

#[test]
fn test_find_index_not_found() {
    let source = "find_index([1, 2, 3], x => x > 10);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), -1.0);
}

// =============================================================================
// REDUCE FUNCTION TESTS
// =============================================================================

#[test]
fn test_reduce_sum() {
    let source = "reduce([1, 2, 3, 4], (acc, val) => acc + val, 0);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 10.0);
}

#[test]
fn test_reduce_product() {
    let source = "reduce([2, 3, 4], (acc, val) => acc * val, 1);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 24.0);
}

// =============================================================================
// EVERY FUNCTION TESTS
// =============================================================================

#[test]
fn test_every_true() {
    let source = "every([2, 4, 6], x => x % 2 == 0);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_every_false() {
    let source = "every([2, 4, 5], x => x % 2 == 0);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), false);
}

#[test]
fn test_every_empty() {
    let source = "every([], x => x > 0);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

// =============================================================================
// SOME FUNCTION TESTS
// =============================================================================

#[test]
fn test_some_true() {
    let source = "some([1, 2, 3], x => x > 2);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_some_false() {
    let source = "some([1, 2, 3], x => x > 10);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), false);
}

// =============================================================================
// ZIP FUNCTION TESTS
// =============================================================================

#[test]
fn test_zip_basic() {
    let source = "zip([1, 2, 3], [4, 5, 6]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    let first_pair = arr[0].as_array().unwrap();
    assert_eq!(first_pair[0].as_number().unwrap(), 1.0);
    assert_eq!(first_pair[1].as_number().unwrap(), 4.0);
}

#[test]
fn test_zip_different_lengths() {
    let source = "zip([1, 2], [3, 4, 5]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
}

// =============================================================================
// FIRST AND LAST FUNCTION TESTS
// =============================================================================

#[test]
fn test_first_basic() {
    let source = "first([1, 2, 3]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 1.0);
}

#[test]
fn test_first_empty() {
    let source = "first([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

#[test]
fn test_last_basic() {
    let source = "last([1, 2, 3]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 3.0);
}

#[test]
fn test_last_empty() {
    let source = "last([]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!(matches!(result, Value::Null));
}

// =============================================================================
// STRING FUNCTION TESTS
// =============================================================================

#[test]
fn test_split_basic() {
    let source = r#"split("a,b,c", ",");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0].as_string().unwrap(), "a");
}

#[test]
fn test_join_basic() {
    let source = r#"join(["a", "b", "c"], ",");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "a,b,c");
}

#[test]
fn test_trim_basic() {
    let source = r#"trim("  hello  ");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "hello");
}

#[test]
fn test_upper_basic() {
    let source = r#"upper("hello");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "HELLO");
}

#[test]
fn test_lower_basic() {
    let source = r#"lower("HELLO");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "hello");
}

#[test]
fn test_contains_true() {
    let source = r#"contains("hello world", "world");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_contains_false() {
    let source = r#"contains("hello", "xyz");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), false);
}

#[test]
fn test_starts_with_true() {
    let source = r#"starts_with("hello", "he");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_starts_with_false() {
    let source = r#"starts_with("hello", "lo");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), false);
}

#[test]
fn test_ends_with_true() {
    let source = r#"ends_with("hello", "lo");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_ends_with_false() {
    let source = r#"ends_with("hello", "he");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), false);
}

#[test]
fn test_replace_basic() {
    let source = r#"replace("hello world", "world", "rust");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "hello rust");
}

#[test]
fn test_len_basic() {
    let source = r#"len("hello");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 5.0);
}

// =============================================================================
// OBJECT FUNCTION TESTS
// =============================================================================

fn make_test_object() -> Value {
    let mut obj = IndexMap::new();
    obj.insert("name".to_string(), Value::String(Rc::from("Alice")));
    obj.insert("age".to_string(), Value::Number(30.0, false));
    obj.insert("active".to_string(), Value::Bool(true));
    Value::Object(Rc::new(RefCell::new(obj)))
}

#[test]
fn test_keys_basic() {
    let root = make_test_object();
    let source = "keys(root);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn test_values_basic() {
    let root = make_test_object();
    let source = "values(root);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn test_entries_basic() {
    let root = make_test_object();
    let source = "entries(root);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    let first_entry = arr[0].as_array().unwrap();
    assert_eq!(first_entry.len(), 2);
}

#[test]
fn test_has_true() {
    let root = make_test_object();
    let source = r#"has(root, "name");"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_has_false() {
    let root = make_test_object();
    let source = r#"has(root, "missing");"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), false);
}

#[test]
fn test_merge_basic() {
    let source = r#"merge({"a": 1, "b": 2}, {"b": 3, "c": 4});"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("a").unwrap().as_number().unwrap(), 1.0);
    assert_eq!(obj.get("b").unwrap().as_number().unwrap(), 3.0);
    assert_eq!(obj.get("c").unwrap().as_number().unwrap(), 4.0);
}

// =============================================================================
// TYPE FUNCTION TESTS
// =============================================================================

#[test]
fn test_typeof_number() {
    let source = "typeof(42);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "number");
}

#[test]
fn test_typeof_string() {
    let source = r#"typeof("hello");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "string");
}

#[test]
fn test_typeof_array() {
    let source = "typeof([1, 2, 3]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "array");
}

#[test]
fn test_is_null_true() {
    let source = "is_null(null);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_is_null_false() {
    let source = "is_null(42);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), false);
}

#[test]
fn test_is_array_true() {
    let source = "is_array([1, 2, 3]);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_is_array_false() {
    let source = "is_array(42);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), false);
}

#[test]
fn test_is_object_true() {
    let source = r#"is_object({"key": "value"});"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_is_string_true() {
    let source = r#"is_string("hello");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_is_number_true() {
    let source = "is_number(42);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_is_bool_true() {
    let source = "is_bool(true);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_bool().unwrap(), true);
}

#[test]
fn test_to_string_number() {
    let source = "to_string(42);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_string().unwrap(), "42");
}

#[test]
fn test_to_number_string() {
    let source = r#"to_number("42");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 42.0);
}

#[test]
fn test_to_number_already_number() {
    let source = "to_number(42);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 42.0);
}

#[test]
fn test_parse_json_basic() {
    let source = r#"parse_json("{\"key\": \"value\"}");"#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("key").unwrap().as_string().unwrap(), "value");
}

// =============================================================================
// MATH FUNCTION TESTS
// =============================================================================

#[test]
fn test_floor_basic() {
    let source = "floor(3.7);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 3.0);
}

#[test]
fn test_ceil_basic() {
    let source = "ceil(3.2);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 4.0);
}

#[test]
fn test_round_basic() {
    let source = "round(3.5);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 4.0);
}

#[test]
fn test_abs_positive() {
    let source = "abs(5);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 5.0);
}

#[test]
fn test_abs_negative() {
    let source = "abs(-5);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 5.0);
}

#[test]
fn test_sqrt_basic() {
    let source = "sqrt(16);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 4.0);
}

#[test]
fn test_pow_basic() {
    let source = "pow(2, 3);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 8.0);
}

#[test]
fn test_sin_basic() {
    let source = "sin(0);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!((result.as_number().unwrap() - 0.0).abs() < 0.001);
}

#[test]
fn test_cos_basic() {
    let source = "cos(0);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!((result.as_number().unwrap() - 1.0).abs() < 0.001);
}

#[test]
fn test_tan_basic() {
    let source = "tan(0);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert!((result.as_number().unwrap() - 0.0).abs() < 0.001);
}

#[test]
fn test_random_range() {
    let source = "random();";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let num = result.as_number().unwrap();
    assert!(num >= 0.0 && num < 1.0);
}
