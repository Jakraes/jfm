//! Stress tests for jfm interpreter
//! Tests performance and correctness under heavy load

use jfm::interpreter::parse_and_run;
use jfm::lexer::Value;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

/// Create a large dataset with N users
fn make_large_dataset(n: usize) -> Value {
    let mut root_obj = IndexMap::new();
    let mut users = Vec::with_capacity(n);
    
    for i in 0..n {
        let mut user = IndexMap::new();
        user.insert("id".to_string(), Value::Number(i as f64));
        user.insert("name".to_string(), Value::String(Rc::from(format!("User{}", i))));
        user.insert("age".to_string(), Value::Number((20 + (i % 50)) as f64));
        user.insert("score".to_string(), Value::Number((i * 17 % 100) as f64));
        user.insert("active".to_string(), Value::Bool(i % 2 == 0));
        user.insert("department".to_string(), Value::String(Rc::from(format!("Dept{}", i % 10))));
        users.push(Value::Object(Rc::new(RefCell::new(user))));
    }
    
    root_obj.insert("users".to_string(), Value::Array(Rc::new(RefCell::new(users))));
    Value::Object(Rc::new(RefCell::new(root_obj)))
}

/// Create deeply nested object structure
fn make_deeply_nested(depth: usize) -> Value {
    let mut current = Value::Number(42.0);
    for i in (0..depth).rev() {
        let mut obj = IndexMap::new();
        obj.insert(format!("level{}", i), current);
        obj.insert("depth".to_string(), Value::Number(i as f64));
        current = Value::Object(Rc::new(RefCell::new(obj)));
    }
    let mut root = IndexMap::new();
    root.insert("nested".to_string(), current);
    Value::Object(Rc::new(RefCell::new(root)))
}

// =============================================================================
// LARGE DATASET TESTS
// =============================================================================

#[test]
fn test_stress_filter_1000_users() {
    let root = make_large_dataset(1000);
    let source = "root.users | .active == true;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 500, "Should filter to 500 active users");
}

#[test]
fn test_stress_filter_5000_users() {
    let root = make_large_dataset(5000);
    let source = "root.users | .age > 40;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert!(arr.len() > 0, "Should have filtered users");
}

#[test]
fn test_stress_map_1000_users() {
    let root = make_large_dataset(1000);
    let source = "root.users | .name;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1000, "Should map all 1000 users");
}

#[test]
fn test_stress_sum_1000_scores() {
    let root = make_large_dataset(1000);
    let source = "sum(root.users | .score);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert!(result.as_number().is_some(), "Should compute sum");
}

#[test]
fn test_stress_avg_5000_ages() {
    let root = make_large_dataset(5000);
    let source = "avg(root.users | .age);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let avg = result.as_number().unwrap();
    assert!(avg >= 20.0 && avg <= 70.0, "Average age should be reasonable");
}

#[test]
fn test_stress_count_large_array() {
    let root = make_large_dataset(10000);
    let source = "count(root.users);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 10000.0);
}

#[test]
fn test_stress_sort_by_1000_users() {
    let root = make_large_dataset(1000);
    let source = r#"sort_by(root.users, "score");"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1000);
    
    // Verify sorting is correct
    let first_score = arr[0].as_object().unwrap().get("score").unwrap().as_number().unwrap();
    let last_score = arr[999].as_object().unwrap().get("score").unwrap().as_number().unwrap();
    assert!(first_score <= last_score, "Should be sorted ascending");
}

#[test]
fn test_stress_group_by_1000_users() {
    let root = make_large_dataset(1000);
    let source = r#"group_by(root.users, "department");"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.len(), 10, "Should have 10 departments");
}

#[test]
fn test_stress_unique_large_array() {
    let source = r#"
        let arr = [];
        for i in 0..500 {
            arr += [i % 50];
        }
        unique(arr);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 50, "Should have 50 unique values");
}

#[test]
fn test_stress_take_from_large_array() {
    let root = make_large_dataset(5000);
    let source = "take(root.users, 100);";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 100);
}

// =============================================================================
// LARGE RANGE TESTS
// =============================================================================

#[test]
fn test_stress_range_1000() {
    let source = "0..999;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1000);
}

#[test]
fn test_stress_range_5000() {
    let source = "0..4999;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 5000);
}

#[test]
fn test_stress_range_sum() {
    let source = "sum(1..1000);";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // Sum of 1 to 1000 = 1000 * 1001 / 2 = 500500
    assert_eq!(result.as_number().unwrap(), 500500.0);
}

#[test]
fn test_stress_range_filter() {
    let source = "1..1000 | _it % 2 == 0;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 500, "Should have 500 even numbers");
}

// =============================================================================
// NESTED STRUCTURE TESTS
// =============================================================================

#[test]
fn test_stress_deeply_nested_access() {
    let root = make_deeply_nested(20);
    let source = "root.nested.level0.level1.level2.level3.level4.depth;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    // We access level0.level1.level2.level3.level4, which is depth 5
    assert_eq!(result.as_number().unwrap(), 5.0);
}

#[test]
fn test_stress_nested_loops() {
    let source = r#"
        let result = 0;
        for i in 0..49 {
            for j in 0..49 {
                result += 1;
            }
        }
        result;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // 0..49 gives 50 elements, 50*50=2500
    assert_eq!(result.as_number().unwrap(), 2500.0);
}

#[test]
fn test_stress_triple_nested_loops() {
    let source = r#"
        let result = 0;
        for i in 0..9 {
            for j in 0..9 {
                for k in 0..9 {
                    result += 1;
                }
            }
        }
        result;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // 0..9 gives 10 elements, 10*10*10=1000
    assert_eq!(result.as_number().unwrap(), 1000.0);
}

// =============================================================================
// CHAIN OPERATIONS TESTS
// =============================================================================

#[test]
fn test_stress_chained_filters() {
    let root = make_large_dataset(1000);
    let source = "root.users | .active == true | .age > 30 | .score > 50;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert!(arr.len() < 500, "Multiple filters should reduce count");
}

#[test]
fn test_stress_filter_sort_take() {
    let root = make_large_dataset(1000);
    let source = r#"take(sort_by(root.users | .active == true, "score"), 10);"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 10);
}

#[test]
fn test_stress_complex_aggregation() {
    let root = make_large_dataset(1000);
    let source = r#"
        let active = root.users | .active == true;
        let ages = active | .age;
        let scores = active | .score;
        let result = {
            "count": count(active),
            "avg_age": avg(ages),
            "total_score": sum(scores),
            "max_score": max(scores),
            "min_score": min(scores)
        };
        result;
    "#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("count").unwrap().as_number().unwrap(), 500.0);
}

// =============================================================================
// MEMORY STRESS TESTS
// =============================================================================

#[test]
fn test_stress_large_array_creation() {
    let source = r#"
        let arr = [];
        for i in 0..999 {
            arr += [{"id": i, "value": i * 2}];
        }
        count(arr);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // 0..999 gives 1000 elements
    assert_eq!(result.as_number().unwrap(), 1000.0);
}

#[test]
fn test_stress_string_concatenation() {
    let source = r#"
        let s = "";
        for i in 0..99 {
            s = s + "x";
        }
        s;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // 0..99 gives 100 iterations
    assert_eq!(result.as_string().unwrap().len(), 100);
}

#[test]
fn test_stress_array_concatenation() {
    let source = r#"
        let arr = [];
        for i in 0..99 {
            arr = arr + [i];
        }
        count(arr);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // 0..99 gives 100 elements
    assert_eq!(result.as_number().unwrap(), 100.0);
}

// =============================================================================
// ARITHMETIC STRESS TESTS
// =============================================================================

#[test]
fn test_stress_large_calculations() {
    let source = r#"
        let result = 0;
        for i in 1..1000 {
            result = result + i * i;
        }
        result;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // Sum of squares from 1 to 1000 (inclusive range)
    let expected: f64 = (1..=1000).map(|i: i64| (i * i) as f64).sum();
    assert_eq!(result.as_number().unwrap(), expected);
}

#[test]
fn test_stress_power_calculations() {
    let source = r#"
        let result = [];
        for i in 1..20 {
            result += [2 ^ i];
        }
        result;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr[0].as_number().unwrap(), 2.0);
    assert_eq!(arr[9].as_number().unwrap(), 1024.0);
}

// =============================================================================
// CONDITIONAL STRESS TESTS
// =============================================================================

#[test]
fn test_stress_many_conditionals() {
    let root = make_large_dataset(1000);
    let source = r#"
        let results = [];
        for user in root.users {
            if user.age < 25 {
                results += ["young"];
            } else if user.age < 35 {
                results += ["middle"];
            } else if user.age < 50 {
                results += ["senior"];
            } else {
                results += ["elder"];
            }
        }
        count(results);
    "#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 1000.0);
}

#[test]
fn test_stress_nested_conditionals() {
    let source = r#"
        let count = 0;
        for i in 0..100 {
            if i % 2 == 0 {
                if i % 3 == 0 {
                    if i % 5 == 0 {
                        count += 1;
                    }
                }
            }
        }
        count;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // Numbers divisible by 2, 3, and 5 (i.e., 30) from 0 to 99: 0, 30, 60, 90 = 4
    assert_eq!(result.as_number().unwrap(), 4.0);
}

// =============================================================================
// COMPOUND ASSIGNMENT STRESS TESTS
// =============================================================================

#[test]
fn test_stress_compound_assignments() {
    let source = r#"
        let x = 0;
        for i in 0..999 {
            x += 1;
        }
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // 0..999 gives 1000 iterations
    assert_eq!(result.as_number().unwrap(), 1000.0);
}

#[test]
fn test_stress_mixed_compound_assignments() {
    let source = r#"
        let x = 1000;
        for i in 0..99 {
            x += 10;
            x -= 5;
            x *= 1;
        }
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    // Each iteration: +10, -5 = +5, then *1 = no change
    // 0..99 gives 100 iterations
    // 1000 + 100 * 5 = 1500
    assert_eq!(result.as_number().unwrap(), 1500.0);
}

// =============================================================================
// PIPE OPERATOR STRESS TESTS  
// =============================================================================

#[test]
fn test_stress_pipe_transform_large() {
    let root = make_large_dataset(1000);
    let source = r#"root.users | {"name": .name, "doubled_age": .age * 2};"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 1000);
}

#[test]
fn test_stress_multiple_pipe_chains() {
    let root = make_large_dataset(500);
    let source = r#"
        let step1 = root.users | .active == true;
        let step2 = step1 | .age > 30;
        let step3 = step2 | .score;
        sum(step3);
    "#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert!(result.as_number().is_some());
}
