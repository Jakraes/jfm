use jfm::lexer::Value;
use jfm::interpreter::parse_and_run;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

// ============================================================================
// Ternary Operator Tests
// ============================================================================

#[test]
fn test_ternary_true_condition() {
    let source = "true ? 1 : 2;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 1.0);
    } else {
        panic!("Expected number 1");
    }
}

#[test]
fn test_ternary_false_condition() {
    let source = "false ? 1 : 2;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 2.0);
    } else {
        panic!("Expected number 2");
    }
}

#[test]
fn test_ternary_with_expression_condition() {
    let source = "5 > 3 ? \"yes\" : \"no\";";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "yes");
    } else {
        panic!("Expected string 'yes'");
    }
}

#[test]
fn test_ternary_with_complex_expressions() {
    let source = "let x = 10; x > 5 ? x * 2 : x / 2;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 20.0);
    } else {
        panic!("Expected number 20");
    }
}

#[test]
fn test_nested_ternary() {
    let source = "let x = 5; x > 10 ? 1 : x > 3 ? 2 : 3;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 2.0);
    } else {
        panic!("Expected number 2");
    }
}

#[test]
fn test_ternary_with_null_condition() {
    let source = "null ? 1 : 2;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 2.0);
    } else {
        panic!("Expected number 2 (null is falsy)");
    }
}

#[test]
fn test_ternary_with_truthy_values() {
    // Non-null, non-false values should be truthy
    let source = "1 ? \"truthy\" : \"falsy\";";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "truthy");
    } else {
        panic!("Expected string 'truthy'");
    }
}

#[test]
fn test_ternary_in_variable() {
    let source = "let result = true ? 42 : 0; result;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 42.0);
    } else {
        panic!("Expected number 42");
    }
}

#[test]
fn test_ternary_with_root_access() {
    let mut obj = IndexMap::new();
    obj.insert("active".to_string(), Value::Bool(true));
    obj.insert("value".to_string(), Value::Number(100.0, false));
    let root = Value::Object(Rc::new(RefCell::new(obj)));
    
    let source = "root.active ? root.value : 0;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 100.0);
    } else {
        panic!("Expected number 100");
    }
}

// ============================================================================
// Null Coalescing Operator Tests
// ============================================================================

#[test]
fn test_null_coalesce_with_null() {
    let source = "null ?? \"default\";";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "default");
    } else {
        panic!("Expected string 'default'");
    }
}

#[test]
fn test_null_coalesce_with_value() {
    let source = "\"actual\" ?? \"default\";";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "actual");
    } else {
        panic!("Expected string 'actual'");
    }
}

#[test]
fn test_null_coalesce_with_number() {
    let source = "0 ?? 100;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 0.0);  // 0 is not null, so should return 0
    } else {
        panic!("Expected number 0");
    }
}

#[test]
fn test_null_coalesce_with_false() {
    let source = "false ?? true;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Bool(b) = result {
        assert_eq!(b, false);  // false is not null, so should return false
    } else {
        panic!("Expected boolean false");
    }
}

#[test]
fn test_chained_null_coalesce() {
    let source = "null ?? null ?? \"fallback\";";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "fallback");
    } else {
        panic!("Expected string 'fallback'");
    }
}

#[test]
fn test_null_coalesce_with_field_access() {
    let mut obj = IndexMap::new();
    obj.insert("missing".to_string(), Value::Null);
    obj.insert("present".to_string(), Value::Number(42.0, false));
    let root = Value::Object(Rc::new(RefCell::new(obj)));
    
    let source = "root.missing ?? root.present;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 42.0);
    } else {
        panic!("Expected number 42");
    }
}

#[test]
fn test_null_coalesce_short_circuit() {
    // The right side should not be evaluated when left is not null
    let source = "let x = 10; x ?? (x = 20); x;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 10.0);  // x should still be 10
    } else {
        panic!("Expected number 10");
    }
}

#[test]
fn test_null_coalesce_in_variable() {
    let source = "let val = null ?? 42; val;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 42.0);
    } else {
        panic!("Expected number 42");
    }
}

// ============================================================================
// Combined Ternary and Null Coalescing Tests
// ============================================================================

#[test]
fn test_ternary_with_null_coalesce() {
    let source = "true ? null ?? \"default\" : \"else\";";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "default");
    } else {
        panic!("Expected string 'default'");
    }
}

#[test]
fn test_null_coalesce_in_ternary_condition() {
    let source = "(null ?? true) ? \"yes\" : \"no\";";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "yes");
    } else {
        panic!("Expected string 'yes'");
    }
}

#[test]
fn test_complex_conditional_chain() {
    let mut obj = IndexMap::new();
    obj.insert("status".to_string(), Value::Null);
    obj.insert("fallback".to_string(), Value::String(Rc::from("pending")));
    let root = Value::Object(Rc::new(RefCell::new(obj)));
    
    let source = "(root.status ?? root.fallback) == \"pending\" ? \"waiting\" : \"done\";";
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "waiting");
    } else {
        panic!("Expected string 'waiting'");
    }
}

#[test]
fn test_optional_chaining_with_null_coalesce() {
    let mut obj = IndexMap::new();
    obj.insert("user".to_string(), Value::Null);
    let root = Value::Object(Rc::new(RefCell::new(obj)));
    
    let source = "root.user?.name ?? \"Anonymous\";";
    let result = parse_and_run(source, root).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "Anonymous");
    } else {
        panic!("Expected string 'Anonymous'");
    }
}

#[test]
fn test_ternary_in_array() {
    let source = "[true ? 1 : 0, false ? 1 : 0];";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    if let Value::Array(arr) = result {
        let borrowed = arr.borrow();
        assert_eq!(borrowed.len(), 2);
        if let Value::Number(n1, _) = &borrowed[0] {
            assert_eq!(*n1, 1.0);
        }
        if let Value::Number(n2, _) = &borrowed[1] {
            assert_eq!(*n2, 0.0);
        }
    } else {
        panic!("Expected array");
    }
}
