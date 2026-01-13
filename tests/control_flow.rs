use jfm::interpreter::parse_and_run;
use jfm::lexer::Value;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

#[allow(dead_code)]
fn make_test_root() -> Value {
    let mut root = IndexMap::new();
    let mut users = Vec::new();
    
    for (name, age) in [("Alice", 25.0), ("Bob", 30.0), ("Charlie", 35.0)] {
        let mut user = IndexMap::new();
        user.insert("name".to_string(), Value::String(Rc::from(name)));
        user.insert("age".to_string(), Value::Number(age));
        users.push(Value::Object(Rc::new(RefCell::new(user))));
    }
    
    root.insert("users".to_string(), Value::Array(Rc::new(RefCell::new(users))));
    Value::Object(Rc::new(RefCell::new(root)))
}

#[test]
fn test_while_loop_basic() {
    let source = r#"
        let x = 0;
        while x < 5 {
            x = x + 1;
        }
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(5.0)));
}

#[test]
fn test_while_loop_condition_false() {
    let source = r#"
        let x = 10;
        while x < 5 {
            x = x + 1;
        }
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(10.0)));
}

#[test]
fn test_while_loop_with_array() {
    let source = r#"
        let sum = 0;
        let i = 0;
        let arr = [1, 2, 3, 4, 5];
        while i < arr.length {
            sum = sum + arr[i];
            i = i + 1;
        }
        sum;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(15.0)));
}

#[test]
fn test_break_in_while_loop() {
    let source = r#"
        let x = 0;
        while true {
            x = x + 1;
            if x >= 5 {
                break;
            }
        }
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(5.0)));
}

#[test]
fn test_continue_in_while_loop() {
    let source = r#"
        let x = 0;
        let sum = 0;
        while x < 10 {
            x = x + 1;
            if x % 2 == 0 {
                continue;
            }
            sum = sum + x;
        }
        sum;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(25.0)));
}

#[test]
fn test_break_in_for_loop() {
    let source = r#"
        let sum = 0;
        for x in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] {
            if x > 5 {
                break;
            }
            sum = sum + x;
        }
        sum;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(15.0)));
}

#[test]
fn test_continue_in_for_loop() {
    let source = r#"
        let sum = 0;
        for x in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] {
            if x % 2 == 0 {
                continue;
            }
            sum = sum + x;
        }
        sum;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(25.0)));
}

#[test]
fn test_nested_loops_with_break() {
    let source = r#"
        let outer = 0;
        let inner = 0;
        while outer < 3 {
            outer = outer + 1;
            inner = 0;
            while inner < 5 {
                inner = inner + 1;
                if inner >= 2 {
                    break;
                }
            }
        }
        inner;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(2.0)));
}

#[test]
fn test_lambda_single_parameter() {
    let source = r#"
        let f = x => x + 1;
        f(5);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(6.0)));
}

#[test]
fn test_lambda_multiple_parameters() {
    let source = r#"
        let add = (x, y) => x + y;
        add(3, 4);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(7.0)));
}

#[test]
fn test_lambda_no_parameters() {
    let source = r#"
        let get_five = () => 5;
        get_five();
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(5.0)));
}

#[test]
fn test_lambda_assigned_and_called() {
    let source = r#"
        let double = x => x * 2;
        double(10);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(20.0)));
}

#[test]
fn test_user_defined_function() {
    let source = r#"
        fn add(x, y) {
            return x + y;
        }
        add(5, 3);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(8.0)));
}

#[test]
fn test_user_defined_function_no_return() {
    let source = r#"
        fn get_ten() {
            let x = 10;
            x;
        }
        get_ten();
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(10.0)));
}

#[test]
fn test_user_defined_function_multiple_statements() {
    let source = r#"
        fn multiply(x, y) {
            let result = x * y;
            return result;
        }
        multiply(4, 7);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(28.0)));
}

#[test]
fn test_function_with_conditional() {
    let source = r#"
        fn max(a, b) {
            if a > b {
                return a;
            } else {
                return b;
            }
        }
        max(5, 10);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(10.0)));
}

#[test]
fn test_function_recursion() {
    let source = r#"
        fn factorial(n) {
            if n <= 1 {
                return 1;
            } else {
                return n * factorial(n - 1);
            }
        }
        factorial(5);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(120.0)));
}

#[test]
fn test_lambda_captures_outer_scope() {
    let source = r#"
        let x = 10;
        let f = y => x + y;
        f(5);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(15.0)));
}

#[test]
fn test_function_calls_function() {
    let source = r#"
        fn double(x) {
            return x * 2;
        }
        fn quadruple(x) {
            return double(double(x));
        }
        quadruple(5);
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(20.0)));
}
