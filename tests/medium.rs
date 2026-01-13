use jfm::interpreter::parse_and_run;
use jfm::lexer::Value;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

fn make_test_root() -> Value {
    let mut root_obj = IndexMap::new();

    let mut users = Vec::new();

    let mut user1 = IndexMap::new();
    user1.insert("id".to_string(), Value::Number(1.0, false));
    user1.insert("name".to_string(), Value::String(Rc::from("Bob")));
    user1.insert("age".to_string(), Value::Number(30.0, false));
    users.push(Value::Object(Rc::new(RefCell::new(user1))));

    let mut user2 = IndexMap::new();
    user2.insert("id".to_string(), Value::Number(2.0, false));
    user2.insert("name".to_string(), Value::String(Rc::from("Alice")));
    user2.insert("age".to_string(), Value::Number(25.0, false));
    users.push(Value::Object(Rc::new(RefCell::new(user2))));

    let mut user3 = IndexMap::new();
    user3.insert("id".to_string(), Value::Number(3.0, false));
    user3.insert("name".to_string(), Value::String(Rc::from("Charlie")));
    user3.insert("age".to_string(), Value::Number(35.0, false));
    users.push(Value::Object(Rc::new(RefCell::new(user3))));

    root_obj.insert("users".to_string(), Value::Array(Rc::new(RefCell::new(users))));
    Value::Object(Rc::new(RefCell::new(root_obj)))
}

#[test]
fn test_for_loop() {
    let source = r#"
        let names = [];
        for u in root.users {
            names = push(names, u.name);
        }
        names;
    "#;
    let root = make_test_root();
    let result = parse_and_run(source, root).unwrap();
    if let Some(Value::Array(arr)) = result {
        let items = arr.borrow();
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], Value::String(Rc::from("Bob")));
        assert_eq!(items[1], Value::String(Rc::from("Alice")));
        assert_eq!(items[2], Value::String(Rc::from("Charlie")));
    } else {
        panic!("Expected array of names, got {:?}", result);
    }
}

#[test]
fn test_if_else() {
    let source = r#"
        let results = [];
        for u in root.users {
            if u.age > 30 {
                results = push(results, "Senior");
            } else {
                results = push(results, "Junior");
            }
        }
        results;
    "#;
    let root = make_test_root();
    let result = parse_and_run(source, root).unwrap();
    if let Some(Value::Array(arr)) = result {
        let items = arr.borrow();
        assert_eq!(items[0], Value::String(Rc::from("Junior")));
        assert_eq!(items[1], Value::String(Rc::from("Junior")));
        assert_eq!(items[2], Value::String(Rc::from("Senior")));
    } else {
        panic!("Expected array of statuses, got {:?}", result);
    }
}

#[test]
fn test_object_literal() {
    let source = r#"
        let first = root.users[0];
        let summary = { "user": first.name, "is_adult": first.age >= 18 };
        summary;
    "#;
    let root = make_test_root();
    let result = parse_and_run(source, root).unwrap();
    if let Some(Value::Object(obj)) = result {
        let map = obj.borrow();
        assert_eq!(map.get("user").unwrap(), &Value::String(Rc::from("Bob")));
        assert_eq!(map.get("is_adult").unwrap(), &Value::Bool(true));
    } else {
        panic!("Expected object, got {:?}", result);
    }
}

#[test]
fn test_array_literal() {
    let source = r#"
        let ids = [root.users[0].id, root.users[1].id, root.users[2].id];
        ids;
    "#;
    let root = make_test_root();
    let result = parse_and_run(source, root).unwrap();
    if let Some(Value::Array(arr)) = result {
        let items = arr.borrow();
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], Value::Number(1.0, false));
    } else {
        panic!("Expected array, got {:?}", result);
    }
}

#[test]
fn test_smart_pipe() {
    let source = r#"
        root.users | .age > 28;
    "#;
    let root = make_test_root();
    let result = parse_and_run(source, root).unwrap();
    if let Some(Value::Array(arr)) = result {
        let items = arr.borrow();
        assert_eq!(items.len(), 2);
    } else {
        panic!("Expected array, got {:?}", result);
    }
}

#[test]
fn test_range_operator() {
    let source = "1..5;";
    let result = parse_and_run(source, Value::Null).unwrap();
    if let Some(Value::Array(arr)) = result {
        let items = arr.borrow();
        assert_eq!(items.len(), 5);
        assert_eq!(items[0], Value::Number(1.0, false));
        assert_eq!(items[4], Value::Number(5.0, false));
    } else {
        panic!("Expected array, got {:?}", result);
    }
}

#[test]
fn test_assignment_operations() {
    let source = r#"
        let x = 10;
        x = x + 5;
        x = x * 2;
        x;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    assert_eq!(result, Some(Value::Number(30.0, false)));
}

#[test]
fn test_optional_chaining() {
    let source = r#"
        let name = root.non_existent?.name;
        name;
    "#;
    let root = make_test_root();
    let result = parse_and_run(source, root).unwrap();
    assert_eq!(result, Some(Value::Null));
}
#[test]
fn test_push() {
    let source = r#"
        let items = [1, 2];
        items = push(items, 3);
        items;
    "#;
    let result = parse_and_run(source, Value::Null).unwrap();
    if let Some(Value::Array(arr)) = result {
        let items = arr.borrow();
        assert_eq!(items.len(), 3);
        assert_eq!(items[2], Value::Number(3.0, false));
    } else {
        panic!("Expected array, got {:?}", result);
    }
}

#[test]
fn test_sort_by() {
    let source = r#"
        sort_by(root.users, "age");
    "#;
    let root = make_test_root();
    let result = parse_and_run(source, root).unwrap();
    if let Some(Value::Array(arr_rc)) = result {
        let arr = arr_rc.borrow();
        assert_eq!(arr.len(), 3);
        assert_eq!(arr[0].as_object().unwrap().get("name").unwrap(), &Value::String(Rc::from("Alice")));
        assert_eq!(arr[1].as_object().unwrap().get("name").unwrap(), &Value::String(Rc::from("Bob")));
        assert_eq!(arr[2].as_object().unwrap().get("name").unwrap(), &Value::String(Rc::from("Charlie")));
    } else {
        panic!("Expected array, got {:?}", result);
    }
}

#[test]
fn test_group_by() {
    let source = r#"
        group_by(root.users, "age");
    "#;
    let root = make_test_root();
    let result = parse_and_run(source, root).unwrap();
    if let Some(Value::Object(obj_rc)) = result {
        let obj = obj_rc.borrow();
        assert!(obj.contains_key("25"));
        assert!(obj.contains_key("30"));
        assert!(obj.contains_key("35"));
        assert_eq!(obj.get("25").unwrap().as_array().unwrap().len(), 1);
    } else {
        panic!("Expected object, got {:?}", result);
    }
}

#[test]
fn test_division_by_zero_error() {
    let source = "let x = 1 / 0;";
    let root = Value::Null;
    let result = parse_and_run(source, root);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Division by zero"));
}

#[test]
fn test_index_out_of_bounds_error() {
    let source = "root.users[10];";
    let root = make_test_root();
    let result = parse_and_run(source, root);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Index out of bounds"));
}

#[test]
fn test_type_error_mismatch() {
    let source = r#"let x = "abc" + 5;"#;
    let root = Value::Null;
    let result = parse_and_run(source, root);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Invalid operation"));
}

#[test]
fn test_undefined_variable_error() {
    let source = "y + 1;";
    let root = Value::Null;
    let result = parse_and_run(source, root);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Undefined variable"));
}

#[test]
fn test_field_access_on_number_error() {
    let source = "let v = 10; v.name;";
    let root = Value::Null;
    let result = parse_and_run(source, root);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Cannot access field"));
}