use jfm::interpreter::parse_and_run;
use jfm::Value;
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

// Note: sort_by and group_by tests are in functions.rs