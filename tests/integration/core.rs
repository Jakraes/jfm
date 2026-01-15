use jfm::Value;
use jfm::interpreter::parse_and_run;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

fn make_simple_data() -> Value {
    let mut obj = IndexMap::new();
    obj.insert("x".to_string(), Value::Number(10.0, false));
    obj.insert("y".to_string(), Value::Number(20.0, false));
    obj.insert("name".to_string(), Value::String(Rc::from("test")));
    Value::Object(Rc::new(RefCell::new(obj)))
}

fn make_user_root() -> Value {
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

fn make_complex_root() -> Value {
    let mut root_obj = IndexMap::new();

    let mut users = Vec::new();
    let data = vec![
        (1, "Bob", 30, true, 120),
        (2, "Alice", 25, false, 80),
        (3, "Charlie", 35, true, 200),
        (4, "David", 22, true, 150),
    ];

    for (id, name, age, active, prb) in data {
        let mut u = IndexMap::new();
        u.insert("id".to_string(), Value::Number(id as f64, false));
        u.insert("name".to_string(), Value::String(Rc::from(name)));
        u.insert("age".to_string(), Value::Number(age as f64, false));
        u.insert("active".to_string(), Value::Bool(active));
        u.insert("prb".to_string(), Value::Number(prb as f64, false));
        users.push(Value::Object(Rc::new(RefCell::new(u))));
    }
    root_obj.insert("users".to_string(), Value::Array(Rc::new(RefCell::new(users))));

    let mut employees = Vec::new();
    let emp_data = vec![
        ("E1", 50000.0),
        ("E2", 60000.0),
    ];
    for (id, salary) in emp_data {
        let mut e = IndexMap::new();
        e.insert("id".to_string(), Value::String(Rc::from(id)));
        e.insert("salary".to_string(), Value::Number(salary, false));
        employees.push(Value::Object(Rc::new(RefCell::new(e))));
    }
    root_obj.insert("employees".to_string(), Value::Array(Rc::new(RefCell::new(employees))));

    Value::Object(Rc::new(RefCell::new(root_obj)))
}

// --- BASIC TESTS (from easy.rs) ---

#[test]
fn test_simple_variable_assignment() {
    let source = "let x = 5;";
    let result = parse_and_run(source, Value::Null);
    assert!(result.is_ok());
}

#[test]
fn test_number_arithmetic() {
    let source = "let result = 10 + 5 * 2; result;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 20.0);
}

#[test]
fn test_field_access() {
    let source = "root.x;";
    let result = parse_and_run(source, make_simple_data()).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 10.0);
}

#[test]
fn test_chained_field_access() {
    let mut inner = IndexMap::new();
    inner.insert("value".to_string(), Value::Number(42.0, false));
    let mut outer = IndexMap::new();
    outer.insert("nested".to_string(), Value::Object(Rc::new(RefCell::new(inner))));
    let root = Value::Object(Rc::new(RefCell::new(outer)));
    
    let source = "root.nested.value;";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 42.0);
}

#[test]
fn test_array_indexing() {
    let arr = vec![Value::Number(10.0, false), Value::Number(20.0, false)];
    let mut obj = IndexMap::new();
    obj.insert("numbers".to_string(), Value::Array(Rc::new(RefCell::new(arr))));
    let root = Value::Object(Rc::new(RefCell::new(obj)));
    
    let source = "root.numbers[0];";
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 10.0);
}

#[test]
fn test_logical_operators() {
    assert_eq!(parse_and_run("10 > 5;", Value::Null).unwrap().unwrap(), Value::Bool(true));
    assert_eq!(parse_and_run("true && false;", Value::Null).unwrap().unwrap(), Value::Bool(false));
    assert_eq!(parse_and_run("true || false;", Value::Null).unwrap().unwrap(), Value::Bool(true));
    assert_eq!(parse_and_run("!false;", Value::Null).unwrap().unwrap(), Value::Bool(true));
}

#[test]
fn test_literals() {
    assert_eq!(parse_and_run(r#""Alice";"#, Value::Null).unwrap().unwrap(), Value::String(Rc::from("Alice")));
    assert_eq!(parse_and_run("null;", Value::Null).unwrap().unwrap(), Value::Null);
    assert_eq!(parse_and_run("-5 + 0;", Value::Null).unwrap().unwrap(), Value::Number(-5.0, false));
}

#[test]
fn test_control_flow_basic() {
    let root = make_user_root();
    
    let source = r#"let names = []; for u in root.users { names = push(names, u.name); } names;"#;
    let result = parse_and_run(source, root.clone()).unwrap().unwrap();
    let items = result.as_array().unwrap();
    assert_eq!(items.len(), 3);
    assert_eq!(items[0], Value::String(Rc::from("Bob")));

    // If-else
    let source = r#"let res = []; for u in root.users { if u.age > 30 { res = push(res, "S"); } else { res = push(res, "J"); } } res;"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let items = result.as_array().unwrap();
    assert_eq!(items[0], Value::String(Rc::from("J")));
    assert_eq!(items[2], Value::String(Rc::from("S")));
}

#[test]
fn test_literals_extended() {
    let root = make_user_root();
    
    let source = r#"let first = root.users[0]; { "user": first.name, "adult": first.age >= 18 };"#;
    let result = parse_and_run(source, root.clone()).unwrap().unwrap();
    let obj = result.as_object().unwrap();
    assert_eq!(obj.get("user").unwrap(), &Value::String(Rc::from("Bob")));

    // Array literal
    let source = r#"[root.users[0].id, root.users[1].id];"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    assert_eq!(result.as_array().unwrap().len(), 2);
}

#[test]
fn test_operators_advanced() {
    let result = parse_and_run("1..5;", Value::Null).unwrap().unwrap();
    assert_eq!(result.as_array().unwrap().len(), 5);

    // Optional chaining
    let root = make_user_root();
    let result = parse_and_run("root.missing?.name;", root).unwrap().unwrap();
    assert_eq!(result, Value::Null);
}

#[test]
fn test_ternary_operator() {
    assert_eq!(parse_and_run("true ? 1 : 2;", Value::Null).unwrap().unwrap().as_number().unwrap(), 1.0);
    assert_eq!(parse_and_run("false ? 1 : 2;", Value::Null).unwrap().unwrap().as_number().unwrap(), 2.0);
    assert_eq!(parse_and_run("5 > 3 ? \"y\" : \"n\";", Value::Null).unwrap().unwrap(), Value::String(Rc::from("y")));
}

#[test]
fn test_null_coalescing() {
    assert_eq!(parse_and_run("null ?? \"def\";", Value::Null).unwrap().unwrap(), Value::String(Rc::from("def")));
    assert_eq!(parse_and_run("\"val\" ?? \"def\";", Value::Null).unwrap().unwrap(), Value::String(Rc::from("val")));
    assert_eq!(parse_and_run("0 ?? 100;", Value::Null).unwrap().unwrap().as_number().unwrap(), 0.0);
}

// --- COMPLEX TESTS (from complex.rs) ---

#[test]
fn test_complex_filtering_and_mapping() {
    let root = make_complex_root();
    
    // Smart pipe filter
    let source = r#"root.users | .active == true;"#;
    let result = parse_and_run(source, root.clone()).unwrap().unwrap();
    assert_eq!(result.as_array().unwrap().len(), 3);

    let source = r#"for e in root.employees { e.salary = e.salary * 1.1; } root.employees;"#;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let salary = result.as_array().unwrap()[0].as_object().unwrap().get("salary").unwrap().as_number().unwrap();
    assert!((salary - 55000.0).abs() < 0.01);
}

#[test]
fn test_complex_aggregates() {
    let root = make_complex_root();
    let result = parse_and_run("avg(root.users | .age);", root).unwrap().unwrap();
    assert_eq!(result.as_number().unwrap(), 28.0);
}
