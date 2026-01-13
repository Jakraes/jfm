use jfm::interpreter::parse_and_run;
use jfm::lexer::Value;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

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

#[test]
fn test_example_1_or_filter() {
    let source = r#"root.users | .name == "Bob" || .name == "Alice";"#;
    let root = make_complex_root();
    let result = parse_and_run(source, root).unwrap();
    let arr = result.unwrap();
    assert_eq!(arr.as_array().unwrap().len(), 2);
}

#[test]
fn test_example_2_map_salary() {
    let source = r#"
        for e in root.employees {
            e.salary = e.salary * 1.10;
        }
        root.employees;
    "#;
    let root = make_complex_root();
    let result = parse_and_run(source, root).unwrap();
    let arr_val = result.unwrap();
    let arr = arr_val.as_array().unwrap();
    let salary = arr[0].as_object().unwrap().get("salary").unwrap().as_number().unwrap();
    assert!((salary - 55000.0).abs() < 0.001, "Expected roughly 55000.0, got {}", salary);
}

#[test]
fn test_example_4_filter_active() {
    let source = r#"root.users | .active == true;"#;
    let root = make_complex_root();
    let result = parse_and_run(source, root).unwrap();
    let arr = result.unwrap();
    assert_eq!(arr.as_array().unwrap().len(), 3);
}

#[test]
fn test_example_6_group_by() {
    let source = r#"group_by(root.users, "active");"#;
    let root = make_complex_root();
    let result = parse_and_run(source, root).unwrap();
    let obj_val = result.unwrap();
    let obj = obj_val.as_object().unwrap();
    assert!(obj.contains_key("true"));
    assert!(obj.contains_key("false"));
    assert_eq!(obj.get("true").unwrap().as_array().unwrap().len(), 3);
}

#[test]
fn test_example_7_sort_by() {
    let source = r#"sort_by(root.users, "prb");"#;
    let root = make_complex_root();
    let result = parse_and_run(source, root).unwrap();
    let arr_val = result.unwrap();
    let arr = arr_val.as_array().unwrap();
    assert_eq!(arr[0].as_object().unwrap().get("name").unwrap().as_string().unwrap(), "Alice");
    assert_eq!(arr[3].as_object().unwrap().get("name").unwrap().as_string().unwrap(), "Charlie");
}

#[test]
fn test_example_8_avg_age() {
    let source = r#"avg(root.users | .age);"#;
    let root = make_complex_root();
    let result = parse_and_run(source, root).unwrap();
    assert_eq!(result.unwrap().as_number().unwrap(), 28.0);
}

#[test]
fn test_example_12_complex_filter() {
    let source = r#"root.users | .prb > 100 && .age < 35;"#;
    let root = make_complex_root();
    let result = parse_and_run(source, root).unwrap();
    let arr = result.unwrap();
    assert_eq!(arr.as_array().unwrap().len(), 2);
}

#[test]
fn test_deep_nesting_and_shadowing() {
    let source = r#"
        let value = 1;
        {
            let value = { inner: { level: { val: 42 } } };
            value.inner.level.val;
        }
        value;
    "#;
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap();
    assert_eq!(result, Some(Value::Number(1.0, false)));
}

#[test]
fn test_large_range_array() {
    let source = "0..199;";
    let root = Value::Null;
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 200);
    assert_eq!(arr[0], Value::Number(0.0, false));
    assert_eq!(arr[199], Value::Number(199.0, false));
}

#[test]
fn test_complex_pipe_chain() {
    let source = r#"
        take(sort_by(root.users, "prb"), 2);
    "#;
    let root = make_complex_root();
    let result = parse_and_run(source, root).unwrap().unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 2);
    let first = arr[0].as_object().unwrap();
    let second = arr[1].as_object().unwrap();
    assert_eq!(first.get("name").unwrap().as_string().unwrap(), "Alice");
    assert_eq!(second.get("name").unwrap().as_string().unwrap(), "Bob");
}