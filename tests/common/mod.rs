use jfm::interpreter::parse_and_run;
use jfm::Value;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

pub fn empty_root() -> Value {
    Value::Object(Rc::new(RefCell::new(IndexMap::new())))
}

pub fn make_test_array() -> Value {
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

pub fn make_user_array() -> Value {
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

pub fn make_test_root() -> Value {
    let mut root_obj = IndexMap::new();

    let mut users = Vec::new();
    let data = vec![
        ("Bob", 30.0),
        ("Alice", 25.0),
        ("Charlie", 35.0),
    ];

    for (name, age) in data {
        let mut user = IndexMap::new();
        user.insert("id".to_string(), Value::Number(users.len() as f64 + 1.0, false));
        user.insert("name".to_string(), Value::String(Rc::from(name)));
        user.insert("age".to_string(), Value::Number(age, false));
        users.push(Value::Object(Rc::new(RefCell::new(user))));
    }

    root_obj.insert("users".to_string(), Value::Array(Rc::new(RefCell::new(users))));
    root_obj.insert("multiplier".to_string(), Value::Number(10.0, false));
    
    Value::Object(Rc::new(RefCell::new(root_obj)))
}

pub fn make_simple_test_root() -> Value {
    let mut root_obj = IndexMap::new();
    root_obj.insert("name".to_string(), Value::String(Rc::from("Alice")));
    root_obj.insert("age".to_string(), Value::Number(30.0, false));
    root_obj.insert("active".to_string(), Value::Bool(true));
    Value::Object(Rc::new(RefCell::new(root_obj)))
}

pub fn make_complex_root() -> Value {
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

pub fn make_include_test_root() -> Value {
    let mut root_obj = IndexMap::new();
    
    let mut users = Vec::new();
    
    let mut user1 = IndexMap::new();
    user1.insert("name".to_string(), Value::String(Rc::from("Alice")));
    user1.insert("age".to_string(), Value::Number(25.0, false));
    users.push(Value::Object(Rc::new(RefCell::new(user1))));
    
    let mut user2 = IndexMap::new();
    user2.insert("name".to_string(), Value::String(Rc::from("Bob")));
    user2.insert("age".to_string(), Value::Number(30.0, false));
    users.push(Value::Object(Rc::new(RefCell::new(user2))));
    
    let mut user3 = IndexMap::new();
    user3.insert("name".to_string(), Value::String(Rc::from("Charlie")));
    user3.insert("age".to_string(), Value::Number(35.0, false));
    users.push(Value::Object(Rc::new(RefCell::new(user3))));
    
    root_obj.insert("users".to_string(), Value::Array(Rc::new(RefCell::new(users))));
    root_obj.insert("multiplier".to_string(), Value::Number(10.0, false));
    
    Value::Object(Rc::new(RefCell::new(root_obj)))
}

pub fn make_test_object() -> Value {
    let mut obj = IndexMap::new();
    obj.insert("name".to_string(), Value::String(Rc::from("Alice")));
    obj.insert("age".to_string(), Value::Number(30.0, false));
    obj.insert("active".to_string(), Value::Bool(true));
    Value::Object(Rc::new(RefCell::new(obj)))
}

pub fn run_query(query: &str) -> Value {
    parse_and_run(query, empty_root())
        .expect("Query should succeed")
        .expect("Query should return a value")
}

pub fn value_to_json(val: &Value) -> String {
    match val {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Number(n, is_float) => {
            if *is_float || n.fract() != 0.0 {
                n.to_string()
            } else {
                format!("{:.0}", n)
            }
        }
        Value::String(s) => format!("\"{}\"", s),
        Value::Array(arr) => {
            let items: Vec<String> = arr.borrow().iter().map(value_to_json).collect();
            format!("[{}]", items.join(","))
        }
        Value::Object(obj) => {
            let items: Vec<String> = obj.borrow().iter()
                .map(|(k, v)| format!("\"{}\":{}", k, value_to_json(v)))
                .collect();
            format!("{{{}}}", items.join(","))
        }
        Value::Function(_) => "\"<function>\"".to_string(),
        Value::Module(m) => format!("\"<module:{}>\"", m.name),
    }
}
