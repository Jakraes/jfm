use jfm::Value;
use jfm::interpreter::parse_and_run;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

// --- Helpers ---

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

// --- ARRAY FUNCTIONS (from functions.rs) ---

#[test]
fn test_array_builtins() {
    let root = make_test_array();
    
    // Length
    assert_eq!(parse_and_run("root.numbers.length;", root.clone()).unwrap().unwrap().as_number().unwrap(), 6.0);
    
    // Sum
    assert_eq!(parse_and_run("sum(root.numbers);", root.clone()).unwrap().unwrap().as_number().unwrap(), 28.0);
    
    // Avg
    let avg_res = parse_and_run("avg(root.numbers);", root.clone()).unwrap().unwrap().as_number().unwrap();
    assert!((avg_res - 28.0/6.0).abs() < 0.001);
    
    // Min/Max
    assert_eq!(parse_and_run("min(root.numbers);", root.clone()).unwrap().unwrap().as_number().unwrap(), 1.0);
    assert_eq!(parse_and_run("max(root.numbers);", root.clone()).unwrap().unwrap().as_number().unwrap(), 9.0);
}

#[test]
fn test_array_transformations() {
    let source = "unique([1, 2, 2, 3]);";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_array().unwrap().len(), 3);

    let source = "reverse([1, 2, 3]);";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_array().unwrap()[0].as_number().unwrap(), 3.0);

    let source = "sort([3, 1, 2]);";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_array().unwrap()[0].as_number().unwrap(), 1.0);
}

#[test]
fn test_complex_array_methods() {
    // Find
    let source = "find([1, 2, 3, 4], x => x > 2);";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 3.0);

    // Reduce
    let source = "reduce([1, 2, 3], (acc, v) => acc + v, 0);";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 6.0);
    
    // Every/Some
    assert_eq!(parse_and_run("every([2, 4], x => x % 2 == 0);", Value::Null).unwrap().unwrap().as_bool().unwrap(), true);
    assert_eq!(parse_and_run("some([1, 2], x => x == 2);", Value::Null).unwrap().unwrap().as_bool().unwrap(), true);
}

// --- STRING FUNCTIONS (from functions.rs) ---

#[test]
fn test_string_builtins() {
    assert_eq!(parse_and_run(r#"split("a,b", ",");"#, Value::Null).unwrap().unwrap().as_array().unwrap().len(), 2);
    assert_eq!(parse_and_run(r#"join(["a", "b"], "-");"#, Value::Null).unwrap().unwrap().as_string().unwrap(), "a-b");
    assert_eq!(parse_and_run(r#"upper("hi");"#, Value::Null).unwrap().unwrap().as_string().unwrap(), "HI");
}

// --- IO FUNCTIONS (from io.rs) ---

#[test]
fn test_io_builtins_smoke() {
    // print returns null
    let source = r#"print("hello", 42);"#;
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap(), Value::Null);
    
    // print nested
    let source = "print([1, {a: 2}]);";
    assert!(parse_and_run(source, Value::Null).is_ok());
}
