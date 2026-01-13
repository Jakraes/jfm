use jfm::lexer::Value;
use jfm::interpreter::parse_and_run;
use std::rc::Rc;
use std::cell::RefCell;
use std::fs::{self, File};
use std::io::Write;
use indexmap::IndexMap;

/// Helper struct to create and automatically clean up temporary script files
struct TempScript {
    path: String,
    escaped_path: String,
}

impl TempScript {
    fn new(name: &str, content: &str) -> std::io::Result<Self> {
        let temp_dir = std::env::temp_dir();
        let path = temp_dir.join(format!("jfm_test_{}.jfm", name));
        let path_str = path.to_string_lossy().to_string();
        let mut file = File::create(&path)?;
        file.write_all(content.as_bytes())?;
        let escaped_path = path_str.replace('\\', "\\\\");
        Ok(Self { path: path_str, escaped_path })
    }

    fn path(&self) -> &str {
        &self.escaped_path
    }
}

impl Drop for TempScript {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

fn make_test_root() -> Value {
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

#[test]
fn test_include_simple_script() {
    // Create a simple script that returns a value
    let script = TempScript::new("simple", "let x = 42; x;").unwrap();
    
    let source = format!(r#"let result = include("{}"); result;"#, script.path());
    let root = Value::Null;
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 42.0, "Included script should return 42");
    } else {
        panic!("Expected number result from included script");
    }
}

#[test]
fn test_include_script_with_arithmetic() {
    // Create a script that performs calculations
    let script = TempScript::new("arithmetic", "let a = 10; let b = 5; a * b + 7;").unwrap();
    
    let source = format!(r#"let result = include("{}"); result;"#, script.path());
    let root = Value::Null;
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 57.0, "Included script should compute 10 * 5 + 7 = 57");
    } else {
        panic!("Expected number result");
    }
}

#[test]
fn test_include_script_accessing_root() {
    // Create a script that accesses the root variable
    let script = TempScript::new("root_access", "let users = root.users; count(users);").unwrap();
    
    let source = format!(r#"let user_count = include("{}"); user_count;"#, script.path());
    let root = make_test_root();
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 3.0, "Included script should count 3 users");
    } else {
        panic!("Expected number result");
    }
}

#[test]
fn test_include_script_returning_array() {
    // Create a script that returns an array
    let script = TempScript::new("return_array", "root.users | .name;").unwrap();
    
    let source = format!(r#"let names = include("{}"); names;"#, script.path());
    let root = make_test_root();
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Array(arr) = result {
        let borrowed = arr.borrow();
        assert_eq!(borrowed.len(), 3, "Should have 3 names");
        
        if let Value::String(s) = &borrowed[0] {
            assert_eq!(s.as_ref(), "Alice");
        } else {
            panic!("Expected string");
        }
    } else {
        panic!("Expected array result");
    }
}

#[test]
fn test_include_script_returning_object() {
    // Create a script that returns an object
    // Note: object literal must be assigned to avoid being parsed as a block
    let script = TempScript::new("return_object", r#"let obj = {"status": "ok", "count": 42}; obj;"#).unwrap();
    
    let source = format!(r#"let obj = include("{}"); obj.count;"#, script.path());
    let root = Value::Null;
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 42.0);
    } else {
        panic!("Expected number from object field");
    }
}

#[test]
fn test_include_script_with_filter() {
    // Create a script that filters data
    let script = TempScript::new("filter_users", "root.users | .age > 28;").unwrap();
    
    let source = format!(r#"let adults = include("{}"); count(adults);"#, script.path());
    let root = make_test_root();
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 2.0, "Should filter to 2 users (Bob and Charlie)");
    } else {
        panic!("Expected number result");
    }
}

#[test]
fn test_include_chained_scripts() {
    // Create two scripts where the second uses the result of the first
    let script1 = TempScript::new("chain1", "let base = 10; base * 2;").unwrap();
    let script2 = TempScript::new("chain2", "let x = 5; x + 3;").unwrap();
    
    let source = format!(
        r#"let a = include("{}"); let b = include("{}"); a + b;"#, 
        script1.path(), 
        script2.path()
    );
    let root = Value::Null;
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 28.0, "Should compute 20 + 8 = 28");
    } else {
        panic!("Expected number result");
    }
}

#[test]
fn test_include_nested_scripts() {
    // Create a script that includes another script
    let inner_script = TempScript::new("inner", "let inner_val = 100; inner_val;").unwrap();
    let outer_content = format!(r#"let from_inner = include("{}"); from_inner * 2;"#, inner_script.path());
    let outer_script = TempScript::new("outer", &outer_content).unwrap();
    
    let source = format!(r#"let result = include("{}"); result;"#, outer_script.path());
    let root = Value::Null;
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 200.0, "Nested include should return 100 * 2 = 200");
    } else {
        panic!("Expected number result from nested include");
    }
}

#[test]
fn test_include_script_with_for_loop() {
    // Create a script that uses a for loop
    let script = TempScript::new("for_loop", r#"
        let total = 0;
        for user in root.users {
            total += user.age;
        }
        total;
    "#).unwrap();
    
    let source = format!(r#"let sum_ages = include("{}"); sum_ages;"#, script.path());
    let root = make_test_root();
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 90.0, "Sum of ages should be 25 + 30 + 35 = 90");
    } else {
        panic!("Expected number result");
    }
}

#[test]
fn test_include_script_with_if_else() {
    // Create a script that uses if-else
    let script = TempScript::new("if_else", r#"
        let user_count = count(root.users);
        let result = "";
        if user_count > 2 {
            result = "many";
        } else {
            result = "few";
        }
        result;
    "#).unwrap();
    
    let source = format!(r#"let result = include("{}"); result;"#, script.path());
    let root = make_test_root();
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::String(s) = result {
        assert_eq!(s.as_ref(), "many", "Should return 'many' for 3 users");
    } else {
        panic!("Expected string result");
    }
}

#[test]
fn test_include_nonexistent_file_error() {
    let source = r#"let result = include("/nonexistent/path/to/script.jfm"); result;"#;
    let root = Value::Null;
    
    let result = parse_and_run(source, root);
    assert!(result.is_err(), "Should error on nonexistent file");
    let err = result.unwrap_err();
    assert!(err.contains("Failed to read script file"), "Error should mention file read failure");
}

#[test]
fn test_include_invalid_script_error() {
    // Create a script with syntax errors
    let script = TempScript::new("invalid", "let x = ; invalid syntax here").unwrap();
    
    let source = format!(r#"let result = include("{}"); result;"#, script.path());
    let root = Value::Null;
    
    let result = parse_and_run(&source, root);
    assert!(result.is_err(), "Should error on invalid script");
}

#[test]
fn test_include_empty_script() {
    // Create an empty script
    let script = TempScript::new("empty", "").unwrap();
    
    let source = format!(r#"let result = include("{}"); result;"#, script.path());
    let root = Value::Null;
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    // Empty script returns null
    assert!(matches!(result, Value::Null), "Empty script should return null");
}

#[test]
fn test_include_script_modifies_data() {
    // Create a script that modifies data and returns it
    let script = TempScript::new("modify", r#"
        let users = root.users;
        for user in users {
            user.age = user.age + 1;
        }
        users;
    "#).unwrap();
    
    let source = format!(r#"let modified = include("{}"); modified[0].age;"#, script.path());
    let root = make_test_root();
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 26.0, "Alice's age should be incremented from 25 to 26");
    } else {
        panic!("Expected number result");
    }
}

#[test]
fn test_include_using_parent_variable() {
    // Create a script that relies on a variable from the calling context
    // Note: included scripts share the environment, so they can access variables
    let script = TempScript::new("use_var", "multiplier * 5;").unwrap();
    
    let source = format!(r#"let multiplier = root.multiplier; let result = include("{}"); result;"#, script.path());
    let root = make_test_root();
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Number(n, _) = result {
        assert_eq!(n, 50.0, "Should compute 10 * 5 = 50 using parent's multiplier");
    } else {
        panic!("Expected number result");
    }
}

#[test]
fn test_include_with_aggregation() {
    // Create a script that uses aggregation functions
    let script = TempScript::new("aggregation", r#"
        let ages = root.users | .age;
        let average = avg(ages);
        let total = sum(ages);
        let stats = {"average": average, "total": total};
        stats;
    "#).unwrap();
    
    let source = format!(r#"let stats = include("{}"); stats;"#, script.path());
    let root = make_test_root();
    
    let result = parse_and_run(&source, root).unwrap().unwrap();
    if let Value::Object(obj) = result {
        let borrowed = obj.borrow();
        if let Some(Value::Number(avg, _)) = borrowed.get("average") {
            assert_eq!(*avg, 30.0, "Average age should be 30");
        } else {
            panic!("Expected average field");
        }
        if let Some(Value::Number(total, _)) = borrowed.get("total") {
            assert_eq!(*total, 90.0, "Total should be 90");
        } else {
            panic!("Expected total field");
        }
    } else {
        panic!("Expected object result");
    }
}

#[test]
fn test_include_requires_string_argument() {
    // Try to call include with a non-string argument
    let source = "let result = include(42); result;";
    let root = Value::Null;
    
    let result = parse_and_run(source, root);
    assert!(result.is_err(), "Should error when include is called with non-string");
    let err = result.unwrap_err();
    assert!(err.contains("string path"), "Error should mention string requirement");
}

#[test]
fn test_include_requires_argument() {
    // Try to call include with no arguments
    let source = "let result = include(); result;";
    let root = Value::Null;
    
    let result = parse_and_run(source, root);
    assert!(result.is_err(), "Should error when include is called without arguments");
    let err = result.unwrap_err();
    assert!(err.contains("file path argument"), "Error should mention argument requirement");
}
