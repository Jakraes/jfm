use jfm::Value;
use jfm::interpreter::parse_and_run;
use std::rc::Rc;
use std::cell::RefCell;
use std::fs::{self, File};
use std::io::Write;
use indexmap::IndexMap;

// --- Helpers ---

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
}

impl Drop for TempScript {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

// --- INCLUDE TESTS (from include.rs) ---

#[test]
fn test_include_basic() {
    let script = TempScript::new("simple", "let x = 42; x;").unwrap();
    let source = format!(r#"include("{}");"#, script.escaped_path);
    assert_eq!(parse_and_run(&source, Value::Null).unwrap().unwrap().as_number().unwrap(), 42.0);
}

#[test]
fn test_include_with_root() {
    let script = TempScript::new("root_acc", "root.val * 2;").unwrap();
    let mut obj = IndexMap::new();
    obj.insert("val".to_string(), Value::Number(10.0, false));
    let root = Value::Object(Rc::new(RefCell::new(obj)));
    
    let source = format!(r#"include("{}");"#, script.escaped_path);
    assert_eq!(parse_and_run(&source, root).unwrap().unwrap().as_number().unwrap(), 20.0);
}

// --- IMPORT TESTS (from include.rs) ---

#[test]
fn test_import_basic() {
    let script = TempScript::new("math", "fn add(a, b) { a + b }").unwrap();
    let source = format!(r#"let m = import("{}"); m::add(1, 2);"#, script.escaped_path);
    assert_eq!(parse_and_run(&source, Value::Null).unwrap().unwrap().as_number().unwrap(), 3.0);
}

#[test]
fn test_import_type() {
    let script = TempScript::new("type", "fn d(){}").unwrap();
    let source = format!(r#"let m = import("{}"); typeof(m);"#, script.escaped_path);
    assert_eq!(parse_and_run(&source, Value::Null).unwrap().unwrap(), Value::String(Rc::from("module")));
}
