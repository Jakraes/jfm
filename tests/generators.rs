use jfm::interpreter::parse_and_run;
use jfm::lexer::Value;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

fn empty_root() -> Value {
    Value::Object(Rc::new(RefCell::new(IndexMap::new())))
}

fn run_query(query: &str) -> Value {
    parse_and_run(query, empty_root())
        .expect("Query should succeed")
        .expect("Query should return a value")
}

fn value_to_json(val: &Value) -> String {
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
    }
}

// ============ REPLICATE TESTS ============

#[test]
fn test_replicate_basic() {
    let result = run_query("replicate(5, i => i);");
    assert_eq!(value_to_json(&result), "[0,1,2,3,4]");
}

#[test]
fn test_replicate_with_transform() {
    let result = run_query("replicate(4, i => i * 10);");
    assert_eq!(value_to_json(&result), "[0,10,20,30]");
}

#[test]
fn test_replicate_objects() {
    let result = run_query("replicate(3, i => { id: i, value: i * 2 });");
    assert_eq!(value_to_json(&result), r#"[{"id":0,"value":0},{"id":1,"value":2},{"id":2,"value":4}]"#);
}

#[test]
fn test_replicate_zero() {
    let result = run_query("replicate(0, i => i);");
    assert_eq!(value_to_json(&result), "[]");
}

// ============ ENUMERATE TESTS ============

#[test]
fn test_enumerate_basic() {
    let result = run_query(r#"enumerate(["a", "b", "c"]);"#);
    assert_eq!(value_to_json(&result), r#"[[0,"a"],[1,"b"],[2,"c"]]"#);
}

#[test]
fn test_enumerate_empty() {
    let result = run_query("enumerate([]);");
    assert_eq!(value_to_json(&result), "[]");
}

#[test]
fn test_enumerate_numbers() {
    let result = run_query("enumerate([10, 20, 30]);");
    assert_eq!(value_to_json(&result), "[[0,10],[1,20],[2,30]]");
}

// ============ RANGE FUNCTION TESTS ============

#[test]
fn test_range_fn_basic() {
    let result = run_query("range(1, 5);");
    assert_eq!(value_to_json(&result), "[1,2,3,4,5]");
}

#[test]
fn test_range_fn_with_step() {
    let result = run_query("range(0, 20, 5);");
    assert_eq!(value_to_json(&result), "[0,5,10,15,20]");
}

#[test]
fn test_range_fn_negative_step() {
    let result = run_query("range(10, 0, -2);");
    assert_eq!(value_to_json(&result), "[10,8,6,4,2,0]");
}

#[test]
fn test_range_fn_floats() {
    let result = run_query("range(0, 1, 0.25);");
    assert_eq!(value_to_json(&result), "[0,0.25,0.5,0.75,1]");
}

// ============ DEEP_MERGE TESTS ============

#[test]
fn test_deep_merge_basic() {
    let result = run_query("deep_merge({ a: 1 }, { b: 2 });");
    assert_eq!(value_to_json(&result), r#"{"a":1,"b":2}"#);
}

#[test]
fn test_deep_merge_nested() {
    let result = run_query("deep_merge({ nested: { x: 1, y: 2 } }, { nested: { y: 99, z: 3 } });");
    assert_eq!(value_to_json(&result), r#"{"nested":{"x":1,"y":99,"z":3}}"#);
}

#[test]
fn test_deep_merge_override_scalar() {
    let result = run_query("deep_merge({ a: 1 }, { a: 100 });");
    assert_eq!(value_to_json(&result), r#"{"a":100}"#);
}

#[test]
fn test_deep_merge_deeply_nested() {
    let result = run_query("deep_merge({ a: { b: { c: 1 } } }, { a: { b: { d: 2 } } });");
    assert_eq!(value_to_json(&result), r#"{"a":{"b":{"c":1,"d":2}}}"#);
}

// ============ CROSS TESTS ============

#[test]
fn test_cross_two_arrays() {
    let result = run_query("cross([1, 2], [3, 4]);");
    assert_eq!(value_to_json(&result), "[[1,3],[1,4],[2,3],[2,4]]");
}

#[test]
fn test_cross_three_arrays() {
    let result = run_query(r#"cross([1], ["a", "b"], [true]);"#);
    assert_eq!(value_to_json(&result), r#"[[1,"a",true],[1,"b",true]]"#);
}

#[test]
fn test_cross_empty() {
    let result = run_query("cross([1, 2], []);");
    assert_eq!(value_to_json(&result), "[]");
}

#[test]
fn test_cross_single_array() {
    let result = run_query("cross([1, 2, 3]);");
    assert_eq!(value_to_json(&result), "[[1],[2],[3]]");
}

// ============ SET_PATH TESTS ============

#[test]
fn test_set_path_basic() {
    let result = run_query(r#"set_path({ a: 1 }, "b", 2);"#);
    assert_eq!(value_to_json(&result), r#"{"a":1,"b":2}"#);
}

#[test]
fn test_set_path_nested() {
    let result = run_query(r#"set_path({}, "a.b.c", 42);"#);
    assert_eq!(value_to_json(&result), r#"{"a":{"b":{"c":42}}}"#);
}

#[test]
fn test_set_path_override() {
    let result = run_query(r#"set_path({ a: { b: 1 } }, "a.b", 99);"#);
    assert_eq!(value_to_json(&result), r#"{"a":{"b":99}}"#);
}

#[test]
fn test_set_path_add_to_existing() {
    let result = run_query(r#"set_path({ a: { x: 1 } }, "a.y", 2);"#);
    assert_eq!(value_to_json(&result), r#"{"a":{"x":1,"y":2}}"#);
}

// ============ CLONE TESTS ============

#[test]
fn test_clone_basic() {
    let result = run_query("let a = { x: 1 }; let b = clone(a); b.x = 99; a.x;");
    assert_eq!(value_to_json(&result), "1");
}

#[test]
fn test_clone_array() {
    let result = run_query("let a = [1, 2, 3]; let b = clone(a); push(b, 4); a.length;");
    assert_eq!(value_to_json(&result), "3");
}

// ============ SPREAD TESTS ============

#[test]
fn test_object_spread_basic() {
    let result = run_query("let base = { a: 1, b: 2 }; ({ ...base, c: 3 });");
    assert_eq!(value_to_json(&result), r#"{"a":1,"b":2,"c":3}"#);
}

#[test]
fn test_object_spread_override() {
    let result = run_query("let base = { a: 1, b: 2 }; ({ ...base, a: 100 });");
    assert_eq!(value_to_json(&result), r#"{"a":100,"b":2}"#);
}

#[test]
fn test_object_spread_multiple() {
    let result = run_query("let a = { x: 1 }; let b = { y: 2 }; ({ ...a, ...b, z: 3 });");
    assert_eq!(value_to_json(&result), r#"{"x":1,"y":2,"z":3}"#);
}

#[test]
fn test_array_spread_basic() {
    let result = run_query("let arr = [2, 3]; [1, ...arr, 4];");
    assert_eq!(value_to_json(&result), "[1,2,3,4]");
}

#[test]
fn test_array_spread_at_start() {
    let result = run_query("let arr = [1, 2]; [...arr, 3, 4];");
    assert_eq!(value_to_json(&result), "[1,2,3,4]");
}

#[test]
fn test_array_spread_at_end() {
    let result = run_query("let arr = [3, 4]; [1, 2, ...arr];");
    assert_eq!(value_to_json(&result), "[1,2,3,4]");
}

#[test]
fn test_array_spread_multiple() {
    let result = run_query("let a = [1, 2]; let b = [3, 4]; [...a, ...b];");
    assert_eq!(value_to_json(&result), "[1,2,3,4]");
}

// ============ COMBINED FEATURE TESTS ============

#[test]
fn test_replicate_with_deep_merge() {
    let result = run_query(r#"let base = { type: "user" }; replicate(3, i => deep_merge(base, { id: i }));"#);
    assert_eq!(value_to_json(&result), r#"[{"type":"user","id":0},{"type":"user","id":1},{"type":"user","id":2}]"#);
}

#[test]
fn test_cross_with_enumerate() {
    let result = run_query("cross([1, 2], [3, 4, 5]).length;");
    assert_eq!(value_to_json(&result), "6");
}

#[test]
fn test_spread_with_replicate() {
    let result = run_query("let extra = replicate(2, i => i + 10); [1, 2, ...extra, 100];");
    assert_eq!(value_to_json(&result), "[1,2,10,11,100]");
}
