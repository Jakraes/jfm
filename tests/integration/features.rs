use jfm::Value;
use jfm::interpreter::parse_and_run;
use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;

// --- CONTROL FLOW TESTS (from control_flow.rs) ---

#[test]
fn test_loops_and_conditionals() {
    // While loop
    let source = "let x = 0; while x < 5 { x = x + 1; } x;";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 5.0);

    // For loop with break
    let source = "let s = 0; for x in [1,2,3,4,5] { if x > 3 { break; } s = s + x; } s;";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 6.0);
    
    // Continue
    let source = "let s = 0; for x in [1,2,3] { if x == 2 { continue; } s = s + x; } s;";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 4.0);
}

#[test]
fn test_user_functions_and_lambdas() {
    // Lambda
    let source = "let f = (x, y) => x + y; f(3, 4);";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 7.0);

    // Named function
    let source = "fn fact(n) { if n <= 1 { 1 } else { n * fact(n-1) } } fact(5);";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 120.0);
}

// --- TEMPLATE LITERAL TESTS (from templates.rs) ---

#[test]
fn test_template_literals_basic() {
    assert_eq!(parse_and_run("`hello`;", Value::Null).unwrap().unwrap(), Value::String(Rc::from("hello")));
    assert_eq!(parse_and_run("let n = \"A\"; `H ${n}`;", Value::Null).unwrap().unwrap(), Value::String(Rc::from("H A")));
    assert_eq!(parse_and_run("`1+2=${1+2}`;", Value::Null).unwrap().unwrap(), Value::String(Rc::from("1+2=3")));
}

#[test]
fn test_template_literals_escapes() {
    assert_eq!(parse_and_run(r#"`\${x}`;"#, Value::Null).unwrap().unwrap(), Value::String(Rc::from("${x}")));
    assert_eq!(parse_and_run(r#"`a\nb`;"#, Value::Null).unwrap().unwrap(), Value::String(Rc::from("a\nb")));
}

// --- V2 FEATURE TESTS (from v2_features.rs) ---

#[test]
fn test_v2_pipes_and_projections() {
    // Smart pipe map
    let source = "[1,2,3] | @ * 2;";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_array().unwrap().len(), 3);
    assert_eq!(result.as_array().unwrap()[0], Value::Number(2.0, false));

    // Projection shorthand
    let source = "let u = {a:1, b:2}; u | { .a };";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_object().unwrap().len(), 1);
    assert!(result.as_object().unwrap().contains_key("a"));
}

#[test]
fn test_v2_deep_extraction_and_defaults() {
    // Deep extraction
    let source = "let d = {a: {v: 42}}; d..v;";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap(), Value::Number(42.0, false));

    // Default params
    let source = "fn g(n, m = 10) { n + m } g(5);";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 15.0);
}

#[test]
fn test_v2_match_and_negative_index() {
    // Match range
    let source = "match 25 { 18..30 => \"y\", _ => \"o\" };";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap(), Value::String(Rc::from("y")));

    // Negative index
    let source = "[1,2,3][-1];";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 3.0);
    
    // Negative index in pipe
    let source = "[1,2,3] | [-1];";
    let result = parse_and_run(source, Value::Null).unwrap().unwrap();
    assert_eq!(result.as_array().unwrap()[0], Value::Number(3.0, false));
}

#[test]
fn test_v2_comments() {
    let source = "# hash\nlet x = 1; /* ml */ x;";
    assert_eq!(parse_and_run(source, Value::Null).unwrap().unwrap().as_number().unwrap(), 1.0);
}
