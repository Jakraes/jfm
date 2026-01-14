use std::fs;
use std::path::Path;
use jfm::interpreter::parse_and_run;
use jfm::format::parse_json;
use jfm::Value;
use jfm::format::json_to_value;

#[test]
fn run_fixtures() {
    let log_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("fixture_log.txt");
    fs::write(&log_path, "Fixture Run Log\n===============\n").ok();
    
    eprintln!("Fixture Discovery Starting...");
    let fixtures_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join("fixtures");
    if !fixtures_dir.exists() {
        eprintln!("Fixtures directory not found at {:?}", fixtures_dir);
        return;
    }

    let mut count = 0;
    run_fixtures_in_dir(&fixtures_dir, &mut count, &log_path);
    eprintln!("Fixture run complete. Processed {} tests.", count);
}

fn run_fixtures_in_dir(dir: &Path, count: &mut usize, log_path: &Path) {
    let entries = fs::read_dir(dir).expect("Failed to read fixtures directory");

    for entry in entries {
        let entry = entry.expect("Failed to read entry");
        let path = entry.path();

        if path.is_dir() {
            let input_path = path.join("input.json");
            let query_path = path.join("query.jfm");
            let expected_path = path.join("expected.json");

            if input_path.exists() && query_path.exists() && expected_path.exists() {
                *count += 1;
                run_test_case(&path, &input_path, &query_path, &expected_path, log_path);
            } else {
                run_fixtures_in_dir(&path, count, log_path);
            }
        }
    }
}



fn run_test_case(test_dir: &Path, input_path: &Path, query_path: &Path, expected_path: &Path, log_path: &Path) {
    let test_name = test_dir.file_name().unwrap().to_string_lossy();
    
    let input_json = fs::read_to_string(input_path).expect("Failed to read input.json");
    let query_str = fs::read_to_string(query_path).expect("Failed to read query.jfm");
    let expected_json = fs::read_to_string(expected_path).expect("Failed to read expected.json");

    let input_val = if input_json.trim().is_empty() || input_json.trim() == "null" {
        Value::Null
    } else {
        match parse_json(&input_json) {
            Ok(serde_val) => json_to_value(serde_val),
            Err(e) => panic!("Failed to parse input.json in {}: {}", test_name, e),
        }
    };

    let expected_val = match parse_json(&expected_json) {
        Ok(serde_val) => json_to_value(serde_val),
        Err(e) => panic!("Failed to parse expected.json in {}: {}", test_name, e),
    };

    let actual_val = match parse_and_run(&query_str, input_val) {
        Ok(Some(val)) => val,
        Ok(None) => Value::Null,
        Err(e) => {
            let err_msg = format!("INTERPRETER ERROR in {}: {}\n", test_name, e);
            append_to_log(log_path, &err_msg);
            panic!("Test {} failed with interpreter error: {}", test_name, e);
        }
    };

    let actual_str = jfm::interpreter::value_utils::value_to_display(&actual_val);
    append_to_log(log_path, &format!("DEBUG: {} actual result: {}\n", test_name, actual_str));
    
    let expected_str = jfm::interpreter::value_utils::value_to_display(&expected_val);

    if !compare_values(&actual_val, &expected_val) {
        let mut msg = format!("FIXTURE TEST FAILED: {}\n", test_name);
        msg.push_str(&format!("Query: {}\n", query_str.trim()));
        msg.push_str(&format!("Expected: {}\n", expected_str));
        msg.push_str(&format!("Actual:   {}\n", actual_str));
        msg.push_str("--------------------------------------------------\n");
        append_to_log(log_path, &msg);
        panic!("Fixture test {} failed. See fixture_log.txt for details.", test_name);
    } else {
        append_to_log(log_path, &format!("Fixture test PASSED: {}\n", test_name));
    }
}


fn append_to_log(path: &Path, msg: &str) {
    use std::io::Write;
    let mut file = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(path)
        .unwrap();
    file.write_all(msg.as_bytes()).unwrap();
    file.flush().unwrap();
}


fn compare_values(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Number(a, _), Value::Number(b, _)) => (a - b).abs() < f64::EPSILON,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Array(arr_a), Value::Array(arr_b)) => {
            let arr_a_ref = arr_a.borrow();
            let arr_b_ref = arr_b.borrow();
            if arr_a_ref.len() != arr_b_ref.len() {
                return false;
            }
            arr_a_ref.iter().zip(arr_b_ref.iter()).all(|(va, vb)| compare_values(va, vb))
        }
        (Value::Object(obj_a), Value::Object(obj_b)) => {
            let obj_a_ref = obj_a.borrow();
            let obj_b_ref = obj_b.borrow();
            if obj_a_ref.len() != obj_b_ref.len() {
                return false;
            }
            obj_a_ref.iter().all(|(k, va)| {
                if let Some(vb) = obj_b_ref.get(k) {
                    compare_values(va, vb)
                } else {
                    false
                }
            })
        }
        _ => false,
    }
}

