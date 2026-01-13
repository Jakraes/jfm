use std::io::Write;
use std::process::{Command, Stdio};

fn get_jfm_binary() -> Command {
    Command::new(env!("CARGO_BIN_EXE_jfm"))
}

#[test]
fn test_version_flag() {
    let output = get_jfm_binary()
        .arg("--version")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Version flag should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("jfm"), "Version output should contain 'jfm'");
    assert!(stdout.contains(env!("CARGO_PKG_VERSION")), "Version output should contain version number");
}

#[test]
fn test_version_short_flag() {
    let output = get_jfm_binary()
        .arg("-V")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Version short flag should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("jfm"), "Version output should contain 'jfm'");
}

#[test]
fn test_compact_output() {
    let json = r#"{"users":[{"name":"Alice","age":30}]}"#;
    let query = "root;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("--compact")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Compact output should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let output_trimmed = stdout.trim();
    
    assert!(!output_trimmed.contains('\n') || output_trimmed.lines().count() <= 2, 
            "Compact output should be minified (no extra newlines)");
    assert!(output_trimmed.starts_with("{"), "Output should be JSON object");
}

#[test]
fn test_pretty_output() {
    let json = r#"{"users":[{"name":"Alice","age":30}]}"#;
    let query = "root;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Pretty output should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    
    assert!(stdout.contains('\n'), "Pretty output should contain newlines");
}

#[test]
fn test_stdin_input() {
    let json = r#"{"name":"test","value":42}"#;
    let query = "root.name;";
    
    let mut child = get_jfm_binary()
        .arg("--query")
        .arg(query)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(json.as_bytes()).unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(output.status.success(), "Stdin input should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.trim() == "\"test\"", "Should extract name from stdin input");
}

#[test]
fn test_stdin_with_compact() {
    let json = r#"{"users":[{"name":"Alice"}]}"#;
    let query = "root;";
    
    let mut child = get_jfm_binary()
        .arg("--query")
        .arg(query)
        .arg("--compact")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(json.as_bytes()).unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(output.status.success(), "Stdin with compact should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let output_trimmed = stdout.trim();
    assert!(!output_trimmed.contains('\n') || output_trimmed.lines().count() <= 2,
            "Output should be compact");
}

#[test]
fn test_verbose_mode() {
    let json = r#"{"name":"test"}"#;
    let query = "root.name;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("--verbose")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Verbose mode should succeed");
    let stderr = String::from_utf8(output.stderr).unwrap();
    
    assert!(stderr.contains("debug") || stderr.contains("jfm"), 
            "Verbose mode should output debug messages to stderr");
}

#[test]
fn test_verbose_with_stdin() {
    let json = r#"{"value":42}"#;
    let query = "root;";
    
    let mut child = get_jfm_binary()
        .arg("--query")
        .arg(query)
        .arg("--verbose")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(json.as_bytes()).unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(output.status.success(), "Verbose with stdin should succeed");
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains("debug") || stderr.contains("jfm"),
            "Should output verbose messages");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.trim().starts_with("{"), "Should still output JSON to stdout");
}

#[test]
fn test_color_flag_always() {
    let json = r#"{"name":"test"}"#;
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--color=always")
        .arg("--query")
        .arg("invalid.query.field.missing")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(!output.status.success(), "Invalid query should fail");
}

#[test]
fn test_color_flag_never() {
    let json = r#"{"name":"test"}"#;
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--color=never")
        .arg("--query")
        .arg("invalid.query.field.missing")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(!output.status.success(), "Invalid query should fail");
}

#[test]
fn test_all_flags_combined() {
    let json = r#"{"users":[{"name":"Alice","age":30}]}"#;
    let query = "root.users[0].name;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("--compact")
        .arg("--verbose")
        .arg("--color=never")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "All flags combined should work");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.trim() == "\"Alice\"", "Should extract name correctly");
}

#[test]
fn test_file_and_stdin_conflict() {
    use std::fs;
    use std::io::Write;
    
    let test_file = "test_cli_input.json";
    fs::File::create(test_file)
        .unwrap()
        .write_all(b"{\"file\":\"data\"}")
        .unwrap();
    
    let json = r#"{"stdin":"data"}"#;
    
    let mut child = get_jfm_binary()
        .arg(json)
        .arg("--file")
        .arg(test_file)
        .arg("--query")
        .arg("root;")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        // Ignore broken pipe error - process may exit early due to argument conflict
        let _ = stdin.write_all(b"{\"stdin\":\"ignored\"}");
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    let _ = fs::remove_file(test_file);
    
    assert!(!output.status.success() || output.stderr.len() > 0,
            "File and JSON argument should conflict");
}

#[test]
fn test_empty_stdin() {
    let mut child = get_jfm_binary()
        .arg("--query")
        .arg("root;")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(b"").unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(!output.status.success(), "Empty stdin should fail");
}

#[test]
fn test_shell_completion_bash() {
    let output = get_jfm_binary()
        .arg("complete")
        .arg("bash")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Bash completion should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("complete") || stdout.contains("_jfm") || stdout.len() > 0,
            "Completion output should be generated");
}

#[test]
fn test_shell_completion_zsh() {
    let output = get_jfm_binary()
        .arg("complete")
        .arg("zsh")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Zsh completion should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.len() > 0, "Completion output should be generated");
}

#[test]
fn test_shell_completion_fish() {
    let output = get_jfm_binary()
        .arg("complete")
        .arg("fish")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Fish completion should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.len() > 0, "Completion output should be generated");
}

#[test]
fn test_shell_completion_powershell() {
    let output = get_jfm_binary()
        .arg("complete")
        .arg("powershell")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "PowerShell completion should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.len() > 0, "Completion output should be generated");
}

#[test]
fn test_query_file() {
    use std::fs;
    use std::io::Write;
    
    let query_file = "test_cli_query.jfm";
    fs::File::create(query_file)
        .unwrap()
        .write_all(b"root.name;")
        .unwrap();
    
    let json = r#"{"name":"test"}"#;
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query-file")
        .arg(query_file)
        .output()
        .expect("Failed to execute jfm");
    
    let _ = fs::remove_file(query_file);
    
    assert!(output.status.success(), "Query file should work");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.trim() == "\"test\"", "Should execute query from file");
}

#[test]
fn test_output_file() {
    use std::fs;
    
    let output_file = "test_cli_output.json";
    let json = r#"{"result":"success"}"#;
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg("root;")
        .arg("--out")
        .arg(output_file)
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Output file should work");
    
    let contents = fs::read_to_string(output_file)
        .expect("Failed to read output file");
    
    assert!(contents.contains("result"), "Output file should contain result");
    
    let _ = fs::remove_file(output_file);
}

#[test]
fn test_compact_output_preserves_validity() {
    let json = r#"{"a":1,"b":[{"c":2,"d":3}]}"#;
    let query = "root;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("--compact")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Compact output should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let trimmed = stdout.trim();
    
    let parsed: Result<serde_json::Value, _> = serde_json::from_str(trimmed);
    assert!(parsed.is_ok(), "Compact output should be valid JSON");
}

// ============================================
// --limit flag tests
// ============================================

#[test]
fn test_limit_flag_basic() {
    let json = r#"[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"#;
    let query = "root;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("--limit")
        .arg("3")
        .arg("--compact")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Limit flag should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let trimmed = stdout.trim();
    
    let parsed: serde_json::Value = serde_json::from_str(trimmed)
        .expect("Output should be valid JSON");
    let arr = parsed.as_array().expect("Output should be array");
    assert_eq!(arr.len(), 3, "Should have exactly 3 elements with --limit 3");
    assert_eq!(arr[0], 1);
    assert_eq!(arr[1], 2);
    assert_eq!(arr[2], 3);
}

#[test]
fn test_limit_flag_short() {
    let json = r#"[1, 2, 3, 4, 5]"#;
    let query = "root;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("-n")
        .arg("2")
        .arg("--compact")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Short limit flag -n should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let trimmed = stdout.trim();
    
    let parsed: serde_json::Value = serde_json::from_str(trimmed)
        .expect("Output should be valid JSON");
    let arr = parsed.as_array().expect("Output should be array");
    assert_eq!(arr.len(), 2, "Should have exactly 2 elements with -n 2");
}

#[test]
fn test_limit_zero() {
    let json = r#"[1, 2, 3]"#;
    let query = "root;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("--limit")
        .arg("0")
        .arg("--compact")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Limit 0 should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let trimmed = stdout.trim();
    
    let parsed: serde_json::Value = serde_json::from_str(trimmed)
        .expect("Output should be valid JSON");
    let arr = parsed.as_array().expect("Output should be array");
    assert_eq!(arr.len(), 0, "Should have 0 elements with --limit 0");
}

#[test]
fn test_limit_larger_than_array() {
    let json = r#"[1, 2, 3]"#;
    let query = "root;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("--limit")
        .arg("100")
        .arg("--compact")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Limit larger than array should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let trimmed = stdout.trim();
    
    let parsed: serde_json::Value = serde_json::from_str(trimmed)
        .expect("Output should be valid JSON");
    let arr = parsed.as_array().expect("Output should be array");
    assert_eq!(arr.len(), 3, "Should have all 3 elements when limit > array length");
}

#[test]
fn test_limit_on_non_array() {
    let json = r#"{"name": "test", "value": 42}"#;
    let query = "root;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("--limit")
        .arg("5")
        .arg("--compact")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Limit on non-array should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let trimmed = stdout.trim();
    
    let parsed: serde_json::Value = serde_json::from_str(trimmed)
        .expect("Output should be valid JSON");
    assert!(parsed.is_object(), "Non-array should be returned unchanged");
    assert_eq!(parsed["name"], "test");
    assert_eq!(parsed["value"], 42);
}

#[test]
fn test_limit_with_filter_query() {
    let json = r#"{"users": [{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}, {"name": "Charlie", "age": 35}, {"name": "Diana", "age": 28}]}"#;
    let query = "root.users | .age > 25;";
    
    let output = get_jfm_binary()
        .arg(json)
        .arg("--query")
        .arg(query)
        .arg("--limit")
        .arg("2")
        .arg("--compact")
        .output()
        .expect("Failed to execute jfm");
    
    assert!(output.status.success(), "Limit with filter should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let trimmed = stdout.trim();
    
    let parsed: serde_json::Value = serde_json::from_str(trimmed)
        .expect("Output should be valid JSON");
    let arr = parsed.as_array().expect("Output should be array");
    assert_eq!(arr.len(), 2, "Should have 2 filtered results with --limit 2");
}

// ============================================
// --stream flag tests
// ============================================

#[test]
fn test_stream_flag_ndjson() {
    // Newline-delimited JSON (NDJSON)
    let ndjson = "{\"name\":\"Alice\",\"value\":1}\n{\"name\":\"Bob\",\"value\":2}\n{\"name\":\"Charlie\",\"value\":3}";
    let query = "root.name;";
    
    let mut child = get_jfm_binary()
        .arg("--query")
        .arg(query)
        .arg("--stream")
        .arg("--compact")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(ndjson.as_bytes()).unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(output.status.success(), "Stream flag with NDJSON should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let lines: Vec<&str> = stdout.lines().collect();
    
    assert_eq!(lines.len(), 3, "Should have 3 lines of output");
    assert_eq!(lines[0].trim(), "\"Alice\"");
    assert_eq!(lines[1].trim(), "\"Bob\"");
    assert_eq!(lines[2].trim(), "\"Charlie\"");
}

#[test]
fn test_stream_flag_json_array() {
    let json = r#"[{"id": 1}, {"id": 2}, {"id": 3}]"#;
    let query = "root.id;";
    
    let mut child = get_jfm_binary()
        .arg("--query")
        .arg(query)
        .arg("--stream")
        .arg("--compact")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(json.as_bytes()).unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(output.status.success(), "Stream flag with JSON array should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let lines: Vec<&str> = stdout.lines().collect();
    
    assert_eq!(lines.len(), 3, "Should have 3 lines of output");
    assert_eq!(lines[0].trim(), "1");
    assert_eq!(lines[1].trim(), "2");
    assert_eq!(lines[2].trim(), "3");
}

#[test]
fn test_stream_with_limit() {
    let ndjson = "{\"v\":1}\n{\"v\":2}\n{\"v\":3}\n{\"v\":4}\n{\"v\":5}";
    let query = "root.v;";
    
    let mut child = get_jfm_binary()
        .arg("--query")
        .arg(query)
        .arg("--stream")
        .arg("--limit")
        .arg("3")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(ndjson.as_bytes()).unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(output.status.success(), "Stream with limit should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let lines: Vec<&str> = stdout.lines().collect();
    
    assert_eq!(lines.len(), 3, "Should have exactly 3 lines due to --limit 3");
    assert_eq!(lines[0].trim(), "1");
    assert_eq!(lines[1].trim(), "2");
    assert_eq!(lines[2].trim(), "3");
}

#[test]
fn test_stream_requires_query() {
    let json = r#"{"test": 1}"#;
    
    let mut child = get_jfm_binary()
        .arg("--stream")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(json.as_bytes()).unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(!output.status.success(), "Stream without query should fail");
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains("query") || stderr.contains("--query"), 
            "Error should mention query requirement");
}

#[test]
fn test_stream_empty_lines_ndjson() {
    // NDJSON with empty lines should be handled gracefully
    let ndjson = "{\"x\":1}\n\n{\"x\":2}\n\n\n{\"x\":3}";
    let query = "root.x;";
    
    let mut child = get_jfm_binary()
        .arg("--query")
        .arg(query)
        .arg("--stream")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(ndjson.as_bytes()).unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(output.status.success(), "Stream with empty lines should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let lines: Vec<&str> = stdout.lines().collect();
    
    assert_eq!(lines.len(), 3, "Should have 3 lines, ignoring empty lines");
}

#[test]
fn test_stream_single_object() {
    // Stream mode with a single object (not array, not NDJSON) should still work
    let json = r#"{"name": "test", "value": 42}"#;
    let query = "root.value;";
    
    let mut child = get_jfm_binary()
        .arg("--query")
        .arg(query)
        .arg("--stream")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn jfm");
    
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(json.as_bytes()).unwrap();
        stdin.flush().unwrap();
    }
    
    let output = child.wait_with_output().expect("Failed to read output");
    
    assert!(output.status.success(), "Stream with single object should succeed");
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert_eq!(stdout.trim(), "42", "Should output the value");
}
