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
        stdin.write_all(b"{\"stdin\":\"ignored\"}").unwrap();
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
