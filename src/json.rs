use serde_json::Value;

pub fn parse_json(json_str: &str) -> Result<Value, String> {
    serde_json::from_str(json_str).map_err(|e| e.to_string())
}

pub fn parse_json_file(file_path: &str) -> Result<Value, String> {
    let json_str = std::fs::read_to_string(file_path).map_err(|e| e.to_string())?;
    parse_json(&json_str)
}
