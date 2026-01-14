use crate::Value;

pub fn value_to_json_string(value: &Value, compact: bool) -> String {
    format_json(value, if compact { None } else { Some(0) })
}

pub fn escape_json_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

pub fn format_json(value: &Value, indent: Option<usize>) -> String {
    const INDENT_SIZE: usize = 2;

    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Number(numeric_value, is_float) => {
            if *is_float {
                let formatted = numeric_value.to_string();
                if formatted.contains('.') || formatted.contains('e') || formatted.contains('E') {
                    formatted
                } else {
                    format!("{}.0", numeric_value)
                }
            } else {
                format!("{:.0}", numeric_value)
            }
        }
        Value::String(s) => format!("\"{}\"", escape_json_string(s)),
        Value::Function(_) => "\"<function>\"".to_string(),
        Value::Module(m) => format!("\"<module:{}>\"" , m.name),
        Value::Array(array) => {
            let items = array.borrow();
            if items.is_empty() {
                return "[]".to_string();
            }
            match indent {
                None => {
                    let elements: Vec<String> =
                        items.iter().map(|item| format_json(item, None)).collect();
                    format!("[{}]", elements.join(","))
                }
                Some(level) => {
                    let indent_str = " ".repeat(level * INDENT_SIZE);
                    let next_indent = " ".repeat((level + 1) * INDENT_SIZE);
                    let elements: Vec<String> = items
                        .iter()
                        .map(|item| format!("{}{}", next_indent, format_json(item, Some(level + 1))))
                        .collect();
                    format!("[\n{}\n{}]", elements.join(",\n"), indent_str)
                }
            }
        }
        Value::Object(object) => {
            let map = object.borrow();
            if map.is_empty() {
                return "{}".to_string();
            }
            match indent {
                None => {
                    let fields: Vec<String> = map
                        .iter()
                        .map(|(k, v)| format!("\"{}\":{}", k, format_json(v, None)))
                        .collect();
                    format!("{{{}}}", fields.join(","))
                }
                Some(level) => {
                    let indent_str = " ".repeat(level * INDENT_SIZE);
                    let next_indent = " ".repeat((level + 1) * INDENT_SIZE);
                    let fields: Vec<String> = map
                        .iter()
                        .map(|(k, v)| {
                            format!(
                                "{}\"{}\": {}",
                                next_indent,
                                k,
                                format_json(v, Some(level + 1))
                            )
                        })
                        .collect();
                    format!("{{\n{}\n{}}}", fields.join(",\n"), indent_str)
                }
            }
        }
    }
}
