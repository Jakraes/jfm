use jfm::interpreter;
use jfm::json;
use std::cell::RefCell;
use clap::Parser;
use std::path::{Path, PathBuf};
use std::io::{self, Write};

#[derive(Parser, Debug)]
#[command(name = "jfm")]
#[command(about = "JSON query language interpreter", long_about = None)]
struct Args {
    #[arg(value_name = "JSON")]
    json: Option<String>,

    #[arg(short, long, value_name = "FILE", conflicts_with = "json")]
    file: Option<PathBuf>,

    #[arg(short, long, value_name = "QUERY")]
    query: Option<String>,

    #[arg(long = "query-file", value_name = "PATH", conflicts_with = "query")]
    query_file: Option<PathBuf>,

    #[arg(short, long, value_name = "OUTPUT_FILE")]
    out: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let json_str = if let Some(file) = args.file {
        read_file(&file).unwrap_or_else(|e| {
            eprintln!("{}", e);
            std::process::exit(1);
        })
    } else if let Some(json) = args.json {
        json
    } else {
        eprintln!("Error: Must provide either --file or JSON string");
        std::process::exit(1);
    };

    let root_value = match json::parse_json(&json_str) {
        Ok(val) => convert_json_to_internal(val),
        Err(e) => {
            eprintln!("JSON parse error: {}", e);
            std::process::exit(1);
        }
    };

    if args.query.is_none() && args.query_file.is_none() {
        run_interactive_mode(root_value, &args.out);
    } else {
        let query_str = if let Some(query) = args.query {
            query
        } else if let Some(query_file) = args.query_file {
            read_file(&query_file).unwrap_or_else(|e| {
                eprintln!("{}", e);
                std::process::exit(1);
            })
        } else {
            unreachable!();
        };

        execute_query(&query_str, &root_value, &args.out, false);
    }
}

fn run_interactive_mode(root_value: jfm::lexer::Value, out_file: &Option<PathBuf>) {
    println!("jfm Interactive Query Editor");
    println!("Type your query (multi-line supported). Exit with Ctrl+D (Ctrl+Z on Windows) or type 'exit' on a new line.");
    println!();

    let mut query = String::new();

    loop {
        print!("jfm> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(0) => break,
            Ok(_) => {
                let trimmed = line.trim();
                
                if trimmed == "exit" || trimmed == "quit" {
                    break;
                }

                query.push_str(&line);
            }
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }
    }

    let trimmed_query = query.trim();
    if trimmed_query.is_empty() {
        eprintln!("No query entered.");
        std::process::exit(1);
    }

    execute_query(trimmed_query, &root_value, out_file, false);
}

fn execute_query(query_str: &str, root_value: &jfm::lexer::Value, out_file: &Option<PathBuf>, is_interactive: bool) {
    let result = match interpreter::parse_and_run(query_str, root_value.clone()) {
        Ok(Some(result)) => {
            format!("{}\n", value_to_json_string(&result))
        }
        Ok(None) => {
            "{}\n".to_string()
        }
        Err(e) => {
            eprintln!("Query error: {}", e);
            return;
        }
    };

    if out_file.is_none() || is_interactive {
        print!("{}", result);
        io::stdout().flush().unwrap();
    }

    if let Some(out_path) = out_file {
        use std::fs::OpenOptions;
        use std::io::Write;
        
        let file_result = if is_interactive {
            OpenOptions::new()
                .create(true)
                .append(true)
                .open(out_path)
        } else {
            OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(out_path)
        };

        match file_result {
            Ok(mut file) => {
                if let Err(e) = file.write_all(result.as_bytes()) {
                    eprintln!("Error writing to output file: {}", e);
                }
            }
            Err(e) => {
                eprintln!("Error opening output file: {}", e);
            }
        }
    }
}

fn read_file(path: &Path) -> Result<String, String> {
    std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path.display(), e))
}

fn convert_json_to_internal(json_val: serde_json::Value) -> jfm::lexer::Value {
    use jfm::lexer::Value;
    use std::rc::Rc;

    match json_val {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Bool(b),
        serde_json::Value::Number(n) => {
            let val = n.as_f64().unwrap_or(0.0);
            Value::Number(val)
        }
        serde_json::Value::String(s) => Value::String(Rc::from(s.as_str())),
        serde_json::Value::Array(arr) => {
            let items: Vec<Value> = arr
                .into_iter()
                .map(convert_json_to_internal)
                .collect();
            Value::Array(Rc::new(RefCell::new(items)))
        }
        serde_json::Value::Object(obj) => {
            let mut map = indexmap::IndexMap::new();
            for (k, v) in obj {
                map.insert(k, convert_json_to_internal(v));
            }
            Value::Object(Rc::new(RefCell::new(map)))
        }
    }
}

fn value_to_json_string(val: &jfm::lexer::Value) -> String {
    value_to_json_string_with_indent(val, 0)
}

fn value_to_json_string_with_indent(val: &jfm::lexer::Value, indent: usize) -> String {
    use jfm::lexer::Value;
    const INDENT_SIZE: usize = 2;
    let indent_str = " ".repeat(indent * INDENT_SIZE);
    let next_indent_str = " ".repeat((indent + 1) * INDENT_SIZE);

    match val {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Number(n) => {
            if n.fract() == 0.0 {
                format!("{:.0}", n)
            } else {
                n.to_string()
            }
        }
        Value::String(s) => format!("\"{}\"", s),
        Value::Array(arr) => {
            let items = arr.borrow();
            if items.is_empty() {
                return "[]".to_string();
            }
            
            let elements: Vec<String> = items
                .iter()
                .map(|item| {
                    let formatted = value_to_json_string_with_indent(item, indent + 1);
                    formatted
                        .lines()
                        .map(|line| format!("{}{}", next_indent_str, line))
                        .collect::<Vec<_>>()
                        .join("\n")
                })
                .collect();
            
            format!("[\n{}\n{}]", elements.join(",\n"), indent_str)
        }
        Value::Object(obj) => {
            let map = obj.borrow();
            if map.is_empty() {
                return "{}".to_string();
            }
            
            let fields: Vec<String> = map
                .iter()
                .map(|(k, v)| {
                    let value_str = value_to_json_string_with_indent(v, indent + 1);
                    
                    if value_str.contains('\n') {
                        let mut lines: Vec<&str> = value_str.lines().collect();
                        let first_line = lines.remove(0);
                        let result = format!("\"{}\": {}", k, first_line);
                        if lines.is_empty() {
                            result
                        } else {
                            format!("{}\n{}", result, lines.join("\n"))
                        }
                    } else {
                        format!("\"{}\": {}", k, value_str)
                    }
                })
                .collect();
            
            let formatted_fields: Vec<String> = fields
                .iter()
                .map(|field| {
                    field
                        .lines()
                        .map(|line| format!("{}{}", next_indent_str, line))
                        .collect::<Vec<_>>()
                        .join("\n")
                })
                .collect();
            
            format!("{{\n{}\n{}}}", formatted_fields.join(",\n"), indent_str)
        }
    }
}
