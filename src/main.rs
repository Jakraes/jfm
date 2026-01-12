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

    // Check if we should enter interactive mode
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
            Ok(0) => {
                // EOF (Ctrl+D on Unix, Ctrl+Z on Windows)
                break;
            }
            Ok(_) => {
                let trimmed = line.trim();
                
                // Handle exit commands
                if trimmed == "exit" || trimmed == "quit" {
                    break;
                }

                // Add line to query
                query.push_str(&line);
            }
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }
    }

    // Execute the query that was built
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

    // Write to stdout (always in interactive, or when no output file specified in non-interactive)
    if out_file.is_none() || is_interactive {
        print!("{}", result);
        io::stdout().flush().unwrap();
    }

    // Write to file if specified
    if let Some(out_path) = out_file {
        use std::fs::OpenOptions;
        use std::io::Write;
        
        let file_result = if is_interactive {
            // In interactive mode, append to file
            OpenOptions::new()
                .create(true)
                .append(true)
                .open(out_path)
        } else {
            // In non-interactive mode, write (overwrite) to file
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
    use jfm::lexer::Value;

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
            let elements: Vec<String> = items.iter().map(value_to_json_string).collect();
            format!("[{}]", elements.join(", "))
        }
        Value::Object(obj) => {
            let map = obj.borrow();
            let fields: Vec<String> = map
                .iter()
                .map(|(k, v)| format!("\"{}\": {}", k, value_to_json_string(v)))
                .collect();
            format!("{{{}}}", fields.join(", "))
        }
    }
}
