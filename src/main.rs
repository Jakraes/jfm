use jfm::interpreter;
use jfm::json;
use std::cell::RefCell;
use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::{generate, Shell};
use std::path::{Path, PathBuf};
use std::io::{self, Write, Read};
use owo_colors::OwoColorize;

#[derive(Parser, Debug)]
#[command(name = "jfm")]
#[command(version = env!("CARGO_PKG_VERSION"))]
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

    #[arg(long = "color", value_name = "WHEN", default_value = "auto")]
    color: ColorChoice,

    #[arg(long = "compact")]
    compact: bool,

    #[arg(short = 'v', long = "verbose")]
    verbose: bool,

    /// Limit output to N results (for array outputs)
    #[arg(short = 'n', long = "limit", value_name = "N")]
    limit: Option<usize>,

    /// Enable streaming mode for large JSON files (processes line-delimited JSON or large arrays)
    #[arg(long = "stream")]
    stream: bool,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Complete {
        #[arg(value_name = "SHELL")]
        shell: Shell,
    },
}

#[derive(Debug, Clone, Copy)]
enum ColorChoice {
    Auto,
    Always,
    Never,
}

impl std::str::FromStr for ColorChoice {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "auto" => Ok(ColorChoice::Auto),
            "always" => Ok(ColorChoice::Always),
            "never" => Ok(ColorChoice::Never),
            _ => Err(format!("Invalid color choice: {}. Must be 'auto', 'always', or 'never'", s)),
        }
    }
}

struct AppConfig {
    color_enabled: bool,
    compact: bool,
    verbose: bool,
    limit: Option<usize>,
    stream: bool,
}

impl AppConfig {
    fn from_args(args: &Args) -> Self {
        let color_enabled = match args.color {
            ColorChoice::Always => true,
            ColorChoice::Never => false,
            ColorChoice::Auto => atty::is(atty::Stream::Stderr) && atty::is(atty::Stream::Stdout),
        };

        AppConfig {
            color_enabled,
            compact: args.compact,
            verbose: args.verbose,
            limit: args.limit,
            stream: args.stream,
        }
    }
}

fn main() {
    let args = Args::parse();

    if let Some(Commands::Complete { shell }) = args.command {
        generate_completions(shell);
        return;
    }

    let config = AppConfig::from_args(&args);

    verbose_log(&config, "Starting jfm");

    let json_str = match read_json_input(&args, &config) {
        Ok(s) => s,
        Err(e) => {
            error_message(&config, &e);
            std::process::exit(1);
        }
    };

    verbose_log(&config, &format!("Read {} bytes of JSON input", json_str.len()));

    // Handle streaming mode
    if config.stream {
        verbose_log(&config, "Streaming mode enabled");
        
        if args.query.is_none() && args.query_file.is_none() {
            error_message(&config, "Streaming mode requires a query (--query or --query-file)");
            std::process::exit(1);
        }
        
        let query_str = match read_query_input(&args, &config) {
            Ok(s) => s,
            Err(e) => {
                error_message(&config, &e);
                std::process::exit(1);
            }
        };
        
        execute_streaming(&json_str, &query_str, &args.out, &config);
        return;
    }

    let root_value = match json::parse_json(&json_str) {
        Ok(val) => {
            verbose_log(&config, "Successfully parsed JSON");
            convert_json_to_internal(val)
        }
        Err(e) => {
            error_message(&config, &format!("JSON parse error: {}", e));
            std::process::exit(1);
        }
    };

    if args.query.is_none() && args.query_file.is_none() {
        run_interactive_mode(root_value, &args.out, &config);
    } else {
        let query_str = match read_query_input(&args, &config) {
            Ok(s) => s,
            Err(e) => {
                error_message(&config, &e);
                std::process::exit(1);
            }
        };

        verbose_log(&config, &format!("Executing query: {}", query_str));
        execute_query(&query_str, &root_value, &args.out, false, &config);
    }
}

fn read_json_input(args: &Args, config: &AppConfig) -> Result<String, String> {
    if let Some(file) = &args.file {
        verbose_log(config, &format!("Reading JSON from file: {}", file.display()));
        read_file(file)
    } else if let Some(json) = &args.json {
        verbose_log(config, "Reading JSON from command-line argument");
        Ok(json.clone())
    } else {
        verbose_log(config, "Reading JSON from stdin");
        let mut buffer = String::new();
        io::stdin()
            .read_to_string(&mut buffer)
            .map_err(|e| format!("Failed to read from stdin: {}", e))?;
        
        if buffer.trim().is_empty() {
            return Err("No input provided. Must provide --file, JSON string argument, or JSON via stdin".to_string());
        }
        
        Ok(buffer)
    }
}

fn read_query_input(args: &Args, config: &AppConfig) -> Result<String, String> {
    if let Some(query) = &args.query {
        verbose_log(config, "Using query from command-line argument");
        Ok(query.clone())
    } else if let Some(query_file) = &args.query_file {
        verbose_log(config, &format!("Reading query from file: {}", query_file.display()));
        read_file(query_file)
    } else {
        unreachable!("Should have query or query-file if not in interactive mode");
    }
}

fn run_interactive_mode(root_value: jfm::lexer::Value, out_file: &Option<PathBuf>, config: &AppConfig) {
    if !config.verbose {
        println!("jfm Interactive Query Editor");
        println!("Type your query (multi-line supported). Exit with Ctrl+D (Ctrl+Z on Windows) or type 'exit' on a new line.");
        println!();
    } else {
        verbose_log(config, "Entering interactive mode");
    }

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
                error_message(config, &format!("Error reading input: {}", e));
                break;
            }
        }
    }

    let trimmed_query = query.trim();
    if trimmed_query.is_empty() {
        error_message(config, "No query entered.");
        std::process::exit(1);
    }

    verbose_log(config, &format!("Executing interactive query: {}", trimmed_query));
    execute_query(trimmed_query, &root_value, out_file, true, config);
}

fn execute_query(query_str: &str, root_value: &jfm::lexer::Value, out_file: &Option<PathBuf>, is_interactive: bool, config: &AppConfig) {
    verbose_log(config, "Parsing query");
    
    let result = match interpreter::parse_and_run(query_str, root_value.clone()) {
        Ok(Some(result)) => {
            verbose_log(config, "Query executed successfully");
            // Apply limit if specified and result is an array
            let limited_result = apply_limit(&result, config.limit);
            format!("{}\n", value_to_json_string(&limited_result, config.compact))
        }
        Ok(None) => {
            verbose_log(config, "Query returned None, outputting empty object");
            "{}\n".to_string()
        }
        Err(e) => {
            error_message(config, &format!("Query error: {}", e));
            std::process::exit(1);
        }
    };

    if out_file.is_none() || is_interactive {
        print!("{}", result);
        io::stdout().flush().unwrap();
    }

    if let Some(out_path) = out_file {
        verbose_log(config, &format!("Writing output to file: {}", out_path.display()));
        
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
                    error_message(config, &format!("Error writing to output file: {}", e));
                } else {
                    verbose_log(config, "Successfully wrote output to file");
                }
            }
            Err(e) => {
                error_message(config, &format!("Error opening output file: {}", e));
            }
        }
    }
}

/// Apply limit to array results, returning only the first N elements
fn apply_limit(value: &jfm::lexer::Value, limit: Option<usize>) -> jfm::lexer::Value {
    use jfm::lexer::Value;
    
    match (value, limit) {
        (Value::Array(arr), Some(n)) => {
            let items = arr.borrow();
            let limited: Vec<Value> = items.iter().take(n).cloned().collect();
            Value::Array(std::rc::Rc::new(RefCell::new(limited)))
        }
        _ => value.clone(),
    }
}

/// Execute query in streaming mode - processes line-delimited JSON (NDJSON) or large JSON arrays
fn execute_streaming(json_str: &str, query_str: &str, out_file: &Option<PathBuf>, config: &AppConfig) {
    use std::fs::OpenOptions;
    
    let mut output_count = 0usize;
    let limit = config.limit.unwrap_or(usize::MAX);
    
    // Try to detect if it's NDJSON (newline-delimited JSON)
    let is_ndjson = json_str.lines().next().is_some_and(|first_line| {
        let trimmed = first_line.trim();
        !trimmed.is_empty() && !trimmed.starts_with('[')
    });
    
    let mut out_writer: Option<std::fs::File> = if let Some(out_path) = out_file {
        match OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(out_path)
        {
            Ok(f) => Some(f),
            Err(e) => {
                error_message(config, &format!("Error opening output file: {}", e));
                std::process::exit(1);
            }
        }
    } else {
        None
    };
    
    if is_ndjson {
        verbose_log(config, "Processing as NDJSON (newline-delimited JSON)");
        
        for line in json_str.lines() {
            if output_count >= limit {
                verbose_log(config, &format!("Reached limit of {} results", limit));
                break;
            }
            
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            
            match json::parse_json(trimmed) {
                Ok(json_val) => {
                    let root_value = convert_json_to_internal(json_val);
                    match interpreter::parse_and_run(query_str, root_value) {
                        Ok(Some(result)) => {
                            let output = format!("{}\n", value_to_json_string(&result, config.compact));
                            
                            if let Some(ref mut writer) = out_writer {
                                let _ = writer.write_all(output.as_bytes());
                            } else {
                                print!("{}", output);
                            }
                            output_count += 1;
                        }
                        Ok(None) => {}
                        Err(e) => {
                            error_message(config, &format!("Query error on line: {}", e));
                        }
                    }
                }
                Err(e) => {
                    error_message(config, &format!("JSON parse error on line: {}", e));
                }
            }
        }
    } else {
        verbose_log(config, "Processing as JSON array stream");
        
        // Parse as a single JSON and iterate if it's an array
        match json::parse_json(json_str) {
            Ok(serde_json::Value::Array(arr)) => {
                for item in arr {
                    if output_count >= limit {
                        verbose_log(config, &format!("Reached limit of {} results", limit));
                        break;
                    }
                    
                    let root_value = convert_json_to_internal(item);
                    match interpreter::parse_and_run(query_str, root_value) {
                        Ok(Some(result)) => {
                            let output = format!("{}\n", value_to_json_string(&result, config.compact));
                            
                            if let Some(ref mut writer) = out_writer {
                                let _ = writer.write_all(output.as_bytes());
                            } else {
                                print!("{}", output);
                            }
                            output_count += 1;
                        }
                        Ok(None) => {}
                        Err(e) => {
                            error_message(config, &format!("Query error on item: {}", e));
                        }
                    }
                }
            }
            Ok(json_val) => {
                // Not an array, process as single item
                let root_value = convert_json_to_internal(json_val);
                match interpreter::parse_and_run(query_str, root_value) {
                    Ok(Some(result)) => {
                        let limited_result = apply_limit(&result, config.limit);
                        let output = format!("{}\n", value_to_json_string(&limited_result, config.compact));
                        
                        if let Some(ref mut writer) = out_writer {
                            let _ = writer.write_all(output.as_bytes());
                        } else {
                            print!("{}", output);
                        }
                    }
                    Ok(None) => {
                        let output = "{}\n";
                        if let Some(ref mut writer) = out_writer {
                            let _ = writer.write_all(output.as_bytes());
                        } else {
                            print!("{}", output);
                        }
                    }
                    Err(e) => {
                        error_message(config, &format!("Query error: {}", e));
                        std::process::exit(1);
                    }
                }
            }
            Err(e) => {
                error_message(config, &format!("JSON parse error: {}", e));
                std::process::exit(1);
            }
        }
    }
    
    verbose_log(config, &format!("Streaming complete. Processed {} results", output_count));
    io::stdout().flush().unwrap();
}

fn generate_completions(shell: Shell) {
    let mut cmd = Args::command();
    let bin_name = cmd.get_name().to_string();
    generate(shell, &mut cmd, &bin_name, &mut io::stdout());
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

fn value_to_json_string(val: &jfm::lexer::Value, compact: bool) -> String {
    if compact {
        value_to_json_string_compact(val)
    } else {
        value_to_json_string_with_indent(val, 0)
    }
}

fn value_to_json_string_compact(val: &jfm::lexer::Value) -> String {
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
        Value::String(s) => {
            let escaped = s
                .replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('\n', "\\n")
                .replace('\r', "\\r")
                .replace('\t', "\\t");
            format!("\"{}\"", escaped)
        }
        Value::Array(arr) => {
            let items = arr.borrow();
            if items.is_empty() {
                return "[]".to_string();
            }
            
            let elements: Vec<String> = items
                .iter()
                .map(|item| value_to_json_string_compact(item))
                .collect();
            
            format!("[{}]", elements.join(","))
        }
        Value::Object(obj) => {
            let map = obj.borrow();
            if map.is_empty() {
                return "{}".to_string();
            }
            
            let fields: Vec<String> = map
                .iter()
                .map(|(k, v)| format!("\"{}\":{}", k, value_to_json_string_compact(v)))
                .collect();
            
            format!("{{{}}}", fields.join(","))
        }
        Value::Function(_) => {
            "\"<function>\"".to_string()
        }
    }
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
        Value::String(s) => {
            let escaped = s
                .replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('\n', "\\n")
                .replace('\r', "\\r")
                .replace('\t', "\\t");
            format!("\"{}\"", escaped)
        }
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
        Value::Function(_) => {
            "\"<function>\"".to_string()
        }
    }
}

fn verbose_log(config: &AppConfig, message: &str) {
    if config.verbose {
        eprintln!("[jfm:debug] {}", message);
    }
}

fn error_message(config: &AppConfig, message: &str) {
    if config.color_enabled {
        eprintln!("{}", message.red().bold());
    } else {
        eprintln!("{}", message);
    }
}
