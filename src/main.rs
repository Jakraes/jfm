use clap::Parser;
use jfm::cli::{generate_completions, AppConfig, Args, Commands};
use jfm::diagnostic::render_diagnostics;
use jfm::format::{json_to_value, parse_json, value_to_json_string};
use jfm::interpreter;
use jfm::Value;
use owo_colors::OwoColorize;
use std::cell::RefCell;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;

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

    verbose_log(
        &config,
        &format!("Read {} bytes of JSON input", json_str.len()),
    );

    if config.stream {
        verbose_log(&config, "Streaming mode enabled");

        if args.query.is_none() && args.query_file.is_none() {
            error_message(
                &config,
                "Streaming mode requires a query (--query or --query-file)",
            );
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

    let root_value = match parse_json(&json_str) {
        Ok(val) => {
            verbose_log(&config, "Successfully parsed JSON");
            json_to_value(val)
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
        verbose_log(
            config,
            &format!("Reading JSON from file: {}", file.display()),
        );
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
            return Err(
                "No input provided. Must provide --file, JSON string argument, or JSON via stdin"
                    .to_string(),
            );
        }

        Ok(buffer)
    }
}

fn read_query_input(args: &Args, config: &AppConfig) -> Result<String, String> {
    if let Some(query) = &args.query {
        verbose_log(config, "Using query from command-line argument");
        Ok(query.clone())
    } else if let Some(query_file) = &args.query_file {
        verbose_log(
            config,
            &format!("Reading query from file: {}", query_file.display()),
        );
        read_file(query_file)
    } else {
        unreachable!("Should have query or query-file if not in interactive mode");
    }
}

fn run_interactive_mode(root_value: Value, out_file: &Option<PathBuf>, config: &AppConfig) {
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

    verbose_log(
        config,
        &format!("Executing interactive query: {}", trimmed_query),
    );
    execute_query(trimmed_query, &root_value, out_file, true, config);
}

fn execute_query(
    query_str: &str,
    root_value: &Value,
    out_file: &Option<PathBuf>,
    is_interactive: bool,
    config: &AppConfig,
) {
    verbose_log(config, "Parsing query");

    let result = match interpreter::parse_and_run_with_diagnostics(query_str, root_value.clone()) {
        Ok(Some(result)) => {
            verbose_log(config, "Query executed successfully");
            let limited_result = apply_limit(&result, config.limit);
            format!("{}\n", value_to_json_string(&limited_result, config.compact))
        }
        Ok(None) => {
            verbose_log(config, "Query returned None, outputting empty object");
            "{}\n".to_string()
        }
        Err(diagnostics) => {
            let rendered = render_diagnostics(query_str, "query", &diagnostics, config.color_enabled);
            eprint!("{}", rendered);
            std::process::exit(1);
        }
    };

    if out_file.is_none() || is_interactive {
        print!("{}", result);
        io::stdout().flush().unwrap();
    }

    if let Some(out_path) = out_file {
        verbose_log(
            config,
            &format!("Writing output to file: {}", out_path.display()),
        );

        use std::fs::OpenOptions;

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

fn apply_limit(value: &Value, limit: Option<usize>) -> Value {
    match (value, limit) {
        (Value::Array(array), Some(n)) => {
            let items = array.borrow();
            let limited: Vec<Value> = items.iter().take(n).cloned().collect();
            Value::Array(Rc::new(RefCell::new(limited)))
        }
        _ => value.clone(),
    }
}

fn write_output(output: &str, writer: &mut Option<std::fs::File>) {
    if let Some(w) = writer {
        let _ = w.write_all(output.as_bytes());
    } else {
        print!("{}", output);
    }
}

fn execute_streaming(
    json_str: &str,
    query_str: &str,
    out_file: &Option<PathBuf>,
    config: &AppConfig,
) {
    use std::fs::OpenOptions;

    let mut output_count = 0usize;
    let limit = config.limit.unwrap_or(usize::MAX);

    let is_ndjson = json_str.lines().next().is_some_and(|first_line| {
        let trimmed = first_line.trim();
        !trimmed.is_empty() && !trimmed.starts_with('[')
    });

    let mut out_writer: Option<std::fs::File> = out_file.as_ref().map(|out_path| {
        OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(out_path)
            .unwrap_or_else(|e| {
                error_message(config, &format!("Error opening output file: {}", e));
                std::process::exit(1);
            })
    });

    let process_result =
        |result: &Value, writer: &mut Option<std::fs::File>, count: &mut usize, compact: bool| {
            let output = format!("{}\n", value_to_json_string(result, compact));
            write_output(&output, writer);
            *count += 1;
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

            match parse_json(trimmed) {
                Ok(json_val) => {
                    let root_value = json_to_value(json_val);
                    match interpreter::parse_and_run(query_str, root_value) {
                        Ok(Some(result)) => {
                            process_result(&result, &mut out_writer, &mut output_count, config.compact)
                        }
                        Ok(None) => {}
                        Err(e) => error_message(config, &format!("Query error on line: {}", e)),
                    }
                }
                Err(e) => error_message(config, &format!("JSON parse error on line: {}", e)),
            }
        }
    } else {
        verbose_log(config, "Processing as JSON array stream");

        match parse_json(json_str) {
            Ok(serde_json::Value::Array(array)) => {
                for item in array {
                    if output_count >= limit {
                        verbose_log(config, &format!("Reached limit of {} results", limit));
                        break;
                    }

                    let root_value = json_to_value(item);
                    match interpreter::parse_and_run(query_str, root_value) {
                        Ok(Some(result)) => {
                            process_result(&result, &mut out_writer, &mut output_count, config.compact)
                        }
                        Ok(None) => {}
                        Err(e) => error_message(config, &format!("Query error on item: {}", e)),
                    }
                }
            }
            Ok(json_val) => {
                let root_value = json_to_value(json_val);
                match interpreter::parse_and_run(query_str, root_value) {
                    Ok(Some(result)) => {
                        let limited_result = apply_limit(&result, config.limit);
                        write_output(
                            &format!("{}\n", value_to_json_string(&limited_result, config.compact)),
                            &mut out_writer,
                        );
                    }
                    Ok(None) => write_output("{}\n", &mut out_writer),
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

    verbose_log(
        config,
        &format!("Streaming complete. Processed {} results", output_count),
    );
    io::stdout().flush().unwrap();
}

fn read_file(path: &Path) -> Result<String, String> {
    std::fs::read_to_string(path).map_err(|e| format!("Failed to read {}: {}", path.display(), e))
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
