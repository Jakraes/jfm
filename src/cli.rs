use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::{generate, Shell};
use std::io;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "jfm")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "JSON query language interpreter", long_about = None)]
pub struct Args {
    #[arg(value_name = "JSON")]
    pub json: Option<String>,

    #[arg(short, long, value_name = "FILE", conflicts_with = "json")]
    pub file: Option<PathBuf>,

    #[arg(short, long, value_name = "QUERY")]
    pub query: Option<String>,

    #[arg(long = "query-file", value_name = "PATH", conflicts_with = "query")]
    pub query_file: Option<PathBuf>,

    #[arg(short, long, value_name = "OUTPUT_FILE")]
    pub out: Option<PathBuf>,

    #[arg(long = "color", value_name = "WHEN", default_value = "auto")]
    pub color: ColorChoice,

    #[arg(long = "compact")]
    pub compact: bool,

    #[arg(short = 'v', long = "verbose")]
    pub verbose: bool,

    #[arg(short = 'n', long = "limit", value_name = "N")]
    pub limit: Option<usize>,

    #[arg(long = "stream")]
    pub stream: bool,

    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    Complete {
        #[arg(value_name = "SHELL")]
        shell: Shell,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ColorChoice {
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
            _ => Err(format!(
                "Invalid color choice: {}. Must be 'auto', 'always', or 'never'",
                s
            )),
        }
    }
}

pub fn generate_completions(shell: Shell) {
    let mut cmd = Args::command();
    let bin_name = cmd.get_name().to_string();
    generate(shell, &mut cmd, &bin_name, &mut io::stdout());
}
