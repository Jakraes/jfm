use crate::cli::{Args, ColorChoice};

pub struct AppConfig {
    pub color_enabled: bool,
    pub compact: bool,
    pub verbose: bool,
    pub limit: Option<usize>,
    pub stream: bool,
}

impl AppConfig {
    pub fn from_args(args: &Args) -> Self {
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
