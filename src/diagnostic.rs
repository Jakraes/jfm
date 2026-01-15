use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn dummy() -> Self {
        Self { start: 0, end: 0 }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn is_dummy(&self) -> bool {
        self.start == 0 && self.end == 0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
            Severity::Info => write!(f, "info"),
            Severity::Hint => write!(f, "hint"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub message: String,
    pub style: LabelStyle,
}

impl Label {
    pub fn primary(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            style: LabelStyle::Primary,
        }
    }

    pub fn secondary(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            style: LabelStyle::Secondary,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            code: None,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Warning,
            code: None,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.notes.push(format!("help: {}", help.into()));
        self
    }
}

struct LineInfo {
    content: String,
}

pub fn line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

fn get_line_info(source: &str, line_num: usize) -> Option<LineInfo> {
    let mut current_line = 1;
    let mut start_offset = 0;

    for (i, ch) in source.char_indices() {
        if current_line == line_num {
            let end = source[i..].find('\n').map(|p| i + p).unwrap_or(source.len());
            return Some(LineInfo {
                content: source[i..end].to_string(),
            });
        }
        if ch == '\n' {
            current_line += 1;
            start_offset = i + 1;
        }
    }

    if current_line == line_num && start_offset <= source.len() {
        Some(LineInfo {
            content: source[start_offset..].to_string(),
        })
    } else {
        None
    }
}

pub struct DiagnosticRenderer<'a> {
    source: &'a str,
    file_name: &'a str,
    use_color: bool,
}

impl<'a> DiagnosticRenderer<'a> {
    pub fn new(source: &'a str, file_name: &'a str, use_color: bool) -> Self {
        Self {
            source,
            file_name,
            use_color,
        }
    }

    pub fn render(&self, diagnostic: &Diagnostic) -> String {
        let mut output = String::new();

        self.render_header(&mut output, diagnostic);

        let mut lines_to_show: Vec<usize> = Vec::new();
        for label in &diagnostic.labels {
            if label.span.is_dummy() {
                continue;
            }
            let (start_line, _) = line_col(self.source, label.span.start);
            let (end_line, _) = line_col(self.source, label.span.end.saturating_sub(1).max(label.span.start));
            for line in start_line..=end_line {
                if !lines_to_show.contains(&line) {
                    lines_to_show.push(line);
                }
            }
        }
        lines_to_show.sort();

        if !lines_to_show.is_empty() {
            let first_label = diagnostic.labels.iter().find(|l| !l.span.is_dummy());
            if let Some(label) = first_label {
                let (line, col) = line_col(self.source, label.span.start);
                output.push_str(&format!("  {} {}:{}:{}\n", 
                    self.style_blue("-->"), self.file_name, line, col));
            }

            let max_line = lines_to_show.last().copied().unwrap_or(1);
            let line_num_width = max_line.to_string().len();

            output.push_str(&format!("{} {}\n", " ".repeat(line_num_width + 1), self.style_blue("|")));

            for &line_num in &lines_to_show {
                self.render_line(&mut output, diagnostic, line_num, line_num_width);
            }

            output.push_str(&format!("{} {}\n", " ".repeat(line_num_width + 1), self.style_blue("|")));
        } else if diagnostic.severity == Severity::Error {
            // Show at least one line marker if we have an error but no spans
            output.push_str(&format!("  {} {}:1:1\n", 
                self.style_blue("-->"), self.file_name));
            output.push_str(&format!("{} {}\n", " ", self.style_blue("|")));
            output.push_str(&format!("{} {}\n", " ", self.style_blue("|")));
        }

        for note in &diagnostic.notes {
            let prefix = if note.starts_with("help:") {
                self.style_cyan("=")
            } else {
                self.style_blue("=")
            };
            output.push_str(&format!("  {} {}\n", prefix, note));
        }

        output
    }

    fn render_header(&self, output: &mut String, diagnostic: &Diagnostic) {
        let severity_str = match diagnostic.severity {
            Severity::Error => self.style_red_bold("error"),
            Severity::Warning => self.style_yellow_bold("warning"),
            Severity::Info => self.style_blue("info"),
            Severity::Hint => self.style_cyan("hint"),
        };

        if let Some(code) = &diagnostic.code {
            output.push_str(&format!("{}[{}]: {}\n", severity_str, code, 
                self.style_bold(&diagnostic.message)));
        } else {
            output.push_str(&format!("{}: {}\n", severity_str, 
                self.style_bold(&diagnostic.message)));
        }
    }

    fn render_line(&self, output: &mut String, diagnostic: &Diagnostic, line_num: usize, width: usize) {
        let line_info = match get_line_info(self.source, line_num) {
            Some(info) => info,
            None => return,
        };

        output.push_str(&format!("{:>width$} {} {}\n",
            self.style_blue(&line_num.to_string()),
            self.style_blue("|"),
            &line_info.content,
            width = width + 1
        ));

        let mut underlines: Vec<(usize, usize, &str, LabelStyle)> = Vec::new();
        for label in &diagnostic.labels {
            if label.span.is_dummy() {
                continue;
            }
            let (label_start_line, start_col) = line_col(self.source, label.span.start);
            let (label_end_line, end_col) = line_col(self.source, label.span.end.saturating_sub(1).max(label.span.start));

            if label_start_line <= line_num && label_end_line >= line_num {
                let col_start = if label_start_line == line_num { start_col } else { 1 };
                let col_end = if label_end_line == line_num { 
                    // Ensure we don't go beyond the line
                    end_col.min(line_info.content.len() + 1)
                } else { 
                    line_info.content.len() + 1 
                };
                if !label.message.is_empty() || !label.span.is_dummy() {
                    underlines.push((col_start, col_end, &label.message, label.style));
                }
            }
        }

        if !underlines.is_empty() {
            let mut underline_str = String::new();
            let mut pos = 1;
            
            underlines.sort_by_key(|(start, _, _, _)| *start);

            for (col_start, col_end, _, style) in &underlines {
                while pos < *col_start {
                    underline_str.push(' ');
                    pos += 1;
                }
                let underline_char = match style {
                    LabelStyle::Primary => '^',
                    LabelStyle::Secondary => '-',
                };
                while pos < *col_end {
                    underline_str.push(underline_char);
                    pos += 1;
                }
            }

            let styled_underline = if underlines.iter().any(|(_, _, _, style)| *style == LabelStyle::Primary) {
                self.style_red(&underline_str)
            } else {
                self.style_blue(&underline_str)
            };

            output.push_str(&format!("{} {} {}\n",
                " ".repeat(width + 1),
                self.style_blue("|"),
                styled_underline
            ));

            for (col_start, _, message, style) in &underlines {
                if !message.is_empty() {
                    let padding = " ".repeat(col_start.saturating_sub(1));
                    let styled_msg = match style {
                        LabelStyle::Primary => self.style_red(message),
                        LabelStyle::Secondary => self.style_blue(message),
                    };
                    output.push_str(&format!("{} {} {}{}\n",
                        " ".repeat(width + 1),
                        self.style_blue("|"),
                        padding,
                        styled_msg
                    ));
                }
            }
        } else if diagnostic.severity == Severity::Error {
            // Show at least a caret if we have an error but no specific span for this line
            output.push_str(&format!("{} {} {}\n",
                " ".repeat(width + 1),
                self.style_blue("|"),
                self.style_red("^")
            ));
        }
    }

    fn style_red(&self, text: &str) -> String {
        if self.use_color {
            format!("\x1b[31m{}\x1b[0m", text)
        } else {
            text.to_string()
        }
    }

    fn style_red_bold(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[1;31m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }

    fn style_yellow_bold(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[1;33m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }

    fn style_blue(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[34m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }

    fn style_cyan(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[36m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }

    fn style_bold(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[1m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }
}

pub fn render_diagnostics(source: &str, file_name: &str, diagnostics: &[Diagnostic], use_color: bool) -> String {
    let renderer = DiagnosticRenderer::new(source, file_name, use_color);
    let mut output = String::new();
    
    for diagnostic in diagnostics {
        output.push_str(&renderer.render(diagnostic));
        output.push('\n');
    }

    let error_count = diagnostics.iter().filter(|d| d.severity == Severity::Error).count();
    let warning_count = diagnostics.iter().filter(|d| d.severity == Severity::Warning).count();

    if error_count > 0 || warning_count > 0 {
        let mut summary_parts = Vec::new();
        if error_count > 0 {
            summary_parts.push(format!("{} error{}", error_count, if error_count == 1 { "" } else { "s" }));
        }
        if warning_count > 0 {
            summary_parts.push(format!("{} warning{}", warning_count, if warning_count == 1 { "" } else { "s" }));
        }
        
        if error_count > 0 {
            output.push_str(&format!("error: aborting due to {}\n", summary_parts.join(" and ")));
        }
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_col() {
        let source = "let x = 5;\nlet y = 10;";
        assert_eq!(line_col(source, 0), (1, 1));
        assert_eq!(line_col(source, 4), (1, 5));
        assert_eq!(line_col(source, 11), (2, 1));
        assert_eq!(line_col(source, 15), (2, 5));
    }

    #[test]
    fn test_span_merge() {
        let s1 = Span::new(5, 10);
        let s2 = Span::new(8, 15);
        let merged = s1.merge(s2);
        assert_eq!(merged.start, 5);
        assert_eq!(merged.end, 15);
    }

    #[test]
    fn test_diagnostic_rendering() {
        let source = "let x = ;\n";
        let diagnostic = Diagnostic::error("expected expression")
            .with_code("E0101")
            .with_label(Label::primary(Span::new(8, 9), "expected expression here"))
            .with_help("provide a value after `=`");
        
        let renderer = DiagnosticRenderer::new(source, "query", false);
        let output = renderer.render(&diagnostic);
        
        assert!(output.contains("error[E0101]"));
        assert!(output.contains("expected expression"));
        assert!(output.contains("query:1:9"));
    }
}
