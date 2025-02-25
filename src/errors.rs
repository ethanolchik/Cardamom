use crate::token::Span;

const RED: &str = "\x1b[38;5;203m";
const CYAN: &str = "\x1b[38;5;117m";
const YELLOW: &str = "\x1b[38;5;227m";
const GREEN: &str = "\x1b[38;5;70m";
const RESET: &str = "\x1b[0m";

const CONTEXT_LINES: usize = 2;

const MAX_LINE_LENGTH: usize = 80;  // Adjust this to your preferred line length

trait Diagnostic {
    fn get_line(&self) -> usize;
    fn get_span(&self) -> &Span;
    fn get_filename(&self) -> &str;
    fn get_message(&self) -> &str;
    fn get_kind(&self) -> &str;
    fn get_colour(&self) -> &str;
    
    fn caret(&self) -> String {
        let mut caret = String::new();
        let span = self.get_span();
        
        if span.end == 0 {
            return caret;
        }

        caret.push_str(&" ".repeat(span.start));
        caret.push_str(&"^".repeat(span.end - span.start));
        caret
    }

    fn source_lines(&self, source: &str) -> Vec<(usize, String)> {
        let lines: Vec<&str> = source.split('\n').collect();
        let error_line = self.get_line();
        let start_line = error_line.saturating_sub(CONTEXT_LINES);
        let end_line = (error_line + CONTEXT_LINES).min(lines.len());
        
        (start_line..=end_line)
            .filter_map(|line_num| {
                if line_num == 0 || line_num > lines.len() {
                    None
                } else {
                    Some((line_num, lines[line_num - 1].to_string()))
                }
            })
            .collect()
    }
    
    fn format_message(&self, colour: &str) -> String {
        let mut output = String::new();
        output.push_str(&format!("{}╭─{} {} in {}\n", colour, self.get_kind(), RESET, self.get_filename()));
        output.push_str(&format!("{}│\n", colour));
        output
    }
}

#[derive(Clone)]
pub struct Error {
    pub message: String,
    pub line: usize,
    pub span: Span,

    pub filename: String,

    pub notes: Vec<Note>,
    pub helps: Vec<Help>,

    source: String,
}

#[derive(Clone)]
pub struct Note {
    pub message: String,
    pub line: usize,
    pub span: Span,

    pub filename: String,
}

#[derive(Clone)]
pub struct Warning {
    pub message: String,
    pub line: usize,
    pub span: Span,

    pub filename: String,

    pub notes: Vec<Note>,
    pub helps: Vec<Help>,

    source: String,
}

#[derive(Clone)]
pub struct Help {
    pub message: String,
    pub line: usize,
    pub span: Span,

    pub filename: String,
}

impl Diagnostic for Error {
    fn get_line(&self) -> usize { self.line }
    fn get_span(&self) -> &Span { &self.span }
    fn get_filename(&self) -> &str { &self.filename }
    fn get_message(&self) -> &str { &self.message }
    fn get_kind(&self) -> &str { "error" }
    fn get_colour(&self) -> &str { RED }
}

impl Diagnostic for Note {
    fn get_line(&self) -> usize { self.line }
    fn get_span(&self) -> &Span { &self.span }
    fn get_filename(&self) -> &str { &self.filename }
    fn get_message(&self) -> &str { &self.message }
    fn get_kind(&self) -> &str { "note" }
    fn get_colour(&self) -> &str { CYAN }
}

impl Diagnostic for Warning {
    fn get_line(&self) -> usize { self.line }
    fn get_span(&self) -> &Span { &self.span }
    fn get_filename(&self) -> &str { &self.filename }
    fn get_message(&self) -> &str { &self.message }
    fn get_kind(&self) -> &str { "warning" }
    fn get_colour(&self) -> &str { YELLOW }
}

impl Diagnostic for Help {
    fn get_line(&self) -> usize { self.line }
    fn get_span(&self) -> &Span { &self.span }
    fn get_filename(&self) -> &str { &self.filename }
    fn get_message(&self) -> &str { &self.message }
    fn get_kind(&self) -> &str { "help" }
    fn get_colour(&self) -> &str { GREEN }
}

impl Error {
    pub fn new(message: String, line: usize, span: Span, filename: String) -> Error {
        Error {
            message,
            line,
            span,
            filename,
            notes: Vec::new(),
            helps: Vec::new(),
            source: String::new(),
        }
    }

    pub fn add_source(&mut self, source: String) {
        self.source = source;
    }

    pub fn add_note(&mut self, note: Note) {
        self.notes.push(note);
    }

    pub fn add_help(&mut self, help: Help) {
        self.helps.push(help);
    }

    fn colourise(&self, content: &str) -> String {
        // Slice the content into parts:
        // 1. The span before the error
        // 2. The span of the error
        // 3. The span after the error
        // Then we colour the 2nd part red and return the concatenation of all parts
        let span = self.get_span();
        let (before, rest) = content.split_at(span.start);
        let (error, after) = rest.split_at(span.end - span.start);
        format!("{}{}{}{}{}", before, RED, error, RESET, after)
    }

    fn wrap_message(message: &str, indent: usize) -> String {
        let available_width;

        if indent > MAX_LINE_LENGTH {
            available_width = MAX_LINE_LENGTH;
        } else {
            available_width = MAX_LINE_LENGTH - indent;
        }

        let mut result = String::new();
        let mut current_line = String::new();
        let mut first_line = true;

        for word in message.split_whitespace() {
            if current_line.len() + word.len() + 1 <= available_width {
                if !current_line.is_empty() {
                    current_line.push(' ');
                }
                current_line.push_str(word);
            } else {
                if !first_line {
                    result.push_str(&format!("\n{:indent$}", ""));
                }
                result.push_str(&current_line);
                current_line.clear();
                current_line.push_str(word);
                first_line = false;
            }
        }

        if !current_line.is_empty() {
            if !first_line {
                result.push_str(&format!("\n{:indent$}", "", indent = indent));
            }
            result.push_str(&current_line);
        }

        result
    }

    pub fn to_string(&self) -> String {
        let mut output = String::new();
        
        // 1) Print the standard error header
        output.push_str(&format!("{}error{}: {}\n", self.get_colour(), RESET, self.message));
        output.push_str(&format!("{}->{} {}:{}\n", self.get_colour(), RESET, self.filename, self.line));

        // 2) Gather "relevant lines":
        //    - The primary error line
        //    - Lines for notes
        //    - Lines for helps
        let mut relevant_lines = Vec::new();
        relevant_lines.push((self.line, true)); // main error line
        for note in &self.notes {
            relevant_lines.push((note.line, false));
        }
        for help in &self.helps {
            relevant_lines.push((help.line, false));
        }
        relevant_lines.sort_by_key(|&(ln, _)| ln);

        let total_lines = self.source.lines().count();

        // 3) Build intervals [start..end] around each relevant line
        //    by expanding CONTEXT_LINES above/below
        let mut intervals = Vec::new();
        for &(line_num, _) in &relevant_lines {
            if line_num == 0 || line_num > total_lines {
                continue; 
            }
            let start = line_num.saturating_sub(CONTEXT_LINES).max(1);
            let end = (line_num + CONTEXT_LINES).min(total_lines);
            intervals.push((start, end));
        }

        // 4) Merge overlapping/adjacent intervals to avoid duplicates
        intervals.sort_by(|a, b| a.0.cmp(&b.0));
        let mut merged = Vec::<(usize, usize)>::new();
        for (start, end) in intervals {
            if let Some((_, prev_end)) = merged.last_mut() {
                // If they overlap or are adjacent, merge them
                if start <= *prev_end + 1 {
                    *prev_end = (*prev_end).max(end);
                } else {
                    merged.push((start, end));
                }
            } else {
                merged.push((start, end));
            }
        }

        // 5) Create quick lookups for primary error line, plus notes/helps by line
        use std::collections::HashMap;
        let mut is_primary_line = HashMap::new();
        for &(ln, primary) in &relevant_lines {
            match is_primary_line.get(&ln) {
                // If it's not yet set, just insert
                None => {
                    is_primary_line.insert(ln, primary);
                }
                Some(old_val) => {
                    // If we already have `true`, don't overwrite it with `false`
                    if !*old_val && primary {
                        is_primary_line.insert(ln, true);
                    }
                }
            }
        }

        let mut notes_by_line: HashMap<usize, Vec<&Note>> = HashMap::new();
        for note in &self.notes {
            notes_by_line.entry(note.line).or_default().push(note);
        }

        let mut helps_by_line: HashMap<usize, Vec<&Help>> = HashMap::new();
        for help in &self.helps {
            helps_by_line.entry(help.line).or_default().push(help);
        }

        // 6) Now print lines from each merged interval, inserting "..." between distant intervals
        let all_source_lines: Vec<&str> = self.source.lines().collect();
        let mut last_printed_line = 0;

        for (start, end) in merged {
            // If there's a big gap from the last printed line, insert ellipsis
            if last_printed_line > 0 && start > last_printed_line + 1 {
                output.push_str(&format!(" {:>4} │ \n", ""));
                output.push_str(&format!(" {:>4} │ ...\n", ""));
                output.push_str(&format!(" {:>4} │ \n", ""));
            }

            // Print each line in the interval
            for current_line in start..=end {
                if current_line == 0 || current_line > total_lines {
                    continue;
                }
                let line_content = all_source_lines[current_line - 1];

                // Check if this line is the *primary error line*
                if let Some(true) = is_primary_line.get(&current_line) {
                    // **Highlight** the erroneous slice in red
                    let highlighted = self.colourise(line_content);
                    output.push_str(&format!(
                        " {}{:>4}{} │ {}\n",
                        self.get_colour(),
                        current_line,
                        RESET,
                        highlighted
                    ));
                } else {
                    // Just print normally
                    output.push_str(&format!(" {:>4} │ {}\n", current_line, line_content));
                }

                // Print notes on this line
                if let Some(line_notes) = notes_by_line.get(&current_line) {
                    for note in line_notes {
                        let caret_indent = "      │ ".len();
                        let note_caret = note.caret();
                        output.push_str(&format!("      │ {}{}{} ",
                            CYAN, note_caret, RESET
                        ));

                        let total_indent = caret_indent + note_caret.len() + 1;
                        let wrapped_message = Self::wrap_message(&note.message, total_indent);

                        output.push_str(&format!("{}{}{}\n", CYAN, wrapped_message, RESET));
                    }
                }

                // Print helps on this line
                if let Some(line_helps) = helps_by_line.get(&current_line) {
                    for help in line_helps {
                        let caret_indent = "      │ ".len();
                        let help_caret = help.caret();
                        output.push_str(&format!("      │ {}{}{} ",
                            GREEN, help_caret, RESET
                        ));

                        let total_indent = caret_indent + help_caret.len() + 1;
                        let wrapped_message = Self::wrap_message(&help.message, total_indent);

                        output.push_str(&format!("{}{}{}\n", GREEN, wrapped_message, RESET));
                    }
                }

                last_printed_line = current_line;
            }
        }

        output
    }
}

impl Note {
    pub fn new(message: String, line: usize, span: Span, filename: String) -> Note {
        Note {
            message,
            line,
            span,
            filename,
        }
    }
}

impl Warning {
    pub fn new(message: String, line: usize, span: Span, filename: String) -> Warning {
        Warning {
            message,
            line,
            span,
            filename,
            notes: Vec::new(),
            helps: Vec::new(),
            source: String::new(),
        }
    }

    pub fn add_help(&mut self, help: Help) {
        self.helps.push(help);
    }

    pub fn add_note(&mut self, note: Note) {
        self.notes.push(note);
    }

    pub fn add_source(&mut self, source: String) {
        self.source = source;
    }

    fn wrap_message(message: &str, indent: usize) -> String {
        let available_width = MAX_LINE_LENGTH - indent;
        let mut result = String::new();
        let mut current_line = String::new();
        let mut first_line = true;

        for word in message.split_whitespace() {
            if current_line.len() + word.len() + 1 <= available_width {
                if !current_line.is_empty() {
                    current_line.push(' ');
                }
                current_line.push_str(word);
            } else {
                if !first_line {
                    result.push_str(&format!("\n{:indent$}", "", indent = indent));
                }
                result.push_str(&current_line);
                current_line.clear();
                current_line.push_str(word);
                first_line = false;
            }
        }

        if !current_line.is_empty() {
            if !first_line {
                result.push_str(&format!("\n{:indent$}", "", indent = indent));
            }
            result.push_str(&current_line);
        }

        result
    }

    pub fn to_string(&self) -> String {
        let mut output = String::new();
        
        // Header
        output.push_str(&format!("{}warning{}: {}\n", self.get_colour(), RESET, self.message));
        output.push_str(&format!("{}->{} {}:{}\n", self.get_colour(), RESET, self.filename, self.line));
        
        // Collect all lines we need to show
        let mut all_lines: Vec<(usize, bool)> = vec![(self.line, true)];
        for note in &self.notes {
            all_lines.push((note.line, false));
        }
        all_lines.sort_by_key(|&(line, _)| line);

        let min_line = all_lines.iter().map(|&(line, _)| line).min().unwrap_or(self.line);
        let max_line = all_lines.iter().map(|&(line, _)| line).max().unwrap_or(self.line);
        let start_line = min_line.saturating_sub(CONTEXT_LINES);
        let end_line = (max_line + CONTEXT_LINES).min(self.source.lines().count());

        // Source code section
        for line_num in start_line..=end_line {
            let line_content = match self.source.lines().nth(line_num - 1) {
                Some(content) => content,
                None => continue,
            };

            // Line number and content
            output.push_str(&format!(" {:>4} │ {}\n", line_num, line_content));

            // Error indicator
            if line_num == self.line {
                output.push_str(&format!("      │ {}{}{}\n",
                    self.get_colour(),
                    self.caret(),
                    RESET
                ));
            }

            // Notes for this line
            for note in &self.notes {
                if note.line == line_num {
                    let caret_indent = "      │ ".len();
                    output.push_str(&format!("      │ {}{}{} ",
                        CYAN,
                        note.caret(),
                        RESET
                    ));
                    
                    // Calculate the indent for wrapped lines
                    let total_indent = caret_indent + note.caret().len() + 1;
                    let wrapped_message = Self::wrap_message(&note.message, total_indent);
                    
                    output.push_str(&format!("{}{}{}\n",
                        CYAN,
                        wrapped_message,
                        RESET
                    ));
                }
            }

            for help in &self.helps {
                if help.line == line_num {
                    let caret_indent = "      │ ".len();
                    output.push_str(&format!("      │ {}{}{} ",
                        GREEN,
                        help.caret(),
                        RESET
                    ));
                    
                    // Calculate the indent for wrapped lines
                    let total_indent = caret_indent + help.caret().len() + 1;
                    let wrapped_message = Self::wrap_message(&help.message, total_indent);
                    
                    output.push_str(&format!("{}{}{}\n",
                        GREEN,
                        wrapped_message,
                        RESET
                    ));
                }
            }
        }

        output
    }
}

impl Help {
    pub fn new(message: String, line: usize, span: Span, filename: String) -> Help {
        Help {
            message,
            line,
            span,
            filename,
        }
    }
}