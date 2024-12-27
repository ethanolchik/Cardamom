const RED: &str = "\x1b[38;5;203m";
const CYAN: &str = "\x1b[38;5;117m";
const RESET: &str = "\x1b[0m";

#[derive(Clone)]
pub struct Error {
    pub message: String,
    pub line: usize,
    pub col: usize,

    pub filename: String,

    pub notes: Vec<Note>,

    source: String,
}

#[derive(Clone)]
pub struct Note {
    pub message: String,
    pub line: usize,
    pub col: usize,

    pub filename: String,
}

impl Error {
    pub fn new(message: String, line: usize, col: usize, filename: String) -> Error {
        Error {
            message,
            line,
            col,
            filename,
            notes: Vec::new(),
            source: String::new(),
        }
    }

    pub fn add_source(&mut self, source: String) {
        self.source = source;
    }

    pub fn add_note(&mut self, note: Note) {
        self.notes.push(note);
    }

    pub fn to_string(&self) -> String {
        let mut s = format!("{RED}Error{RESET} in {} at {}:{}", self.filename, self.line, self.col);

        let source_line = self.source_line();
        let caret = self.caret();

        s.push_str(&format!("\n{:>5} | {}", self.line, source_line));
        s.push_str(&format!("\n{:>5} | {RED}{}{RESET}", "", caret));

        s.push_str(&format!("\n{RED}{}{RESET}\n", self.message));

        for note in self.notes.iter() {
            s.push_str(&format!("\n{}", note.to_string()));
        }

        s
    }

    fn source_line(&self) -> String {
        let lines: Vec<&str> = self.source.split('\n').collect();
        lines[self.line - 1].to_string()
    }

    fn caret(&self) -> String {
        let mut caret = String::new();
        for _ in 0..self.col - 1 {
            caret.push(' ');
        }
        caret.push('^');
        caret
    }
}

impl Note {
    pub fn new(message: String, line: usize, col: usize, filename: String) -> Note {
        Note {
            message,
            line,
            col,
            filename,
        }
    }

    pub fn to_string(&self) -> String {
        let mut s = format!("{CYAN}Note{RESET} in {} at {}:{}", self.filename, self.line, self.col);

        s.push_str(&format!("\n{CYAN}{}\n{RESET}", self.message));

        s
    }
}