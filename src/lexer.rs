use crate::token::{Token, TokenKind};
use crate::errors::Error;

pub struct Lexer {
    pub source: String,
    pub filename: String,
    pub tokens: Vec<Token>,

    pub start: usize,
    pub current: usize,
    pub line: usize,
    pub col: usize,

    pub had_error: bool,
    pub error_tokens: Vec<Token>
}

impl Lexer {
    pub fn new(source: String, filename: String) -> Lexer {
        Lexer {
            source,
            filename,
            tokens: Vec::new(),

            start: 0,
            current: 0,
            line: 1,
            col: 1,
            had_error: false,
            error_tokens: Vec::new()
        }
    }

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.add_token(TokenKind::EndOfFile);

        let error_tokens: Vec<Token> = self.error_tokens.clone();
        for token in error_tokens.iter() {
            let message = format!("Unexpected token '{}'", token.lexeme);
            self.lexerr(&message, token.clone());
        }
    }

    pub fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenKind::LParen),
            ')' => self.add_token(TokenKind::RParen),
            '{' => self.add_token(TokenKind::LBrace),
            '}' => self.add_token(TokenKind::RBrace),
            '[' => self.add_token(TokenKind::LBracket),
            ']' => self.add_token(TokenKind::RBracket),
            ',' => self.add_token(TokenKind::Comma),
            '.' => {
                let token_kind = if self.match_token('.') { TokenKind::Range } else { TokenKind::Dot };
                self.add_token(token_kind);
            },
            ':' => self.add_token(TokenKind::Colon),
            ';' => self.add_token(TokenKind::Semicolon),
            '+' => {
                let token_kind = if self.match_token('=') { TokenKind::PlusEq } else { TokenKind::Plus };
                self.add_token(token_kind);
            },
            '-' => {
                let token_kind = if self.match_token('=') {
                    TokenKind::MinusEq
                } else {
                    let next_token = self.match_token('>');
                    if next_token {
                        TokenKind::Arrow
                    } else {
                        TokenKind::Minus
                    }
                };
                self.add_token(token_kind);
            },
            '*' => {
                if self.match_token('=') {
                    self.add_token(TokenKind::MulEq);
                } else if self.match_token('*') {
                    self.add_token(TokenKind::Pow);
                } else {
                    self.add_token(TokenKind::Mul);
                }
            },
            '/' => {
                if self.match_token('/') {
                    self.scan_comment();
                } else {
                    let token_kind = if self.match_token('=') { TokenKind::DivEq } else { TokenKind::Div };
                    self.add_token(token_kind);
                }
            },
            '%' => {
                let is_match = self.match_token('=');
                let token_kind = if is_match { TokenKind::ModEq } else { TokenKind::Mod };
                self.add_token(token_kind);
            },
            '&' => {
                let token_kind = if self.match_token('=') {
                    TokenKind::AmpEq
                } else if self.match_token('&') {
                    TokenKind::And
                } else {
                    TokenKind::Amp
                };
                self.add_token(token_kind);
            },
            '|' => {
                let token_kind = if self.match_token('=') {
                    TokenKind::PipeEq
                } else if self.match_token('|') {
                    TokenKind::Or
                } else {
                    TokenKind::Pipe
                };
                self.add_token(token_kind);
            },
            '^' => {
                let token_kind = if self.match_token('=') { TokenKind::CaretEq } else { TokenKind::Caret };
                self.add_token(token_kind);
            },
            '#' => self.add_token(TokenKind::Hash),
            '~' => self.add_token(TokenKind::Tilde),
            '!' => {
                let is_match = self.match_token('=');
                let token_kind = if is_match { TokenKind::Neq } else { TokenKind::Bang };
                self.add_token(token_kind);
            },
            '?' => {
                let is_match = self.match_token('?');
                let token_kind = if is_match { TokenKind::QuestionQuestion } else { TokenKind::Question };
                self.add_token(token_kind);
            },
            '=' => {
                let token_kind = if self.match_token('=') { TokenKind::EqEq } else { TokenKind::Eq };
                self.add_token(token_kind);
            },
            '<' => {
                let token_kind = if self.match_token('=') {
                    TokenKind::Lte
                } else {
                    let is_lshift = self.match_token('<');
                    if is_lshift {
                        if self.match_token('=') {
                            TokenKind::LShiftEq
                        } else {
                            TokenKind::LShift
                        }
                    } else {
                        TokenKind::Lt
                    }
                };
                self.add_token(token_kind);
            },
            '>' => {
                let token_kind = if self.match_token('=') {
                    TokenKind::Gte
                } else {
                    let is_rshift = self.match_token('>');
                    if is_rshift {
                        if self.match_token('=') {
                            TokenKind::RShiftEq
                        } else {
                            TokenKind::RShift
                        }
                    } else {
                        TokenKind::Gt
                    }
                };
                self.add_token(token_kind);
            },
            '"' => self.scan_string(c),
            '\'' => self.scan_string(c),
            ' ' | '\r' | '\t' => (),
            '\n' => {
                self.line += 1;
                self.col = 1;
            },
            _ => {
                if c.is_digit(10) {
                    self.scan_number();
                } else if c.is_alphabetic() {
                    self.scan_identifier();
                } else {
                    self.add_token(TokenKind::Error);
                }
            }
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        self.col += 1;
        c
    }

    fn match_token(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.current).unwrap()
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.chars().nth(self.current + 1).unwrap()
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        let lexeme = self.source[self.start..self.current].to_string();
        let token = Token::new(kind.clone(), lexeme, self.line, self.col);
        self.tokens.push(token.clone());

        if kind == TokenKind::Error {
            self.error_tokens.push(token);
        }
    }

    fn scan_string(&mut self, delimiter: char) {
        while self.peek() != delimiter && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.add_token(TokenKind::Error);
            return;
        }

        self.advance(); // Closing quote
        self.add_token(TokenKind::String);
    }

    fn scan_number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance(); // Consume the '.'
            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        self.add_token(TokenKind::Float);
    }

    fn scan_identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let kind = match text {
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "class" => TokenKind::Class,
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "const" => TokenKind::Const,
            "public" => TokenKind::Public,
            "private" => TokenKind::Private,
            "protected" => TokenKind::Protected,
            "static" => TokenKind::Static,
            "import" => TokenKind::Import,
            "as" => TokenKind::As,
            "new" => TokenKind::New,
            _ => TokenKind::Identifier,
        };
        self.add_token(kind);
    }

    fn scan_comment(&mut self) {
        while self.peek() != '\n' && !self.is_at_end() {
            self.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn lexerr(&mut self, message: &str, token: Token) {
        let mut error = Error::new(message.to_string(), token.line, token.col, self.filename.clone());
        error.add_source(self.source.clone());
        eprintln!("{}", error.to_string());

        self.had_error = true;
    }

    pub fn print_tokens(&self) {
        for token in &self.tokens {
            println!("{}", token.to_string());
        }
    }
}