#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub span: Span,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum TokenKind {
    EndOfFile,

    // Literals
    Integer, Float, String, Identifier,

    // Operators
    Plus, Minus, Mul, Div, Mod, Amp, Pipe, Caret, Tilde,
    Bang, Question, Eq, Neq, Lt, Gt, Lte, Gte, EqEq, And, Or,
    PlusEq, MinusEq, MulEq, DivEq, ModEq, AmpEq, PipeEq, CaretEq,
    QuestionQuestion, Pow, PowEq, LShift, RShift, LShiftEq, RShiftEq,
    Range, Arrow, Hash, StaticAccess,

    // Delimiters
    LParen, RParen, LBrace, RBrace, LBracket, RBracket, Comma, Dot, Colon, Semicolon,

    // Keywords
    If, Else, While, For, Break, Continue, Return,
    Class, Fn, Let, Const, Public, Private, Protected, Static,
    Import, As, New, Extern, Extend,

    // Misc
    Error
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize, span: Span) -> Token {
        Token { kind, lexeme, line, span }
    }

    pub fn to_string(&self) -> String {
        format!("{} {} at {}:{}", self.kind.clone().to_string(), self.lexeme, self.line, self.span.start)
    }

    pub fn dummy(lexeme: &str) -> Token {
        Token {
            kind: TokenKind::EndOfFile,
            lexeme: lexeme.to_string(),
            line: 0,
            span: Span::new(0, 0)
        }
    }
}

impl TokenKind {
    pub fn to_string(self) -> String {
        match self {
            TokenKind::EndOfFile => "EndOfFile".to_string(),
            TokenKind::Integer => "Integer".to_string(),
            TokenKind::Float => "Float".to_string(),
            TokenKind::String => "String".to_string(),
            TokenKind::Identifier => "Identifier".to_string(),
            TokenKind::Plus => "Plus".to_string(),
            TokenKind::Minus => "Minus".to_string(),
            TokenKind::Mul => "Mul".to_string(),
            TokenKind::Div => "Div".to_string(),
            TokenKind::Mod => "Mod".to_string(),
            TokenKind::Amp => "Amp".to_string(),
            TokenKind::Pipe => "Pipe".to_string(),
            TokenKind::Caret => "Caret".to_string(),
            TokenKind::Tilde => "Tilde".to_string(),
            TokenKind::Bang => "Bang".to_string(),
            TokenKind::Question => "Question".to_string(),
            TokenKind::Eq => "Eq".to_string(),
            TokenKind::Neq => "Neq".to_string(),
            TokenKind::Lt => "Lt".to_string(),
            TokenKind::Gt => "Gt".to_string(),
            TokenKind::Lte => "Lte".to_string(),
            TokenKind::Gte => "Gte".to_string(),
            TokenKind::EqEq => "EqEq".to_string(),
            TokenKind::And => "And".to_string(),
            TokenKind::Or => "Or".to_string(),
            TokenKind::PlusEq => "PlusEq".to_string(),
            TokenKind::MinusEq => "MinusEq".to_string(),
            TokenKind::MulEq => "MulEq".to_string(),
            TokenKind::DivEq => "DivEq".to_string(),
            TokenKind::ModEq => "ModEq".to_string(),
            TokenKind::AmpEq => "AmpEq".to_string(),
            TokenKind::PipeEq => "PipeEq".to_string(),
            TokenKind::CaretEq => "CaretEq".to_string(),
            TokenKind::QuestionQuestion => "QuestionQuestion".to_string(),
            TokenKind::Pow => "Pow".to_string(),
            TokenKind::PowEq => "PowEq".to_string(),
            TokenKind::LShift => "LShift".to_string(),
            TokenKind::RShift => "RShift".to_string(),
            TokenKind::LShiftEq => "LShiftEq".to_string(),
            TokenKind::RShiftEq => "RShiftEq".to_string(),
            TokenKind::Range => "Range".to_string(),
            TokenKind::Arrow => "Arrow".to_string(),
            TokenKind::Hash => "Hash".to_string(),
            TokenKind::StaticAccess => "StaticAccess".to_string(),
            TokenKind::LParen => "LParen".to_string(),
            TokenKind::RParen => "RParen".to_string(),
            TokenKind::LBrace => "LBrace".to_string(),
            TokenKind::RBrace => "RBrace".to_string(),
            TokenKind::LBracket => "LBracket".to_string(),
            TokenKind::RBracket => "RBracket".to_string(),
            TokenKind::Comma => "Comma".to_string(),
            TokenKind::Dot => "Dot".to_string(),
            TokenKind::Colon => "Colon".to_string(),
            TokenKind::Semicolon => "Semicolon".to_string(),
            TokenKind::If => "If".to_string(),
            TokenKind::Else => "Else".to_string(),
            TokenKind::While => "While".to_string(),
            TokenKind::For => "For".to_string(),
            TokenKind::Break => "Break".to_string(),
            TokenKind::Continue => "Continue".to_string(),
            TokenKind::Return => "Return".to_string(),
            TokenKind::Class => "Class".to_string(),
            TokenKind::Fn => "Fn".to_string(),
            TokenKind::Let => "Let".to_string(),
            TokenKind::Const => "Const".to_string(),
            TokenKind::Public => "Public".to_string(),
            TokenKind::Private => "Private".to_string(),
            TokenKind::Protected => "Protected".to_string(),
            TokenKind::Static => "Static".to_string(),
            TokenKind::Import => "Import".to_string(),
            TokenKind::As => "As".to_string(),
            TokenKind::New => "New".to_string(),
            TokenKind::Extern => "Extern".to_string(),
            TokenKind::Extend => "Extend".to_string(),
            TokenKind::Error => "Error".to_string()
        }
    }
}