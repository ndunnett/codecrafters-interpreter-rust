use std::collections::HashMap;
use std::fmt;
use std::ops::Index;

#[derive(Debug, Clone, Copy)]
enum TokenType {
    EndOfFile,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Star,
    Dot,
    Comma,
    Semicolon,
    Plus,
    Minus,
    Equal,
    DoubleEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Slash,
    String,
    Number,
    Identifier,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EndOfFile => write!(f, "EOF"),
            Self::LeftParen => write!(f, "LEFT_PAREN"),
            Self::RightParen => write!(f, "RIGHT_PAREN"),
            Self::LeftBrace => write!(f, "LEFT_BRACE"),
            Self::RightBrace => write!(f, "RIGHT_BRACE"),
            Self::Star => write!(f, "STAR"),
            Self::Dot => write!(f, "DOT"),
            Self::Comma => write!(f, "COMMA"),
            Self::Semicolon => write!(f, "SEMICOLON"),
            Self::Plus => write!(f, "PLUS"),
            Self::Minus => write!(f, "MINUS"),
            Self::Equal => write!(f, "EQUAL"),
            Self::DoubleEqual => write!(f, "EQUAL_EQUAL"),
            Self::Bang => write!(f, "BANG"),
            Self::BangEqual => write!(f, "BANG_EQUAL"),
            Self::Less => write!(f, "LESS"),
            Self::LessEqual => write!(f, "LESS_EQUAL"),
            Self::Greater => write!(f, "GREATER"),
            Self::GreaterEqual => write!(f, "GREATER_EQUAL"),
            Self::Slash => write!(f, "SLASH"),
            Self::String => write!(f, "STRING"),
            Self::Number => write!(f, "NUMBER"),
            Self::Identifier => write!(f, "IDENTIFIER"),
            Self::And => write!(f, "AND"),
            Self::Class => write!(f, "CLASS"),
            Self::Else => write!(f, "ELSE"),
            Self::False => write!(f, "FALSE"),
            Self::For => write!(f, "FOR"),
            Self::Fun => write!(f, "FUN"),
            Self::If => write!(f, "IF"),
            Self::Nil => write!(f, "NIL"),
            Self::Or => write!(f, "OR"),
            Self::Print => write!(f, "PRINT"),
            Self::Return => write!(f, "RETURN"),
            Self::Super => write!(f, "SUPER"),
            Self::This => write!(f, "THIS"),
            Self::True => write!(f, "TRUE"),
            Self::Var => write!(f, "VAR"),
            Self::While => write!(f, "WHILE"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Literal<'a> {
    Nil,
    Boolean(bool),
    String(&'a str),
    Number(f64),
}

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Number(n) => write!(f, "{n}{}", if n.round() == *n { ".0" } else { "" }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    type_: TokenType,
    lexeme: Option<&'a str>,
    literal: Literal<'a>,
    line: usize,
    column: usize,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (lex, lit) = match self.literal {
            Literal::Nil => (self.lexeme.unwrap_or("").to_string(), "null".to_string()),
            Literal::Boolean(_) => (self.lexeme.unwrap_or("").to_string(), "null".to_string()),
            Literal::String(s) => (format!("\"{}\"", self.lexeme.unwrap_or("")), s.to_string()),
            Literal::Number(n) => (
                self.lexeme.unwrap_or("").to_string(),
                format!("{n}{}", if n.round() == n { ".0" } else { "" }),
            ),
        };

        write!(f, "{} {lex} {lit}", self.type_)
    }
}

#[derive(Debug, Clone)]
enum ScannerErrorType {
    UnexpectedCharacter(String),
    UnterminatedString,
}

impl fmt::Display for ScannerErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedCharacter(s) => write!(f, "Unexpected character: {s}"),
            Self::UnterminatedString => write!(f, "Unterminated string."),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScannerError {
    type_: ScannerErrorType,
    line: usize,
    column: usize,
}

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.type_)
    }
}

const KEYWORDS: [(&str, TokenType); 16] = [
    ("and", TokenType::And),
    ("class", TokenType::Class),
    ("else", TokenType::Else),
    ("false", TokenType::False),
    ("for", TokenType::For),
    ("fun", TokenType::Fun),
    ("if", TokenType::If),
    ("nil", TokenType::Nil),
    ("or", TokenType::Or),
    ("print", TokenType::Print),
    ("return", TokenType::Return),
    ("super", TokenType::Super),
    ("this", TokenType::This),
    ("true", TokenType::True),
    ("var", TokenType::Var),
    ("while", TokenType::While),
];

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    source: &'a str,
    pub tokens: Vec<Token<'a>>,
    pub error: Option<ScannerError>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    keywords: HashMap<&'a str, TokenType>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            error: None,
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            keywords: KEYWORDS.into_iter().collect(),
        }
    }

    pub fn scan_tokens(&mut self) -> Self {
        while !self.is_at_end() && self.error.is_none() {
            self.start = self.current;
            self.scan_token();
        }

        if self.error.is_none() {
            self.add_token(TokenType::EndOfFile);
        }

        self.clone()
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            type_: token_type,
            lexeme: self.source.get(self.start..self.current),
            literal: Literal::Nil,
            line: self.line,
            column: self.column,
        });
        self.start = self.current;
    }

    fn set_error(&mut self, error_type: ScannerErrorType) {
        self.error = Some(ScannerError {
            type_: error_type,
            line: self.line,
            column: self.column,
        })
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> &str {
        self.current += 1;
        self.column += 1;
        self.source.index(self.current - 1..=self.current - 1)
    }

    fn peek(&self) -> &str {
        self.source.index(self.current..=self.current)
    }

    fn peek_next(&self) -> &str {
        self.source.index(self.current + 1..=self.current + 1)
    }

    fn matches(&mut self, c: &str) -> bool {
        if self.is_at_end() || self.peek() != c {
            false
        } else {
            self.current += 1;
            self.column += 1;
            true
        }
    }

    fn scan_token(&mut self) {
        match self.advance() {
            "(" => self.add_token(TokenType::LeftParen),
            ")" => self.add_token(TokenType::RightParen),
            s => {
                let e = ScannerErrorType::UnexpectedCharacter(s.into());
                self.set_error(e);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    fn runner(input: &str, expected: &str) {
        let scanner = super::Scanner::new(input).scan_tokens();

        if let Some(e) = scanner.error {
            panic!("failed to scan tokens: {e}");
        } else {
            let result = scanner
                .tokens
                .iter()
                .map(|token| format!("{token}"))
                .collect::<Vec<_>>()
                .join("\n");

            assert_eq!(result, expected);
        }
    }

    #[test]
    fn parentheses() {
        let expected = "LEFT_PAREN ( null
LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";

        runner("(()", expected);
    }
}
