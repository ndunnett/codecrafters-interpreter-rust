use std::fmt;

use crate::literals::Literal;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
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

impl From<TokenType> for String {
    fn from(type_: TokenType) -> String {
        format!("{type_}")
    }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub type_: TokenType,
    pub lexeme: Option<&'a str>,
    pub literal: Literal,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (lex, lit) = match &self.literal {
            Literal::Nil => (self.lexeme.unwrap_or("").into(), "null".into()),
            Literal::Boolean(_) => (self.lexeme.unwrap_or("").into(), "null".into()),
            Literal::String(s) => (format!("\"{}\"", self.lexeme.unwrap_or("")), s.into()),
            Literal::Number(n) => (self.lexeme.unwrap_or("").into(), format!("{n:?}")),
        };

        write!(f, "{} {lex} {lit}", self.type_)
    }
}

impl From<Token<'_>> for String {
    fn from(token: Token<'_>) -> String {
        format!("{token}")
    }
}
