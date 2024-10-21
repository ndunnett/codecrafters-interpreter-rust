use std::collections::HashMap;
use std::fmt;

use crate::{
    literals::Literal,
    tokens::{Token, TokenType},
};

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
    tokens: Vec<Token<'a>>,
    errors: Vec<ScannerError>,
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
            errors: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            keywords: KEYWORDS.into_iter().collect(),
        }
    }

    pub fn scan_tokens(&mut self) -> (Vec<Token<'a>>, Vec<ScannerError>) {
        while !self.is_at_end() {
            self.scan_token();
        }

        self.add_token(TokenType::EndOfFile);
        (self.tokens.clone(), self.errors.clone())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn set_start(&mut self) {
        self.start = self.current;
    }

    fn new_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }

    fn get(&self, index: usize) -> &str {
        // we're not interested in unicode around these parts
        // https://doc.rust-lang.org/book/ch08-02-strings.html
        self.source.get(index..=index).unwrap_or("\0")
    }

    fn get_char(&self, index: usize) -> char {
        // not proud of this
        self.get(index).chars().next().unwrap()
    }

    fn advance(&mut self) -> &str {
        self.current += 1;
        self.column += 1;
        self.get(self.current - 1)
    }

    fn peek(&self) -> &str {
        self.get(self.current)
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

    fn add_token(&mut self, type_: TokenType) {
        let literal = match type_ {
            TokenType::String => self
                .source
                .get(self.start..self.current)
                .map(String::from)
                .map(Literal::String)
                .unwrap_or(Literal::Nil),
            TokenType::Number => self
                .source
                .get(self.start..self.current)
                .map(str::parse)
                .unwrap_or(Ok(0.))
                .map(Literal::Number)
                .unwrap_or(Literal::Nil),
            TokenType::True => Literal::Boolean(true),
            TokenType::False => Literal::Boolean(false),
            _ => Literal::Nil,
        };

        self.tokens.push(Token {
            type_,
            lexeme: self.source.get(self.start..self.current),
            literal,
            line: self.line,
            column: self.column,
        });

        self.set_start();
    }

    fn add_error(&mut self, error_type: ScannerErrorType) {
        self.errors.push(ScannerError {
            type_: error_type,
            line: self.line,
            column: self.column,
        });
        self.set_start();
    }

    fn scan_token(&mut self) {
        match self.advance() {
            "\t" | " " => self.set_start(),
            "\n" => {
                self.new_line();
                self.set_start();
            }
            "(" => self.add_token(TokenType::LeftParen),
            ")" => self.add_token(TokenType::RightParen),
            "{" => self.add_token(TokenType::LeftBrace),
            "}" => self.add_token(TokenType::RightBrace),
            "*" => self.add_token(TokenType::Star),
            "." => self.add_token(TokenType::Dot),
            "," => self.add_token(TokenType::Comma),
            ";" => self.add_token(TokenType::Semicolon),
            "+" => self.add_token(TokenType::Plus),
            "-" => self.add_token(TokenType::Minus),
            "=" => {
                if self.matches("=") {
                    self.add_token(TokenType::DoubleEqual);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            "!" => {
                if self.matches("=") {
                    self.add_token(TokenType::BangEqual);
                } else {
                    self.add_token(TokenType::Bang);
                }
            }
            "<" => {
                if self.matches("=") {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::Less);
                }
            }
            ">" => {
                if self.matches("=") {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::Greater);
                }
            }
            "/" => {
                if self.matches("/") {
                    while !self.matches("\n") && !self.is_at_end() {
                        self.advance();
                    }
                    self.new_line();
                    self.set_start();
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            "\"" => self.string_literal(),
            _ => {
                let c = self.get_char(self.current - 1);

                if c.is_numeric() {
                    self.number_literal();
                } else if c.is_alphabetic() || c == '_' {
                    self.identifier();
                } else {
                    let e = ScannerErrorType::UnexpectedCharacter(c.into());
                    self.add_error(e);
                }
            }
        }
    }

    fn string_literal(&mut self) {
        self.set_start();

        loop {
            match self.peek() {
                "\"" => break,
                "\n" => self.new_line(),
                _ => {
                    self.advance();
                }
            }

            if self.is_at_end() {
                self.add_error(ScannerErrorType::UnterminatedString);
                return;
            }
        }

        self.add_token(TokenType::String);
        self.advance();
        self.set_start();
    }

    fn number_literal(&mut self) {
        while self.get_char(self.current).is_numeric() {
            self.advance();
        }

        if self.peek() == "." && self.get_char(self.current + 1).is_numeric() {
            self.advance();
        }

        while self.get_char(self.current).is_numeric() {
            self.advance();
        }

        self.add_token(TokenType::Number);
    }

    fn identifier(&mut self) {
        while self.peek().chars().all(|c| c.is_alphanumeric() || c == '_') {
            self.advance();
        }

        let name = self.source.get(self.start..self.current).unwrap();

        if let Some(&keyword) = self.keywords.get(name) {
            self.add_token(keyword);
        } else {
            self.add_token(TokenType::Identifier);
        }
    }
}

#[cfg(test)]
mod tests {
    fn happy_case(input: &str, expected: &str) {
        let (tokens, errors) = super::Scanner::new(input).scan_tokens();

        assert!(errors.is_empty());

        let token_output = tokens
            .iter()
            .map(|token| format!("{token}"))
            .collect::<Vec<_>>()
            .join("\n");

        assert_eq!(token_output, expected);
    }

    fn sad_case(input: &str, expected: &str) {
        let (tokens, errors) = super::Scanner::new(input).scan_tokens();

        assert!(!errors.is_empty());

        let error_output = errors
            .iter()
            .map(|e| format!("{e}"))
            .collect::<Vec<_>>()
            .join("\n");

        let token_output = tokens
            .iter()
            .map(|token| format!("{token}"))
            .collect::<Vec<_>>()
            .join("\n");

        assert_eq!([error_output, token_output].join("\n"), expected);
    }

    #[test]
    fn parentheses() {
        let expected = "LEFT_PAREN ( null
LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";

        happy_case("(()", expected);
    }

    #[test]
    fn braces() {
        let expected = "LEFT_BRACE { null
LEFT_BRACE { null
RIGHT_BRACE } null
RIGHT_BRACE } null
EOF  null";

        happy_case("{{}}", expected);
    }

    #[test]
    fn other_single_chars() {
        let expected = "LEFT_PAREN ( null
LEFT_BRACE { null
STAR * null
DOT . null
COMMA , null
PLUS + null
STAR * null
RIGHT_BRACE } null
RIGHT_PAREN ) null
EOF  null";

        happy_case("({*.,+*})", expected);
    }

    #[test]
    fn lexical_errors() {
        let expected = "[line 1] Error: Unexpected character: $
[line 1] Error: Unexpected character: #
COMMA , null
DOT . null
LEFT_PAREN ( null
EOF  null";

        sad_case(",.$(#", expected);
    }

    #[test]
    fn assignment_and_equality() {
        let expected = "EQUAL = null
LEFT_BRACE { null
EQUAL_EQUAL == null
EQUAL = null
RIGHT_BRACE } null
EOF  null";

        happy_case("={===}", expected);
    }

    #[test]
    fn negation_and_inequality() {
        let expected = "BANG ! null
BANG_EQUAL != null
EQUAL_EQUAL == null
EOF  null";

        happy_case("!!===", expected);
    }

    #[test]
    fn relational_operators() {
        let expected = "LESS < null
LESS_EQUAL <= null
GREATER > null
GREATER_EQUAL >= null
EOF  null";

        happy_case("<<=>>=", expected);
    }

    #[test]
    fn division_and_comments() {
        let expected1 = "LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";

        happy_case("()// Comment", expected1);

        let expected2 = "SLASH / null
LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";

        happy_case("/()", expected2);

        let expected3 = "EOF  null";

        happy_case("///Unicode:£§᯽☺♣", expected3);
    }

    #[test]
    fn whitespace() {
        let expected = "LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";

        happy_case("(\t\n )", expected);
    }

    #[test]
    fn multiline_errors() {
        let expected1 = "[line 1] Error: Unexpected character: #
[line 2] Error: Unexpected character: @
LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";

        sad_case("# (\n)\t@", expected1);

        let expected2 = "[line 1] Error: Unexpected character: #
[line 2] Error: Unexpected character: @
[line 3] Error: Unexpected character: $
[line 7] Error: Unexpected character: #
LEFT_PAREN ( null
RIGHT_PAREN ) null
LEFT_BRACE { null
RIGHT_BRACE } null
PLUS + null
PLUS + null
PLUS + null
PLUS + null
PLUS + null
PLUS + null
EOF  null";

        sad_case("()  #\t{}\n@\n$\n+++\n// Let's Go!\n+++\n#", expected2);
    }

    #[test]
    fn string_literals() {
        let expected1 = "STRING \"foo baz\" foo baz
EOF  null";

        happy_case("\"foo baz\"", expected1);

        let expected2 = "[line 1] Error: Unterminated string.
EOF  null";

        sad_case("\"bar", expected2);
    }

    #[test]
    fn number_literals() {
        let expected1 = "NUMBER 42 42.0
EOF  null";

        happy_case("42", expected1);

        let expected2 = "NUMBER 1234.1234 1234.1234
EOF  null";

        happy_case("1234.1234", expected2);
    }

    #[test]
    fn identifiers() {
        let expected = "IDENTIFIER foo null
IDENTIFIER bar null
IDENTIFIER _hello null
EOF  null";

        happy_case("foo bar _hello", expected);
    }
}
