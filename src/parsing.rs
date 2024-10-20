use std::fmt;

use crate::scanning::{Literal, Token, TokenType};

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Negative,
    Negation,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Negative => write!(f, "-"),
            Self::Negation => write!(f, "!"),
        }
    }
}

impl From<TokenType> for UnaryOperator {
    fn from(type_: TokenType) -> Self {
        match type_ {
            TokenType::Minus => UnaryOperator::Negative,
            TokenType::Bang => UnaryOperator::Negation,
            _ => panic!("faulty UnaryOperator conversion"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Equality,
    NonEquality,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

impl From<TokenType> for BinaryOperator {
    fn from(type_: TokenType) -> Self {
        match type_ {
            TokenType::DoubleEqual => BinaryOperator::Equality,
            TokenType::BangEqual => BinaryOperator::NonEquality,
            TokenType::Less => BinaryOperator::LessThan,
            TokenType::LessEqual => BinaryOperator::LessThanEqual,
            TokenType::Greater => BinaryOperator::GreaterThan,
            TokenType::GreaterEqual => BinaryOperator::GreaterThanEqual,
            TokenType::Plus => BinaryOperator::Addition,
            TokenType::Minus => BinaryOperator::Subtraction,
            TokenType::Star => BinaryOperator::Multiplication,
            TokenType::Slash => BinaryOperator::Division,
            _ => panic!("faulty BinaryOperator conversion"),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Equality => write!(f, "=="),
            Self::NonEquality => write!(f, "!="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanEqual => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanEqual => write!(f, ">="),
            Self::Addition => write!(f, "+"),
            Self::Subtraction => write!(f, "-"),
            Self::Multiplication => write!(f, "*"),
            Self::Division => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    None,
    Literal(Literal<'a>),
    Unary {
        op: UnaryOperator,
        right: Box<Expr<'a>>,
    },
    Binary {
        op: BinaryOperator,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
    Grouping(Box<Expr<'a>>),
    Program(Vec<Expr<'a>>),
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => Ok(()),
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Unary { op, right } => write!(f, "({op} {right})"),
            Self::Binary { op, left, right } => write!(f, "({op} {left} {right})"),
            Self::Grouping(expr) => write!(f, "(group {expr})"),
            Self::Program(prog) => write!(
                f,
                "{}",
                prog.iter()
                    .map(|e| format!("{e}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParserErrorType {
    UnexpectedToken(String),
    ExpectedToken(String),
}

impl fmt::Display for ParserErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken(s) => write!(f, "Unexpected token: {s}"),
            Self::ExpectedToken(s) => write!(f, "Expected token: {s}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParserError {
    type_: ParserErrorType,
    line: usize,
    column: usize,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.type_)
    }
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    errors: Vec<ParserError>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            tokens,
            errors: Vec::new(),
            current: 0,
        }
    }

    pub fn parse_tokens(&mut self) -> (Expr<'a>, Vec<ParserError>) {
        let errors = self.errors.clone();
        let expr = self.expression();
        (expr, errors)
    }

    fn is_at_end(&self) -> bool {
        self.peek().type_ == TokenType::EndOfFile
    }

    fn matches(&self, type_: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().type_ == *type_
        }
    }

    fn matches_one_of(&self, types: &[TokenType]) -> bool {
        types.iter().any(|type_| self.matches(type_))
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1
        }

        self.previous()
    }

    fn consume(&mut self, type_: &TokenType) -> bool {
        if self.matches(type_) {
            self.advance();
            true
        } else {
            self.add_error(ParserErrorType::ExpectedToken(format!(
                "{}, found {}",
                type_,
                self.peek(),
            )));
            false
        }
    }

    fn add_error(&mut self, error_type: ParserErrorType) {
        let token = self.peek();

        self.errors.push(ParserError {
            type_: error_type,
            line: token.line,
            column: token.column,
        });

        self.advance();

        while !self.is_at_end() {
            if self.previous().type_ == TokenType::Semicolon {
                break;
            } else {
                self.advance();
            }
        }
    }

    fn expression(&mut self) -> Expr<'a> {
        self.equality()
    }

    fn equality(&mut self) -> Expr<'a> {
        let left = self.comparison();

        if self.matches_one_of(&[TokenType::BangEqual, TokenType::DoubleEqual]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.comparison());
            Expr::Binary {
                op,
                left: Box::new(left),
                right,
            }
        } else {
            left
        }
    }

    fn comparison(&mut self) -> Expr<'a> {
        let left = self.term();

        if self.matches_one_of(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.term());
            Expr::Binary {
                op,
                left: Box::new(left),
                right,
            }
        } else {
            left
        }
    }

    fn term(&mut self) -> Expr<'a> {
        let left = self.factor();

        if self.matches_one_of(&[TokenType::Minus, TokenType::Plus]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.factor());
            Expr::Binary {
                op,
                left: Box::new(left),
                right,
            }
        } else {
            left
        }
    }

    fn factor(&mut self) -> Expr<'a> {
        let left = self.unary();

        if self.matches_one_of(&[TokenType::Slash, TokenType::Star]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.unary());
            Expr::Binary {
                op,
                left: Box::new(left),
                right,
            }
        } else {
            left
        }
    }

    fn unary(&mut self) -> Expr<'a> {
        if self.matches_one_of(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.unary());
            Expr::Unary { op, right }
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expr<'a> {
        if self.matches_one_of(&[
            TokenType::Number,
            TokenType::String,
            TokenType::True,
            TokenType::False,
            TokenType::Nil,
        ]) {
            let expr = Expr::Literal(self.tokens[self.current].literal);
            self.advance();
            expr
        } else if self.matches(&TokenType::LeftParen) {
            self.advance();
            let expr = Expr::Grouping(Box::new(self.expression()));

            if self.consume(&TokenType::RightParen) {
                expr
            } else {
                Expr::None
            }
        } else {
            self.add_error(ParserErrorType::UnexpectedToken(format!("{}", self.peek())));
            Expr::None
        }
    }
}

#[cfg(test)]
mod tests {
    fn happy_case(input: &str, expected: &str) {
        let (tokens, _) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = super::Parser::new(tokens);
        let (expressions, errors) = parser.parse_tokens();

        if errors.is_empty() {
            assert_eq!(format!("{expressions}"), expected);
        } else {
            for e in errors {
                eprintln!("{e}")
            }
            panic!("failed to parse program");
        }
    }

    fn sad_case(input: &str, expected: &str) {
        let (tokens, _) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = super::Parser::new(tokens);
        let (expressions, errors) = parser.parse_tokens();

        if !errors.is_empty() {
            let error_output = errors
                .iter()
                .map(|e| format!("{e}"))
                .collect::<Vec<_>>()
                .join("\n");

            assert_eq!(
                [error_output, format!("{expressions}")].join("\n"),
                expected
            );
        } else {
            panic!("failed to return errors");
        }
    }

    #[test]
    fn booleans_and_nil() {
        happy_case("true", "true");
    }

    #[test]
    fn number_literals() {
        happy_case("42.47", "42.47");
    }

    #[test]
    fn string_literals() {
        happy_case("\"hello\"", "hello");
    }

    #[test]
    fn parentheses() {
        happy_case("(\"foo\")", "(group foo)");
    }

    #[test]
    fn unary_operators() {
        happy_case("!true", "(! true)");
    }
}
