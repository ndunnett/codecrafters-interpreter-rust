use std::fmt;

use crate::{
    ast::{Expr, Statement},
    tokens::{Token, TokenType},
};

#[derive(Debug, Clone)]
pub enum ParserErrorType {
    UnexpectedToken(String),
    ExpectedToken(String),
    InvalidAssignmentTarget,
    InvalidAssignmentValue,
    IncompleteParse,
}

impl fmt::Display for ParserErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken(s) => write!(f, "Unexpected token: {s}"),
            Self::ExpectedToken(s) => write!(f, "Expected token: {s}"),
            Self::InvalidAssignmentTarget => write!(f, "Invalid assignment target."),
            Self::InvalidAssignmentValue => write!(f, "Invalid assignment value."),
            Self::IncompleteParse => write!(f, "Failed to consume all tokens."),
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

    pub fn parse_program(&mut self) -> (Vec<Statement>, Vec<ParserError>) {
        let mut program = Vec::new();

        while let Some(stmt) = self.statement() {
            program.push(stmt);

            if self.is_at_end() {
                break;
            }
        }

        if !self.is_at_end() {
            self.add_error(ParserErrorType::IncompleteParse);
        }

        (program, self.errors.clone())
    }

    pub fn parse_expression(&mut self) -> (Option<Expr>, Vec<ParserError>) {
        let expr = self.expression();

        if self.matches(&TokenType::Semicolon) {
            self.advance();
        }

        if !self.is_at_end() {
            self.add_error(ParserErrorType::IncompleteParse);
        }

        (expr, self.errors.clone())
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

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1
        }
    }

    fn consume(&mut self, type_: &TokenType) -> Option<&Token> {
        if self.matches(type_) {
            self.advance();
            Some(self.previous())
        } else {
            self.add_error(ParserErrorType::ExpectedToken(format!(
                "{}, found {}",
                type_,
                self.peek(),
            )));
            None
        }
    }

    fn add_error(&mut self, error_type: ParserErrorType) {
        let token = self.peek();

        self.errors.push(ParserError {
            type_: error_type,
            line: token.line,
            column: token.column,
        });

        while !self.is_at_end() {
            self.advance();

            if self.previous().type_ == TokenType::Semicolon {
                break;
            }
        }
    }

    fn statement(&mut self) -> Option<Statement> {
        match self.peek().type_ {
            TokenType::Var => {
                self.advance();
                self.variable_declaration()
            }
            TokenType::Print => {
                self.advance();
                self.print_stmt()
            }
            _ => self.expression_stmt(),
        }
    }

    fn variable_declaration(&mut self) -> Option<Statement> {
        let name = String::from(self.consume(&TokenType::Identifier)?.lexeme?);

        let initialiser = if self.matches(&TokenType::Equal) {
            self.advance();
            self.expression()
        } else {
            None
        };

        self.consume(&TokenType::Semicolon);
        Some(Statement::VarDecl(name, initialiser))
    }

    fn print_stmt(&mut self) -> Option<Statement> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon);
        Some(Statement::Print(expr))
    }

    fn expression_stmt(&mut self) -> Option<Statement> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon);
        Some(Statement::Expr(expr))
    }

    fn expression(&mut self) -> Option<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Option<Expr> {
        let expr = self.equality();

        if self.matches(&TokenType::Equal) {
            if let Some(Expr::Variable(name)) = expr {
                if let Some(value) = self.assignment() {
                    Some(Expr::Assignment(name, Box::new(value)))
                } else {
                    self.add_error(ParserErrorType::InvalidAssignmentValue);
                    None
                }
            } else {
                self.add_error(ParserErrorType::InvalidAssignmentTarget);
                None
            }
        } else {
            expr
        }
    }

    fn equality(&mut self) -> Option<Expr> {
        let mut expr = self.comparison()?;

        while self.matches_one_of(&[TokenType::BangEqual, TokenType::DoubleEqual]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.comparison()?);

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right,
            }
        }

        Some(expr)
    }

    fn comparison(&mut self) -> Option<Expr> {
        let mut expr = self.term()?;

        while self.matches_one_of(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.term()?);

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right,
            }
        }

        Some(expr)
    }

    fn term(&mut self) -> Option<Expr> {
        let mut expr = self.factor()?;

        while self.matches_one_of(&[TokenType::Minus, TokenType::Plus]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.factor()?);

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right,
            }
        }

        Some(expr)
    }

    fn factor(&mut self) -> Option<Expr> {
        let mut expr = self.unary()?;

        while self.matches_one_of(&[TokenType::Slash, TokenType::Star]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.unary()?);

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right,
            }
        }

        Some(expr)
    }

    fn unary(&mut self) -> Option<Expr> {
        if self.matches_one_of(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.peek().type_.into();
            self.advance();
            let right = Box::new(self.unary()?);
            Some(Expr::Unary { op, right })
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Option<Expr> {
        if self.matches_one_of(&[
            TokenType::Number,
            TokenType::String,
            TokenType::True,
            TokenType::False,
            TokenType::Nil,
        ]) {
            self.advance();
            Some(Expr::Literal(self.previous().literal.clone()))
        } else if self.matches(&TokenType::LeftParen) {
            self.advance();
            let expr = Expr::Grouping(Box::new(self.expression()?));

            if self.consume(&TokenType::RightParen).is_some() {
                Some(expr)
            } else {
                None
            }
        } else if self.matches(&TokenType::Identifier) {
            Some(Expr::Variable(String::from(
                self.consume(&TokenType::Identifier)?.lexeme?,
            )))
        } else {
            self.add_error(ParserErrorType::UnexpectedToken(format!("{}", self.peek())));
            None
        }
    }
}

#[cfg(test)]
mod expr_tests {
    fn happy_case(input: &str, expected: &str) {
        let (tokens, scan_errors) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = super::Parser::new(tokens);
        let (expr, parse_errors) = parser.parse_expression();
        assert!(scan_errors.is_empty());
        assert!(parse_errors.is_empty());
        assert!(expr.is_some());
        assert_eq!(format!("{:?}", expr.unwrap()), expected);
    }

    fn sad_case(input: &str, expected: &str) {
        let (tokens, scan_errors) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = super::Parser::new(tokens);
        let (expr, parse_errors) = parser.parse_expression();
        assert!(scan_errors.is_empty());
        assert!(!parse_errors.is_empty());

        let error_output = parse_errors
            .iter()
            .map(|e| format!("{e}"))
            .collect::<Vec<_>>()
            .join("\n");

        if let Some(expr) = expr {
            assert_eq!([error_output, format!("{expr:?}")].join("\n"), expected);
        } else {
            assert_eq!(error_output, expected);
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

    #[test]
    fn arithmetic_1() {
        happy_case("16 * 38 / 58", "(/ (* 16.0 38.0) 58.0)");
    }

    #[test]
    fn arithmetic_2() {
        happy_case("52 + 80 - 94", "(- (+ 52.0 80.0) 94.0)");
    }

    #[test]
    fn comparison() {
        happy_case("83 < 99 < 115", "(< (< 83.0 99.0) 115.0)");
    }

    #[test]
    fn equality() {
        happy_case("\"baz\" == \"baz\"", "(== baz baz)");
    }

    #[test]
    fn syntactic_errors() {
        let input = "(72 +)";
        let (tokens, _) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = super::Parser::new(tokens);
        let (_, errors) = parser.parse_program();
        assert!(!errors.is_empty());
    }
}
