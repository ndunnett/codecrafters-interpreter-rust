use std::fmt;

use crate::{
    parsing::{BinaryOperator, Expr, UnaryOperator},
    scanning::Literal,
};

#[derive(Debug, Clone)]
pub enum InterpreterError {
    TypeError(String),
    RuntimeError(String),
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeError(msg) => write!(f, "TypeError: {msg}"),
            Self::RuntimeError(msg) => write!(f, "RuntimeError: {msg}"),
        }
    }
}

pub struct Interpreter {
    expr: Expr,
}

impl Interpreter {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }

    pub fn evaluate(&self) -> Result<String, InterpreterError> {
        expression(&self.expr).map(String::from)
    }
}

fn expression(expr: &Expr) -> Result<Literal, InterpreterError> {
    match expr {
        Expr::None => Ok(Literal::Nil),
        Expr::Literal(literal) => Ok(literal.clone()),
        Expr::Grouping(inner) => expression(inner),
        Expr::Unary { op, right } => unary(op, right),
        Expr::Binary { op, left, right } => binary(op, left, right),
        Expr::Program(exprs) => program(exprs),
    }
}

fn unary(op: &UnaryOperator, right: &Expr) -> Result<Literal, InterpreterError> {
    let value = expression(right)?;

    match op {
        UnaryOperator::Negation => Ok(Literal::Boolean(!bool::from(value))),
        UnaryOperator::Negative => match value {
            Literal::Number(a) => Ok(Literal::Number(-a)),
            _ => Err(InterpreterError::TypeError("Expected a number.".into())),
        },
    }
}

fn binary(op: &BinaryOperator, left: &Expr, right: &Expr) -> Result<Literal, InterpreterError> {
    let left_val = expression(left)?;
    let right_val = expression(right)?;

    match op {
        BinaryOperator::Addition => add(left_val, right_val),
        BinaryOperator::Subtraction => subtract(left_val, right_val),
        BinaryOperator::Multiplication => multiply(left_val, right_val),
        BinaryOperator::Division => divide(left_val, right_val),
        BinaryOperator::Equality | BinaryOperator::NonEquality => equality(op, left_val, right_val),
        BinaryOperator::GreaterThan
        | BinaryOperator::GreaterThanEqual
        | BinaryOperator::LessThan
        | BinaryOperator::LessThanEqual => comparison(op, left_val, right_val),
    }
}

fn add(left: Literal, right: Literal) -> Result<Literal, InterpreterError> {
    match (left, right) {
        (Literal::Number(a), Literal::Number(b)) => Ok(Literal::Number(a + b)),
        (Literal::String(a), Literal::String(b)) => Ok(Literal::String(a + &b)),
        _ => Err(InterpreterError::TypeError(
            "Expected either two strings or two numbers for addition.".into(),
        )),
    }
}

fn subtract(left: Literal, right: Literal) -> Result<Literal, InterpreterError> {
    match (left, right) {
        (Literal::Number(a), Literal::Number(b)) => Ok(Literal::Number(a - b)),
        _ => Err(InterpreterError::TypeError(
            "Expected two numbers for subtraction.".into(),
        )),
    }
}

fn multiply(left: Literal, right: Literal) -> Result<Literal, InterpreterError> {
    match (left, right) {
        (Literal::Number(a), Literal::Number(b)) => Ok(Literal::Number(a * b)),
        _ => Err(InterpreterError::TypeError(
            "Expected two numbers for multiplication.".into(),
        )),
    }
}

fn divide(left: Literal, right: Literal) -> Result<Literal, InterpreterError> {
    match (left, right) {
        (Literal::Number(a), Literal::Number(b)) => {
            if b == 0. {
                Err(InterpreterError::RuntimeError("Divide by zero.".into()))
            } else {
                Ok(Literal::Number(a / b))
            }
        }
        _ => Err(InterpreterError::TypeError(
            "Expected two numbers for division.".into(),
        )),
    }
}

fn equality(
    op: &BinaryOperator,
    left: Literal,
    right: Literal,
) -> Result<Literal, InterpreterError> {
    match op {
        BinaryOperator::Equality => Ok(Literal::Boolean(left == right)),
        BinaryOperator::NonEquality => Ok(Literal::Boolean(left != right)),
        _ => unreachable!(),
    }
}

fn comparison(
    op: &BinaryOperator,
    left: Literal,
    right: Literal,
) -> Result<Literal, InterpreterError> {
    match (op, left, right) {
        (BinaryOperator::GreaterThan, Literal::Number(a), Literal::Number(b)) => {
            Ok(Literal::Boolean(a > b))
        }
        (BinaryOperator::GreaterThanEqual, Literal::Number(a), Literal::Number(b)) => {
            Ok(Literal::Boolean(a >= b))
        }
        (BinaryOperator::LessThan, Literal::Number(a), Literal::Number(b)) => {
            Ok(Literal::Boolean(a < b))
        }
        (BinaryOperator::LessThanEqual, Literal::Number(a), Literal::Number(b)) => {
            Ok(Literal::Boolean(a <= b))
        }
        _ => Err(InterpreterError::TypeError(
            "Expected two numbers for comparison.".into(),
        )),
    }
}

fn program(_exprs: &[Expr]) -> Result<Literal, InterpreterError> {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    fn happy_case(input: &str, expected: &str) {
        let (tokens, scan_errors) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = crate::parsing::Parser::new(tokens);
        let (expressions, parse_errors) = parser.parse_tokens();
        assert!(scan_errors.is_empty());
        assert!(parse_errors.is_empty());

        let result = super::Interpreter::new(expressions).evaluate();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), expected);
    }

    fn sad_case(input: &str, _expected: &str) {
        let (tokens, scan_errors) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = crate::parsing::Parser::new(tokens);
        let (expressions, parse_errors) = parser.parse_tokens();
        assert!(scan_errors.is_empty());
        assert!(parse_errors.is_empty());

        let result = super::Interpreter::new(expressions).evaluate();
        assert!(result.is_err());
    }

    #[test]
    fn booleans_and_nil() {
        happy_case("2 + 3", "5");
    }
}
