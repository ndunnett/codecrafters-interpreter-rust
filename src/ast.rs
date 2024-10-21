use std::fmt;

use crate::{literals::Literal, tokens::TokenType};

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

#[derive(Clone)]
pub enum Statement {
    Expr(Expr),
    Print(Expr),
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => write!(f, "{expr:?}"),
            Self::Print(expr) => write!(f, "(print {expr:?})"),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => write!(f, "{expr}"),
            Self::Print(expr) => write!(f, "(print {expr})"),
        }
    }
}

impl From<Statement> for String {
    fn from(statement: Statement) -> String {
        format!("{statement}")
    }
}

#[derive(Clone)]
pub enum Expr {
    Literal(Literal),
    Unary {
        op: UnaryOperator,
        right: Box<Expr>,
    },
    Binary {
        op: BinaryOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal:?}"),
            Self::Unary { op, right } => write!(f, "({op} {right:?})"),
            Self::Binary { op, left, right } => write!(f, "({op} {left:?} {right:?})"),
            Self::Grouping(expr) => write!(f, "(group {expr:?})"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Unary { op, right } => write!(f, "({op} {right})"),
            Self::Binary { op, left, right } => write!(f, "({op} {left} {right})"),
            Self::Grouping(expr) => write!(f, "(group {expr})"),
        }
    }
}

impl From<Expr> for String {
    fn from(expr: Expr) -> String {
        format!("{expr}")
    }
}
