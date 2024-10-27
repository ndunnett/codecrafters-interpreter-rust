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

#[derive(Debug, Clone, Copy)]
pub enum LogicalOperator {
    And,
    Or,
}

impl From<TokenType> for LogicalOperator {
    fn from(type_: TokenType) -> Self {
        match type_ {
            TokenType::And => LogicalOperator::And,
            TokenType::Or => LogicalOperator::Or,
            _ => panic!("faulty LogicalOperator conversion"),
        }
    }
}

impl fmt::Display for LogicalOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
        }
    }
}

#[derive(Clone)]
pub enum Statement {
    VarDecl(String, Option<Expr>),
    Expr(Expr),
    Print(Expr),
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::VarDecl(name, init) => {
                if let Some(init) = init {
                    write!(f, "(var {name} = {init:?})")
                } else {
                    write!(f, "(var {name})")
                }
            }
            Self::Expr(expr) => write!(f, "{expr:?}"),
            Self::Print(expr) => write!(f, "(print {expr:?})"),
            Self::Block(stmts) => write!(
                f,
                "(block {})",
                stmts
                    .iter()
                    .map(|s| format!("{s:?}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::If(expr, then, else_) => {
                if let Some(else_) = else_ {
                    write!(f, "(if {expr:?} then {:?} else {else_:?})", *then)
                } else {
                    write!(f, "(if {expr:?} then {:?})", *then)
                }
            }
            Self::While(expr, body) => write!(f, "(while {expr:?} then {:?})", *body),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::VarDecl(name, init) => {
                if let Some(init) = init {
                    write!(f, "(var {name} = {init})")
                } else {
                    write!(f, "(var {name})")
                }
            }
            Self::Expr(expr) => write!(f, "{expr}"),
            Self::Print(expr) => write!(f, "(print {expr})"),
            Self::Block(stmts) => write!(
                f,
                "(block {})",
                stmts
                    .iter()
                    .map(|s| format!("{s}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::If(expr, then, else_) => {
                if let Some(else_) = else_ {
                    write!(f, "(if {expr} then {} else {else_})", *then)
                } else {
                    write!(f, "(if {expr} then {})", *then)
                }
            }
            Self::While(expr, body) => write!(f, "(while {expr} then {})", *body),
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
    Assignment(String, Box<Expr>),
    Variable(String),
    Logical {
        op: LogicalOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
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
            Self::Assignment(name, expr) => write!(f, "(assign {name}, {expr:?})"),
            Self::Variable(name) => write!(f, "{name}"),
            Self::Logical { op, left, right } => write!(f, "({op} {left:?} {right:?})"),
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
            Self::Assignment(name, expr) => write!(f, "(assign {name}, {expr})"),
            Self::Variable(name) => write!(f, "{name}"),
            Self::Logical { op, left, right } => write!(f, "({op} {left} {right})"),
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
