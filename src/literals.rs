use std::fmt;

#[derive(Clone, PartialEq)]
pub enum Literal {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Number(n) => write!(f, "{n}{}", if n.round() == *n { ".0" } else { "" }),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Number(n) => write!(f, "{n}"),
        }
    }
}

impl From<Literal> for bool {
    fn from(literal: Literal) -> bool {
        match literal {
            Literal::Boolean(b) => b,
            Literal::Nil => false,
            _ => true,
        }
    }
}

impl From<Literal> for String {
    fn from(literal: Literal) -> String {
        match literal {
            Literal::Nil => "nil".into(),
            Literal::Boolean(b) => format!("{b}"),
            Literal::String(s) => s,
            Literal::Number(n) => format!("{n}"),
        }
    }
}
