use std::{
    fmt,
    io::{self, Write},
};

use crate::{
    ast::{BinaryOperator, Expr, Statement, UnaryOperator},
    literals::Literal,
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

#[derive(Default, Debug, Clone)]
pub struct Buffer {
    buf: Vec<u8>,
}

impl Buffer {
    pub fn new() -> Self {
        Self::default()
    }
}

impl From<Buffer> for String {
    fn from(buffer: Buffer) -> String {
        String::from_utf8(buffer.buf).unwrap()
    }
}

#[derive(Debug)]
pub enum Output<'a> {
    Null,
    Buffer(&'a mut Buffer),
    Stdout(io::Stdout),
    Stderr(io::Stderr),
}

impl From<io::Stdout> for Output<'_> {
    fn from(output: io::Stdout) -> Self {
        Self::Stdout(output)
    }
}

impl From<io::Stderr> for Output<'_> {
    fn from(output: io::Stderr) -> Self {
        Self::Stderr(output)
    }
}

impl<'a> From<&'a mut Buffer> for Output<'a> {
    fn from(output: &'a mut Buffer) -> Self {
        Self::Buffer(output)
    }
}

impl io::Write for Output<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match *self {
            Self::Null => Ok(0),
            Self::Buffer(ref mut w) => w.buf.write(buf),
            Self::Stdout(ref mut w) => w.write(buf),
            Self::Stderr(ref mut w) => w.write(buf),
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        match *self {
            Self::Null => Ok(()),
            Self::Buffer(ref mut w) => w.buf.write_all(buf),
            Self::Stdout(ref mut w) => w.write_all(buf),
            Self::Stderr(ref mut w) => w.write_all(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match *self {
            Self::Null => Ok(()),
            Self::Buffer(ref mut w) => w.buf.flush(),
            Self::Stdout(ref mut w) => w.flush(),
            Self::Stderr(ref mut w) => w.flush(),
        }
    }
}

pub struct Interpreter<'a> {
    pub stdout: Output<'a>,
    pub stderr: Output<'a>,
}

impl Default for Interpreter<'_> {
    fn default() -> Self {
        Self {
            stdout: Output::Stdout(io::stdout()),
            stderr: Output::Stderr(io::stderr()),
        }
    }
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn redirect_stdout(&mut self, output: Output<'a>) {
        self.stdout = output;
    }

    pub fn redirect_stderr(&mut self, output: Output<'a>) {
        self.stderr = output;
    }

    pub fn evaluate(&self, program: &[Statement]) -> Result<String, InterpreterError> {
        let mut buffer = Buffer::new();
        let mut output = Output::Buffer(&mut buffer);

        for stmt in program {
            let _ = match stmt {
                Statement::Expr(expr) => match expression(expr) {
                    Ok(result) => writeln!(output, "{result}"),
                    Err(e) => return Err(e),
                },
                Statement::Print(_) => {
                    return Err(InterpreterError::RuntimeError(
                        "Evaluation mode only supports simple expressions.".into(),
                    ))
                }
            };
        }

        Ok(buffer.into())
    }

    pub fn run(&mut self, program: &[Statement]) -> Result<(), InterpreterError> {
        for stmt in program {
            self.statement(stmt)?;
        }

        Ok(())
    }

    fn statement(&mut self, stmt: &Statement) -> Result<(), InterpreterError> {
        match stmt {
            Statement::Expr(expr) => match expression(expr) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            Statement::Print(expr) => match expression(expr) {
                Ok(result) => {
                    let _ = writeln!(self.stdout, "{result}");
                    Ok(())
                }
                Err(e) => Err(e),
            },
        }
    }
}

fn expression(expr: &Expr) -> Result<Literal, InterpreterError> {
    match expr {
        Expr::Literal(literal) => Ok(literal.clone()),
        Expr::Grouping(inner) => expression(inner),
        Expr::Unary { op, right } => unary(op, right),
        Expr::Binary { op, left, right } => binary(op, left, right),
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

#[cfg(test)]
mod eval_tests {
    fn happy_case(input: &str, expected: &str) {
        let (tokens, scan_errors) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = crate::parsing::Parser::new(tokens);
        let (program, parse_errors) = parser.parse();
        assert!(scan_errors.is_empty());
        assert!(parse_errors.is_empty());

        let result = super::Interpreter::new().evaluate(&program);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), format!("{expected}\n"));
    }

    fn sad_case(input: &str) {
        let (tokens, scan_errors) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = crate::parsing::Parser::new(tokens);
        let (program, parse_errors) = parser.parse();
        assert!(scan_errors.is_empty());
        assert!(parse_errors.is_empty());
        assert!(!program.is_empty());

        let result = super::Interpreter::new().evaluate(&program);
        assert!(result.is_err());
    }

    #[test]
    fn booleans_and_nil() {
        happy_case("true", "true");
        happy_case("false", "false");
        happy_case("nil", "nil");
    }

    #[test]
    fn strings_and_numbers() {
        happy_case("\"hello world!\"", "hello world!");
        happy_case("10.40", "10.4");
        happy_case("10", "10");
    }

    #[test]
    fn parentheses() {
        happy_case("(\"hello world!\")", "hello world!");
        happy_case("(true)", "true");
        happy_case("(10.40)", "10.4");
        happy_case("((false))", "false");
    }

    #[test]
    fn unary_operators() {
        happy_case("-73", "-73");
        happy_case("!true", "false");
        happy_case("!10.40", "false");
        happy_case("!((false))", "true");
    }

    #[test]
    fn arithmetic_1() {
        happy_case("42 / 5", "8.4");
        happy_case("18 * 3 / (3 * 6)", "3");
        happy_case("(10.40 * 2) / 2", "10.4");
    }

    #[test]
    fn arithmetic_2() {
        happy_case("70 - 65", "5");
        happy_case("69 - 93", "-24");
        happy_case("10.40 - 2", "8.4");
        happy_case("23 + 28 - (-(61 - 99))", "13");
    }

    #[test]
    fn string_concatenation() {
        happy_case("\"hello\" + \" world!\"", "hello world!");
        happy_case("\"42\" + \"24\"", "4224");
        happy_case("\"foo\" + \"bar\"", "foobar");
    }

    #[test]
    fn relational_operators() {
        happy_case("57 > -65", "true");
        happy_case("11 >= 11", "true");
        happy_case("(54 - 67) >= -(114 / 57 + 11)", "true");
    }

    #[test]
    fn equality_operators() {
        happy_case("\"hello\" == \"world\"", "false");
        happy_case("\"foo\" != \"bar\"", "true");
        happy_case("\"foo\" == \"foo\"", "true");
        happy_case("61 == \"61\"", "false");
    }

    #[test]
    fn error_unary() {
        sad_case("-\"foo\"");
        sad_case("-true");
        sad_case("-(\"foo\" + \"bar\")");
        sad_case("-false");
    }

    #[test]
    fn error_binary_1() {
        sad_case("\"foo\" * 42");
        sad_case("true / 2");
        sad_case("\"foo\" * \"bar\"");
        sad_case("false / true");
    }

    #[test]
    fn error_binary_2() {
        sad_case("\"foo\" + true");
        sad_case("42 - true");
        sad_case("true + false");
        sad_case("\"foo\" - \"bar\"");
    }

    #[test]
    fn error_relational() {
        sad_case("\"foo\" < false");
        sad_case("true < 2");
        sad_case("(\"foo\" - \"bar\") < 42");
        sad_case("false > true");
    }
}

#[cfg(test)]
mod run_tests {
    fn happy_case(input: &str, expected: &str) {
        let (tokens, scan_errors) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = crate::parsing::Parser::new(tokens);
        let (program, parse_errors) = parser.parse();
        assert!(scan_errors.is_empty());
        assert!(parse_errors.is_empty());

        let mut buffer = super::Buffer::new();
        let output = super::Output::Buffer(&mut buffer);
        let mut interpreter = super::Interpreter::new();
        interpreter.redirect_stdout(output);

        assert!(interpreter.run(&program).is_ok());
        assert_eq!(String::from(buffer), format!("{expected}\n"));
    }

    fn sad_case(input: &str) {
        let (tokens, scan_errors) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = crate::parsing::Parser::new(tokens);
        let (program, parse_errors) = parser.parse();
        assert!(scan_errors.is_empty());
        assert!(parse_errors.is_empty());

        let result = super::Interpreter::new().run(&program);
        assert!(result.is_err());
    }

    #[test]
    fn print() {
        happy_case("print \"Hello, World!\";", "Hello, World!");
        happy_case("print 42;", "42");
        happy_case("print 12 + 24;", "36");
    }

    #[test]
    fn print_multiple() {
        happy_case(
            "print \"world\" + \"baz\" + \"bar\";
print 27 - 26;
print \"bar\" == \"quz\";",
            "worldbazbar
1
false",
        );

        happy_case(
            "print \"hello\"; print true;
print false;
print \"bar\"; print 43;",
            "hello
true
false
bar
43",
        );

        happy_case(
            "print 81;
    print 81 + 46;
        print 81 + 46 + 19;",
            "81
127
146",
        );

        happy_case(
            "print true != true;

print \"36
10
78
\";

print \"There should be an empty line above this.\";",
            "false
36
10
78

There should be an empty line above this.",
        );
    }
}
