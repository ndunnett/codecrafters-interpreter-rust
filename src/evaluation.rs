use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    io::{self, Write},
    rc::Rc,
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

#[derive(Default)]
struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    values: HashMap<String, Literal>,
}

impl Scope {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::default()))
    }

    pub fn child_from(parent: &Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent.clone()),
            ..Default::default()
        }))
    }

    pub fn get(&self, name: &str) -> Result<Literal, InterpreterError> {
        if let Some(value) = self.values.get(name) {
            Ok(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            Err(InterpreterError::RuntimeError(format!(
                "Undefined variable '{name}'."
            )))
        }
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        self.values.insert(String::from(name), value);
    }

    pub fn assign(&mut self, name: &str, value: Literal) {
        if self.values.contains_key(name) {
            self.values.insert(String::from(name), value);
        } else if let Some(parent) = &mut self.parent {
            parent.borrow_mut().assign(name, value);
        }
    }

    pub fn exists(&self, name: &str) -> bool {
        if self.values.contains_key(name) {
            true
        } else if let Some(parent) = &self.parent {
            parent.borrow().exists(name)
        } else {
            false
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

    pub fn evaluate(&mut self, expr: &Expr) -> Result<String, InterpreterError> {
        let mut buffer = Buffer::new();
        let mut output = Output::Buffer(&mut buffer);

        match expression(&Scope::new(), expr) {
            Ok(result) => {
                let _ = writeln!(output, "{result}");
                Ok(buffer.into())
            }
            Err(e) => Err(e),
        }
    }

    pub fn run(&mut self, program: &[Statement]) -> Result<(), InterpreterError> {
        let scope = Scope::new();

        for stmt in program {
            self.statement(&scope, stmt)?;
        }

        Ok(())
    }

    fn statement(
        &mut self,
        scope: &Rc<RefCell<Scope>>,
        stmt: &Statement,
    ) -> Result<(), InterpreterError> {
        match stmt {
            Statement::VarDecl(name, expr) => {
                if let Some(expr) = expr {
                    match expression(scope, expr) {
                        Ok(result) => {
                            scope.borrow_mut().define(name, result);
                            Ok(())
                        }
                        Err(e) => Err(e),
                    }
                } else {
                    scope.borrow_mut().define(name, Literal::Nil);
                    Ok(())
                }
            }
            Statement::Expr(expr) => match expression(scope, expr) {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            },
            Statement::Print(expr) => match expression(scope, expr) {
                Ok(result) => {
                    let _ = writeln!(self.stdout, "{result}");
                    Ok(())
                }
                Err(e) => Err(e),
            },
            Statement::Block(stmts) => {
                let child_scope = Scope::child_from(scope);

                for stmt in stmts {
                    self.statement(&child_scope, stmt)?;
                }

                Ok(())
            }
        }
    }
}

fn expression(scope: &Rc<RefCell<Scope>>, expr: &Expr) -> Result<Literal, InterpreterError> {
    match expr {
        Expr::Assignment(name, expr) => {
            if scope.borrow().exists(name) {
                let value = expression(scope, expr)?;
                scope.borrow_mut().assign(name, value.clone());
                Ok(value)
            } else {
                Err(InterpreterError::RuntimeError(format!(
                    "Undefined variable '{name}'."
                )))
            }
        }
        Expr::Variable(name) => Ok(scope.borrow().get(name)?),
        Expr::Literal(literal) => Ok(literal.clone()),
        Expr::Grouping(inner) => expression(scope, inner),
        Expr::Unary { op, right } => unary(scope, op, right),
        Expr::Binary { op, left, right } => binary(scope, op, left, right),
    }
}

fn unary(
    scope: &Rc<RefCell<Scope>>,
    op: &UnaryOperator,
    right: &Expr,
) -> Result<Literal, InterpreterError> {
    let value = expression(scope, right)?;

    match op {
        UnaryOperator::Negation => Ok(Literal::Boolean(!bool::from(value))),
        UnaryOperator::Negative => match value {
            Literal::Number(a) => Ok(Literal::Number(-a)),
            _ => Err(InterpreterError::TypeError("Expected a number.".into())),
        },
    }
}

fn binary(
    scope: &Rc<RefCell<Scope>>,
    op: &BinaryOperator,
    left: &Expr,
    right: &Expr,
) -> Result<Literal, InterpreterError> {
    let left_val = expression(scope, left)?;
    let right_val = expression(scope, right)?;

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
        let (expr, parse_errors) = parser.parse_expression();
        assert!(scan_errors.is_empty());
        assert!(parse_errors.is_empty());
        assert!(expr.is_some());

        let result = super::Interpreter::new().evaluate(&expr.unwrap());
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), format!("{expected}\n"));
    }

    fn sad_case(input: &str) {
        let (tokens, scan_errors) = crate::scanning::Scanner::new(input).scan_tokens();
        let mut parser = crate::parsing::Parser::new(tokens);
        let (expr, parse_errors) = parser.parse_expression();
        assert!(scan_errors.is_empty());
        assert!(parse_errors.is_empty());
        assert!(expr.is_some());

        let result = super::Interpreter::new().evaluate(&expr.unwrap());
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
        let (program, parse_errors) = parser.parse_program();
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
        let (program, parse_errors) = parser.parse_program();
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

    #[test]
    fn expression_statements() {
        happy_case(
            "(37 + 42 - 21) > (76 - 37) * 2;
print !false;
\"baz\" + \"hello\" + \"quz\" + \"bar\" == \"bazhelloquzbar\";
print !false;",
            "true
true",
        );

        happy_case(
            r#"27 - 60 >= -99 * 2 / 99 + 76;
true == true;
("world" == "bar") == ("baz" != "hello");
print true;"#,
            "true",
        );

        sad_case(
            r#"print "the expression below is invalid";
49 + "baz";
print "this should not be printed";"#,
        );

        sad_case(
            r#"print "79" + "baz";
print false * (18 + 84);"#,
        );
    }

    #[test]
    fn declare_variables() {
        happy_case(
            "var world = 10;
print world;",
            "10",
        );

        happy_case(
            "var bar = 99;
var foo = 99;
print bar + foo;
var quz = 99;
print bar + foo + quz;",
            "198
297",
        );

        happy_case(
            "var foo = (8 * (62 + 62)) / 4 + 62;
print foo;",
            "310",
        );

        happy_case(
            "var quz = 76;
var baz = quz;
print baz + quz;",
            "152",
        );
    }

    #[test]
    fn var_runtime_errors() {
        sad_case(
            "print 22;
print x;",
        );

        sad_case(
            "var baz = 96;
print hello;",
        );

        sad_case(
            "var hello = 85;
var result = (hello + bar) / world;
print result;",
        );

        sad_case(
            "var quz = 20;
var world = 51;
var hello = 56;
print quz + world + he",
        );
    }

    #[test]
    fn initialise_variables() {
        happy_case(
            r#"var baz = "foo";
var bar;
print bar;"#,
            "nil",
        );

        happy_case(
            r#"var baz = 69;
var world;
var quz;
print world;"#,
            "nil",
        );

        happy_case(
            r#"var quz = 73 + 26 * 20;
print quz;
var hello = 26 * 20;
print quz + hello;
var foo;
print foo;"#,
            "593
1113
nil",
        );
    }

    #[test]
    fn redeclare_variables() {
        happy_case(
            r#"var baz = "before";
print baz;
var baz = "after";
print baz;"#,
            "before
after",
        );

        happy_case(
            r#"var hello = "after";
var hello = "before";
var hello = hello;
print hello;"#,
            "before",
        );

        happy_case(
            r#"var world = 2;
print world;
var world = 3;
print world;
var hello = 5;
print hello;
var world = hello;
print world;"#,
            "2
3
5
5",
        );

        sad_case("var baz = bar;");
    }

    #[test]
    fn assignment_operation() {
        happy_case(
            r#"var quz;
quz = 1;
print quz;
print quz = 2;
print quz;"#,
            "1
2
2",
        );

        happy_case(
            r#"var hello = 93;
var bar = 93;
bar = hello;
hello = bar;
print hello + bar;"#,
            "186",
        );

        happy_case(
            r#"var quz;
var hello;

quz = hello = 16 + 34 * 92;
print quz;
print hello;"#,
            "3144
3144",
        );

        happy_case(
            r#"var hello = 65;
var baz;
var quz;

hello = baz = quz = hello * 2;
print hello;
print baz;
print baz;"#,
            "130
130
130",
        );
    }

    #[test]
    fn block_syntax() {
        happy_case(
            r#"{
    var hello = "baz";
    print hello;
}"#,
            "baz",
        );

        happy_case(
            r#"{
    var world = "before";
    print world;
}
{
    var world = "after";
    print world;
}"#,
            "before
after",
        );

        happy_case(
            r#"{
    var hello = 88;
    {
        var foo = 88;
        print foo;
    }
    print hello;
}"#,
            "88
88",
        );
    }
}
