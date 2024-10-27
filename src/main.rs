#![allow(dead_code)]

use std::{env, fs, process};

mod ast;
mod evaluation;
mod literals;
mod parsing;
mod scanning;
mod tokens;

use crate::{evaluation::Interpreter, parsing::Parser, scanning::Scanner};

enum ExitCode {
    Ok = 0,
    UsageError = 2,
    SyntaxError = 65,
    RuntimeError = 70,
    UnableToExecute = 126,
    CommandNotFound = 127,
}

impl ExitCode {
    fn exit(self) -> ! {
        process::exit(self as i32)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        ExitCode::UsageError.exit()
    }

    let command = &args[1];
    let filename = &args[2];

    if let Ok(file_contents) = fs::read_to_string(filename) {
        match command.as_str() {
            "tokenize" => {
                let (tokens, errors) = Scanner::new(&file_contents).scan_tokens();

                let exit_code = if errors.is_empty() {
                    ExitCode::Ok
                } else {
                    ExitCode::SyntaxError
                };

                for error in errors {
                    eprintln!("{error}");
                }

                for token in tokens {
                    println!("{token}");
                }

                exit_code.exit()
            }
            "parse" => {
                let (tokens, scan_errors) = Scanner::new(&file_contents).scan_tokens();
                let mut parser = Parser::new(tokens);
                let (expr, parse_errors) = parser.parse_expression();

                let exit_code = if scan_errors.is_empty() && parse_errors.is_empty() {
                    ExitCode::Ok
                } else {
                    ExitCode::SyntaxError
                };

                for error in scan_errors {
                    eprintln!("{error}");
                }

                for error in parse_errors {
                    eprintln!("{error}");
                }

                if let Some(expr) = expr {
                    println!("{expr:?}");
                }

                exit_code.exit()
            }
            "evaluate" => {
                let (tokens, scan_errors) = Scanner::new(&file_contents).scan_tokens();
                let mut parser = Parser::new(tokens);
                let (expr, parse_errors) = parser.parse_expression();

                if scan_errors.is_empty() && parse_errors.is_empty() && expr.is_some() {
                    match Interpreter::new().evaluate(&expr.unwrap()) {
                        Ok(result) => {
                            println!("{result}");
                            ExitCode::Ok.exit()
                        }
                        Err(e) => {
                            eprintln!("{e}");
                            ExitCode::RuntimeError.exit()
                        }
                    }
                } else {
                    scan_errors.iter().for_each(|e| eprintln!("{e}"));
                    parse_errors.iter().for_each(|e| eprintln!("{e}"));
                }

                ExitCode::SyntaxError.exit()
            }
            "run" => {
                let (tokens, scan_errors) = Scanner::new(&file_contents).scan_tokens();
                let mut parser = Parser::new(tokens);
                let (program, parse_errors) = parser.parse_program();

                if scan_errors.is_empty() && parse_errors.is_empty() {
                    match Interpreter::new().run(&program) {
                        Ok(_) => ExitCode::Ok.exit(),
                        Err(e) => {
                            eprintln!("{e}");
                            ExitCode::RuntimeError.exit()
                        }
                    }
                } else {
                    scan_errors.iter().for_each(|e| eprintln!("{e}"));
                    parse_errors.iter().for_each(|e| eprintln!("{e}"));
                }

                ExitCode::SyntaxError.exit()
            }
            _ => {
                eprintln!("Unknown command: {command}");
                ExitCode::CommandNotFound.exit()
            }
        }
    } else {
        eprintln!("Failed to read file: {filename}");
        ExitCode::UnableToExecute.exit()
    }
}
