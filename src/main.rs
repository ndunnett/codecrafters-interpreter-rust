#![allow(dead_code)]

use std::{env, fs, process};

mod parsing;
mod scanning;

use crate::{parsing::Parser, scanning::Scanner};

enum ExitCode {
    Ok = 0,
    Error = 1,
    UsageError = 2,
    RuntimeError = 65,
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
                    ExitCode::RuntimeError
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
                let (expressions, parse_errors) = parser.parse_tokens();

                let exit_code = if scan_errors.is_empty() && parse_errors.is_empty() {
                    ExitCode::Ok
                } else {
                    ExitCode::RuntimeError
                };

                for error in scan_errors {
                    eprintln!("{error}");
                }

                for error in parse_errors {
                    eprintln!("{error}");
                }

                println!("{expressions}");
                exit_code.exit()
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
