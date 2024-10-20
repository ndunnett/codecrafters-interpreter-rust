#![allow(dead_code)]

use std::{env, fs, process};

mod scanning;

enum ExitCode {
    Ok = 0,
    Error = 1,
    UsageError = 2,
    LexicalError = 65,
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
                let (tokens, errors) = scanning::Scanner::new(&file_contents).scan_tokens();

                let exit_code = if errors.is_empty() {
                    ExitCode::Ok
                } else {
                    ExitCode::LexicalError
                };

                for e in errors {
                    eprintln!("{e}");
                }

                for token in tokens {
                    println!("{token}");
                }

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
