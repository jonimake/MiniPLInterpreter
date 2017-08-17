pub mod lexer;
pub mod parser;

#[macro_use]
extern crate log;
extern crate simplelog;
extern crate clap;

use clap::{Arg, App};

use lexer::lexeme_iterator::LexemeIterator;
use parser::token::Token;
use parser::token_iterator::TokenIterator;
use parser::interpreter::Interpreter;
use parser::interpreter::InterpreterState;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::io::BufReader;
use std::env;
use std::io;
use std::vec::Vec;

fn main() {

    let _ = App::new("MiniPL Interpreter")
        .version("1.0")
        .author("Joni M. <joni.makela@gmail.com>")
        .about("MiniPL language interpreter")
        .arg(Arg::with_name("INPUT")
            .help("File to interpret")
            .required(false)
            .index(1))
        .arg(Arg::with_name("log")
            .short("l")
            .help("Sets the level of log verbosity"))
        .get_matches();


    let args = env::args().collect::<Vec<String>>();
    info!("{:?}", args);
    if args.len() > 2 {
        let level: simplelog::LogLevelFilter = match args[2].as_ref() {
            "info" => simplelog::LogLevelFilter::Info,
            "debug" => simplelog::LogLevelFilter::Debug,
            "error" => simplelog::LogLevelFilter::Error,
            "trace" => simplelog::LogLevelFilter::Trace,
            "warn" => simplelog::LogLevelFilter::Warn,
            _ => simplelog::LogLevelFilter::Error,
        };
        let _ = simplelog::TermLogger::init(level, simplelog::Config::default());
        info!("Log level: {}", level);

    } else {
        let _ = simplelog::TermLogger::init(simplelog::LogLevelFilter::Info,
                                            simplelog::Config::default());
    }

    info!("MiniPL Interpreter starting!");

    let mut path_str: &str = "";
    if args.len() > 1 {
        path_str = &args[1];
    }

    let path = Path::new(path_str);

    let mut absolute_path = env::current_dir().unwrap();
    let mut state = InterpreterState::new();

    debug!("The current directory is {}", absolute_path.display());
    absolute_path.push(path);
    let exit_keyword = "exit";
    debug!("The file is {}", absolute_path.display());

    match path.exists() && path.is_file() {
        true => {
            //lexemeize file

            let f = File::open(path).unwrap();
            let mut reader = BufReader::new(f);

            let mut buffer = String::new();
            reader.read_to_string(&mut buffer).unwrap();
            info!("File read");
            buffer.trim_left_matches("\u{feff}");

            let _ = eval_file(&buffer);
        }
        false => {
            //capture input and start lexemeizing that
            info!("Type commands");
            info!("Type {:?} to exit", exit_keyword);
            info!("MiniPL> ");
            loop {
                let mut input_text: String = String::new();
                io::stdin()
                    .read_line(&mut input_text)
                    .expect("failed to read from stdin");
                if input_text.to_string().trim() == exit_keyword {
                    break;
                }
                eval_line(&input_text.to_string().trim() , &mut state);

            }
        }
    };
}

fn eval_file(file_contents: &str) -> Result<(), String> {
    let lexeme_it = LexemeIterator::new(file_contents);
    let mut token_iterator: TokenIterator<LexemeIterator> = TokenIterator { lex_iter: lexeme_it };
    let mut state = InterpreterState::new();
    let mut interpreter = Interpreter::new(&mut token_iterator as &mut Iterator<Item = Token>,
                                           &mut state);
    interpreter.interpret()
}

fn eval_line(line: &str, mut state: &mut InterpreterState) {
    info!("{:?}", line);
    let it = LexemeIterator::new(line);
    let mut token_iterator: TokenIterator<LexemeIterator> = TokenIterator { lex_iter: it };
    let mut interpreter = Interpreter::new(&mut token_iterator, &mut state);
    match interpreter.interpret() {
        Err(msg) => info!("Error: {}", msg),
        _ => {}
    }
}

#[test]
fn sample1_var_definition_expression_print() {
    let _ = simplelog::TermLogger::init(simplelog::LogLevelFilter::Info,
                                        simplelog::Config::default());
    let code = r#"
var X : int := 4 + (6 * 2);
print X;
"#;
    eval_file(code).unwrap();

}


#[test]
fn sample2_loop_print() {
    let _ = simplelog::TermLogger::init(simplelog::LogLevelFilter::Debug,
                                        simplelog::Config::default());

    let code = r#"
var nTimes : int := 0;
print nTimes;
print "How many times?";
nTimes := 3;
print nTimes;
var x : int;
for x in 0..nTimes-1 do
	print x;
	print " : Hello, World!\n";
end for;
print x;
print nTimes;
assert (x = (nTimes-1));
"#;
    eval_file(code).unwrap();
}


#[test]
fn sample4_decl_assign_print() {
    let _ = simplelog::TermLogger::init(simplelog::LogLevelFilter::Info,
                                        simplelog::Config::default());
    let code = r#"
var X : int;
X := 15;
print X;
"#;
    eval_file(code).unwrap();
}

#[test]
fn test_decl_assign_boolean() {
    let _ = simplelog::TermLogger::init(simplelog::LogLevelFilter::Info,
                                        simplelog::Config::default());
    let code = r#"
var X : bool;
X := 15;
print X;
"#;
    eval_file(code).unwrap();
}