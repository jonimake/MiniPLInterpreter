#![allow(unknown_lints)]
#![warn(clippy)]

pub mod lexer;
pub mod parser;

#[macro_use]
extern crate log;
extern crate simplelog;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use simplelog::Config;
use simplelog::LogLevelFilter;
use simplelog::TermLogger;
use structopt::StructOpt;

use lexer::lexeme_iterator::LexemeIterator;
use parser::interpreter::Interpreter;
use parser::interpreter::InterpreterState;
use parser::token::Token;
use parser::token_iterator::TokenIterator;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;

#[derive(StructOpt, Debug)]
#[structopt(name = "MiniPLInterpreter")]
struct Cli {
    #[structopt(
        long = "inputpath",
        short = "i",
        help = "Path to file to be interpreted. If left empty, the program will wait for stdin to be interpreted."
    )]
    input_path: Option<String>,

    #[structopt(long = "loglevel", short = "l")]
    log_level: Option<LogLevel>,
}

#[derive(StructOpt, Debug)]
#[structopt(name = "loglevel")]
enum LogLevel {
    #[structopt(name = "error")]
    Error,
    #[structopt(name = "info")]
    Info,
    #[structopt(name = "warning")]
    Warning,
    #[structopt(name = "debug")]
    Debug,
    #[structopt(name = "trace")]
    Trace,
}

impl std::str::FromStr for LogLevel {
    type Err = std::string::ParseError;

    fn from_str(text: &str) -> std::result::Result<Self, Self::Err> {
        match text {
            "info" => Result::Ok(LogLevel::Info),
            "warning" => Result::Ok(LogLevel::Warning),
            "debug" => Result::Ok(LogLevel::Debug),
            "trace" => Result::Ok(LogLevel::Trace),
            "error" => Result::Ok(LogLevel::Error),
            _ => Result::Ok(LogLevel::Error),
        }
    }
}

impl Into<LogLevelFilter> for LogLevel {
    fn into(self) -> LogLevelFilter {
        match self {
            LogLevel::Error => LogLevelFilter::Error,
            LogLevel::Info => LogLevelFilter::Info,
            LogLevel::Warning => LogLevelFilter::Warn,
            LogLevel::Debug => LogLevelFilter::Debug,
            LogLevel::Trace => LogLevelFilter::Trace,
        }
    }
}

fn main() {
    let _example = 1230000000;
    let args = Cli::from_args();
    let ll = args.log_level;
    let level_filter: LogLevelFilter = match ll {
        Some(level) => level.into(),
        _ => LogLevelFilter::Error,
    };
    let _ = TermLogger::init(level_filter, Config::default());

    info!("MiniPL Interpreter starting!");

    let path_str: String = args.input_path.unwrap_or("".to_string());
    let path = Path::new(&path_str);

    let mut absolute_path = env::current_dir().unwrap();
    let mut state = InterpreterState::new();

    debug!("The current directory is {}", absolute_path.display());
    absolute_path.push(path);
    let exit_keyword = "exit";
    debug!("The file is {}", absolute_path.display());

    if path.exists() && path.is_file() {
        let f = File::open(path).unwrap();
        let mut reader = BufReader::new(f);
        let mut buffer = String::new();
        reader.read_to_string(&mut buffer).unwrap();
        info!("File read");
        buffer.trim_left_matches("\u{feff}");

        let _ = eval_file(&buffer);
    } else {
        println!("Type commands");
        println!("Type {:?} to exit", exit_keyword);

        loop {
            print!("MiniPL> ");
            io::stdout().flush().unwrap();
            let mut input_text: String = String::new();
            io::stdin().read_line(&mut input_text).expect("failed to read from stdin");
            trace!("Read: {}", input_text);
            io::stdout().flush().unwrap();
            if input_text.to_string().trim() == exit_keyword {
                break;
            }
            eval_line(&input_text.to_string().trim(), &mut state);
        }
    };
}

fn eval_file(file_contents: &str) -> Result<(), String> {
    let lexeme_it = LexemeIterator::new(file_contents);
    let mut token_iterator: TokenIterator<LexemeIterator> = TokenIterator::new(lexeme_it);
    let mut state = InterpreterState::new();
    let mut interpreter = Interpreter::new(&mut token_iterator as &mut Iterator<Item = Token>, &mut state);
    interpreter.interpret()
}

fn eval_line(line: &str, mut state: &mut InterpreterState) {
    debug!("{:?}", line);
    let it = LexemeIterator::new(line);
    let mut token_iterator: TokenIterator<LexemeIterator> = TokenIterator::new(it);
    let mut interpreter = Interpreter::new(&mut token_iterator, &mut state);
    match interpreter.interpret() {
        Err(msg) => error!("{}", msg),
        _ => {}
    }
}

#[test]
fn sample1_var_definition_expression_print() {
    let code = r#"
var X : int := 4 + (6 * 2);
print X;
"#;
    eval_file(code).unwrap();
}

#[test]
fn sample2_loop_print() {
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
    let code = r#"
var X : int;
X := 15;
print X;
"#;
    eval_file(code).unwrap();
}

#[test]
fn test_decl_assign_boolean() {
    let code = r#"
var X : bool;
X := 15;
print X;
"#;
    eval_file(code).unwrap();
}
