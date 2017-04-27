#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

pub mod lexeme;
pub mod lexeme_iterator;
pub mod parser;

#[macro_use]
extern crate log;
extern crate simplelog;

use lexeme_iterator::LexemeIterator;
use lexeme::Lexeme;
use parser::token::Token;
use parser::token_iterator::TokenIterator;
use parser::interpreter::Interpreter;
//use parser::Token;

use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::io::BufReader;
use std::str::SplitWhitespace;
use std::iter::Peekable;
use std::env;
use std::path::PathBuf;
use std::io;
use std::collections::HashMap;
use std::vec::Vec;


static HAS_INIT: bool = false;

fn main() {
	let args = env::args().collect::<Vec<String>>();
	info!("{:?}", args);
	if args.len() > 2 {
		let level: simplelog::LogLevelFilter = match args[2].as_ref() {
			"info" => simplelog::LogLevelFilter::Info,
			"debug" => simplelog::LogLevelFilter::Debug,
			"error" => simplelog::LogLevelFilter::Error,
			"trace" => simplelog::LogLevelFilter::Trace,
			"warn" => simplelog::LogLevelFilter::Warn,
			_ => simplelog::LogLevelFilter::Info
		};
		let log = simplelog::TermLogger::init(level, simplelog::Config::default());
		info!("Log level: {}", level);

	} else {
		let _ = simplelog::TermLogger::init(simplelog::LogLevelFilter::Info, simplelog::Config::default());
	}


	info!("MiniPL Interpreter starting!");

	let mut path = Path::new("");
	let mut pathStr: &str = "";
    if args.len() > 1 {
		pathStr = &args[1];
    }

	path = Path::new(pathStr);

	let mut absolute_path = env::current_dir().unwrap();
	let mut state: HashMap<String, Token> = HashMap::new();

	debug!("The current directory is {}", absolute_path.display());
	absolute_path.push(path);
	let exit_keyword = "exit";
	debug!("The file is {}", absolute_path.display());
	let exitString = "exit".to_string();
	match path.exists() && path.is_file() {
		true => { //lexemeize file

			let f = File::open(path).unwrap();
			let mut reader = BufReader::new(f);

			let mut buffer = String::new();
			reader.read_to_string(&mut buffer).unwrap();
			info!("File read");
			buffer.trim_left_matches("\u{feff}");

			eval_file(&buffer);
		},
		false => { //capture input and start lexemeizing that
			info!("Type commands");
			info!("Type {:?} to exit", exit_keyword);
			info!("MiniPL> ");
			let stdin = io::stdin();
			for line in stdin.lock().lines() {
				match line {
					Ok(content) => {
						if content == exit_keyword {break;}
						eval_line(&content, &mut state);
					},
					Err(errmsg) => {error!("Error: {}", errmsg); break},
				}
			}
		} ,
	};
}

fn eval_file(file_contents: &str) {
	let mut state = HashMap::new();
	let mut lexeme_it = LexemeIterator::new(file_contents);
	let mut tokenIterator: TokenIterator<LexemeIterator> = TokenIterator{lexIter:lexeme_it};
	let mut interpreter = Interpreter::new(&mut tokenIterator as &mut Iterator<Item=Token>, &mut state);
	//let mut interpreter = Interpreter::new(&mut state);
	interpreter.interpret();
	//let mut peekable_iterator: Peekable<LexemeIterator> = lexeme_it.peekable();
	//let mut tokenIterator: TokenIterator = TokenIterator{lexIter:lexeme_it};
	//let tokens: Vec<Token> = tokenIterator.collect();
	//let mut ast = AST::new(tokens.as_ref());
	//ast.interpret();

}

fn eval_line(line: &str, state: &mut HashMap<String, Token>) {
	info!("{:?}", line);
	let it = LexemeIterator::new(line);


	let mut tokenIterator: TokenIterator<LexemeIterator> = TokenIterator{lexIter:it};

	//let mut interpreter = Interpreter::new(tokenIterator);
	let mut state = HashMap::new();
	let mut interpreter = Interpreter::new(&mut tokenIterator as &mut Iterator<Item=Token>, &mut state);
	//let mut interpreter = Interpreter::new(&mut state);
	interpreter.interpret();


	//let interpreter = Interpreter::new(&mut tokenIterator as &mut Iterator<Item=Token>);
	//let m: Vec<Token> = tokenIterator.collect();
	//info!("tokens:{:?}", m);
	//let mut ast = AST::new(m.as_ref());
	//ast.interpret();

}

//#[test]
fn sample1_var_definition_expression_print(){
	let code =r#"
var X : int := 4 + (6 * 2);
print X;
"#;
	eval_file(code);
	assert!(true);

}


//#[test]
fn sample2_loop_print(){
	let code =r#"
var nTimes : int := 0;
print "How many times?";
read nTimes;
var x : int;
for x in 0..nTimes-1 do
	print x;
	print " : Hello, World!\n";
end for;
assert (x = nTimes);
"#;
	eval_file(code);
}

//#[test]
fn sample3_loop_multiply(){
	let code =r#"
print "Give a number";
var n : int;
read n;
var f : int := 1;
var i : int;
for i in 1..n do
	f := f * i;
end for;
print "The result is: ";
print f;
"#;
	eval_file(code);
}

//#[test]
fn sample4_decl_assign_print(){
	let code =r#"
var X : int;
X := 15;
print X;
"#;
	eval_file(code);
}