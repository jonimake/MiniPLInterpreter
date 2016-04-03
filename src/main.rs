#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

pub mod token;
pub mod token_iterator;
pub mod parser;
pub mod ast;

use token_iterator::TokenIterator;
use token::Token;
use parser::Parser;

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


fn main() {
	let args = env::args().collect::<Vec<String>>();
	let mut path = Path::new("");
	let mut pathStr: &str = "";
    if args.len() > 1 {
		pathStr = &args[1];
    }

	path = Path::new(pathStr);

	let mut absolute_path = env::current_dir().unwrap();
	

	println!("The current directory is {}", absolute_path.display());
	absolute_path.push(path);
	let exit_keyword = "exit";
	println!("The file is {}", absolute_path.display());
	let exitString = "exit".to_string();
	match (path.exists() && path.is_file()) {
		true => { //tokenize file

			let f = File::open(path).unwrap();
			let mut reader = BufReader::new(f);

			let mut buffer = String::new();
			reader.read_to_string(&mut buffer).unwrap();
			println!("File contents...");
			println!("{}", buffer);

			buffer.trim_left_matches("\u{feff}");

			eval_file(&buffer);
		},
		false => { //capture input and start tokenizing that
			println!("Type {:?} to exit", exit_keyword);
			println!("MiniPL> ");
			let stdin = io::stdin();
			for line in stdin.lock().lines() {
				match line {
					Ok(content) => {
						if(content == exit_keyword) {break;}
						eval_line(&content);
					},
					Err(errmsg) => {println!("Error: {}", errmsg); break},
				}
			}
		} ,
	};
}

fn eval_file(file_contents: &str) {
	println!("{}\n",file_contents);

	let mut token_it = TokenIterator::new(file_contents);
	let tokens: Vec<Token> = token_it.collect();
	let mut p = Parser::new(tokens);
	p.interpret();

}

fn eval_line(line: &str) {
	println!("{:?}", line);
	//let mut variables = HashMap::new();
	let mut it = TokenIterator::new(line);
	let all_tokens: Vec<Token> = it.collect();

	for x in all_tokens {
		println!("{:?}", x);
	}

	/*
	let mut token_it = all_tokens.into_iter().peekable();
	while let Some(token) = token_it.next() {
		match token {
			"var" => {
				let id = token_it.next().unwrap();
				if(token_it.next().unwrap() != ":") {panic!("Syntax error, expected : after variable id");}
				let ty = token_it.next().unwrap();
				let mut val: Option<&str> = None;
				match token_it.peek() {
					Some(&":=") => {
						token_it.next(); //consume assignment token
						val = token_it.next()
					}
					_ => {},
				}
				variables.insert(id, (ty, val));
				println!("{}: {} => {:?}", id, ty, val);
			},
			_ => {println!("Unrecognized token:{}", token)}
		};
	}
	/*
	while let Some(token) = it.next() {	
		println!("{}",token);
	}*/
	*/
}

#[test]
fn sample1_var_definition_expression_print(){
	let code =r#"
var X : int := 4 + (6 * 2);
print X;
"#;
	eval_file(code);
}

#[test]
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

#[test]
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

#[test]
fn sample4_decl_assign_print(){
	let code =r#"
var X : int;
X := 15;
print X;
"#;
	eval_file(code);
}