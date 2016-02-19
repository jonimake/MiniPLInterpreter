#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern crate regex;

pub mod token_iterator;
pub mod definitions;
pub mod token_mapper;

use token_iterator::TokenIterator;

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
	println!("The file is {}", absolute_path.display());

	let exitString = "exit".to_string();
	match (path.exists() && path.is_file()) {
		true => { //tokenize file

			let f = File::open(path).unwrap();
			let mut reader = BufReader::new(f);
			for line in reader.lines() {
					let line = line.unwrap();
					eval_line(&line);
			}
			/*let f = try!(File::open(path));
				let mut reader = BufReader::new(f);
				for line in reader.lines() {
					let line = try!(line);
					eval_line(&line);
				}*/
		}, 
		false => { //capture input and start tokenizing that
			let stdin = io::stdin();
			for line in stdin.lock().lines() {
				match line {
					Ok(content) => {
						if(&content == "exit") {break;}
						eval_line(&content);
					},
					Err(errmsg) => {println!("Error: {}", errmsg); break},
				}
			}
		} ,
	};
}

fn eval_line(line: &str) {
	let mut variables = HashMap::new();
	let mut it = TokenIterator::new(line);
	let all_tokens: Vec<&str> = it.collect();
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
}