#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern crate regex;

pub mod token_iterator;
pub mod definitions;

use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::io::BufReader;
use std::str::SplitWhitespace;
use std::iter::Peekable;

fn main() {
	let path = Path::new("sample1.txt");
	assert!(path.exists());

	let f = File::open(path).unwrap();
}
