//extern crate statement;

#[cfg(test)]
mod tests {
	#![allow(dead_code)]


	use std::error::Error;
	use std::fs::File;
	use std::io::prelude::*;
	use std::io::BufReader;
	use std::path::Path;
	use std::str::SplitWhitespace;
	use std::iter::Peekable;

	use super::*;

	#[test]
	fn get_tokens() -> () {
		
		
		let path = Path::new("sample1.txt");
		assert!(path.exists());

		let f = File::open(path).unwrap();
		
		let mut reader = BufReader::new(f);
		for line in reader.lines().filter_map(|result| result.ok()) {
			for c in line.chars() {
				print!("{}", c);
			}
			print!("\n");
        }	
	}	
}