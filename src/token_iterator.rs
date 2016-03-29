use regex;
use regex::Regex;
use std::str::FromStr;
use std::str::Lines;
use std::cmp;
use std::i32;
use std::usize;
use std::iter::Enumerate;
use std::iter::Peekable;
use std::str::CharIndices;

/*
#[derive(PartialEq, Debug)]
enum PLVar<'a> {
	PLString(&'a str),
	PLBool(bool),
	PLInt(i32)
}
*/

#[derive(PartialEq,Debug)]
pub struct Token<'a> {
	line: usize,
	column: usize,
	lexeme: &'a str,
	token_type: TokenType,
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum TokenType {
	single_char,
	two_char,
	keyword,
	integer,
	string_literal,
	identifier,
	NA,
}


pub struct TokenIterator<'a>  {
	text: &'a str,
	lines: Enumerate<Lines<'a>>,

	allRegex: Vec<(TokenType,Regex)>,

	current_line_number: usize,
	current_line_char_pos: usize,
	current_line: &'a str,

	current_column: usize,
	slice_start: usize,

	initialized: bool
}

impl<'a>  TokenIterator<'a>  {
	//type Item = str;

	pub fn new(string: &'a str) -> TokenIterator {

		let single_char_token = Regex::new(r"\+|-|\*|/|<|=|&|!|\(|\)|;|\.|:").unwrap();
		let two_char_token = Regex::new(r"(\.{2})|(:={1})").unwrap();
		let keyword = Regex::new(r"(assert)|(string)|(print)|(bool)|(read)|(var)|(for)|(end)|(int)|(in)|(do)").unwrap();
		let integer = Regex::new(r"\b\d+\b").unwrap();
		let string_literal = Regex::new(r#"^"([^"\\]|\\.)*"$"#).unwrap();
		let identifier = Regex::new(r"^[A-Za-z][A-Za-z0-9]*|([A-Za-z]+[0-9]*)").unwrap();

		let allRegex: Vec<(TokenType, Regex)> = vec!(
		(TokenType::string_literal, string_literal),
		(TokenType::keyword, keyword),
		(TokenType::identifier, identifier),
		(TokenType::two_char, two_char_token),
		(TokenType::single_char, single_char_token),
		(TokenType::integer, integer));

		let t = TokenIterator {
			current_column: 0,
			initialized: false,
            text: string,
			allRegex: allRegex,
			lines: string.lines().enumerate(),
			current_line: "",
			slice_start: 0,
			current_line_number: 1,
			current_line_char_pos: 0,
        };
		t
    }

	fn handle_end_of_line(&mut self) -> bool {
		println!("checking end of line, current char pos {}, line len {}", self.current_line_char_pos, self.current_line.len());
		if(self.current_line_char_pos >= self.current_line.len() ) { //get next line chars iterator if at the end
			println!("Self line chars is none");
			match self.lines.next() {
				Some((line_number, line)) => {
					if(!self.initialized) {
						self.initialized = true;
					} else {
						self.slice_start += 1;
					}
					println!("Setting line counter to {}: {}", line_number, line);
					self.current_line_number = line_number;
					self.current_line = line;
					self.current_line_char_pos = 0;
				},
				None => {
					//return none if lines exhausted too
					return false;
				}
			}
		}
		true
	}


	fn visualize_line_pos(&self, pos: usize) {
		let line1 = self.current_line;
		let mut buffer = String::new();
		for x in 0 .. pos+1 {
			buffer.push(' ');
		}
		buffer.push('^');

		println!("#####################");
		println!("{:?}", line1);
		println!("{}", buffer);
		println!("#####################");
	}

	fn visualize_line_slice(&self, slice: &str, start: usize, end: usize) {
		let line1 = self.current_line;
		let mut buffer = String::new();
		for x in 0 .. start+1 {
			buffer.push(' ');
		}
		buffer.push('^');

		if(end - start > 0){
			for x in 0 .. (end - start - 1) {
				buffer.push(' ');
			}

			buffer.push('^');
		}

		println!("#####################");

		println!("slice: \t{:?}",slice);
		println!("line: \t{:?}",line1);
		println!("      \t{}",buffer);
		println!("{}-{}",start, end);
		println!("#####################");
	}
}

impl<'a> Iterator for TokenIterator<'a> {
	type Item = Token<'a>;

	fn next(&mut self) -> Option<Token<'a>> {
		let mut token: Option<Token> = None;

		if !self.handle_end_of_line() {
			return token;
		}

		let mut lexeme = "";
		let mut lexeme_candidate;
		let mut found = false;
		let mut skipped_initial_whitespace = false;
		let mut skipping_whitespace = false;
		let mut token_start_on_line = 0;
		let mut t_start = usize::MAX;
		let mut t_end = usize::MIN;
		let mut token_type = TokenType::NA;

		for column in self.current_line_char_pos .. self.current_line.len()+1 {
			unsafe {
				lexeme_candidate = self.current_line.slice_unchecked(self.current_line_char_pos, column);
			}


			if ( !skipped_initial_whitespace && lexeme_candidate == " " ) {
				//skip rest of whitespace
				skipping_whitespace = true;
				self.current_line_char_pos += 1;
				continue;
			} else if (skipping_whitespace && lexeme_candidate != " ") {
				skipped_initial_whitespace = true;
			}
			println!("\nlooping regexes for slice: {:?} ", lexeme_candidate);
			for tuple_ref in self.allRegex.iter() {
				let (tokentype, ref rex): (TokenType, Regex) = *tuple_ref;
				println!("checking type {:?} for {:?}", tokentype, lexeme_candidate);
				if let Some((start, end)) = rex.find(lexeme_candidate) {
					let line_pos_start = self.current_line_char_pos+start;
					let line_pos_end = self.current_line_char_pos+end;
					println!("Found {:?}  {:?}", tokentype, lexeme_candidate);

					if(line_pos_start <= t_start && line_pos_end > t_end) {

						//self.visualize_line_slice(lexeme_candidate, line_pos_start, line_pos_end);


						lexeme = lexeme_candidate;
						t_start = line_pos_start;
						t_end = column;

						token_start_on_line = line_pos_start;
						token_type = tokentype;
						found = true;
					}
				}
			}
			//println!(" ");
		}

		if found {
			//println!("token_start_on_line = {}, t_start = {}, self.current_line_char_pos (0 ind) = {}",
			//		token_start_on_line, t_start, self.current_line_char_pos);
			token = Some(Token{
				line: self.current_line_number + 1,
				column: token_start_on_line+1,
				lexeme: lexeme,
				token_type: token_type});

			//set next slice start on line

			self.current_line_char_pos = t_end;

		}
//		println!(
//		"##################################################################################################");
		println!("{:?}", token);
		token
	}
}

#[test]
fn tokenize_two_consecutive() -> () {
	let code = include_str!("../sample4.txt");
	println!("{}", code);
	let mut iterator = TokenIterator::new(code);
	assert_eq!(Some(Token{line:1, column: 1, lexeme: "var", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 5, lexeme: "X", token_type: TokenType::identifier}),iterator.next());
}

#[test]
fn tokenize_full_line() -> () {
	let code = include_str!("../sample4.txt");
	println!("{}", code);
	let mut iterator = TokenIterator::new(code);
	assert_eq!(Some(Token{line:1, column: 1, lexeme: "var", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 5, lexeme: "X", token_type: TokenType::identifier}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 7, lexeme: ":", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 9, lexeme: "int", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 12, lexeme: ";", token_type: TokenType::single_char}),iterator.next());
}

#[test]
fn tokenize_multiple_lines() -> () {
	let code = include_str!("../sample4.txt");
	println!("{}", code);
	let mut iterator = TokenIterator::new(code);
	iterator.next();
	iterator.next();
	iterator.next();
	iterator.next();
	iterator.next();
	assert_eq!(Some(Token{line:2, column: 1, lexeme: "X", token_type: TokenType::identifier}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 3, lexeme: ":=", token_type: TokenType::two_char}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 6, lexeme: "15", token_type: TokenType::integer}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 8, lexeme: ";", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(Some(Token{line:3, column: 1, lexeme: "print", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:3, column: 7, lexeme: "X", token_type: TokenType::identifier}),iterator.next());
	assert_eq!(Some(Token{line:3, column: 8, lexeme: ";", token_type: TokenType::single_char}),iterator.next());
}