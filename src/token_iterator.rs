use std::str::FromStr;
use std::str::Lines;
use std::cmp;
use std::i32;
use std::usize;
use std::iter::Enumerate;
use std::iter::Peekable;
use std::str::CharIndices;

use token::Token;
use token::TokenType;

const single_char_token: &'static [&'static str] = &["+","-","*","/","<","=","&","!","(",")",";",".",":"];
const two_char_token: &'static [&'static str] = &["..",":="];
const keyword: &'static [&'static str] = &["assert","string","print","bool","read","var","for","end","int","in","do"];

pub struct TokenIterator<'a>  {
	text: &'a str,
	lines: Enumerate<Lines<'a>>,

	token_matchers: Vec<(TokenType, fn(&str) -> bool)>,

	current_line_number: usize,
	current_line_char_pos: usize,
	current_line: &'a str,

	slice_start: usize,

	initialized: bool
}


impl<'a>  TokenIterator<'a>  {
	//type Item = str;

	pub fn new(string: &'a str) -> TokenIterator {

		let token_matchers: Vec<(TokenType, fn(&str) -> bool)> = vec!(
		(TokenType::string_literal, is_string_literal),
		(TokenType::keyword, is_keyword_token),
		(TokenType::identifier, is_identifier),
		(TokenType::two_char, is_two_char_token),
		(TokenType::single_char, is_single_char_token),
		(TokenType::integer, is_integer));
		let src = string.trim_left_matches("\u{feff}"); //strip BOM

		let t = TokenIterator {
			initialized: false,
            text: src,
			token_matchers: token_matchers,
			lines: src.lines().enumerate(),
			current_line: "",
			slice_start: 0,
			current_line_number: 1,
			current_line_char_pos: 0,
        };
		t
    }

	fn handle_end_of_line(&mut self) -> bool {
		//println!("{}, {}", self.current_line_char_pos, self.current_line.len());
		//self.visualize_line_slice("", self.current_line_char_pos, self.current_line.len());
		if(self.current_line_char_pos >= self.current_line.len() ) { //get next line chars iterator if at the end
			match self.lines.next() {
				Some((line_number, line)) => {
					if(!self.initialized) {
						self.initialized = true;
					} else {
						self.slice_start += 1;
					}
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
		for _ in 0 .. pos+1 {
			buffer.push(' ');
		}
		buffer.push('^');

		println!("#####################");
		println!("{}", line1);
		println!("{}", buffer);
		println!("#####################");
	}

	fn visualize_line_slice(&self, slice: &str, start: usize, end: usize) {
		let line1 = self.current_line;
		let mut buffer = String::new();
		for _ in 0 .. start {
			buffer.push(' ');
		}
		buffer.push('^');

		if(end - start > 0){
			for _ in 0 .. (end - start - 1) {
				buffer.push(' ');
			}

			buffer.push('^');
		}

		println!("#####################");

		println!("slice: \t{}",slice);
		println!("line: \t{}",line1);
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
		let mut longest_lexeme = usize::MIN;

		for column in self.current_line_char_pos .. self.current_line.len()+1 {
			//println!("{}",self.current_line.chars().nth(column).unwrap_or(' '));
			unsafe {
				lexeme_candidate = self.current_line.slice_unchecked(self.current_line_char_pos, column);
			}
			//println!("{}",lexeme_candidate);

			if ( !skipped_initial_whitespace && lexeme_candidate.chars().nth(0).unwrap_or('a').is_whitespace() ) {
				//skip rest of whitespace
				skipping_whitespace = true;
				self.current_line_char_pos += 1;
				continue;
			} else if (skipping_whitespace && lexeme_candidate != " ") {
				skipped_initial_whitespace = true;
			}

			for tuple_ref in self.token_matchers.iter() {
				let (tokentype, matcher_fn): (TokenType, fn(&str) -> bool) = *tuple_ref;
				if(matcher_fn(lexeme_candidate)) {
					let line_pos_start = self.current_line_char_pos;
					let line_pos_end = self.current_line_char_pos + lexeme_candidate.len();
					let lexeme_length = lexeme_candidate.chars().count();
					if(lexeme_length > longest_lexeme) { //prioritize length

						//println!("New longest lexeme {:?}, {:?}, {:?}", lexeme_candidate ,tokentype, lexeme_length);
						longest_lexeme = lexeme_length;
						lexeme = lexeme_candidate;
						t_start = line_pos_start;
						t_end = column;
						token_start_on_line = line_pos_start;
						token_type = tokentype;
						found = true;
					}
				}
			}
		}

		if found {
			token = Some(Token{
				line: self.current_line_number + 1,
				column: token_start_on_line + 1,
				lexeme: lexeme,
				token_type: token_type});

			//set next slice start on line
			self.current_line_char_pos = t_end;

		} else {
			return self.next(); //not found, try again. EOL breaks recursive iteration
		}
		token
	}
}

fn is_single_char_token(lexeme: &str) -> bool {single_char_token.iter().any(|&x| x == lexeme)}

fn is_two_char_token(lexeme: &str) -> bool {two_char_token.iter().any(|&x| x == lexeme)}

fn is_keyword_token(lexeme: &str) -> bool {	keyword.iter().any(|&x| x == lexeme)}

fn is_string_literal(lexeme: &str) -> bool {
	let starts_with_quote = lexeme.starts_with("\"");
	let mut chars = lexeme.chars();
	let mut escape_next = false;
	let mut num_unescaped_quotes = 0;
	let mut last_ch = ' ';
	while let Some(ch) = chars.next() {
		last_ch = ch;
		match ch {
			'\\' => escape_next = true,
			'"'	=> {
				if(!escape_next) {
					num_unescaped_quotes += 1;
				}
			}
			_ => escape_next = false
		}
	}
	starts_with_quote && last_ch == '"' && num_unescaped_quotes % 2 == 0

}

fn is_integer(lexeme: &str) -> bool {lexeme.parse::<i32>().is_ok()}

fn is_identifier(lexeme: &str) -> bool {
	let allAreAlphaNum = lexeme.chars().all(|ch: char| {
		ch.is_alphanumeric() || ch == '_'
	});
	let firstIsAlphabetic = lexeme.chars().nth(0).unwrap_or('1').is_alphabetic();
	firstIsAlphabetic && allAreAlphaNum
}

#[test]
fn recornize_identifier() -> () {
	assert_eq!(false, is_identifier("var X : int"));
	assert_eq!(false, is_identifier("var X : i"));
	assert_eq!(true, is_identifier("X"));
	assert_eq!(true, is_identifier("awesome_var1"));
	assert_eq!(false, is_identifier("1illegalvar"));
}

#[test]
fn recognize_string_literal() -> () {
	let ok = r#""foo bar baz""#;
	let missing_quote = r#""foo bar baz"#;
	let unescaped_quote = r#""foo "bar baz""#;
	let escaped_quote = r#""foo \"bar baz""#;
	assert_eq!(true, is_string_literal(ok));
	assert_eq!(false, is_string_literal(missing_quote));
	assert_eq!(false, is_string_literal(unescaped_quote));
	assert_eq!(true, is_string_literal(escaped_quote));
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
fn tokenize_two_consecutive_file_3() -> () {
	let code = include_str!("../sample3.txt");
	println!("{}", code);
	let mut iterator = TokenIterator::new(code);
	assert_eq!(Some(Token{line:1, column: 1, lexeme: "print", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 7, lexeme: "\"Give a number\"", token_type: TokenType::string_literal}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 22, lexeme: ";", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 1, lexeme: "var", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 5, lexeme: "n", token_type: TokenType::identifier}),iterator.next());
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
