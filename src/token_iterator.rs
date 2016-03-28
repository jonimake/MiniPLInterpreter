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

#[derive(PartialEq, Debug)]
enum PLVar<'a> {
	PLString(&'a str),
	PLBool(bool),
	PLInt(i32)
}


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

	initialized: bool,

	single_char_token: Regex,
	two_char_token:	Regex,
	keyword: Regex,
	integer: Regex,
	comment: Regex,
	whitespace: Regex,
	string_literal: Regex,
	identifier: Regex,

}

impl<'a>  TokenIterator<'a>  {
	//type Item = str;

	pub fn new(string: &'a str) -> TokenIterator {
		let single_char_token = Regex::new(r"\+|-|\*|/|<|=|&|!|\(|\)|;|\.|:").unwrap();
		let two_char_token = Regex::new(r"(\.{2})|(:={1})").unwrap();
		let keyword = Regex::new(r"(var)|(for)|(end)|(in)|(do)|(read)|(print)|(int)|(string)|(bool)|(assert)").unwrap();
		let integer = Regex::new(r"\b\d+\b").unwrap();
		let comment = Regex::new(r"(/\*.*\*/)|(//\p{White_Space}*)").unwrap();
		let string_literal = Regex::new(r#"^"([^"\\]|\\.)*"$"#).unwrap();
		let whitespace = Regex::new(r"\p{White_Space}*").unwrap();
		let identifier = Regex::new(r"^[A-Za-z][A-Za-z0-9]*|(\b[A-Za-z]+[0-9]*)").unwrap();
		//let allRegex: Vec<Regex> = vec!(single_char_token.clone(), two_char_token.clone(), keyword.clone(), integer.clone(), real_number.clone(), comment.clone(), identifier.clone());
		let allRegex: Vec<(TokenType, Regex)> = vec!(
		(TokenType::string_literal, string_literal.clone()),
		(TokenType::keyword, keyword.clone()),
		(TokenType::identifier, identifier.clone()),
		(TokenType::two_char, two_char_token.clone()),
		(TokenType::single_char, single_char_token.clone()),
		(TokenType::integer, integer.clone()));
        let t = TokenIterator {
			current_column: 0,
			initialized: false,
            single_char_token: single_char_token,
			two_char_token:	two_char_token,	
			keyword: keyword,
			integer: integer,
			comment: comment,
			string_literal: string_literal,
			whitespace: whitespace,
			identifier: identifier,
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
		//println!("{}, {}", self.current_line_char_pos, self.current_line.len());
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
		let mut lexeme_candidate = "";
		let mut found = false;
		let mut skipped_initial_whitespace = false;
		let mut skipping_whitespace = false;
		let mut token_start_on_line = 0;
		let mut t_start = usize::MAX;
		let mut t_end = usize::MIN;
		let mut token_type = TokenType::NA;
		println!(
"##################################################################################################");
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
			println!("looping regexes for slice: {:?} ", lexeme_candidate);
			for tuple_ref in self.allRegex.iter() {
				let (tokentype, ref rex): (TokenType, Regex) = *tuple_ref;
				println!("{:?}", tokentype);
				if let Some((start, end)) = rex.find(lexeme_candidate) {
					let line_pos_start = self.current_line_char_pos+start;
					let line_pos_end = self.current_line_char_pos+end;
					self.visualize_line_slice("", line_pos_start, line_pos_end);
					if(line_pos_start <= t_start && line_pos_end > t_end) {



						lexeme = lexeme_candidate;
						t_start = line_pos_start;
						t_end = line_pos_end;
						token_start_on_line = line_pos_start;
						token_type = tokentype;
						found = true;

						//self.visualize_line_slice(lexeme_candidate, self.current_line_char_pos, column);
					}
				}
			}
			println!(" ");
		}

		if found {
			token = Some(Token{
				line: self.current_line_number + 1,
				column: token_start_on_line + t_start + self.current_line_char_pos + 1,
				lexeme: lexeme,
				token_type: token_type});

			//set next slice start on line

			self.current_line_char_pos += token_start_on_line + t_end;

		}
		println!(
		"##################################################################################################");
		token
	}
}

#[test]
fn match_line_enumerator_with_slice() -> () {
	let code = include_str!("../sample4.txt");
	println!("{}", code);
	let mut iterator = TokenIterator::new(code);
	assert_eq!(Some(Token{line:1, column: 1, lexeme: "var", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 5, lexeme: "X", token_type: TokenType::identifier}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 7, lexeme: ":", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 9, lexeme: "int", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 12, lexeme: ";", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 1, lexeme: "X", token_type: TokenType::keyword}),iterator.next());
	//assert_eq!(Some(Token{line:2, column: 3, lexeme: ":=", token_type: TokenType::keyword}),iterator.next());
	//assert_eq!(Some(Token{line:2, column: 6, lexeme: "15", token_type: TokenType::keyword}),iterator.next());
	//assert_eq!(Some(Token{line:2, column: 8, lexeme: ";", token_type: TokenType::keyword}),iterator.next());
	//assert_eq!(Some(Token{line:3, column: 1, lexeme: "print", token_type: TokenType::keyword}),iterator.next());
	//assert_eq!(Some(Token{line:3, column: 7, lexeme: "X", token_type: TokenType::keyword}),iterator.next());
	//assert_eq!(Some(Token{line:3, column: 8, lexeme: ";", token_type: TokenType::keyword}),iterator.next());

}

/*
#[test]
fn tokenize_int_var_definition() -> () {
	let code = "  var x : int := 56;";
	let mut iterator = TokenIterator::new(code);
	assert_eq!(Some(Token{line:1, column: 3, lexeme: "var", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 7, lexeme:"x", token_type: TokenType::identifier}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 9, lexeme:":", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 11, lexeme:"int", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 15, lexeme:":=", token_type: TokenType::two_char}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 18, lexeme:"56", token_type: TokenType::integer}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 20, lexeme:";", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(None,iterator.next());
}

#[test]
fn tokenize_int_var_declaration_definition_multiline() -> () {
	let code = "  var x : int;\nx := 56;";
	let mut iterator = TokenIterator::new(code);
	assert_eq!(Some(Token{line:1, column: 3, lexeme: "var", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 7, lexeme:"x", token_type: TokenType::identifier}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 9, lexeme:":", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 11, lexeme:"int", token_type: TokenType::keyword}),iterator.next());
	assert_eq!(Some(Token{line:1, column: 14, lexeme:";", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 1, lexeme:"x", token_type: TokenType::identifier}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 3, lexeme:":=", token_type: TokenType::two_char}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 6, lexeme:"56", token_type: TokenType::integer}),iterator.next());
	assert_eq!(Some(Token{line:2, column: 8, lexeme:";", token_type: TokenType::single_char}),iterator.next());
	assert_eq!(None,iterator.next());
}
*/
/*
#[test]
fn tokenize_bool_var_definition() -> () {
	let code = "var x : bool := true;";
	let mut iterator = TokenIterator::new(code);
	assert_eq!("var",iterator.next().unwrap());
	assert_eq!("x",iterator.next().unwrap());
	assert_eq!(":",iterator.next().unwrap());
	assert_eq!("bool",iterator.next().unwrap());
	assert_eq!(":=",iterator.next().unwrap());
	assert_eq!("true",iterator.next().unwrap());
	assert_eq!(";",iterator.next().unwrap());
	assert_eq!(None, iterator.next())
}
#[test]
fn tokenize_for_loop() -> () {
	let code =		"for x in 0..nTimes-1 do \
						print x; \
					end for;";
	let mut iterator = TokenIterator::new(code);
	assert_eq!("for",iterator.next().unwrap());
	assert_eq!("x",iterator.next().unwrap());
	assert_eq!("in",iterator.next().unwrap());
	assert_eq!("0",iterator.next().unwrap());
	assert_eq!("..",iterator.next().unwrap());
	assert_eq!("nTimes",iterator.next().unwrap());
	assert_eq!("-",iterator.next().unwrap());
	assert_eq!("1",iterator.next().unwrap());
	assert_eq!("do",iterator.next().unwrap());
	assert_eq!("print",iterator.next().unwrap());
	assert_eq!("x",iterator.next().unwrap());
	assert_eq!(";",iterator.next().unwrap());
	assert_eq!("end",iterator.next().unwrap());
	assert_eq!("for",iterator.next().unwrap());
	assert_eq!(";",iterator.next().unwrap());
	
}

#[test]
fn tokenize_string_literal() -> () {
	let code = r#""Give a number";"#;
	let mut iterator = TokenIterator::new(code);
	assert_eq!(r#""Give a number""#,iterator.next().unwrap());
	assert_eq!(";",iterator.next().unwrap());
}

#[test]
fn test_regex_string_literal() -> () {
	let mut parser = TokenIterator::new("\"test string\"");
	let string_literal: Regex = parser.string_literal;
	
	assert!(string_literal.is_match(&r#""test string""#));
}

#[test]
fn test_regex_empty_string_literal() -> () {
	let mut parser = TokenIterator::new("\"\"");
	let string_literal: Regex = parser.string_literal;

	assert!(string_literal.is_match(&r#""test string""#));
}

#[test]
fn test_regex_invalid_string_literal() -> () {
	let mut parser = TokenIterator::new("test string\"");
	let string_literal: Regex = parser.string_literal;

	assert!(!string_literal.is_match(&r#"test string""#));
}
*/
/*
#[test]
fn read_string_token() -> () {
	let code = "\"Give a number\";";
	let mut iterator = TokenIterator::new(code);
	let token = iterator.next().unwrap();
	assert_eq!(Token::String_literal("\"Give a number\""), token);
}*/
/*



#[test]
fn tokenize_keyword() -> () {
	let code = "print for var ";
	let mut iterator = TokenIterator::new(code);
	assert_eq!("print", iterator.next().unwrap());
	assert_eq!("for", iterator.next().unwrap());
	assert_eq!("var", iterator.next().unwrap());
}*/
/*
#[test]
fn tokenize_int_var_declaration() -> () {
	let code = "var x : int;";
	let mut iterator = TokenIterator::new(code);
	let token = iterator.next().unwrap();
	assert_eq!(Token::Var_decl("x", PLVar::PLInt(0)), token);
	
}

#[test]
fn tokenize_string_var_definition() -> () {
	let code = "var x : string := \"this is a string\";";
	let mut iterator = TokenIterator::new(code);
	let token = iterator.next().unwrap();
	assert_eq!(Token::Var_decl("x", PLVar::PLString("\"this is a string\"")), token);
}
*/
/*
#[test]
fn test_regex_whitespace() -> () {	
	let mut parser = TokenIterator::new("test string");
	let whitespace = parser.whitespace;
	assert!(whitespace.is_match(&" "), "failure");
	assert!(whitespace.is_match(&"   "), "failure");
	assert!(whitespace.is_match(&"			"), "failure");
	assert!(whitespace.is_match(&"			"), "failure");

}

#[test]
fn test_regex_keyword() -> () {
	let mut parser = TokenIterator::new("test string");
	let keyword: Regex = parser.keyword;
	assert!(keyword.is_match(&"var"));
	assert!(keyword.is_match(&" var "));
	assert!(keyword.is_match(&"for"));
}

#[test]
fn test_regex_comments() -> () {
	let mut parser = TokenIterator::new("test string");
	let comment: Regex = parser.comment;
	assert!(comment.is_match(&"/*this is a comment*/"));
	assert!(comment.is_match(&"//this is another comment"));
}

#[test]
fn test_regex_integer() -> () {
	let mut parser = TokenIterator::new("test string");
	let integer: Regex = parser.integer;
	
	assert!(integer.is_match(&"1"));
	assert!(integer.is_match(&"-1"));
	assert!(!integer.is_match(&" "));
	assert!(integer.is_match(&"123"));
	assert!(!integer.is_match(&"123aasd"));
	assert!(!integer.is_match(&"asd123"));
}

#[test]
fn test_regex_two_char_token() -> () {
	let mut parser = TokenIterator::new("test string");
	let two_char_token: Regex =	parser.two_char_token;
	assert!(two_char_token.is_match(&".."));
	assert!(two_char_token.is_match(&":="));
	assert!(two_char_token.is_match(&":==")); //throw an error after checking tokens
	assert!(two_char_token.is_match(&"..."));
	assert!(!two_char_token.is_match(&"."));
}

#[test]
fn test_regex_identitier() -> () {
	let mut parser = TokenIterator::new("test string");
	let identifier: Regex =	parser.identifier;
	assert!(identifier.is_match(&"foo"));
	assert!(identifier.is_match(&"foo123"));
	assert!(!identifier.is_match(&"123foo"));
}

#[test]
fn test_regex_single_token() -> () {
	let mut parser = TokenIterator::new("test string");
	let single_char_token: Regex = parser.single_char_token;
	assert!(single_char_token.is_match(&"/"));
	assert!(single_char_token.is_match(&"-"));
	assert!(single_char_token.is_match(&"("));
	assert!(single_char_token.is_match(&")"));
	assert!(single_char_token.is_match(&"+"));
	assert!(single_char_token.is_match(&"."));
}
*/