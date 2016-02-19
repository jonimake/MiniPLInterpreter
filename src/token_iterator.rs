use regex;
use regex::Regex;
use std::str::FromStr;
use std::cmp;

#[derive(PartialEq, Debug)]
enum PLVar<'a> {
	PLString(&'a str),
	PLBool(bool),
	PLInt(i32)
}

#[derive(PartialEq,Debug)]
enum Token<'a>  {
	Var_decl(&'a str, PLVar<'a>),
	String_literal(&'a str),
	Keyword(&'a str),
}


struct TokenIterator<'a>  {
	single_char_token: Regex,
	two_char_token:	Regex,	
	keyword: Regex,
	integer: Regex,
	comment: Regex,
	whitespace: Regex,
	string_literal: Regex,
	identifier: Regex,
	var_decl: Regex,
	text: &'a str,
	allRegex: Vec<Regex>
}

impl<'a>  TokenIterator<'a>  {
	//type Item = str;

	pub fn new(string: &'a str) -> TokenIterator {
		let single_char_token = Regex::new(r"\+|-|\*|/|<|=|&|!|\(|\)|;|\.|:").unwrap();
		let two_char_token = Regex::new(r"(\.{2})|(:={1})").unwrap();
		let keyword = Regex::new(r"(\bvar\b)|(\bfor\b)|(\bend\b)|(\bin\b)|(\bdo\b)|(\bread\b)|(\bprint\b)|(\bint\b)|(\bstring\b)|(\bbool\b)|(\bassert\b)").unwrap();
		let integer = Regex::new(r"\b\d+\b").unwrap();
		let comment = Regex::new(r"(/\*.*\*/)|(//\p{White_Space}*)").unwrap();
		let string_literal = Regex::new(r#"^"([^"\\]|\\.)*"$"#).unwrap();
		let whitespace = Regex::new(r"\p{White_Space}*").unwrap();
		let identifier = Regex::new(r"^[A-Za-z][A-Za-z0-9]*|(\b[A-Za-z]+[0-9]*)").unwrap();
		let var_decl = Regex::new(r"var .*:.*(int|string|bool)").unwrap();
		//let allRegex: Vec<Regex> = vec!(single_char_token.clone(), two_char_token.clone(), keyword.clone(), integer.clone(), real_number.clone(), comment.clone(), identifier.clone());
		let allRegex: Vec<Regex> = vec!(string_literal.clone(),keyword.clone(), identifier.clone(), two_char_token.clone(), single_char_token.clone(), integer.clone());
        TokenIterator {
            single_char_token: single_char_token,
			two_char_token:	two_char_token,	
			keyword: keyword,
			integer: integer,
			comment: comment,
			string_literal: string_literal,
			whitespace: whitespace,
			identifier: identifier,
			text: string,
			var_decl: var_decl,
			allRegex: allRegex
        }
    }
}

impl<'a> Iterator for TokenIterator<'a> {
	type Item = &'a str;
	//type Item = Token<'a>;

    fn next(&mut self) -> Option<&'a str> {
		self.text = self.text.trim_left();

		if(self.text.len() == 0) {
			return None;
		}

		let mut char_indices = self.text.char_indices().peekable();

		let mut end_index: usize = 0;
		let mut token: Option<&'a str> = None;
		let mut last_valid_index = 0;
		let mut slice = ""; 
		//need to find longest match
		while let Some(tuple) = char_indices.next() {	
			let (ind, ch) = tuple;			
			unsafe {
				slice = self.text.slice_unchecked(0, ind+1); //ind plus one so the slice encompasses the chars between 0 and this potition
			}
			println!("index:{}, slice:{}", ind, slice);
			
			let peek_tuple: Option<&(usize, char)> = char_indices.peek();
			let mut found = false;
			for rex in &self.allRegex {
				if let Some((start, end)) = rex.find(slice) {
					if(start == 0) {
						last_valid_index = cmp::max(last_valid_index, end);
						found = true;
					}
				}
			}
		}
		
		let (first, second) = self.text.split_at(last_valid_index);
		println!("'{}'<=>'{}'", first, second);
		self.text = second;
		Some(first)
    }
}

#[test]
fn tokenize_int_var_definition() -> () {
	let code = "var x : int := 56;";
	let mut iterator = TokenIterator::new(code);
	assert_eq!("var",iterator.next().unwrap());
	assert_eq!("x",iterator.next().unwrap());
	assert_eq!(":",iterator.next().unwrap());
	assert_eq!("int",iterator.next().unwrap());
	assert_eq!(":=",iterator.next().unwrap());
	assert_eq!("56",iterator.next().unwrap());
	assert_eq!(";",iterator.next().unwrap());
}

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