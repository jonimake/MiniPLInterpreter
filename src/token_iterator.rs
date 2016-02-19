use regex;
use regex::Regex;
use std::str::FromStr;
use std::cmp;

pub struct TokenIterator<'a>  {
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
		let string_literal = Regex::new(r#"^"([^"\\]|\\.)*"?$"#).unwrap();
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
		let mut last_was_escape = false;
		//need to find longest match
		let mut is_string = false;
		while let Some(tuple) = char_indices.next() {	
			let (ind, ch) = tuple;			
			unsafe {
				slice = self.text.slice_unchecked(0, ind+1); //ind plus one so the slice encompasses the chars between 0 and this potition
			}
			//special case handling for strings
			if(ind == 0 && ch == '"' || is_string) {
				is_string = true;
				last_valid_index = ind+1;
				if(ch == '\\') {
					last_was_escape = true;
					continue;
				}
				if(ind > 0 && ch == '"' && !last_was_escape) {
					break;
				}
				last_was_escape = false;
				continue;
			}


			//println!("index:{}, slice:{}", ind, slice);
			let mut found = false;
			let peek_tuple: Option<&(usize, char)> = char_indices.peek();
			for rex in &self.allRegex {
				if let Some((start, end)) = rex.find(slice) {
					if(start == 0) {
						last_valid_index = cmp::max(last_valid_index, end);
						found = true;
					}
				}
			}
			if !found {break}
		}
		let (first, second) = self.text.split_at(last_valid_index);
		self.text = second;
		if(first.len() == 0) {
			return None;
		}
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
	let r = regex::quote("\"(\\\"|[^\"])*\"");
	println!("{:?}", r);
	//assert_eq!(true, false);
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
fn tokenize_keyword() -> () {
	let code = "print for var ";
	let mut iterator = TokenIterator::new(code);
	assert_eq!("print", iterator.next().unwrap());
	assert_eq!("for", iterator.next().unwrap());
	assert_eq!("var", iterator.next().unwrap());
}
