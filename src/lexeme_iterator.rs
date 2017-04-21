use std::str::FromStr;
use std::str::Lines;
use std::cmp;
use std::i32;
use std::usize;
use std::iter::Enumerate;
use std::iter::Peekable;
use std::str::CharIndices;

use lexeme::Lexeme;
use lexeme::LexemeType;

const SINGLE_CHAR_LEXEME: &'static [&'static str] = &["+","-","*","/","<","=","&","!","(",")",";",".",":"];
const TWO_CHAR_LEXEME: &'static [&'static str] = &["..",":="];
const KEYWORD: &'static [&'static str] = &["assert","string","print","bool","read","var","for","end","int","in","do"];

//#[derive(Clone)]
pub struct LexemeIterator<'a>  {
	text: &'a str,
	lines: Enumerate<Lines<'a>>,


	current_line_number: usize,
	current_line_char_pos: usize,
	current_line: &'a str,

	slice_start: usize,
	initialized: bool,
	lexeme_matchers: Vec<(LexemeType, fn(&str) -> bool)>
}


impl<'a> Clone for LexemeIterator<'a> {
	fn clone(&self) -> LexemeIterator<'a> {
		let lexeme_matchers: Vec<(LexemeType, fn(&str) -> bool)> = vec!(
			(LexemeType::string_literal, is_string_literal),
			(LexemeType::keyword, is_keyword_lexeme),
			(LexemeType::identifier, is_identifier),
			(LexemeType::two_char, is_two_char_lexeme),
			(LexemeType::single_char, is_single_char_lexeme),
			(LexemeType::integer, is_integer));

		//let l = Enumerate<Lines<'a>>
		LexemeIterator {
			initialized: self.initialized,
			text: self.text,
			lexeme_matchers: lexeme_matchers,
			lines: self.text.lines().enumerate(),
			current_line: self.current_line,
			slice_start: self.slice_start,
			current_line_number: self.current_line_number,
			current_line_char_pos: self.current_line_char_pos,
		}
	}
}

impl<'a>  LexemeIterator<'a>  {
	//type Item = str;

	pub fn new(string: &'a str) -> LexemeIterator {

		let lexeme_matchers: Vec<(LexemeType, fn(&str) -> bool)> = vec!(
			(LexemeType::string_literal, is_string_literal),
			(LexemeType::keyword, is_keyword_lexeme),
			(LexemeType::identifier, is_identifier),
			(LexemeType::two_char, is_two_char_lexeme),
			(LexemeType::single_char, is_single_char_lexeme),
			(LexemeType::integer, is_integer));
		let src = string.trim_left_matches("\u{feff}"); //strip BOM

		let t = LexemeIterator {
			initialized: false,
            text: src,
			lexeme_matchers: lexeme_matchers,
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

impl<'a> Iterator for LexemeIterator<'a> {
	type Item = Lexeme<'a>;

	fn next(&mut self) -> Option<Lexeme<'a>> {
		let mut lexeme: Option<Lexeme> = None;

		if !self.handle_end_of_line() {
			return lexeme;
		}

		let mut lexemeStr = "";
		let mut lexeme_candidate;
		let mut found = false;
		let mut skipped_initial_whitespace = false;
		let mut skipping_whitespace = false;
		let mut lexeme_start_on_line = 0;
		let mut t_end = usize::MIN;
		let mut lexeme_type = LexemeType::NA;
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

			for tuple_ref in self.lexeme_matchers.iter() {
				let (lexemetype, matcher_fn): (LexemeType, fn(&str) -> bool) = *tuple_ref;
				if(matcher_fn(lexeme_candidate)) {
					let line_pos_start = self.current_line_char_pos;
					let line_pos_end = self.current_line_char_pos + lexeme_candidate.len();
					let lexeme_length = lexeme_candidate.chars().count();
					if(lexeme_length > longest_lexeme) { //prioritize length
						//println!("New longest lexeme {:?}, {:?}, {:?}", lexeme_candidate ,lexemetype, lexeme_length);
						longest_lexeme = lexeme_length;
						lexemeStr = lexeme_candidate;

						t_end = column;
						lexeme_start_on_line = line_pos_start;
						lexeme_type = lexemetype;
						found = true;
					}
				}
			}
		}

		if found {
			lexeme = Some(Lexeme{
				line: self.current_line_number + 1,
				column: lexeme_start_on_line + 1,
				lexeme: lexemeStr,
				lexeme_type: lexeme_type});

			//set next slice start on line
			self.current_line_char_pos = t_end;

		} else {
			return self.next(); //not found, try again. EOL breaks recursive iteration
		}
		lexeme
	}
}

fn is_single_char_lexeme(lexeme: &str) -> bool { SINGLE_CHAR_LEXEME.iter().any(|&x| x == lexeme)}

fn is_two_char_lexeme(lexeme: &str) -> bool { TWO_CHAR_LEXEME.iter().any(|&x| x == lexeme)}

fn is_keyword_lexeme(lexeme: &str) -> bool {	KEYWORD.iter().any(|&x| x == lexeme)}

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

fn is_integer(lexeme: &str) -> bool {
	!lexeme.starts_with("-") && !lexeme.starts_with("+") && lexeme.parse::<i32>().is_ok()
}

fn is_identifier(lexeme: &str) -> bool {
	let allAreAlphaNum = lexeme.chars().all(|ch: char| {
		ch.is_alphanumeric() || ch == '_'
	});
	let firstIsAlphabetic = lexeme.chars().nth(0).unwrap_or('1').is_alphabetic();
	firstIsAlphabetic && allAreAlphaNum
}



#[test]
fn recornize_integer() -> () {
	assert_eq!(false, is_integer("1.0"));
	assert_eq!(true, is_integer("1"));
	assert_eq!(false, is_integer("+1"));
	assert_eq!(false, is_integer("-1"));
	assert_eq!(false, is_integer("-123"));
	assert_eq!(true, is_integer("123"));
	assert_eq!(false, is_integer("+123"));
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
fn lexemeize_integer_assignment() -> () {
	let code = r#"var x : int :=4-2;"#;
	println!("{}", code);
	let mut iterator = LexemeIterator::new(code);
	assert_eq!(Some(Lexeme{line:1, column: 1, lexeme: "var", lexeme_type: LexemeType::keyword}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 5, lexeme: "x", lexeme_type: LexemeType::identifier}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 7, lexeme: ":", lexeme_type: LexemeType::single_char}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 9, lexeme: "int", lexeme_type: LexemeType::keyword}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 13, lexeme: ":=", lexeme_type: LexemeType::two_char}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 15, lexeme: "4", lexeme_type: LexemeType::integer}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 16, lexeme: "-", lexeme_type: LexemeType::single_char}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 17, lexeme: "2", lexeme_type: LexemeType::integer}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 18, lexeme: ";", lexeme_type: LexemeType::single_char}),iterator.next());
}

#[test]
fn lexemeize_two_consecutive() -> () {
	let code = include_str!("../sample4.txt");
	println!("{}", code);
	let mut iterator = LexemeIterator::new(code);
	assert_eq!(Some(Lexeme{line:1, column: 1, lexeme: "var", lexeme_type: LexemeType::keyword}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 5, lexeme: "X", lexeme_type: LexemeType::identifier}),iterator.next());
}

#[test]
fn lexemeize_two_consecutive_file_3() -> () {
	let code = include_str!("../sample3.txt");
	println!("{}", code);
	let mut iterator = LexemeIterator::new(code);
	assert_eq!(Some(Lexeme{line:1, column: 1, lexeme: "print", lexeme_type: LexemeType::keyword}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 7, lexeme: "\"Give a number\"", lexeme_type: LexemeType::string_literal}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 22, lexeme: ";", lexeme_type: LexemeType::single_char}),iterator.next());
	assert_eq!(Some(Lexeme{line:2, column: 1, lexeme: "var", lexeme_type: LexemeType::keyword}),iterator.next());
	assert_eq!(Some(Lexeme{line:2, column: 5, lexeme: "n", lexeme_type: LexemeType::identifier}),iterator.next());
}

#[test]
fn lexemeize_full_line() -> () {
	let code = include_str!("../sample4.txt");
	println!("{}", code);
	let mut iterator = LexemeIterator::new(code);
	assert_eq!(Some(Lexeme{line:1, column: 1, lexeme: "var", lexeme_type: LexemeType::keyword}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 5, lexeme: "X", lexeme_type: LexemeType::identifier}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 7, lexeme: ":", lexeme_type: LexemeType::single_char}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 9, lexeme: "int", lexeme_type: LexemeType::keyword}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 12, lexeme: ";", lexeme_type: LexemeType::single_char}),iterator.next());
}

#[test]
fn lexemeize_multiple_lines() -> () {
	let code = include_str!("../sample4.txt");
	println!("{}", code);
	let mut iterator = LexemeIterator::new(code);
	iterator.next();
	iterator.next();
	iterator.next();
	iterator.next();
	iterator.next();
	assert_eq!(Some(Lexeme{line:2, column: 1, lexeme: "X", lexeme_type: LexemeType::identifier}),iterator.next());
	assert_eq!(Some(Lexeme{line:2, column: 3, lexeme: ":=", lexeme_type: LexemeType::two_char}),iterator.next());
	assert_eq!(Some(Lexeme{line:2, column: 6, lexeme: "15", lexeme_type: LexemeType::integer}),iterator.next());
	assert_eq!(Some(Lexeme{line:2, column: 8, lexeme: ";", lexeme_type: LexemeType::single_char}),iterator.next());
	assert_eq!(Some(Lexeme{line:3, column: 1, lexeme: "print", lexeme_type: LexemeType::keyword}),iterator.next());
	assert_eq!(Some(Lexeme{line:3, column: 7, lexeme: "X", lexeme_type: LexemeType::identifier}),iterator.next());
	assert_eq!(Some(Lexeme{line:3, column: 8, lexeme: ";", lexeme_type: LexemeType::single_char}),iterator.next());
}

#[test]
fn recorgnize_expression() -> () {
	let code = "1+1";
	println!("{}", code);
	let mut iterator = LexemeIterator::new(code);
	assert_eq!(Some(Lexeme{line:1, column: 1, lexeme: "1", lexeme_type: LexemeType::integer}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 2, lexeme: "+", lexeme_type: LexemeType::single_char}),iterator.next());
	assert_eq!(Some(Lexeme{line:1, column: 3, lexeme: "1", lexeme_type: LexemeType::integer}),iterator.next());
}
