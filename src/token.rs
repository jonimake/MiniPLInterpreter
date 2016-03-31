#[derive(PartialEq,Debug)]
pub struct Token<'a> {
    pub line: usize,
    pub column: usize,
    pub lexeme: &'a str,
    pub token_type: TokenType
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenType {
    single_char,
    two_char,
    keyword,
    integer,
    string_literal,
    identifier,
    NA
}

//let single_char_token = Regex::new(r"\+|-|\*|/|<|=|&|!|\(|\)|;|\.|:").unwrap();
//let two_char_token = Regex::new(r"(\.{2})|(:={1})").unwrap();