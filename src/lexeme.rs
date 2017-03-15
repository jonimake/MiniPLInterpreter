use std::fmt;

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Lexeme<'a> {
    pub lexeme_type: LexemeType,
    pub lexeme: &'a str,
    pub line: usize,
    pub column: usize
}

impl<'a> Lexeme<'a> {
    pub fn new(s: &'a str) -> Lexeme<'a> {
        Lexeme {
            lexeme_type: LexemeType::default(),
            lexeme: s,
            line: 0,
            column: 0
        }
    }
}

impl<'a> fmt::Display for Lexeme<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\t ({}:{})\t [{:?}]", self.lexeme, self.line, self.column, self.lexeme_type)
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum LexemeType {
    single_char,
    two_char,
    keyword,
    integer,
    string_literal,
    identifier,
    NA
}

impl<'a> Default for Lexeme<'a> {
    fn default() -> Lexeme<'a> {
        Lexeme{column: 0, line: 0,lexeme:"",lexeme_type:LexemeType::default()}
    }
}

impl Default for LexemeType {
    fn default() -> LexemeType {
        LexemeType::NA
    }
}