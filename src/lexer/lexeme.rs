use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub struct Lexeme {
    pub lexeme_type: LexemeType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Lexeme {
    pub fn new(s: &str) -> Lexeme {
        Lexeme {
            lexeme_type: LexemeType::default(),
            lexeme: s.to_owned(),
            line: 0,
            column: 0,
        }
    }
}

impl<'a> From<&'a str> for Lexeme {
    fn from(text: &'a str) -> Lexeme {
        Lexeme {
            lexeme_type: LexemeType::default(),
            lexeme: text.to_owned(),
            line: 0,
            column: 0,
        }
    }
}

impl fmt::Display for Lexeme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "{}\t ({}:{})\t [{:?}]",
               self.lexeme,
               self.line,
               self.column,
               self.lexeme_type)
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum LexemeType {
    SingleChar,
    TwoChar,
    Keyword,
    Bool,
    Integer,
    StringLiteral,
    Identifier,
    NA,
}

impl Default for Lexeme {
    fn default() -> Lexeme {
        Lexeme {
            column: 0,
            line: 0,
            lexeme: "".to_owned(),
            lexeme_type: LexemeType::default(),
        }
    }
}

impl Default for LexemeType {
    fn default() -> LexemeType {
        LexemeType::NA
    }
}
