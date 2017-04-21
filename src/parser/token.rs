use lexeme::Lexeme;
use parser::token_type::TokenType;

use std::fmt;

#[derive(PartialEq, Debug, Clone, Copy)]
//#[derive(PartialEq, Debug, Clone)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: Lexeme<'a>
}

impl<'a> Token<'a> {
    pub fn new(tt: TokenType, l: Lexeme<'a>) -> Token<'a> {
        Token{
            token_type: tt,
            lexeme: l
        }
    }

    pub fn newString(tt: TokenType, l: &'a str) -> Token<'a> {
        let mut lexeme = Lexeme::default();
        lexeme.lexeme = l;
        Token{
            token_type: tt,
            lexeme: lexeme
        }
    }
}


impl<'a> Default for Token<'a> {
    fn default() -> Token<'a> {
        Token{token_type: TokenType::EOF, lexeme: Lexeme::default()}
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.token_type)
    }
}
