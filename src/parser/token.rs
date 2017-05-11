use lexeme::Lexeme;
use parser::token_type::TokenType;

use std::fmt;

//#[derive(PartialEq, Debug, Clone, Copy)]
#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Lexeme,
}

impl Token {
    pub fn new(tt: TokenType, l: Lexeme) -> Token {
        Token {
            token_type: tt,
            lexeme: l,
        }
    }

    pub fn newString(tt: TokenType, l: &str) -> Token {
        let mut lexeme = Lexeme::default();
        lexeme.lexeme = l.to_string();
        Token {
            token_type: tt,
            lexeme: lexeme,
        }
    }
}


impl Default for Token {
    fn default() -> Token {
        Token {
            token_type: TokenType::EOF,
            lexeme: Lexeme::default(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_type)
    }
}
