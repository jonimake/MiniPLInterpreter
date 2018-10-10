use ::std::collections::hash_map::DefaultHasher;
use ::std::fmt;
use ::std::hash::Hash;
use ::std::hash::Hasher;

use crate::lexer;
use crate::lexer::lexeme::Lexeme;
use crate::lexer::lexeme::LexemeType;
use crate::parser::token_type::TokenType;

#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<Lexeme>,
}

impl Token {
    pub fn new(tt: TokenType, l: Lexeme) -> Token {
        Token {
            token_type: tt,
            lexeme: Some(l),
        }
    }

    pub fn new_string(tt: TokenType, l: &str) -> Token {
        let mut lexeme = Lexeme::default();
        lexeme.lexeme = l.to_string();
        Token {
            token_type: tt,
            lexeme: Some(lexeme),
        }
    }
}

impl Default for Token {
    fn default() -> Token {
        Token {
            token_type: TokenType::EOF,
            lexeme: None,
        }
    }
}
/*
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(t) = self.lexeme.clone() {
            write!(f, "{}, '{}', [line {}, column {}]", self.token_type, t.lexeme, t.line, t.column)
        } else {
            write!(f, "{}", self.token_type)
        }
    }
}
*/
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

#[derive(Debug)]
pub enum TokenParseError {
    Fail(String),
}

impl lexer::lexeme::FromLexeme for Token {
    type Err = TokenParseError;
    fn from_lexeme(lx: Lexeme) -> Result<Token, Self::Err> {
        let token = get_token(lx);
        match token {
            Ok(t) => Ok(t),
            Err(msg) => Result::Err(TokenParseError::Fail(msg)),
        }
    }
}

fn get_token(lx: Lexeme) -> Result<Token, String> {
    match lx {
        Lexeme {
            lexeme_type: LexemeType::SingleChar,
            ..
        } => get_single_char_token(lx),
        Lexeme {
            lexeme_type: LexemeType::TwoChar,
            ..
        } => get_two_char_token(lx),
        Lexeme {
            lexeme_type: LexemeType::Keyword,
            ..
        } => get_keyword_token(lx),
        Lexeme {
            lexeme_type: LexemeType::Bool,
            ..
        } => get_bool_token(lx),
        Lexeme {
            lexeme_type: LexemeType::Integer,
            ..
        } => get_integer_token(lx),
        Lexeme {
            lexeme_type: LexemeType::Identifier,
            ..
        } => get_identifier_token(lx),
        Lexeme {
            lexeme_type: LexemeType::StringLiteral,
            ..
        } => get_string_literal_token(lx),
        _ => Err(format!("Invalid token:{:?}", lx)),
    }
}

fn get_string_literal_token(lx: Lexeme) -> Result<Token, String> {
    let mut hasher = DefaultHasher::new();
    lx.lexeme.hash(&mut hasher);
    let hash: u64 = hasher.finish();
    Ok(Token::new(TokenType::StringLiteral(hash), lx))
}

fn get_identifier_token(lx: Lexeme) -> Result<Token, String> {
    Ok(Token::new(TokenType::Identifier, lx))
}

fn get_integer_token(lx: Lexeme) -> Result<Token, String> {
    if let Ok(value) = lx.lexeme.parse::<i32>() {
        Ok(Token::new(TokenType::IntegerValue(value), lx))
    } else {
        Err(format!("Invalid token:{:?}", lx))
    }
}

fn get_bool_token(lx: Lexeme) -> Result<Token, String> {
    if let Ok(value) = lx.lexeme.parse::<bool>() {
        Ok(Token::new(TokenType::BooleanValue(value), lx))
    } else {
        Err(format!("Invalid token:{:?}", lx))
    }
}

fn get_two_char_token(lx: Lexeme) -> Result<Token, String> {
    match lx.lexeme.to_lowercase().as_ref() {
        ".." => Ok(Token::new(TokenType::RangeDots, lx)),
        ":=" => Ok(Token::new(TokenType::ValueDefinition, lx)),
        _ => Err(format!("Invalid token:{:?}", lx)),
    }
}

fn get_keyword_token(lx: Lexeme) -> Result<Token, String> {
    match lx.lexeme.to_lowercase().as_ref() {
        "var" => Ok(Token::new(TokenType::VarKeyword, lx)),
        "assert" => Ok(Token::new(TokenType::Assert, lx)),
        "string" => Ok(Token::new(TokenType::StringType, lx)),
        "print" => Ok(Token::new(TokenType::Print, lx)),
        "read" => Ok(Token::new(TokenType::Read, lx)),
        "write" => Ok(Token::new(TokenType::Write, lx)),
        "end" => Ok(Token::new(TokenType::End, lx)),
        "in" => Ok(Token::new(TokenType::In, lx)),
        "do" => Ok(Token::new(TokenType::Do, lx)),
        "for" => Ok(Token::new(TokenType::For, lx)),
        "int" => Ok(Token::new(TokenType::IntegerType, lx)),
        "bool" => Ok(Token::new(TokenType::BooleanType, lx)),
        "true" => Ok(Token::new(TokenType::BooleanValue(true), lx)),
        "false" => Ok(Token::new(TokenType::BooleanValue(false), lx)),
        _ => Err(format!("Invalid token:{:?}", lx)),
    }
}

fn get_single_char_token(lx: Lexeme) -> Result<Token, String> {
    match lx.lexeme.to_lowercase().as_ref() {
        "(" => Ok(Token::new(TokenType::LParen, lx)),
        ")" => Ok(Token::new(TokenType::RParen, lx)),
        "+" => Ok(Token::new(TokenType::Addition, lx)),
        "-" => Ok(Token::new(TokenType::Subtraction, lx)),
        "*" => Ok(Token::new(TokenType::Multiplication, lx)),
        "/" => Ok(Token::new(TokenType::Division, lx)),
        "<" => Ok(Token::new(TokenType::LessThan, lx)),
        "=" => Ok(Token::new(TokenType::Equal, lx)),
        "&" => Ok(Token::new(TokenType::And, lx)),
        "!" => Ok(Token::new(TokenType::Negation, lx)),
        ";" => Ok(Token::new(TokenType::StatementEnd, lx)),
        "." => Ok(Token::new(TokenType::Stop, lx)),
        ":" => Ok(Token::new(TokenType::TypeDeclaration, lx)),
        _ => Err(format!("Invalid token:{:?}", lx)),
    }
}
