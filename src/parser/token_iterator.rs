use std::iter::Peekable;
use std::hash::Hash;
use std::hash::Hasher;
use std::collections::hash_map::DefaultHasher;

use lexer::lexeme::Lexeme;
use lexer::lexeme::LexemeType;
use lexer::lexeme_iterator::LexemeIterator;
use parser::token::Token;
use parser::token_type::TokenType;

fn get_token(lx: Lexeme) -> Result<Token, String> {
    match lx {
        Lexeme { lexeme_type: LexemeType::SingleChar, .. } => get_single_char_token(lx),
        Lexeme { lexeme_type: LexemeType::TwoChar, .. } => get_two_char_token(lx),
        Lexeme { lexeme_type: LexemeType::Keyword, .. } => get_keyword_token(lx),
        Lexeme { lexeme_type: LexemeType::Bool, .. } => get_bool_token(lx),
        Lexeme { lexeme_type: LexemeType::Integer, .. } => get_integer_token(lx),
        Lexeme { lexeme_type: LexemeType::Identifier, .. } => get_identifier_token(lx),
        Lexeme { lexeme_type: LexemeType::StringLiteral, .. } => get_string_literal_token(lx),
        _ => Err(format!("Invalid token:{:?}", lx)),
    }
}


fn get_string_literal_token(lx: Lexeme) -> Result<Token, String> {
    let mut hasher = DefaultHasher::new();
    lx.lexeme.hash(&mut hasher);
    let hash: u64 = hasher.finish();
    Ok(Token {
           lexeme: lx,
           token_type: TokenType::StringLiteral(hash),
       })
}

fn get_identifier_token(lx: Lexeme) -> Result<Token, String> {
    Ok(Token {
           lexeme: lx,
           token_type: TokenType::Identifier,
       })
}

fn get_integer_token(lx: Lexeme) -> Result<Token, String> {
    if let Ok(value) = lx.lexeme.parse::<i32>() {
        Ok(Token {
               lexeme: lx,
               token_type: TokenType::IntegerValue(value),
           })
    } else {
        Err(format!("Invalid token:{:?}", lx))
    }
}

fn get_bool_token(lx: Lexeme) -> Result<Token, String> {
    if let Ok(value) = lx.lexeme.parse::<bool>() {
        Ok(Token {
            lexeme: lx,
            token_type: TokenType::BooleanValue(value)
        })
    } else {
        Err(format!("Invalid token:{:?}", lx))
    }
}

fn get_two_char_token(lx: Lexeme) -> Result<Token, String> {
    match lx.lexeme.to_lowercase().as_ref() {
        ".." => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::RangeDots,
               })
        }
        ":=" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::ValueDefinition,
               })
        }
        _ => Err(format!("Invalid token:{:?}", lx)),
    }
}

fn get_keyword_token(lx: Lexeme) -> Result<Token, String> {
    match lx.lexeme.to_lowercase().as_ref() {
        "var" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::VarKeyword,
               })
        }
        "assert" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Assert,
               })
        }
        "string" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::StringType,
               })
        }
        "print" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Print,
               })
        }
        "read" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Read,
               })
        }
        "write" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Write,
               })
        }
        "end" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::End,
               })
        }
        "in" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::In,
               })
        }
        "do" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Do,
               })
        }
        "for" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::For,
               })
        }
        "int" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::IntegerType,
               })
        }
        "bool" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::BooleanType,
               })
        }
        "true" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::BooleanValue(true),
               })
        }
        "false" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::BooleanValue(false),
               })
        }
        _ => Err(format!("Invalid token:{:?}", lx)),
    }
}

fn get_single_char_token(lx: Lexeme) -> Result<Token, String> {
    match lx.lexeme.to_lowercase().as_ref() {
        "(" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::LParen,
               })
        }
        ")" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::RParen,
               })
        }
        "+" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Addition,
               })
        }
        "-" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Subtraction,
               })
        }
        "*" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Multiplication,
               })
        }
        "/" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Division,
               })
        }
        "<" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::LessThan,
               })
        }
        "=" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Equal,
               })
        }
        "&" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::And,
               })
        }
        "!" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Negation,
               })
        }
        ";" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::StatementEnd,
               })
        }
        "." => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::Stop,
               })
        }
        ":" => {
            Ok(Token {
                   lexeme: lx,
                   token_type: TokenType::TypeDeclaration,
               })
        }
        _ => Err(format!("Invalid token:{:?}", lx)),
    }
}


pub fn parse_statements<'a>(lexemes: &mut Peekable<LexemeIterator<'a>>) -> Result<Vec<Token>, String> {
    let items: Result<Vec<Token>, String> = lexemes.map(get_token).collect();
    items
}

pub fn parse_statement(lexemes: &mut Peekable<LexemeIterator>) -> bool {
    for lx in lexemes {
        println!("{:?}", get_token(lx));
    }
    false
}

#[derive(Clone)]
pub struct TokenIterator<I>
    where I: Iterator<Item = Lexeme>
{
    pub lex_iter: I,
}

impl<'a, I> Iterator for TokenIterator<I>
    where I: Iterator<Item = Lexeme>
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let maybe_lexeme = self.lex_iter.next();
        match maybe_lexeme {
            Some(lexeme) => {
                let x = get_token(lexeme);
                match x {
                    Ok(t) => Some(t),
                    Err(_) => {
                        error!("{:?}",x);
                        None
                    },
                }
            }
            None => None,
        }
    }
}
