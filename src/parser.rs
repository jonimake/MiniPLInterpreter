use std::iter::Peekable;
use std::option::Option;

use lexeme::Lexeme;
use lexeme::LexemeType;
use lexeme_iterator::LexemeIterator;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenType {
    Identifier,
    IntegerValue(i32),
    StringLiteral,
    BooleanValue(bool),

    LParen,
    RParen,
    ValueDefinition,
    TypeDeclaration,

    //keywords and control structures
    VarKeyword,
    For,
    In,
    Do,
    Assert,
    End,
    Print,
    Type(ValueType),
    Write,
    Read,

    //symbols
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LessThan,
    Equal,
    Exclamation,
    And,
    Stop,
    StatementEnd,

    RangeDots,

    EOF
}
#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    String,
    Integer,
    Boolean,
    Any
}

impl PartialEq for ValueType {
    fn eq(&self, other: &ValueType) -> bool {
       true
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: Lexeme<'a>
}


impl<'a> Default for Token<'a> {
    fn default() -> Token<'a> {
        Token{token_type: TokenType::EOF, lexeme: Lexeme::default()}
    }
}

fn getToken<'a> (lx: Lexeme<'a>) -> Result<Token<'a>, String> {
    match lx {
        Lexeme{lexeme_type: LexemeType::single_char,..} => getSingleCharToken(lx),
        Lexeme{lexeme_type: LexemeType::two_char,..} => getTwoCharToken(lx),
        Lexeme{lexeme_type: LexemeType::keyword,..} => getKeywordToken(lx),
        Lexeme{lexeme_type: LexemeType::integer,..} => getIntegerToken(lx),
        Lexeme{lexeme_type: LexemeType::identifier,..} => getIdentifierToken(lx),
        Lexeme{lexeme_type: LexemeType::string_literal,..} => getStringLiteralToken(lx),
        _ => Err(format!("Invalid token:{:?}", lx))
    }
}


fn getStringLiteralToken<'a>(lx: Lexeme<'a>) -> Result<Token<'a>, String> {
    Ok(Token{lexeme:lx, token_type:TokenType::StringLiteral})
}

fn getIdentifierToken<'a>(lx: Lexeme<'a>) -> Result<Token<'a>, String> {
    Ok(Token{lexeme:lx, token_type:TokenType::Identifier})
}

fn getIntegerToken<'a>(lx: Lexeme<'a>) -> Result<Token<'a>, String> {
    if let Ok(value) = lx.lexeme.parse::<i32>() {
        Ok(Token{lexeme:lx, token_type:TokenType::IntegerValue(value)})
    } else {
        Err(format!("Invalid token:{:?}", lx))
    }

}

fn getTwoCharToken<'a>(lx: Lexeme<'a>) -> Result<Token<'a>, String> {
    match lx.lexeme.to_lowercase().as_ref() {
        ".." => Ok(Token{lexeme:lx, token_type:TokenType::RangeDots}),
        ":=" => Ok(Token{lexeme:lx, token_type:TokenType::ValueDefinition}),
        _ => Err(format!("Invalid token:{:?}", lx))
    }
}


fn getKeywordToken<'a>(lx: Lexeme<'a>) -> Result<Token<'a>, String> {
    match lx.lexeme.to_lowercase().as_ref() {
        "var" => Ok(Token{lexeme:lx, token_type:TokenType::VarKeyword}),
        "assert" => Ok(Token{lexeme:lx, token_type:TokenType::Assert}),
        "string" => Ok(Token{lexeme:lx, token_type:TokenType::Type(ValueType::String)}),
        "print" => Ok(Token{lexeme:lx, token_type:TokenType::Print}),
        "read" => Ok(Token{lexeme:lx, token_type:TokenType::Read}),
        "write" => Ok(Token{lexeme:lx, token_type:TokenType::Write}),
        "end" => Ok(Token{lexeme:lx, token_type:TokenType::End}),
        "in" => Ok(Token{lexeme:lx, token_type:TokenType::In}),
        "do" => Ok(Token{lexeme:lx, token_type:TokenType::Do}),
        "for" => Ok(Token{lexeme:lx, token_type:TokenType::For}),
        "int" => Ok(Token{lexeme:lx, token_type:TokenType::Type(ValueType::Integer)}),
        "bool" => Ok(Token{lexeme:lx, token_type:TokenType::Type(ValueType::Boolean)}),
        _ => Err(format!("Invalid token:{:?}", lx))
    }
}

fn getSingleCharToken<'a>(lx: Lexeme<'a>) -> Result<Token<'a>, String> {
    match lx.lexeme.to_lowercase().as_ref() {
        "(" => Ok(Token{lexeme:lx, token_type:TokenType::LParen}),
        ")" => Ok(Token{lexeme:lx, token_type:TokenType::RParen}),
        "+" => Ok(Token{lexeme:lx, token_type:TokenType::Addition}),
        "-" => Ok(Token{lexeme:lx, token_type:TokenType::Subtraction}),
        "*" => Ok(Token{lexeme:lx, token_type:TokenType::Multiplication}),
        "/" => Ok(Token{lexeme:lx, token_type:TokenType::Division}),
        "<" => Ok(Token{lexeme:lx, token_type:TokenType::LessThan}),
        "=" => Ok(Token{lexeme:lx, token_type:TokenType::Equal}),
        "&" => Ok(Token{lexeme:lx, token_type:TokenType::And}),
        "!" => Ok(Token{lexeme:lx, token_type:TokenType::Exclamation}),
        ";" => Ok(Token{lexeme:lx, token_type:TokenType::StatementEnd}),
        "." => Ok(Token{lexeme:lx, token_type:TokenType::Stop}),
        ":" => Ok(Token{lexeme:lx, token_type:TokenType::TypeDeclaration}),
        _ => Err(format!("Invalid token:{:?}", lx))
    }
}

pub fn parseStatements<'a>(lexemes: &mut Peekable<LexemeIterator<'a>>) -> Result<Vec<Token<'a>>, String> {
    let items: Result<Vec<Token<'a>>, String> = lexemes.map(getToken).collect();
    items
}

pub fn parseStatement(lexemes: &mut Peekable<LexemeIterator>) -> bool {
    for lx in lexemes {
        println!("{:?}", getToken(lx));
    }
    false
}

#[derive(Clone)]
pub struct TokenIterator<'a> {
    pub lexIter: LexemeIterator<'a>
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        let maybeLexeme = self.lexIter.next();
        println!("{:?}", maybeLexeme);

        match maybeLexeme {
            Some(lexeme) => {
                let x = getToken(lexeme);
                match x {
                    Ok(x) => Some(x),
                    Err(_) => None
                }
            },
            None    => None
        }


    }
}