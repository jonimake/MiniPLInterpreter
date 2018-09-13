use crate::lexer::lexeme::FromLexeme;
use crate::lexer::lexeme::Lexeme;
use crate::parser::token::Token;

#[derive(Clone)]
pub struct TokenIterator<I>
where
    I: Iterator<Item = Lexeme>,
{
    lex_iter: I,
}

impl<'a, I> TokenIterator<I>
where
    I: Iterator<Item = Lexeme>,
{
    pub fn new(it: I) -> TokenIterator<I> {
        TokenIterator { lex_iter: it }
    }
}

impl<'a, I> Iterator for TokenIterator<I>
where
    I: Iterator<Item = Lexeme>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let maybe_lexeme = self.lex_iter.next();
        match maybe_lexeme {
            Some(lexeme) => {
                let x = Token::from_lexeme(lexeme);
                match x {
                    Ok(t) => Some(t),
                    Err(_) => {
                        error!("{:?}", x);
                        None
                    }
                }
            }
            None => None,
        }
    }
}
