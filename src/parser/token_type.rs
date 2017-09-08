use std::fmt;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenType {
    Identifier,
    IntegerValue(i32),
    StringLiteral(u64),
    //StringLiteral(&'a Lexeme),
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
    Write,
    Read,
    StringType,
    IntegerType,
    BooleanType,

    //symbols
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LessThan,
    Equal,
    Negation,
    And,
    Stop,
    StatementEnd,

    RangeDots,

    EOF,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::IntegerValue(integer) => write!(f, "{}", integer),
            TokenType::BooleanValue(boolean) => write!(f, "{}", boolean),
            _ => write!(f, "{:?}", self),
        }
    }
}
