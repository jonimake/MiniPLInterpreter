use std::fmt;


#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenType {
    Identifier,
    IntegerValue(i32),
    StringLiteral(u64),
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
    Exclamation,
    And,
    Stop,
    StatementEnd,

    RangeDots,

    EOF
}
