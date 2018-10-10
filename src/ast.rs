/*
stmt    -> var id : type [:= expr]              {var}
stmt    -> id := expr                           {id}
stmt    -> for id in expr .. do stmts end for   {for}
stmt    -> read id                              {read}
stmt    -> print expr                           {print}
stmt    -> assert ( expr )                      {assert}
*/

use crate::visualizer::Tree;
use crate::visualizer::Node;
use crate::visualizer::NodePtr;
use crate::parser::token::Token;
use crate::parser::token_type::TokenType;
use ::std::collections::hash_map::DefaultHasher;
use ::std::hash::Hash;
use ::std::hash::Hasher;
use ::std::convert::TryFrom;

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum Ast {
    Statements(Vec<Statement>)
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum Statement {
    VarDecl(Id, Type),
    VarDefn(Id, Type, Expression),
    IdAssign(Id, Expression),
    ForLoop(Id, Expression, Expression, Box<Ast>),
    Read(Id),
    Print(Expression),
    Assert(Expression),
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum Expression {
    ScalarExpression(Scalar),
    Unary(Operator, Box<Expression>),
    Binary(Box<Expression>, Operator, Box<Expression>),
}

#[derive(PartialEq, Debug, Clone, Copy, Hash)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    LT,
    Neg,
    And
}

#[derive(PartialEq, Debug, Clone, Copy, Hash)]
pub enum Type {
    Str,
    Int,
    Bool
}


impl TryFrom<Token> for Type {
    type Error = String;

    fn try_from(token: Token) -> Result<Self, <Self as TryFrom<Token>>::Error> {
        match token.token_type {
            TokenType::StringType => Ok(Type::Str),
            TokenType::IntegerType => Ok(Type::Int),
            TokenType::BooleanType => Ok(Type::Bool),
            t @ _ => return Err(format!("Unexpected type token {:?}", t)),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub enum Scalar {
    Var(Id),
    Str(String),
    Int(i32),
    Bool(bool)
}

#[derive(PartialEq, Debug, Clone, Hash)]
pub struct Id(pub String);

#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct StringId(pub u64);

impl TryFrom<Token> for Operator {
    type Error = String;

    fn try_from(token: Token) -> Result<Self, <Self as TryFrom<Token>>::Error> {
        match token.token_type {
            TokenType::Addition => Ok(Operator::Add),
            TokenType::Subtraction => Ok(Operator::Sub),
            TokenType::Multiplication => Ok(Operator::Mul),
            TokenType::Division => Ok(Operator::Div),
            TokenType::LessThan => Ok(Operator::LT),
            TokenType::Equal => Ok(Operator::Eq),
            TokenType::Negation => Ok(Operator::Neg),
            TokenType::And => Ok(Operator::And),
            t @ _ => return Err(format!("Unexpected operator token {:?}", t)),
        }
    }
}

impl TryFrom<Token> for Scalar {
    type Error = String;

    fn try_from(token: Token) -> Result<Self, <Self as TryFrom<Token>>::Error> {
        match token.token_type {
            TokenType::Identifier => Ok(Scalar::Var(Id(token.lexeme.unwrap_or_default().lexeme))),
            TokenType::IntegerValue(i) => Ok(Scalar::Int(i)),
            TokenType::StringLiteral(_) => Ok(Scalar::Str(token.lexeme.unwrap().lexeme)),
            TokenType::BooleanValue(b) => Ok(Scalar::Bool(b)),
            t @ _ => return Err(format!("Unexpected operand token {:?}", t))
        }
    }
}

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}