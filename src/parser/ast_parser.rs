#[allow(unused)]

use std::collections::HashMap;
use std::iter::Peekable;
use std::vec::Vec;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::convert::TryFrom;

use crate::lexer::lexeme::Lexeme;
use crate::parser::token::Token;
use crate::parser::token_type::TokenType;
use crate::ast::*;

type TokenIteratorType<'a> = &'a mut dyn Iterator<Item = Token>;

enum Err {
    ParseError(String),
    UnexpectedToken(String),
}

pub struct AstParser<'a> {
    //interpreter_state: &'b mut InterpreterState,
    iterator: Peekable<TokenIteratorType<'a>>,
    //errors: Vec<Err>,
}

impl<'a> AstParser<'a> {
    pub fn new(iter: TokenIteratorType<'a>) -> AstParser<'a> {
        AstParser {
            iterator: iter.peekable()
        }
    }

    pub fn build_ast(&mut self) -> Result<Ast, String> {
        let mut statements: Vec<Statement> = Vec::new();
        while self.iterator.peek().cloned().is_some() {
            let stmt = self.statement()?;
            statements.push(stmt);
        }
        Ok(Ast::Statements(statements))
    }

    /*
   stmt    -> var id : type [:= expr]              {var}
   stmt    -> id := expr                           {id}
   stmt    -> for id in expr .. do stmts end for   {for}
   stmt    -> read id                              {read}
   stmt    -> print expr                           {print}
   stmt    -> assert ( expr )                      {assert}
   */
    fn statement(&mut self) -> Result<Statement, String> {
        let next: Option<Token> = self.iterator.peek().cloned();
        let tt: Token = next.unwrap();
        let statement = match tt.token_type {
            TokenType::VarKeyword => self.var_decl(),
            TokenType::Identifier => self.id_assign(),
            TokenType::For => self.for_loop(),
            TokenType::Read => self.read(),
            TokenType::Print => self.print(),
            TokenType::Assert => self.assert(),
            t @ _ => Err(format!("Unexpected token in statement parsing {:?}", t)),
        };
        self.expect_next(TokenType::StatementEnd)?;
        statement

    }

    fn read(&mut self) -> Result<Statement, String> {

        let _ = self.expect_next(TokenType::Read)?;
        let id = self.expect_next(TokenType::Identifier)?;
        Ok(Statement::Read(Id(id.lexeme.unwrap().lexeme)))
    }

    fn assert(&mut self) -> Result<Statement, String> {
        self.expect_next(TokenType::Assert)?;
        let expr = self.expression(None)?;
        Ok(Statement::Assert(expr))
    }

    fn for_loop(&mut self) -> Result<Statement, String> {
        self.expect_next(TokenType::For)?;
        let loop_id = self.expect_next(TokenType::Identifier)?;
        let lex = loop_id.lexeme.unwrap();

        self.expect_next(TokenType::In)?;
        let range_start = self.expression(Some(TokenType::RangeDots))?;

        self.expect_next(TokenType::RangeDots)?;
        let range_end = self.expression(Some(TokenType::Do))?;

        self.expect_next(TokenType::Do)?;
        //read all tokens until end for
        let mut for_loop_tokens: Vec<Token> = Vec::new();

        loop {
            let next: Option<Token> = self.iterator.next();
            if let Some(tt) = next {
                if tt.token_type == TokenType::End {
                    self.expect_next(TokenType::For)?;
                    break;
                } else {
                    for_loop_tokens.push(tt);
                }
            } else {
                break;
            }
        }
        let mut t_iter = for_loop_tokens.into_iter();
        let mut loop_parser = AstParser::new(&mut t_iter);
        let ast = loop_parser.build_ast()?;

        let statement = Statement::ForLoop(Id(lex.lexeme.to_owned()),
                           range_start,
                           range_end,
                           Box::new(ast)); //todo
        Ok(statement)
    }

    fn set_variable_value(&mut self, name: Token, token: Token) {
        let name_lexeme = name.lexeme.unwrap();
        debug!("set {:?} => {}", name_lexeme.lexeme, token);
    }

    fn get_variable_value(&self, name: &Token) -> Option<Token> {
        let name_lexeme = name.clone().lexeme.unwrap();
        let name: String = name_lexeme.lexeme.to_string();
        None
    }

    fn id_assign(&mut self) -> Result<Statement, String> {
        let id = self.expect_next(TokenType::Identifier)?;
        self.expect_peek(TokenType::ValueDefinition);
        let assignment = self.assign(id.clone())?;
        let lex = id.lexeme.unwrap();
        let statement = Statement::IdAssign(Id(lex.lexeme), Expression::ScalarExpression(Scalar::Int(0))); //todo
        Ok(statement)
    }

    fn var_decl(&mut self) -> Result<Statement, String> {
        self.expect_next(TokenType::VarKeyword)?;
        let id: Token = self.expect_next(TokenType::Identifier)?;

        self.expect_next(TokenType::TypeDeclaration)?;
        let tokentype: Token = self.iterator.next().unwrap();

        let parsed_type = Type::try_from(tokentype)?;
        let lex = id.lexeme.unwrap();
        let statement = if self.expect_peek(TokenType::ValueDefinition) {
            self.expect_next(TokenType::ValueDefinition)?;
            let expr = self.expression(None)?;
            Statement::VarDefn(Id(lex.lexeme), parsed_type, expr)
        } else {
            Statement::VarDecl(Id(lex.lexeme), parsed_type)
        };

        Ok(statement)
    }

    fn assign(&mut self, id: Token) -> Result<Statement, String> {
        self.expect_next(TokenType::ValueDefinition)?;
        let expression = self.expression(None)?;
        let lex = id.lexeme.unwrap();
        let statement = Statement::VarDefn(Id(lex.lexeme), Type::Int, expression); //todo
        Ok(statement)
    }

    fn print(&mut self) -> Result<Statement, String> {
        let _ = self.expect_next(TokenType::Print)?;

        let expr = self.expression(None)?;
        let statement = Statement::Print(expr);
        Ok(statement)
    }
    fn expression(&mut self, terminator_symbol: Option<TokenType>) -> Result<Expression, String> {
        let mut all_tokens: Vec<Token> = Vec::new();
        loop {
            let token = self
                .iterator
                .peek()
                .cloned()
                .unwrap_or_else(|| Token::new(terminator_symbol.unwrap_or(TokenType::StatementEnd), Lexeme::default()));
            if token.token_type == terminator_symbol.unwrap_or(TokenType::StatementEnd) {
                break;
            } else {
                all_tokens.push(self.iterator.next().unwrap());
            }
        }

        let mut output: Vec<Token> = Vec::new();
        let mut opstack: Vec<Token> = Vec::new();

        let mut counter: i32 = 0;
        for token in all_tokens {
            let token_type = token.token_type;
            trace!("expression {:?}", token_type);
            match token_type {
                //handle operator
                TokenType::Addition
                | TokenType::Multiplication
                | TokenType::Division
                | TokenType::Subtraction
                | TokenType::Equal
                | TokenType::Negation => {
                    while let Some(y) = opstack.last().cloned() {
                        counter += 1;
                        if is_operator(&y) && precedence_equal_or_less_than(token_type, y.token_type) {
                            output.push(opstack.pop().unwrap());
                        } else {
                            break;
                        }
                        if counter > 10000 {
                            error!("{:?}", y);
                            return Err("loop detected".to_string());
                        }
                    }
                    opstack.push(token);
                }
                TokenType::LParen => opstack.push(token),

                TokenType::RParen => {
                    while let Some(token) = opstack.last().cloned() {
                        let tt: TokenType = token.token_type;
                        match tt {
                            TokenType::LParen => {
                                opstack.pop();
                            }
                            _ => output.push(opstack.pop().unwrap()),
                        }
                    }
                }
                //handle operand
                _ => output.push(token),
            }
        }
        while let Some(token) = opstack.pop() {
            output.push(token);
        }

        self.postfix_to_expr(&mut output)
    }

    fn postfix_to_expr(&mut self, tokens: &mut Vec<Token>) -> Result<Expression, String> {
        let mut stack: Vec<Expression> = Vec::new();
        tokens.reverse();
        let debug: Vec<TokenType> = tokens.iter().map(|t| {
            t.token_type
        }).collect();

        while let Some(next_token) = tokens.pop() {
            if is_operator(&next_token) {
                let operator_token_type = next_token.token_type;
                if operator_token_type == TokenType::Negation {
                    let t = tokens.pop().unwrap();
                    let op = Operator::try_from(next_token)?;
                    let result = Expression::Unary(op, Box::new(stack.pop().unwrap()));
                    stack.push(result);
                    unimplemented!()
                } else {
                    let t1 = stack.pop();
                    let t2 = stack.pop();
                    let t1 = t1.unwrap();
                    let t2 = t2.unwrap();
                    let operator: Operator = Operator::try_from(next_token)?;
                    let result = Expression::Binary(Box::new(t2), operator, Box::new(t1));
                    stack.push(result);
                }
            } else {
                let scalar = Scalar::try_from(next_token)?;
                stack.push(Expression::ScalarExpression(scalar));
            }
        }

        if stack.len() != 1 {
            error!("Stack: {:?}", stack);
            if stack.is_empty() {
                Err("Expected input while it was empty".to_string())
            } else {
                panic!("Stack length wasn't 1, error occurred");
            }
        } else {
            Ok(stack[0].clone())
        }
    }

    fn expect_next(&mut self, tt: TokenType) -> Result<Token, String> {
        let maybetoken = self.iterator.next();
        if let Some(token) = maybetoken {
            trace!("expect_next {:?}, actual {:?}", tt, token.clone());
            if token.token_type == tt {
                return Ok(token);
            } else {
                return Err(format!("Unexpected token {:?}, expected type {:?}", token, tt));
            }
        }
        Err("Expected token but iterator was empty".to_string())
    }

    fn expect_peek(&mut self, tt: TokenType) -> bool {
        let maybetoken = self.iterator.peek();
        if let Some(token) = maybetoken {
            return token.token_type == tt;
        }
        false
    }
}


fn is_operator(token: &Token) -> bool {
    let token_type = token.token_type;
    match token_type {
        TokenType::Addition
        | TokenType::Multiplication
        | TokenType::Division
        | TokenType::Equal
        | TokenType::Subtraction
        | TokenType::Negation => true,
        _ => false,
    }
}

fn binary_integer_op(a: i32, b: i32, op: TokenType) -> Result<Token, String> {
    let tt = match op {
        TokenType::Addition => TokenType::IntegerValue(a + b),
        TokenType::Subtraction => TokenType::IntegerValue(a - b),
        TokenType::Multiplication => TokenType::IntegerValue(a * b),
        TokenType::Equal => TokenType::BooleanValue(a == b),
        TokenType::Division => {
            if b == 0 {
                return Err(format!("Division by zero: {}/{}", a, b));
            }
            TokenType::IntegerValue(a / b)
        }
        _ => return Err(format!("binary_integer_op error: {:?}", op)),
    };
    trace!("{:?} {:?} {:?} = {:?}", a, op, b, tt);
    Ok(Token::new(tt, Lexeme::default()))
}

fn precedence_equal_or_less_than(x: TokenType, y: TokenType) -> bool {
    match x {
        TokenType::Multiplication | TokenType::Division => match y {
            TokenType::Addition | TokenType::Subtraction => false,
            _ => true,
        },
        _ => true,
    }
}

#[test]
fn parse_var_decl() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut ast_parser = AstParser::new(&mut iter);
    let ast = ast_parser.build_ast();

    let stmt = vec![Statement::VarDecl(Id("x".to_owned()), Type::Int)];
    assert_eq!(Ok(Ast::Statements(stmt)), ast);
}



#[test]
fn parse_var_definition() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut ast_parser = AstParser::new(&mut iter);
    let ast = ast_parser.build_ast();

    let stmt = vec![Statement::VarDefn(Id("x".to_owned()), Type::Int, Expression::ScalarExpression(Scalar::Int(1)))];
    assert_eq!(Ok(Ast::Statements(stmt)), ast);
}

#[test]
fn parse_var_definition_2() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::StringType, "string"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::StringLiteral(0), "\"foobar\""),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut ast_parser = AstParser::new(&mut iter);
    let ast = ast_parser.build_ast();
    let stmt = vec![
        Statement::VarDefn(Id("x".to_owned()), Type::Str, Expression::ScalarExpression(Scalar::Str("\"foobar\"".to_owned())))
    ];
    assert_eq!(Ok(Ast::Statements(stmt)), ast);
}

#[test]
fn parse_expression() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::IntegerValue(2), "2"),
        Token::new_string(TokenType::Subtraction, "-"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
    ];

    let mut iter = tokens.into_iter();
    let mut int = AstParser::new(&mut iter);
    let result = int.expression(None);
    assert_eq!(result, Ok(Expression::Binary(
        Box::new(Expression::ScalarExpression(Scalar::Int(2))),
        Operator::Sub,
        Box::new(Expression::ScalarExpression(Scalar::Int(1)))
    )))
}


#[test]
fn evaluate_postfix_simple_addition() {
    let mut tokens: Vec<Token> = vec![
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::IntegerValue(2), "2"),
        Token::new_string(TokenType::Addition, "+"),
    ];

    let mut iter = vec![].into_iter();
    let mut ast_builder = AstParser::new(&mut iter);

    let result = ast_builder.postfix_to_expr(&mut tokens);
    assert_eq!(result, Ok(Expression::Binary(
        Box::new(Expression::ScalarExpression(Scalar::Int(1))),
        Operator::Add,
        Box::new(Expression::ScalarExpression(Scalar::Int(2)))
    )))
}

#[test]
fn for_loop_test() {
    //for x in 0..3 do
    //  print x;
    //end for;

    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::For, "for"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::In, "in"),
        Token::new_string(TokenType::IntegerValue(0), "0"),
        Token::new_string(TokenType::RangeDots, ".."),
        Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::Do, "do"),
        Token::new_string(TokenType::Print, "print"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::StatementEnd, ";"),
        Token::new_string(TokenType::End, "end"),
        Token::new_string(TokenType::For, "for"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];
    let mut iter = tokens.into_iter();
    let mut ast_builder = AstParser::new(&mut iter);
    let result: Result<Statement, String> = ast_builder.for_loop();
    let inner_ast: Box<Ast> = Box::new(Ast::Statements(vec![
        Statement::Print(Expression::ScalarExpression(Scalar::Var(Id("x".to_owned()))))
    ]));
    assert_eq!(result,
               Ok(Statement::ForLoop(
                Id("x".to_owned()),
                Expression::ScalarExpression(Scalar::Int(0)),
                Expression::ScalarExpression(Scalar::Int(3)), inner_ast)));

    println!("{:?}", result.unwrap());
}

#[test]
fn print_string_integer() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::Print, "print"),
        Token::new_string(TokenType::IntegerValue(23), "23"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut ast_parser = AstParser::new(&mut iter);
    let ast = ast_parser.build_ast();
    assert_eq!(ast,
        Ok(Ast::Statements(
            vec![Statement::Print(Expression::ScalarExpression(Scalar::Int(23)))]
        )));

}


#[test]
fn for_loop_assert_test() {
    //var nTimes : int := 3;
    //for x in 0..nTimes-1 do
    //  print x;
    //end for;
    //assert (x = (nTimes - 1));

    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "nTimes"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::StatementEnd, ";"),
        Token::new_string(TokenType::For, "for"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::In, "in"),
        Token::new_string(TokenType::IntegerValue(0), "0"),
        Token::new_string(TokenType::RangeDots, ".."),
        //Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::Identifier, "nTimes"),
        Token::new_string(TokenType::Subtraction, "-"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Do, "do"),
        Token::new_string(TokenType::Print, "print"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::StatementEnd, ";"),
        Token::new_string(TokenType::End, "end"),
        Token::new_string(TokenType::For, "for"),
        Token::new_string(TokenType::StatementEnd, ";"),
        Token::new_string(TokenType::Assert, "assert"),
        Token::new_string(TokenType::LParen, "("),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::Equal, "="),
        Token::new_string(TokenType::LParen, "("),
        Token::new_string(TokenType::Identifier, "nTimes"),
        Token::new_string(TokenType::Subtraction, "-"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::RParen, ")"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();

    let mut ast_parser = AstParser::new(&mut iter);
    let result = ast_parser.build_ast();

    let loop_ast = Box::new(Ast::Statements(vec![Statement::Print(Expression::ScalarExpression(Scalar::Var(Id("x".to_owned()))))]));

    assert_eq!(result, Ok(
        Ast::Statements(vec![
            Statement::VarDefn(Id("nTimes".to_owned()), Type::Int, Expression::ScalarExpression(Scalar::Int(3))),
            Statement::ForLoop(Id("x".to_owned()),
                               Expression::ScalarExpression(Scalar::Int(0)),
                               Expression::Binary(
                                   Box::new(Expression::ScalarExpression(Scalar::Var(Id("nTimes".to_owned())))),
                                   Operator::Sub, Box::new(Expression::ScalarExpression(Scalar::Int(1)))),
                               loop_ast),
            Statement::Assert(
                Expression::Binary(
                    Box::new(Expression::ScalarExpression(Scalar::Var(Id("x".to_owned())))),
                    Operator::Eq,
                    Box::new(Expression::Binary(
                        Box::new(Expression::ScalarExpression(Scalar::Var(Id("nTimes".to_owned())))),
                        Operator::Sub,
                        Box::new(Expression::ScalarExpression(Scalar::Int(1))),
                    ))
                )
            )
        ])
    ))
}
/*
#[test]
fn evaluate_postfix_complex() {
    //5 1 2 + 4 * + 3 −
    let mut tokens: Vec<Token> = vec![
        Token::new_string(TokenType::IntegerValue(5), "5"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::IntegerValue(2), "2"),
        Token::new_string(TokenType::Addition, "+"),
        Token::new_string(TokenType::IntegerValue(4), "4"),
        Token::new_string(TokenType::Multiplication, "*"),
        Token::new_string(TokenType::Addition, "+"),
        Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::Subtraction, "-"),
    ];
    let mut iter = vec![].into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let res = int.evaluate_postfix(&mut tokens);
    assert_eq!(res.unwrap().token_type, TokenType::IntegerValue(14));
}

#[test]
fn parse_print_string_integer() {
    let _lexeme = Lexeme::default();
    let _id = Token::new_string(TokenType::Identifier, "x");
    let _val = Token::new_string(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::Print, "print"),
        Token::new_string(TokenType::IntegerValue(23), "23"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();
    assert!(int.iterator.count() == 0);
}

#[test]
fn parse_print_string_literal() {
    let _lexeme = Lexeme::default();
    let _id = Token::new_string(TokenType::Identifier, "x");
    let _val = Token::new_string(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::Print, "print"),
        Token::new_string(TokenType::StringLiteral(0), "foobar"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();

    assert!(int.iterator.count() == 0);
}

#[test]
fn parse_print_complicated() {
    let _lexeme = Lexeme::default();
    let id = Token::new_string(TokenType::Identifier, "x");
    let val = Token::new_string(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        val,
        Token::new_string(TokenType::StatementEnd, ";"),
        Token::new_string(TokenType::Print, "print"),
        id.clone(),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();

    assert!(int.get_variable_value(&id).is_some());
    assert!(int.iterator.count() == 0);
}

#[test]
fn parse_expression_1() {
    let _lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Addition, "+"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.expression(None);
    assert_eq!(result.unwrap(), Token::new(TokenType::IntegerValue(2), Lexeme::default()));
}

#[test]
fn parse_assert() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::Assert, "assert"),
        Token::new_string(TokenType::LParen, "("),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Equal, "="),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::RParen, ")"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let _result = int.assert().unwrap();
}

#[test]
fn parse_assert_identifier() {
    //var x : int := 3;
    //var y : int := 3;
    //assert(x = y);
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::StatementEnd, ";"),
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "y"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::StatementEnd, ";"),
        Token::new_string(TokenType::Assert, "assert"),
        Token::new_string(TokenType::LParen, "("),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::Equal, "="),
        Token::new_string(TokenType::Identifier, "y"),
        Token::new_string(TokenType::RParen, ")"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret().unwrap()
    //let result = int.interpret().unwrap();
}

#[test]
fn evaluate_postfix_equality() {
    let mut tokens: Vec<Token> = vec![
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Equal, "="),
    ];

    let mut iter = vec![].into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let _result = int.evaluate_postfix(&mut tokens).unwrap();
}

#[test]
fn parse_expression_2() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Subtraction, "-"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.expression(None);
    assert_eq!(result.unwrap(), Token::new(TokenType::IntegerValue(0), Lexeme::default()));
}

#[test]
fn parse_expression_5() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::Subtraction, "-"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.expression(None);
    assert_eq!(result.unwrap(), Token::new(TokenType::IntegerValue(2), Lexeme::default()));
}

#[test]
fn parse_var_definition_expression_1() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Addition, "+"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();
    assert!(int.interpreter_state.variables.contains_key("x"));
    assert_eq!(
        int.interpreter_state.variables.get("x"),
        Some(&Token::new_string(TokenType::IntegerValue(2), ""))
    );
}

#[test]
fn is_operator_test() {
    assert!(is_operator(&Token::new_string(TokenType::Addition, "+")));
    assert!(is_operator(&Token::new_string(TokenType::Addition, "*")));
    assert!(is_operator(&Token::new_string(TokenType::Addition, "-")));
    assert!(is_operator(&Token::new_string(TokenType::Addition, "/")));
    assert!(is_operator(&Token::new_string(TokenType::Negation, "!")));
}

#[test]
fn parse_var_definition_expression_2() {
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Addition, "+"),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Addition, "+"),
        Token::new_string(TokenType::IntegerValue(7), "7"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();
    assert!(int.interpreter_state.variables.contains_key("x"));
    assert_eq!(
        int.interpreter_state.variables.get("x"),
        Some(&Token::new_string(TokenType::IntegerValue(9), ""))
    );
}

#[test]
fn parse_var_definition_expression_3() {
    // var x: int := (1+3)*4
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::LParen, "("),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Addition, "+"),
        Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::RParen, ")"),
        Token::new_string(TokenType::Multiplication, "*"),
        Token::new_string(TokenType::IntegerValue(4), "4"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    assert!(int.interpret().is_ok());
    assert!(int.interpreter_state.variables.contains_key("x"));
    let expected_value = Token::new_string(TokenType::IntegerValue(16), "");
    assert_eq!(int.interpreter_state.variables.get("x"), Some(&expected_value));
}

#[test]
fn parse_expression_3() {
    let _lexeme = Lexeme::default();
    // (1+3)*4
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::LParen, "("),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Addition, "+"),
        Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::RParen, ")"),
        Token::new_string(TokenType::Multiplication, "*"),
        Token::new_string(TokenType::IntegerValue(4), "4"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.expression(None);
    println!("result = {:?}", result);
    let expected_value = Token::new_string(TokenType::IntegerValue(16), "");
    assert_eq!(result.unwrap(), expected_value);
}

#[test]
fn parse_expression_4() {
    let _lexeme = Lexeme::default();
    // (1+3)*4
    let tokens: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        Token::new_string(TokenType::IntegerValue(1), "1"),
        Token::new_string(TokenType::Addition, "+"),
        Token::new_string(TokenType::IntegerValue(3), "3"),
        Token::new_string(TokenType::Multiplication, "*"),
        Token::new_string(TokenType::IntegerValue(2), "2"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    {
        let mut int = Interpreter::new(&mut iter, &mut state);
        int.interpret();
    }

    assert_eq!(state.variables.get("x"), Some(&Token::new_string(TokenType::IntegerValue(7), "")));
}

#[test]
fn test_precedence() {
    assert!(precedence_equal_or_less_than(TokenType::Addition, TokenType::Addition));
    assert!(precedence_equal_or_less_than(TokenType::Addition, TokenType::Multiplication));
    assert!(precedence_equal_or_less_than(TokenType::Addition, TokenType::Division));
    assert!(!precedence_equal_or_less_than(TokenType::Multiplication, TokenType::Addition));
    assert!(!precedence_equal_or_less_than(TokenType::Division, TokenType::Subtraction));
}

#[test]
fn stateful_var_definition() {
    //let _ = simplelog::TermLogger::init(simplelog::LogLevelFilter::Trace, simplelog::Config::default());
    let val = Token::new_string(TokenType::IntegerValue(1), "1");

    let tokens1: Vec<Token> = vec![
        Token::new_string(TokenType::VarKeyword, "var"),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::TypeDeclaration, ":"),
        Token::new_string(TokenType::IntegerType, "int"),
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let tokens2: Vec<Token> = vec![
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::ValueDefinition, ":="),
        val,
        Token::new_string(TokenType::StatementEnd, ";"),
    ];

    let mut state = InterpreterState::new();
    {
        let mut iter = tokens1.into_iter();
        let mut int = Interpreter::new(&mut iter, &mut state);
        let _ = int.interpret();
    }
    {
        let mut iter = tokens2.into_iter();
        let mut int2 = Interpreter::new(&mut iter, &mut state);
        let _ = int2.interpret();
    }

    assert!(state.variables.contains_key("x"));
    assert_eq!(state.variables.get("x"), Some(&Token::new_string(TokenType::IntegerValue(1), "1")));
}
*/