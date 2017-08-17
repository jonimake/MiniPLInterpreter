use simplelog;

use std::collections::HashMap;
use std::iter::Peekable;
use std::vec::Vec;
use std::result::Result;

use parser::token::Token;
use parser::token_type::TokenType;
use lexer::lexeme::Lexeme;

type TokenIteratorType<'a> = &'a mut Iterator<Item = Token>;

#[derive(Debug, Clone)]
pub struct InterpreterState {
    pub variables: HashMap<String, Token>,
    pub string_cache: HashMap<u64, String>,
}

impl InterpreterState {
    pub fn new() -> InterpreterState {
        InterpreterState {
            variables: HashMap::new(),
            string_cache: HashMap::new()
        }
    }
}

pub struct Interpreter<'a, 'b:'a> {
    interpreter_state: &'b mut InterpreterState,
    iterator: Peekable<TokenIteratorType<'a>>,

}

impl<'a, 'b: 'a> Interpreter<'a, 'b> {
    pub fn new(iter: TokenIteratorType<'a>,
               state: &'b mut InterpreterState)
               -> Interpreter<'a, 'b> {
        Interpreter {
            interpreter_state: state,
            iterator: iter.peekable()
        }
    }


    pub fn interpret(&mut self) -> Result<(), String> {
        while self.iterator.peek().cloned().is_some() {
            self.statement()?
        }
        Ok(())
    }

    /*
   stmt    -> var id : type [:= expr]              {var}
   stmt    -> id := expr                           {id}
   stmt    -> for id in expr .. do stmts end for   {for}
   stmt    -> read id                              {read}
   stmt    -> print expr                           {print}
   stmt    -> assert ( expr )                      {assert}
   */
    fn statement(&mut self) -> Result<(), String> {
        let next = self.iterator.peek().cloned();
        if let Some(tt) = next {
            trace!("statement token {:?}", tt.clone());
            match tt.token_type {
                TokenType::VarKeyword => self.var_decl()?,
                TokenType::Identifier => self.id_assign()?,
                TokenType::For => self.for_loop()?,
                TokenType::Read => self.read()?,
                TokenType::Print => self.print()?,
                TokenType::Assert => self.assert()?,
                t @ _ => return Err(format!("Unexpected token in statement parsing {:?}", t)),
            };

            self.expect_next(TokenType::StatementEnd)?;

        }
        Ok(())
    }

    fn read(&mut self) -> Result<(), String> {

        let tokens: (Token, Token) = {
            let _ = self.expect_next(TokenType::Read)?;
            let id = self.expect_next(TokenType::Identifier)?;
            let maybetoken = self.get_variable_value(&id);
            let variable = match maybetoken {
                Some(t) => t,
                None => return Err(format!("Can't read variable with an invalid variable ID")),
            };

            use std::io;

            let mut input_text = String::new();
            io::stdin()
                .read_line(&mut input_text)
                .expect("failed to read from stdin");

            let trimmed = input_text.trim();

            let t: Token = match variable {
                Token { token_type: TokenType::IntegerValue(_), .. }
                | Token { token_type: TokenType::IntegerType, .. }
                => {
                    let parsed_value = trimmed.parse::<i32>().unwrap_or(0);
                    let tt = TokenType::IntegerValue(parsed_value);
                    Token::new_string(tt, trimmed)
                },
                Token { token_type: TokenType::BooleanType, .. }
                | Token { token_type: TokenType::BooleanValue(_), .. }
                => {
                    let parsed_value = trimmed.parse::<bool>().unwrap_or(false);
                    let tt = TokenType::BooleanValue(parsed_value);
                    Token::new_string(tt, trimmed)
                },
                Token { token_type: TokenType::StringType, .. }
                | Token { token_type: TokenType::StringLiteral(_), .. }
                => {
                    let tt = TokenType::StringLiteral(0);
                    Token::new_string(tt, trimmed)
                }
                _ => return Err(format!("Variable not defined for read"))
            };
            (id, t)
        };
        self.set_variable_value(tokens.0, tokens.1);
        Ok(())
    }

    fn assert(&mut self) -> Result<(), String> {
        self.expect_next(TokenType::Assert)?;
        let assert_value = self.expression(None)?;
        match assert_value {
            Token { token_type: TokenType::BooleanValue(true), .. } => Ok(()),
            Token { token_type: TokenType::BooleanValue(false), .. } => Err(format!("assertion error")),
            _ => Err("error evaluating assertion value".to_string()),
        }
    }

    fn for_loop(&mut self) -> Result<(), String> {
        self.expect_next(TokenType::For)?;
        let loop_id = self.expect_next(TokenType::Identifier)?;
        self.expect_next(TokenType::In)?;
        let range_start: Token = self.expression(Some(TokenType::RangeDots))?;
        self.set_variable_value(loop_id.clone(), range_start.clone());
        self.expect_next(TokenType::RangeDots)?;
        let range_end: Token = self.expression(Some(TokenType::Do))?;

        self.expect_next(TokenType::Do)?;
        //read all tokens until end for
        let mut all_tokens: Vec<Token> = Vec::new();

        loop {
            let next = self.iterator.next();
            if let Some(tt) = next {
                if tt.token_type == TokenType::End {
                    self.expect_next(TokenType::For)?;
                    break;
                } else {
                    all_tokens.push(tt);
                }
            } else {
                break;
            }
        }

        match (range_start, range_end) {
            (Token { token_type: TokenType::IntegerValue(start_int), .. }, Token { token_type: TokenType::IntegerValue(end_int), .. }) => {
                for x in start_int..end_int+1 {
                    let mut token_iter = all_tokens.clone().into_iter();
                    //not sure if doing something incredibly dirty
                    self.set_variable_value(loop_id.clone(),
                                            Token::new(TokenType::IntegerValue(x), Lexeme::default()));

                    debug!("state before interpret: {:?}", self.interpreter_state);
                    {
                        let mut int = Interpreter::new(&mut token_iter, &mut self.interpreter_state);
                        int.interpret()?;
                    }
                    debug!("state after interpret: {:?}", self.interpreter_state);
                }
            }
            _ => return Err(format!("Ranges not implemented for other than Integer starts and ends")),
        }
        Ok(())
    }

    fn set_variable_value(&mut self, name: Token, token: Token) {
        println!("set {:?} => {:?}", name.lexeme.lexeme, token);
        self.interpreter_state.variables.insert(name.lexeme.lexeme.to_string(), token);
    }

    fn get_variable_value(&self, name: &Token) -> Option<Token> {

        let name: String = name.lexeme.lexeme.to_string();
        match self.interpreter_state.variables.get(&name) {
            Some(t) => {
                trace!("get {:?} = {:?}", name, t.clone());
                Some(t.clone())
            },
            None => {
                trace!("get {:?} = None", name);
                None
            }
        }
    }

    fn id_assign(&mut self) -> Result<(), String> {
        let id = self.expect_next(TokenType::Identifier)?;
        self.expect_peek(TokenType::ValueDefinition);
        self.assign(id)?;
        Ok(())
    }

    fn var_decl(&mut self) -> Result<(), String> {
        self.expect_next(TokenType::VarKeyword)?;
        let id: Token = self.expect_next(TokenType::Identifier)?;
        self.expect_next(TokenType::TypeDeclaration)?;
        let tokentype = self.iterator.next().unwrap();
        self.set_variable_value(id.clone(), tokentype);
        if self.expect_peek(TokenType::ValueDefinition) {
            return self.assign(id);
        }
        Ok(())
    }

    fn assign(&mut self, id: Token) -> Result<(), String> {
        self.expect_next(TokenType::ValueDefinition)?;
        let expression_value: Token = self.expression(None)?;
        self.set_variable_value(id, expression_value);
        Ok(())
    }

    fn print(&mut self) -> Result<(), String> {
        let _ = self.expect_next(TokenType::Print)?;

        let token = self.expression(None)?;
        let tt: TokenType = token.token_type;
        match tt {
            TokenType::Identifier => {
                match self.get_variable_value(&token) {
                    Some(var) => println!("{} => {:?}", token.lexeme.lexeme, var),
                    None => println!("{} => None", token.lexeme.lexeme),
                };
            }
            TokenType::StringLiteral(hash) => {
                println!("{:?} => '{}'", token, self.interpreter_state.string_cache.get(&hash).unwrap_or(&"''".to_string()));
            }
            TokenType::IntegerValue(x) => {
                println!("{}", x);
            }
            t @ _ => println!("{:?}", t),
        }
        Ok(())
    }
    fn expression(&mut self, terminator_symbol: Option<TokenType>) -> Result<Token, String> {

        let mut all_tokens: Vec<Token> = Vec::new();
        loop {
            let token = self.iterator
                .peek()
                .cloned()
                .unwrap_or(Token::new(terminator_symbol.unwrap_or(TokenType::StatementEnd),
                                      Lexeme::default()));
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
                | TokenType::Negation
                => {

                    while let Some(y) = opstack.last().cloned() {
                        counter = counter+1;
                        if is_operator(&y) && precedence_equal_or_less_than(token_type, y.token_type) {
                            output.push(opstack.pop().unwrap());
                        }
                        else {
                            break;
                        }
                        if counter > 10000 {
                            error!("{:?}", y);
                            return Err(format!("loop detected"));
                        }
                    }
                    opstack.push(token);
                },
                TokenType::LParen => {opstack.push(token)},

                TokenType::RParen => {
                    while let Some(token) = opstack.last().cloned() {
                        let tt: TokenType = token.token_type;
                        match tt {
                            TokenType::LParen => {opstack.pop();},
                            _ => output.push(opstack.pop().unwrap())
                        }
                    }

                },
                //handle operand
                _ => {output.push(token)}
            }
        }
        while let Some(token) = opstack.pop() {
            output.push(token);
        }

        return self.evaluate_postfix(&mut output);
    }

    fn expect_next(&mut self, tt: TokenType) -> Result<Token, String> {
        let maybetoken = self.iterator.next();
        if let Some(token) = maybetoken {
            trace!("expect_next {:?}, actual {:?}", tt.clone(), token.clone());
            if token.token_type == tt {
                return Ok(token);
            } else {
                return Err(format!("\nUnexpected token {:?},\nexpected type {:?}\n", token, tt));
            }
        }
        Err(format!("Expected token but iterator was empty"))
    }

    fn expect_peek(&mut self, tt: TokenType) -> bool {
        let maybetoken = self.iterator.peek();
        if let Some(token) = maybetoken {
            return token.token_type == tt;
        }
        false
    }


    fn evaluate_postfix(&mut self, mut tokens: &mut Vec<Token>) -> Result<Token, String> {
        let mut stack: Vec<Token> = Vec::new();

        trace!("eval tokens {:?}", tokens.clone());
        fn binary_op(t1: &Token, t2: &Token, operator_token_type: &TokenType) -> Result<Token, String> {
            let type1 = t1.token_type;
            let type2 = t2.token_type;
            debug!("types = {:?}, {:?}", type1, type2);
            match (type1, type2) {

                (TokenType::IntegerValue(a), TokenType::IntegerValue(b)) => binary_integer_op(a, b, *operator_token_type),
                t @ _ => return Err(format!("Binary operation {:?} not defined for types {:?}", operator_token_type, t)),
            }
        }

        tokens.reverse();
        while let Some(next_token) = tokens.pop() {
            if is_operator(&next_token) {
                let operator_token_type = next_token.token_type;
                if operator_token_type == TokenType::Negation {
                    let _ = tokens.pop();
                    unimplemented!();
                } else {
                    let t1: Token = stack.pop().unwrap();
                    let t2: Token = stack.pop().unwrap();

                    let result: Token = match (t1.token_type, t2.token_type) {
                        (TokenType::IntegerValue(a), TokenType::IntegerValue(b)) => binary_integer_op(b, a, operator_token_type)?,
                        (TokenType::Identifier, TokenType::Identifier) => {
                            let a = self.get_variable_value(&t1).unwrap();
                            let b = self.get_variable_value(&t2).unwrap();
                            binary_op(&a, &b, &operator_token_type)?
                        }
                        (TokenType::Identifier, _) => {
                            binary_op(&t2,
                            &self.get_variable_value(&t1).unwrap(),
                                      &operator_token_type)?
                        }
                        (_, TokenType::Identifier) => {
                            binary_op(&self.get_variable_value(&t2).unwrap(),
                                      &t1,
                                      &operator_token_type)?
                        }
                        (TokenType::StringLiteral(_), TokenType::StringLiteral(_)) => {
                            Token::new(TokenType::StringLiteral(0), Lexeme::default())
                        }
                        _ => return Err(format!("Unimplemented for tokens {:?} and {:?}", t1, t2)),
                    };
                    stack.push(result);
                }
            } else {
                stack.push(next_token);
            }
        }
        if stack.len() != 1 {
            panic!("Stack length wasn't 1, error occurred");
        } else {
            let result = Ok(stack[0].clone());
            return result;
        }
    }
}

fn is_operator(token: &Token) -> bool {
    let token_type = token.token_type;
    match token_type {
        TokenType::Addition |
        TokenType::Multiplication |
        TokenType::Division |
        TokenType::Equal |
        TokenType::Subtraction |
        TokenType::Negation => true,
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
        TokenType::Multiplication |
        TokenType::Division => {
            match y {
                TokenType::Addition | TokenType::Subtraction => false,
                _ => true,
            }
        }
        _ => true,
    }
}

#[test]
fn evaluate_postfix_simple_addition() {
    let mut tokens: Vec<Token> = vec![Token::new_string(TokenType::IntegerValue(1), "1"),
                                      Token::new_string(TokenType::IntegerValue(2), "2"),
                                      Token::new_string(TokenType::Addition, "+")];

    let mut iter = vec![].into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);

    let res = int.evaluate_postfix(&mut tokens);
    assert_eq!(res.unwrap().token_type, TokenType::IntegerValue(3));
}

#[test]
fn for_loop_test() {
    //for x in 0..3 do
    //  print x;
    //end for;

    let tokens: Vec<Token> = vec![Token::new_string(TokenType::For, "for"),
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
                                      Token::new_string(TokenType::StatementEnd, ";")];
    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let foo: Result<(), String> = int.for_loop();
    assert_eq!(Ok(()), foo);
    assert_eq!(int.interpreter_state.variables.get("x"),
               Some(&Token{token_type: TokenType::IntegerValue(3), lexeme: Lexeme::default()}));
}

#[test]
fn for_loop_assert_test() {
    //var nTimes : int := 3;
    //for x in 0..nTimes-1 do
    //  print x;
    //end for;
    //assert (x = nTimes);

    let _ = simplelog::TermLogger::init(simplelog::LogLevelFilter::Trace,
                                        simplelog::Config::default());

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
        Token::new_string(TokenType::StatementEnd, ";")];
/*
        Token::new_string(TokenType::Assert, "assert"),
        Token::new_string(TokenType::LParen, "("),
        Token::new_string(TokenType::Identifier, "x"),
        Token::new_string(TokenType::Equal, "="),
        Token::new_string(TokenType::Identifier, "nTimes"),
        Token::new_string(TokenType::RParen, ")"),
        Token::new_string(TokenType::StatementEnd, ";")];
*/
    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let foo: Result<(), String> = int.interpret();
    println!("{:?}", int.interpreter_state.variables);
    assert_eq!(Ok(()), foo);
    assert_eq!(int.interpreter_state.variables.get("x"),
               Some(&Token{token_type: TokenType::IntegerValue(2), lexeme: Lexeme::default()}));
}


#[test]
fn evaluate_postfix_complex() {
    //5 1 2 + 4 * + 3 −
    let mut tokens: Vec<Token> = vec![Token::new_string(TokenType::IntegerValue(5), "5"),
                                      Token::new_string(TokenType::IntegerValue(1), "1"),
                                      Token::new_string(TokenType::IntegerValue(2), "2"),
                                      Token::new_string(TokenType::Addition, "+"),
                                      Token::new_string(TokenType::IntegerValue(4), "4"),
                                      Token::new_string(TokenType::Multiplication, "*"),
                                      Token::new_string(TokenType::Addition, "+"),
                                      Token::new_string(TokenType::IntegerValue(3), "3"),
                                      Token::new_string(TokenType::Subtraction, "-")];
    let mut iter = vec![].into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let res = int.evaluate_postfix(&mut tokens);
    assert_eq!(res.unwrap().token_type, TokenType::IntegerValue(14));
}

#[test]
fn parse_var_definition() {
    let val = Token::new_string(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::VarKeyword, "var"),
                                  Token::new_string(TokenType::Identifier, "x"),
                                  Token::new_string(TokenType::TypeDeclaration, ":"),
                                  Token::new_string(TokenType::IntegerType, "int"),
                                  Token::new_string(TokenType::ValueDefinition, ":="),
                                  val.clone(),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();
    assert!(int.interpreter_state.variables.contains_key("x"));
    assert_eq!(int.interpreter_state.variables.get("x"), Some(&val));
}

#[test]
fn parse_var_definition_2() {
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::VarKeyword, "var"),
                                  Token::new_string(TokenType::Identifier, "x"),
                                  Token::new_string(TokenType::TypeDeclaration, ":"),
                                  Token::new_string(TokenType::IntegerType, "string"),
                                  Token::new_string(TokenType::ValueDefinition, ":="),
                                  Token::new_string(TokenType::StringLiteral(0), "\"foobar\""),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();
}

#[test]
fn parse_var_declaration() {
    let xtoken = Token::new_string(TokenType::Identifier, "x");
    let tokentype = Token::new_string(TokenType::IntegerType, "int");
    let lexeme = Lexeme::default();
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::VarKeyword, "var"),
                                  Token::new_string(TokenType::Identifier, "x"),
                                  Token::new_string(TokenType::TypeDeclaration, ":"),
                                  tokentype.clone(),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();
    assert!(int.interpreter_state.variables.contains_key("x"));
    assert_eq!(int.interpreter_state.variables.get("x"), Some(&tokentype));
}

#[test]
fn parse_print_string_integer() {
    let lexeme = Lexeme::default();
    let id = Token::new_string(TokenType::Identifier, "x");
    let val = Token::new_string(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::Print, "print"),
                                  Token::new_string(TokenType::IntegerValue(23), "23"),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();
    assert!(int.iterator.count() == 0);
}

#[test]
fn parse_print_string_literal() {
    let lexeme = Lexeme::default();
    let id = Token::new_string(TokenType::Identifier, "x");
    let val = Token::new_string(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::Print, "print"),
                                  Token::new_string(TokenType::StringLiteral(0), "foobar"),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();

    assert!(int.iterator.count() == 0);
}

#[test]
fn parse_print_complicated() {
    let lexeme = Lexeme::default();
    let id = Token::new_string(TokenType::Identifier, "x");
    let val = Token::new_string(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::VarKeyword, "var"),
                                  Token::new_string(TokenType::Identifier, "x"),
                                  Token::new_string(TokenType::TypeDeclaration, ":"),
                                  Token::new_string(TokenType::IntegerType, "int"),
                                  Token::new_string(TokenType::ValueDefinition, ":="),
                                  val,
                                  Token::new_string(TokenType::StatementEnd, ";"),
                                  Token::new_string(TokenType::Print, "print"),
                                  id.clone(),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();

    assert!(int.get_variable_value(&id).is_some());
    assert!(int.iterator.count() == 0);
}

#[test]
fn parse_expression_1() {
    let lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::Addition, "+"),
                                  Token::new_string(TokenType::IntegerValue(1), "1")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.expression(None);
    assert_eq!(result.unwrap(),
               Token::new(TokenType::IntegerValue(2), Lexeme::default()));
}

#[test]
fn parse_assert() {
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::Assert, "assert"),
                                  Token::new_string(TokenType::LParen, "("),
                                  Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::Equal, "="),
                                  Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::RParen, ")"),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.assert().unwrap();
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
                                Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.interpret().unwrap();
}

#[test]
fn evaluate_postfix_equality() {
    let mut tokens: Vec<Token> = vec![Token::new_string(TokenType::IntegerValue(1), "1"),
                                      Token::new_string(TokenType::IntegerValue(1), "1"),
                                      Token::new_string(TokenType::Equal, "=")];

    let mut iter = vec![].into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.evaluate_postfix(&mut tokens).unwrap();
}

#[test]
fn parse_expression_2() {
    let lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::Subtraction, "-"),
                                  Token::new_string(TokenType::IntegerValue(1), "1")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.expression(None);
    assert_eq!(result.unwrap(),
               Token::new(TokenType::IntegerValue(0), Lexeme::default()));
}


#[test]
fn parse_expression_5() {
    let lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![Token::new_string(TokenType::IntegerValue(3), "3"),
                                  Token::new_string(TokenType::Subtraction, "-"),
                                  Token::new_string(TokenType::IntegerValue(1), "1")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    let result = int.expression(None);
    assert_eq!(result.unwrap(),
               Token::new(TokenType::IntegerValue(2), Lexeme::default()));
}

#[test]
fn parse_var_definition_expression_1() {
    let lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![Token::new_string(TokenType::VarKeyword, "var"),
                                  Token::new_string(TokenType::Identifier, "x"),
                                  Token::new_string(TokenType::TypeDeclaration, ":"),
                                  Token::new_string(TokenType::IntegerType, "int"),
                                  Token::new_string(TokenType::ValueDefinition, ":="),
                                  Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::Addition, "+"),
                                  Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();
    assert!(int.interpreter_state.variables.contains_key("x"));
    assert_eq!(int.interpreter_state.variables.get("x"),
               Some(&Token::new_string(TokenType::IntegerValue(2), "")));
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
    let lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![Token::new_string(TokenType::VarKeyword, "var"),
                                  Token::new_string(TokenType::Identifier, "x"),
                                  Token::new_string(TokenType::TypeDeclaration, ":"),
                                  Token::new_string(TokenType::IntegerType, "int"),
                                  Token::new_string(TokenType::ValueDefinition, ":="),
                                  Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::Addition, "+"),
                                  Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::Addition, "+"),
                                  Token::new_string(TokenType::IntegerValue(7), "7"),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    let mut int = Interpreter::new(&mut iter, &mut state);
    int.interpret();
    assert!(int.interpreter_state.variables.contains_key("x"));
    assert_eq!(int.interpreter_state.variables.get("x"),
               Some(&Token::new_string(TokenType::IntegerValue(9), "")));
}

#[test]
fn parse_var_definition_expression_3() {

    // var x: int := (1+3)*4
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::VarKeyword, "var"),
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
                                  Token::new_string(TokenType::StatementEnd, ";")];

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
    let lexeme = Lexeme::default();
    // (1+3)*4
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::LParen, "("),
                                  Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::Addition, "+"),
                                  Token::new_string(TokenType::IntegerValue(3), "3"),
                                  Token::new_string(TokenType::RParen, ")"),
                                  Token::new_string(TokenType::Multiplication, "*"),
                                  Token::new_string(TokenType::IntegerValue(4), "4")];

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
    let lexeme = Lexeme::default();
    // (1+3)*4
    let tokens: Vec<Token> = vec![Token::new_string(TokenType::VarKeyword, "var"),
                                  Token::new_string(TokenType::Identifier, "x"),
                                  Token::new_string(TokenType::TypeDeclaration, ":"),
                                  Token::new_string(TokenType::IntegerType, "int"),
                                  Token::new_string(TokenType::ValueDefinition, ":="),
                                  Token::new_string(TokenType::IntegerValue(1), "1"),
                                  Token::new_string(TokenType::Addition, "+"),
                                  Token::new_string(TokenType::IntegerValue(3), "3"),
                                  Token::new_string(TokenType::Multiplication, "*"),
                                  Token::new_string(TokenType::IntegerValue(2), "2"),
                                  Token::new_string(TokenType::StatementEnd, ";")];

    let mut iter = tokens.into_iter();
    let mut state = InterpreterState::new();
    {
        let mut int = Interpreter::new(&mut iter, &mut state);
        int.interpret();
    }

    assert_eq!(state.variables.get("x"),
               Some(&Token::new_string(TokenType::IntegerValue(7), "")));
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

    let tokens1: Vec<Token> = vec![Token::new_string(TokenType::VarKeyword, "var"),
                                   Token::new_string(TokenType::Identifier, "x"),
                                   Token::new_string(TokenType::TypeDeclaration, ":"),
                                   Token::new_string(TokenType::IntegerType, "int"),
                                   Token::new_string(TokenType::StatementEnd, ";")];

    let tokens2: Vec<Token> = vec![Token::new_string(TokenType::Identifier, "x"),
                                   Token::new_string(TokenType::ValueDefinition, ":="),
                                   val,
                                   Token::new_string(TokenType::StatementEnd, ";")];


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
    assert_eq!(state.variables.get("x"),
               Some(&Token::new_string(TokenType::IntegerValue(1), "1")));
}
