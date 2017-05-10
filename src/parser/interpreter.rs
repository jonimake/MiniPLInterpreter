use std::collections::HashMap;
use std::iter::Peekable;
use std::vec::Vec;
use std::vec::IntoIter;
use std::slice::Iter;
use std::result::Result;

use parser::token::Token;
use parser::token_iterator::TokenIterator;
use parser::token_type::TokenType;
use lexeme::Lexeme;

use simplelog;

pub enum InterpError {
    foo,
    bar
}

type TokenIteratorType<'a> = &'a mut Iterator<Item=Token>;

//static STRING_CACHE: HashMap<u64, String>;

pub struct InterpreterState {
    pub variables: HashMap<String, Token>,
    pub string_cache: HashMap<u64, String>
}


pub struct Interpreter<'a>  {
    pub variables: &'a mut HashMap<String, Token>,
    iterator: Peekable<TokenIteratorType<'a>>,
    string_cache: &'a mut HashMap<u64, String>
}

impl<'a> Interpreter<'a> {
    pub fn new(iter: TokenIteratorType<'a>, state: &'a mut HashMap<String, Token>, string_cache: &'a mut HashMap<u64, String>) -> Interpreter<'a> {
        Interpreter {
            variables: state,
            iterator: iter.peekable(),
            string_cache: string_cache
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
    fn statement(&mut self) -> Result<(), String>{
        let next = self.iterator.peek().cloned();
        if let Some(tt) = next {
            debug!("foobar {:?}", tt.clone());
            match tt.token_type {
                TokenType::VarKeyword => self.varDecl()?,
                TokenType::Identifier => self.idAssign()?,
                //TokenType::For => self.forLoop(),
                //TokenType::Read => self.read(),
                TokenType::Print => self.print()?,
                //TokenType::Assert => self.assertStmt(),
                _ => {}
            };

            self.expectNext(TokenType::StatementEnd)?;

        }
        Ok(())
    }

    fn setVariableValue(&mut self, name: Token, token: Token) {
        debug!("{:?} => {:?}", name.lexeme.lexeme, token);
        self.variables.insert(name.lexeme.lexeme.to_string(), token);
    }

    fn getVariableValue(&mut self, name: Token) -> Option<&Token> {
        debug!("get {:?}", name);
        let name: String = name.lexeme.lexeme.to_string();
        self.variables.get(&name)
    }

    fn idAssign(&mut self) -> Result<(), String>{
        let id = self.expectNext(TokenType::Identifier)?;
        self.expectPeek(TokenType::ValueDefinition);
        self.assign(id)?;
        Ok(())
    }

    fn varDecl(&mut self) -> Result<(), String> {
        debug!("fn varDecl");
        let keyword: Token = self.expectNext(TokenType::VarKeyword)?;
        let id: Token = self.expectNext(TokenType::Identifier)?;
        let typeassign: Token = self.expectNext(TokenType::TypeDeclaration)?;
        let tokentype = self.iterator.next().unwrap();
        self.setVariableValue(id.clone(), tokentype);
        if self.expectPeek(TokenType::ValueDefinition) {
            debug!("has value def");
            return self.assign(id);
        }
        Ok(())
    }

    fn assign(&mut self, id: Token) -> Result<(), String>{
        self.expectNext(TokenType::ValueDefinition)?;
        let expressionValue: Token = self.expression()?;
        debug!("Expression value: {:?}", expressionValue);
        self.setVariableValue(id, expressionValue);
        Ok(())
    }

    fn print(&mut self) -> Result<(), String>{
        debug!("fn print");
        let token = self.expectNext(TokenType::Print);
        debug!("print {:?}", token.clone());
        let next = self.iterator.next();
        if let Some(token) = next {
            let tt: TokenType = token.token_type;
            match tt {
                TokenType::Identifier => {
                    info!("{} => {:?}", token.lexeme.lexeme, self.getVariableValue(token.clone()));
                }
                TokenType::StringLiteral(_) => {
                    info!("{:?}", tt);
                }
                TokenType::IntegerValue(x)=> {
                    info!("{}", x);
                }
                _ => return Err(format!("Not implemented yet"))
            }
        }
        Ok(())
    }
    fn expression(&mut self) -> Result<Token, String> {

        let mut all_tokens: Vec<Token> = Vec::new();
        loop {
            let token = self.iterator.peek().cloned().unwrap_or(Token::new(TokenType::StatementEnd, Lexeme::default()));
            if token.token_type == TokenType::StatementEnd {break;}
            else {
                all_tokens.push(self.iterator.next().unwrap());
            }
        }

        debug!("after take while {:?}", self.iterator.peek().cloned());
        debug!("after take while all tokens: {:?}", all_tokens);

        let mut output: Vec<Token> = Vec::new();
        let mut opstack: Vec<Token> = Vec::new();

        let mut counter: i32 = 0;
        for token in all_tokens {
            debug!("{:?}", token.clone());
            let token_type = token.token_type;
            match token_type {
                //handle operator
                TokenType::Addition
                | TokenType::Multiplication
                | TokenType::Division
                | TokenType::Subtraction
                | TokenType::Exclamation //negation
                => {

                    while let Some(y) = opstack.last().cloned() {
                        counter = counter+1;
                        if isOperator(&y) && precedenceEqualOrLessThan(token_type,y.token_type) {
                            output.push(opstack.pop().unwrap());
                        }
                        else {
                            break;
                        }
                        if(counter > 10000) {
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
                            _ => {output.push(opstack.pop().unwrap())}
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
        debug!("Evaluating postfix");
        return self.evaluate_postfix(&mut output);
    }

    fn expectNext(&mut self, tt: TokenType) -> Result<Token, String> {
        let maybetoken = self.iterator.next();
        if let Some(token) = maybetoken {
            trace!("expectNext {:?}, actual {:?}", tt.clone(), token.clone());
            if token.token_type == tt {
                return Ok(token);
            } else {
                return Err(format!("\nUnexpected token {:?},\nexpected type {:?}\n", token, tt));
            }
        }
        Err(format!("Expected token but iterator was empty"))
    }

    fn expectPeek(&mut self, tt: TokenType) -> bool {
        let maybetoken = self.iterator.peek();
        if let Some(token) = maybetoken {
            return token.token_type == tt
        }
        false
    }


    fn evaluate_postfix(&mut self, mut tokens: &mut Vec<Token>) -> Result<Token, String> {
        let mut stack: Vec<Token> = Vec::new();

        fn binary_op(t1: &Token, t2: &Token, operator_token_type: &TokenType) -> Result<Token, String> {
            let type1 = t1.token_type;
            let type2 = t2.token_type;

            match (type1, type2) {
                (TokenType::IntegerValue(a), TokenType::IntegerValue(b)) => binary_integer_op(a, b, *operator_token_type),
                t @ _ => return Err(format!("Binary operation not defined for types {:?}", t))
            }
        }

        for token in tokens.clone() {
            debug!("{:?}", token);
        }

        tokens.reverse();
        while let Some(nextToken) = tokens.pop() {
            if isOperator(&nextToken) {
                //debug!("eval next token {:?}", nextToken.token_type);
                let operator_token_type = nextToken.token_type;
                if operator_token_type == TokenType::Exclamation {
                    let t: Token = tokens.pop().unwrap();
                } else {
                    let t1: Token = stack.pop().unwrap();
                    trace!("pop stack {:?}", t1.clone().token_type);
                    let t2: Token = stack.pop().unwrap();
                    trace!("pop stack {:?}", t2.clone().token_type);

                    let result: Token = match (t1.token_type, t2.token_type) {
                        (TokenType::IntegerValue(a), TokenType::IntegerValue(b)) => binary_integer_op(b, a, operator_token_type)?,
                        (TokenType::Identifier, _) => {
                            binary_op(self.getVariableValue(t1).unwrap(), &t2, &operator_token_type)?
                        },
                        (_, TokenType::Identifier) => {
                            binary_op(&t1, self.getVariableValue(t2).unwrap(), &operator_token_type)?
                        },
                        (TokenType::StringLiteral(_), TokenType::StringLiteral(_)) => {
                            Token::new(TokenType::StringLiteral(0), Lexeme::default())
                        },
                        _ => unimplemented!()
                    };
                    //debug!("push stack {:?}", result.clone().token_type);
                    stack.push(result);
                }
            }  else {
                //debug!("push stack {:?}", nextToken.clone().token_type);
                stack.push(nextToken);
            }
        }
        if stack.len() != 1 {
            panic!("Stack length wasn't 1, error occurred");
        } else {
            return Ok(stack[0].clone());
        }
    }
}

fn isOperator(token: &Token) -> bool {
    let token_type = token.token_type;
    match token_type {
        TokenType::Addition
        | TokenType::Multiplication
        | TokenType::Division
        | TokenType::Subtraction
        | TokenType::Exclamation => true,
        _ => false
    }
}


fn binary_integer_op(a: i32, b: i32, op: TokenType) -> Result<Token, String>{
    let tt = match op {
        TokenType::Addition => TokenType::IntegerValue(a+b),
        TokenType::Subtraction => TokenType::IntegerValue(a-b),
        TokenType::Multiplication => TokenType::IntegerValue(a*b),
        TokenType::Division => {
            if b == 0 {
                return Err(format!("Division by zero: {}/{}",a, b));
            }
            TokenType::IntegerValue(a/b)
        },
        _ => {
            return Err(format!("binary_integer_op error: {:?}", op))

        }
    };
    Ok(Token::new(tt, Lexeme::default()))
}

fn precedenceEqualOrLessThan(x: TokenType, y: TokenType) -> bool {
    match x {
        TokenType::Multiplication | TokenType::Division => {
            match y {
                TokenType::Addition | TokenType::Subtraction => false,
                _ => true
            }
        }
        _ => true
    }
}

fn RPN(all_tokens: Vec<Token>) -> Vec<Token>{

    let mut output: Vec<Token> = Vec::new();
    let mut stack: Vec<Token> = Vec::new();
    for token in all_tokens {
        let token_type = token.token_type;
        match token_type {
            TokenType::Addition
            | TokenType::Multiplication
            | TokenType::Division
            | TokenType::Subtraction
            | TokenType::Exclamation //negation
            => {
                while let Some(y) = stack.first().cloned() {
                    if isOperator(&y) && precedenceEqualOrLessThan(token_type,y.token_type) {
                        output.push(stack.pop().unwrap());
                    }
                }
                stack.push(token);
            },
            TokenType::LParen => {stack.push(token)},
            TokenType::RParen => {
                loop {
                    if let Some(top) = stack.last().cloned(){
                        if top.token_type ==  TokenType::LParen {
                            break;
                        }
                        output.push(stack.pop().unwrap()); //pop last
                    }
                        else {
                            break;
                        }
                }
                stack.pop();
            },
            _ => {output.push(token)}
        }
    }
    while let Some(token) = stack.pop() {
        output.push(token);
    }
    debug!("RPN output");
    for t in output.clone() {
        debug!("{:?}", t.token_type);
    }
    debug!("RPN output end");
    output
}

#[test]
fn evaluate_postfix_simple_addition() {
    let mut tokens: Vec<Token> = vec![
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::IntegerValue(2), "2"),
        Token::newString(TokenType::Addition, "+")
    ];

    let mut iter = vec![].into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);

    let res = int.evaluate_postfix(&mut tokens);
    assert_eq!(res.unwrap().token_type, TokenType::IntegerValue(3));
}

#[test]
fn evaluate_postfix_complex() {
    //5 1 2 + 4 * + 3 −
    let mut tokens: Vec<Token> = vec![

        Token::newString(TokenType::IntegerValue(5), "5"),
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::IntegerValue(2), "2"),
        Token::newString(TokenType::Addition, "+"),
        Token::newString(TokenType::IntegerValue(4), "4"),
        Token::newString(TokenType::Multiplication, "*"),
        Token::newString(TokenType::Addition, "+"),
        Token::newString(TokenType::IntegerValue(3), "3"),
        Token::newString(TokenType::Subtraction, "-")
    ];
    let mut iter = vec![].into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    let res = int.evaluate_postfix(&mut tokens);
    assert_eq!(res.unwrap().token_type, TokenType::IntegerValue(14));
}

#[test]
fn parse_var_definition() {
    let lexeme = Lexeme::default();
    let val = Token::newString(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::VarKeyword, "var"),
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::TypeDeclaration, ":"),
        Token::newString(TokenType::IntegerType, "int"),
        Token::newString(TokenType::ValueDefinition, ":="),
        val.clone(),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    int.interpret();
    assert!(int.variables.contains_key("x"));
    assert_eq!(int.variables.get("x"), Some(&val));
}

#[test]
fn parse_var_definition_2() {
    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::VarKeyword, "var"),
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::TypeDeclaration, ":"),
        Token::newString(TokenType::IntegerType, "string"),
        Token::newString(TokenType::ValueDefinition, ":="),
        Token::newString(TokenType::StringLiteral(0), "\"foobar\""),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    int.interpret();
}

#[test]
fn parse_var_declaration() {
    let xtoken = Token::newString(TokenType::Identifier, "x");
    let tokentype = Token::newString(TokenType::IntegerType, "int");
    let lexeme = Lexeme::default();
    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::VarKeyword, "var"),
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::TypeDeclaration, ":"),
        tokentype.clone(),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    int.interpret();
    assert!(int.variables.contains_key("x"));
    assert_eq!(int.variables.get("x"), Some(&tokentype));
}

#[test]
fn parse_print_string_integer() {
    let lexeme = Lexeme::default();
    let id = Token::newString(TokenType::Identifier, "x");
    let val = Token::newString(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::Print, "print"),
        Token::newString(TokenType::IntegerValue(23), "23"),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    int.interpret();
    assert!(int.iterator.count() == 0);
}

#[test]
fn parse_print_string_literal() {
    let lexeme = Lexeme::default();
    let id = Token::newString(TokenType::Identifier, "x");
    let val = Token::newString(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::Print, "print"),
        Token::newString(TokenType::StringLiteral(0), "foobar"),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();

    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    int.interpret();

    assert!(int.iterator.count() == 0);
}

#[test]
fn parse_print_complicated() {
    let lexeme = Lexeme::default();
    let id = Token::newString(TokenType::Identifier, "x");
    let val = Token::newString(TokenType::IntegerValue(1), "1");
    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::VarKeyword, "var"),
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::TypeDeclaration, ":"),
        Token::newString(TokenType::IntegerType, "int"),
        Token::newString(TokenType::ValueDefinition, ":="),
        val,
        Token::newString(TokenType::StatementEnd, ";"),
        Token::newString(TokenType::Print, "print"),
        id.clone(),
        Token::newString(TokenType::StatementEnd, ";"),

    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();

    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    int.interpret();

    assert!(int.getVariableValue(id).is_some());
    assert!(int.iterator.count() == 0);
}

#[test]
fn parse_expression_1() {
    let lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::Addition, "+"),
        Token::newString(TokenType::IntegerValue(1), "1"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    let result = int.expression();
    debug!("{:?}", result);
    assert_eq!(result.unwrap(), Token::new(TokenType::IntegerValue(2), Lexeme::default()));
}

#[test]
fn parse_expression_2() {
    let lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::Subtraction, "-"),
        Token::newString(TokenType::IntegerValue(1), "1"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    let result = int.expression();
    debug!("{:?}", result);
    assert_eq!(result.unwrap(), Token::new(TokenType::IntegerValue(0), Lexeme::default()));
}


#[test]
fn parse_var_definition_expression_1() {
    let lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::VarKeyword, "var"),
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::TypeDeclaration, ":"),
        Token::newString(TokenType::IntegerType, "int"),
        Token::newString(TokenType::ValueDefinition, ":="),
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::Addition, "+"),
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    int.interpret();
    assert!(int.variables.contains_key("x"));
    assert_eq!(int.variables.get("x"), Some(&Token::newString(TokenType::IntegerValue(2), "")));
}

#[test]
fn is_operator_test() {
    assert!(isOperator(&Token::newString(TokenType::Addition, "+")));
    assert!(isOperator(&Token::newString(TokenType::Addition, "*")));
    assert!(isOperator(&Token::newString(TokenType::Addition, "-")));
    assert!(isOperator(&Token::newString(TokenType::Addition, "/")));
    assert!(isOperator(&Token::newString(TokenType::Exclamation, "!")));
}

#[test]
fn parse_var_definition_expression_2() {
    let lexeme = Lexeme::default();

    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::VarKeyword, "var"),
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::TypeDeclaration, ":"),
        Token::newString(TokenType::IntegerType, "int"),
        Token::newString(TokenType::ValueDefinition, ":="),
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::Addition, "+"),
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::Addition, "+"),
        Token::newString(TokenType::IntegerValue(7), "7"),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    int.interpret();
    assert!(int.variables.contains_key("x"));
    assert_eq!(int.variables.get("x"), Some(&Token::newString(TokenType::IntegerValue(9), "")));
}

//#[test]
fn parse_var_definition_expression_3() {
    let lexeme = Lexeme::default();
    // var x: int := (1+3)*4
    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::VarKeyword, "var"),
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::TypeDeclaration, ":"),
        Token::newString(TokenType::IntegerType, "int"),
        Token::newString(TokenType::ValueDefinition, ":="),
        Token::newString(TokenType::LParen, "("),
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::Addition, "+"),
        Token::newString(TokenType::IntegerValue(3), "3"),
        Token::newString(TokenType::RParen, ")"),
        Token::newString(TokenType::Multiplication, "*"),
        Token::newString(TokenType::IntegerValue(4), "4"),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    int.interpret();
    assert!(int.variables.contains_key("x"));
    let expectedValue = Token::newString(TokenType::IntegerValue(12), "12");
    assert_eq!(int.variables.get("x"), Some(&expectedValue));
}


#[test]
fn parse_expression_3() {
    let lexeme = Lexeme::default();
    // (1+3)*4
    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::LParen, "("),
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::Addition, "+"),
        Token::newString(TokenType::IntegerValue(3), "3"),
        Token::newString(TokenType::RParen, ")"),
        Token::newString(TokenType::Multiplication, "*"),
        Token::newString(TokenType::IntegerValue(4), "4"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
    let result = int.expression();
    println!("result = {:?}", result);
    let expectedValue = Token::newString(TokenType::IntegerValue(16), "");
    assert_eq!(result.unwrap(), expectedValue);
}

#[test]
fn parse_expression_4() {
    let lexeme = Lexeme::default();
    // (1+3)*4
    let tokens: Vec<Token> = vec![
        Token::newString(TokenType::VarKeyword, "var"),
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::TypeDeclaration, ":"),
        Token::newString(TokenType::IntegerType, "int"),
        Token::newString(TokenType::ValueDefinition, ":="),
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::Addition, "+"),
        Token::newString(TokenType::IntegerValue(3), "3"),
        Token::newString(TokenType::Multiplication, "*"),
        Token::newString(TokenType::IntegerValue(2), "2"),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    {
        let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
        int.interpret();
    }

    assert_eq!(state.get("x"), Some(&Token::newString(TokenType::IntegerValue(7), "")));
}

#[test]
fn test_precedence() {
    assert!(precedenceEqualOrLessThan(TokenType::Addition, TokenType::Addition));
    assert!(precedenceEqualOrLessThan(TokenType::Addition, TokenType::Multiplication));
    assert!(precedenceEqualOrLessThan(TokenType::Addition, TokenType::Division));
    assert!(!precedenceEqualOrLessThan(TokenType::Multiplication, TokenType::Addition));
    assert!(!precedenceEqualOrLessThan(TokenType::Division, TokenType::Subtraction));
}

#[test]
fn stateful_var_definition() {
    //let _ = simplelog::TermLogger::init(simplelog::LogLevelFilter::Trace, simplelog::Config::default());
    let val = Token::newString(TokenType::IntegerValue(1), "1");

    let tokens1: Vec<Token> = vec![
        Token::newString(TokenType::VarKeyword, "var"),
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::TypeDeclaration, ":"),
        Token::newString(TokenType::IntegerType, "int"),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let tokens2: Vec<Token> = vec![
        Token::newString(TokenType::Identifier, "x"),
        Token::newString(TokenType::ValueDefinition, ":="),
        val,
        Token::newString(TokenType::StatementEnd, ";"),
    ];


    let mut state = HashMap::new();
    let mut strings = HashMap::new();
    {
        let mut iter = tokens1.into_iter();
        let mut int = Interpreter::new(&mut iter, &mut state, &mut strings);
        int.interpret();
    }
    {
        let mut iter = tokens2.into_iter();
        let mut int2 = Interpreter::new(&mut iter, &mut state, &mut strings);
        int2.interpret();
    }


    assert!(state.contains_key("x"));
    assert_eq!(state.get("x"), Some(&Token::newString(TokenType::IntegerValue(1), "1")));
}