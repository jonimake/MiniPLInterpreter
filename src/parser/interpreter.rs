use std::collections::HashMap;
use std::iter::Peekable;
use std::vec::Vec;
use std::vec::IntoIter;
use std::slice::Iter;
use parser::token::Token;
use parser::token_iterator::TokenIterator;
use parser::token_type::TokenType;
use lexeme::Lexeme;

type TokenIteratorType<'a> = &'a mut Iterator<Item=Token<'a>>;

pub struct Interpreter<'a>  {
    variables: HashMap<String, Token<'a>>,
    iterator: Peekable<TokenIteratorType<'a>>
}

impl<'a> Interpreter<'a> {
    //pub fn new(iter: &'a Iterator<Item = Token<'a>>) -> Interpreter<'a> {
    pub fn new(iter: TokenIteratorType<'a>)-> Interpreter<'a>  {
        Interpreter {
            variables: HashMap::new(),
            iterator: iter.peekable()
        }
    }
/*
    pub fn default()-> Interpreter<'a>  {
        let empty:Vec<Token<'a>> =vec![];
        let iter: IntoIter<Token<'a>> = empty.into_iter();
        let asd: &mut Iterator<Item=Token> = &mut iter;
        Interpreter {
            variables: HashMap::new(),
            iterator: asd.peekable()
        }
    }
*/
    pub fn interpret(&mut self) {
        while self.iterator.peek().cloned().is_some() {
            self.statement();
        }
    }

    pub fn interpret_tokens(&mut self, tokens: TokenIteratorType<'a>) {
        self.iterator = tokens.peekable();
        self.interpret();
    }

    /*
   stmt    -> var id : type [:= expr]              {var}
   stmt    -> id := expr                           {id}
   stmt    -> for id in expr .. do stmts end for   {for}
   stmt    -> read id                              {read}
   stmt    -> print expr                           {print}
   stmt    -> assert ( expr )                      {assert}
   */
    fn statement(&mut self) {
        let next = self.iterator.peek().cloned();
        if let Some(tt) = next {
            println!("foobar {:?}", tt.clone());
            match tt.token_type {
                TokenType::VarKeyword => self.varDecl(),
                TokenType::Identifier => self.idAssign(),
                //TokenType::For => self.forLoop(),
                //TokenType::Read => self.read(),
                TokenType::Print => self.print(),
                //TokenType::Assert => self.assertStmt(),
                _ => {}
            };

            self.expectNext(TokenType::StatementEnd);
        }
    }

    fn setVariableValue(&mut self, name: Token<'a>, token: Token<'a>) {
        println!("{:?} => {:?}", name.lexeme.lexeme, token);
        self.variables.insert(name.lexeme.lexeme.to_string(), token);
    }

    fn getVariableValue(&mut self, name: Token<'a>) -> Option<&Token<'a>> {
        println!("get {:?}", name);
        let name: String = name.lexeme.lexeme.to_string();
        self.variables.get(&name)
    }

    fn idAssign(&mut self) {
        let id = self.expectNext(TokenType::Identifier);
        self.expectNext(TokenType::ValueDefinition);
        self.assign(id);
    }

    fn varDecl(&mut self) {
        println!("fn varDecl");
        let keyword: Token = self.expectNext(TokenType::VarKeyword);
        let id: Token = self.expectNext(TokenType::Identifier);
        let typeassign: Token = self.expectNext(TokenType::TypeDeclaration);
        let tokentype = self.iterator.next().unwrap();
        self.setVariableValue(id, tokentype);
        if self.expectPeek(TokenType::ValueDefinition) {
            println!("has value def");
            self.assign(id);
        }
    }

    fn assign(&mut self, id: Token<'a>) {
        self.expectNext(TokenType::ValueDefinition);
        let expressionValue: Token<'a> = self.expression();
        println!("Expression value: {:?}", expressionValue);
        self.setVariableValue(id, expressionValue);
    }

    fn print(&mut self) {
        println!("fn print");
        let token = self.expectNext(TokenType::Print);
        println!("print {:?}", token.clone());
        let next = self.iterator.next();
        if let Some(token) = next {
            let tt: TokenType = token.token_type;
            match tt {
                TokenType::Identifier => {
                    println!("print variable: {:?}", self.getVariableValue(token.clone()));
                }
                TokenType::StringLiteral => {
                    println!("print literal: {:?}", tt);
                }
                TokenType::IntegerValue(x)=> {
                    println!("print integer: {:?}", x);
                }
                _ => unimplemented!()
            }
        }

    }

    fn expression(&mut self) -> Token<'a> {

        let mut all_tokens: Vec<Token<'a>> = Vec::new();
        loop {
            let token = self.iterator.peek().cloned().unwrap_or(Token::new(TokenType::StatementEnd, Lexeme::default()));
            if token.token_type == TokenType::StatementEnd {break;}
            else {
                all_tokens.push(self.iterator.next().unwrap());
            }
        }


        //let all_tokens: Vec<Token<'a>> = self.iterator.by_ref()
         //   .take_while_ref(|x| x.token_type != TokenType::StatementEnd)
          //  .collect();

        println!("after take while {:?}", self.iterator.peek().cloned());
        println!("after take hwile all tokens: {:?}", all_tokens);

        let mut output: Vec<Token<'a>> = Vec::new();
        let mut stack: Vec<Token<'a>> = Vec::new();
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
                        if isOperator(y) && precedenceEqualOrLessThan(token_type,y.token_type) {
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
        return evaluate_postfix(&mut output);
    }

    fn expectNext(&mut self, tt: TokenType) -> Token<'a> {
        let maybetoken = self.iterator.next();
        if let Some(token) = maybetoken {
            if token.token_type == tt {
                return token;
            } else {
                panic!("\nUnexpected token {:?},\nexpected type {:?}\n", token, tt)
            }
        }
        panic!("Expected token but iterator was empty")
    }

    fn expectPeek(&mut self, tt: TokenType) -> bool {
        let maybetoken = self.iterator.peek();
        if let Some(token) = maybetoken {
            return token.token_type == tt
        }
        false
    }
}

fn isOperator<'a>(token: Token<'a>) -> bool {
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

fn evaluate_postfix<'a>(mut tokens: &mut Vec<Token<'a>>) -> Token<'a> {
    let mut stack: Vec<Token<'a>> = Vec::new();

    tokens.reverse();
    while let Some(nextToken) = tokens.pop() {
        if isOperator(nextToken) {
            //println!("eval next token {:?}", nextToken.token_type);
            let operator_token_type = nextToken.token_type;
            if operator_token_type == TokenType::Exclamation {
                let t: Token<'a> = tokens.pop().unwrap();
            } else {
                let t1: Token<'a> = stack.pop().unwrap();
                //println!("pop stack {:?}", t1.clone().token_type);
                let t2: Token<'a> = stack.pop().unwrap();
                //println!("pop stack {:?}", t2.clone().token_type);

                let result = match (t1.token_type, t2.token_type) {
                    (TokenType::IntegerValue(a), TokenType::IntegerValue(b)) => binary_integer_op(b, a, operator_token_type),
                    (TokenType::StringLiteral, TokenType::StringLiteral) => {
                        Token::new(TokenType::StringLiteral, Lexeme::default())
                    },
                    _ => unimplemented!()
                };
                //println!("push stack {:?}", result.clone().token_type);
                stack.push(result);
            }
        }  else {
            //println!("push stack {:?}", nextToken.clone().token_type);
            stack.push(nextToken);
        }
    }
    if stack.len() != 1 {
        panic!("Stack length wasn't 1, error occurred");
    } else {
        return stack[0];
    }
}

fn binary_integer_op<'a>(a: i32, b: i32, op: TokenType) -> Token<'a>{
    let tt = match op {
        TokenType::Addition => TokenType::IntegerValue(a+b),
        TokenType::Subtraction => TokenType::IntegerValue(a-b),
        TokenType::Multiplication => TokenType::IntegerValue(a*b),
        TokenType::Division => TokenType::IntegerValue(a/b),
        _ => {
            //println!("binary_integer_op error: {:?}", op);
            unimplemented!()
        }
    };
    Token::new(tt, Lexeme::default())
}

fn operation<'a>(left: Token<'a>, op: Token<'a>, right: Token<'a>) -> Token<'a> {
    let types = (left.token_type, right.token_type);

    let result: Token<'a> = match op.token_type {
        TokenType::Addition =>
            match types {
                (TokenType::IntegerValue(leftVal), TokenType::IntegerValue(rightVal)) => {
                    Token::new(TokenType::IntegerValue(leftVal+rightVal), Lexeme::default())
                }
                _=> unimplemented!()
            },
        TokenType::Subtraction =>
            match types {
                (TokenType::IntegerValue(leftVal), TokenType::IntegerValue(rightVal)) => {
                    Token::new(TokenType::IntegerValue(leftVal-rightVal), Lexeme::default())
                }
                _=> unimplemented!()
            },
        TokenType::Multiplication =>
            match types {
                (TokenType::IntegerValue(leftVal), TokenType::IntegerValue(rightVal)) => {
                    Token::new(TokenType::IntegerValue(leftVal*rightVal), Lexeme::default())
                }
                _=> unimplemented!()
            },
        TokenType::Division =>
            match types {
                (TokenType::IntegerValue(leftVal), TokenType::IntegerValue(rightVal)) => {
                    Token::new(TokenType::IntegerValue(leftVal/rightVal), Lexeme::default())
                }
                _=> unimplemented!()
            },
        _ => unimplemented!()
    };
    println!("op result:{:?}", result);
    result
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

fn RPN<'a>(all_tokens: Vec<Token<'a>>) -> Vec<Token<'a>>{

    let mut output: Vec<Token<'a>> = Vec::new();
    let mut stack: Vec<Token<'a>> = Vec::new();
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
                    if isOperator(y) && precedenceEqualOrLessThan(token_type,y.token_type) {
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
    println!("RPN output");
    for t in output.clone() {
        println!("{:?}", t.token_type);
    }
    println!("RPN output end");
    output
}

#[test]
fn evaluate_postfix_simple_addition() {
    let mut tokens: Vec<Token> = vec![
        Token::newString(TokenType::IntegerValue(1), "1"),
        Token::newString(TokenType::IntegerValue(2), "2"),
        Token::newString(TokenType::Addition, "+")
    ];

    let res = evaluate_postfix(&mut tokens);
    assert_eq!(res.token_type, TokenType::IntegerValue(3));
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

    let res = evaluate_postfix(&mut tokens);
    assert_eq!(res.token_type, TokenType::IntegerValue(14));
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
        val,
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut int = Interpreter::new(&mut iter);
    int.interpret();
    assert!(int.variables.contains_key("x"));
    assert_eq!(int.variables.get("x"), Some(&val));
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
        tokentype,
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut int = Interpreter::new(&mut iter);
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
    let mut int = Interpreter::new(&mut iter);
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
        Token::newString(TokenType::StringLiteral, "foobar"),
        Token::newString(TokenType::StatementEnd, ";"),
    ];

    let mut iter = tokens.into_iter();
    let mut int = Interpreter::new(&mut iter);
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
        id,
        Token::newString(TokenType::StatementEnd, ";"),

    ];

    let mut iter = tokens.into_iter();
    let mut int = Interpreter::new(&mut iter);
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
    let mut int = Interpreter::new(&mut iter);
    let result = int.expression();
    println!("{:?}", result);
    assert_eq!(result, Token::new(TokenType::IntegerValue(2), Lexeme::default()));
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
    let mut int = Interpreter::new(&mut iter);
    let result = int.expression();
    println!("{:?}", result);
    assert_eq!(result, Token::new(TokenType::IntegerValue(0), Lexeme::default()));
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
    let mut int = Interpreter::new(&mut iter);
    int.interpret();
    assert!(int.variables.contains_key("x"));
    assert_eq!(int.variables.get("x"), Some(&Token::newString(TokenType::IntegerValue(2), "")));
}

#[test]
fn is_operator_test() {
    assert!(isOperator(Token::newString(TokenType::Addition, "+")));
    assert!(isOperator(Token::newString(TokenType::Addition, "*")));
    assert!(isOperator(Token::newString(TokenType::Addition, "-")));
    assert!(isOperator(Token::newString(TokenType::Addition, "/")));
    assert!(isOperator(Token::newString(TokenType::Exclamation, "!")));
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
    let mut int = Interpreter::new(&mut iter);
    int.interpret();
    assert!(int.variables.contains_key("x"));
    assert_eq!(int.variables.get("x"), Some(&Token::newString(TokenType::IntegerValue(9), "")));
}

#[test]
fn test_precedence() {
    assert!(precedenceEqualOrLessThan(TokenType::Addition, TokenType::Addition));
    assert!(precedenceEqualOrLessThan(TokenType::Addition, TokenType::Multiplication));
    assert!(precedenceEqualOrLessThan(TokenType::Addition, TokenType::Division));
    assert!(!precedenceEqualOrLessThan(TokenType::Multiplication, TokenType::Addition));
    assert!(!precedenceEqualOrLessThan(TokenType::Division, TokenType::Subtraction));
}