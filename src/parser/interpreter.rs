use std::collections::HashMap;
use std::iter::Peekable;
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
    //pub fn new(tokens: &'a Iterator<Item = Token<'a>>) -> Interpreter<'a> {
    pub fn new(iter: TokenIteratorType<'a>)-> Interpreter<'a>  {
        Interpreter {
            variables: HashMap::new(),
            iterator: iter.peekable()
        }
    }

    pub fn interpret(&mut self) {
        while self.iterator.peek().cloned().is_some() {
            self.statement();
        }
    }

    pub fn interpret_tokens(&mut self, tokens: TokenIteratorType<'a>) {
        self.iterator = tokens.peekable();
        self.statement();
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
        println!("{:?}", self.variables);
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
        let keyword: Token = self.expectNext(TokenType::VarKeyword);
        let id: Token = self.expectNext(TokenType::Identifier);
        let typeassign: Token = self.expectNext(TokenType::TypeDeclaration);
        let tokentype = self.iterator.next().unwrap();
        self.setVariableValue(id, tokentype);
        if self.expectPeek(TokenType::ValueDefinition) {
            self.assign(id);
        }
    }

    fn assign(&mut self, id: Token<'a>) {
        self.expectNext(TokenType::ValueDefinition);
        let expressionValue: Token<'a> = self.expression();
        self.setVariableValue(id, expressionValue);
    }

    fn print(&mut self) {
        self.expectNext(TokenType::Print);
        let next = self.iterator.next();
        if let Some(token) = next {
            let tt: TokenType = token.token_type;
            match tt {
                TokenType::Identifier => {
                    println!("print variable: {:?}", self.getVariableValue(token.clone()));
                }
                _ => unimplemented!()
            }
        }

    }

    fn expression(&mut self) -> Token<'a> {
        println!("Parsing expression");
        let next: Token<'a> = self.iterator.next().unwrap();
        let leftOperand: Token<'a> = match next.token_type {
            TokenType::LParen => { //handle nested expression
                let res = self.expression();
                self.expectNext(TokenType::RParen);
                res
            },
            TokenType::IntegerValue(_) => next,
            TokenType::StringLiteral => next,
            TokenType::Identifier => {* self.getVariableValue(next).unwrap()},
            _ => panic!("Unexpected token {:?}", next)
        };
        println!("Left operand:{:?}", leftOperand);

        let operation = *(self.iterator.peek().unwrap());
        let singleoperand = match operation.token_type {
            TokenType::Addition |
            TokenType::Subtraction |
            TokenType::Multiplication |
            TokenType::Division => false,
            _ => true
        };
        println!("Is single operand: {} {:?}", singleoperand, operation);
        if !singleoperand {
            let operation = self.iterator.next().unwrap();
            let rightOperand: Token<'a> = match next.token_type {
                TokenType::LParen => { //handle nested expression
                    let res = self.expression();
                    self.expectNext(TokenType::RParen);
                    res
                },
                TokenType::IntegerValue(_) => next,
                TokenType::StringLiteral => next,
                TokenType::Identifier => * self.getVariableValue(next).unwrap(),
                _ => panic!("Unexpected token {:?}", next)
            };
            println!("right operand:{:?}", rightOperand);
            let result = self.operation(leftOperand, operation, rightOperand);
            return result;
        } else {
            return leftOperand;
        }
    }

    fn operation(&mut self, left: Token<'a>, op: Token<'a>, right: Token<'a>) -> Token<'a> {
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
        result
    }



    fn expectNext(&mut self, tt: TokenType) -> Token<'a> {
        let maybetoken = self.iterator.next();
        if let Some(token) = maybetoken {
            if token.token_type == tt {
                return token;
            } else {
                panic!("Unexpected token {:?}, expected type {:?}", token, tt)
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
fn parse_print() {
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
    assert_eq!(int.variables.get("x"), Some(&Token::newString(TokenType::IntegerValue(2), "2")));
}
